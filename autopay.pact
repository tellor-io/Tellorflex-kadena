(namespace (read-msg "ns"))

(module autopay TELLOR-ADMIN
  (defcap TELLOR-ADMIN ()
    (enforce-guard (keyset-ref-guard (+ (read-msg "ns") ".tellor-admin-keyset")))
  )

  ;******************************OBJECT-SCHEMA**********************************
  (defschema TIP
      amount:integer
      cumulative-tips:integer
      timestamp:integer
  )
  ;******************************TABLE-SCHEMA***********************************
  (defschema global-schema
      fee:integer
      query-ids-with-funding:[string]
  )
  (defschema tips-schema
      tips:[object{TIP}]
  )
  (defschema query-ids-with-funding-index-schema
      index:integer
  )
  (defschema user-tips-total-schema
      total:integer
  )
  ;******************************TABLE-DEFINITION*******************************
  (deftable global:{global-schema})
  (deftable tips:{tips-schema})
  (deftable query-ids-with-funding-index:{query-ids-with-funding-index-schema})
  (deftable user-tips-total:{user-tips-total-schema})
  ;******************************CAP/CONST/GUARD********************************
  (defcap TipAdded:bool
      (query-id:string amount:integer query-data:string tipper:string)
      @event true
  )
  (defcap OneTimeTipClaimed:bool
      (query-id:string amount:integer claimant:string)
      @event true
  )
  (defcap PRIVATE ()
      true
  )
  (defun cap:bool ()
      (require-capability (PRIVATE))
  )
  (defconst AUTOPAY_GUARD
      (create-user-guard (cap))
  )
  (defconst PRECISION (^ 10 12))
  ; ;******************************SETTERS**************************************
  (defun constructor (fee:integer)
      (insert global "global" { "fee": fee, "query-ids-with-funding": [] })
      (coin.create-account "autopay" AUTOPAY_GUARD)
  )
  (defun claim-one-time-tip
    (claimant:string query-id:string timestamps:[integer])
      (enforce-keyset (read-keyset claimant))
      (let ((tips (at "tips" (read tips query-id))))
          (enforce (> (length tips) 0) "no tips submitted for this queryId")
          (let* ( (cumulative-reward (fold (calc-one-time-rewards)
                      { "query-id": query-id,
                        "timestamp-lis":  timestamps,
                        "reward": 0 }
                      (enumerate 0 (length timestamps))))
                  (fee (at "fee" (read global "global")))
                  (fee-total (/ (* cumulative-reward fee) 1000))
                  (pay-amount (- cumulative-reward fee-total)))
              (install-capability "autopay" claimant (to-decimal pay-amount))
              (coin.transfer "autopay" claimant (to-decimal pay-amount))
              (tellorflex.add-staking-rewards "autopay" fee-total)
              (if (= (get-current-tip query-id) 0)
                  (if (= (at "index" (read query-ids-with-funding-index query-id)) 0)
                      (let* ((idx (- (at "index" (read query-ids-with-funding-index query-id)) 1))
                             (query-ids (at "query-ids-with-funding" (read global "global")))
                             (last-item (take -1 query-ids)))
                        (update global "global"
                            { "query-ids-with-funding":
                            (+
                                (+ (take idx query-ids) last-item)
                                (take (* -1 idx) query-ids))})
                        (write query-ids-with-funding-index last-item
                            { "index": (+ 1 idx) })
                        (write query-ids-with-funding-index query-id
                            { "index": 0 })
                      )
                  )
              )
            (emit-event (OneTimeTipClaimed query-id pay-amount claimant))
          )
      )
  )
  (defun tip (tipper:string query-id:string amount:integer query-data:string)
      (enforce-keyset (read-keyset tipper))
      (enforce (= (hash query-data) query-id) "id must be hash of bytes data")
      (enforce (> amount 0) "tip must be greater than zero")
      (with-default-read tips query-id
          { "tips": [] }
          { "tips" := all-tips }
          (if (= (length all-tips) 0)
              [ (write tips query-id { "tips": [
                                      { "amount": amount
                                      , "cumulative-tips": amount
                                      , "timestamp": (blockTime) }]} )
                (queryDataStorage.store-data query-data) ]
              (let (  (last-item (at (- (length all-tips) 1) all-tips))
                      (current-value-timestamp
                          (try {"value": "", "timestamp": 0}
                            (tellorflex.get-data-before
                              query-id (+ (blockTime) 1) ))))
                  (bind last-item
                      { "amount" := tip-amount
                      , "cumulative-tips" := cumulative-tips
                      , "timestamp" := tip-timestamp }
                    (if (< (at "timestamp" current-value-timestamp) tip-timestamp)
                        (let ((drop-last (drop (- 1) all-tips)))
                            (update tips query-id
                              { "tips" : (+ drop-last [
                                { "amount": (+ tip-amount amount)
                                , "cumulative-tips": (+ cumulative-tips amount)
                                , "timestamp": (blockTime) }]) } )
                        )
                        (update tips query-id
                          { "tips": (+ all-tips
                                  [{ "amount": amount
                                   , "cumulative-tips": (+ cumulative-tips amount)
                                   , "timestamp": (blockTime) }] )}
                        )
                    )
                 )
              )
          )
          (with-default-read query-ids-with-funding-index query-id
              { "index": 0 }{ "index" := idx}
              (if (and (= idx 0) (> (get-current-tip query-id) 0))
                  (let ((query-ids (at "query-ids-with-funding" (read global "global"))))
                        (update global "global"
                            { "query-ids-with-funding": (+ query-ids [query-id])})
                        (write query-ids-with-funding-index query-id
                            { "index": (+ (length query-ids) 1)})

                  )
                  "Query id already exists in funding ids/tip greater than 0"
              )
          )
          (coin.transfer tipper "autopay" (to-decimal amount))
          (with-default-read user-tips-total tipper
              { "total": 0 }{ "total" := tips-total }
              (write user-tips-total tipper
                  { "total": (+ tips-total amount) }
              )
          )
      )
      (emit-event (TipAdded query-id amount query-data tipper))
  )
  (defun calc-one-time-rewards (x y)
      (let* ( (query-id (at "query-id" x))
              (timestamp (at y (at "timestamp-lis" x)))
              (reward (get-one-time-tip-amount query-id timestamp)))
          { "query-id": query-id,
            "timestamp-lis":  (at "timestamp-lis" x),
            "reward": (+ reward (at "reward" x))}
      )
  )
  (defun search (x _)
      (let ((max (at "max" x))
            (min (at "min" x))  )
          (if (> (- max min) 1)
              (let ((mid (/ (+ max min) 2)))
                  (if (> (at mid (at "list" x)) (at "timestamp" x))
                      { "max": mid
                      , "min": min
                      , "timestamp": (at "timestamp" x)
                      , "list": tips }
                      { "max": max
                      , "min": mid
                      , "timestamp": (at "timestamp" x)
                      , "list": tips }
                  )
              )
              { "max": (length tips)
              , "min": 0
              , "timestamp": (at "timestamp" x)
              , "list": tips }
          )
      )
  )
  (defun get-one-time-tip-amount
    (claimant:string query-id:string timestamp:integer)
    (enforce-keyset (read-keyset claimant))
    (enforce (> (- tellorflex.block-time-in-seconds timestamp) (hours 12))
      "buffer time has not passed")
    (enforce (tellorflex.is-in-dispute query-id timestamp) "value disputed")
    (enforce (= claimant (tellorflex.get-reporter-by-timestamp query-id timestamp))
      "msg sender must be reporter address")
    (let* ( (tips-lis (at "tips" (read tips query-id)))
            (search-object
              (fold (search)
                    { "max": (length tips-lis)
                    , "min": 0
                    , "timestamp": timestamp
                    , "list": tips-lis }
                    (enumerate 0 (log 2 (length tips))) ))
            (timestamp-before
              (at "timestamp" (tellorflex.get-data-before query-id timestamp)))
            (min-timestamp
              (at "timestamp" (at (at "min" search-object) tips-lis)))
            (min-amount (at "amount" (at (at "min" search) tips-lis))))
        (enforce (< timestamp-before min-timestamp)
          "tip earned by previous submission")
        (enforce (>= timestamp min-timestamp)
          (format "{} timestamp not eligible for tip" [timestamp]))
        (enforce (> min-amount 0) "tip already claimed")
        (let* ( (timestamp-after
                  (at "timestamp" (tellorflex.get-data-before query-id (+ timestamp 1))))
                (timestamp-closest
                  (at "timestamp" (try {"value": "", "timestamp": 0}
                    (tellorflex.get-data-before query-id (+ (blockTime) 1) ))))
                (index-now
                  (tellorflex.get-timestamp-index-by-timestamp query-id timestamp-after))
                (index-before
                  (tellorflex.get-timestamp-index-by-timestamp query-id timestamp-before)))
            (if (or (> (- index-now index-before) 1) (= "" (at "value" timestamp-closest)))
                (at "cumlative-tips" (at (at "min" search) tips-lis))
                (let ((search-object2
                        (fold (search)
                              { "max": (at "min" search-object)
                              , "min": 0
                              , "timestamp": timestamp-before
                              , "list": tips-lis }
                              (enumerate 0 (log 2 (length tips-lis))) )))
                    (if (< (+ 1 (at "min" search-object2)) (at "min" search-object))
                        (bind search-object2 { "min":= min }
                          (- (at "cumulative-tips" (at "min" search-object) tips-lis)
                          (+ (at "cumulative-tips" (at min tips-lis)) (at "amount" (at min tips-lis))))
                        )
                        (at "amount" (at "min" tips-lis))
                    )
                )
            )
        )
     )
  )
  (defun blockTime:integer ()
    (str-to-int 10 (format-time "%s" (at 'block-time (chain-data))))
  )
  (defun fee:integer ()
    (at "fee" (read global "global"))
  )
  (defun get-current-tip:integer (query-id:string)
      (with-default-read tips query-id
          { "tips": [] }{ "tips" := all-tips}
          (if (= (length all-tips) 0)
              0
              (let ((current-timestamp
                      (try {"value": "", "timestamp": 0}
                        (tellorflex.get-data-before query-id (+ (blockTime) 1)) ))
                  (last-item (at (- (length all-tips) 1) all-tips)))
                  (if (< (at "timestamp" current-timestamp) (at "timestamp" last-item))
                      (at "amount" last-item)
                      0
                  )
              )
          )
      )
  )
  (defun get-tips-by-user (user:string)
      (at "total" (read user-tips-total user))
  )
  (defun to-decimal:decimal (amount:integer)
    (/ (/ amount 1.0) PRECISION)
  )
)
(if (read-msg "upgrade")
    ["upgrade"]
    [
      (create-table global)
      (create-table tips)
      (create-table query-ids-with-funding-index)
      (create-table user-tips-total)
    ]
)
