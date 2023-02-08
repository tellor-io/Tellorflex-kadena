(namespace (read-msg "ns"))

(module autopay TELLOR-ADMIN
; ***************************CAPABILITIES**************************************
  (defcap TELLOR-ADMIN ()
    (enforce-guard (keyset-ref-guard (+ (read-msg "ns") ".tellor-admin-keyset")))
  )
  (defcap PRIVATE ()
    true
  )
; ***************************EVENT-CAPS****************************************
  (defcap TipAdded:bool
    (query-id:string amount:integer query-data:string tipper:string)
    @event true
  )
  (defcap OneTimeTipClaimed:bool
    (query-id:string amount:integer claimant:string)
    @event true
  )
; ***************************CONSTANTS*****************************************
  (defun private-user-cap:bool ()
    (require-capability (PRIVATE))
  )
  (defconst AUTOPAY_GUARD:guard
    (create-user-guard (private-user-cap))
  )
  (defconst PRECISION:integer 
    (^ 10 12)
  )
; ***************************TABLE-SCHEMA**************************************
  (defschema global-schema
    autopay-account:string
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
; ***************************OBJECT-SCHEMA*************************************
  (defschema TIP
    amount:integer
    cumulative-tips:integer
    timestamp:integer
  )
; ***************************TABLE-DEFINITION**********************************
  (deftable global:{global-schema})
  (deftable tips:{tips-schema})
  (deftable query-ids-with-funding-index:{query-ids-with-funding-index-schema})
  (deftable user-tips-total:{user-tips-total-schema})
; ***************************MAIN-FUNCTIONS************************************
  (defun constructor (autopay-account:string fee:integer)
      (insert global "global" 
      { "autopay-account": autopay-account
      , "fee": fee
      , "query-ids-with-funding": [] })
      (coin.create-account autopay-account AUTOPAY_GUARD)
  )
  (defun claim-one-time-tip
    (claimant:string query-id:string timestamps:[integer])
    @doc "Claim tips for data submission"
      (enforce-keyset (read-keyset claimant))
      (let ((tips (at "tips" (read tips query-id))))
        (enforce (> (length tips) 0) "no tips submitted for this queryId")
        (with-capability (PRIVATE)
          (let* ( (cumulative-reward 
                    (fold (calc-total-rewards)
                      { "claimant": claimant,
                        "query-id": query-id,
                        "timestamp-lis": timestamps,
                        "reward": 0 }
                      (enumerate 0 (- (length timestamps) 1))))
                  (fee (at "fee" (read global "global")))
                  (reward (at "reward" cumulative-reward))
                  (fee-total (/ (* reward fee) 1000))
                  (pay-amount (- reward fee-total)))
             
              (install-capability (coin.TRANSFER "autopay" claimant (to-decimal pay-amount)))
              (coin.transfer "autopay" claimant (to-decimal pay-amount))
              (install-capability (coin.TRANSFER "autopay" "tellorflex" (to-decimal fee-total)))
              (tellorflex.add-staking-rewards "autopay" fee-total)
              (if (= (get-current-tip query-id) 0)
                  (if (!= (at "index" (read query-ids-with-funding-index query-id)) 0)
                      (let* ((idx (at "index" (read query-ids-with-funding-index query-id)))
                             (query-ids (at "query-ids-with-funding" (read global "global")))
                             (last-item (take -1 query-ids))
                             (replace-qid (fold (+) (take (- idx 1) query-ids) [last-item (drop idx query-ids)]))
                             (drop-last (drop -1 replace-qid)))
                        (update global "global" { "query-ids-with-funding": drop-last })
                        (update query-ids-with-funding-index (at 0 last-item) { "index": idx })
                        (update query-ids-with-funding-index query-id { "index": 0 })
                      )
                      "none"
                  )
                  "none"
              )
            (emit-event (OneTimeTipClaimed query-id pay-amount claimant))
          )
        )
      )
  )
  (defun tip (tipper:string query-id:string amount:integer query-data:string)
    @doc "Tip a query id to get data"
    (enforce-keyset (read-keyset tipper))
    (enforce (= (hash query-data) query-id) "id must be hash of bytes data")
    (enforce (> amount 0) "tip must be greater than zero")
    (with-default-read tips query-id
        { "tips": [] }{ "tips" := all-tips }
        (if (= (length all-tips) 0)
            (let ((set-vals 
                    (lambda (tip time) 
                    { "amount": tip
                    , "cumulative-tips": tip, "timestamp": time})))
              (write tips query-id { "tips": [(set-vals amount (blockTime))]})
              (queryDataStorage.store-data query-data)
            )
            (let (  (last-item (at (- (length all-tips) 1) all-tips))
                    (current-value-timestamp
                        (tellorflex.get-data-before query-id (+ (blockTime) 1))))
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
                (format "Query id {} has existing tips"[query-id])
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
; ***************************INTERNAL-FUNCTIONS********************************
  (defun calc-total-rewards (reward-details idx)
    @doc "Calculate total tips payout"
    (require-capability (PRIVATE))
    (bind reward-details { "query-id" := query-id
                         , "timestamp-lis" := timestamps
                         , "claimant" := claimant 
                         , "reward" := reward}
      (let* ((timestamp (at idx timestamps))
             (total (get-one-time-tip-amount claimant query-id timestamp)))
        { "query-id": query-id
        , "timestamp-lis": timestamps, "reward": (+ reward total) }
      )
    )
  )
  (defun search (tips-details _)
    @doc "Binary search tips list"
    (require-capability (PRIVATE))
    (bind tips-details { "max" := max
                       , "min" := min
                       , "tips" := tips-lis, "timestamp" := timestamp }
      (let ((search-obj (lambda (mx mn) 
                          { "max": mx
                          , "min": mn
                          , "tips": tips-lis, "timestamp": timestamp })))
        (if (> (- max min) 1)
            (let ((mid (/ (+ max min) 2)))
              (if (> (at "timestamp" (at mid tips-lis)) timestamp)
                  (search-obj mid min)
                  (search-obj max mid)
              )
            )
            (search-obj (length tips-lis) 0)
        )
      )
    )
  )
  (defun get-one-time-tip-amount
    (claimant:string query-id:string timestamp:integer)
    @doc "Internal function which determines tip eligibility for a given oracle submission"
    (require-capability (PRIVATE))
    (enforce (> (- (blockTime) timestamp) 43200)
      (format "12hr. buffer time has not passed for timestamp: {}"[timestamp]))
    (let ((is-disputed (tellorflex.is-in-dispute query-id timestamp) )
          (reporter (tellorflex.get-reporter-by-timestamp query-id timestamp)))
      (enforce (not is-disputed) "value disputed")
      (enforce (= claimant reporter) "msg sender must be reporter address")
    )
    (let* ((tips-lis (at "tips" (read tips query-id)))
           (count (length tips-lis))
           (search-object
              (fold (search) { "max": count, "min": 0, "timestamp": timestamp, "tips": tips-lis }
                    (enumerate 0 (+ 1 (log 2 count))) ))
           (timestamp-before
              (at "timestamp" (tellorflex.get-data-before query-id timestamp))))
      (bind search-object { "min" := min }
        (let ((min-timestamp
                (at "timestamp" (at min tips-lis)))
              (min-amount (at "amount" (at min tips-lis))))
          (enforce (< timestamp-before min-timestamp)
            "tip earned by previous submission")
          (enforce (>= timestamp min-timestamp)
            (format "{} timestamp not eligible for tip" [timestamp]))
          (enforce (> min-amount 0) "tip already claimed"))
        
        (let* ((tip-amount (at "amount" (at min tips-lis)))
               (updated-tips (fold (+) (take min tips-lis) 
                [[{ "amount": 0, "cumulative-tips": (at "cumulative-tips" (at min tips-lis))
                  , "timestamp": (at "timestamp" (at min tips-lis))}] (drop (+ min 1) tips-lis)])) 
               (time-after-submission
                (at "timestamp" (tellorflex.get-data-before query-id (+ timestamp 1))))
               (time-b4-submission (tellorflex.get-data-before query-id (+ timestamp-before 1) ))
               (index-now
                (tellorflex.get-timestamp-index-by-timestamp query-id time-after-submission))
               (index-before
                (try 0 (tellorflex.get-timestamp-index-by-timestamp query-id (at "timestamp" time-b4-submission))))
               (before-val (at "value" time-b4-submission)))
          (update tips query-id { "tips": updated-tips })
          (if (or (> (- index-now index-before) 1) (= "" before-val))
              (if (= "" before-val)
                  (at "cumulative-tips" (at min updated-tips))
                  (let ((search-object2 (fold (search)
                                          { "max": min
                                          , "min": 0
                                          , "timestamp": timestamp-before
                                          , "tips": updated-tips }
                                        (enumerate 0 (log 2 (length updated-tips))) )))
                    (bind search-object2 { "min":= min2 }
                      (if (< (+ 1 min2) min)
                          (- (at "cumulative-tips" min updated-tips)
                            (+ (at "cumulative-tips" (at min2 updated-tips)) 
                               (at "amount" (at min2 updated-tips)))
                          )
                          tip-amount
                      )
                    )
                  )
              )
              tip-amount
          )
        )
      )
    )
  )
; ***************************GETTERS*******************************************
  (defun autopay-account:string ()
    @doc "Autopay account name in token module"
    (at "autopay-account" (read global "global"))
  )
  (defun blockTime:integer ()
    @doc "Block time in seconds"
    (str-to-int 10 (format-time "%s" (at 'block-time (chain-data))))
  )
  (defun fee:integer ()
    @doc "Fee amount dispersed to reporters"
    (at "fee" (read global "global"))
  )
  (defun get-current-tip:integer (query-id:string)
    @doc "Getter function to current oneTime tip by queryId"
    (with-default-read tips query-id
      { "tips": [] }{ "tips" := all-tips }
      (if (= (length all-tips) 0)
          0
          (let ((current-timestamp
                  (tellorflex.get-data-before query-id (+ (blockTime) 1) ))
                (last-item (at (- (length all-tips) 1) all-tips)))
            (if (< (at "timestamp" current-timestamp) (at "timestamp" last-item))
                (at "amount" last-item)
                0
            )
          )
      )
    )
  )
  (defun get-funded-query-ids:list ()
    @doc "Getter function to get list of funded query ids"
    (at "query-ids-with-funding" (read global "global"))
  )
  (defun get-past-tip-count (query-id:string) 
    @doc "Getter function to get number of past tips"
    (length (get-past-tips query-id))
  )
  (defun get-past-tips (query-id:string)
    @doc "Getter function for past tips"
    (try [] (at "tips" (read tips query-id)))
  )
  (defun get-past-tip-by-index (query-id:string index:integer)
    @doc "Getter function for past tips by index"
    (at index (at "tips" (read tips query-id)))
  )
  (defun get-query-ids-with-funding-index:integer (query-id:string)
    @doc "Get index of a funded query id"
    (at "index" (read query-ids-with-funding-index query-id))
  )
  (defun get-tips-by-user (user:string)
      (try 0 (at "total" (read user-tips-total user)))
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
      (constructor 
        (read-string "autopay-account-name") 
        (read-integer "autopay-fee"))
    ]
)
