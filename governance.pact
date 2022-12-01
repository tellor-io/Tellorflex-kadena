; tellor governance on kadena
(namespace "free")

(if (read-msg "upgrade")
  "Upgrading contract"

  [
    (enforce-keyset (read-keyset "tellor-admin-keyset"))
    (define-keyset "free.tellor-admin-keyset" (read-keyset "tellor-admin-keyset"))
  ]
)


(module governance TELLOR
  (use tellorflex)

  (defcap TELLOR ()
    (enforce-guard (keyset-ref-guard "free.tellor-admin-keyset"))
  )

  (defcap PRIVATE ()
    true
  )

  (defschema dispute-schema
    query-id:string
    timestamp:integer
    value:string
    disputed-reporter:string
    slashed-amount:integer)

  (defschema tally-schema
    does-support:integer
    against:integer
    invalid-query:integer)

  (defschema global-schema
    oracle:string
    team-multisig:string
    vote-count:integer
    autopay-query-id:string)

  (defschema vote-rounds-schema
    dispute-ids:[integer])

  (defschema open-disputes-on-id-schema
    open-disputes-on-id:integer)

  (defschema vote-schema
    identifier-hash:string
    vote-round:integer
    start-date:integer
    block-number:integer
    fee:integer
    tally-date:integer
    token-holders:object{tally-schema}
    users:object{tally-schema}
    reporters:object{tally-schema}
    team-multisig:object{tally-schema}
    executed:bool
    result:string
    initiator:string
    voters:[string])

  (defschema dispute-ids-by-reporter-schema
    dispute-ids:[integer])

  (deftable dispute-info:{dispute-schema})
  (deftable tally:{tally-schema})
  (deftable vote-info:{vote-schema})
  (deftable vote-rounds:{vote-rounds-schema})
  (deftable open-disputes-on-id:{open-disputes-on-id-schema})
  (deftable global:{global-schema})
  (deftable dispute-ids-by-reporter)

  (defun module-assets ()
    (create-module-guard "governance-contract-assets")
  )

  (defun init-global ()
    (insert global 'global-vars { 'oracle: "" , 'team-multisig: "" , 'vote-count: 0 , 'autopay-query-id: ""})
  )

  (defun begin-dispute (query-id:string timestamp:integer)
    @doc "Initializes a dispute/vote in the system"

    (enforce (!= (tellorflex.get-block-number-by-timestamp query-id timestamp) 0) "No value exists at given timestamp")

    (let ((hash (hash [query-id timestamp]))
          (dispute-id (+ (at 'vote-count (read global 'global-vars)) 1))
          (dispute-fee (get-dispute-fee))
          (block-time (tellorflex.block-time-in-seconds))
          (disputed-reporter (tellorflex.get-reporter-by-timestamp query-id timestamp)))

        (with-default-read vote-rounds hash { 'dispute-ids: []} { 'dispute-ids := dispute-ids }
          (write vote-rounds hash { 'dispute-ids: (+ dispute-ids dispute-id)})
        )
        (write vote-info dispute-id
          { 'identifier-hash: hash
          , 'vote-round: (length (at 'dispute-ids (read vote-rounds hash)))
          , 'start-date: block-time
          , 'block-number: (tellorflex.block-height)
          , 'fee: 0
          , 'tally-date: 0
          , 'token-holders: {'does-support: 0, 'against: 0, 'invalid-query: 0}
          , 'users: {'does-support: 0, 'against: 0, 'invalid-query: 0}
          , 'reporters: {'does-support: 0, 'against: 0, 'invalid-query: 0}
          , 'team-multisig: {'does-support: 0, 'against: 0, 'invalid-query: 0}
          , 'executed: false
          , 'result: ""
          , 'initiator: (tellorflex.msg-sender)
          , 'voters: []
          })

        (write dispute-info dispute-id
          { 'query-id: query-id
          , 'timestamp: timestamp
          , 'value: ""
          , 'disputed-reporter: disputed-reporter
          , 'slashed-amount: 0
          })
        (with-default-read dispute-ids-by-reporter disputed-reporter { 'dispute-ids: [] }{ 'dispute-ids := dispute-ids }
          (update dispute-ids-by-reporter disputed-reporter { 'dispute-ids: (+ dispute-ids [dispute-id])}))

          (if (= vote-rounds 1)
            (begin-dispute-pact dispute-id query-id timestamp)
            (else-begin-dispute-pact block-time dispute-id dispute-fee (at 'dispute-ids (read vote-rounds hash)))
            )
        (with-read global 'global-vars { 'vote-count := vote-count }
          (update global 'global-vars { 'vote-count: (+ vote-count 1)}))
        )
  )

  (defun execute-vote (dispute-id:integer)
    (enforce (and (<= dispute-id (length (keys dispute-info))) (> dispute-id 0)) "Dispute ID must be valid")
    (with-read vote-info dispute-id
      { 'executed := executed
      , 'tally-date := tally-date
      , 'vote-round := vote-round
      , 'hash := hash
      , 'result := result
      }
      (enforce (not (executed)) "Vote has already been executed")
      (enforce (> tally-date 0) "Vote must be tallied")
      (with-read vote-rounds hash { 'dispute-ids := dispute-ids }
        (enforce (= (length dispute-ids vote-round)) "Must be the final vote")
        (enforce (>= (- (tellorflex.block-time-in-seconds) tally-date) (days 1)) "1 day has to pass after tally to allow for disputes"))
      (update vote-info dispute-id { 'executed: true })

      (let* ( (query-id (at 'query-id (read dispute-info dispute-id)))
              (open-disputes-count (at 'open-disputes-on-id (read open-disputes-on-id query-id))))

        (update open-disputes-on-id query-id { 'open-disputes-on-id: (- open-disputes-count 1) })
      )
    (with-capability (PRIVATE)
      (if (= result 'PASSED)
          (fold (vote-passed) hash (enumerate (length (at 'dispute-ids (read vote-rounds hash))) 1))

          (if (= result 'INVALID)

              [
                (fold (vote-invalid) hash (enumerate (length (at 'dispute-ids (read vote-rounds hash))) 1))
                (with-read dispute-info dispute-id
                  { 'disputed-reporter := disputed-reporter , 'slashed-amount := slashed-amount }
                  (coin.transfer 'governance disputed-reporter slashed-amount))
              ]
          )
              (if (= result 'FAILED)
                (coin.transfer
                  'governance
                  (at 'disputed-reporter (read dispute-info dispute-id))
                  (+ (fold (vote-failed) 0 (at 'dispute-ids (read vote-rounds hash))) (at 'slashed-amount (read dispute-info dispute-id)))
                "Invalid result"
              )
          )
        )
      )
    )
  )

  ; *****************************************************************************
  ; *                                                                           *
  ; *                 (execute-vote) helpers                                    *
  ; *                                                                           *
  ; *****************************************************************************

  (defun vote-passed (hash:string idx:integer)
    (require-capability (PRIVATE))
    (let* ((vote-round (at 'dispute-ids (read vote-rounds hash)))
           (vote-id (at (- idx 1) vote-round)))
      (with-read vote-info vote-id { 'initiator := initiator , 'slashed-amount := slashed-amount , 'fee := fee }
        (if (= idx 1)
            (coin.transfer 'governance initiator (+ slashed-amount fee))
            (coin.transfer 'governance initiator fee))
      )
    )
    hash
  )

  (defun vote-invalid (hash:string idx:integer)
    (require-capability (PRIVATE))
    (let* ((vote-round (at 'dispute-ids (read vote-rounds hash)))
           (vote-id (at (- idx 1) vote-round)))
      (with-read vote-info vote-id { 'initiator := initiator , 'slashed-amount := slashed-amount , 'fee := fee }
        (coin.transfer 'governance initiator fee)
      )
    )
    hash
  )

  (defun vote-failed (num1:integer dispute-id:integer)
    (require-capability (PRIVATE))
    (with-read vote-info dispute-id { 'fee := fee }
      (+ num1 fee))
  )

  ; *****************************************************************************
  ; *                                                                           *
  ; *                  (begin-dispute) helpers                                  *
  ; *                                                                           *
  ; *****************************************************************************

  (defpact begin-dispute-pact (dispute-id:integer query-id:string timestamp:integer dispute-fee:integer)

    (step
      (require-capability (PRIVATE))
      (enforce (< (- (tellorflex.block-time-in-seconds) timestamp) (hours 12)) "Dispute must be started within reporting lock time"))

    (step
      (require-capability (PRIVATE))
      (with-default-read open-disputes-on-id query-id
        { 'open-disputes-on-id: 0 } { 'open-disputes-on-id := open-disputes-on-id }
        (update open-disputes-on-id query-id { 'open-disputes-on-id: (+ open-disputes-on-id 1)})))

    (step
      (require-capability (PRIVATE))
      (update dispute-info dispute-id
      { 'slashed-amount: (tellorflex.slash-reporter (at 'disputed-reporter (read dispute-info dispute-id)) 'governance)
      , 'value: (tellorflex.retrieve-data query-id timestamp)}))

    (step
      (require-capability (PRIVATE))
      (tellorflex.remove-value query-id timestamp))

    (step
      (require-capability (PRIVATE))
      (let ( (fee (* dispute-fee (^ 2 (- (at 'open-disputes-on-id (read open-disputes-on-id query-id)) 1)))))
        (update vote-info dispute-id { 'fee: fee })
        (coin.transfer (tellorflex.msg-sender) 'governance fee)))
  )

  (defpact else-begin-dispute-pact (block-time:integer dispute-id:integer dispute-fee:integer dispute-ids:[integer])
      (step
        (require-capability (PRIVATE))
        (let ( (prev-tally-date (at 'tally-date (read vote-info (- (length (at 'dispute-ids (read vote-rounds hash))) 2)))))
        (enforce (< (- block-time prev-tally-date) (days 1)) "New dispute round must be started within a day")))
      (step
        (require-capability (PRIVATE))
        (with-read dispute-info (at 0 (at 'dispute-ids (read vote-rounds hash)))
          { 'slashed-amount := slashed-amount , 'value := value }
          (update dispute-info dispute-id
            { 'slashed-amount: slashed-amount , 'value: value })))
      (step
        (require-capability (PRIVATE))
        (let ( (fee (* dispute-fee (^ 2 (- (length (at 'dispute-ids (read vote-rounds hash))) 1)))))
          (update vote-info dispute-id { 'fee: fee })
          (coin.transfer (tellorflex.msg-sender) 'governance fee)))

  )
  ; *****************************************************************************
  ; *                                                                           *
  ; *                         Getters                                           *
  ; *                                                                           *
  ; *****************************************************************************
  (defun get-dispute-fee ()
    (/ (free.tellorflex.get-stake-amount) 10)
  )
)
; *****************************************************************************
; *                                                                           *
; *                         Initialize                                        *
; *                                                                           *
; *****************************************************************************
; (if (read-msg "upgrade")
;   [
;   (create-table dispute-info)
;   (create-table tally)
;   (create-table vote-info)
;   (create-table vote-rounds)
;   (create-table open-disputes-on-id)
;   (create-table global)
;   ; (init-global)
;   ])
