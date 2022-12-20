; tellor governance on kadena
(namespace "free")

(if (read-msg "upgrade")
  "Upgrading contract"

  [
    (enforce-keyset (read-keyset "tellor-admin-keyset"))
    (define-keyset "free.tellor-admin-keyset" (read-keyset "tellor-admin-keyset"))
  ]
)


(module governance TELLOR-GOV
  (implements i-governance)

  (defcap TELLOR-GOV ()
    (enforce-guard (keyset-ref-guard "free.tellor-admin-keyset"))
  )

  (defcap PRIVATE () true )

  ; TODO:
  (defun capping:bool () (require-capability (PRIVATE)) )

  (defun create-guard:guard () (create-user-guard (capping)) )

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
    oracle:module{i-flex}
    token:module{fungible-v2}
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
  (defschema vote-tally-by-address-schema
    vote-tally-by-address:integer)

  (deftable global:{global-schema})
  (deftable dispute-info:{dispute-schema})
  (deftable open-disputes-on-id:{open-disputes-on-id-schema})
  (deftable vote-info:{vote-schema})
  (deftable vote-rounds:{vote-rounds-schema})
  (deftable vote-tally-by-address:{vote-tally-by-address-schema})
  (deftable dispute-ids-by-reporter:{dispute-ids-by-reporter-schema})


  (defun init-global (oracle:module{free.i-flex} token:module{fungible-v2})
    (insert global 'global-vars { 'oracle: oracle , 'token: token, 'team-multisig: "" , 'vote-count: 0 , 'autopay-query-id: ""})
  )

  (defun call-tellorflex ()
    (let ((tellorflex:module{i-flex} (at 'oracle (read global 'global-vars))))
      (with-capability (TELLOR-GOV)
        (tellorflex::init-gov-guard (create-guard))
      )
    )
  )

  (defun begin-dispute:string (account:string query-id:string timestamp:integer)
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
          , 'initiator: account
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
            (begin-dispute-pact account dispute-id query-id timestamp)
            (else-begin-dispute-pact account block-time dispute-id dispute-fee (at 'dispute-ids (read vote-rounds hash)))
            )
        (with-read global 'global-vars { 'vote-count := vote-count }
          (update global 'global-vars { 'vote-count: (+ vote-count 1)}))
        )
  )

  (defun execute-vote:string (dispute-id:integer)
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
              (open-disputes-count (at 'open-disputes-on-id (read open-disputes-on-id query-id)))
              (token:module{fungible-v2} (at 'token (read global 'global-vars))))

        (update open-disputes-on-id query-id { 'open-disputes-on-id: (- open-disputes-count 1) })

    (with-capability (PRIVATE)
      (if (= result 'PASSED)
          (fold (vote-passed) hash (enumerate (length (at 'dispute-ids (read vote-rounds hash))) 1))

          (if (= result 'INVALID)
              [
                (fold (vote-invalid) hash (enumerate (length (at 'dispute-ids (read vote-rounds hash))) 1))
                (with-read dispute-info dispute-id
                  { 'disputed-reporter := disputed-reporter , 'slashed-amount := slashed-amount }
                  (token::transfer 'governance disputed-reporter slashed-amount))
              ]
          )
              (if (= result 'FAILED)
                (token::transfer
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
  )

  (defun tally-votes (dispute-id:integer)
    (with-read vote-info dispute-id
      { 'tally-date := tally-date
      , 'vote-round := vote-round
      , 'start-date := start-date
      , 'token-holders := token-holders
      , 'reporters := reporters
      , 'users := users
      , 'team-multisig := team-multisig
      }
      (enforce (= tally-date 0) "Vote has already been tallied")
      (let ((vote-count (at 'vote-count (read global 'global-vars))))
      (enforce (and (<= dispute-id vote-count) (> dispute-id 0)) "Vote doesn't exist"))
      (enforce (or
        (>= (- (tellorflex.block-time-in-seconds) start-date) (* 86400 vote-round))
        (>= (- (tellorflex.block-time-in-seconds) start-date) (* 86400 6))) "Time for voting has not elapsed")
      (let* (
        (token-vote-sum

          (fold (+) 0
            [(at 'does-support token-holders) (at 'against token-holders) (at 'invalid-query token-holders)]))
        (reporters-vote-sum
          (fold (+) 0
            [(at 'does-support reporters) (at 'against reporters) (at 'invalid-query reporters)]))
        (multisig-vote-sum
          (fold (+) 0
            [(at 'does-support team-multisig) (at 'against team-multisig) (at 'invalid-query team-multisig)]))
        (users-vote-sum
          (fold (+) 0
            [(at 'does-support users) (at 'against users) (at 'invalid-query users)]))
        (token-vote-sum-updated (if (= token-vote-sum 0) (+ token-vote-sum 1)(+ token-vote-sum 0)))
        (reporters-vote-sum-updated (if (= reporters-vote-sum 0) (+ reporters-vote-sum 1)(+ reporters-vote-sum 0)))
        (multisig-vote-sum-updated (if (= multisig-vote-sum 0) (+ multisig-vote-sum 1)(+ multisig-vote-sum 0)))
        (users-vote-sum-updated (if (= users-vote-sum 0) (+ users-vote-sum 1)(+ users-vote-sum 0)))
        (scaled-does-support
          (fold (+) 0 (zip (calculate-vote-to-scale)
          [(at 'does-support token-holders) (at 'does-support reporters) (at 'does-support team-multisig) (at 'does-support users)]
          [token-vote-sum-updated reporters-vote-sum-updated multisig-vote-sum-updated users-vote-sum-updated])))
        (scaled-against
          (fold (+) 0 (zip (calculate-vote-to-scale)
            [(at 'against token-holders) (at 'against reporters) (at 'against team-multisig) (at 'against users)]
            [token-vote-sum-updated reporters-vote-sum-updated multisig-vote-sum-updated users-vote-sum-updated])))
        (scaled-invalid
          (fold (+) 0 (zip (calculate-vote-to-scale)
            [(at 'invalid-query token-holders) (at 'invalid-query reporters) (at 'invalid-query team-multisig) (at 'invalid-query users)]
            [token-vote-sum-updated reporters-vote-sum-updated multisig-vote-sum-updated users-vote-sum-updated]))))

          (if (> scaled-does-support (+ scaled-against scaled-invalid))
              (update vote-info dispute-id { 'result: "PASSED" })
              (if (> scaled-against (+ scaled-does-support scaled-invalid))
                  (update vote-info dispute-id { 'result: "FAILED" })
                  (update vote-info dispute-id { 'result: "INVALID" }))))

          (update vote-info dispute-id { 'tally-date: (tellorflex.block-time-in-seconds)}))
  )

  (defun vote (dispute-id:integer supports:bool invalid-query:bool voter-account:string)
    (enforce-keyset (read-keyset voter-account))
    (with-read vote-info dispute-id
      { 'tally-date := tally-date
      , 'voters := voters
      , 'token-holders := token-holders
      , 'reporters := reporters
      , 'users := users
      , 'team-multisig := team-multisig

      }
      ; TODO: catch errors for voter that doesn't exist as staker info
      (let ((vote-count (at 'vote-count (read global 'global-vars))))
        (enforce (and (<= dispute-id vote-count) (> dispute-id 0)) "Vote doesn't exist"))
      (enforce = tally-date 0 "Vote has already been tallied")
      (enforce (not (contains voter-account voters)) "Voter has already voted");check how efficient this is?
      (update vote-info dispute-id { 'voters : (+ voters [voter-account])})
      (let* ((token:module{fungible-v2} (at 'token (read global 'global-vars)))
             (team-multisig (at 'team-multisig (read global 'global-vars)))
             (voter-stake-info (tellorflex.get-staker-info voter-account))
             (voter-balance
               ; TODO: voter doesn't have to be staker could just holder
               (fold (+) 0 [
               (token::get-balance voter-account)
               (at 'staked-balance voter-stake-info)
               (at 'locked-balance voter-stake-info)])))
            (if invalid-query
              [
              (update vote-info dispute-id
                { 'token-holders: { 'does-support: (at 'does-support token-holders)
                                  , 'against: (at 'against token-holders)
                                  , 'invalid-query: (+ (at 'invalid-query token-holders) voter-balance)}
                , 'reporters: { 'does-support: (at 'does-support reporters)
                              , 'against: (at 'against reporters)
                              , 'invalid-query: (+ (at 'invalid-query reporters) (tellorflex.get-reports-submitted-by-address voter-account))}
                , 'users: { 'does-support: (at 'does-support users)
                              , 'against: (at 'against users)
                              , 'invalid-query: (+ (at 'invalid-query users) (get-user-tips voter-account))}})
              (if (= voter-account team-multisig)
                (update vote-info dispute-id
                  { 'team-multisig: { 'does-support: (at 'does-support team-multisig)
                                    , 'against: (at 'against team-multisig)
                                    , 'invalid-query: (+ (at 'invalid-query team-multisig) 1)}}) "")

              ]
              (if supports
                [
                (update vote-info dispute-id
                  { 'token-holders: { 'does-support: (+ (at 'does-support token-holders) voter-balance)
                                    , 'against: (at 'against token-holders)
                                    , 'invalid-query: (at 'invalid-query token-holders)}
                  , 'reporters: { 'does-support: (+ (at 'does-support reporters) (tellorflex.get-reports-submitted-by-address voter-account))
                                , 'against: (at 'against reporters)
                                , 'invalid-query: (at 'invalid-query reporters)}
                  , 'users: { 'does-support: (+ (at 'does-support users) (get-user-tips voter-account))
                            , 'against: (at 'against users)
                            , 'invalid-query: (at 'invalid-query users)}})
                (if (= voter-account team-multisig)
                  (update vote-info dispute-id
                    { 'team-multisig: { 'does-support: (+ (at 'does-support team-multisig) 1)
                                      , 'against: (at 'against team-multisig)
                                      , 'invalid-query: (at 'invalid-query team-multisig)}}) "")
                ]
                )
                [
                (update vote-info dispute-id
                  { 'token-holders: { 'does-support: (at 'does-support token-holders)
                                    , 'against: (+ (at 'against token-holders) voter-balance)
                                    , 'invalid-query: (at 'invalid-query token-holders)}
                  , 'reporters: { 'does-support: (at 'does-support reporters)
                                , 'against: (+ (at 'against reporters) (tellorflex.get-reports-submitted-by-address voter-account))
                                , 'invalid-query: (at 'invalid-query reporters)}
                  , 'users: { 'does-support: (at 'does-support users)
                            , 'against: (+ (at 'against users) (get-user-tips voter-account))
                            , 'invalid-query: (at 'invalid-query users)}})
                (if (= voter-account team-multisig)
                  (update vote-info dispute-id
                    { 'team-multisig: { 'does-support: (at 'does-support team-multisig)
                                      , 'against: (+ (at 'against team-multisig) 1)
                                      , 'invalid-query: (at 'invalid-query team-multisig)}}) "")
                ]))
                (with-default-read vote-tally-by-address voter-account
                  { 'vote-tally-by-address: 0 }{ 'vote-tally-by-address := vote-tally-by-address}
                  (update vote-tally-by-address voter-account { 'vote-tally-by-address: (+ vote-tally-by-address 1)})))
  )
  ; *****************************************************************************
  ; *                                                                           *
  ; *                  (begin-dispute) helpers                                  *
  ; *                                                                           *
  ; *****************************************************************************
  (defpact begin-dispute-pact (account:string dispute-id:integer query-id:string timestamp:integer dispute-fee:integer)

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
      (let ( (fee (* dispute-fee (^ 2 (- (at 'open-disputes-on-id (read open-disputes-on-id query-id)) 1))))
             (token:module{fungible-v2} (at 'token (read global 'global-vars))))
        (update vote-info dispute-id { 'fee: fee })
        (token::transfer-create account 'governance (create-guard) fee)))
  )

  (defpact else-begin-dispute-pact (account:string block-time:integer dispute-id:integer dispute-fee:integer dispute-ids:[integer])
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
        (let ( (fee (* dispute-fee (^ 2 (- (length (at 'dispute-ids (read vote-rounds hash))) 1))))
               (token:module{fungible-v2} (at 'token (read global 'global-vars))))
          (update vote-info dispute-id { 'fee: fee })
          (token::transfer-create account 'governance (create-guard) fee)))
  )
  ; *****************************************************************************
  ; *                                                                           *
  ; *                 (execute-vote) helpers                                    *
  ; *                                                                           *
  ; *****************************************************************************

  (defun vote-passed (hash:string idx:integer)
    (require-capability (PRIVATE))
    (let* ((vote-round (at 'dispute-ids (read vote-rounds hash)))
           (vote-id (at (- idx 1) vote-round))
           (token:module{fungible-v2} (at 'token (read global 'global-vars))))
      (with-read vote-info vote-id { 'initiator := initiator , 'slashed-amount := slashed-amount , 'fee := fee }
        (if (= idx 1)
            (token::transfer 'governance initiator (+ slashed-amount fee))
            (token::transfer 'governance initiator fee))
      )
    )
    hash
  )

  (defun vote-invalid (hash:string idx:integer)
    (require-capability (PRIVATE))
    (let* ((vote-round (at 'dispute-ids (read vote-rounds hash)))
           (vote-id (at (- idx 1) vote-round))
           (token:module{fungible-v2} (at 'token (read global 'global-vars))))
      (with-read vote-info vote-id { 'initiator := initiator , 'slashed-amount := slashed-amount , 'fee := fee }
        (token::transfer 'governance initiator fee)
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
  ; *                 (vote) helpers                                            *
  ; *                                                                           *
  ; *****************************************************************************

  (defun calculate-vote-to-scale (votes vote-sum)
    ( / (* votes (^ 10 18)) vote-sum)
  )
  ; *****************************************************************************
  ; *                                                                           *
  ; *                         Getters                                           *
  ; *                                                                           *
  ; *****************************************************************************
  (defun did-vote:bool (dispute-id:integer voter:string)
    @doc "Determines if an address voted for a specific vote"
    (contains voter (at 'voters (read vote-info dispute-id)))
  )

  (defun get-dispute-fee:integer ()
    @doc "Get the latest dispute fee"
    (/ (free.tellorflex.stake-amount) 10)
  )

  (defun get-disputes-by-reporter:[integer] (reporter:string)
    @doc "Get dispute ids for a reporter"
    (read dispute-ids-by-reporter reporter [ 'dispute-ids ])
  )

  (defun get-dispute-info:object{dispute-schema} (dispute-id:integer)
    @doc "Returns info on a dispute for a given ID"
    (read dispute-info dispute-id)
  )

  (defun get-open-disputes-on-id:integer (query-id:string)
    @doc "Returns the number of open disputes for a specific query ID"
    (read open-disputes-on-id query-id [ 'open-disputes-on-id ])
  )

  (defun get-vote-count:integer ()
    @doc "Returns the total number of votes"
    (at 'vote-count (read global 'global-vars))
  )

  (defun getVoteInfo:object{vote-schema} (dispute-id:integer)
    @doc "Returns info on a vote for a given vote ID"
    (read vote-info dispute-id)
  )

  (defun get-vote-rounds:[integer] (hash:string)
    @doc "Returns an array of voting rounds for a given vote"
    (read vote-rounds hash)
  )

  (defun get-vote-tally-by-address:integer (voter:string)
    @doc "Returns the total number of votes cast by an address"
    (try 0 (read vote-tally-by-address voter [ 'vote-tally-by-address ]))
  )
  (defun get-user-tips:integer (user:string)
  ; requires autopay
    ; (require-capability (PRIVATE))
    (let* ((data-before (tellorflex.get-data-before (at 'autopay-query-id (read global 'global-vars)) (- (tellorflex.block-time-in-seconds) (hours 12))))
           (timestamp (at 'timestamp data-before))
           (value (at 'value data-before)))
           0)

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
