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
    oracle:module{free.i-flex}
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

  (defconst TWELVE_HOURS 43200)
  (defconst ONE_DAY 86400)
  (defconst GOV_ACCOUNT 'governance)

  (defun init-global (oracle:module{free.i-flex} token:module{fungible-v2})
    (insert global 'global-vars
      { 'oracle: oracle
      , 'token: token
      , 'team-multisig: ""
      , 'vote-count: 0
      , 'autopay-query-id: ""})

    (token::create-account 'governance (create-guard))
    "Global variables set!"
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
    (enforce-keyset (read-keyset account))
    (let* ((tellorflex:module{free.i-flex} (at 'oracle (read global 'global-vars)))
           (block-number (tellorflex::get-block-number-by-timestamp query-id timestamp))
           (hash (hash [query-id timestamp]))
           (dispute-id (+ (at 'vote-count (read global 'global-vars)) 1))
           (dispute-fee (get-dispute-fee))
           (block-time (block-time))
           (disputed-reporter (tellorflex::get-reporter-by-timestamp query-id timestamp)))
        (enforce (!= block-number 0) (format "No value exists at given timestamp {}"[block-number]))
        (with-default-read vote-rounds hash
          { 'dispute-ids: []} { 'dispute-ids := dispute-ids }
          (let ((disputes (+ 1 (length dispute-ids))))
          (with-capability (PRIVATE)
            (if (= disputes 1)
              (with-default-read open-disputes-on-id query-id
                { 'open-disputes-on-id: 0 } { 'open-disputes-on-id := open-disputes }
                (enforce (< (- block-time timestamp) TWELVE_HOURS)
                  "Dispute must be started within reporting lock time")
                (let ((fee (* dispute-fee (^ 2 open-disputes)))
                      (token:module{fungible-v2} (token)))
                    (token::transfer account GOV_ACCOUNT (float fee))
                    (insert dispute-info (str dispute-id)
                      { 'query-id: query-id
                      , 'timestamp: timestamp
                      , 'value: (tellorflex::retrieve-data query-id timestamp)
                      , 'disputed-reporter: disputed-reporter
                      , 'slashed-amount:
                        (tellorflex::slash-reporter disputed-reporter 'governance)
                      })
                    (tellorflex::remove-value query-id timestamp)
                    (insert vote-info (str dispute-id)
                      { 'identifier-hash: hash
                      , 'vote-round: disputes
                      , 'start-date: block-time
                      , 'block-number: (at 'block-height (chain-data))
                      , 'fee: fee
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
                    (write open-disputes-on-id query-id
                      { 'open-disputes-on-id: (+ open-disputes 1)})
                  )
                )
                (let ((prev-tally-date (at 'tally-date (read vote-info (str (- disputes 1)))))
                      (fee (* dispute-fee (^ 2 (- dispute-id 1))))
                      (token:module{fungible-v2} (token)))
                    (enforce (< (- block-time prev-tally-date) ONE_DAY)
                      "New dispute round must be started within a day")
                    (token::transfer account GOV_ACCOUNT (float fee))
                    (insert vote-info (str dispute-id)
                      { 'identifier-hash: hash
                      , 'vote-round: disputes
                      , 'start-date: block-time
                      , 'block-number: (at 'block-height (chain-data))
                      , 'fee: fee
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
                    (with-read dispute-info (str (at 0 dispute-ids))
                      { 'slashed-amount := slashed-amount , 'value := value }
                      (insert dispute-info (str dispute-id)
                        { 'query-id: query-id
                        , 'timestamp: timestamp
                        , 'value: value
                        , 'disputed-reporter: disputed-reporter
                        , 'slashed-amount: slashed-amount
                        }))
                )
              )
            )
            (write vote-rounds hash { 'dispute-ids: (+ dispute-ids [dispute-id])})
            (with-default-read dispute-ids-by-reporter disputed-reporter
              { 'dispute-ids: [] }{ 'dispute-ids := dispute-ids }
              (write dispute-ids-by-reporter disputed-reporter
                { 'dispute-ids: (+ dispute-ids [dispute-id])}))

            (with-read global 'global-vars { 'vote-count := vote-count }
              (update global 'global-vars { 'vote-count: (+ vote-count 1)}))
          )
        )
      )
  )

  (defun execute-vote:string (dispute-id:integer)
    (let ((vote-count (at 'vote-count (read global 'global-vars))))
      (enforce (and (<= dispute-id vote-count) (> dispute-id 0))
        "Dispute ID must be valid"))
    (with-read vote-info (str dispute-id)
      { 'executed := executed
      , 'tally-date := tally-date
      , 'vote-round := vote-round
      , 'identifier-hash := identifier-hash
      , 'result := result
      }
      (enforce (not executed) "Vote has already been executed")
      (enforce (> tally-date 0) "Vote must be tallied")
      (with-read vote-rounds identifier-hash { 'dispute-ids := dispute-ids }
        (enforce (= (length dispute-ids) vote-round) "Must be the final vote")
        (enforce (>= (- (block-time) tally-date) ONE_DAY)
          "1 day has to pass after tally to allow for disputes")
        (update vote-info (str dispute-id) { 'executed: true })

      (let* ((query-id (at 'query-id (read dispute-info (str dispute-id))))
             (open-disputes-count
               (at 'open-disputes-on-id (read open-disputes-on-id query-id)))
             (token:module{fungible-v2} (at 'token (read global 'global-vars))))
          (with-capability (PRIVATE)
            (if (= result 'PASSED)
                (fold
                  (vote-passed) identifier-hash
                  (enumerate 1 (length dispute-ids))
                )
                (if (= result 'INVALID)
                    (with-read dispute-info (str dispute-id)
                      { 'disputed-reporter := disputed-reporter
                      , 'slashed-amount := slashed-amount }
                    (fold
                      (vote-invalid) identifier-hash
                      (enumerate (length dispute-ids) 1))
                    (token::transfer GOV_ACCOUNT disputed-reporter slashed-amount)
                    )

                    (if (= result 'FAILED)
                      (let ((reporter (at 'disputed-reporter (read dispute-info (str dispute-id))))
                        (amount
                          (float (+
                    (fold (vote-failed) 0 dispute-ids)
                    (at 'slashed-amount (read dispute-info (str dispute-id)))))))

                    (install-capability
                      (token::TRANSFER GOV_ACCOUNT reporter amount))
                    (token::transfer GOV_ACCOUNT reporter amount)
                  )
              "Invalid result"
              )
          )
        )
      )
    (update open-disputes-on-id query-id
      { 'open-disputes-on-id: (- open-disputes-count 1) })
     )
    )
   )
  )

  (defun tally-votes (dispute-id:integer)
    (with-read vote-info (str dispute-id)
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
        (>= (- (block-time) start-date) (* 86400 vote-round))
        (>= (- (block-time) start-date) (* 86400 6))) "Time for voting has not elapsed")
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
              (update vote-info (str dispute-id) { 'result: "PASSED" })
              (if (> scaled-against (+ scaled-does-support scaled-invalid))
                  (update vote-info (str dispute-id){ 'result: "FAILED" })
                  (update vote-info (str dispute-id) { 'result: "INVALID" }))))

          (update vote-info (str dispute-id) { 'tally-date: (block-time)}))
  )

  (defun vote (dispute-id:integer supports:bool invalid:bool voter-account:string)
    (enforce-keyset (read-keyset voter-account))
    (with-read vote-info (str dispute-id)
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
      (enforce (= tally-date 0) "Vote has already been tallied")
      (enforce (not (contains voter-account voters)) "Voter has already voted");check how efficient this is?
      (update vote-info (str dispute-id) { 'voters : (+ voters [voter-account])})

      (let* ((tellorflex:module{free.i-flex} (at 'oracle (read global 'global-vars)))
             (token:module{fungible-v2} (at 'token (read global 'global-vars)))
             (team-multisig (at 'team-multisig (read global 'global-vars)))
             (voter-stake-info (try {} (tellorflex::get-staker-info voter-account)))
             (reports-submitted (try 0
               (tellorflex::get-reports-submitted-by-address voter-account)))
             (voter-balance
               ; TODO: voter doesn't have to be staker could just holder
               (if (= {} voter-stake-info) (round (* (token::get-balance voter-account) (^ 10 18)))
                   (fold (+) 0 [
                   (round (* (token::get-balance voter-account) (^ 10 18)))
                   (at 'staked-balance voter-stake-info)
                   (at 'locked-balance voter-stake-info)]))))
            (if invalid
              (let ((update-voteinfo
                (update vote-info (str dispute-id)
                  { 'token-holders:
                    { 'does-support: (at 'does-support token-holders)
                    , 'against: (at 'against token-holders)
                    , 'invalid-query: (+
                                        (at 'invalid-query token-holders)
                                        voter-balance)
                    }
                  , 'reporters:
                    { 'does-support: (at 'does-support reporters)
                    , 'against: (at 'against reporters)
                    , 'invalid-query: (+
                                        (at 'invalid-query reporters)
                                        reports-submitted
                                      )
                    }
                , 'users:
                  { 'does-support: (at 'does-support users)
                  , 'against: (at 'against users)
                  , 'invalid-query: (+
                                      (at 'invalid-query users)
                                      (get-user-tips voter-account)
                                    )
                  }
                })))
                update-voteinfo
                (if (= voter-account team-multisig)
                  (update vote-info (str dispute-id)
                    { 'team-multisig:
                      { 'does-support: (at 'does-support team-multisig)
                      , 'against: (at 'against team-multisig)
                      , 'invalid-query: (+ (at 'invalid-query team-multisig) 1)}})
                  "not multisig")
              )
              (if supports
                (let ((update-voteinfo
                (update vote-info (str dispute-id)
                  { 'token-holders:
                    { 'does-support: (+
                                        (at 'does-support token-holders)
                                        voter-balance
                                     )
                    , 'against: (at 'against token-holders)
                    , 'invalid-query: (at 'invalid-query token-holders)
                    }
                  , 'reporters:
                    { 'does-support: (+
                                        (at 'does-support reporters)
                                        reports-submitted
                                     )
                    , 'against: (at 'against reporters)
                    , 'invalid-query: (at 'invalid-query reporters)}
                  , 'users:
                    { 'does-support: (+
                                        (at 'does-support users)
                                        (get-user-tips voter-account)
                                     )
                    , 'against: (at 'against users)
                    , 'invalid-query: (at 'invalid-query users)
                    }
                  }
                )))

                  update-voteinfo
                  (if (= voter-account team-multisig)
                      (update vote-info (str dispute-id)
                        { 'team-multisig:
                          { 'does-support: (+ (at 'does-support team-multisig) 1)
                          , 'against: (at 'against team-multisig)
                          , 'invalid-query: (at 'invalid-query token-holders)
                          }
                        }
                      )
                      "not multisig"
                   )
                )
                (let ((update-voteinfo
                  (update vote-info (str dispute-id)
                    { 'token-holders:
                      { 'does-support: (at 'does-support token-holders)
                      , 'against: (+ (at 'against token-holders) voter-balance)
                      , 'invalid-query: (at 'invalid-query token-holders)
                      }
                    , 'reporters:
                      { 'does-support: (at 'does-support reporters)
                      , 'against: (+
                                    (at 'against reporters)
                                    reports-submitted
                                  )
                      , 'invalid-query: (at 'invalid-query reporters)
                      }
                    , 'users:
                      { 'does-support: (at 'does-support users)
                      , 'against: (+
                                    (at 'against users)
                                    (get-user-tips voter-account)
                                  )
                      , 'invalid-query: (at 'invalid-query users)
                      }
                  })))

                  update-voteinfo
                  (if (= voter-account team-multisig)
                      (update vote-info (str dispute-id)
                        { 'team-multisig:
                          { 'does-support: (at 'does-support team-multisig)
                          , 'against: (+ (at 'against team-multisig) 1)
                          , 'invalid-query: (at 'invalid-query token-holders)
                          }
                        }
                      )
                      "not multisig"
                   )
                )

              )
             )
      )
      (with-default-read vote-tally-by-address voter-account
        { 'vote-tally-by-address: 0 } { 'vote-tally-by-address := vote-tally }
        (write vote-tally-by-address voter-account
          { 'vote-tally-by-address: (+ vote-tally 1) })) ;token-holders
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
      (with-read vote-info (str vote-id)
        { 'initiator := initiator , 'fee := fee }
        (if (= idx 1)
          (let* ((slashed-amount (at 'slashed-amount (read dispute-info (str vote-id)))))
                (transfer initiator (float (+ slashed-amount fee))))
          (transfer initiator (float fee))
      )
    )
    hash
  )
)
(defun transfer (account amount)
  (require-capability (PRIVATE))
  (let ((token:module{fungible-v2} (at 'token (read global 'global-vars))))
    (install-capability
      (token::TRANSFER GOV_ACCOUNT account amount))
      (token::transfer GOV_ACCOUNT account amount))

)
  (defun vote-invalid (hash:string idx:integer)
    (require-capability (PRIVATE))
    (let* ((vote-round (at 'dispute-ids (read vote-rounds hash)))
           (vote-id (at (- idx 1) vote-round))
           (token:module{fungible-v2} (at 'token (read global 'global-vars))))
      (with-read vote-info (str vote-id)
        { 'initiator := initiator , 'fee := fee }
        (install-capability
          (token::TRANSFER GOV_ACCOUNT initiator (float fee)))
        (token::transfer GOV_ACCOUNT initiator (float fee))
      )
    )
    hash
  )

  (defun vote-failed (num1:integer dispute-id:integer)
    (require-capability (PRIVATE))
    (with-read vote-info (str dispute-id) { 'fee := fee }
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
    (let ((tellorflex:module{free.i-flex} (at 'oracle (read global 'global-vars))))
      (/ (tellorflex::stake-amount) 10))
  )

  (defun get-disputes-by-reporter:[integer] (reporter:string)
    @doc "Get dispute ids for a reporter"
    (read dispute-ids-by-reporter reporter [ 'dispute-ids ])
  )

  (defun get-dispute-info:object{dispute-schema} (dispute-id:integer)
    @doc "Returns info on a dispute for a given ID"
    (read dispute-info (str dispute-id))
  )

  (defun get-open-disputes-on-id:integer (query-id:string)
    @doc "Returns the number of open disputes for a specific query ID"
    (read open-disputes-on-id query-id [ 'open-disputes-on-id ])
  )

  (defun get-vote-count:integer ()
    @doc "Returns the total number of votes"
    (at 'vote-count (read global 'global-vars))
  )

  (defun get-vote-info:object{vote-schema} (dispute-id:integer)
    @doc "Returns info on a vote for a given vote ID"
    (read vote-info (str dispute-id))
  )

  (defun get-vote-rounds:[integer] (hash:string)
    @doc "Returns an array of voting rounds for a given vote"
    (read vote-rounds hash)
  )

  (defun get-vote-tally-by-address:integer (voter:string)
    @doc "Returns the total number of votes cast by an address"
    (try 0 (read vote-tally-by-address voter [ 'vote-tally-by-address ]))
  )

  (defun str (num:integer) (int-to-str 10 num))

  (defun float (num:integer) (/ (/ num 1.0) (^ 10 18)))

  (defun block-time ()
    (str-to-int 10
      (format-time "%s" (at 'block-time (chain-data))))
  )

  (defun token () (at 'token (read global 'global-vars)) )

  (defun get-user-tips:integer (user:string) 0 )
)
