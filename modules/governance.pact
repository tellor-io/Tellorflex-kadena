; tellor governance on kadena
; read namespace from deploy msg
(namespace (read-msg "ns"))

(module governance TELLOR
  (implements i-governance)
; ***************************CAPABILITIES**************************************
  (defcap TELLOR:bool ()
    (enforce-guard 
      (keyset-ref-guard (+ (read-msg "ns") ".admin-keyset")))
  )
  (defcap PRIVATE:bool () 
    true
  )
; ***************************EVENT-CAPS****************************************
  (defcap NewDispute:bool
    ( dispute-id:integer 
      query-id:string 
      timestamp:integer
      reporter:string )
      @event true
  )
  (defcap Voted:bool
    ( dispute-id:integer
      supports:bool
      voter:string
      invalid-query:bool )
      @event true
  )
  (defcap VoteExecuted:bool
    ( dispute-id:integer
      result:string )
      @event true
  )
  (defcap VoteTallied:bool
    ( dispute-id:integer
      result:string
      initiator:string
      reporter:string )
      @event true
  )
; ***************************GUARD*********************************************
  (defun gov-guard:guard ()
    (create-capability-guard (PRIVATE))
  )
; ***************************SCHEMA********************************************
  (defschema dispute-ids-by-reporter-schema
    dispute-ids:[integer])
  (defschema dispute-schema
    query-id:string
    timestamp:integer
    value:string
    disputed-reporter:string
    slashed-amount:integer)
  (defschema global-schema
    team-multisig:string
    gov-account-name:string
    vote-count:integer)
  (defschema open-disputes-on-id-schema
    open-disputes-on-id:integer)
  (defschema results-schema
    does-support:integer
    against:integer
    invalid-query:integer)
  (defschema vote-rounds-schema
    dispute-ids:[integer])
  (defschema vote-schema
    identifier-hash:string
    vote-round:integer
    start-date:integer
    block-number:integer
    fee:integer
    tally-date:integer
    token-holders:object{results-schema}
    users:object{results-schema}
    reporters:object{results-schema}
    team-multisig:object{results-schema}
    executed:bool
    result:string
    initiator:string
    voters:[string])
  (defschema vote-tally-by-address-schema
    vote-tally-by-address:integer)
; ***************************TABLES********************************************
  (deftable dispute-ids-by-reporter:{dispute-ids-by-reporter-schema})
  (deftable dispute-info:{dispute-schema})
  (deftable global:{global-schema})
  (deftable open-disputes-on-id:{open-disputes-on-id-schema})
  (deftable vote-info:{vote-schema})
  (deftable vote-rounds:{vote-rounds-schema})
  (deftable vote-tally-by-address:{vote-tally-by-address-schema})
; ***************************CONSTANTS*****************************************
  (defconst TWELVE_HOURS:integer 43200)
  (defconst ONE_DAY:integer 86400)
  (defconst GOV_ACCOUNT (create-principal (gov-guard)))
; ***************************MAIN-FUNCTIONS************************************
  (defun constructor:string (team-multisig:string gov-account-name:string)
    (with-capability (TELLOR)
      (insert global "global-vars"
        { "team-multisig": team-multisig, "gov-account-name": GOV_ACCOUNT, "vote-count": 0 })

      (f-TRB.create-account GOV_ACCOUNT (gov-guard))

      "Global variables set!"
    )
  )

  (defun register-gov-guard:guard ()
    @doc "Registers the gov guard with the oracle"
      (with-capability (TELLOR) (gov-guard))
  )

  (defun begin-dispute:bool 
    (account:string query-id:string timestamp:integer)
    @doc "Begins a dispute for a given query id and timestamp"
    (let* ( (block-number 
              (tellorflex.get-block-number-by-timestamp query-id timestamp))
            (stake-amount (tellorflex.stake-amount))
            (hash (hash [query-id timestamp]))
            (dispute-id (+ (vote-count) 1))
            (dispute-fee (get-dispute-fee))
            (block-time (block-time))
            (disputed-reporter 
              (tellorflex.get-reporter-by-timestamp query-id timestamp)))
      (enforce (!= block-number 0)
        (format "No value exists at given timestamp {}"[block-number]))
      ;  reads dispute ids list from vote-rounds table
      ;  checking for existing disputes for the given query id and timestamp
      (with-default-read vote-rounds hash
        { "dispute-ids": [] }{ "dispute-ids" := dispute-ids }
        (let ((vote-round (+ 1 (length dispute-ids))))
          (with-capability (PRIVATE)
          ;  if no disputes exist for this report then create a new vote round
            (if (= vote-round 1)
            ;  read open disputes on id to calculate fee
                (with-default-read open-disputes-on-id query-id
                  { "open-disputes-on-id": 0 } 
                  { "open-disputes-on-id" := open-disputes }
                  (enforce (< (- block-time timestamp) TWELVE_HOURS)
                    "Dispute must be started within reporting lock time")
                  (let* ( (calc-fee (* dispute-fee (^ 2 open-disputes)))
                          (fee 
                            (if (> calc-fee stake-amount) 
                                stake-amount calc-fee )) )
                    ;  transfer fee to gov account
                    (f-TRB.transfer account (gov-account) (float fee))
                    ;  insert dispute info into dispute-info table
                    (insert dispute-info (str dispute-id)
                      { "query-id": query-id
                      , "timestamp": timestamp
                      , "value": (tellorflex.retrieve-data query-id timestamp)
                      , "disputed-reporter": disputed-reporter
                      , "slashed-amount":
                        ; slash reporter and transfer stake to gov account
                        (tellorflex.slash-reporter 
                          disputed-reporter (gov-account)) 
                      })
                    ;  set report value to empty string in oracle
                    (tellorflex.remove-value query-id timestamp)
                    ;  insert vote info into vote-info table
                    (insert-vote-info (str dispute-id) 
                      hash vote-round block-time fee account)
                    ; increment open disputes on id by 1
                    (write open-disputes-on-id query-id
                      { "open-disputes-on-id": (+ open-disputes 1)})
                  )
                )
                ; if a dispute already exists for this report then create a new dispute round
                (let* ( (prev-id (at (- vote-round 2) dispute-ids))
                        (prev-tally-date (at "tally-date" (get-vote-info prev-id)))
                        (calc-fee (* dispute-fee (^ 2 (length dispute-ids))))
                        (fee (if (> calc-fee stake-amount) stake-amount calc-fee)) )
                  (enforce (< (- block-time prev-tally-date) ONE_DAY)
                    "New dispute round must be started within a day")
                  ;  transfer fee to gov account
                  (f-TRB.transfer account (gov-account) (float fee))
                  ;  insert vote info into vote-info table
                  (insert-vote-info (str dispute-id) 
                    hash vote-round block-time fee account)
                  ; read slashed amount and value from first round dispute
                  (with-read dispute-info (str (at 0 dispute-ids))
                    { "slashed-amount" := slashed-amount , "value" := value }
                    (insert dispute-info (str dispute-id)
                      { "query-id": query-id
                      , "timestamp": timestamp
                      , "value": value
                      , "disputed-reporter": disputed-reporter
                      , "slashed-amount": slashed-amount })
                  )
                )
              )
            )
            ;  add this round's dispute id to list of dispute ids for this report
            (write vote-rounds hash 
              { "dispute-ids": (+ dispute-ids [dispute-id]) })
            ;  add this dispute id to list of dispute ids for disputed reporter
            (with-default-read dispute-ids-by-reporter disputed-reporter
              { "dispute-ids": [] }{ "dispute-ids" := dispute-ids }
              (write dispute-ids-by-reporter disputed-reporter
                { "dispute-ids": (+ dispute-ids [dispute-id]) }))
            ;  increment global vote count
            (with-read global "global-vars" { "vote-count" := vote-count }
              (update global "global-vars" { "vote-count": (+ vote-count 1) }))
        )
      )
      (emit-event 
        (NewDispute dispute-id query-id timestamp disputed-reporter))
    )
  )

  (defun execute-vote:bool (dispute-id:integer)
    @doc "Execute vote for a given dispute ID, \
    \ finalizing the vote and transferring the fees and stake"
    (let ((vote-count (at "vote-count" (read global "global-vars"))))
      (enforce (and (<= dispute-id vote-count) (> dispute-id 0))
        "Dispute ID must be valid"))
    (with-read vote-info (str dispute-id)
      { "executed" := executed, "tally-date" := tally-date
      , "vote-round" := vote-round, "identifier-hash" := identifier-hash
      , "result" := result }
      (enforce (not executed) "Vote has already been executed")
      (enforce (> tally-date 0) "Vote must be tallied")
      (with-read vote-rounds identifier-hash { "dispute-ids" := dispute-ids }
        (enforce (= (length dispute-ids) vote-round) 
        "Must be the final vote round")
        (enforce (>= (- (block-time) tally-date) ONE_DAY)
          "1 day has to pass after tally to allow for disputes")
        ;  update vote info to show that vote has been executed
        (update vote-info (str dispute-id) { 'executed: true })

        (with-capability (PRIVATE)
        ; transfer stake and fees to appropriate parties given the result of the vote
          (with-read dispute-info (str dispute-id)
            { "disputed-reporter" := disputed-reporter
            , "slashed-amount" := slashed-amount, "query-id" := query-id }
            (if (= result "PASSED") 
                (fold (vote-passed) identifier-hash
                (enumerate 1 (length dispute-ids)))
              (if (= result "INVALID")
                (let ((reporter-amount (float slashed-amount)))
                  (fold (vote-invalid) identifier-hash
                  (enumerate (length dispute-ids) 1) )
                  ; transfer slashed amount back to disputed reporter
                  (transfer-from-gov disputed-reporter reporter-amount))
              (if (= result "FAILED")
                (let* ( (total-fees (fold (vote-failed) 0 dispute-ids))
                        (transfer-total 
                          (float (+ slashed-amount total-fees))) )
                  (transfer-from-gov disputed-reporter transfer-total))
              "Invalid result value")))
            ;  decrement open disputes count for this query
            (update open-disputes-on-id query-id
              { "open-disputes-on-id": (- (open-disputes-count query-id) 1)}) ) )
      )
      (emit-event (VoteExecuted dispute-id result))
    )
  )

  (defun tally-votes:bool (dispute-id:integer)
    (with-read vote-info (str dispute-id)
      { "tally-date" := tally-date, "vote-round" := vote-round
      , "start-date" := start-date, "token-holders" := token-holders
      , "reporters" := reporters, "users" := users
      , "team-multisig" := team-multisig, "initiator" := initiator }

      (enforce (= tally-date 0) "Vote has already been tallied")
      (let ((vote-count (at "vote-count" (read global "global-vars"))))
        (enforce (and (<= dispute-id vote-count) (> dispute-id 0)) 
          "Vote doesn't exist"))
      (enforce (or
        (>= (- (block-time) start-date) (* 86400 vote-round))
        (>= (- (block-time) start-date) (* 86400 6))) 
        "Time for voting has not elapsed")

      (with-capability (PRIVATE)
        (let* ( (support (lambda (obj) (at "does-support" obj)))
                (against (lambda (obj) (at "against" obj)))
                (invalid (lambda (obj) (at "invalid-query" obj)))
                (sum (lambda (obj) (bind obj
                        { "does-support" := support, "against" := against
                        , "invalid-query" := invalid } 
                        (fold (+) support [against invalid]))))
                (token-vote-sum (sum token-holders))
                (reporters-vote-sum (sum reporters))
                (multisig-vote-sum (sum team-multisig))
                (users-vote-sum (sum users))
                (token-vote-sum-updated
                  (if (= token-vote-sum 0) (+ token-vote-sum 1) token-vote-sum ))
                (reporters-vote-sum-updated 
                  (if (= reporters-vote-sum 0)
                      (+ reporters-vote-sum 1) reporters-vote-sum ))
                (multisig-vote-sum-updated 
                  (if (= multisig-vote-sum 0) 
                      (+ multisig-vote-sum 1) multisig-vote-sum ))
                (users-vote-sum-updated 
                  (if (= users-vote-sum 0) (+ users-vote-sum 1) users-vote-sum))
                (scaled-does-support
                  (fold (+) 0 (zip (calculate-vote-to-scale)
                  [(support token-holders) (support reporters)
                    (support team-multisig) (support users)]
                  [token-vote-sum-updated reporters-vote-sum-updated 
                    multisig-vote-sum-updated users-vote-sum-updated])))
                (scaled-against
                  (fold (+) 0 (zip (calculate-vote-to-scale)
                    [(against token-holders) (against reporters)
                      (against team-multisig) (against users)]
                    [token-vote-sum-updated reporters-vote-sum-updated 
                      multisig-vote-sum-updated users-vote-sum-updated])))
                (scaled-invalid
                  (fold (+) 0 (zip (calculate-vote-to-scale)
                    [(invalid token-holders) (invalid reporters)
                      (invalid team-multisig) (invalid users)]
                    [token-vote-sum-updated reporters-vote-sum-updated 
                      multisig-vote-sum-updated users-vote-sum-updated]))) )

            (let ((reporter 
              (at "disputed-reporter" (get-dispute-info dispute-id)))
                  (result 
                    (if 
                      (> scaled-does-support (+ scaled-against scaled-invalid))
                      "PASSED"
                      (if (> scaled-against (+ scaled-does-support scaled-invalid))
                      "FAILED"
                      "INVALID") )) )
              (update vote-info (str dispute-id) 
              { "result": result, "tally-date": (block-time)})
              (emit-event (VoteTallied dispute-id result initiator reporter)) )
        )
      )
    )
  )

  (defun vote:bool
    (dispute-id:integer supports:bool invalid:bool voter-account:string)
    (with-read vote-info (str dispute-id)
      { "tally-date" := tally-date, "voters" := voters
      , "token-holders" := token-holders, "reporters" := reporters
      , "users" := users, "team-multisig" := team-multisig }

      (bind (f-TRB.details voter-account) { "guard" := g, "balance" := bal } 
        (enforce-guard g)

      (let ((vote-count (at "vote-count" (read global "global-vars"))))
        (enforce (and (<= dispute-id vote-count) (> dispute-id 0))
        "Vote doesn't exist"))
      (enforce (= tally-date 0) "Vote has already been tallied")
      (enforce (not (contains voter-account voters)) "Voter has already voted")
      (update vote-info (str dispute-id) {"voters": (+ voters [voter-account])})

      (let* ( (multisig (at "team-multisig" (read global "global-vars")))
              (voter-stake-info 
                (try {} (tellorflex.get-staker-info voter-account)))
              (reports-submitted (try 0
                (tellorflex.get-reports-submitted-by-address voter-account)))
              (voter-balance
                (if (= {} voter-stake-info) (round (* bal (^ 10 18)))
                  (fold (+) (round (* bal (^ 10 18))) 
                  [(at "staked-balance" voter-stake-info)
                    (at "locked-balance" voter-stake-info)]))) )
        (with-capability (PRIVATE)
          (if invalid
            (let ((holder-invalid-bal 
                    (+ (at "invalid-query" token-holders) voter-balance))
                  (reporter-invalid-bal 
                    (+ (at "invalid-query" reporters) reports-submitted))
                  (user-invalid-bal 
                    (+ (at "invalid-query" users) (get-user-tips voter-account))))
              (update vote-info (str dispute-id)
                { "token-holders": 
                (replace-key-value token-holders "invalid-query" 
                {'invalid-query: holder-invalid-bal })
                , "reporters": 
                (replace-key-value reporters "invalid-query" 
                {'invalid-query: reporter-invalid-bal })
                , "users": 
                (replace-key-value users "invalid-query" 
                {'invalid-query: user-invalid-bal })} )
              (if (= voter-account multisig)
                  (update vote-info (str dispute-id)
                    { "team-multisig": 
                    (replace-key-value team-multisig "invalid-query"
                    { "invalid-query": (+ (at 'invalid-query team-multisig) 1)})
                    }) "invalid query voter is not multisig") )
            (if supports
                (let ((holder-support-bal 
                        (+ (at "does-support" token-holders) voter-balance))
                      (reporter-support-bal 
                        (+ (at "does-support" reporters) reports-submitted))
                      (user-support-bal 
                        (+ (at "does-support" users) 
                        (get-user-tips voter-account))))
                  (update vote-info (str dispute-id)
                    { "token-holders": 
                    (replace-key-value token-holders "does-support" 
                    {"does-support": holder-support-bal })
                    , "reporters": 
                    (replace-key-value reporters "does-support" 
                    {"does-support": reporter-support-bal })
                    , "users": 
                    (replace-key-value users "does-support" 
                    {"does-support": user-support-bal })
                    })
                  (if (= voter-account multisig)
                      (update vote-info (str dispute-id)
                      { "team-multisig": 
                      (replace-key-value team-multisig "does-support"
                      {"does-support": (+ (at "does-support" team-multisig) 1) })
                      })
                      "support voter is not multisig") )
                (let ((holder-against-bal 
                        (+ (at "against" token-holders) voter-balance))
                      (reporter-against-bal 
                        (+ (at "against" reporters) reports-submitted))
                      (user-against-bal 
                        (+ (at "against" users) (get-user-tips voter-account))))
                  (update vote-info (str dispute-id)
                    { "token-holders": 
                    (replace-key-value token-holders "against"
                    { "against": holder-against-bal })
                    , "reporters": 
                    (replace-key-value reporters "against" 
                    { "against": reporter-against-bal })
                    , "users": 
                    (replace-key-value users "against" 
                    { "against": user-against-bal })
                    })
                  (if (= voter-account multisig)
                      (update vote-info (str dispute-id)
                        { "team-multisig": 
                        (replace-key-value team-multisig "against"
                        { "against": (+ (at "against" team-multisig) 1) })
                        })
                      "against voter is not multisig") )
            )
          )
        )
        (write vote-tally-by-address voter-account 
          { "vote-tally-by-address": 
          (+ (get-vote-tally-by-address voter-account ) 1) })
        (emit-event (Voted dispute-id supports voter-account invalid))
      )
      )
    )
  )
; ***************************INTERNAL-FUNCTIONS********************************
  (defun transfer-from-gov (account:string amount:decimal)
    (require-capability (PRIVATE))
    (install-capability
      (f-TRB.TRANSFER (gov-account) account amount))
      (f-TRB.transfer (gov-account) account amount)
  )

  (defun vote-passed (hash:string idx:integer)
    @doc "Internal helper function to transfer fee and \
    \ stake to initiator and fee only to other disputers"
    (require-capability (PRIVATE))
    (let* ((vote-round (at "dispute-ids" (read vote-rounds hash)))
           (vote-id (at (- idx 1) vote-round)))
      (with-read vote-info (str vote-id)
        { "initiator" := initiator , "fee" := fee }
        (if (= idx 1)
            (let* ((slashed-amount
                     (at "slashed-amount" (read dispute-info (str vote-id)))))
              (transfer-from-gov initiator (float (+ slashed-amount fee))))
            (transfer-from-gov initiator (float fee))
        )
      )
    hash
    )
  )

  (defun vote-invalid (hash:string idx:integer)
    @doc "Internal helper function to transfer fee \
    \ to each dispute round initiator"
    (require-capability (PRIVATE))
    (let* ((vote-round (at "dispute-ids" (read vote-rounds hash)))
           (vote-id (at (- idx 1) vote-round)))
      (with-read vote-info (str vote-id)
        { "initiator" := initiator , "fee" := fee }

        (transfer-from-gov initiator (float fee))
      )
    )
    hash
  )

  (defun vote-failed (num:integer dispute-id:integer)
    @doc "Sum the fee of each round"
    (require-capability (PRIVATE))
    (with-read vote-info (str dispute-id) { "fee" := fee }
      (+ num fee))
  )

  (defun calculate-vote-to-scale (votes vote-sum)
    (require-capability (PRIVATE))
    ( / (* votes (^ 10 18)) vote-sum)
  )

  (defun insert-vote-info
    (dispute-id:string hash:string vote-round:integer
      block-time:integer  fee:integer initiator:string)
    (require-capability (PRIVATE))
    (let ((init-tally 
      (lambda () {'does-support: 0, 'against: 0, 'invalid-query: 0})))
      (insert vote-info dispute-id
        { 'identifier-hash: hash, 'vote-round: vote-round
        , 'start-date: block-time
        , 'block-number: (at 'block-height (chain-data))
        , 'fee: fee, 'tally-date: 0
        , 'token-holders: (init-tally), 'users: (init-tally)
        , 'reporters: (init-tally), 'team-multisig: (init-tally)
        , 'executed: false, 'result: ""
        , 'initiator: initiator, 'voters: []}) )
  )

  (defun replace-key-value:object{results-schema}
    (data:object{results-schema} key:string value:object)
    (require-capability (PRIVATE))
    (+ (drop [key] data) value)
  )
; ***************************GETTERS*******************************************
  (defun did-vote:bool (dispute-id:integer voter:string)
    @doc "Determines if an address voted for a specific vote"
    (contains voter (at 'voters (read vote-info (str dispute-id))))
  )

  (defun get-dispute-fee:integer ()
    @doc "Get the latest dispute fee"
    (/ (tellorflex.stake-amount) 10)
  )

  (defun get-disputes-by-reporter:[integer] (reporter:string)
    @doc "Get dispute ids for a reporter"
    (at 'dispute-ids (read dispute-ids-by-reporter reporter))
  )

  (defun get-dispute-info:object{dispute-schema} (dispute-id:integer)
    @doc "Returns info on a dispute for a given ID"
    (read dispute-info (str dispute-id))
  )

  (defun get-open-disputes-on-id:integer (query-id:string)
    @doc "Returns the number of open disputes for a specific query ID"
    (at 'open-disputes-on-id (read open-disputes-on-id query-id))
  )

  (defun get-vote-count:integer ()
    @doc "Returns the total number of votes"
    (at "vote-count" (read global "global-vars"))
  )

  (defun get-vote-info:object{vote-schema} (dispute-id:integer)
    @doc "Returns info on a vote for a given vote ID"
    (read vote-info (str dispute-id))
  )

  (defun get-vote-rounds:[integer] (hash:string)
    @doc "Returns an array of voting rounds for a given vote"
    (at 'dispute-ids (read vote-rounds hash))
  )

  (defun get-vote-tally-by-address:integer (voter:string)
    @doc "Returns the total number of votes cast by an address"
    (try 0 (at 'vote-tally-by-address (read vote-tally-by-address voter)))
  )

  (defun get-user-tips:integer (user:string)
    (require-capability (PRIVATE))
    (autopay.get-tips-by-user user)
  )

  (defun gov-account:string ()
    @doc "Returns governance account name"
    (at "gov-account-name" (read global "global-vars"))
  )

  (defun open-disputes-count:integer (query-id:string)
    (at "open-disputes-on-id" (read open-disputes-on-id query-id))
  )

  (defun vote-count:integer ()
    (at "vote-count" (read global "global-vars"))
  )
; ***************************HELPERS*******************************************
  (defun str (num:integer) (int-to-str 10 num) )

  (defun float:decimal (num:integer) (/ (/ num 1.0) (^ 10 18)) )

  (defun block-time ()
    (str-to-int 10
      (format-time "%s" (at "block-time" (chain-data))))
  )
)
; ***************************INITIALIZE****************************************
(if (read-msg "upgrade")
    ["upgrade"]
    [
      (create-table dispute-info)
      (create-table vote-info)
      (create-table vote-rounds)
      (create-table open-disputes-on-id)
      (create-table global)
      (create-table dispute-ids-by-reporter)
      (create-table vote-tally-by-address)
      (constructor 
        "admin-keyset"
        "governance")
      (tellorflex.init governance)
    ])
