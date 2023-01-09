; tellor governance on kadena
; read namespace from deploy msg
(namespace (read-msg 'ns))

(if (read-msg "upgrade")
 "Upgrading contract"

 [
   (enforce-keyset (read-keyset "tellor-admin-keyset"))
   (define-keyset "free.tellor-admin-keyset" (read-keyset "tellor-admin-keyset"))
 ]
)


(module governance GOV
  (implements free.i-governance)
; *****************************************************************************
; *                                                                           *
; *                          CAPABILITIES                                     *
; *                                                                           *
; *****************************************************************************
  (defcap GOV ()
    (enforce-guard (keyset-ref-guard "free.tellor-admin-keyset")) )

  (defcap PRIVATE () true )
; *****************************************************************************
; *                                                                           *
; *                          GUARD                                            *
; *                                                                           *
; *****************************************************************************
  (defun capping:bool () (require-capability (PRIVATE)) )

  (defun create-gov-guard:guard () (create-user-guard (capping)) )
; *****************************************************************************
; *                                                                           *
; *                          SCHEMA                                           *
; *                                                                           *
; *****************************************************************************
  (defschema dispute-ids-by-reporter-schema
    dispute-ids:[integer])
  (defschema dispute-schema
    query-id:string
    timestamp:integer
    value:string
    disputed-reporter:string
    slashed-amount:integer)
  (defschema global-schema
    oracle:module{free.i-flex}
    token:module{fungible-v2}
    team-multisig:string
    gov-account-name:string
    vote-count:integer
    autopay-query-id:string)
  (defschema open-disputes-on-id-schema
    open-disputes-on-id:integer)
  (defschema tally-schema
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
    token-holders:object{tally-schema}
    users:object{tally-schema}
    reporters:object{tally-schema}
    team-multisig:object{tally-schema}
    executed:bool
    result:string
    initiator:string
    voters:[string])
  (defschema vote-tally-by-address-schema
    vote-tally-by-address:integer)
; *****************************************************************************
; *                                                                           *
; *                          TABLES                                           *
; *                                                                           *
; *****************************************************************************
  (deftable dispute-ids-by-reporter:{dispute-ids-by-reporter-schema})
  (deftable dispute-info:{dispute-schema})
  (deftable global:{global-schema})
  (deftable open-disputes-on-id:{open-disputes-on-id-schema})
  (deftable vote-info:{vote-schema})
  (deftable vote-rounds:{vote-rounds-schema})
  (deftable vote-tally-by-address:{vote-tally-by-address-schema})
; *****************************************************************************
; *                                                                           *
; *                          CONSTANTS                                        *
; *                                                                           *
; *****************************************************************************
  (defconst TWELVE_HOURS 43200)
  (defconst ONE_DAY 86400)
; *****************************************************************************
; *                                                                           *
; *                          CORE FUNCTIONS                                   *
; *                                                                           *
; *****************************************************************************
  (defun constructor:string (
    oracle:module{free.i-flex}
    token:module{fungible-v2}
    team-multisig:string
    gov-account-name:string
    autopay-query-id:string )
    (insert global 'global-vars
      { 'oracle: oracle
      , 'token: token
      , 'team-multisig: team-multisig
      , 'gov-account-name: gov-account-name
      , 'vote-count: 0
      , 'autopay-query-id: autopay-query-id })

    (token::create-account gov-account-name (create-gov-guard))
    "Global variables set!"
  )

  (defun register-gov-guard:string ()
    @doc "Registers the gov guard with the oracle"
    (let ((tellorflex:module{free.i-flex} (tellorflex)))
      (with-capability (GOV)
        (tellorflex::init-gov-guard (create-gov-guard))
      )
    "Gov guard registered")
  )

  (defun begin-dispute:string (account:string query-id:string timestamp:integer)
    @doc "Begins a dispute for a given query id and timestamp"
    (enforce-keyset (read-keyset account))
    (let* ((tellorflex:module{free.i-flex} (tellorflex))
           (token:module{fungible-v2} (token))
           (block-number (tellorflex::get-block-number-by-timestamp query-id timestamp))
           (hash (hash [query-id timestamp]))
           (dispute-id (+ (vote-count) 1))
           (dispute-fee (get-dispute-fee))
           (block-time (block-time))
           (disputed-reporter (tellorflex::get-reporter-by-timestamp query-id timestamp)))
        (enforce (!= block-number 0)
          (format "No value exists at given timestamp {}"[block-number]))
        ;  reads dispute ids list from vote-rounds table
        ;  checking for existing disputes for the given query id and timestamp
        (with-default-read vote-rounds hash
          { 'dispute-ids: []} { 'dispute-ids := dispute-ids }
          (let ((vote-round (+ 1 (length dispute-ids))))
          (with-capability (PRIVATE)
          ;  if no disputes exist for this report then create a new vote round
            (if (= vote-round 1)
            ;  read open disputes on id to calculate fee
              (with-default-read open-disputes-on-id query-id
                { 'open-disputes-on-id: 0 } { 'open-disputes-on-id := open-disputes }
                (enforce (< (- block-time timestamp) TWELVE_HOURS)
                  "Dispute must be started within reporting lock time")
                (let ((fee (* dispute-fee (^ 2 open-disputes))) )
                    ;  transfer fee to gov account
                    (token::transfer account (gov-account) (float fee))
                    ;  insert dispute info into dispute-info table
                    (insert dispute-info (str dispute-id)
                      { 'query-id: query-id
                      , 'timestamp: timestamp
                      , 'value: (tellorflex::retrieve-data query-id timestamp)
                      , 'disputed-reporter: disputed-reporter
                      , 'slashed-amount:
                        (tellorflex::slash-reporter disputed-reporter (gov-account)) ; slash reporter and transfer stake to gov account
                      })
                    ;  set report value to empty string in oracle
                    (tellorflex::remove-value query-id timestamp)
                    ;  insert vote info into vote-info table
                    (insert-vote-info (str dispute-id) hash vote-round block-time fee account)
                    ; increment open disputes on id by 1
                    (write open-disputes-on-id query-id
                      { 'open-disputes-on-id: (+ open-disputes 1)})
                  )
                )
                ;  if a dispute already exists for this report then create a new dispute round
                (let ((prev-tally-date
                        (at 'tally-date (read vote-info (str (at (- vote-round 2) dispute-ids)))))
                      (fee (* dispute-fee (^ 2 (length dispute-ids)))) )
                    (enforce (< (- block-time prev-tally-date) ONE_DAY)
                      "New dispute round must be started within a day")
                    ;  transfer fee to gov account
                    (token::transfer account (gov-account) (float fee))
                    ;  insert vote info into vote-info table
                    (insert-vote-info (str dispute-id) hash vote-round block-time fee account)
                    ; read slashed amount and value from first round dispute
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
            ;  add this round's dispute id to list of dispute ids for this report
            (write vote-rounds hash { 'dispute-ids: (+ dispute-ids [dispute-id])})
            ;  add this dispute id to list of dispute ids for disputed reporter
            (with-default-read dispute-ids-by-reporter disputed-reporter
              { 'dispute-ids: [] }{ 'dispute-ids := dispute-ids }
              (write dispute-ids-by-reporter disputed-reporter
                { 'dispute-ids: (+ dispute-ids [dispute-id])}))
            ;  increment global vote count
            (with-read global 'global-vars { 'vote-count := vote-count }
              (update global 'global-vars { 'vote-count: (+ vote-count 1)}))
          )
        )
      )
  )

  (defun execute-vote:string (dispute-id:integer)
    @doc "Execute vote for a given dispute ID, \
    \ finalizing the vote and transferring the fees and stake"
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
        (enforce (= (length dispute-ids) vote-round) "Must be the final vote round")
        (enforce (>= (- (block-time) tally-date) ONE_DAY)
          "1 day has to pass after tally to allow for disputes")
        ;  update vote info to show that vote has been executed
        (update vote-info (str dispute-id) { 'executed: true })

        (with-capability (PRIVATE)
        ; transfer stake and fees to appropriate parties given the result of the vote
          (with-read dispute-info (str dispute-id)
            { 'disputed-reporter := disputed-reporter
            , 'slashed-amount := slashed-amount
            , 'query-id := query-id }
            (if (= result 'PASSED)
                (fold (vote-passed) identifier-hash
                  (enumerate 1 (length dispute-ids)) )

                (if (= result 'INVALID)
                    (let ((reporter-amount (float slashed-amount)))
                      (fold (vote-invalid) identifier-hash
                        (enumerate (length dispute-ids) 1) )
                      ; transfer slashed amount back to disputed reporter
                      (transfer-from-gov disputed-reporter reporter-amount)
                    )
                    (if (= result 'FAILED)
                      ;  transfer fees and slashed amount to disputed reporter
                        (let* ((total-fees (fold (vote-failed) 0 dispute-ids))
                               (transfer-total (float (+ slashed-amount total-fees))) )
                          (transfer-from-gov disputed-reporter transfer-total)
                        )
                    "Invalid result value"
                    )
                )
            )
              ;  decrement open disputes count for this query
              (update open-disputes-on-id query-id
                { 'open-disputes-on-id: (- (open-disputes-count query-id) 1) })
          )
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
      (let* ( (holders-support (at 'does-support token-holders))
              (holders-against (at 'against token-holders))
              (holders-invalid (at 'invalid-query token-holders))
              (reporters-support (at 'does-support reporters))
              (reporters-against (at 'against reporters))
              (reporters-invalid (at 'invalid-query reporters))
              (users-support (at 'does-support users))
              (users-against (at 'against users))
              (users-invalid (at 'invalid-query users))
              (multisig-support (at 'does-support team-multisig))
              (multisig-against (at 'against team-multisig))
              (multisig-invalid (at 'invalid-query team-multisig))
              (token-vote-sum (fold (+) holders-support [holders-against holders-invalid]))
              (reporters-vote-sum (fold (+) reporters-support [reporters-against reporters-invalid]))
              (multisig-vote-sum (fold (+) multisig-support [multisig-against multisig-invalid]))
              (users-vote-sum (fold (+) users-support [users-against users-invalid]))
              (token-vote-sum-updated (if (= token-vote-sum 0) (+ token-vote-sum 1) token-vote-sum ))
              (reporters-vote-sum-updated (if (= reporters-vote-sum 0) (+ reporters-vote-sum 1) reporters-vote-sum ))
              (multisig-vote-sum-updated (if (= multisig-vote-sum 0) (+ multisig-vote-sum 1) multisig-vote-sum ))
              (users-vote-sum-updated (if (= users-vote-sum 0) (+ users-vote-sum 1) users-vote-sum ))
              (scaled-does-support
                (fold (+) 0 (zip (calculate-vote-to-scale)
                [holders-support reporters-support multisig-support users-support]
                [token-vote-sum-updated reporters-vote-sum-updated multisig-vote-sum-updated users-vote-sum-updated])))
              (scaled-against
                (fold (+) 0 (zip (calculate-vote-to-scale)
                  [holders-against reporters-against multisig-against users-against]
                  [token-vote-sum-updated reporters-vote-sum-updated multisig-vote-sum-updated users-vote-sum-updated])))
              (scaled-invalid
                (fold (+) 0 (zip (calculate-vote-to-scale)
                  [holders-invalid reporters-invalid multisig-invalid users-invalid]
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

      (let ((vote-count (at 'vote-count (read global 'global-vars))))
        (enforce (and (<= dispute-id vote-count) (> dispute-id 0)) "Vote doesn't exist"))
      (enforce (= tally-date 0) "Vote has already been tallied")
      (enforce (not (contains voter-account voters)) "Voter has already voted");check how efficient this is?
      (update vote-info (str dispute-id) { 'voters : (+ voters [voter-account])})

      (let* ((tellorflex:module{free.i-flex} (tellorflex))
             (token:module{fungible-v2} (token) )
             (multisig (at 'team-multisig (read global 'global-vars)))
             (voter-stake-info (try {} (tellorflex::get-staker-info voter-account)))
             (reports-submitted (try 0
               (tellorflex::get-reports-submitted-by-address voter-account)))
             (voter-balance
               (if (= {} voter-stake-info) (round (* (token::get-balance voter-account) (^ 10 18)))
                   (fold (+) 0 [
                   (round (* (token::get-balance voter-account) (^ 10 18)))
                   (at 'staked-balance voter-stake-info)
                   (at 'locked-balance voter-stake-info)]))))
            (with-capability (PRIVATE)
            (if invalid
              (let ((holder-invalid-bal (+ (at 'invalid-query token-holders) voter-balance))
                    (reporter-invalid-bal (+ (at 'invalid-query reporters) reports-submitted))
                    (user-invalid-bal (+ (at 'invalid-query users) (get-user-tips voter-account))))

                (update vote-info (str dispute-id)
                  { 'token-holders: (replace-key-value token-holders
                    'invalid-query {'invalid-query: holder-invalid-bal })
                  , 'reporters: (replace-key-value reporters
                    'invalid-query {'invalid-query: reporter-invalid-bal })
                  , 'users: (replace-key-value users
                    'invalid-query {'invalid-query: user-invalid-bal })
                  })
                (if (= voter-account multisig)
                    (update vote-info (str dispute-id)
                    { 'team-multisig: (replace-key-value team-multisig
                      'invalid-query
                      {'invalid-query: (+ (at 'invalid-query team-multisig) 1) })
                    })
                    "invalid query voter is not multisig"
                )
              )
              (if supports
                (let ((holder-support-bal (+ (at 'does-support token-holders) voter-balance))
                      (reporter-support-bal (+ (at 'does-support reporters) reports-submitted))
                      (user-support-bal (+ (at 'does-support users) (get-user-tips voter-account))))
                  (update vote-info (str dispute-id)
                    { 'token-holders: (replace-key-value token-holders
                      'does-support {'does-support: holder-support-bal })
                    , 'reporters: (replace-key-value reporters
                      'does-support {'does-support: reporter-support-bal })
                    , 'users: (replace-key-value users
                      'does-support {'does-support: user-support-bal })
                    })
                  (if (= voter-account multisig)
                      (update vote-info (str dispute-id)
                      { 'team-multisig: (replace-key-value team-multisig
                        'does-support
                        {'does-support: (+ (at 'does-support team-multisig) 1) })
                      })
                      "support voter is not multisig"
                  )
                )
                (let ((holder-against-bal (+ (at 'against token-holders) voter-balance))
                      (reporter-against-bal (+ (at 'against reporters) reports-submitted))
                      (user-against-bal (+ (at 'against users) (get-user-tips voter-account))))
                  (update vote-info (str dispute-id)
                    { 'token-holders: (replace-key-value token-holders
                      'against { 'against: holder-against-bal })
                    , 'reporters: (replace-key-value reporters
                      'against { 'against: reporter-against-bal })
                    , 'users: (replace-key-value users
                      'against { 'against: user-against-bal })
                    }
                  )
                  (if (= voter-account multisig)
                      (update vote-info (str dispute-id)
                        { 'team-multisig: (replace-key-value team-multisig
                          'against
                          { 'against: (+ (at 'against team-multisig) 1) })
                        }
                      )
                      "against voter is not multisig"
                  )
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
  (defun transfer-from-gov (account:string amount:decimal)
    (require-capability (PRIVATE))
    (let ((token:module{fungible-v2} (token)))
      (install-capability
        (token::TRANSFER (gov-account) account amount))
        (token::transfer (gov-account) account amount))
  )

  (defun vote-passed (hash:string idx:integer)
    @doc "Internal helper function to transfer fee and \
    \ stake to initiator and fee only to other disputers"
    (require-capability (PRIVATE))
    (let* ((vote-round (at 'dispute-ids (read vote-rounds hash)))
           (vote-id (at (- idx 1) vote-round)))
      (with-read vote-info (str vote-id)
        { 'initiator := initiator , 'fee := fee }
        (if (= idx 1)
            (let* ((slashed-amount
                     (at 'slashed-amount (read dispute-info (str vote-id)))))
              (transfer-from-gov initiator (float (+ slashed-amount fee))))
            (transfer-from-gov initiator (float fee))
        )
      )
    hash
    )
  )

  (defun vote-invalid (hash:string idx:integer)
    @doc "Internal helper function to transfer fee to each dispute round initiator"
    (require-capability (PRIVATE))
    (let* ((vote-round (at 'dispute-ids (read vote-rounds hash)))
           (vote-id (at (- idx 1) vote-round))
           (token:module{fungible-v2} (token)))
      (with-read vote-info (str vote-id)
        { 'initiator := initiator , 'fee := fee }

        (transfer-from-gov initiator (float fee))
      )
    )
    hash
  )

  (defun vote-failed (num:integer dispute-id:integer)
    @doc "Sum the fee of each round"
    (require-capability (PRIVATE))
    (with-read vote-info (str dispute-id) { 'fee := fee }
      (+ num fee))
  )
; *****************************************************************************
; *                                                                           *
; *                 (tally-vote) helpers                                      *
; *                                                                           *
; *****************************************************************************
  (defun calculate-vote-to-scale (votes vote-sum)
    ( / (* votes (^ 10 18)) vote-sum)
  )
; *****************************************************************************
; *                                                                           *
; *                 (vote) helpers                                            *
; *                                                                           *
; *****************************************************************************
  (defun insert-vote-info
    (dispute-id:string hash:string vote-round:integer
      block-time:integer  fee:integer initiator:string)
    (require-capability (PRIVATE))
    (insert vote-info dispute-id
      { 'identifier-hash: hash
      , 'vote-round: vote-round
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
      , 'initiator: initiator
      , 'voters: []
      }))

  (defun replace-key-value:object{tally-schema}
    (data:object{tally-schema} key:string value:object)
    (+ (drop [key] data) value)
  )
; *****************************************************************************
; *                                                                           *
; *                         Getters                                           *
; *                                                                           *
; *****************************************************************************
  (defun autopay-id:string () (at 'autopay-query-id (read global 'global-vars)))

  (defun did-vote:bool (dispute-id:integer voter:string)
    @doc "Determines if an address voted for a specific vote"
    (contains voter (at 'voters (read vote-info (str dispute-id))))
  )

  (defun get-dispute-fee:integer ()
    @doc "Get the latest dispute fee"
    (let ((tellorflex:module{free.i-flex} (tellorflex)))
      (/ (tellorflex::stake-amount) 10)
    )
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
    (at 'vote-count (read global 'global-vars))
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
    (let* ((flex:module{free.i-flex} (tellorflex))
          (autopay
            (try {'value: "", 'timestamp: 0}
              (flex::get-data-before (autopay-id) (- (block-time) 43200) ))))
        (if (> (at 'timestamp autopay) 0)
            ; TODO: iterate through autopay addresses and get user tips
            0
            ; else return 0
            0
        )
    )
  )

  (defun gov-account:string ()
    @doc "Returns governance account name"
    (at 'gov-account-name (read global 'global-vars))
  )

  (defun open-disputes-count:integer (query-id:string)
    (at 'open-disputes-on-id (read open-disputes-on-id query-id))
  )

  (defun tellorflex:module{free.i-flex} ()
    (at 'oracle (read global 'global-vars))
  )

  (defun token () (at 'token (read global 'global-vars)) )

  (defun vote-count:integer ()
    (at 'vote-count (read global 'global-vars))
  )
; *****************************************************************************
; *                                                                           *
; *                         helpers                                           *
; *                                                                           *
; *****************************************************************************
  (defun str (num:integer) (int-to-str 10 num) )

  (defun float:decimal (num:integer) (/ (/ num 1.0) (^ 10 18)) )

  (defun block-time ()
    (str-to-int 10
      (format-time "%s" (at 'block-time (chain-data))))
  )
)
; *****************************************************************************
; *                                                                           *
; *                         Initialize                                        *
; *                                                                           *
; *****************************************************************************

