; tellorflex on kadena
(namespace "free")

(if (read-msg "upgrade")
  "Upgrading contract"

  [
    (enforce-keyset (read-keyset "tellor-admin-keyset"))
    (define-keyset "free.tellor-admin-keyset" (read-keyset "tellor-admin-keyset"))
  ]
)
(module tellorflex TELLOR


  @doc
    "'tellorflex' represents the tellor oracle contract. This contract         \
    \allows reporters to stake and report values to make available on chain    \
    \  > (free.tellorflex.submit-value ...)                                    \
    \allows users to read values                                               \
    \  > (free.tellorflex.retrieve-value ...)"

  @model
    [ (defproperty owner-authorized (authorized-by "free.tellor-admin-keyset"))
    ]
    (implements free.i-flex)
; *****************************************************************************
; *                                                                           *
; *                          Constants                                        *
; *                                                                           *
; *****************************************************************************
  (defconst TELLOR_FLEX_ACCOUNT "tellorflex")

  (defconst TIME_BASED_REWARD (* 5 (^ 10 17)))

  (defconst PRECISION (^ 10 18))

  (defconst SEVEN_DAYS 604800)

  (defconst THIRTY_DAYS 2592000)

; *****************************************************************************
; *                                                                           *
; *                          Schema                                           *
; *                                                                           *
; *****************************************************************************
  (defschema constructor-schema
    token:module{fungible-v2}
    accumulated-reward-per-share:integer
    minimum-stake-amount:integer
    reporting-lock:integer
    stake-amount:integer
    stake-amount-dollar-target:integer
    reward-rate:integer
    staking-rewards-balance:integer
    staking-token-price-query-id:string
    time-based-reward:integer
    time-of-last-allocation:integer
    time-of-last-new-value:integer
    total-reward-debt:integer
    total-stake-amount:integer
    total-stakers:integer
    to-withdraw:integer)
  (defschema report
    index:integer
    query-id:string
    timestamp:integer
    block-height:integer
    value:string
    reporter:string
    is-disputed:bool)
  (defschema stake-info
    start-date:integer
    staked-balance:integer
    locked-balance:integer
    reward-debt:integer
    reporter-last-timestamp:integer
    reports-submitted:integer
    start-vote-count:integer
    start-vote-tally:integer
    is-staked:bool
    guard:guard)
  (defschema reports-submitted-by-queryid
    reports-submitted-by-queryid:integer)
  (defschema timestamps-schema
    timestamps:[object{timestamp-dispute-object}])
  (defschema governance-schema
    governance:module{i-governance})
  (defschema gov-guard-schema
    guard:guard)
; *****************************************************************************
; *                                                                           *
; *                          Object-Schema                                    *
; *                                                                           *
; *****************************************************************************
  (defschema timestamp-dispute-object
    timestamp:integer
    disputed:bool)
  (defschema binary-search-object
    found:bool
    target:integer
    start:integer
    end:integer
    timestamp-before:integer
    timestamps:[object{timestamp-dispute-object}]
    disputed:bool)
; *****************************************************************************
; *                                                                           *
; *                          Tables                                           *
; *                                                                           *
; *****************************************************************************
  (deftable staker-details:{stake-info})
  (deftable reports-submitted-count:{reports-submitted-by-queryid})
  (deftable reports:{report})
  (deftable timestamps:{timestamps-schema})
  (deftable global-variables:{constructor-schema})
  (deftable governance-table:{governance-schema})
  (deftable gov-guard:{gov-guard-schema})

; *****************************************************************************
; *                                                                           *
; *                          Capabilities                                     *
; *                                                                           *
; *****************************************************************************
  (defcap TELLOR ()
    @doc "Enforce only owner."
    (enforce-guard (keyset-ref-guard "free.tellor-admin-keyset"))
  )

  (defcap PRIVATE () true )

  (defcap STAKER (account-name:string)
    (enforce-keyset (read-keyset account-name))
    (enforce-guard (at 'guard (read staker-details account-name)))
    (compose-capability (PRIVATE)) )

  (defcap GOV_GUARD ()
    (enforce-guard (at 'guard (read gov-guard 'gov-guard)))
    (compose-capability (PRIVATE)) )
; *****************************************************************************
; *                                                                           *
; *                          Guard                                            *
; *                                                                           *
; *****************************************************************************
  (defun capping:bool () (require-capability (PRIVATE)) )

  (defun create-guard:guard () (create-user-guard (capping)) )

; *****************************************************************************
; *                                                                           *
; *                          Global-vars getter functions                     *
; *                                                                           *
; *****************************************************************************
  (defun token:module{fungible-v2} ()
    @doc "Token"
    (at 'token (read global-variables 'global-vars))
  )

  (defun get-governance-module:module{i-governance} ()
    (at 'governance (read governance-table 'governance))
  )

  (defun accumulated-reward-per-share ()
    (at 'accumulated-reward-per-share (read global-variables 'global-vars))
  )

  (defun minimum-stake-amount:decimal ()
    (at 'minimum-stake-amount (read global-variables 'global-vars))
  )

  (defun reporting-lock:integer ()
    (at 'reporting-lock (read global-variables 'global-vars))
  )

  (defun reward-rate:integer ()
    (at 'reward-rate (read global-variables 'global-vars))
  )

  (defun stake-amount:integer ()
    @doc "Get timestamp of last reported value of any query id"
    (at 'stake-amount (read global-variables 'global-vars))
  )

  (defun stake-amount-dollar-target:integer ()
    (at 'stake-amount-dollar-target (read global-variables 'global-vars))
  )

  (defun staking-rewards-balance:integer ()
    (at 'staking-rewards-balance (read global-variables 'global-vars))
  )

  (defun staking-token-price-query-id:string ()
    (at 'staking-token-price-query-id (read global-variables 'global-vars))
  )

  (defun time-based-reward:integer ()
    TIME_BASED_REWARD
  )

  (defun time-of-last-allocation:integer ()
    (at 'time-of-last-allocation (read global-variables 'global-vars))
  )

  (defun time-of-last-new-value:integer ()
    (at 'time-of-last-new-value (read global-variables 'global-vars))
  )

  (defun total-reward-debt:integer ()
    (at 'total-reward-debt (read global-variables 'global-vars))
  )

  (defun total-stake-amount:integer ()
    @doc "Get total stake amount in oracle"
    (at 'total-stake-amount (read global-variables 'global-vars))
  )

  (defun total-stakers:integer ()
    @doc "Get total number of stakers in oracle"
    (at 'total-stakers (read global-variables 'global-vars))
  )

  (defun to-withdraw:integer ()
    @doc "Get withdraw amount currently locked for withdrawal"
    (at 'to-withdraw (read global-variables 'global-vars))
  )
; *****************************************************************************
; *                                                                           *
; *                          Main functions                                   *
; *                                                                           *
; *****************************************************************************
  (defun constructor:string (
    token:module{fungible-v2}
    reporting-lock:integer
    stake-amount-dollar-target:integer
    staking-token-price:integer
    minimum-stake-amount:integer
    staking-token-price-query-id:string)

    @doc "Set global variables"

    (with-capability (TELLOR)
      (let ((potential-amount (/ stake-amount-dollar-target staking-token-price)))
        (insert global-variables 'global-vars
          { 'token: token
          , 'accumulated-reward-per-share: 0
          , 'minimum-stake-amount: minimum-stake-amount
          , 'reporting-lock: reporting-lock
          , 'reward-rate: 0
          , 'stake-amount: (if (<  potential-amount minimum-stake-amount) minimum-stake-amount potential-amount)
          , 'stake-amount-dollar-target: stake-amount-dollar-target
          , 'staking-rewards-balance: 0
          , 'staking-token-price-query-id: staking-token-price-query-id
          , 'time-based-reward: TIME_BASED_REWARD
          , 'time-of-last-allocation: 0
          , 'time-of-last-new-value: (block-time-in-seconds)
          , 'total-reward-debt: 0
          , 'total-stake-amount: 0
          , 'total-stakers: 0
          , 'to-withdraw: 0}
        )
      )

      (token::create-account TELLOR_FLEX_ACCOUNT (create-guard)

      )"Global variables set!"
    )
  )

  (defun init-gov-guard:string (guard:guard)
    ; fails if governance hasn't been initialized
    (get-governance-module)
    (enforce-keyset "free.tellor-admin-keyset")
    (insert gov-guard 'gov-guard {'guard: guard})
  )

  (defun init (governance:module{i-governance})
    @doc "Allows the owner to initialize the governance (flex addy needed for governance deployment)"
    (with-capability (TELLOR)
      (insert governance-table 'governance { 'governance: governance }))
  )

  (defun add-staking-rewards (account:string amount:decimal)
    @doc "Funds the flex contract with staking rewards (autopay and miniting) anyone can add at will"

    (let ((token:module{fungible-v2} (token)))
      (token::transfer account TELLOR_FLEX_ACCOUNT amount)

      (with-capability (PRIVATE) (update-rewards) )
        (update global-variables 'global-vars
          { 'staking-rewards-balance: (precision amount)})
          (update global-variables 'global-vars
            { 'reward-rate: (/ (calculate-reward-rate) THIRTY_DAYS)})
    )
  )

  (defun add-staker (staker:string guard:guard)
    (require-capability (PRIVATE))
    (with-default-read staker-details staker
      { 'start-date: 0
      , 'staked-balance: 0
      , 'locked-balance: 0
      , 'reward-debt: 0
      , 'reporter-last-timestamp: 0
      , 'reports-submitted: 0
      , 'start-vote-count: 0
      , 'start-vote-tally: 0
      , 'is-staked: false
      , 'guard: guard
      }
      { 'staked-balance := staked-balance
      , 'locked-balance := locked-balance
      , 'reward-debt := reward-debt
      , 'reporter-last-timestamp := reporter-last-timestamp
      , 'reports-submitted:=reports-submitted
      , 'start-vote-count:=start-vote-count
      , 'start-vote-tally:=start-vote-tally
      , 'start-date := start-date
      , 'is-staked := is-staked
      , 'guard := guarded
      }
      (write staker-details staker
        { 'staked-balance: staked-balance
        , 'locked-balance: locked-balance
        , 'reward-debt: reward-debt
        , 'reporter-last-timestamp: reporter-last-timestamp
        , 'reports-submitted: reports-submitted
        , 'start-vote-count: start-vote-count
        , 'start-vote-tally: start-vote-tally
        , 'start-date: start-date
        , 'is-staked: is-staked
        , 'guard: guarded
        }))
  )

  (defun deposit-stake (staker:string guard:guard amount:integer)
    @doc "Allows a reporter to submit stake"
    ; (enforce (> amount 0) "Amount must be greater than 0")
    (let ((governance:module{i-governance} (get-governance-module))
          (block-time (block-time-in-seconds))
          (token:module{fungible-v2} (token)))

      (with-capability (PRIVATE) (add-staker staker guard) )
      (with-capability (STAKER staker) ;TODO: consolidate
       (with-read staker-details staker
         { 'locked-balance := locked-balance , 'staked-balance := staked-balance }
         (if (> locked-balance 0)
            (if (>= locked-balance amount)
              [(update staker-details staker
                  { 'locked-balance: (- locked-balance amount)})
                (update global-variables 'global-vars
                  { 'to-withdraw: (- (to-withdraw) amount)})]
              [(token::transfer staker TELLOR_FLEX_ACCOUNT (to-decimal (- amount locked-balance)))
                (update global-variables 'global-vars
                 { 'to-withdraw: (- (to-withdraw) locked-balance) })
                (update staker-details staker { 'locked-balance: 0 })]
            )

            [(if ( = staked-balance 0)
                (update staker-details staker
                  { 'start-vote-count: (governance::get-vote-count)
                  , 'start-vote-tally: (governance::get-vote-tally-by-address staker)
                  })
                "else pass"
              )
              (token::transfer staker TELLOR_FLEX_ACCOUNT (to-decimal (- amount locked-balance)))
              ]
            )

          )
          (update-stake-and-pay-rewards staker (+ (at 'staked-balance (read staker-details staker)) amount))
          (update staker-details staker { 'start-date: block-time } )
      )

    )
  )

  (defun remove-value (query-id:string timestamp:integer)
    @doc "Remove disputed value only by governance"
    (with-capability (GOV_GUARD)
    (let ((is-in-dispute (is-in-dispute query-id timestamp)))
      (enforce (not is-in-dispute) "Value already disputed"))
      (update reports (concatenate query-id timestamp)
        { 'value: "" , 'is-disputed: true })
    )
  )

  (defun request-staking-withdraw (staker:string amount:integer)
    @doc "Allows a reporter to request to withdraw their stake"
    (let ((block-time (block-time-in-seconds))
          (to-withdraw (to-withdraw)))
         (with-capability (STAKER staker)
           (with-read staker-details staker
             { "staked-balance" := staked-balance
             , "locked-balance" := locked-balance
             , "start-date" := start-date
             }
             (enforce (>= staked-balance amount) "Insufficient staked balance")
             (update-stake-and-pay-rewards staker (- staked-balance amount))
             (update staker-details staker
               { "locked-balance": (+ locked-balance amount)
               , "start-date": block-time})
             )
           (update global-variables 'global-vars
            { 'to-withdraw: (+ to-withdraw amount)})
        )
    )
  )

  (defun slash-reporter (reporter:string recipient:string)
      (with-read staker-details reporter
        { 'staked-balance := staked-balance ,'locked-balance := locked-balance }
        (enforce (> (+ staked-balance locked-balance) 0) "Zero staker balance")
        (let* ((token:module{fungible-v2} (token))
               (stake-amount (stake-amount))
               (to-withdraw (to-withdraw))
               (slash-amount (if
                 (or
                   (>= locked-balance stake-amount)
                   (>= (+ locked-balance staked-balance) stake-amount))
                   stake-amount
                   (+ staked-balance locked-balance))))
      (with-capability (GOV_GUARD)
      (install-capability (token::TRANSFER TELLOR_FLEX_ACCOUNT recipient
        (to-decimal slash-amount)))

        (if (>= locked-balance stake-amount)
            [(update staker-details reporter
              {'locked-balance: (- locked-balance stake-amount)})
             (update global-variables 'global-vars
              {'to-withdraw: (- to-withdraw stake-amount)})]

            (if (>= (+ locked-balance staked-balance) stake-amount)
              [
              (install-capability
                (token::TRANSFER TELLOR_FLEX_ACCOUNT reporter
                  (to-decimal (- staked-balance (- stake-amount locked-balance)))))
              (update-stake-and-pay-rewards reporter
                (- staked-balance (- stake-amount locked-balance)))
               (update global-variables 'global-vars
                { 'to-withdraw: (- to-withdraw locked-balance) })
               (update staker-details reporter { 'locked-balance: 0 })]

              [(update-stake-and-pay-rewards reporter 0)
               (update global-variables 'global-vars
                { 'to-withdraw: (- to-withdraw locked-balance) })
               (update staker-details reporter { 'locked-balance: 0 })]
            )
          )
        (token::transfer TELLOR_FLEX_ACCOUNT recipient (to-decimal slash-amount))
        )
      )
    )
  )

  (defun submit-value
    (query-id:string
     value:string
     nonce:integer
     query-data:string
     staker:string)
    @doc "Enables staked reporters to submit values into the oracle"
    (enforce (!= value "") "value must be submitted")
    (enforce (= query-id (hash query-data))
      "query id must be hash of query data")
    (let ((block-time (block-time-in-seconds))
          (stake-amount (stake-amount))
          (block-height (block-height))
          (reporting-lock (reporting-lock)))

        (with-capability (STAKER staker)
          (with-default-read timestamps query-id
            { 'timestamps: [] }
            { 'timestamps := timestamps-lis }
            (enforce (or (= nonce (length timestamps-lis)) (= nonce 0))
              "Nonce must match timestamps list length")

           (with-read staker-details staker
             { 'staked-balance := staked-balance
             , 'reporter-last-timestamp := reporter-last-timestamp
             , 'reports-submitted := reports-submitted }

             (enforce (>= staked-balance stake-amount)
               "balance must be greater than stake amount")

             (enforce (>
               (- block-time reporter-last-timestamp)
               (/ reporting-lock (/ staked-balance stake-amount)))
               "still in reporter time lock, please wait!")
             ; can't report same timestamp and queryId due to key existing in table
             (insert reports (concatenate query-id block-time)
               { 'index: (length timestamps-lis)
               , 'query-id: query-id
               , 'timestamp: block-time
               , 'block-height: block-height
               , 'value: value
               , 'reporter: staker
               , 'is-disputed: false
               })

             (write timestamps query-id
               { 'timestamps:
               (+ timestamps-lis  [{'timestamp: block-time, 'disputed: false}])}
             )
            (let ((time-reward (calculate-time-based-reward block-time))
                  (token:module{fungible-v2} (token)))
              (if (> time-reward 0.0)
                (let ((transfer-cap
                  (install-capability
                    (token::TRANSFER TELLOR_FLEX_ACCOUNT staker time-reward))))
                  transfer-cap
                  (token::transfer TELLOR_FLEX_ACCOUNT staker time-reward)
                )
                  "pass"
              )
            )
            (update global-variables 'global-vars
               { 'time-of-last-new-value: block-time})

            (update staker-details staker
              { 'reports-submitted: (plus-one reports-submitted)
              , 'reporter-last-timestamp: block-time})

            (with-default-read reports-submitted-count (+ query-id staker)
              { 'reports-submitted-by-queryid: 0}
              { 'reports-submitted-by-queryid := reports-submitted-by-queryid }
              (write reports-submitted-count (+ query-id staker)
              { 'reports-submitted-by-queryid:
                (plus-one reports-submitted-by-queryid)})
            )

        )
        )
      )
    )
  )

  (defun update-stake-amount ()
    @doc "Updates the stake amount after retrieving the 12 hour old price"
    (with-default-read timestamps (staking-token-price-query-id)
      { 'timestamps: [] } { 'timestamps := timestamps }
      (if (> (length timestamps) 0)
      (let* ((twelve-hour-price
        (get-data-before
          (staking-token-price-query-id)
          (- (block-time-in-seconds) (reporting-lock)))))
        (if (= twelve-hour-price {"value": "", "timestamp":0}) "no value"
        (let* ((twelve-hour-price (get-data-before (staking-token-price-query-id) (- (block-time-in-seconds) (reporting-lock))))
             (stake-amount (stake-amount))
             (minimum-stake-amount (minimum-stake-amount))
             (stake-amount-dollar-target (stake-amount-dollar-target))
             (price-decoded (str-to-int 10 (base64-decode (at 'value twelve-hour-price))))
             (adjusted-stake-amount (/ (* stake-amount-dollar-target PRECISION)  price-decoded)))
            ;   validate price
          (enforce (and? (<=  (precision 0.01)) (> (precision 1000000.0)) price-decoded)
           "invalid staking token price")
          (if (< adjusted-stake-amount minimum-stake-amount)
            (update global-variables 'global-vars {'stake-amount: minimum-stake-amount})
            (update global-variables 'global-vars {'stake-amount: adjusted-stake-amount})
            )
      )
     )
    )
    "no price available"
    )
   )
  )

  (defun withdraw-stake (staker:string)
    @doc "Withdraws a reporter's stake after the lock period expires"
    (let ((token:module{fungible-v2} (token))
          (block-time (block-time-in-seconds))
          (to-withdraw (to-withdraw)))

         (with-capability (STAKER staker)
           (with-read staker-details staker
             { "start-date" := start-date
             , "locked-balance" := locked-balance }
             (enforce (> (- block-time start-date) SEVEN_DAYS) "7 days didn't pass")
             (enforce (> locked-balance 0) "reporter not locked for withdrawal")
             (install-capability (token::TRANSFER TELLOR_FLEX_ACCOUNT staker (to-decimal locked-balance)))
             (token::transfer TELLOR_FLEX_ACCOUNT staker (to-decimal locked-balance))

             (update staker-details staker { "locked-balance": 0 } )

             (update global-variables 'global-vars
              { 'to-withdraw: (- to-withdraw locked-balance )})
        )
      )
     )
    )
; *****************************************************************************
; *                                                                           *
; *                          Getter functions                                 *
; *                                                                           *
; *****************************************************************************
  (defun get-block-number-by-timestamp:integer (query-id:string timestamp:integer)
    @doc "Returns the block number at a given timestamp"
    (try 0 (at 'block-height (read reports (concatenate query-id timestamp))))
  )

  (defun get-current-value (query-id:string)
    @doc "Get last reported value for query id"
    (get-data-before query-id (plus-one (block-time-in-seconds)) )
  )

  (defschema data-before-return
    timestamp:integer
    value:string)

  (defun get-data-before:object{data-before-return} (query-id:string timestamp:integer)
    @doc "Retrieves the latest value for the queryId before the specified timestamp"
    (let* ((data-before (data-before query-id timestamp)))
      (if (= {'value: "", 'timestamp: 0} data-before) data-before
      (let* ((timestamp-before (at 'timestamp-before data-before))
             (found (at 'found data-before))
             (not-disputed (not (at 'disputed data-before))))
        (if (and found not-disputed)
          {'value: (at 'value (read reports (concatenate query-id timestamp-before)))
          , 'timestamp: timestamp-before}
          {'value: "", 'timestamp: 0}
        )
        )
      )
    )
  )

  (defun get-new-value-countby-query-id:integer (query-id:string)
    @doc "Get the number of values submitted for a query id"
    (let ((timestamps (at 'timestamps (read timestamps query-id))))
      (length timestamps) )
  )

  (defun get-pending-reward-by-staker:integer (staker:string)
    @doc "Returns the pending staking reward for a given address"
    (with-read staker-details staker
      { 'staked-balance := staked-balance
      , 'reward-debt := reward-debt
      , 'start-vote-count := start-vote-count }
      (let* ((pending-reward (/
                              (* staked-balance
                                 (get-updated-accumulated-reward-per-share))
                              (- PRECISION reward-debt)))
            (governance:module{i-governance} (get-governance-module))
            (vote-count (governance::get-vote-count))
            (number-of-votes (- vote-count start-vote-count))
            (vote-tally (if (> number-of-votes 0) (governance::get-vote-tally-by-address staker) 0)))
          (/ (* pending-reward (- vote-tally start-vote-count)) number-of-votes)))
  )

  (defun get-real-staking-rewards-balance:integer ()
    @doc "Returns the real staking rewards balance after accounting for unclaimed rewards"
    (with-read global-variables 'global-vars
      { 'total-stake-amount := total-stake-amount
      , 'total-reward-debt := total-reward-debt
      , 'staking-rewards-balance := staking-rewards-balance
      }
      (-
        staking-rewards-balance
        (/
          (* (get-updated-accumulated-reward-per-share) total-stake-amount)
          (- PRECISION total-reward-debt)
        )
       )
    )
  )

  (defun get-report-details (query-id:string timestamp:integer)
    (with-read reports (concatenate query-id timestamp)
      { 'reporter := reporter , 'is-disputed := is-disputed }
      { 'reporter: reporter , 'disputed: is-disputed }
    )
  )

  (defun get-reporter-by-timestamp (query-id:string timestamp:integer)
    (at 'reporter (read reports (concatenate query-id timestamp)))
  )

  (defun get-reporter-last-timestamp (reporter:string)
    (at 'reporter-last-timestamp (read staker-details reporter))
  )

  (defun get-reports-submitted-by-address (reporter:string)
    (at 'reports-submitted (read staker-details reporter))
  )

  (defun get-reports-submitted-by-address-and-queryId:integer
    (reporter:string query-id:string)
    (at 'reports-submitted-by-queryid
      (read reports-submitted-count (+ query-id reporter)))
  )

  (defun get-staker-info:object (reporter:string)
    @doc "Get staker info details"
    (read staker-details reporter)
  )

  (defun get-timestampby-query-id-and-index:integer (query-id:string index:integer)
    @doc "Gets the timestamp for the value based on their index"
    (at 'timestamp (at index (at 'timestamps (read timestamps query-id))))
  )

  (defun get-timestamp-index-by-timestamp:integer (query-id:string timestamp:integer)
    (at 'index (read reports (concatenate query-id timestamp)))
  )

  (defun get-total-time-based-rewards-balance:integer ()
    (with-read global-variables 'global-vars
      { 'total-stake-amount := total-stake-amount
      , 'staking-rewards-balance := staking-rewards-balance
      , 'to-withdraw := to-withdraw
      }
      (let ((token:module{fungible-v2} (token)))
      (-
        (precision (token::get-balance TELLOR_FLEX_ACCOUNT))
        (fold (+) to-withdraw [total-stake-amount staking-rewards-balance]))
      ))
  )

  (defun is-in-dispute:bool (query-id:string timestamp:integer)
    @doc "Check if given timestamp report for query id is in dispute"
      (at 'is-disputed (read reports (concatenate query-id timestamp)))
  )

  (defun retrieve-data:integer (query-id:string timestamp:integer)
    @doc "Get value for a query id at given timestamp"
      (at 'value (read reports (concatenate query-id timestamp)))
  )
   ; *****************************************************************************
   ; *                                                                           *
   ; *                          Private functions                                *
   ; *                                                                           *
   ; *****************************************************************************
   (defun update-rewards ()
     (require-capability (PRIVATE))
     (with-read global-variables 'global-vars
       { 'time-of-last-allocation := time-of-last-allocation
       , 'reward-rate := reward-rate
       , 'total-stake-amount := total-stake-amount
       , 'total-reward-debt := total-reward-debt
       , 'staking-rewards-balance := staking-rewards-balance
       , 'accumulated-reward-per-share := accumulated-reward-per-share }
       (let ((block-time (block-time-in-seconds)))
             (if
               (= time-of-last-allocation block-time)
               "was allocated"
             (if
               (or? (= total-stake-amount) (= reward-rate) 0)
               (update global-variables 'global-vars { 'time-of-last-allocation: block-time })
             (let ((accumulated-reward (accumulated-reward total-stake-amount total-reward-debt) ))
             (if
               (>= accumulated-reward staking-rewards-balance)
               (update global-variables 'global-vars
                 { 'accumulated-reward-per-share: (+ accumulated-reward-per-share (/ (* (calculate-reward-rate) PRECISION) total-stake-amount))
                 , 'reward-rate: 0})
             (update global-variables 'global-vars { 'accumulated-reward-per-share: (new-accumulated-reward-per-share)})))))

       (update global-variables 'global-vars { 'time-of-last-allocation: block-time})
       )
     )
   )

   (defun update-stake-and-pay-rewards (staker:string new-staked-balance:integer)
    (require-capability (PRIVATE))
    (update-rewards)
    (with-read staker-details staker
      { 'staked-balance := staked-balance
      , 'reward-debt := reward-debt
      , 'start-vote-count := start-vote-count
      , 'start-vote-tally := start-vote-tally
      , 'is-staked := staked
      }
      ( with-read global-variables 'global-vars
        { 'token := token:module{fungible-v2}
        , 'staking-rewards-balance := staking-rewards-balance
        , 'accumulated-reward-per-share := accumulated-reward-per-share
        , 'total-stake-amount := total-stake-amount
        , 'total-reward-debt := total-reward-debt }

      (if (> staked-balance 0)
        (let* ((governance:module{i-governance} (get-governance-module))
               (pending-reward
                  (-
                    (/ (* staked-balance accumulated-reward-per-share) PRECISION )
                    reward-debt))
               (vote-count (governance::get-vote-count))
               (number-of-votes (- vote-count start-vote-count)))

          (if (> number-of-votes 0)
              (let* ((vote-tally (governance::get-vote-tally-by-address staker))
                     (temp-pending-reward
                       (/
                         (* pending-reward (- vote-tally start-vote-tally))
                         number-of-votes))
                     (pay-amount
                       (if (< temp-pending-reward pending-reward)
                           temp-pending-reward pending-reward)))
                (install-capability
                  (token::TRANSFER TELLOR_FLEX_ACCOUNT staker
                    (to-decimal pay-amount)))
                (token::transfer
                  TELLOR_FLEX_ACCOUNT staker (to-decimal pay-amount))
                (update global-variables 'global-vars
                  { 'staking-rewards-balance: (- staking-rewards-balance pay-amount)
                  })
              )
              [
              (install-capability
                (token::TRANSFER TELLOR_FLEX_ACCOUNT staker
                  (to-decimal pending-reward)))
              (token::transfer
                TELLOR_FLEX_ACCOUNT staker (to-decimal pending-reward))
              (update global-variables 'global-vars
                { 'staking-rewards-balance: (- staking-rewards-balance pending-reward) })]
        )
        (update global-variables 'global-vars
          { 'total-reward-debt: (- total-reward-debt reward-debt)
          , 'total-stake-amount: (- total-stake-amount staked-balance)})
        )
       "else" )
      )
     )
    (update staker-details staker { 'staked-balance: new-staked-balance })
    (with-read staker-details staker
      { 'staked-balance := staked-balance
      , 'is-staked := staked
      , 'reward-debt := reward-debt
      }
      (if (>= staked-balance (stake-amount))
          [
          (if (not staked)
              (update global-variables 'global-vars { 'total-stakers: (+ (total-stakers) 1)})
              "0")
          (update staker-details staker {'is-staked: true })
          ]
          [
          (if (and staked (> (total-stakers) 0))
            (update global-variables 'global-vars { "total-stakers": (- (total-stakers) 1)})
            "0")
          (update staker-details staker {'is-staked: false })
          ]
      )
      (update staker-details staker { 'reward-debt: (/ (* staked-balance (accumulated-reward-per-share)) PRECISION)})
    )
    (with-read staker-details staker { 'staked-balance := staked-balance , 'reward-debt := reward-debt}
      (update global-variables 'global-vars
      { 'total-stake-amount: (+ (total-stake-amount) staked-balance)
      , 'total-reward-debt: (+ (total-reward-debt) reward-debt)
      , 'reward-rate: (if (= (reward-rate) 0) (/ (calculate-reward-rate) THIRTY_DAYS) (reward-rate))
      }))
   )

   (defun get-updated-accumulated-reward-per-share:integer ()
    @doc "Retrieves updated accumulated-reward-per-share"
    (require-capability (PRIVATE))
    (with-read global-variables 'global-vars
      { 'total-stake-amount := total-stake-amount
      , 'total-reward-debt := total-reward-debt
      , 'staking-rewards-balance := staking-rewards-balance
      , 'accumulated-reward-per-share := accumulated-reward-per-share }
      (if (= total-stake-amount 0)
        accumulated-reward-per-share
        (let ((new-pending-rewards (- staking-rewards-balance (/ (* accumulated-reward-per-share total-stake-amount) (- PRECISION total-reward-debt)) ))
              (accumulated-reward (accumulated-reward total-stake-amount total-reward-debt)))
          (if (>= accumulated-reward staking-rewards-balance)
              (/
                (plus-one
                  (+
                    accumulated-reward-per-share
                    (* new-pending-rewards PRECISION)) )
              )
              total-stake-amount)))
    )
   )
; *****************************************************************************
; *                                                                           *
; *                          Helper functions                                 *
; *                                                                           *
; *****************************************************************************
   (defun concatenate (query-id:string timestamp:integer)
    (+ (+ query-id "-") (int-to-str 10 timestamp))
   )

   (defun block-time:time ()
    (at 'block-time (chain-data))
   )

   (defun block-height:integer ()
    (at 'block-height (chain-data))
   )

   (defun block-time-in-seconds:integer ()
    (str-to-int 10 (format-time "%s" (block-time)))
   )

   (defun precision:integer (amount:decimal)
    (round (* amount PRECISION))
   )

   (defun plus-one:integer (amount:integer)
    (+ amount 1)
   )

   (defun calculate-reward-rate:integer ()
     (with-read global-variables 'global-vars
       { 'staking-rewards-balance := staking-rewards-balance
       , 'accumulated-reward-per-share := accumulated-reward-per-share
       , 'total-stake-amount := total-stake-amount
       , 'total-reward-debt := total-reward-debt
       }
       (-
         staking-rewards-balance
           (-
             (/ (* accumulated-reward-per-share total-stake-amount) PRECISION)
              total-reward-debt
           )
       )

     )
   )
   (defun to-decimal:decimal (amount:integer)
     (/ (/ amount 1.0) PRECISION)
   )
   (defun calculate-time-based-reward:decimal (block-time:integer)
     (with-read global-variables 'global-vars
       { 'token := token:module{fungible-v2}
       , 'total-stake-amount := total-stake-amount
       , 'staking-rewards-balance := staking-rewards-balance
       , 'to-withdraw := to-withdraw
       , 'time-of-last-new-value := time-of-last-new-value
       , 'time-based-reward := time-based-reward }

       (let* ((reward (/ (*
               (- block-time time-of-last-new-value) time-based-reward) 300))
              (contract-balance (token::get-balance TELLOR_FLEX_ACCOUNT))
              (total-time-based-rewards-balance
                (- (precision contract-balance)
                  (fold (+) total-stake-amount
                   [staking-rewards-balance to-withdraw]))))

             (if (and (> total-time-based-rewards-balance 0) (> reward 0) )
                 (if (< total-time-based-rewards-balance reward)
                   (to-decimal total-time-based-rewards-balance)
                   (to-decimal reward)
                 )
                 0.0
             )
       )
     )
   )

   (defun accumulated-reward:integer (total-stake-amount:integer total-reward-debt:integer)
     (- 
      (/
       (* (new-accumulated-reward-per-share) total-stake-amount) 
       PRECISION
      )
      total-reward-debt
      )

   )

   (defun new-accumulated-reward-per-share:integer ()
    @doc "Helper function to calculate new-accumulated-reward-per-share"
    (with-read global-variables 'global-vars
      { 'time-of-last-allocation := time-of-last-allocation
      , 'reward-rate := reward-rate
      , 'total-stake-amount := total-stake-amount
      , 'total-reward-debt := total-reward-debt
      , 'staking-rewards-balance := staking-rewards-balance
      , 'accumulated-reward-per-share := accumulated-reward-per-share
      }
      (+
        accumulated-reward-per-share
          (/ (* (*
                (- (block-time-in-seconds) time-of-last-allocation)
                reward-rate)
                PRECISION
            )
            total-stake-amount))
    )
   )

   (defun search-data-before (a:object{binary-search-object} b:integer)
     @doc "helper"
     (if (and (at 'found a) (not (at 'disputed a)))
     ; then return object as is
     a
     ; else
     (let* ((timestamps (at 'timestamps a))
            (target (at 'target a))
            (start (at 'start a))
            (end (at 'end a))
            (middle (/ (- end start) (+ (+ 2 1) start))))

           (if (< (at 'timestamp (at middle timestamps)) target)
             ; then
             (if (>= (at 'timestamp (at (+ middle 1) timestamps)) target)
             ; then
             (if (not (at 'disputed (at middle timestamps)))
               { 'found: true
               , 'target: target
               , 'start: (+ middle 1)
               , 'end: end
               , 'timestamp-before: (at 'timestamp (at middle timestamps))
               , 'timestamps: timestamps
               , 'disputed: (at 'disputed (at middle timestamps))
               }
               (if (> middle 0)
                 (fold (search-if-disputed)
                   { 'found: true
                   , 'target: target
                   , 'start: (+ middle 1)
                   , 'end: middle
                   , 'timestamp-before: (at 'timestamp (at middle timestamps))
                   , 'timestamps: timestamps
                   , 'disputed: (at 'disputed (at middle timestamps))
                   } (enumerate (+ 1 (log 2 (+ middle 1))) 0))
                   { 'found: true
                   , 'target: target
                   , 'start: (+ middle 1)
                   , 'end: middle
                   , 'timestamp-before: (at 'timestamp (at middle timestamps))
                   , 'timestamps: timestamps
                   , 'disputed: (at 'disputed (at middle timestamps))
                   })
               )
             ; else
               { 'found: false
               , 'target: target
               , 'start: (+ middle 1)
               , 'end: end
               , 'timestamp-before: (at 'timestamp (at middle timestamps))
               , 'timestamps: timestamps
               , 'disputed: (at 'disputed (at middle timestamps))
               })
               ; else
               (if (< (at 'timestamp (at (- middle 1) timestamps)) target)
               ; then
               (if (> middle 0)
                 (fold (search-if-disputed)
                   { 'found: true
                   , 'target: target
                   , 'start: (+ middle 1)
                   , 'end: middle
                   , 'timestamp-before: (at 'timestamp (at middle timestamps))
                   , 'timestamps: timestamps
                   , 'disputed: (at 'disputed (at middle timestamps))
                   } (enumerate (+ 1 (log 2 (+ middle 1))) 0))
                   { 'found: false
                   , 'target: 0
                   , 'start: 0
                   , 'end: 0
                   , 'timestamp-before: 0
                   , 'timestamps: [{'timestamp:0,'disputed:true}]
                   , 'disputed: true
                   })
               ; else
               { 'found: false
               , 'target: target
               , 'start: start
               , 'end: (- middle 1)
               , 'timestamp-before: (at 'timestamp (at middle timestamps))
               , 'timestamps: timestamps
               , 'disputed: (at 'disputed (at middle timestamps))
               })

               )
       )
     )
   )

   (defun search-if-disputed:object{binary-search-object} (a:object{binary-search-object} b:integer)
     (let ((end (at 'end a))
           (timestamp (at 'target a))
           (timestamps (at 'timestamps a)))
       (if (at 'disputed a) a
         { 'found: true
         , 'target: timestamp
         , 'start: 0
         , 'end: (- end 1)
         , 'timestamp-before: (at 'timestamp (at (- end 1) timestamps))
         , 'timestamps: timestamps
         , 'disputed: (at 'disputed (at (- end 1) timestamps))
         }))
   )

   (defun data-before:object{binary-search-object} (query-id:string timestamp:integer)
     @doc "helper"
     (let* ((timestamps (at 'timestamps (read timestamps query-id)))
           (count (length timestamps))
           (end (- count 1)))

           (if (>= (at 'timestamp (at 0 timestamps)) timestamp)
             ; timestamp before doesn't exist
             ; cause the smallest timestamp is after given timestamp
             { 'found: false
             , 'target: timestamp
             , 'start: 0
             , 'end: end
             , 'timestamp-before: (at 'timestamp (at 0 timestamps))
             , 'timestamps: timestamps
             , 'disputed: true
             }
           ; check if last reported timestamp is less than given timestamp
           (if (< (at 'timestamp (at end timestamps)) timestamp)
             ; check if disputed
             ; if not disputed timestamp has been found so return
             (if (not (at 'disputed (at end timestamps)))
               { 'found: true
               , 'target: timestamp
               , 'start: 0
               , 'end: end
               , 'timestamp-before: (at 'timestamp (at end timestamps))
               , 'timestamps: timestamps
               , 'disputed: (at 'disputed (at end timestamps))
               }
               ; else search for disputed using log 2 instead of linear?!
               (fold (search-if-disputed)
                 { 'found: true
                 , 'target: timestamp
                 , 'start: 0
                 , 'end: end
                 , 'timestamp-before: (at 'timestamp (at end timestamps))
                 , 'timestamps: timestamps
                 , 'disputed: (at 'disputed (at end timestamps))
                 } (enumerate (+ 1 (log 2 end)) 0)))
           (fold
             (search-data-before)
             { 'found: false
             , 'start: 0
             , 'end: end
             , 'timestamp-before: (at 'timestamp (at end timestamps))
             , 'timestamps: timestamps
             , 'target: timestamp
             , 'disputed: true
             }
             (enumerate (+ 1 (log 2 count)) 0)))))
   )

)

; *****************************************************************************
; *                                                                           *
; *                         Initialize                                        *
; *                                                                           *
; *****************************************************************************
; (if (read-msg "upgrade")
;   ["upgrade"]
;   [
;   ; (create-table stake-info)
;   ; (create-table reports-table)
;   ; (create-table timestamps)
;   ; (create-table global-variables)
;   (constructor)
;   ])
