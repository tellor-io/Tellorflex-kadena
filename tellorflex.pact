; tellorflex on kadena
(namespace (read-msg 'ns))
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
  (defconst TIME_BASED_REWARD (* 5 (^ 10 11)))

  (defconst PRECISION (^ 10 12))

  (defconst SEVEN_DAYS 604800)

  (defconst THIRTY_DAYS 2592000)

; *****************************************************************************
; *                                                                           *
; *                          Schema                                           *
; *                                                                           *
; *****************************************************************************
  (defschema constructor-schema
    tellorflex-account:string
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

  (defun create-flex-guard:guard () (create-user-guard (capping)) )

; *****************************************************************************
; *                                                                           *
; *                          Global-vars getter functions                     *
; *                                                                           *
; *****************************************************************************
  (defun tellorflex-account:string ()
    (at 'tellorflex-account (read global-variables 'global-vars))
  )

  (defun token:module{fungible-v2} ()
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
    (at 'total-stake-amount (read global-variables 'global-vars))
  )

  (defun total-stakers:integer ()
    (at 'total-stakers (read global-variables 'global-vars))
  )

  (defun to-withdraw:integer ()
    (at 'to-withdraw (read global-variables 'global-vars))
  )
; *****************************************************************************
; *                                                                           *
; *                          Main functions                                   *
; *                                                                           *
; *****************************************************************************
  (defun constructor:string (
    tellorflex-account:string
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
          { 'tellorflex-account: tellorflex-account
          , 'token: token
          , 'accumulated-reward-per-share: 0
          , 'minimum-stake-amount: minimum-stake-amount
          , 'reporting-lock: reporting-lock
          , 'reward-rate: 0
          , 'stake-amount: (if (<  potential-amount minimum-stake-amount)
                                minimum-stake-amount
                                potential-amount)
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

      (token::create-account tellorflex-account (create-flex-guard))
      "Global variables set!"
    )
  )

  (defun init-gov-guard:string (guard:guard)
    ; fails if governance hasn't been initialized
    (get-governance-module)
    (enforce-guard (keyset-ref-guard "free.tellor-admin-keyset"))
    (insert gov-guard 'gov-guard {'guard: guard})
    "Gov guard registered"
  )

  (defun init (governance:module{i-governance})
    @doc "Allows the owner to initialize the governance (flex addy needed for governance deployment)"
    (with-capability (TELLOR)
      (insert governance-table 'governance { 'governance: governance }))
  )

  (defun add-staking-rewards (account:string amount:integer)
    @doc "Funds the flex contract with staking rewards (autopay and miniting) anyone can add at will"
    (with-capability (PRIVATE)
      (transfers-to-flex amount account)
      (with-capability (PRIVATE) (update-rewards) )
        (update global-variables 'global-vars
          { 'staking-rewards-balance: amount})
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
    (let ((governance:module{i-governance} (get-governance-module))
          (block-time (block-time-in-seconds)))

      (with-capability (PRIVATE) (add-staker staker guard) )

      (with-capability (STAKER staker)
       (with-read staker-details staker
         { 'locked-balance := locked-balance , 'staked-balance := staked-balance }
         (if (> locked-balance 0)
            (if (>= locked-balance amount)
              (let ((updated-locked-balance (- locked-balance amount))
                    (updated-withdraw-amount (- (to-withdraw) amount)))
                  (update staker-details staker
                  { 'locked-balance: updated-locked-balance})
                  (update global-variables 'global-vars
                  { 'to-withdraw: updated-withdraw-amount})
              )
              (let ((updated-withdraw-amount (- (to-withdraw) locked-balance))
                    (updated-amount (- amount locked-balance)))
                  ;  if the amount is greater than the locked balance, transfer the difference
                  ;  and update the locked balance to 0
                  (transfers-to-flex updated-amount staker)
                  (update staker-details staker
                  { 'locked-balance: 0})
                  (update global-variables 'global-vars
                  { 'to-withdraw: updated-withdraw-amount})
              )
            )
            (let ((vote-count (governance::get-vote-count))
                  (vote-tally (governance::get-vote-tally-by-address staker)))
                ;  if the staked balance is 0, update the start vote count and tally
                (if (= staked-balance 0)
                    (update staker-details staker
                      { 'start-vote-count: vote-count
                      , 'start-vote-tally: vote-tally
                      })
                    "Staked balance is not 0 vote tally and count not updated"
                )
              (transfers-to-flex amount staker)
            )
          )
          (update-stake-and-pay-rewards staker
            (+ (at 'staked-balance (read staker-details staker)) amount))
          (update staker-details staker { 'start-date: block-time } )
       )
     )
   )
  )

  (defun remove-value:string (query-id:string timestamp:integer)
    @doc "Remove disputed value only by governance"
    (with-capability (GOV_GUARD)
      (let ((is-in-dispute (is-in-dispute query-id timestamp))
            (idx (get-timestamp-index-by-timestamp query-id timestamp)))

          (enforce (not is-in-dispute) "Value already disputed")
          (update reports (concatenate query-id timestamp)
            { 'value: "" , 'is-disputed: true })
          (with-read timestamps query-id { 'timestamps := timestamps-lis }
            (update timestamps query-id
              { 'timestamps:
                (+
                  (+
                    (take idx timestamps-lis)
                    [{'timestamp: timestamp, 'disputed: true}]
                  )
                  (drop (+ 1 idx) timestamps-lis)
                )
              }
            )
          )
      )
    ) (format "value for {} at {} removed" [query-id timestamp])
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

  (defun slash-reporter:integer (reporter:string recipient:string)
    (with-capability (GOV_GUARD)
      (with-read staker-details reporter
        { 'staked-balance := staked-balance ,'locked-balance := locked-balance }
        (enforce (> (+ staked-balance locked-balance) 0) "Zero staker balance")
        (let* ((stake-amount (stake-amount))
               (to-withdraw (to-withdraw)))

        (if (>= locked-balance stake-amount)
            (let ((locked-bal (- locked-balance stake-amount))
                  (withdraw-bal (- to-withdraw stake-amount)))
                (transfers-from-flex stake-amount recipient)

                (update staker-details reporter {'locked-balance: locked-bal})
                (update global-variables 'global-vars {'to-withdraw: withdraw-bal})
                stake-amount
            )
            (if (>= (+ locked-balance staked-balance) stake-amount)
                (let ((withdraw-bal (- to-withdraw locked-balance)))
                  (update-stake-and-pay-rewards reporter (- staked-balance (- stake-amount locked-balance)))
                  (transfers-from-flex stake-amount recipient)
                  (update staker-details reporter {'locked-balance: 0})
                  (update global-variables 'global-vars {'to-withdraw: withdraw-bal})
                  stake-amount
                )
                (let ((amount (+ staked-balance locked-balance))
                       (withdraw-bal (- to-withdraw locked-balance)))

                    (update-stake-and-pay-rewards reporter 0)
                    (transfers-from-flex amount reporter)

                    (update global-variables 'global-vars { 'to-withdraw: withdraw-bal })
                    (update staker-details reporter { 'locked-balance: 0 })
                    amount
                )
            )
          )
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
          (block-height (at 'block-height (chain-data)))
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
               (+ timestamps-lis [{'timestamp: block-time, 'disputed: false}])}
             )

            (transfers-from-flex (calculate-time-based-reward block-time) staker)

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
                (let* ((stake-amount (stake-amount))
                      (minimum-stake-amount (minimum-stake-amount))
                      (stake-amount-dollar-target (stake-amount-dollar-target))
                      (price-decoded (str-to-int 10 (base64-decode (at 'value twelve-hour-price))))
                      (adjusted-stake-amount
                        (/ (* stake-amount-dollar-target PRECISION)  price-decoded)))
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
    (with-capability (STAKER staker)
      (with-read staker-details staker
        { "start-date" := start-date
        , "locked-balance" := locked-balance }
        (enforce (> (- (block-time-in-seconds) start-date) SEVEN_DAYS) "7 days didn't pass")
        (enforce (> locked-balance 0) "reporter not locked for withdrawal")
        (transfers-from-flex locked-balance staker)

        (update staker-details staker { "locked-balance": 0 } )

        (update global-variables 'global-vars
        { 'to-withdraw: (- (to-withdraw) locked-balance )})
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

  (defun get-data-before:object{i-flex.data-before-value}
    (query-id:string timestamp:integer)
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

  (defun get-new-value-count-by-query-id:integer (query-id:string)
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

  (defun get-reporter-by-timestamp:string (query-id:string timestamp:integer)
    (at 'reporter (read reports (concatenate query-id timestamp)))
  )

  (defun get-reporter-last-timestamp (reporter:string)
    (at 'reporter-last-timestamp (read staker-details reporter))
  )

  (defun get-reports-submitted-by-address:integer (reporter:string)
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
        (precision (token::get-balance (tellorflex-account)))
        (fold (+) to-withdraw [total-stake-amount staking-rewards-balance]))
      ))
  )

  (defun is-in-dispute:bool (query-id:string timestamp:integer)
      (at 'is-disputed (read reports (concatenate query-id timestamp)))
  )

  (defun retrieve-data:string (query-id:string timestamp:integer)
      (at 'value (read reports (concatenate query-id timestamp)))
  )
; *****************************************************************************
; *                                                                           *
; *                          Private functions                                *
; *                                                                           *
; *****************************************************************************
  (defun transfers-from-flex (amount:integer to:string)
    (require-capability (PRIVATE))
    (if (> amount 0)
        (let ((token:module{fungible-v2} (token))
              (flex (tellorflex-account)))
          (enforce (> amount 0) "Amount must be greater than zero")
          (install-capability (token::TRANSFER flex to (to-decimal amount)))
          (token::transfer flex to (to-decimal amount))
        )
        "nothing to transfer"
    )
  )
  (defun transfers-to-flex (amount:integer from:string)
    (require-capability (PRIVATE))
    (if (> amount 0)
        (let ((token:module{fungible-v2} (token)))
          (enforce (> amount 0) "Amount must be greater than zero")
          (token::transfer from (tellorflex-account) (to-decimal amount))
        )
        "nothing to transfer"
    )
  )
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
          (if (= time-of-last-allocation block-time)
              "time-of-last-allocation equals block-time"
              (if (or? (= total-stake-amount) (= reward-rate) 0)
                  (update global-variables 'global-vars
                    { 'time-of-last-allocation: block-time } )
                  (let ((accumulated-reward
                          (accumulated-reward total-stake-amount total-reward-debt) ))
                    (if (>= accumulated-reward staking-rewards-balance)
                        (update global-variables 'global-vars
                          { 'accumulated-reward-per-share:
                          (pending-new-accumulated-reward-per-share
                            accumulated-reward-per-share
                            total-stake-amount)
                          , 'reward-rate: 0 })
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
        { 'staking-rewards-balance := staking-rewards
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
                            (/ (* pending-reward (- vote-tally start-vote-tally))
                              number-of-votes ))
                         (pay-amount
                            (if (< temp-pending-reward pending-reward)
                                temp-pending-reward
                                pending-reward
                            )))
                    (transfers-from-flex pay-amount staker)
                    (update global-variables 'global-vars
                      { 'staking-rewards-balance: (- staking-rewards pay-amount)
                      })
                  )
                  (let ((updated-staking-rewards (- staking-rewards pending-reward)))

                      (transfers-from-flex pending-reward staker)
                      (update global-variables 'global-vars
                        { 'staking-rewards-balance: updated-staking-rewards })
                  )
              )
              (update global-variables 'global-vars
                { 'total-reward-debt: (- total-reward-debt reward-debt)
                , 'total-stake-amount: (- total-stake-amount staked-balance)})
            )
            "staked balance <= 0"
        )
      )
    )
    (update staker-details staker { 'staked-balance: new-staked-balance })
    (with-read staker-details staker
      { 'staked-balance := staked-balance
      , 'is-staked := staked
      , 'reward-debt := reward-debt
      }
      (if (>= staked-balance (stake-amount))
          (let ((stakers-total (plus-one (total-stakers))))
            (if (not staked)
                (update global-variables 'global-vars { 'total-stakers: stakers-total })
                "is staked!"
            )
            (update staker-details staker { 'is-staked: true })
          )
          (let ((stakers-total (- (total-stakers) 1)))
            (if (and staked (> (total-stakers) 0))
                (update global-variables 'global-vars { "total-stakers": stakers-total })
                "staked but total-stakers <= to zero ?!"
            )
            (update staker-details staker {'is-staked: false })
          )
      )
      (update staker-details staker
        { 'reward-debt: (/ (* staked-balance (accumulated-reward-per-share)) PRECISION)})
    )
    (with-read staker-details staker
      { 'staked-balance := staked-balance , 'reward-debt := reward-debt}
      (update global-variables 'global-vars
        { 'total-stake-amount: (+ (total-stake-amount) staked-balance)
        , 'total-reward-debt: (+ (total-reward-debt) reward-debt)
        , 'reward-rate: (if (= (reward-rate) 0)
                            (/ (calculate-reward-rate) THIRTY_DAYS)
                            (reward-rate)
                        )
        }
      )
    )
  )

  (defun get-updated-accumulated-reward-per-share:integer ()
    (require-capability (PRIVATE))
    (with-read global-variables 'global-vars
      { 'total-stake-amount := total-stake-amount
      , 'total-reward-debt := total-reward-debt
      , 'staking-rewards-balance := staking-rewards-balance
      , 'accumulated-reward-per-share := accumulated-reward-per-share
      }
      (if (= total-stake-amount 0)
          accumulated-reward-per-share
          (let ((accumulated-reward
                  (accumulated-reward total-stake-amount total-reward-debt)))
              (if (>= accumulated-reward staking-rewards-balance)
                  (pending-new-accumulated-reward-per-share
                    accumulated-reward-per-share
                    total-stake-amount )
                  (+ accumulated-reward-per-share (/ (* (calculate-reward-rate) PRECISION) total-stake-amount))
                  (new-accumulated-reward-per-share)
              )
          )
      )
    )
  )
; *****************************************************************************
; *                                                                           *
; *                          Helper functions                                 *
; *                                                                           *
; *****************************************************************************
  (defun accumulated-reward:integer
    (total-stake-amount:integer total-reward-debt:integer)
    (require-capability (PRIVATE))
    (-
      (/ (* (new-accumulated-reward-per-share) total-stake-amount)
        PRECISION )
      total-reward-debt )
  )

  (defun block-time-in-seconds:integer ()
    (str-to-int 10 (format-time "%s" (at 'block-time (chain-data))))
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
             (contract-balance (token::get-balance (tellorflex-account)))
             (total-time-based-rewards-balance
              (- (precision contract-balance)
                (fold (+) total-stake-amount
                  [staking-rewards-balance to-withdraw]))))

          (if (and (> total-time-based-rewards-balance 0) (> reward 0) )
              (if (< total-time-based-rewards-balance reward)
                  total-time-based-rewards-balance
                  reward
              )
              0
          )
      )
    )
  )

  (defun calculate-reward-rate:integer ()
    (require-capability (PRIVATE))
    (with-read global-variables 'global-vars
      { 'staking-rewards-balance := staking-rewards-balance
      , 'accumulated-reward-per-share := accumulated-reward-per-share
      , 'total-stake-amount := total-stake-amount
      , 'total-reward-debt := total-reward-debt
      }
      (- staking-rewards-balance
        (- (/ (* accumulated-reward-per-share total-stake-amount) PRECISION )
          total-reward-debt )
      )
    )
  )

  (defun concatenate (query-id:string timestamp:integer)
    (+ (+ query-id "-") (int-to-str 10 timestamp))
  )

  (defun plus-one:integer (amount:integer)
    (+ amount 1)
  )

  (defun precision:integer (amount:decimal)
    (round (* amount PRECISION))
  )

  (defun to-decimal:decimal (amount:integer)
    (/ (/ amount 1.0) PRECISION)
  )

  (defun new-accumulated-reward-per-share:integer ()
    (with-read global-variables 'global-vars
      { 'time-of-last-allocation := time-of-last-allocation
      , 'reward-rate := reward-rate
      , 'total-stake-amount := total-stake-amount
      , 'total-reward-debt := total-reward-debt
      , 'staking-rewards-balance := staking-rewards-balance
      , 'accumulated-reward-per-share := accumulated-reward-per-share
      }
        (+ accumulated-reward-per-share
          (/
            (*
              (- (block-time-in-seconds) time-of-last-allocation)
              (* reward-rate PRECISION)
            )
          total-stake-amount )
        )
    )
  )

  (defun pending-new-accumulated-reward-per-share:integer
    (reward-per-share:integer total-stake:integer)
    (require-capability (PRIVATE))
    (+ reward-per-share (/ (* (calculate-reward-rate) PRECISION) total-stake))
  )

  (defun search-data-before
    (search-object:object{binary-search-object} _:integer)
      (if (and (at 'found search-object) (not (at 'disputed search-object)))
          search-object
          (let* ((timestamps (at 'timestamps search-object))
                 (target (at 'target search-object))
                 (start (at 'start search-object))
                 (end (at 'end search-object))
                 (middle (/ (- end start) (+ (+ 2 1) start))))

              (if (< (at 'timestamp (at middle timestamps)) target)
                  (if (>= (at 'timestamp (at (+ middle 1) timestamps)) target)
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
                                    }
                                    (enumerate (+ 1 (log 2 (+ middle 1))) 0))
                              { 'found: true
                              , 'target: target
                              , 'start: (+ middle 1)
                              , 'end: middle
                              , 'timestamp-before: (at 'timestamp (at middle timestamps))
                              , 'timestamps: timestamps
                              , 'disputed: (at 'disputed (at middle timestamps))
                              }
                          )
                      )
                      { 'found: false
                      , 'target: target
                      , 'start: (+ middle 1)
                      , 'end: end
                      , 'timestamp-before: (at 'timestamp (at middle timestamps))
                      , 'timestamps: timestamps
                      , 'disputed: (at 'disputed (at middle timestamps))
                      }
                  )
                  (if (< (at 'timestamp (at (- middle 1) timestamps)) target)
                      (if (> middle 0)
                          (fold (search-if-disputed)
                                { 'found: true
                                , 'target: target
                                , 'start: (+ middle 1)
                                , 'end: middle
                                , 'timestamp-before: (at 'timestamp (at middle timestamps))
                                , 'timestamps: timestamps
                                , 'disputed: (at 'disputed (at middle timestamps))
                                }
                                (enumerate (+ 1 (log 2 (+ middle 1))) 0)
                          )
                          { 'found: false
                          , 'target: 0
                          , 'start: 0
                          , 'end: 0
                          , 'timestamp-before: 0
                          , 'timestamps: [{'timestamp:0,'disputed:true}]
                          , 'disputed: true
                          }
                      )
                      { 'found: false
                      , 'target: target
                      , 'start: start
                      , 'end: (- middle 1)
                      , 'timestamp-before: (at 'timestamp (at middle timestamps))
                      , 'timestamps: timestamps
                      , 'disputed: (at 'disputed (at middle timestamps))
                      }
                  )
              )
          )
      )
  )

  (defun search-if-disputed:object{binary-search-object}
    (search-object:object{binary-search-object} _:integer)
      (let ((end (at 'end search-object))
            (timestamp (at 'target search-object))
            (timestamps (at 'timestamps search-object)))
        (if (not (at 'disputed search-object))
            search-object
            { 'found: true
            , 'target: timestamp
            , 'start: 0
            , 'end: (- end 1)
            , 'timestamp-before: (at 'timestamp (at (- end 1) timestamps))
            , 'timestamps: timestamps
            , 'disputed: (at 'disputed (at (- end 1) timestamps))
            }
        )
      )
  )

  (defun data-before:object{binary-search-object}
    (query-id:string timestamp:integer)
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
                              }
                              (enumerate (+ 1 (log 2 end)) 0)
                        )
                    )
                    (fold (search-data-before)
                          { 'found: false
                          , 'start: 0
                          , 'end: end
                          , 'timestamp-before: (at 'timestamp (at end timestamps))
                          , 'timestamps: timestamps
                          , 'target: timestamp
                          , 'disputed: true
                          }
                          (enumerate (+ 1 (log 2 count)) 0)
                    )
                )
            )
      )
  )

)
; *****************************************************************************
; *                                                                           *
; *                         Initialize                                        *
; *                                                                           *
; *****************************************************************************
(if (read-msg "upgrade")
    ["upgrade"]
    [
      (create-table global-variables)
      (create-table governance-table)
      (create-table gov-guard)
      (create-table reports)
      (create-table reports-submitted-count)
      (create-table staker-details)
      (create-table timestamps)
    ]
)
