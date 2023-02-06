; tellorflex on kadena
(namespace (read-msg "ns"))
  (if (read-msg "upgrade")
    "Upgrading contract"

    [
      (enforce-keyset (read-keyset "tellor-admin-keyset"))
      (define-keyset (+ (read-msg "ns") ".tellor-admin-keyset") (read-keyset "tellor-admin-keyset"))
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

; ***************************CAPABILITIES**************************************
  (defcap TELLOR ()
    @doc "Enforce only owner."
    (enforce-guard (keyset-ref-guard (+ (read-msg "ns") ".tellor-admin-keyset")))
  )
  (defcap PRIVATE () true )
  (defcap STAKER (account-name:string)
    (enforce-guard (at 'guard (read staker-details account-name)))
    (compose-capability (PRIVATE)) 
  )
  (defcap GOV_GUARD ()
    (enforce-guard (at "guard" (read gov-guard "gov-guard")))
    (compose-capability (PRIVATE)) 
  )
; ***************************EVENT-CAPS****************************************
  (defcap NewReport 
    (query-id:string
      time:integer
      value:string
      nonce:integer
      query-data:string
      reporter:string)
      @event true
  )
  (defcap NewStakeAmount
    (new-stake-amount:integer)
    @event true
  )
  (defcap NewStaker 
    (staker:string
      amount:integer)
      @event true
  )
  (defcap ReporterSlashed
    (reporter:string
      recipient:string
      slashed-amount:integer)
      @event true
  )
  (defcap StakeWithdrawn
    (staker:string)
    @event true
  )
  (defcap StakeWithdrawRequested
    (staker:string
      amount:integer)
    @event true
  )
  (defcap ValueRemoved
    (query-id:string
      timestamp:integer)
      @event true
  )
; ***************************CONSTANTS*****************************************
  (defconst TIME_BASED_REWARD (* 5 (^ 10 11)))
  (defconst PRECISION (^ 10 12))
  (defconst SEVEN_DAYS 604800)
  (defconst THIRTY_DAYS 2592000)
; ***************************TABLE-SCHEMA**************************************
  (defschema constructor-schema
    tellorflex-account:string
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
; ***************************OBJECT-SCHEMA*************************************
  (defschema timestamp-dispute-object
    timestamp:integer
    disputed:bool)
  (defschema binary-search-object
    found:bool
    target:integer
    start:integer
    end:integer
    timestamp-before:integer
    reports:[object{timestamp-dispute-object}]
    disputed:bool)
; ***************************TABLES********************************************
  (deftable staker-details:{stake-info})
  (deftable reports-submitted-count:{reports-submitted-by-queryid})
  (deftable reports:{report})
  (deftable timestamps:{timestamps-schema})
  (deftable global-variables:{constructor-schema})
  (deftable governance-table:{governance-schema})
  (deftable gov-guard:{gov-guard-schema}) 
; ***************************GUARDS********************************************
  (defun private-user-cap:bool () (require-capability (PRIVATE)) )
  (defun create-flex-guard:guard () (create-user-guard (private-user-cap)) )
; ***************************Global-GETTERS************************************
  (defun tellorflex-account:string ()
    (at "tellorflex-account" (read global-variables "global-vars"))
  )
  (defun get-governance-module:module{i-governance} ()
    (at "governance" (read governance-table "governance"))
  )
  (defun accumulated-reward-per-share ()
    (at "accumulated-reward-per-share" (read global-variables "global-vars"))
  )
  (defun minimum-stake-amount:decimal ()
    (at "minimum-stake-amount" (read global-variables "global-vars"))
  )
  (defun reporting-lock:integer ()
    (at "reporting-lock" (read global-variables "global-vars"))
  )
  (defun reward-rate:integer ()
    (at "reward-rate" (read global-variables "global-vars"))
  )
  (defun stake-amount:integer ()
    (at "stake-amount" (read global-variables "global-vars"))
  )
  (defun stake-amount-dollar-target:integer ()
    (at "stake-amount-dollar-target" (read global-variables "global-vars"))
  )
  (defun staking-rewards-balance:integer ()
    (at "staking-rewards-balance" (read global-variables "global-vars"))
  )
  (defun staking-token-price-query-id:string ()
    (at "staking-token-price-query-id" (read global-variables "global-vars"))
  )
  (defun time-based-reward:integer ()
    TIME_BASED_REWARD
  )
  (defun time-of-last-allocation:integer ()
    (at "time-of-last-allocation" (read global-variables "global-vars"))
  )
  (defun time-of-last-new-value:integer ()
    (at "time-of-last-new-value" (read global-variables "global-vars"))
  )
  (defun total-reward-debt:integer ()
    (at "total-reward-debt" (read global-variables "global-vars"))
  )
  (defun total-stake-amount:integer ()
    (at "total-stake-amount" (read global-variables "global-vars"))
  )
  (defun total-stakers:integer ()
    (at "total-stakers" (read global-variables "global-vars"))
  )
  (defun to-withdraw:integer ()
    (at "to-withdraw" (read global-variables "global-vars"))
  )
; ***************************MAIN-FUNCTIONS************************************
  (defun constructor:string (
    tellorflex-account:string
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

      (coin.create-account tellorflex-account (create-flex-guard))
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
    (emit-event (NewStaker staker amount))
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
      (emit-event (ValueRemoved query-id timestamp))
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
          (emit-event (StakeWithdrawRequested staker amount))
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
                (emit-event (ReporterSlashed reporter recipient stake-amount))
                stake-amount
            )
            (if (>= (+ locked-balance staked-balance) stake-amount)
                (let ((withdraw-bal (- to-withdraw locked-balance)))
                  (update-stake-and-pay-rewards reporter (- staked-balance (- stake-amount locked-balance)))
                  (transfers-from-flex stake-amount recipient)
                  (update staker-details reporter {'locked-balance: 0})
                  (update global-variables 'global-vars {'to-withdraw: withdraw-bal})
                  (emit-event (ReporterSlashed reporter recipient stake-amount))
                  stake-amount
                )
                (let ((amount (+ staked-balance locked-balance))
                       (withdraw-bal (- to-withdraw locked-balance)))

                    (update-stake-and-pay-rewards reporter 0)
                    (transfers-from-flex amount recipient)

                    (update global-variables 'global-vars { 'to-withdraw: withdraw-bal })
                    (update staker-details reporter { 'locked-balance: 0 })
                    (emit-event (ReporterSlashed reporter recipient amount))
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
      (emit-event (NewReport query-id block-time value nonce query-data staker))
    )
  )
  (defun update-stake-amount ()
    @doc "Updates the stake amount after retrieving the 12 hour old price"
    (let* ((twelve-hour-price
            (get-data-before
              (staking-token-price-query-id)
              (- (block-time-in-seconds) (reporting-lock)))))
      (if (= twelve-hour-price {"value": "", "timestamp":0}) 
          (format "No value available for staking token {}"
            [(staking-token-price-query-id)])
          (let* ((stake-amount (stake-amount))
                (minimum-stake-amount (minimum-stake-amount))
                (stake-amount-dollar-target (stake-amount-dollar-target))
                (price-decoded (str-to-int 10 (base64-decode (at "value" twelve-hour-price))))
                (adjusted-stake-amount
                  (/ (* stake-amount-dollar-target PRECISION)  price-decoded)))
            (enforce (and? (<=  (precision 0.01)) (> (precision 1000000.0)) price-decoded)
            "invalid staking token price")
            (if (< adjusted-stake-amount minimum-stake-amount)
              (update global-variables "global-vars" {"stake-amount": minimum-stake-amount})
              (update global-variables "global-vars" {"stake-amount": adjusted-stake-amount})
            )
          )
      )
      (emit-event (NewStakeAmount (stake-amount)))
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

        (update global-variables "global-vars"
        { "to-withdraw": (- (to-withdraw) locked-balance )})
      )
      (emit-event (StakeWithdrawn staker))
    )
  )

; ***************************GETTERS*******************************************
  (defun get-block-number-by-timestamp:integer (query-id:string timestamp:integer)
    @doc "Returns the block number at a given timestamp"
    (try 0 (at 'block-height (read reports (concatenate query-id timestamp))))
  )
  (defun get-current-value (query-id:string)
    @doc "Get last reported value for query id"
    (get-data-before query-id (plus-one (block-time-in-seconds)) )
  )
  (defun get-data-before:object{i-flex.data-before-value} (query-id:string timestamp:integer)
    (with-capability (PRIVATE)
      (let* ((reports 
            (try [] (at "timestamps" (read timestamps query-id))))
          (count (length reports)))
        (if (> count 0)
            (let* ((search 
                    (fold (binary-search)
                      { "found": false, "target": timestamp, "start": 0, "end": (- count 1)
                      , "timestamp-before": 0, "reports": reports, "disputed": true }
                      (enumerate 0 (+ (log 2 count) 1))))
                  (timestamp-before (at "timestamp-before" search)))
              (if (and (at "found" search) (not (at "disputed" search)))
                  { "value": (retrieve-data query-id timestamp-before)
                  , "timestamp": timestamp-before }
                  { "value": "", "timestamp": 0 }
              )
            )
            { "value": "", "timestamp": 0 }
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
      (-
        (precision (coin.get-balance (tellorflex-account)))
        (fold (+) to-withdraw [total-stake-amount staking-rewards-balance])
      )  
    )
  )
  (defun is-in-dispute:bool (query-id:string timestamp:integer)
      (at 'is-disputed (read reports (concatenate query-id timestamp)))
  )
  (defun retrieve-data:string (query-id:string timestamp:integer)
      (at 'value (read reports (concatenate query-id timestamp)))
  )
; ***************************INTERNAL-FUNCTIONS********************************
  (defun transfers-from-flex (amount:integer to:string)
    (require-capability (PRIVATE))
    (if (> amount 0)
        (let ((flex (tellorflex-account)))
          (install-capability (coin.TRANSFER flex to (to-decimal amount)))
          (coin.transfer flex to (to-decimal amount))
        )
        "nothing to transfer"
    )
  )
  (defun transfers-to-flex (amount:integer from:string)
    (require-capability (PRIVATE))
    (if (> amount 0)
        (coin.transfer from (tellorflex-account) (to-decimal amount))
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
; ***************************HELPER-FUNCTIONS**********************************
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
    (require-capability (PRIVATE))
    (with-read global-variables 'global-vars
      { 'total-stake-amount := total-stake-amount
      , 'staking-rewards-balance := staking-rewards-balance
      , 'to-withdraw := to-withdraw
      , 'time-of-last-new-value := time-of-last-new-value
      , 'time-based-reward := time-based-reward }

      (let* ((reward (/ (*
              (- block-time time-of-last-new-value) time-based-reward) 300))
             (contract-balance (coin.get-balance (tellorflex-account)))
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
    (require-capability (PRIVATE))
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
  (defun search-if-disputed:object{binary-search-object}
    (search-obj:object{binary-search-object} _:integer)
    (require-capability (PRIVATE))
    ;  linear search to check if timestamp is in dispute
    (bind search-obj 
      { "target" := timestamp
      , "reports" := reports
      , "end" := end
      , "disputed" := disputed }
      (if (not disputed) search-obj
          (let* ((next (- end 1))
                (next-item (at next reports)))
            { 'found: true
            , 'target: timestamp
            , 'start: 0
            , 'end: next
            , 'timestamp-before: (at 'timestamp next-item)
            , 'reports: reports
            , 'disputed: (at 'disputed next-item)
            }
          )
      )
    )
  )
  (defun binary-search:object{binary-search-object} (search-obj:object{binary-search-object} _:integer)
    (require-capability (PRIVATE))
    (bind search-obj
      { "start" := start, "end" := end, "target" := time
      , "reports" := reports }
      (let* ((search-obj 
              (lambda (found timestamp low high target lis is-disputed)
                { "found": found, "target": timestamp, "start": low
                , "end": high, "timestamp-before": target, "reports": lis
                , "disputed": is-disputed }))
             (found-obj 
              (lambda (s e m) 
                (search-obj true time s e (at "timestamp" (at m reports)) 
                  reports (at "disputed" (at m reports))))))

        (if (>= (at "timestamp" (at start reports)) time)
            (search-obj false time 0 0 0 reports true)
            (if (< (at "timestamp" (at end reports)) time)
                (if (not (at "disputed" (at end reports))) 
                    (found-obj start end end)
                    (fold (search-if-disputed) 
                      (found-obj start end end)
                      (enumerate 0 (+ 1 (log 2 end))))
                )
                (let* ((mid (/ (+ start end) 2))
                        (mid-time (at mid reports)))
                  (if (< (at "timestamp" (at mid reports)) time)
                  ;  return the object and add mid + 1 to start
                      (if (>= (at "timestamp" (at (+ 1 mid) reports)) time)
                      ;  search-if-disputed
                          (if (not (at "disputed" (at mid reports)))
                              (found-obj start end mid)
                              (fold (search-if-disputed) 
                                (found-obj start mid mid)
                                (enumerate 0 (+ 1 (log 2 mid))))
                          )
                          ;  return objec whilst adding mid+1 to start
                          (search-obj false time (+ mid 1) end 0 reports true)
                      )
                      (if (< (at "timestamp" (at (- mid 1) reports)) time)
                        ;  search-if-disputed
                          (let ((mid1 (- mid 1)))
                            (if (not (at "disputed" (at mid1 reports)))
                                (found-obj start end mid1)
                                (fold (search-if-disputed) 
                                  (found-obj start mid1 mid1)
                                  (enumerate 0 (+ 1 (log 2 mid))))
                            )
                          )
                          ;  middle -1 to end
                          (search-obj false time start (- mid 1) 0 reports true)
                      )
                  )            
                )
            )
        )
      )
    )
  )
)
; **************************INITIALIZE*****************************************
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
