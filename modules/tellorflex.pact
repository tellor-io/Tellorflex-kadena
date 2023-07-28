; tellorflex on kadena
(namespace (read-msg "ns"))
  (if (read-msg "upgrade")
    "Upgrading contract"

    [
      (enforce-keyset (read-keyset "admin-keyset"))
      (define-keyset (+ (read-msg "ns") ".admin-keyset") (read-keyset "admin-keyset"))
    ]
  )

(module tellorflex NOT-UPGRADEABLE


  @doc
    "This is a streamlined Tellor oracle system which handles staking, reporting,  \
    \slashing, and user data getters in one contract. This contract is controlled  \
    \by a single address known as 'governance', which could be an externally owned \
    \account or a contract, allowing for a flexible, modular design."

; ***************************CAPABILITIES**************************************
  (defcap NOT-UPGRADEABLE () (enforce false "Enforce non-upgradeability"))
  (defcap TELLOR ()
    "Capability to enforce admin only operations"
    (enforce-guard (keyset-ref-guard (+ (read-msg "ns") ".admin-keyset"))))
  (defcap PRIVATE () 
    "Capability for internal only operations"
    true)
  (defcap STAKER (account-name:string)
    "Capability to enforce staker only operations with 'PRIVATE' capability enabled"
    (enforce-guard (at "guard" (read staker-details account-name)))
    (compose-capability (PRIVATE)))
  (defcap GOV_GUARD ()
    "Capability to enforce governance module only operations with 'PRIVATE' capability enabled"
    (enforce-guard (at "guard" (read gov-guard "gov-guard")))
    (compose-capability (PRIVATE)) )
; ***************************EVENT-CAPS****************************************
  (defcap NewReport 
    (query-id:string
      time:integer
      value:string
      nonce:integer
      query-data:string
      reporter:string)
      "Event for when a new report is submitted"
      @event true)
  (defcap NewStakeAmount
    (new-stake-amount:integer)
    "Event for when stake amount is changed when (updateStakeAmount) is called"
    @event true)
  (defcap NewStaker 
    (staker:string
      amount:integer)
    "Event for when a reporter stakes a deposit"
    @event true)
  (defcap ReporterSlashed
    (reporter:string
      recipient:string
      slashed-amount:integer)
    "Event for when a reporters stake is slashed due to a dispute triggered by governance module"
    @event true)
  (defcap StakeWithdrawn
    (staker:string)
    "Event for when a reporter withdraws their stake"
    @event true)
  (defcap StakeWithdrawRequested
    (staker:string
      amount:integer)
    "Event for when a reporter requests to withdraw their stake"
    @event true)
  (defcap ValueRemoved
    (query-id:string
      timestamp:integer)
    "Event for when a value is removed due to a dispute triggered by governance module"
    @event true)
; ***************************CONSTANTS*****************************************
  (defconst TIME_BASED_REWARD (* 5 (^ 10 17))
    "Amount of TRB rewards released per 5 minutes")
  (defconst PRECISION (^ 10 18)
    "Eighteen decimals")
  (defconst SEVEN_DAYS 604800
    "Seven days in seconds")
  (defconst THIRTY_DAYS 2592000
    "Thirty days in seconds")
; ***************************TABLE-SCHEMA**************************************
  (defschema constructor-schema
    "Global variables schema that must be initialized before contract can be used"
    tellorflex-account:string  ;account name for oracle contract token account
    accumulated-reward-per-share:integer  ;accumulated staking reward per staked token
    minimum-stake-amount:integer  ;minimum amount of tokens required to stake
    reporting-lock:integer  ;base amount of time before a reporter is able to submit a value again
    stake-amount:integer  ;minimum amount required to be a staker
    stake-amount-dollar-target:integer  ;amount of US dollars required to be a staker
    reward-rate:integer  ;total staking rewards released per second
    staking-rewards-balance:integer  ;total amount of staking rewards
    staking-token-price-query-id:string  ;staking token SpotPrice queryId, used for updating stakeAmount
    time-of-last-allocation:integer  ;time of last update to accumulated-reward-per-share
    time-of-last-new-value:integer  ;time of the last new submitted value, originally set to the block timestamp
    total-reward-debt:integer  ;staking reward debt, used to calculate real staking rewards balance
    total-stake-amount:integer  ;total amount of tokens locked in contract (via stake)
    total-stakers:integer  ;total number of stakers with at least stakeAmount staked, not exact
    to-withdraw:integer)  ;total amount locked for withdrawal
  (defschema report
    "Submission reports' schema" ; Report struct in solidity contract
    index:integer  ;index for reports location in list of reports
    query-id:string  ;report identifier
    timestamp:integer  ;timestamp of when report was submitted
    block-height:integer  ;block where report was added
    value:string  ;submission's reported value
    reporter:string  ;reporter's identifier that submitted report
    is-disputed:bool)  ;report's dispute status 
  (defschema stake-info
    "Reporter's stake info schema"
    start-date:integer  ;date staker staked token's in contract
    staked-balance:integer  ;staker's token balance currently staked and used to report
    locked-balance:integer  ;staker's token balance currently locked for withdrawal not used to report
    reward-debt:integer  ;tracks rewards to be paid out per staker, used for calculating staking reward
    reporter-last-timestamp:integer  ;staker's last reported timestamp
    reports-submitted:integer  ;staker's count of reports submitted
    start-vote-count:integer  ;total number of governance votes
    start-vote-tally:integer  ;staker vote tally 
    is-staked:bool  ;staker's status
    guard:guard)  ;staker's keyset guard, used to enforce staker
  (defschema reports-submitted-by-queryid
    "Reports per query id schema"
    reports-submitted-by-queryid:integer)  ;count of reports per query id
  (defschema timestamps-schema
    "Reports' timestamps schema"
    timestamps:[object{timestamp-dispute-object}])  ;list of key-value of timestamp and dispute status
  (defschema governance-schema
    "Governance interface schema"
    governance:module{i-governance})
  (defschema gov-guard-schema
    "Governance guard schema"
    guard:guard)
; ***************************OBJECT-SCHEMA*************************************
  (defschema timestamp-dispute-object
    timestamp:integer  ;report's timestamp
    disputed:bool)  ;dispute status
  (defschema binary-search-object
    "Used for finding an undisputed report"
    found:bool
    target:integer
    start:integer
    end:integer
    timestamp-before:integer
    reports:[object{timestamp-dispute-object}]
    disputed:bool)
  (defschema data-before-value
    "Undisputed report's timestamp and value"
    timestamp:integer
    value:string)
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
    "Getter of Tellorflex account identifier in token contract"
    (at "tellorflex-account" (read global-variables "global-vars"))
  )
  (defun get-governance-module:module{i-governance} ()
    "Getter of Governance module"
    (at "governance" (read governance-table "governance"))
  )
  (defun accumulated-reward-per-share ()
    "Getter of accumulated reward in contract"
    (at "accumulated-reward-per-share" (read global-variables "global-vars"))
  )
  (defun minimum-stake-amount:decimal ()
    "Getter of minimum stake amount required to stake"
    (at "minimum-stake-amount" (read global-variables "global-vars"))
  )
  (defun reporting-lock:integer ()
    "Getter of seconds required to wait between reports per stake"
    (at "reporting-lock" (read global-variables "global-vars"))
  )
  (defun reward-rate:integer ()
    "Getter of reward amount per second"
    (at "reward-rate" (read global-variables "global-vars"))
  )
  (defun stake-amount:integer ()
    "Getter of minimum amount required to be a staker"
    (at "stake-amount" (read global-variables "global-vars"))
  )
  (defun stake-amount-dollar-target:integer ()
    "Getter of minimum amount required to be a staker in US dollars"
    (at "stake-amount-dollar-target" (read global-variables "global-vars"))
  )
  (defun staking-rewards-balance:integer ()
    "Getter of total amount of staking rewards in contract"
    (at "staking-rewards-balance" (read global-variables "global-vars"))
  )
  (defun staking-token-price-query-id:string ()
    "Getter of staking token's query id (TRB)"
    (at "staking-token-price-query-id" (read global-variables "global-vars"))
  )
  (defun time-based-reward:integer ()
    "Getter of amount of token rewards released per 5 minutes"
    TIME_BASED_REWARD
  )
  (defun time-of-last-allocation:integer ()
    "Getter of when rewards were last calculated/allocated"
    (at "time-of-last-allocation" (read global-variables "global-vars"))
  )
  (defun time-of-last-new-value:integer ()
    "Getter of when a report was last submitted to oracle"
    (at "time-of-last-new-value" (read global-variables "global-vars"))
  )
  (defun total-reward-debt:integer ()
    "Getter of total reward debt, thats used when calculating staking rewards"
    (at "total-reward-debt" (read global-variables "global-vars"))
  )
  (defun total-stake-amount:integer ()
    "Getter of the total stake amount locked in contract"
    (at "total-stake-amount" (read global-variables "global-vars"))
  )
  (defun total-stakers:integer ()
    "Getter of number of stakers"
    (at "total-stakers" (read global-variables "global-vars"))
  )
  (defun to-withdraw:integer ()
    "Getter of amount of tokens locked in contract"
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

    @doc "Set global variables and initialize contract"

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
          , 'time-of-last-allocation: 0
          , 'time-of-last-new-value: (block-time-in-seconds)
          , 'total-reward-debt: 0
          , 'total-stake-amount: 0
          , 'total-stakers: 0
          , 'to-withdraw: 0}
        )
      )
      ;  create contract's account in token contract using the private guard
      ;  allows contract to hold staked tokens and release them to stakers when withdrawing
      ;  or disperse rewards appropriately. Funds are only controlled by contract's logic (no owner)
      (f-TRB.create-account tellorflex-account (create-flex-guard))
      "Global variables set!"
    )
  )
  (defun init-gov-guard:string (guard:guard)
    "Register governance module guard in this contract"
    ; fails if governance hasn't been initialized
    (get-governance-module)
    ;  can only be called by admin once and has to be called from governance contract
    (enforce-guard (keyset-ref-guard (+ (read-msg "ns") ".admin-keyset")))
    (insert gov-guard 'gov-guard {'guard: guard})
    "Gov guard registered"
  )
  (defun init (governance:module{i-governance})
    @doc "Allows the owner to initialize the governance (flex addy needed for governance deployment)"
    (with-capability (TELLOR)
      (insert governance-table 'governance { 'governance: governance })
      (governance::register-gov-guard))
  )
  (defun add-staking-rewards (account:string amount:integer)
    @doc "Funds the flex contract with staking rewards (autopay and miniting) anyone can add at will"
    (with-capability (PRIVATE)
      (enforce (> amount 0) "Amount can't be less than or equal to zero")
      (transfers-to-flex amount account)
      (update-rewards)
      (with-read global-variables 'global-vars { 'staking-rewards-balance := bal }
        (update global-variables 'global-vars { 'staking-rewards-balance: (+ bal amount) }))
      (update global-variables 'global-vars { 'reward-rate: (/ (calculate-reward-rate) THIRTY_DAYS)})
    )
  )
  (defun add-staker (staker:string guard:guard)
    "Internal function to add new stakers and initialize their info"
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
  (defun deposit-stake (
    staker:string  ;staker identifier
    guard:guard  ;staker's guard to be stored. To be enforced when submitting values
    amount:integer)  ;amount of tokens staker wishes to stake
    @doc "Allows a reporter to submit stake"
    ;  assert amount is >= 0
    (enforce (>= amount 0) "Amount can't be less than zero")
    ;  pull governance module from db
    (let ((governance:module{i-governance} (get-governance-module))
          (block-time (block-time-in-seconds)))
      ; with private cap add staker plus guard to contract's db
      ; if staker not registered
      (with-capability (PRIVATE) (add-staker staker guard) )

      (with-capability (STAKER staker)
        ;  after adding staker using STAKER cap pull information from db
        ;  and update stake amounts
       (with-read staker-details staker
         { 'locked-balance := locked-balance , 'staked-balance := staked-balance }
         (if (> locked-balance 0)
        ;  if staker has a locked balance and covers the full amount the staker wants to stake
        ;  then use that amount and update staker's balance with that amount
            (if (>= locked-balance amount)
              (let ((updated-locked-balance (- locked-balance amount))
                    (updated-withdraw-amount (- (to-withdraw) amount)))
                  (update staker-details staker
                  { 'locked-balance: updated-locked-balance})
                  (update global-variables 'global-vars
                  { 'to-withdraw: updated-withdraw-amount})
              )
              ;  else subtract locked balance from amount and transfer the remaining amount
              ;  from staker's account
              (let ((updated-withdraw-amount (- (to-withdraw) locked-balance))
                    (updated-amount (- amount locked-balance)))
                  ;  if the amount is greater than the locked balance,
                  ;  transfer the difference and update the locked balance to 0
                  (transfers-to-flex updated-amount staker)
                  (update staker-details staker
                  { 'locked-balance: 0})
                  (update global-variables 'global-vars
                  { 'to-withdraw: updated-withdraw-amount})
              )
            )
            ;  if locked balance is 0
            (let ((vote-count (governance::get-vote-count))
                  (vote-tally (governance::get-vote-tally-by-address staker)))
              ;  if the staked balance is 0, update the start vote count and tally
              ;  in staker's info by pulling counts from governance contract
              ;  used when calculating rewards
              (if (= staked-balance 0)
                  (update staker-details staker
                    { 'start-vote-count: vote-count
                    , 'start-vote-tally: vote-tally
                    })
                    ;  else nothing to be updated
                  "Staked balance is not 0 vote tally and count not updated"
              )
              ;  transfer full amount from staker's account into contract
              (transfers-to-flex amount staker)
            )
          )
          ;  update staking rewards and transfer any pending rewards and update
          ; staker's stake amount
          (update-stake-and-pay-rewards staker
            (+ (at 'staked-balance (read staker-details staker)) amount))
          ;  reset stakers start date to now
          (update staker-details staker { 'start-date: block-time } )
       )
     )
    (emit-event (NewStaker staker amount))
   )
  )
  (defun remove-value:string (query-id:string timestamp:integer)
    @doc "Remove disputed value only called by governance"
    ;  enforce governance guard (since only callable by governance module)
    ;  and remove value by updating report's value to empty string and updating 
    ;  dispute status. Also update reports' timestamps list at index edit dispute status.
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
    @doc "Allows a reporter to request an amount of their stake to withdraw"
    (enforce (> amount 0) "Amount can't be less than or equal to zero")
    (let ((block-time (block-time-in-seconds))
          (to-withdraw (to-withdraw)))
         (with-capability (STAKER staker)
        ;   with staker capability pull staker info from db
           (with-read staker-details staker
             { "staked-balance" := staked-balance
             , "locked-balance" := locked-balance
             , "start-date" := start-date
             }
             (enforce (>= staked-balance amount) "Insufficient staked balance")
             (update-stake-and-pay-rewards staker (- staked-balance amount))
            ;   update staker info locked balance and reset start date to now 
            ;  7 days from this date until allowed to withdraw stake
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
    "Slash reporter when a dispute of report begins in governance module"
    ;  Called by governance contract only 
    (with-capability (GOV_GUARD)
      (with-read staker-details reporter
        { 'staked-balance := staked-balance ,'locked-balance := locked-balance }
        ;  reporter has to have a balance
        (enforce (> (+ staked-balance locked-balance) 0) "Zero staker balance")
        (let* ((stake-amount (stake-amount))
               (to-withdraw (to-withdraw)))
        ;  if reporter has a locked balance then transfer to recipient from locked balance
        (if (>= locked-balance stake-amount)
            (let ((locked-bal (- locked-balance stake-amount))
                  (withdraw-bal (- to-withdraw stake-amount)))
                (transfers-from-flex stake-amount recipient)
              ;  update reporters locked balance and global to withdraw amount
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
    ; identifier for specific data feed. 
    ; Equals hash of base64 encoded string ie. (hash (base64-encode "{SpotPrice: [trb,usd]}"))
    (query-id:string
     value:string  ; base64 encoded string
     nonce:integer ; either 0 or next index in reports
     query-data:string  ; base64 encoded string ie. (base64-encode "{SpotPrice: [trb,usd]}")
     staker:string)  ;staker identifier that was used to stake
    @doc "Enables staked reporters to submit values to the oracle"
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
              "Nonce must match timestamps list length or be zero")

           (with-read staker-details staker
             { 'staked-balance := staked-balance
             , 'reporter-last-timestamp := reporter-last-timestamp
             , 'reports-submitted := reports-submitted }

             (enforce (>= staked-balance stake-amount)
               "balance must be greater than stake amount")
              ;  forces reporter to abide by reporting lock
             (enforce (>
               (* 1000 (- block-time reporter-last-timestamp))
               (/ (* 1000 reporting-lock) (/ staked-balance stake-amount)))
               "still in reporter time lock, please wait!")
            ;  can't report same timestamp and queryId due to key existing in table
            ;  blocks double reporting of timestamps
             (insert reports (concatenate query-id block-time)
               { 'index: (length timestamps-lis)
               , 'query-id: query-id
               , 'timestamp: block-time
               , 'block-height: block-height
               , 'value: value
               , 'reporter: staker
               , 'is-disputed: false
               })
            ;  add timestamp to list of timestamps for the query id being reported
             (write timestamps query-id
               { 'timestamps:
               (+ timestamps-lis [{'timestamp: block-time, 'disputed: false}])}
             )
            ;  transfer to reporter any time based rewards in contract
            (transfers-from-flex (calculate-time-based-reward block-time) staker)

            (update global-variables 'global-vars
               { 'time-of-last-new-value: block-time})
            ;  update reporters last timestamp and total reports count
            (update staker-details staker
              { 'reports-submitted: (plus-one reports-submitted)
              , 'reporter-last-timestamp: block-time})
            ;  update reporters submission count for a query id
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
    "Updates the stake amount after retrieving the 12 hour old price"
    ;  get the 12 hour old valid price of staking token and calculate a current stake amount
    ;  has to be called by anyone to update stake amount
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
    "Withdraws a reporter's stake after the lock period expires"
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
    "Returns the block number at a given timestamp"
    (try 0 (at 'block-height (read reports (concatenate query-id timestamp))))
  )
  (defun get-current-value (query-id:string)
    "Get last reported value for query id"
    (get-data-before query-id (plus-one (block-time-in-seconds)) )
  )
  (defun get-data-before:object{data-before-value} (query-id:string timestamp:integer)
    "Getter of a report for a given query id before a given timestamp"
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
    "Get the number of values submitted for a query id"
    (let ((timestamps (at 'timestamps (read timestamps query-id))))
      (length timestamps) )
  )
  (defun get-pending-reward-by-staker:integer (staker:string)
    "Returns the pending staking reward for a given address"
    (with-capability (PRIVATE) (with-read staker-details staker
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
          (if (> number-of-votes 0) (/ (* pending-reward (- vote-tally start-vote-count)) number-of-votes) 0))))
  )
  (defun get-real-staking-rewards-balance:integer ()
    "Returns the real staking rewards balance after accounting for unclaimed rewards"
    (with-capability (PRIVATE) (with-read global-variables 'global-vars
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
  )
  (defun get-report-details (query-id:string timestamp:integer)
    "Get reporter and status given query id and timestamp"
    (with-read reports (concatenate query-id timestamp)
      { 'reporter := reporter , 'is-disputed := is-disputed }
      { 'reporter: reporter , 'disputed: is-disputed }
    )
  )
  (defun get-reporter-by-timestamp:string (query-id:string timestamp:integer)
    "Get reporter of a report given query id and timestamp"
    (at 'reporter (read reports (concatenate query-id timestamp)))
  )
  (defun get-reporter-last-timestamp (reporter:string)
    "Get last reported timestamp by a reporter"
    (at 'reporter-last-timestamp (read staker-details reporter))
  )
  (defun get-reports-submitted-by-address:integer (reporter:string)
    "Get count of reports submitted by reporter"
    (at 'reports-submitted (read staker-details reporter))
  )
  (defun get-reports-submitted-by-address-and-queryId:integer
    (reporter:string query-id:string)
    "Get count of a specific report by a reporter given reporter and query id"
    (at 'reports-submitted-by-queryid
      (read reports-submitted-count (+ query-id reporter)))
  )
  (defun get-staker-info:object (reporter:string)
    "Get staker info details"
    (read staker-details reporter)
  )
  (defun get-timestampby-query-id-and-index:integer (query-id:string index:integer)
    "Gets the timestamp for the value based on their index"
    (at 'timestamp (at index (at 'timestamps (read timestamps query-id))))
  )
  (defun get-timestamp-index-by-timestamp:integer (query-id:string timestamp:integer)
    "Get index of a report given a query id for list of reports"
    (at 'index (read reports (concatenate query-id timestamp)))
  )
  (defun get-total-time-based-rewards-balance:integer ()
    "Get the balance of rewards accrued from time based rewards"
    (with-read global-variables 'global-vars
      { 'total-stake-amount := total-stake-amount
      , 'staking-rewards-balance := staking-rewards-balance
      , 'to-withdraw := to-withdraw
      }
      (-
        (precision (f-TRB.get-balance (tellorflex-account)))
        (fold (+) to-withdraw [total-stake-amount staking-rewards-balance])
      )  
    )
  )
  (defun is-in-dispute:bool (query-id:string timestamp:integer)
    "Get dispute status of a report given a query id and timestamp"
      (at 'is-disputed (read reports (concatenate query-id timestamp)))
  )
  (defun retrieve-data:string (query-id:string timestamp:integer)
    "Get value given a query id and timestamp, doesn't care about dispute status"
      (at 'value (read reports (concatenate query-id timestamp)))
  )
; ***************************INTERNAL-FUNCTIONS********************************
  (defun transfers-from-flex (amount:integer to:string)
    "Internal function to transfer tokens from contract account to user"
    (require-capability (PRIVATE))
    (if (> amount 0)
        (let ((flex (tellorflex-account)))
          (install-capability (f-TRB.TRANSFER flex to (to-decimal amount)))
          (f-TRB.transfer flex to (to-decimal amount))
        )
        "nothing to transfer"
    )
  )
  (defun transfers-to-flex (amount:integer from:string)
    "Internal function to transfer tokens from user to tellorflex account"
    (require-capability (PRIVATE))
    (enforce (>= amount 0) "Amount can't be less than zero")
    (if (> amount 0)
        (f-TRB.transfer from (tellorflex-account) (to-decimal amount))
        "nothing to transfer"
    )
  )
  (defun update-rewards ()
    "Internal function to update accumulated staking rewards per staked token"
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
    "Internal function called whenever a user's stake amount changes. First updates staking rewards, \
    \ transfers pending rewards to user's address, and finally updates user's stake amount  \
    \ and other relevant variables."
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
        ;  if address already has a staked balance, calculate and transfer pending rewards
            (let* ((governance:module{i-governance} (get-governance-module))
                   (pending-reward
                      (-
                        (/ (* staked-balance accumulated-reward-per-share) PRECISION )
                        reward-debt))
                   (vote-count (governance::get-vote-count))
                   (number-of-votes (- vote-count start-vote-count)))

              (if (> number-of-votes 0)
              ;  staking reward = pending reward * voting participation rate
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
          (if (not staked)
            (let ((stakers-total (plus-one (total-stakers))))
              (update global-variables 'global-vars { 'total-stakers: stakers-total })
              (update staker-details staker { 'is-staked: true })
            )
            "is staked!"
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
    "Internal function, gets the updated accumulated reward per share"
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
  (defun calculate-time-based-reward:integer (block-time:integer)
    (require-capability (PRIVATE))
    (with-read global-variables 'global-vars
      { 'total-stake-amount := total-stake-amount
      , 'staking-rewards-balance := staking-rewards-balance
      , 'to-withdraw := to-withdraw
      , 'time-of-last-new-value := time-of-last-new-value }

      (let* ((reward (/ (*
              (- block-time time-of-last-new-value) TIME_BASED_REWARD) 300))
             (contract-balance (f-TRB.get-balance (tellorflex-account)))
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
    "Internal function, helper for search if a report is disputed"
    (require-capability (PRIVATE))
    ;  linear search to check if timestamp is in dispute
    (bind search-obj 
      { "target" := timestamp
      , "reports" := reports
      , "end" := end
      , "disputed" := disputed }
      (if (not disputed) search-obj
          (let* (
            (next (if (> end 0) (- end 1) end))
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
    "Internal function binary search for an undisputed report given a timestamp and a query id"
    (require-capability (PRIVATE))
    ;  create variables for each key in search obj dictionary parameter
    (bind search-obj
      { "start" := start, "end" := end, "target" := time, "reports" := reports }
      (let* (
        (search-obj (lambda (found timestamp low high target lis is-disputed)
          { "found": found, "target": timestamp, "start": low, "end": high,
            "timestamp-before": target, "reports": lis, "disputed": is-disputed }))
        (found-obj (lambda (s e m) 
                (search-obj true time s e (at "timestamp" (at m reports)) 
                  reports (at "disputed" (at m reports))))))

        (if (>= (at "timestamp" (at start reports)) time)
        ;  not found cause first timestamp in existing reports is > user input
        ;  so no before timestamps
            (search-obj false time 0 0 0 reports true)
            (if (< (at "timestamp" (at end reports)) time)
            ; check wether user input is > last existing report's timestamp
            ;  if so no need to look further but still need to check if disputed
                (if (not (at "disputed" (at end reports)))
                ;  if not disputed item is found
                    (found-obj start end end)
                    ;  else linearly search for a non disputed item
                    ;  but cut the list to search to log2 n+1
                    (fold (search-if-disputed) 
                      (found-obj start end end)
                      (enumerate 0 (log 2 (+ 1 end))))
                )
                ;  binary search if list if timestampbefore isn't found
                (let ((mid (/ (+ start end) 2)))
                  (if (< (at "timestamp" (at mid reports)) time)
                  ;  return the object and add mid + 1 to start
                      (if (>= (at "timestamp" (at (+ 1 mid) reports)) time)
                      ;  search-if-disputed
                          (if (not (at "disputed" (at mid reports)))
                              (found-obj start end mid)
                              (fold (search-if-disputed) 
                                (found-obj start mid mid)
                                (enumerate 0 (+ 1 (log 2 (+ 1 mid)))))
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
                                  (enumerate 0 (+ 1 (log 2 (+ 1 mid)))))
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
      (constructor 
        "tellorflex"
        43200
        (* 500 PRECISION)
        (round (* (read-decimal "token-price") PRECISION))
        (* 10 PRECISION)
        (hash (base64-encode "{SpotPrice: [trb,usd]}")))
    ]    
)
