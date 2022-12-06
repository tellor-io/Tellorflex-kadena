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
    (implements i-flex)
; *****************************************************************************
; *                                                                           *
; *                          Constants                                        *
; *                                                                           *
; *****************************************************************************
  (defconst TELLOR_FLEX_ACCOUNT "tellorflex"
      @doc "Account name of the oracle account that holds and disburses funds.")

  (defconst PRECISION (^ 10 18)
    @doc "PRECISION")

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
  (defschema timestamp-dispute
    timestamp:integer
    disputed:bool)
  (defschema timestamps-schema
    timestamps:[object{timestamp-dispute}])
  (defschema governance-schema
    governance:module{tellor-governance})
  (defschema gov-guard-schema
    guard:guard)
; *****************************************************************************
; *                                                                           *
; *                          Tables                                           *
; *                                                                           *
; *****************************************************************************
  (deftable staker-details:{stake-info})
  (deftable reports-submitted-count:{reports-submitted-by-queryid})
  (deftable reports:{report})
  (deftable timestamp-table:{timestamps-schema})
  (deftable global-table:{constructor-schema})
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

  (defcap PRIVATE ()
    true
  )

  (defcap STAKER (account-name:string)
    (enforce-keyset (read-keyset account-name))
    (enforce-guard (at 'guard (read staker-details account-name)))
  )

  (defcap GOV ()
    (enforce-guard (at 'guard (read gov-guard 'gov-guard)))
  )
; *****************************************************************************
; *                                                                           *
; *                          Guard                                            *
; *                                                                           *
; *****************************************************************************
  (defun capping ()
    (require-capability (PRIVATE))
  )

  (defun create-guard ()
    (create-user-guard (capping))
  )
; *****************************************************************************
; *                                                                           *
; *                          Main functions                                   *
; *                                                                           *
; *****************************************************************************
  (defun constructor (
    token:module{fungible-v2}
    reporting-lock:integer
    stake-amount-dollar-target:integer
    staking-token-price:integer
    minimum-stake-amount:integer
    staking-token-price-query-id:string)

    @doc "Constructor"

    (with-capability (TELLOR)
      (let ((potential-amount (/ stake-amount-dollar-target staking-token-price)))
        (insert global-table 'global-vars
          { 'token: token
          , 'accumulated-reward-per-share: 0
          , 'minimum-stake-amount: minimum-stake-amount
          , 'reporting-lock: reporting-lock
          , 'reward-rate: 0
          , 'stake-amount: (if (<  potential-amount minimum-stake-amount) minimum-stake-amount potential-amount)
          , 'stake-amount-dollar-target: stake-amount-dollar-target
          , 'staking-rewards-balance: 0
          , 'staking-token-price-query-id: staking-token-price-query-id
          , 'time-based-reward: (* 5 (^ 10 17))
          , 'time-of-last-allocation: 0
          , 'time-of-last-new-value: (block-time-in-seconds)
          , 'total-reward-debt: 0
          , 'total-stake-amount: 0
          , 'total-stakers: 0
          , 'to-withdraw: 0}
        )
      )
    )
  )

  (defun init-gov-guard:string (guard:guard)
    ; fails if governance hasn't been initialized
    (get-governance-module)
    (enforce-keyset "free.tellor-admin-keyset")
    (insert gov-guard 'gov-guard {'guard: guard})
  )

  (defun init (governance:module{tellor-governance})
    @doc "Allows the owner to initialize the governance (flex addy needed for governance deployment)"
    (with-capability (TELLOR)
      (insert governance-table 'governance { 'governance: governance }))
  )

  (defun add-staking-rewards (account:string amount:decimal)
    @doc "Funds the flex contract with staking rewards (autopay and miniting) anyone can add at will"

    (let ((token:module{fungible-v2} (token)))
      (token::transfer-create account TELLOR_FLEX_ACCOUNT (create-guard) amount)

      (with-capability (PRIVATE)
        (update-rewards) )

      (update global-table 'global-vars
        { 'staking-rewards-balance: (use-precision amount)
        , 'reward-rate: (/ (calculate-reward-rate) (round (days 30)))
        })
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
    (enforce (> amount 0) "Amount must be greater than 0")

      (let ((governance:module{tellor-governance}(try "governance not set" (get-governance-module)))
            (block-time (block-time-in-seconds))
            (token:module{fungible-v2} (token)))

      (with-capability (PRIVATE)
        (add-staker staker guard))
    (with-capability (STAKER staker) ;TODO: consolidate
     (with-read staker-details staker
       { 'locked-balance := locked-balance , 'staked-balance := staked-balance }
       (if (> locked-balance 0)
          (if (>= locked-balance amount)
            [
            (update staker-details staker
              { 'locked-balance: (- locked-balance amount)})
            (update global-table 'global-vars
              { 'to-withdraw: (- (get-to-withdraw-amount) amount)})
            ]
            [
            (token::transfer staker TELLOR_FLEX_ACCOUNT (- amount locked-balance))
            (update global-table 'global-vars
             { 'to-withdraw: (- (get-to-withdraw-amount) locked-balance) })
            (update staker-details staker { 'locked-balance: 0 })
            ])

          [(if ( = staked-balance 0)
              (update staker-details staker
                { 'start-vote-count: (governance::get-vote-count)
                , 'start-vote-tally: (governance::get-vote-tally-by-address staker)
                , 'staked-balance: (+ amount staked-balance)})
              "No else just ifs"
            )
            (token::transfer staker TELLOR_FLEX_ACCOUNT (/ (/ (- amount locked-balance)(^ 10  18)) 1.0))
            ]
          )
        )
;  change token
      (with-capability (PRIVATE)
        (update-stake-and-pay-rewards staker (+ (at 'staked-balance (read staker-details staker)) amount)))
      (update staker-details staker { 'start-date: block-time })
      )
    )
  )

  (defun remove-value (query-id:string timestamp:integer)
    @doc "Remove disputed value only by governance"
    (with-capability (GOV)
      (enforce (not (is-in-dispute query-id timestamp)) "Value already disputed")
      (update reports (concatenate query-id timestamp)
        { 'value: "" , 'is-disputed: true })
    )
  )

  (defun request-staking-withdraw (staker:string amount:integer)
    @doc "Allows a reporter to request to withdraw their stake"
    (let ( (block-time (block-time-in-seconds))
           (guard (at 'guard (read staker-details staker)))
           (to-withdraw (at 'to-withdraw (read global-table 'global-vars)))
           )
        (with-capability (STAKER staker)
         (with-read staker-details staker
           { "staked-balance" := staked-balance
           , "locked-balance" := locked-balance
           , "start-date" := start-date
           }
           (enforce (>= staked-balance amount) "Insufficient staked balance")
           ; TODO: governance contract
           (with-capability (PRIVATE)
             (update-stake-and-pay-rewards staker (- staked-balance amount)))
           (update staker-details staker
             { "locked-balance": (+ locked-balance amount)
             , "start-date": block-time})
           )
           (update global-table 'global-vars
            { 'to-withdraw: (+ to-withdraw amount)})
      )
    )
  )

  (defun slash-reporter (reporter:string recipient:string)
    (with-capability (GOV)
      (with-read staker-details reporter
        { 'staked-balance := staked-balance ,'locked-balance := locked-balance }
        (enforce (> (+ staked-balance locked-balance) 0) "Zero staker balance")
        (let* ((token:module{fungible-v2} (token))
               (stake-amount (at 'stake-amount (read global-table 'global-vars)))
               (to-withdraw (at 'to-withdraw (read global-table 'global-vars))))

        (if (>= locked-balance stake-amount)
            [
            (update staker-details reporter
              {'locked-balance: (- locked-balance stake-amount)})
            (update global-table 'global-vars
              {'to-withdraw: (- to-withdraw stake-amount)})
            (token::transfer TELLOR_FLEX_ACCOUNT recipient (/ (/ stake-amount 1.0) (^ 10  18)))
            ]
            (if (>= (+ locked-balance staked-balance) stake-amount)
              [
                (update-stake-and-pay-rewards reporter (- staked-balance (- stake-amount staked-balance)))
                (update global-table 'global-vars
                  { 'to-withdraw: (- to-withdraw locked-balance) })
                (update staker-details reporter { 'locked-balance: 0 })
                (token::transfer TELLOR_FLEX_ACCOUNT recipient (/ (/ stake-amount 1.0) (^ 10  18)))
              ]
              [
              (update-stake-and-pay-rewards reporter 0)
              (update global-table 'global-vars
                { 'to-withdraw: (- to-withdraw locked-balance) })
              (update staker-details reporter { 'locked-balance: 0 })
              (token::transfer TELLOR_FLEX_ACCOUNT recipient (/ (/ (+ staked-balance locked-balance) 1.0) (^ 10  18)))
              ]
            )
          )
        )
      )
    )
  )

  (defun submit-value
    ( query-id:string
      nonce:integer
      query-data:string
      value:string
      staker:string
    )
    @doc "Enables staked reporters to submit values into the oracle"
    (enforce (= query-id (hash query-data)) "Query id not hash of query data")
    (enforce (>= nonce 0) "Nonce too low")
    (enforce (!= value "") "Value empty!")
    (let ((block-time (block-time-in-seconds))
          (stake-amount (get-stake-amount))
          (block-height (block-height))
          (guard (at 'guard (read staker-details staker))))
        (with-capability (STAKER staker)

          (with-default-read timestamp-table query-id
            { 'timestamps: [] }
            { 'timestamps := timestamps }
            (enforce (or (= nonce (length timestamps)) (= nonce 0)) "Nonce must match timestamps list length")

           (with-read staker-details staker
             { 'staked-balance := staked-balance
             , 'reporter-last-timestamp := reporter-last-timestamp
             , 'reports-submitted := reports-submitted }

             (enforce (>= staked-balance stake-amount) "Don't have enough stake")

             (enforce (>
               (- block-time reporter-last-timestamp)(/ 43200 (/ staked-balance stake-amount)))
               "Still in reporter time lock")

             (insert reports (concatenate query-id block-time)
               { 'index: (- (length timestamps) 1)
               , 'query-id: query-id
               , 'timestamp: (block-time-in-seconds)
               , 'block-height: 0
               , 'value: value
               , 'reporter: staker
               , 'is-disputed: false
               })

             (write timestamp-table query-id
               { 'timestamps: (+ timestamps [{'timestamp: block-time, 'disputed: false}])})


            (update staker-details staker
              { 'reports-submitted: (+ reports-submitted 1)
              , 'reporter-last-timestamp: block-time})

            (with-default-read reports-submitted-count (+ query-id staker)
              { 'reports-submitted-by-queryid: 0}
              { 'reports-submitted-by-queryid := reports-submitted-by-queryid }
              (write reports-submitted-count (+ query-id staker)
                { 'reports-submitted-by-queryid: (+ reports-submitted-by-queryid 1)})
            )

        )
        (update global-table 'global-vars
          { 'time-of-last-new-value: block-time})
        )
      )
    )
  )

  (defun update-stake-amount ()
    @doc "Updates the stake amount after retrieving the 12 hour old price"
    (let* ((twelve-hour-price (get-data-before (staking-token-query-id) (round (- (block-time-in-seconds) (hours 12)))))
           (price-decoded (str-to-int 10 (base64-decode (at 'value twelve-hour-price))))
           (stake-amount (at 'stake-amount (read global-table 'global-vars)))
           (minimum-stake-amount (at 'minimum-stake-amount (read global-table 'global-vars)))
           (stake-amount-dollar-target (at 'stake-amount-dollar-target (read global-table 'global-vars)))
           (adjusted-stake-amount (/ (* stake-amount-dollar-target PRECISION) price-decoded)))
; TODO: validate price
        (if (< adjusted-stake-amount minimum-stake-amount)
          (update global-table 'global-vars {'stake-amount: minimum-stake-amount})
          (update global-table 'global-vars {'stake-amount: adjusted-stake-amount})
          )
    )
  )

  (defun withdraw-stake (staker:string)
    @doc "Withdraws a reporter's stake after the lock period expires"
    (let ( (token:module{fungible-v2} (token))
           (block-time (block-time-in-seconds))
           (to-withdraw (at 'to-withdraw (read global-table 'global-vars)))
           )
         (with-capability (STAKER staker)
         (with-read staker-details staker
           { "start-date" := start-date
           , "locked-balance" := locked-balance }
           (enforce (> (- block-time start-date) (round (days 7))) "7 days didn't pass")
           (enforce (> locked-balance 0) "Reporter not locked for withdrawal")
           (with-capability (PRIVATE)
           (token::transfer TELLOR_FLEX_ACCOUNT staker 10.0))

           (update staker-details staker
             { "locked-balance": 0 })

           (update global-table 'global-vars
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
  (defun accumulated-reward-per-share ()
    (at 'accumulated-reward-per-share (read global-table 'global-vars))
  )

  (defun get-block-number-by-timestamp (query-id:string timestamp:integer)
    @doc "Returns the block number at a given timestamp"
    (at 'block-height (read reports (concatenate query-id timestamp)))
  )

  (defun get-current-value:string (query-id:string)
    @doc "Get last reported value for query id"
    (let ((timestamp (at 0 (take (- 1) (at 'timestamps (read timestamp-table query-id))))))
      (at 'value (read reports (concatenate query-id (at 'timestamp timestamp))))
    )
  )

  (defun get-data-before (query-id:string timestamp:integer)
    @doc "Retrieves the latest value for the queryId before the specified timestamp"
     (at 0
       (select reports ['timestamp, 'value]
        (and?
          (where 'query-id (= query-id))
          (and?
            (where 'timestamp (> timestamp))
            (where 'is-disputed (= false))))))
  )

  (defun get-governance-module ()
    @doc "Get governance module"
    (at 'governance (read governance-table 'governance))
  )

  (defun minimum-stake-amount:decimal ()
    @doc "Get stake amount"
    (at 'minimum-stake-amount(read global-table 'global-vars))
  )

  (defun get-new-value-countby-query-id:integer (query-id:string)
    @doc "Get the number of values submitted for a query id"
    (let ((timestamps (at 'timestamp (at 'timestamps (read timestamp-table query-id)))))
    (- (length timestamps) 1))
  )

  ; (defun get-pending-reward-by-staker:integer (staker-address:string)
  ;
  ; )

  (defun get-real-staking-rewards-balance:integer ()
    @doc "Returns the real staking rewards balance after accounting for unclaimed rewards"
    (with-read global-table 'global-vars
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
    { 'reporter: (get-reporter-by-timestamp)
    , 'disputed: (at 'is-disputed (read reports (concatenate query-id timestamp)))
    }
  )

  (defun get-reporter-last-timestamp (reporter:string)
    (at 'reporter-last-timestamp (read staker-details reporter))
  )

  (defun get-reporter-by-timestamp (query-id:string timestamp:integer)
    (at 'reporter (read reports (concatenate query-id timestamp)))
  )

  (defun get-reporting-lock:integer ()
    @doc "Get reporting lock interval time"
    (at 'reporting-lock (read global-table 'global-vars))
  )

  (defun get-reports-submitted-by-address (reporter:string)
    (at 'reports-submitted (read staker-details reporter))
  )

  (defun get-reports-submitted-by-address-and-queryId:integer (reporter:string query-id:string)
    (at 'reports-submitted-by-queryid (read reports-submitted-count (+ query-id reporter)))
  )

  (defun get-stake-amount:integer ()
    @doc "Get timestamp of last reported value of any query id"
    (at 'stake-amount (read global-table 'global-vars))
  )

  (defun get-staker-info:object (reporter:string)
    @doc "Get staker info details"
    (read staker-details reporter)
  )

  (defun get-time-of-last-new-value:integer ()
    @doc "Get timestamp of last reported value of any query id"
    (at 'time-of-last-new-value (read global-table 'global-vars))
  )

  (defun get-timestampby-query-id-and-index:integer (query-id:string index:integer)
    @doc "Gets the timestamp for the value based on their index"
    (at 'timestamp (at index (at 'timestamps (read timestamp-table query-id))))
  )

  (defun get-total-reward-debt:integer ()
    @doc "Get total reward debt in oracle"
    (at 'total-reward-debt (read global-table 'global-vars))
  )

  (defun get-total-stake-amount:integer ()
    @doc "Get total stake amount in oracle"
    (at 'total-stake-amount (read global-table 'global-vars))
  )

  (defun get-total-stakers:integer ()
    @doc "Get total number of stakers in oracle"
    (at 'total-stakers (read global-table 'global-vars))
  )

  (defun get-total-time-based-rewards-balance:integer ()
    (with-read global-table 'global-vars
      { 'total-stake-amount := total-stake-amount
      , 'staking-reward-balance := staking-reward-balance
      , 'to-withdraw := to-withdraw
      }
      (- (coin.get-balance TELLOR_FLEX_ACCOUNT) (+ (+ total-stake-amount staking-reward-balance) to-withdraw))
      )
  )

  (defun get-to-withdraw-amount:integer ()
    @doc "Get withdraw amount currently locked for withdrawal"
    (at 'to-withdraw (read global-table 'global-vars))
  )

  (defun reward-rate:integer ()
    (at 'reward-rate (read global-table 'global-vars))
  )

  (defun staking-rewards-balance:integer ()
    (at 'staking-rewards-balance (read global-table 'global-vars))
  )

  (defun staking-token-query-id:string ()
    (at 'staking-token-price-query-id (read global-table 'global-vars))
  )

  (defun token:module{fungible-v2} ()
    @doc "Token"
    (at 'token (read global-table 'global-vars))
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
   (defun accumulated-reward:integer (total-stake-amount:integer total-reward-debt:integer)
     (/
       (* (new-accumulated-reward-per-share) total-stake-amount)
       (- PRECISION total-reward-debt)
      )
   )
   (defun new-accumulated-reward-per-share:integer ()
    @doc "Helper function to calculate new-accumulated-reward-per-share"
    (with-read global-table 'global-vars
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
   (defun get-updated-accumulated-reward-per-share:integer ()
    @doc "Retrieves updated accumulated-reward-per-share"
    (require-capability (PRIVATE))
    (with-read global-table 'global-vars
      { 'total-stake-amount := total-stake-amount
      , 'total-reward-debt := total-reward-debt
      , 'staking-rewards-balance := staking-rewards-balance
      , 'accumulated-reward-per-share := accumulated-reward-per-share }
      (if (= total-stake-amount 0)
        accumulated-reward-per-share
        (let ((new-pending-rewards (- staking-rewards-balance (/ (* accumulated-reward-per-share total-stake-amount) (- PRECISION total-reward-debt)) ))
              (accumulated-reward (accumulated-reward total-stake-amount total-reward-debt)))
          (if (>= accumulated-reward staking-rewards-balance)
              (/ (+ accumulated-reward-per-share (* new-pending-rewards PRECISION)))
              total-stake-amount)))
    )
   )

   (defun update-rewards ()
     (require-capability (PRIVATE))
     (with-read global-table 'global-vars
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
               (update global-table 'global-vars { 'time-of-last-allocation: block-time })
             (let ((accumulated-reward (accumulated-reward total-stake-amount total-reward-debt) ))
             (if
               (>= accumulated-reward staking-rewards-balance)
               (update global-table 'global-vars
                 { 'accumulated-reward-per-share: (+ accumulated-reward-per-share (/ (* (calculate-reward-rate) PRECISION) total-stake-amount))
                 , 'reward-rate: 0})
             (update global-table 'global-vars { 'accumulated-reward-per-share: (new-accumulated-reward-per-share)})))))

       (update global-table 'global-vars { 'time-of-last-allocation: block-time})
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
      ( with-read global-table 'global-vars
        { 'token := token:module{fungible-v2}
        , 'staking-rewards-balance := staking-rewards-balance
        , 'accumulated-reward-per-share := accumulated-reward-per-share
        , 'total-stake-amount := total-stake-amount
        , 'total-reward-debt := total-reward-debt }

      (if (> staked-balance 0)

        (let* ((governance:module{tellor-governance} (get-governance-module))
               (vote-count (governance::get-vote-count)))
          (if (> vote-count 0)
              (let* ((vote-tally (governance::get-vote-tally-by-address staker))
                     (pending-reward (/ (* staked-balance accumulated-reward-per-share) (- PRECISION reward-debt)))
                     (temp-pending-reward (/ (* pending-reward (- vote-tally start-vote-tally)) vote-count))
                     (pay-amount (if (< temp-pending-reward pending-reward) temp-pending-reward pending-reward)))
                (token::transfer TELLOR_FLEX_ACCOUNT staker pay-amount)
                (update global-table 'global-vars
                  { 'staking-rewards-balance: (- staking-rewards-balance pay-amount)
                  , 'total-reward-debt: (- total-reward-debt reward-debt)
                  , 'total-stake-amount: (- total-stake-amount staked-balance)}
                  )) ""

        )
       ) ""
      )
     )
    )
    (update staker-details staker { 'staked-balance: new-staked-balance })
    (with-read staker-details staker
      { 'staked-balance := staked-balance
      , 'is-staked := staked
      , 'reward-debt := reward-debt}
      (if (>= staked-balance (get-stake-amount))
          [
          (if (not staked)
              (update global-table 'global-vars { 'total-stakers: (+ (get-total-stakers) 1)})
              0)
          (update staker-details staker {'is-staked: true })
          ]
          [
          (if (and staked (> (get-total-stakers) 0))
            (update global-table 'global-vars { "total-stakers": (- (get-total-stakers) 1)})
            0)
          (update staker-details staker {'is-staked: false })
          ]
      )
      (update staker-details staker { 'reward-debt: (/ (* staked-balance (accumulated-reward-per-share)) PRECISION)})
      (update global-table 'global-vars
        { 'total-stake-amount: (+ (get-total-stake-amount) staked-balance)
        , 'total-reward-debt: (+ (get-total-reward-debt) reward-debt)
        , 'reward-rate: (if (= (reward-rate) 0) (/ (calculate-reward-rate) (round (days 30))) (reward-rate))
        })
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

   (defun use-precision:integer (amount:decimal)
    (round (* amount PRECISION))
   )

   (defun calculate-reward-rate:integer ()
     (with-read global-table 'global-vars
       { 'staking-rewards-balance := staking-rewards-balance
       , 'accumulated-reward-per-share := accumulated-reward-per-share
       , 'total-stake-amount := total-stake-amount
       , 'total-reward-debt := total-reward-debt
       }
       (-
         staking-rewards-balance
         (/
           (* accumulated-reward-per-share total-stake-amount)
           (- PRECISION total-reward-debt)
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
; (if (read-msg "upgrade")
;   ["upgrade"]
;   [
;   ; (create-table stake-info)
;   ; (create-table reports-table)
;   ; (create-table timestamp-table)
;   ; (create-table global-table)
;   (constructor)
;   ])
