; tellorflex on kadena
(namespace "free")

(if (read-msg "upgrade")
  "Upgrading contract"

  [ (enforce-keyset (read-keyset "tellor-admin-keyset"))
    (define-keyset "free.tellor-admin-keyset" (read-keyset "tellor-admin-keyset"))
  ]
)
(module tellorflex OWNER

  @doc
    "'tellorflex' represents the tellor oracle contract. This contract         \
    \allows reporters to stake and report values to make available on chain    \
    \  > (free.tellorflex.submit-value ...)                                    \
    \allows users to read values                                               \
    \  > (free.tellorflex.retrieve-value ...)"

  @model
    [ (defproperty owner-authorized (authorized-by "free.tellor-admin-keyset"))
    ]
; *****************************************************************************
; *                                                                           *
; *                          Constants                                        *
; *                                                                           *
; *****************************************************************************
  (defconst TELLOR_FLEX_ACCOUNT "tellorflex"
  ;  need to change this to guard for contract to hold funds, i think
      @doc "Account name of the oracle account that holds and disburses funds.")

  (defconst PRECISION (^ 10 18)
    @doc "PRECISION")

; *****************************************************************************
; *                                                                           *
; *                          Schema                                           *
; *                                                                           *
; *****************************************************************************
  (defschema reporter-schema

    start-date:integer
    reports-count:integer
    staked-balance:integer
    locked-balance:integer
    reporter-last-timestamp:integer
    is-staked:bool)

  (defschema reports-schema

      index:integer
      query-id:string
      timestamp:integer
      block-height:integer
      value:string
      reporter:string
      is-disputed:bool)

  (defschema timestamps-schema

    timestamps:[integer])

  (defschema constructor-schema

    token:module{fungible-v2}
    governance:string
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
    to-withdraw:integer)

; *****************************************************************************
; *                                                                           *
; *                          Tables                                           *
; *                                                                           *
; *****************************************************************************
  (deftable reporters-table:{reporter-schema})

  (deftable reports-table:{reports-schema})

  (deftable timestamp-table:{timestamps-schema})

  (deftable global-table:{constructor-schema})

; *****************************************************************************
; *                                                                           *
; *                          Capabilities                                     *
; *                                                                           *
; *****************************************************************************
  (defcap OWNER ()
    @doc "Enforce only owner."
    ; (enforce false "no upgrades")
    (enforce-guard (keyset-ref-guard "free.tellor-admin-keyset"))
  )

  (defcap PRIVATE ()
    true
  )

  (defun module-assets ()
    (create-module-guard "module-owned-assets")
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

    (with-capability (OWNER)
      (let ((potential-amount (/ stake-amount-dollar-target staking-token-price)))
      (insert global-table 'global-vars
        { 'token: token
        , 'governance: ""
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
        , 'to-withdraw: 0}))(block-time-in-seconds))
  )

  (defun init (governance-address:string)
    (with-capability (OWNER)
      (enforce (!= governance-address "") "Parameter can't be empty string")
      (let ((governance-addy (at 'governance (read global-table 'global-vars))))
      (enforce (= governance-addy "") "Governance address already set!")
      (update global-table 'global-vars { 'governance: governance-address })))
  )

  (defun add-staking-rewards (amount:decimal)
    @doc "Funds the flex contract with staking rewards (autopay and miniting)"
    (with-capability (PRIVATE)
      ( with-read global-table 'global-vars
        { 'token := token:module{fungible-v2}
        , 'staking-rewards-balance := staking-rewards-balance
        , 'accumulated-reward-per-share := accumulated-reward-per-share
        , 'total-stake-amount := total-stake-amount
        , 'total-reward-debt := total-reward-debt }
        (token::transfer (msg-sender) TELLOR_FLEX_ACCOUNT amount)
        (update-rewards)
        ;TODO: calculate rewardRate!!!
        (let* ((sub-reward-rate (- (* 1 PRECISION) total-reward-debt))
               (per-share (* accumulated-reward-per-share total-stake-amount))
               (reward-rate (- staking-rewards-balance (/ per-share sub-reward-rate))))
        (update global-table 'global-vars { 'staking-rewards-balance: (use-precision amount), 'reward-rate: reward-rate})
      )
     )
    )
  )

  (defun deposit-stake (amount:integer)
    @doc "Enable reporter to submit a stake in order to report"
    (let ((governance (get-governance-address)))
     (enforce (!= governance "") "Governance address hasn't been set!"))
    (enforce (> amount 0) "Amount must be greater than 0")

    (with-default-read reporters-table (msg-sender)
      { 'start-date: 0
      , 'reports-count: 0
      , 'staked-balance: 0
      , 'locked-balance: 0
      , 'reporter-last-timestamp: 0
      , 'is-staked: false
      }
      { 'start-date := start-date
      , 'reports-count := reports-count
      , 'staked-balance := staked-balance
      , 'locked-balance := locked-balance
      , 'reporter-last-timestamp := reporter-last-timestamp
      , 'is-staked := is-staked
      }
      (let (
        (account (msg-sender))
        (block-time (block-time-in-seconds))
        (token:module{fungible-v2} (token))
        (transfer-amount
          (if (> locked-balance 0)
              (if (>= locked-balance amount) 0 (- amount locked-balance))
              amount
              )))
      (token::transfer account TELLOR_FLEX_ACCOUNT (/ (/ transfer-amount (^ 10  18)) 1.0))
;  change token
      (write reporters-table account
        { 'start-date: block-time
        , 'reports-count: reports-count
        , 'staked-balance: (+ amount staked-balance); fix this
        , 'locked-balance: (if (>= locked-balance amount) (- locked-balance amount) 0)
        , 'reporter-last-timestamp: reporter-last-timestamp
        , 'is-staked: true
        })
        )
      (update global-table 'global-vars
        (let ((withdraw-amount (get-to-withdraw-amount)))
        { 'to-withdraw: (if (>= locked-balance amount)
                            (- withdraw-amount amount)
                            (- withdraw-amount locked-balance))})
                            )
    )
  )

  (defun remove-value (query-id:string timestamp:integer)
    @doc "Remove disputed value only by governance"
    (enforce (= (msg-sender) (at 'governance (read global-table 'global-vars))) "Caller must be governance address")
    (enforce (is-in-dispute query-id timestamp) "Value already disputed")
    (update reports-table (concatenate query-id timestamp)
      { 'value: "" , 'is-disputed: true })
  )

  (defun request-staking-withdraw (amount:integer)
    @doc "Allows a reporter to request to withdraw their stake"
    (let ( (sender (msg-sender) )
           (block-time (block-time-in-seconds))
           (to-withdraw (at 'to-withdraw (read global-table 'global-vars)))
           )
         (with-read reporters-table sender
           { "staked-balance" := staked-balance
           , "locked-balance" := locked-balance
           , "start-date" := start-date
           }
           (enforce (>= staked-balance amount) "Insufficient staked balance")
           (update reporters-table sender
             { "locked-balance": (+ locked-balance amount)
             , "start-date": block-time})
           )
           (update global-table 'global-vars
            { 'to-withdraw: (+ to-withdraw amount)})

           (format "locked time {}" [block-time])
    )
  )

  (defun slash-reporter (reporter:string recipient:string)
    ; (let ((governance (at 'governance (read global-table 'global-vars))))
    ;   (enforce (= (msg-sender) governance) "Only governance can slash reporter"))
    (with-read reporters-table reporter
      { 'staked-balance := staked-balance ,'locked-balance := locked-balance }
      ; (enforce (> (+ staked-balance locked-balance) 0) "Zero staker balance")
      (let* ((stake-amount (at 'stake-amount (read global-table 'global-vars)))
             (to-withdraw (at 'to-withdraw (read global-table 'global-vars)))
             (slash-amount
               (if (or (>= locked-balance stake-amount) (>= (+ locked-balance staked-balance) stake-amount))
               stake-amount
               (+ staked-balance locked-balance))))
       ; (if (>= locked-balance stake-amount)
       ;     (update-stake-and-pay-rewards reporter (- staked-balance (- stake-amount staked-balance)))
       ;     (update-stake-and-pay-rewards reporter 0)
      (if (>= locked-balance stake-amount)
          (update global-table 'global-vars
            {'to-withdraw: (- to-withdraw stake-amount)})
          (update global-table 'global-vars
            {'to-withdraw: (- to-withdraw locked-balance)}))
      (if (>= locked-balance stake-amount)
          (update reporters-table reporter
            {'locked-balance: (- locked-balance stake-amount)})
          (update reporters-table reporter
            {'locked-balance: 0}))
        ; TODO:
      ; (coin.transfer TELLOR_FLEX_ACCOUNT recipient (/ (/ slash-amount 1.0) (^ 10  18)))
      )
    )
  )

  (defun submit-value
    ( query-id:string
      nonce:integer
      query-data:string
      value:string
    )
    @doc "Enables staked reporters to submit values into the oracle"
    (enforce (= query-id (hash query-data)) "Query id not hash of query data")
    (enforce (>= nonce 0) "Nonce too low")
    (enforce (!= value "") "Value empty!")
    (let ((block-time (block-time-in-seconds))
          (msg-sender (msg-sender))
          (stake-amount (get-stake-amount))
          (block-height (block-height)))

          (with-default-read timestamp-table query-id
            { 'timestamps: [0] }
            { 'timestamps := timestamps }
            (enforce (or (= nonce (+ (length timestamps) 1)) (= nonce 0)) "Nonce must match timestamps list length")

           (with-read reporters-table msg-sender
             { 'staked-balance := staked-balance
             , 'reporter-last-timestamp := reporter-last-timestamp
             , 'reports-count := reports-count }

             (enforce (>= staked-balance stake-amount) "Don't have enough stake")

             (enforce (>
               (- block-time reporter-last-timestamp)(/ 43200 (/ staked-balance stake-amount)))
               "Still in reporter time lock")

             (insert reports-table (concatenate query-id block-time)
               { 'index: (- (length timestamps) 1)
               , 'query-id: query-id
               , 'timestamp: (block-time-in-seconds)
               , 'block-height: 0
               , 'value: value
               , 'reporter: msg-sender
               , 'is-disputed: false
               })

             (write timestamp-table query-id
               { 'timestamps: (+ timestamps [block-time])})


            (update reporters-table msg-sender
              { 'reports-count: (+ reports-count 1)
              , 'reporter-last-timestamp: block-time})

        )
        (update global-table 'global-vars
          { 'time-of-last-new-value: block-time})
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

  (defun withdraw-stake ()
    @doc "Withdraws a reporter's stake after the lock period expires"
    (let ( (sender (msg-sender) )
           (block-time (block-time-in-seconds))
           (to-withdraw (at 'to-withdraw (read global-table 'global-vars)))
           )
         (with-read reporters-table sender
           { "start-date" := start-date
           , "locked-balance" := locked-balance }
           (enforce (> (- block-time start-date) (round (days 7))) "7 days didn't pass")
           (enforce (> locked-balance 0) "Reporter not locked for withdrawal")

           (coin.transfer TELLOR_FLEX_ACCOUNT sender (/ (/ locked-balance 1.0) (^ 10  18)))

           (update reporters-table sender
             { "locked-balance": 0 })

           (update global-table 'global-vars
            { 'to-withdraw: (- to-withdraw locked-balance )})
      )
     )
    )
; *****************************************************************************
; *                                                                           *
; *                          Getter functions                                 *
; *                                                                           *
; *****************************************************************************
  (defun get-block-number-by-timestamp (query-id:string timestamp:integer)
    @doc "Returns the block number at a given timestamp"
    (at 'block-height (read reports-table (concatenate query-id timestamp)))
  )

  (defun get-current-value:string (query-id:string)
    @doc "Get last reported value for query id"
    (let ((timestamp (at 0 (take (- 1) (at 'timestamps (read timestamp-table query-id))))))
      (at 'value (read reports-table (concatenate query-id timestamp)))
    )
  )

  (defun get-data-before (query-id:string timestamp:integer)
    @doc "Retrieves the latest value for the queryId before the specified timestamp"
     (at 0
       (select reports-table ['timestamp, 'value]
        (and?
          (where 'query-id (= query-id))
          (and?
            (where 'timestamp (> timestamp))
            (where 'is-disputed (= false))))))
  )

  (defun get-governance-address:string ()
    @doc "Get governance address"
    (at 'governance (read global-table 'global-vars))
  )

  (defun get-minimum-stake-amount:decimal ()
    @doc "Get stake amount"
    (at 'minimum-stake-amount(read global-table 'global-vars))
  )

  (defun get-new-value-countby-query-id:integer (query-id:string)
    @doc "Get the number of values submitted for a query id"
    (let ((timestamps (at 'timestamps (read timestamp-table query-id))))
    (- (length timestamps) 2))
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
    , 'disputed: (at 'is-disputed (read reports-table (concatenate query-id timestamp)))
    }
  )

  (defun get-reporter-last-timestamp (reporter:string)
    (at 'reporter-last-timestamp (read reporters-table reporter))
  )

  (defun get-reporter-by-timestamp (query-id:string timestamp:integer)
    (at 'reporter (read reports-table (concatenate query-id timestamp)))
  )

  (defun get-reporting-lock:integer ()
    @doc "Get reporting lock interval time"
    (at 'reporting-lock (read global-table 'global-vars))
  )

  (defun get-reports-submitted-by-address (reporter:string)
    (at 'reports-count (read reporters-table reporter))
  )

  (defun get-reports-submitted-by-address-and-queryId:integer (reporter:string query-id:string)
    (+ (at 'index (at 0
      (select reports-table ['index]
        (and?
          (where 'query-id (= query-id))
          (where 'reporter (= reporter)))))) 1)
  )

  (defun get-stake-amount:integer ()
    @doc "Get timestamp of last reported value of any query id"
    (at 'stake-amount (read global-table 'global-vars))
  )

  (defun get-staker-info:object (reporter:string)
    @doc "Get staker info details"
    (read reporters-table reporter)
  )

  (defun get-time-of-last-new-value:integer ()
    @doc "Get timestamp of last reported value of any query id"
    (at 'time-of-last-new-value (read global-table 'global-vars))
  )

  (defun get-timestampby-query-id-and-index:integer (query-id:string index:integer)
    @doc "Gets the timestamp for the value based on their index"
    (at index (at 'timestamps (read timestamp-table query-id)))
  )

  (defun get-total-stakers:integer ()
    @doc "Get total number of stakers in oracle"
    (length (keys reporters-table))
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

  (defun staking-token-query-id:string ()
    (at 'staking-token-price-query-id (read global-table 'global-vars))
  )

  (defun token:module{fungible-v2} ()
    @doc "Token"
    (at 'token (read global-table 'global-vars))
  )

  (defun is-in-dispute:bool (query-id:string timestamp:integer)
    @doc "Check if given timestamp report for query id is in dispute"
      (at 'is-disputed (read reports-table (concatenate query-id timestamp)))
  )

  (defun retrieve-data:integer (query-id:string timestamp:integer)
    @doc "Get value for a query id at given timestamp"
      (at 'value (read reports-table (concatenate query-id timestamp)))
   )
   ; *****************************************************************************
   ; *                                                                           *
   ; *                          Private functions                                 *
   ; *                                                                           *
   ; *****************************************************************************
   (defun get-updated-accumulated-reward-per-share:integer ()
    (with-read global-table 'global-vars
      { 'time-of-last-allocation := time-of-last-allocation
      , 'reward-rate := reward-rate
      , 'total-stake-amount := total-stake-amount
      , 'total-reward-debt := total-reward-debt
      , 'staking-rewards-balance := staking-rewards-balance
      , 'accumulated-reward-per-share := accumulated-reward-per-share }
      (if (= total-stake-amount 0)
        accumulated-reward-per-share
        ; TODO: consolidate this code
        (let* ((time-diff (- block-time time-of-last-allocation))
               (rate (* time-diff reward-rate))
               (debt (- PRECISION total-reward-debt))
               (new-pending-rewards (- staking-rewards-balance (/ (* accumulated-reward-per-share total-stake-amount) debt) ))
               (new-accumulated-reward-per-share (+ accumulated-reward-per-share (/ (* (* time-diff reward-rate) PRECISION) total-stake-amount)))
               (accumulated-reward (/ (* new-accumulated-reward-per-share total-stake-amount) debt)))
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
       (let* ( (block-time (block-time-in-seconds))
               (time-diff (- block-time time-of-last-allocation))
               (rate (* time-diff reward-rate))
               (debt (- PRECISION total-reward-debt))
               (new-pending-rewards (- staking-rewards-balance (/ (* accumulated-reward-per-share total-stake-amount) debt) )))
             (if
               (= time-of-last-allocation block-time)
               "was just allocated"
             (if
               (or? (= total-stake-amount) (= reward-rate) 0)
               (update global-table 'global-vars { 'time-of-last-allocation: block-time})
             (if
               (let* ((new-accumulated-reward-per-share (+ accumulated-reward-per-share (/ (* (* time-diff reward-rate) PRECISION) total-stake-amount)))
               (accumulated-reward (/ (* new-accumulated-reward-per-share total-stake-amount) debt)))
               (>= accumulated-reward staking-rewards-balance)
               (update global-table 'global-vars
                 { 'accumulated-reward-per-share: (+ accumulated-reward-per-share (/ (* new-pending-rewards PRECISION) total-stake-amount))
                 , 'reward-rate: 0})
             ; else
             (update global-table 'global-vars { 'accumulated-reward-per-share: new-accumulated-reward-per-share})))))

       (update global-table 'global-vars { 'time-of-last-allocation: block-time})
       )
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

   (defun msg-sender:string ()
    (at 'sender (chain-data))
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

)

; *****************************************************************************
; *                                                                           *
; *                         Initialize                                        *
; *                                                                           *
; *****************************************************************************
(if (read-msg "upgrade")
  ["upgrade"]
  [
  (create-table reporters-table)
  (create-table reports-table)
  (create-table timestamp-table)
  (create-table global-table)
  ])
