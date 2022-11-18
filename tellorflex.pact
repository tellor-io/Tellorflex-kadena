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

  (defconst TWELVE_HOURS 43200
      @doc "Twelve hour reporter lock period")

  (defconst MINIMUM_STAKE 10.0
    @doc "Minimum stake amount to be a reporter")

  (defconst TIME_ZERO (time "1970-01-01T00:00:00Z")
    @doc "Beginning of time")

; *****************************************************************************
; *                                                                           *
; *                          Schema                                           *
; *                                                                           *
; *****************************************************************************
  (defschema reporter-schema

    start-date:time
    reports-count:integer
    staked-balance:decimal
    locked-balance:decimal
    reporter-last-timestamp:time
    is-staked:bool)

  (defschema reports-schema

      timestamps:[time]
      block-height:[integer]
      reporter:[string]
      values:[string]
      is-disputed:[bool])

  (defschema constants-schema

    time-of-last-value:time
    to-withdraw:decimal)

; *****************************************************************************
; *                                                                           *
; *                          Tables                                           *
; *                                                                           *
; *****************************************************************************

  (deftable reporters-table:{reporter-schema})
  (deftable reports-table:{reports-schema})
  (deftable constants-table:{constants-schema})
; *****************************************************************************
; *                                                                           *
; *                          Capabilities                                     *
; *                                                                           *
; *****************************************************************************
  (defcap OWNER ()
    @doc "Enforce only owner."
    (enforce-guard (keyset-ref-guard "tellor-admin-keyset")))

; *****************************************************************************
; *                                                                           *
; *                          Main functions                                   *
; *                                                                           *
; *****************************************************************************
  (defun init ()
  ; rethink This
    @doc "Init updateable global constants before using contract"
    ; (with-capability (OWNER)
      (with-default-read constants-table "constants"
      { "time-of-last-value": TIME_ZERO
      , "to-withdraw": 0.0
      }
      { "time-of-last-value" := time-of-last-value
      , "to-withdraw" := to-withdraw
      }
      (write constants-table "constants"
      ; set enforce >
      { "time-of-last-value" : time-of-last-value
      , "to-withdraw": to-withdraw
      }))
    ; )
  )
  (defun deposit-stake (amount:decimal)
    @doc "Enable reporter to submit a stake in order to report"

    (enforce (> amount 0.0) "Amount must be greater than 0.0")

    (with-default-read reporters-table (at 'sender (chain-data))
      { "start-date": TIME_ZERO
      , "reports-count": 0
      , "staked-balance": 0.0
      , "locked-balance": 0.0
      , "reporter-last-timestamp": TIME_ZERO
      , "is-staked": false
      }
      { "start-date" := start-date
      , "reports-count" := reports-count
      , "staked-balance" := staked-balance
      , "locked-balance" := locked-balance
      , "reporter-last-timestamp" := reporter-last-timestamp
      , "is-staked" := is-staked
      }
      (let (
        (account (at 'sender (chain-data)))
        (block-time (at 'block-time (chain-data)))
        (transfer-amount
          (cond
            ((= locked-balance amount) amount)
            ((>= locked-balance amount) (- locked-balance amount))
            amount)
          ))
      (coin.transfer account TELLOR_FLEX_ACCOUNT amount)
;  change token
      (write reporters-table account

        { "start-date": (if (= start-date TIME_ZERO) block-time start-date)
        , "reports-count": reports-count
        , "staked-balance": (+ amount staked-balance)
        , "locked-balance": (if (>= locked-balance transfer-amount) (- locked-balance transfer-amount) 0.0)
        , "reporter-last-timestamp": reporter-last-timestamp
        , "is-staked": is-staked
        })

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
    (with-default-read reports-table query-id
      { "timestamps": [TIME_ZERO]
      , "block-height": [0]
      , "reporter": [""]
      , "values": [""]
      , "is-disputed": [false]
      }
      { "timestamps" := timestamps
      , "block-height" := block-num
      , "reporter" := reporter
      , "values" := values
      , "is-disputed" := is-disputed
      }
      (length reporter)

      (if
        (= reporter [""]) (enforce (= nonce 0) "Nonce should be zero for first time report")
        (enforce (= nonce (+ (length reporter) 1)) "Nonce must match reports list length")
      )
      (let (
            (block-time (at 'block-time (chain-data)))
            (block-height (at 'block-height (chain-data)))
            (sender (at 'sender (chain-data)))
           )
           (with-read reporters-table sender
             { "staked-balance" := staked-balance
             , "reporter-last-timestamp" := reporter-last-timestamp
             , 'reports-count := reports-count
           }
             (enforce (>= staked-balance MINIMUM_STAKE) "Don't have enough stake")

             (enforce (>
               (diff-time block-time reporter-last-timestamp)(/ TWELVE_HOURS (/ staked-balance MINIMUM_STAKE)))
               "Still in reporter time lock")

            (write reports-table query-id
              { "timestamps": (+ timestamps [block-time])
              , "block-height": (+ block-num [block-height])
              , "reporter": (+ reporter [sender])
              , "values": (+ values [value])
              , "is-disputed": (+ is-disputed [false])
              }
            )
          (update reporters-table sender
            { "reports-count": (+ reports-count 1)
            , "reporter-last-timestamp": block-time})

        )
        (update constants-table "constants"
          { "time-of-last-value": block-time})
      )
    )
  )

  (defun request-staking-withdraw (amount:decimal)
    @doc "Allows a reporter to request to withdraw their stake"
    (let ( (sender (at 'sender (chain-data) ) )
           (block-time (at 'block-time (chain-data)))
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
          (with-read constants-table "constants"
          { "to-withdraw" := to-withdraw }
          (update constants-table "constants"
          { "to-withdraw": (+ to-withdraw amount)}))
          (format "locked time {}" [block-time])
      )
  )

  (defun withdraw-stake ()
    @doc "Withdraws a reporter's stake after the lock period expires"
    (let ( (sender (at 'sender (chain-data) ) )
           (block-time (at 'block-time (chain-data)))
           )
         (with-read reporters-table sender
           { "start-date" := start-date
           , "locked-balance" := locked-balance }
           (enforce (> (diff-time block-time start-date) (days 7)) "7 days didn't pass")
           (enforce (> locked-balance 0.0) "Reporter not locked for withdrawal")

           (coin.transfer TELLOR_FLEX_ACCOUNT sender locked-balance)

           (update reporters-table sender
             { "locked-balance": 0.0 })
           (with-read constants-table "constants"
             { "to-withdraw" := to-withdraw }
           (update constants-table "constants"
             { "to-withdraw": (- to-withdraw locked-balance) })
           )
         )
    )
)
  ;
  ;   (defun remove-value (queryId:string, timestamp:time))
; *****************************************************************************
; *                                                                           *
; *                          Getter functions                                 *
; *                                                                           *
; *****************************************************************************
  ; (defun check-key (table-name key)
  ;   @doc "Enforce key in table"
  ;   (let (
  ;         (keys (keys table-name))
  ;        )
  ;        (enforce (contains key keys) (format "{} not found!" [key]))
  ;   )
  ; )

  (defun get-current-value:string (query-id:string)
    @doc "Get last reported value for query id"
    (with-read reports-table query-id
      {"values" := values}
      (at (- (length values) 1) values)
    )
  )

  (defun get-reporting-lock:integer ()
    @doc "Get reporting lock interval time"
    TWELVE_HOURS
  )

  (defun get-stake-amount:decimal ()
    @doc "Get stake amount"
    MINIMUM_STAKE
  )

  (defun get-staker-info:object (reporter:string)
    @doc "Get staker info details"
   ; (check-key reporters-table reporter)

    (with-read reporters-table reporter
      { "start-date" := start-date
      , "reports-count" := reports-count
      , "staked-balance" := staked-balance
      , "locked-balance" := locked-balance
      , "reporter-last-timestamp" := reporter-last-timestamp
      , "is-staked" := is-staked
      }
      { "start-date": start-date
      , "reports-count": reports-count
      , "staked-balance": staked-balance
      , "locked-balance": locked-balance
      , "reporter-last-timestamp": reporter-last-timestamp
      , "is-staked": is-staked
      }
    )
  )

  (defun get-time-of-last-value:time ()
    @doc "Get timestamp of last reported value of any query id"
    (with-read constants-table "constants"
      {"time-of-last-value" := time-of-last-value}
      time-of-last-value)
  )

  (defun get-total-stakers:integer ()
    @doc "Get total number of stakers in oracle"
    (length (keys reporters-table))
  )

  (defun get-to-withdraw-amount:decimal ()
    @doc "Get withdraw amount currently locked for withdrawal"
    (with-read constants-table "constants"
      { "to-withdraw" := to-withdraw }
      to-withdraw)
  )

  (defun retrieve-data:string (query-id:string timestamp:time)
    @doc "Get value for a query id at given timestamp"
    ; (check-key reports-table query-id)
    (with-read reports-table query-id
      { "timestamps" := timestamps
      , "values" := values
      }
      (enforce (contains timestamp timestamps) "There is no report at given timestamp")
      (at (at 0 (search timestamps timestamp)) values)

    )
  )

; *****************************************************************************
; *                                                                           *
; *                          Helper functions                                 *
; *                                                                           *
; *****************************************************************************
;  shout out to CryptoPascal31 for these util function
;  https://github.com/CryptoPascal31/pact-util-lib/blob/main/pact/contracts/util-lists.pact
;  gets errors on verfication for typing on lam function. need alternative
  (defun search:[integer] (in:list item)
    @doc "Search an item into the list and returns a list of index"
    ; Save gas if item is not in list => use the native contains to return empty
    (if (contains item in)
        (let* ((match-func (lambda (out-list x)
                                  (if (= (at 'v x) item)
                                      (append-last out-list (at 'i x))
                                      out-list))))
          (fold match-func [] (enumerate-list in)))
        [])
  )

  (defun enumerate-list:[object] (in:list)
    "Returns a list of objects {'i:idx, 'v:value} where i is the index, and v the value"

    (let ((indexes (enumerate 0 (length in))))
      (zip (lambda (idx x) {'i:idx, 'v:x}) indexes in))
  )

  (defun append-last:object (in item)
    "Append an item at the end of the list"
    (+ in [item]))


)

  ; ; ----------
  ; ; GOVERNANCE RELATED
  ; ; ----------
  ; (defun slash-reporter (reporter:keyset, reward-receipient:keyset))

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
  (create-table constants-table)
  ])
