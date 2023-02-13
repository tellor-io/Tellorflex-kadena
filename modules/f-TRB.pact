;  copied from coin-v5 with changes for testing
(namespace (read-msg "ns"))
  (if (read-msg "upgrade")
    "Upgrading contract"

    [
      (enforce-keyset (read-keyset "admin-keyset"))
      (define-keyset (+ (read-msg "ns") ".admin-keyset") (read-keyset "admin-keyset"))
      (define-keyset (+ (read-msg "ns") ".reporter-1") (read-keyset "reporter-1"))
    ]
  )
(module f-TRB GOVERNANCE

    (implements fungible-v2)
  
    ; --------------------------------------------------------------------------
    ; Schemas and Tables
  
    (defschema coin-schema
      @doc "The coin contract token schema"
      @model [ (invariant (>= balance 0.0)) ]
  
      balance:decimal
      guard:guard)
  
    (deftable coin-table:{coin-schema})
  
    ; --------------------------------------------------------------------------
    ; Capabilities
  
    (defcap GOVERNANCE ()
      (enforce-guard 
        (keyset-ref-guard (+ (read-msg "ns") ".admin-keyset"))))
  
    (defcap DEBIT (sender:string)
      "Capability for managing debiting operations"
      (enforce-guard (at 'guard (read coin-table sender)))
      (enforce (!= sender "") "valid sender"))
  
    (defcap CREDIT (receiver:string)
      "Capability for managing crediting operations"
      (enforce (!= receiver "") "valid receiver"))
  
    (defcap ROTATE (account:string)
      @doc "Autonomously managed capability for guard rotation"
      @managed
      true)
  
    (defcap TRANSFER:bool
      ( sender:string
        receiver:string
        amount:decimal
      )
      @managed amount TRANSFER-mgr
      (enforce (!= sender receiver) "same sender and receiver")
      (enforce-unit amount)
      (enforce (> amount 0.0) "Positive amount")
      (compose-capability (DEBIT sender))
      (compose-capability (CREDIT receiver))
    )
  
    (defun TRANSFER-mgr:decimal
      ( managed:decimal
        requested:decimal
      )
  
      (let ((newbal (- managed requested)))
        (enforce (>= newbal 0.0)
          (format "TRANSFER exceeded for balance {}" [managed]))
        newbal)
    )

    (defcap TRANSFER_XCHAIN:bool
      ( sender:string
        receiver:string
        amount:decimal
        target-chain:string
      )
  
      @managed amount TRANSFER_XCHAIN-mgr
      (enforce-unit amount)
      (enforce (> amount 0.0) "Cross-chain transfers require a positive amount")
      (compose-capability (DEBIT sender))
    )
    
    (defun TRANSFER_XCHAIN-mgr:decimal
      ( managed:decimal
        requested:decimal
      )
  
      (enforce (>= managed requested)
        (format "TRANSFER_XCHAIN exceeded for balance {}" [managed]))
      0.0
    )

    (defcap TRANSFER_XCHAIN_RECD:bool
      ( sender:string
        receiver:string
        amount:decimal
        source-chain:string
      )
      @event true
    )
    ; --------------------------------------------------------------------------
    ; Constants
  
    (defconst COIN_CHARSET CHARSET_LATIN1
      "The default coin contract character set")
  
    (defconst MINIMUM_PRECISION 18
      "Minimum allowed precision for coin transactions")
  
    (defconst MINIMUM_ACCOUNT_LENGTH 3
      "Minimum account length admissible for coin accounts")
  
    (defconst MAXIMUM_ACCOUNT_LENGTH 256
      "Maximum account name length admissible for coin accounts")
    
    (defconst VALID_CHAIN_IDS (map (int-to-str 10) (enumerate 0 19))
      "List of all valid Chainweb chain ids")
    ; --------------------------------------------------------------------------
    ; Utilities
  
    (defun enforce-unit:bool (amount:decimal)
      @doc "Enforce minimum precision allowed for coin transactions"
  
      (enforce
        (= (floor amount MINIMUM_PRECISION)
           amount)
        (format "Amount violates minimum precision: {}" [amount]))
      )
  
    (defun validate-account (account:string)
      @doc "Enforce that an account name conforms to the coin contract \
           \minimum and maximum length requirements, as well as the    \
           \latin-1 character set."
  
      (enforce
        (is-charset COIN_CHARSET account)
        (format
          "Account does not conform to the coin contract charset: {}"
          [account]))
  
      (let ((account-length (length account)))
  
        (enforce
          (>= account-length MINIMUM_ACCOUNT_LENGTH)
          (format
            "Account name does not conform to the min length requirement: {}"
            [account]))
  
        (enforce
          (<= account-length MAXIMUM_ACCOUNT_LENGTH)
          (format
            "Account name does not conform to the max length requirement: {}"
            [account]))
        )
    )
  
    ; --------------------------------------------------------------------------
    ; Coin Contract
  
    (defun create-account:string (account:string guard:guard)
      @model [ (property (valid-account account)) ]
  
      (validate-account account)
      (enforce-reserved account guard)
  
      (insert coin-table account
        { "balance" : 0.0
        , "guard"   : guard
        })
      )
  
    (defun get-balance:decimal (account:string)
      (with-read coin-table account
        { "balance" := balance }
        balance
        )
      )
  
    (defun details:object{fungible-v2.account-details}
      ( account:string )
      (with-read coin-table account
        { "balance" := bal
        , "guard" := g }
        { "account" : account
        , "balance" : bal
        , "guard": g })
      )
  
    (defun rotate:string (account:string new-guard:guard)
      (with-capability (ROTATE account)
        (with-read coin-table account
          { "guard" := old-guard }
  
          (enforce-guard old-guard)
  
          (update coin-table account
            { "guard" : new-guard }
            )))
      )
  
  
    (defun precision:integer
      ()
      MINIMUM_PRECISION)
  
    (defun transfer:string (sender:string receiver:string amount:decimal)
      @model [ (property conserves-mass)
               (property (> amount 0.0))
               (property (valid-account sender))
               (property (valid-account receiver))
               (property (!= sender receiver)) ]
  
      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")
  
      (validate-account sender)
      (validate-account receiver)
  
      (enforce (> amount 0.0)
        "transfer amount must be positive")
  
      (enforce-unit amount)
  
      (with-capability (TRANSFER sender receiver amount)
        (debit sender amount)
        (with-read coin-table receiver
          { "guard" := g }
  
          (credit receiver g amount))
        )
      )
  
    (defun transfer-create:string
      ( sender:string
        receiver:string
        receiver-guard:guard
        amount:decimal )
  
      @model [ (property conserves-mass) ]
  
      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")
  
      (validate-account sender)
      (validate-account receiver)
  
      (enforce (> amount 0.0)
        "transfer amount must be positive")
  
      (enforce-unit amount)
  
      (with-capability (TRANSFER sender receiver amount)
        (debit sender amount)
        (credit receiver receiver-guard amount))
      )
  
    (defun mint:string (account:string account-guard:guard amount:decimal)  
      @model [ (property (valid-account account))
               (property (> amount 0.0))
             ]
  
      (validate-account account)
      (enforce-unit amount)
  
      (with-capability (GOVERNANCE)
        (emit-event (TRANSFER "" account amount)) ;v3
        (with-capability (CREDIT account)
          (credit account account-guard amount))
        )
      )
  
    (defun debit:string (account:string amount:decimal)
      @doc "Debit AMOUNT from ACCOUNT balance"
  
      @model [ (property (> amount 0.0))
               (property (valid-account account))
             ]
  
      (validate-account account)
  
      (enforce (> amount 0.0)
        "debit amount must be positive")
  
      (enforce-unit amount)
  
      (require-capability (DEBIT account))
      (with-read coin-table account
        { "balance" := balance }
  
        (enforce (<= amount balance) "Insufficient funds")
  
        (update coin-table account
          { "balance" : (- balance amount) }
          ))
      )
  
  
    (defun credit:string (account:string guard:guard amount:decimal)
      @doc "Credit AMOUNT to ACCOUNT balance"
  
      @model [ (property (> amount 0.0))
               (property (valid-account account))
             ]
  
      (validate-account account)
  
      (enforce (> amount 0.0) "credit amount must be positive")
      (enforce-unit amount)
  
      (require-capability (CREDIT account))
      (with-default-read coin-table account
        { "balance" : -1.0, "guard" : guard }
        { "balance" := balance, "guard" := retg }
        ; we don't want to overwrite an existing guard with the user-supplied one
        (enforce (= retg guard)
          "account guards do not match")
  
        (let ((is-new
               (if (= balance -1.0)
                   (enforce-reserved account guard)
                 false)))
  
          (write coin-table account
            { "balance" : (if is-new amount (+ balance amount))
            , "guard"   : retg
            }))
        ))
  
    (defun check-reserved:string (account:string)
      " Checks ACCOUNT for reserved name and returns type if \
      \ found or empty string. Reserved names start with a \
      \ single char and colon, e.g. 'c:foo', which would return 'c' as type."
      (let ((pfx (take 2 account)))
        (if (= ":" (take -1 pfx)) (take 1 pfx) "")))
  
    (defun enforce-reserved:bool (account:string guard:guard)
      @doc "Enforce reserved account name protocols."
      (if (validate-principal guard account)
        true
        (let ((r (check-reserved account)))
          (if (= r "")
            true
            (if (= r "k")
              (enforce false "Single-key account protocol violation")
              (enforce false
                (format "Reserved protocol guard violation: {}" [r]))
              )))))


    (defschema crosschain-schema
      @doc "Schema for yielded value in cross-chain transfers"
      receiver:string
      receiver-guard:guard
      amount:decimal
      source-chain:string)

    (defpact transfer-crosschain:string
      ( sender:string
        receiver:string
        receiver-guard:guard
        target-chain:string
        amount:decimal )
  
      @model [ (property (> amount 0.0))
                (property (valid-account sender))
                (property (valid-account receiver))
              ]
  
      (step
        (with-capability
          (TRANSFER_XCHAIN sender receiver amount target-chain)
  
          (validate-account sender)
          (validate-account receiver)
  
          (enforce (!= "" target-chain) "empty target-chain")
          (enforce (!= (at 'chain-id (chain-data)) target-chain)
            "cannot run cross-chain transfers to the same chain")
  
          (enforce (> amount 0.0)
            "transfer quantity must be positive")
  
          (enforce-unit amount)
  
          (enforce (contains target-chain VALID_CHAIN_IDS)
            "target chain is not a valid chainweb chain id")
  
          ;; step 1 - debit delete-account on current chain
          (debit sender amount)
          (emit-event (TRANSFER sender "" amount))
  
          (let
            ((crosschain-details:object{crosschain-schema}
              { "receiver" : receiver
              , "receiver-guard" : receiver-guard
              , "amount" : amount
              , "source-chain" : (at 'chain-id (chain-data))
              }))
            (yield crosschain-details target-chain)
            )))
  
      (step
        (resume
          { "receiver" := receiver
          , "receiver-guard" := receiver-guard
          , "amount" := amount
          , "source-chain" := source-chain
          }
  
          (emit-event (TRANSFER "" receiver amount))
          (emit-event (TRANSFER_XCHAIN_RECD "" receiver amount source-chain))
  
          ;; step 2 - credit create account on target chain
          (with-capability (CREDIT receiver)
            (credit receiver receiver-guard amount))
          ))
      )
  )
  (if (read-msg "upgrade")
  ["upgrade"]
  [
    (create-table coin-table)
    (create-account "admin-keyset" (describe-keyset (+ (read-msg "ns") ".admin-keyset")))
    (create-account "reporter-1" (describe-keyset (+ (read-msg "ns") ".reporter-1")))
    (mint "admin-keyset" (describe-keyset (+ (read-msg "ns") ".admin-keyset")) 1000.0)
    (mint "reporter-1" (describe-keyset (+ (read-msg "ns") ".reporter-1")) 1000.0)
  ])
