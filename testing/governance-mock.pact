(namespace "free")

(if (read-msg "upgrade")
  "Upgrading contract"

  [
    (enforce-keyset (read-keyset "tellor-admin-keyset"))
    (define-keyset "free.tellor-admin-keyset" (read-keyset "tellor-admin-keyset"))
  ]
)

(module governance-mock TELLOR-GOV
  (implements i-governance)

  (defcap PRIVATE () true )

  (defun private:bool () (require-capability (PRIVATE)) )

  (defcap TELLOR-GOV ()
    (enforce-guard (keyset-ref-guard "free.tellor-admin-keyset"))
  )

  (defun call-tellorflex ()
    (let ((tellorflex:module{i-flex} (at 'oracle (read global 'global))))
      (with-capability (TELLOR-GOV)
        (tellorflex::init-gov-guard (create-user-guard (private)))
      )
    )
  )

  (defschema global-schema
    oracle:module{i-flex}
    vote-count:integer)

  (defschema vote-tally-by-address
    vote-tally-by-address:integer)

  (defschema vote-schema
    voters:[string])

  (deftable vote-table:{vote-schema})
  (deftable vote-tally:{vote-tally-by-address})
  (deftable global:{global-schema})

  (defun constructor (oracle)
    (insert global 'global { 'oracle: oracle , 'vote-count: 0})
  )
  (defun begin-dispute:string (account:string query-id:string timestamp:integer)
    (with-read global 'global { 'vote-count := vote-count}
    (update global 'global { 'vote-count: (+ 1 vote-count)}))
  )
  (defun vote (dispute-id:integer supports:bool invalid-query:bool voter-account:string)
    (let ((vote-count (at 'vote-count (read global 'global))))
      (enforce (and (<= dispute-id vote-count) (> dispute-id 0)) "Vote doesn't exist"))
    (with-default-read vote-table (int-to-str 10 dispute-id)
      { 'voters : [] }{ 'voters := voters }
    (enforce (not (contains voter-account voters)) "Voter has already voted")
    (with-default-read vote-tally voter-account
      { 'vote-tally-by-address: 0 }{ 'vote-tally-by-address := vote-tally-by-address }
      (write vote-tally voter-account { 'vote-tally-by-address: (+ vote-tally-by-address 1)}))

      (write vote-table (int-to-str 10 dispute-id) { 'voters: (+ voters [voter-account])}))
    )
  (defun get-vote-tally-by-address:integer (voter:string)
    (try 0 (at 'vote-tally-by-address (read vote-tally voter))))
  (defun get-vote-count:integer ()
    (at 'vote-count (read global 'global)))
)
