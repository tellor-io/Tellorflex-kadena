(namespace (read-msg 'ns))
(interface i-governance


  (defun begin-dispute:string (account:string query-id:string timestamp:integer)
  )
  (defun get-vote-count:integer ()
  )
  (defun get-vote-tally-by-address:integer (voter:string)
  )

)
