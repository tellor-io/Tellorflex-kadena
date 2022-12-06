(namespace "free")
(interface tellor-governance


  (defun begin-dispute:string (account:string query-id:string timestamp:integer)
  )
  (defun execute-vote:string (dispute-id:integer)
  )
  (defun get-vote-count:integer ()
  )
  (defun get-vote-tally-by-address:integer (voter:string)
  )

)
