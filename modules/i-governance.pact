(namespace (read-msg "ns"))
(interface i-governance

  (defun get-vote-count:integer ()
  )
  (defun get-vote-tally-by-address:integer (voter:string)
  )
  (defun register-gov-guard:guard () )

  )
