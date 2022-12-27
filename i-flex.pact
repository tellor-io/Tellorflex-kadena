(namespace "free")
(interface i-flex

  (defun init-gov-guard:string (guard:guard))

  (defun get-block-number-by-timestamp:integer (query-id:string timestamp:integer))

  (defun get-reporter-by-timestamp:string (query-id:string timestamp:integer))

  (defun get-reports-submitted-by-address:integer (reporter:string))

  (defun remove-value:string (query-id:string timestamp:integer))

  (defun retrieve-data:string (query-id:string timestamp:integer))

  (defun slash-reporter:integer (reporter:string recipient:string))

  (defun stake-amount:integer ())

  (defun get-staker-info:object (reporter:string))

)
