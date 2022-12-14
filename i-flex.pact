(namespace (read-msg 'ns))
(interface i-flex

  (defschema data-before-value
    timestamp:integer
    value:string)

  (defun init-gov-guard:string (guard:guard))

  (defun get-block-number-by-timestamp:integer (query-id:string timestamp:integer))

  (defun get-data-before:object{data-before-value}
    (query-id:string timestamp:integer) )

  (defun get-reporter-by-timestamp:string (query-id:string timestamp:integer))

  (defun get-reports-submitted-by-address:integer (reporter:string))

  (defun remove-value:string (query-id:string timestamp:integer))

  (defun retrieve-data:string (query-id:string timestamp:integer))

  (defun slash-reporter:integer (reporter:string recipient:string))

  (defun stake-amount:integer ())

  (defun get-staker-info:object (reporter:string))

)
