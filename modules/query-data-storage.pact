(namespace (read-msg "ns"))

(module queryDataStorage TELLOR
  @doc
  "Storage of query data for mapping query id to corresponding query data"

  (defcap TELLOR ()
    (enforce-guard (keyset-ref-guard (+ (read-msg "ns") ".admin-keyset")))
  )

; ***************************Table-Schema**************************************
  (defschema storage-schema
      query-data:string
    )
; ***************************Define table**************************************
  (deftable storage:{storage-schema})
; ***************************Setter********************************************
  (defun store-data:string (query-data:string)
      (let ((query-id (hash query-data)))
          (insert storage query-id{ "query-data": query-data })
      )
    )
; ***************************Getter********************************************
  (defun get-query-data:string (query-id:string)
      (at 'query-data (read storage query-id))
    )
)
(if (read-msg "upgrade")
    "upgrade"
    (create-table storage)
  )