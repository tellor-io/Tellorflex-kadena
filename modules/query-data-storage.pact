(namespace (read-msg "ns"))

(module queryDataStorage NOT-UPGRADEABLE
  @doc
  "Storage of query data for mapping query id to corresponding query data"

  (defcap NOT-UPGRADEABLE () (enforce false "Enforce non-upgradeability"))

; ***************************Table-Schema**************************************
  (defschema storage-schema
      query-data:string
    )
; ***************************Define table**************************************
  (deftable storage:{storage-schema})
; ***************************Setter********************************************
  (defun store-data:string (query-data:string)
    (write storage (hash query-data) { "query-data": query-data })
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