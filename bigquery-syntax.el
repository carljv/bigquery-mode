(provide 'bigquery-syntax)

(defconst bigquery/syntax-data-types-atomic
  '("int64" "float64" "numeric"
    "bool" "string" "bytes"
    "date" "datetime" "time" "timestamp"))


(defconst bigquery/syntax-data-types-composite
  '("array" "struct" "geography"))


(defconst bigquery/syntax-data-types
  (append bigquery/syntax-data-types-atomic
	  bigquery/syntax-data-types-composite))


(defconst bigquery/syntax-keywords
  '("all" "alter" "and" "as" "asc" "assert"
    "begin" "between" "break" "by"
    "call" "case" "continue" "create" "cross"
    "declare" "delete" "desc" "distinct" "drop"
    "else" "end" "except" "exception" "execute" "exists" 
    "false" "following" "for" "from" "full" "function"
    "group"
    "having"
    "ignore" "immediate" "in" "inner" "insert" "intersect"
    "interval" "is" "iterate"
    "join"
    "leave" "left" "like" "limit" "loop"
    "dayofweek" "day" "dayofyear" "week" "isoweek" "month"
    "quarter" "year" "isoyear"
    "sunday" "monday" "tuesday" "wednesday" "thursday"
    "friday" "saturday"
    "hour" "minute" "second" "millisecond" "microsecond"
    "time zone"
    "materialized" "merge"
    "not" "null" "nulls"
    "of" "on" "or" "order" "outer" "over"
    "partition" "preceding" "procedure"
    "raise" "respect" "return" "right" "rollup" "rows" 
    "unbounded" "union" "unnest" "update" "using"
    "view"
    "when" "where" "while" "window" "with"
    "safe" "select" "set"
    "table" "then" "true"))


(defconst bigquery/syntax-functions-conversion
  '("cast" "safe_cast"))


(defconst bigquery/syntax-functions-analytic
  '("any_value" "array_agg" "avg"
   "corr" "count" "countif" "covar_pop" "covar_samp"
   "max" "min"
   "st_clusterdbscan" "stddev_pop" "stddev_samp" "string_agg" "sum"
   "var_pop" "var_samp"))


(defconst bigquery/syntax-functions-aggregate
  '("any_value"
    "array_agg"
    "array_concat_agg"
    "avg"
    "bit_and" "bit_or" "bit_xor"
    "count" "countif"
    "logical_and" "logical_or"
    "max" "min"
    "string_agg"
    "sum"))


(defconst bigquery/syntax-functions-approximate-aggregate
  '("approx_count_distinct"
    "approx_quantiles"
    "approx_top_count"
    "approx_top_sum"))


(defconst bigquery/syntax-functions-array
  '("array"
    "array_concat"
    "array_length"
    "array_to_string"
    "generate_array"
    "generate_date_array"
    "generate_timestamp"
    "offset"
    "ordinal"
    "array_reverse"
    "safe_offset"
    "safe_ordinal"))


(defconst bigquery/syntax-functions-bit
  '("bit_count"))


(defconst bigquery/syntax-functions-date
  '("current_date"
    "extract"
    "date"
    "date_add"
    "date_sub"
    "date_diff"
    "date_trunc"
    "date_from_unix_date"
    "format_date"
    "parse_date"
    "unix_date"))


(defconst bigquery/syntax-functions-datetime
  '("datetime"
    "extract"
    "datetime_add"
    "datetime_sub"
    "datetime_diff"
    "datetime_trunc"
    "format_datetime"
    "parse_datetime"))


(defconst bigquery/syntax-functions-debugging
  '("error"))


(defconst bigquery/syntax-functions-federated
  '("external_query"))


(defconst bigquery/syntax-functions-geography
  '("st_geopoint" "st_makeline" "st_makepolygon" "st_makepolygonoriented"
    "st_geogfromtext" "st_geogfromwkb" "st_geogpointfromgeohash"
    "st_asgeojson" "st_astext" "st_geohash" "st_asbinary" "st_boundary"
    "st_centroid" "st_closestpoint" "st_difference" "st_intersection"
    "st_snaptogrid" "st_simplify" "st_convexhull" "st_union" "st_contains"
    "st_coveredby" "st_covers" "st_disjoin" "st_dwithin" "st_equals"
    "st_intersects" "st_intersectbox" "st_touches" "st_within" "st_isempty"
    "st_iscollection" "st_dimension" "st_numpoints" "st_dump" "st_x"
    "st_y" "st_area" "st_distance" "st_length" "st_maxdistance"
    "st_perimeter" "st_union_agg" "st_centroid_agg" "st_clusterdbscan"))


(defconst bigquery/syntax-functions-hash
  '("farm_fingerprint"
    "md5" "sha1" "sha256" "sha512"))


(defconst bigquery/syntax-functions-hyperloglogpp
  '("hll_count.init"
    "hll_count.merge"
    "hll_count.merge_partial"
    "hll_count.extract"))


(defconst bigquery/syntax-functions-json
  '("json_extract"
    "json_extract_scalar"
    "json_extract_array"
    "json_query"
    "json_value"
    "to_json_string"))


(defconst bigquery/syntax-functions-math
  '("abs" "sign" "is_inf" "is_nan"
    "ieee_divide" "rand" "sqrt" "pow"
    "power" "exp" "ln" "log" "log10"
    "greatest" "least" "div" "safe_divide"
    "safe_multiple" "safe_negate" "safe_add"
    "safe_subtract" "mod" "round" "trunc"
    "ceil" "ceiling" "floor" "cos" "cosh"
    "acos" "acosh" "sin" "sinh" "asin" "asinh"
    "tan" "tanh" "atan" "atanh" "atan2"
    "range_bucket"))


(defconst bigquery/syntax-functions-navigation
  '("first_value" "last_value" "nth_value"
    "lead" "lag" "percentile_cont"
    "percentile_disc"))


(defconst bigquery/syntax-functions-net
  '("net.ip_from_string"
    "net.safe_ip_from_string"
    "net.ip_to_string"
    "net.ip_net_mask"
    "net.ip_trunc"
    "net.ipv4_from_int64"
    "net.ipv4_to_int64"
    "net.host"
    "net.public_suffix"
    "net.reg_domain"))


(defconst bigquery/syntax-functions-numbering
  '("rank" "dense_rank" "percent_rank"
    "cume_dist" "ntile"))


(defconst bigquery/syntax-functions-security
  '("session_user"))


(defconst bigquery/syntax-functions-statistical
  '("corr" "covar_pop" "covar_samp"
    "stddev_pop" "stddev_samp" "stddev"
    "var_pop" "var_samp" "variance"))


(defconst bigquery/syntax-functions-string
  '("byte_length" "char_length"
    "character_length" "code_points_to_bytes"
    "code_points_to_string" "concat"
    "ends_with" "format" "from_base32"
    "from_base64" "from_hex" "length" "lpad"
    "lower" "ltrim" "normalize"
    "normalize_and_casefold" "regexp_contains"
    "regexp_extract" "regexp_extract_all"
    "regexp_replace" "replace" "repeat"
    "reverse" "rpad" "rtrim"
    "safe_convert_bytes_to_string"
    "split" "starts_with" "strpos"
    "substr" "to_base32" "to_base64"
    "to_code_points" "to_hex" "trim"
    "upper"))


(defconst bigquery/syntax-functions-time
  '("current_time" "time" "extract"
    "time_add" "time_sub" "time_diff"
    "time_trunc" "format_time" "parse_time"))


(defconst bigquery/syntax-functions-timestamp
  '("current_timestamp" "extract" "string"
    "timestamp" "timestamp_add" "timestamp_sub"
    "timestamp_diff" "timestamp_trun"
    "format_timestamp" "parse_timestamp"
    "timestamp_seconds" "timestamp_millis"
    "timestamp_micros" "unix_seconds"
    "unix_millis" "unix_micros"))


(defconst bigquery/syntax-functions-uuid
  '("generate_uuid"))


(defconst bigquery/syntax-functions-conditionals
  '("coalesce" "if" "ifnull"))


(defconst bigquery/syntax-functions
  (append bigquery/syntax-functions-analytic
	  bigquery/syntax-functions-aggregate
	  bigquery/syntax-functions-approximate-aggregate
	  bigquery/syntax-functions-array
	  bigquery/syntax-functions-bit
	  bigquery/syntax-functions-conditionals
	  bigquery/syntax-functions-conversion
	  bigquery/syntax-functions-date
	  bigquery/syntax-functions-datetime
	  bigquery/syntax-functions-debugging
	  bigquery/syntax-functions-federated
	  bigquery/syntax-functions-geography
	  bigquery/syntax-functions-hash
	  bigquery/syntax-functions-hyperloglogpp
	  bigquery/syntax-functions-json
	  bigquery/syntax-functions-math
	  bigquery/syntax-functions-navigation
	  bigquery/syntax-functions-net
	  bigquery/syntax-functions-numbering
	  bigquery/syntax-functions-security
	  bigquery/syntax-functions-statistical
	  bigquery/syntax-functions-string
	  bigquery/syntax-functions-time
	  bigquery/syntax-functions-timestamp
	  bigquery/syntax-functions-uuid))
	 

(defconst bigquery/syntax-operators
  '("." "[]" "-" "~" "*" "/" "||"
    "+" "-" "<<" ">>" "&" "^" "|"
    "=" "<" ">" "<=" ">=" "!=" "<>"))


(defconst bigquery/syntax-system-variables
  '("@@current_job_id"
    "@@last_job_id"
    "@@project_id"
    "@@row_count"
    "@@script.bytes_billed"
    "@@script.bytes_processed"
    "@@script.creation_time"
    "@@script.job_id"
    "@@script.num_child_jobs"
    "@@script.slot_ms"
    "@@time_zone"
    "@@error.formatted_stack_trace"
    "@@error.message"
    "@@error.stack_trace"
    "@@error.statement_text"))


(defun bigquery/syntax-make-font-lock-entry (kws face)
  (cons (concat "\\_<" (regexp-opt kws) "\\_>") face))


(defconst bigquery/syntax-font-lock-defaults
  (list
   (bigquery/syntax-make-font-lock-entry
    bigquery/syntax-keywords 'font-lock-keyword-face)
   (bigquery/syntax-make-font-lock-entry
    bigquery/syntax-functions 'font-lock-function-name-face)
   (bigquery/syntax-make-font-lock-entry
    bigquery/syntax-data-types 'font-lock-type-face)
   (bigquery/syntax-make-font-lock-entry
    bigquery/syntax-system-variables 'font-lock-variable-name-face)))


(defconst bigquery/syntax-table
  (let ((table (make-syntax-table)))
    ;; C-style comments /**/ (see elisp manual "Syntax Flags"))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    ;; # is also a single line comment.
    (modify-syntax-entry ?# "<   " table)
    ;; double-dash starts comments
    (modify-syntax-entry ?- ". 12b" table)
    ;; newline and formfeed end comments
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\f "> b" table)
    ;; single quotes (') delimit strings
    (modify-syntax-entry ?' "\"" table)
    ;; double quotes (") don't delimit strings
    (modify-syntax-entry ?\" "." table)
    ;; Make these all punctuation
    (mapc (lambda (c) (modify-syntax-entry c "." table))
          (string-to-list "!$%&+,:;<=>?\\|"))
    ;; Make . a word character, since it's an accessor.
    ;; This way "keyword" doesn't get highlighted in
    ;; "array.keyword"
    (modify-syntax-entry ?. "w" table)
    ;; @@ is used for system variables.
    (modify-syntax-entry ?@ "w" table)
    table))


(defconst bigquery/syntax-completions
  (append bigquery/syntax-functions bigquery/syntax-keywords bigquery/syntax-system-variables))


(defun company-bigquery-backend (command &optional arg &rest ignored)
   (interactive (list 'interactive))

   (cl-case command
     (interactive (company-begin-backend 'company-sample-backend))
     (prefix (and (eq major-mode 'bigquery-mode)
                 (company-grab-symbol)))
     (candidates
     (cl-remove-if-not
       (lambda (c) (string-prefix-p arg c))
       bigquery/syntax-completions))))


	  
