
(defvar bigquery/data-types--atomic
  '("int64" "float64" "numeric"
    "bool" "string" "bytes"
    "date" "datetime" "time" "timestamp"))


(defvar bigquery/data-types--composite
  '("array" "struct" "geography"))


(defvar bigquery/keywords
  '("all" "alter" "and" "as" "asc" "assert"
    "begin" "between" "break" "by"
    "call" "case" "continue" "create" "cross"
    "declare" "delete" "desc" "distinct" "drop"
    "else" "end" "except" "exception" "execute" "exists" 
    "false" "following" "for" "from" "full" "function"
    "group"
    "having"
    "if" "ignore" "immediate" "in" "inner" "insert" "intersect"
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


(defvar bigquery/functions--analytic
  '("any_value" "array_agg" "avg"
   "corr" "count" "countif" "covar_pop" "covar_samp"
   "max" "min"
   "st_clusterdbscan" "stddev_pop" "stddev_samp" "string_agg" "sum"
   "var_pop" "var_samp"))


(defvar bigquery/functions--aggregate
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

(defvar bigquery/functions--approximate-aggregate
  '("approx_count_distinct"
    "approx_quantiles"
    "approx_top_count"
    "approx_top_sum"))

(defvar bigquery/functions--array
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

(defvar bigquery/functions--bit
  '("bit_count"))


(defvar bigquery/functions--date
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


(defvar bigquery/functions--datetime
  '("datetime"
    "extract"
    "datetime_add"
    "datetime_sub"
    "datetime_diff"
    "datetime_trunc"
    "format_datetime"
    "parse_datetime"))


(defvar bigquery/functions--debugging
  '("error"))


(defvar bigquery/functions--federated
  '("external_query"))


(defvar bigquery/functions--geography
  '())


(defvar bigquery/functions--hash
  '("farm_fingerprint"
    "md5" "sha1" "sha256" "sha512"))


(defvar bigquery/functions--hyperloglogpp
  '("hll_count.init"
    "hll_count.merge"
    "hll_count.merge_partial"
    "hll_count.extract"))


(defvar bigquery/functions--json
  '("json_extract"
    "json_extract_scalar"
    "json_extract_array"
    "json_query"
    "json_value"
    "to_json_string"))


(defvar bigquery/functions--math
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


(defvar bigquery/functions--navigation
  '("first_value" "last_value" "nth_value"
    "lead" "lag" "percentile_cont"
    "percentile_disc"))


(defvar bigquery/functions--net
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


(defvar bigquery/functions--numbering
  '("rank" "dense_rank" "percent_rank"
    "cume_dist" "ntile"))


(defvar bigquery/functions--security
  '("session_user"))


(defvar bigquery/functions--statistical
  '("corr" "covar_pop" "covar_samp"
    "stddev_pop" "stddev_samp" "stddev"
    "var_pop" "var_samp" "variance"))


(defvar bigquery/functions--string
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


(defvar bigquery/functions--time
  '("current_time" "time" "extract"
    "time_add" "time_sub" "time_diff"
    "time_trunc" "format_time" "parse_time"))
   

(defvar bigquery/function--timestamp
  '("current_timestamp" "extract" "string"
    "timestamp" "timestamp_add" "timestamp_sub"
    "timestamp_diff" "timestamp_trun"
    "format_timestamp" "parse_timestamp"
    "timestamp_seconds" "timestamp_millis"
    "timestamp_micros" "unix_seconds"
    "unix_millis" "unix_micros"))


(defvar bigquery/functions--uuid
  '("generate_uuid"))


(defvar bigquery/functions--operators
  '("." "[]" "-" "~" "*" "/" "||"
    "+" "-" "<<" ">>" "&" "^" "|"
    "=" "<" ">" "<=" ">=" "!=" "<>"))


(defvar bigquery/functions--conditional
  '("coalesce" "if" "ifnull"))


(defvar bigquery/system-variables
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


