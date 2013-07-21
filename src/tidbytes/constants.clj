(ns tidbytes.constants)

(def
  ^{:doc "All the acknowledged HTTP response codes and their corresponding text."}
  status-codes
  {
    ;; Information
    100 "Continue",
    101 "Switching Protocols",
    102 "Processing",
    ;; Success
    200 "OK",
    201 "Created",
    202 "Accepted",
    203 "Non-Authoritative Information",
    204 "No Content",
    205 "Reset Content",
    206 "Partial Content",
    207 "Multi-Status",
    208 "Already Reported",
    226 "IM Used",
    ;; Redirection
    300 "Multiple Choices",
    301 "Moved Permanently",
    302 "Found",
    303 "See Other",
    304 "Not Modified",
    305 "Use Proxy",
    306 "Switch Proxy",
    307 "Temporary Redirect",
    308 "Permanent Redirect",
    ;; Client Error
    400 "Bad Request",
    401 "Unauthorized",
    402 "Payment Required"
    403 "Forbidden",
    404 "Not Found",
    405 "Method Not Allowed",
    406 "Not Acceptable",
    407 "Proxy Authentication Required",
    408 "Request Timeout",
    409 "Conflict",
    410 "Gone",
    411 "Length Required",
    412 "Precondition Failed",
    413 "Request Entity Too Large",
    414 "Request-URI Too Long",
    415 "Unsupported Media Type",
    416 "Requested Range Not Satisfiable",
    417 "Expectation Failed",
    418 "I'm a teapot",
    420 "Enhance Your Calm",
    423 "Locked",
    424 "Failed Dependency",
    425 "Unordered Collection",
    426 "Upgrade Required",
    428 "Precondition Required",
    429 "Too Many Requests",
    431 "Request Header Fields Too Large",
    444 "No Response",
    449 "Retry With",
    450 "Blocked by Windows Parental Controls",
    451 "Unavailable For Legal Reasons",
    494 "Request Header Too Large",
    495 "Cert Error",
    496 "No Cert",
    497 "HTTP to HTTPS",
    499 "Client Closed Request"
  })

(def
  ^{:doc "A single line break as specified by HTTP."}
  linebreak
  "\r\n")

(def
  ^{:doc "A double line break as specified by HTTP."}
  fullbreak
  "\r\n\r\n")