(ns tidbytes.core
  (:import (java.net ServerSocket Socket SocketTimeoutException)
           (java.io BufferedReader InputStreamReader PrintWriter)
           (java.text SimpleDateFormat)
           (java.util Date Locale))
  (:use [clojure.string :only [split] :as string]
        [tidbytes.constants :as const]))

;;; ## Records, Protocols, and Types
;;; Define all relevant records and protocols used in tibytes.core.

(defrecord
  RequestLine
  [^String method
   ^String uri
   ^String version])

(defrecord
  Request
  [^RequestLine request-line
   ^clojure.lang.PersistentArrayMap headers
   ^String body])

(defrecord
  ResponseLine
  [^String version
   ^Integer status-code
   ^String reason])

(defprotocol
  Responsive
  "`Protocol`

   Responsive captures the concept of something tidbytes can serve to a client
   as a response. Any Responsive type has to provide the following functions:

   - `(status-line [this])`: returns a `ResponseLine`.
   - `(headers [this])`: returns a map of headers in the form of `:keyword` to
     `\"string\"`.
   - `(body [this])`: the body of the response in the form of a string."
  (^ResponseLine status-line [this])
  (^clojure.lang.PersistentArrayMap headers [this])
  (^String body [this]))

(deftype Response
  [status-code headers body]
  Responsive
  (status-line [_] (ResponseLine. "HTTP/1.1" status-code
                                  (status-codes status-code)))
  (headers [_] headers)
  (body [_] body))

(extend-protocol Responsive
  Long
  (status-line [this] (ResponseLine. "HTTP/1.1" this (status-codes this)))
  (headers [_] {})
  (body [_] "")
  String
  (status-line [_] (ResponseLine. "HTTP/1.1" 200 "OK"))
  (headers [_] {})
  (body [this] this))

;;; Correct the metadata to display more pleasingly in documentation.

(alter-meta! #'map->RequestLine assoc :no-doc true)
(alter-meta! #'->RequestLine assoc :doc
  "`Record`

   A RequestLine is the first line of an HTTP request.

   - method: `:get`, `:post`, or similar HTTP method.
   - uri: the uri relative to the root domain (f.x. `/path/to/file.html`).
   - version: the HTTP version (`1.0` or `1.1` in time  of writing).")

(alter-meta! #'map->Request assoc :no-doc true)
(alter-meta! #'->Request assoc :doc
  "`Record`

   A Request is the data type passed to handlers. It represents a full HTTP
   request.

   - request-line: see `->RequestLine`.
   - headers: a map of headers in the form of `keyword` to `string`.
   - body: the body of the request in the form of a string.")

(alter-meta! #'map->ResponseLine assoc :no-doc true)
(alter-meta! #'->ResponseLine assoc :doc
  "`Record`

   A ResponseLine is the first line of an HTTP response.

   - version: the HTTP version (`1.0` or `1.1` in time  of writing).
   - status-code: see `tidbytes.constants/status-codes`.
   - reason: see `tidbytes.constants/status-codes`.")

(alter-meta! #'->Response assoc :doc
  "`Type`

   A Response is the data type returned by handlers. It represents a full HTTP
   response.

   - status-code: see `tidbytes.constants/status-codes`.
   - headers: a map of headers in the form of `keyword` to `string`.
   - body: the body of the response in the form of a string.")

;;; ## Private functions and data
;;; The state of the server is stored as agents.

(def
  ^{:private true}
  state (agent {}))
(send state assoc :handlers #{})
(send state assoc :running? false)

;;; Plenty of helper functions.

(defn- string-to-keyword
  "Converts the string to a lower-case keyword for headers."
  [string]
  (keyword (string/lower-case string)))

(defn- keyword-to-string
  "Converts a lower-case keyword to a proper-case string."
  [keyword]
  (let [parts (string/split (name keyword) #"-")
        big-parts (map string/capitalize parts)
        joined (string/join "-" big-parts)]
    joined))

(defn- fitness
  [^Request request
   handler]
  0)

(defn- get-request-line
  "Reads and makes sense of the first line in an HTTP request."
  [reader]
  (let [line (.readLine reader)
        string (if (nil? line) "" line)
        [method uri version] (split string #"\s")]
    (RequestLine. method uri version)))

(defn get-current-date
  []
  (str (.format (SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss"
                 Locale/ENGLISH) (Date.)) " GMT"))

(defn- get-all-lines
  [stream]
  (loop [line (.readLine stream)
         body []]
    (if (zero? (count line))
      body
      (recur
        (.readLine stream)
        (conj body line)))))

(defn- get-handler
  [request]
  (first (sort-by #(fitness request %) (:handlers @state))))

(defn- get-headers
  "Reads the headers section of an HTTP request and returns a key value map."
  [reader]
  (let [lines (get-all-lines reader)
        f (fn [line]
            (let [parts (split line #"\:\s")]
              (if (odd? (count parts))
                (conj parts "")
                parts)))]
    (map f lines)))

(defn- get-body
  "Reads the body, if any, of an HTTP request. Returns an empty string if no
  body is present."
  [reader]
  (if (.ready reader)
    (get-all-lines reader)
    ""))

(defn- write-response
  [response writer]
  (doto writer
    (.write (str (-> response status-line .version) " "
                 (-> response status-line .status-code) " "
                 (-> response status-line .reason) const/linebreak))
    (.write
      (string/join const/linebreak
        (for [[header value] (headers response)]
          (str (keyword-to-string header) ": " value))))
    (.write const/fullbreak)
    (.write (str (body response)))
    (.flush))
  nil)

;;; ## Public API

(defmacro defhandler
  "Docstring."
  [handler-name uri method args & body]
  `(do (defn ~handler-name ~args ~@body)
       (send state
         (fn [state#]
           (assoc state# :handlers
             (conj (:handlers state#) ~handler-name))))
       ``~handler-name))

(defn server-shutdown
  "Gracefully stops the server, serving any pending requests before doing so.
   The server may wait for a maximum of `2000` ms to allow blocking IO to
   complete safely."
  []
  (send state
    (fn [state]
      (assoc state :running? false))))

(defn server-start
  "Starts the server, listening for connections on the given port."
  [port]
  (let [server (doto (ServerSocket. port)
                 (.setSoTimeout 2000))]
    (if (do (send state #(assoc %1 :running? true))
            (await-for 2000 state))
      (while (:running? @state)
        (future-call
          (try
            (with-open [client (.accept server)
                        reader (-> (.getInputStream client)
                                   (InputStreamReader.)
                                   (BufferedReader.))
                        writer (-> (.getOutputStream client)
                                   (PrintWriter.))]
              (let [request (Request.
                              (get-request-line reader)
                              (get-headers reader)
                              (get-body reader))
                    handler (get-handler request)
                    response (handler request nil)]
                (write-response response writer)))
            (catch SocketTimeoutException exception)
            (catch Exception exception
              (server-shutdown)
              (format "Server failed unexpectedly. (%s)"
                      exception)))))
      (throw (Exception. "Could not reach agent. Something is very wrong.")))))