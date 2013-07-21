(ns tidbytes.core
  (:import (java.net ServerSocket Socket)
           (java.io BufferedReader InputStreamReader PrintWriter)
           (java.text SimpleDateFormat)
           (java.util Date Locale))
  (:use [clojure.string :only [split] :as string]
        [tidbytes.constants :as const]))

;;; ### Records and protocols.
;;; Defines a few immutable data types useful for passing around data.
(defrecord RequestLine
  [^String method
   ^String uri
   ^String version])

(defrecord Request
  [^RequestLine request-line
   ^clojure.lang.PersistentArrayMap headers
   ^String body])

(defrecord ResponseLine
  [^String version
   ^Integer status-code
   ^String reason])

;;; ### The `Responsive` protocol
;;; `Responsive` is the primary protocol for responses, and anything
;;; implementing it is capable of being served as an HTTP response.
(defprotocol Responsive
  (^ResponseLine status-line [this])
  (^clojure.lang.PersistentArrayMap headers [this])
  (^String body [this]))

;;; ### Protocol extensions
;;; Extend various types to implement the Response protocol. This makes is
;;; possible to simply return a string or an integer as a response. The
;;; interpretation is:
;;; - An integer is a status code. If application defines a response for this
;;;   particular status code, that response is returned. Otherwise a response
;;;   with an empty body, default headers, and the given status code is
;;;   returned.
;;; - A string is the body of a response with status code 200 and default
;;;   headers.
(extend-protocol Responsive
  Long
  (status-line [this] (ResponseLine. "HTTP/1.1" this (status-codes this)))
  (headers [_] {})
  (body [_] "")
  String
  (status-line [_] (ResponseLine. "HTTP/1.1" 200 "OK"))
  (headers [_] {})
  (body [this] this))

;;; `Response` is a convenient way to create an instance of `Responsive`, given
;;; a status code, a map with headers, and a string with the body.
(deftype Response
  [status-code headers body]
  Responsive
  (status-line [_] (ResponseLine. "HTTP/1.1" status-code
                                  (status-codes status-code)))
  (headers [_] headers)
  (body [_] body))

;;; `handlers` holds the set of handlers to choose from when a request arrives.
(def handlers (atom #{}))

;;; Define a bunch of private helper functions.
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
  (first (sort-by #(fitness request %) @handlers)))

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

(defn- send-response
  [response writer reader client]
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
  (.close client)
  (.close writer)
  (.close reader)
  nil)

;;; Add a handler to the handler list.
(defn add-handler
  "Adds a `handler` to the list of handlers used in processing requests."
  [handler]
  (swap! handlers conj handler))

;;; Start the server.
(defn start
  "Starts the server, listening for HTTP requests until stopped."
  []
  (let [server (ServerSocket. 8080)]  
    (println "Server running on port 8080") 
    (while true
      (let [client (.accept server)]
        (future-call
          (let [reader (-> (.getInputStream client)
                           (InputStreamReader.)
                           (BufferedReader.))
                writer (-> (.getOutputStream client)
                           (PrintWriter.))
                request (Request.
                          (get-request-line reader)
                          (get-headers reader)
                          (get-body reader))
                handler (get-handler request)
                response (handler request)]
            (send-response response writer reader client)))))))