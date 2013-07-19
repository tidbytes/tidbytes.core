(ns tidbytes.core-test
  (:require [clojure.test :refer :all]
        	  [tidbytes.core :refer :all]))

(def get-request
  (tidbytes.core.Request.
  	(tidbytes.core.RequestLine. "GET" "/subpage/mypage.html" "HTTP/1.1")
  	{ :host "testdomain.com" }
  	"My body is ready."))

(deftest fetching-request-information
  (testing "Ensure requests can supply request line information."
  	(is (not (nil? (-> get-request :request-line :method))))
  	(is (not (nil? (-> get-request :request-line :uri))))
  	(is (not (nil? (-> get-request :request-line :version)))))
  (testing "Ensure requests can supply header and body information."
  	(is (not (nil? ((-> get-request :headers) :host))))
  	(is (not (nil? (-> get-request :body))))))