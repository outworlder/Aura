(use ssax)
(use uri-common)
(use http-client)
(use intarweb)

;; Eve central market data

(determine-proxy
 (lambda (uri)
   (uri-reference "http://localhost:3128")))

(define *method-urls*
  '((marketstat . "http://api.eve-central.com/api/marketstat")
    (quicklook . "http://eve-central.com/api/quicklook")))
  
(define (request-api method params)
  (with-input-from-request
   (make-request method: 'POST
    		 uri: (uri-reference (cdr (assq method *method-urls*))))
   (filter-false params)
   (lambda ()
     (ssax:xml->sxml (current-input-port) '()))))

(define (marketstat item #!key hours minq regionlimit)
  (request-api 'marketstat
	       `((typeid . ,item)
		 (hours . ,hours)
		 (minQ . ,minq)
		 (regionlimit . ,regionlimit))))

(define (quicklook item #!key sethours regionlimit usesystem minq)
  (request-api 'quicklook
	       `((typeid . ,item)
		 (sethours . ,sethours)
		 (regionlimit . ,regionlimit)
		 (usesystem . ,usesystem)
		 (setminQ . ,minq))))

;; EVE Central parsing

(define-record market-data volume average max min standard-deviation median)
(define-record market-item typeid type stat)

(define (parse-marketstat sxml)
  #f)

;; (define (parse-market-stat-response xml)
;;   (map (lambda (item)
;; 	 )((sxpath '(// marketstat type)) marketstat-response))

(define (parse-market-item item)
  (let ([type-id (cadar ((sxpath '(// marketstat type @ id)) item))]
	[node ])

(define (parse-market-data alist)
  (apply make-market-data (get-data alist 'volume 'avg 'max 'min 'stddev 'median)))

(define (get-data alist #!rest keys)
  (map (lambda (item)
	 (print item)
	 (cdr (assoc item alist))) keys))

;; Utility functions
;; Filter unused (#f) parameters in the given alist
(define (filter-false alist)
  (compress (map
	     (lambda (item)
	       (cdr item)) alist)
	    alist))
