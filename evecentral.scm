(use ssax)
(use uri-common)
(use http-client)
(use intarweb)
(use sxpath)

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

(define (marketstat items #!key hours minq regionlimit)
  (request-api 'marketstat
	       (append (map (lambda (item) `(typeid . ,item)) items)
		       `( 
			 (hours . ,hours)
			 (minQ . ,minq)
			 (regionlimit . ,regionlimit)))))

(define (quicklook items #!key sethours regionlimit usesystem minq)
  (request-api 'quicklook
	       `( ,(map (lambda (item) `(typeid . ,item)) items)
		 (sethours . ,sethours)
		 (regionlimit . ,regionlimit)
		 (usesystem . ,usesystem)
		 (setminQ . ,minq))))

;; EVE Central parsing

(define-record market-data volume average max min standard-deviation median)
(define-record market-item typeid type data)

(define (parse-marketstat sxml)
  (map (lambda (item)
	 (parse-market-item item)) ((sxpath '(// marketstat *)) sxml)))

;; (define (parse-market-stat-response xml)
;;   (map (lambda (item)
;; 	 )((sxpath '(// marketstat type)) marketstat-response))

(define (parse-market-item item)
  (let ([type-id (string->number (cadar ((sxpath '(@ id)) item)))]
	[node ((sxpath '(*)) item)])
    (map (lambda (subnode)
	   (let ([type (car subnode)]
		 [data (parse-market-data (cdr subnode))])
	     (apply make-market-item (list type-id type data)))) node)))
    

(define (parse-market-data alist)
  (apply make-market-data (all->number (get-data alist 'volume 'avg 'max 'min 'stddev 'median))))

(define (get-data alist #!rest keys)
  (map (lambda (item)
	 (cdr (assoc item alist))) keys))

;; Utility functions
;; Filter unused (#f) parameters in the given alist
(define (filter-false alist)
  (compress (map
	     (lambda (item)
	       (cdr item)) alist)
	    alist))

(define (all->number list)
  (map (lambda (item)
	 (string->number (car item))) list))