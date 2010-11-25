(use ssax)
(use uri-common)
(use http-client)
(use intarweb)
(use sxpath)
(use coops)
(use coops-primitive-objects)

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

(define-class <market-data> ()
  (volume average max min standard-deviation median all buy sell))

(define-class <market-item> ()
  (typeid all buy sell))

(define-method (print-object (obj <market-item>) #!optional (port (current-output-port)))
  (fprintf port "<#market-item typeID:[~A] all:[~A] buy:[~A] sell:[~A]>"
	   (slot-value obj 'typeid)
	   (slot-value obj 'all)
	   (slot-value obj 'buy)
	   (slot-value obj 'sell)))

(define-method (print-object (obj <market-data>) #!optional (port (current-output-port)))
  (fprintf port "<#market-data volume:[~A] average:[~A] max:[~A] min:[~A] stddev:[~A] median:[~A]>"
	   (slot-value obj 'volume)
	   (slot-value obj 'average)
	   (slot-value obj 'max)
	   (slot-value obj 'min)
	   (slot-value obj 'standard-deviation)
	   (slot-value obj 'median)))


(define (parse-marketstat sxml)
  (map (lambda (item)
	 (parse-market-item item)) ((sxpath '(// marketstat *)) sxml)))

;; (define (parse-market-stat-response xml)
;;   (map (lambda (item)
;; 	 )((sxpath '(// marketstat type)) marketstat-response))

(define (parse-market-item item)
  (let ([type-id (string->number (cadar ((sxpath '(@ id)) item)))]
	[node ((sxpath '(*)) item)]
	[market-item (make <market-item>)])
    (for-each (lambda (subnode)
	       (let ([type (car subnode)]
		     [data (parse-market-data (cdr subnode))])
		 (set! (slot-value market-item 'typeid) type-id)
		 (set! (slot-value market-item type) data)))
	     node)
    market-item))

(define (parse-market-data alist)
  (apply make
	 (flatten (list <market-data>
			(flatten (zip
				  '(volume average max min standard-deviation median)
				  (all->number (get-data alist 'volume 'avg 'max 'min 'stddev 'median))))))))

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

(define-method (buy-price (item <market-item>))
  (slot-value (slot-value item 'buy) 'min))

(define-method (sell-price (item <market-item>))
  (slot-value (slot-value item 'sell) 'max))