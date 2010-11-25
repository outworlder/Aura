(include "evecentral")
(include "evedatabase")

(define-class <bill-of-material> ()
  (item amount price))

(define (get-item-dependencies item-name)
  (let ([item (find-all-by-string <inventory-types> 'typeName item-name)])
    (if (null? item)
	(begin
	  (print "Item \"" item-name "\" not found.")
	  #f)
	(map (lambda (match)
	       (make-production-tree match)) item))))

(define (retrieve-prices tree)
  (parse-marketstat (marketstat (filter-prices tree))))

(define (filter-prices tree)
  (filter-map (lambda (item)
		(if (eq? (class-of item) <inventory-types>)
		    (slot-value item 'typeID)
		    #f)) (flatten tree)))

(define (get-production-prices item-name)
  (let ([dependencies (get-item-dependencies item-name)])
    (if dependencies
	(let ([prices (retrieve-prices dependencies)])
	  (bill-of-materials-tree dependencies prices)))))

(define (map-tree function node)
    (if (list? node)
      (map (lambda (element)
	     (map-tree function element)) node)
      (function node)))

(define (bill-of-materials-tree item-tree prices)
  (fold (lambda (item)
	  (find (lambda (el)
		  (= (slot-value el 'typeid) (slot-value item 'typeID))) prices)) item-tree))
  ;; (map-tree (lambda (it)
  ;; 	      (find (lambda (el)
  ;; 		      (= (slot-value el 'typeid) (slot-value it 'typeID))) prices)) item-tree))

