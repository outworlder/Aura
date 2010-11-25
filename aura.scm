(include "evecentral")
(include "evedatabase")

(define (get-item-dependencies item-name)
  (let ([item (find-all-by-string <inventory-types> 'typeName item-name)])
    (if (null? item)
	(begin
	  (print "Item \"" item-name "\" not found.")
	  #f)
	(map (lambda (match)
	       (make-production-tree match)) item))))

(define (retrieve-prices tree)
  (parse-marketstat (marketstat (map (lambda (item)
				       (slot-value item 'typeID)) (flatten tree)))))



(define (get-production-prices item-name)
  (let ([dependencies (get-item-dependencies item-name)])
    (if dependencies
	(let ([prices (retrieve-prices dependencies)])
	  (annotate-tree dependencies prices)))))

(define (map-tree function node)
    (if (list? node)
      (map (lambda (element)
	     (map-tree function element)) node)
      (function node)))

(define (annotate-tree item-tree prices)
  (map-tree (lambda (it)
	      (find (lambda (el)
		      (= (slot-value el 'typeid) (slot-value it 'typeID))) prices)) item-tree))

