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

;; Put those in 
(define (retrieve-prices tree)
  (parse-marketstat (marketstat (map (lambda (item)
				       (slot-value item 'typeID)) (flatten tree)))))


(define (annotate-tree item-tree prices)
  (if (list? item-tree)
      (map (lambda (item)
	     (annotate-tree item prices)) item-tree)
      (list item-tree (find (lambda (it)
			      (= (slot-value it 'typeid) (slot-value item-tree 'typeID))) prices))))

(define (get-production-prices item-name)
  (let ([dependencies (get-item-dependencies item-name)])
    (if dependencies
	(let ([prices (retrieve-prices dependencies)])
	  (annotate-tree dependencies prices)))))