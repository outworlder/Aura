(use coops)

(define-class <bill-of-materials> ()
  (item amount prices))

(define-method (print-object (obj <bill-of-materials>) #!optional (port (current-output-port)))
  (fprintf port "<#bill-of-materials item:[~A] amount:[~A] prices:[~A]>"
           (slot-value obj 'item)
           (slot-value obj 'amount)
           (slot-value obj 'prices)))

(define (make-production-tree item #!key (amount 1))
  (let ([schematic (get-schematic item)])
    (if (null? schematic)
        (make <bill-of-materials> 'item item 'amount amount)
	(let ([inputs (get-inputs schematic)])
          (list (make <bill-of-materials> 'item item 'amount amount)
                (map (lambda (input)
                       (make-production-tree (get-item input) amount: (slot-value input 'quantity))) inputs))))))

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
  (map (lambda (item)
         (slot-value  (slot-value item 'item) 'typeID)) (flatten tree)))

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
  ;; (fold (lambda (item)
  ;;         (find (lambda (el)
  ;;       	  (= (slot-value el 'typeid) (slot-value (slot-value item 'typeID) item))) prices)) '() item-tree))
  (map-tree (lambda (it)
              (set! (slot-value it 'prices)
                    (find (lambda (el)
                            (= (slot-value el 'typeid) (slot-value (slot-value it 'item) 'typeID))) prices))
              it) item-tree))

;; (define (find-production-costs tree)
;;   (map-tree (lambda (item)
;;               )))
