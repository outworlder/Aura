(use aql)
(use sql-de-lite)
(use coops)

(include "orly")

(define-model <inventory-categories> "invcategories"
  (categoryID categoryName description iconID published))

(has-many <inventory-categories> <inventory-groups> groups foreign-key: categoryID)

(define-model <inventory-type-reactions> "invtypereactions"
  (reactionTypeID input typeID quantity))

(belongs-to <inventory-type-reactions> <inventory-types> inventory foreign-key: typeID)

(define-model <inventory-groups> "invgroups"
  (groupID categoryID groupName description iconID useBasePrice allowManufacture allowRecycler anchored anchorable fittableNonSingleton published))

(belongs-to <inventory-groups> <inventory-categories> category foreign-key: categoryID)
(has-many <inventory-groups> <inventory-types> inventories foreign-key: groupID)

(define-model <inventory-types> "invtypes"
  (typeID groupID typeName description iconID radius mass volume capacity portionSize raceID basePrice published marketGroupID chanceOfDuplicating))

(belongs-to <inventory-types> <inventory-groups> group foreign-key: groupID)
(has-many <inventory-types> <inventory-type-reactions> reactions foreign-key: typeID)
(has-many <inventory-types> <planet-schematics-typemap> typemaps foreign-key: typeID)

(define-model <planet-schematics> "planetSchematics"
  (schematicID schematicName cycleTime))

(has-many <planet-schematics> <planet-schematics-typemap> typemaps foreign-key: schematicID)

(define-model <planet-schematics-typemap> "planetschematicstypemap"
  (schematicID typeID quantity isInput))

(belongs-to <planet-schematics-typemap> <planet-schematics> schematic foreign-key: schematicID)
(belongs-to <planet-schematics-typemap> <inventory-types> inventory foreign-key: typeID primary-key: typeID)

(define (get-planetary-category)
  (find-by-id <inventory-categories> 43))

(define (get-planetary-groups category-id)
  (find-by-id <inventory-groups> category-id))

(define (get-planetary-items)
  (let ([category (get-planetary-category)])
    (map (lambda (group)
	   (inventories group)) (groups category))))

(define (make-production-tree item)
  (let ([schematic (get-schematic item)])
    (if (null? schematic)
	item
	(let ([inputs (get-inputs schematic)])
	  (if (null? inputs)
	      schematic
	      (list item (map
			  (lambda (input)
			    (make-production-tree (get-item input))) inputs)))))))


(define-method (get-inputs (schematic <planet-schematics>))
  (filter (lambda (item)
	    (= (slot-value item 'isInput) 1)) (typemaps schematic)))

(define-method (get-output (schematic <planet-schematics>))
  (car (filter (lambda (item)
		 (= (slot-value item 'isInput) 0))
	       (typemaps schematic))))

(define-method (get-item (schematic <planet-schematics>))
  (inventory (get-output schematic)))

(define-method (get-item (schematic-typemap <planet-schematics-typemap>))
  (inventory schematic-typemap))

(define-method (get-schematic (item <inventory-types>))
  (let ([schematic-typemap
	 (filter (lambda (item)
		   (equal? (slot-value item 'isInput) 0)) (typemaps item))])
    (if (null? schematic-typemap)
	schematic-typemap
	(schematic (car schematic-typemap)))))

