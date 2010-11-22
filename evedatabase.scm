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

(define-model <inventory-types> "invtypes"
  (typeID groupID typeName description iconID radius mass volume capacity portionSize raceID basePrice published marketGroupID chanceOfDuplicating))

(belongs-to <inventory-types> <inventory-groups> group foreign-key: groupID)
(has-many <inventory-types> <inventory-type-reactions> reactions foreign-key: typeID)

(define-model <planet-schematics> "planetSchematics"
  (schematicID schematicName cycleTime))

(has-many <planet-schematics> <planet-schematics-typemap> typemaps foreign-key: schematicID)

(define-model <planet-schematics-typemap> "planetschematicstypemap"
  (schematicID typeID quantity isInput))

(belongs-to <planet-schematics-typemap> <planet-schematics> schematic foreign-key: schematicID)
(belongs-to <planet-schematics-typemap> <inventory-types> inventory foreign-key: typeID)

(define (get-planetary-category)
  (find-first-by-id <inventory-categories> 43))

(define (get-planetary-groups category-id)
  (find-by-id <inventory-groups> category-id))

(define (get-planetary-items)
  (let ([category (get-planetary-category)])
    (map (lambda (group)
	   (find-all <inventory-types> conditions: `(where (= 'groupID ,(slot-value group 'groupID))))) (get-planetary-groups (slot-value category 'categoryID)))))

(define (make-production-tree schematic #!optional acum)
  (let ([inputs (get-inputs schematic)])
    (if (null? inputs)
        (append schematic acum)
        (map
         (lambda (input)
               (make-production-tree (get-schematic (get-item input)) acum)) inputs))))
         
(define-method (get-inputs (schematic <planet-schematics>))
  (find-all <planet-schematics-typemap> conditions: `(where (and
							(= 'schematicID ,(slot-value schematic 'schematicID))
							(= 'isInput 1)))))

(define-method (get-output (schematic <planet-schematics>))
  (find-first <planet-schematics-typemap> conditions: `(where (and
							(= 'schematicID ,(slot-value schematic 'schematicID))
							(= 'isInput 0)))))

(define-method (get-item (schematic <planet-schematics>))
  (first <inventory-types> conditions: `(where (= 'typeID ,(slot-value schematic 'typeID)))))

(define-method (get-item (schematic-typemap <planet-schematics-typemap>))
  (first <inventory-types> conditions: `(where (= 'typeID ,(slot-value schematic-typemap 'typeID)))))

(define-method (get-schematic (item <inventory-types>))
  (let ([schematic-typemap
         (first <planet-schematics-typemap> conditions: `(where (and
							  (= 'typeID ,(slot-value item 'typeID))
							  (= 'isInput 0))))])
    (find-by-id <planet-schematics> (slot-value schematic-typemap 'schematicID))))

