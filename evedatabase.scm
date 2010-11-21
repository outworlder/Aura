(include "orly")

(define-model <inventory-categories> "invcategories"
  (categoryID categoryName description iconID published))

(define-model <inventory-type-reactions> "invtypereactions"
  (reactionTypeID input typeID quantity))

(define-model <inventory-groups> "invgroups"
  (groupID categoryID groupName description iconID useBasePrice allowManufacture allowRecycler anchored anchorable fittableNonSingleton published))

(define-model <inventory-types> "invtypes"
  (typeID groupID typeName description iconID radius mass volume capacity portionSize raceID basePrice published marketGroupID chanceOfDuplicating))

(define-model <planet-schematics> "planetSchematics"
  (schematicID schematicName cycleTime))

(define-model <planet-schematics-typemap> "planetschematicstypemap"
  (schematicID typeID quantity isInput))

(define (get-planetary-category)
  (car (all <inventory-categories> conditions: '(where (= 'categoryID 43)))))

(define (get-planetary-groups category-id)
  (find-by-id <inventory-groups> category-id))

(define (get-planetary-items)
  (let ([category (get-planetary-category)])
    (map (lambda (group)
	   (all <inventory-types> conditions: `(where (= 'groupID ,(slot-value group 'groupID))))) (get-planetary-groups (slot-value category 'categoryID)))))

(define (make-production-tree schematic #!optional acum)
  (let ([inputs (get-inputs schematic)])
    (if (null? inputs)
        (append schematic acum)
        (map
         (lambda (input)
               (make-production-tree (get-schematic (get-item input)) acum)) inputs))))
         
(define-method (get-inputs (schematic <planet-schematics>))
  (all <planet-schematics-typemap> conditions: `(where (and
							(= 'schematicID ,(slot-value schematic 'schematicID))
							(= 'isInput 1)))))

(define-method (get-output (schematic <planet-schematics>))
  (first <planet-schematics-typemap> conditions: `(where (and
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

