(include "orly")

(define-model <inventory-categories> "invcategories"
  (categoryID categoryName description graphicID published))

(define-model <inventory-type-reactions> "invtypereactions"
  (reactionTypeID input typeID quantity))

(define-model <inventory-groups> "invgroups"
  (groupID categoryID groupName description graphicID useBasePrice allowManufacture allowRecycler anchored anchorable fittableNonSingleton published))

(define-model <inventory-types> "invtypes"
  (typeID groupID typeName description graphicID radius mass volume capacity portionSize raceID basePrice published marketGroupID chanceOfDuplicating))

(define-model <planet-schematics> "planetSchematics"
  (schematicID schematicName cycleTime))

(define-model <planet-schematics-typemap> "planetschematicstypemap"
  (schematicID typeID quantity isInput))

(define (get-planetary-category)
  (car (all <inventory-categories> conditions: '(where (= 'categoryID 43)))))

(define (get-planetary-groups category-id)
  (all <inventory-groups> conditions: `(where (= 'categoryID ,category-id))))

(define (get-planetary-items)
  (let ([category (get-planetary-category)])
    (map (lambda (group)
	   (all <inventory-types> conditions: `(where (= 'groupID ,(slot-value group 'groupID))))) (get-planetary-groups (slot-value category 'categoryID)))))

;; Function below can be adapted to moon mining.
;; (define (get-production-tree item)
;;   (let ([reactions (all <inventory-type-reactions> conditions: `(where (= 'typeID ,(slot-value item 'typeID))))])
;;     (if (null? reactions)
;; 	item
;; 	(map (lambda (reaction-item)
;; 	       (get-production-tree reaction-item)) reactions))))

;; (define-method (get-production-tree (schematic <planet-schematics>))
;;   (let ([schematic-inputs (get-inputs schematic)])
;;     (if (null? schematic-inputs)
;; 	(get-output schematic))
    

(define-method (get-inputs (schematic <planet-schematics>))
  (all <planet-schematics-typemap> conditions: `(where (and
							(= 'schematicID ,(slot-value schematic 'schematicID))
							(= 'isInput 1)))))

(define-method (get-output (schematic <planet-schematics>))
  (first <planet-schematics-typemap> conditions: `(where (and
							(= 'schematicID ,(slot-value schematic 'schematicID))
							(= 'isInput 0)))))

(define-method (get-item (schematic <planet-schematics>))
  (first <inventory-types> conditions: `(where (= 'typeID ,(slot-value (get-output schematic) 'typeID)))))

(define-method (get-schematic (item <inventory-types>))
  (first <planet-schematics-typemap> conditions: `(where (and
							  (= 'typeID ,(slot-value item 'typeID))
							  (= 'isInput 0)))))

;; "select * from invcategories where categoryID in (41,42,43);"
;; [
;;     { "categoryID" : """41""", "categoryName" : """Planetary Interaction""", "description" : """Stuff for planetary interaction""", "graphicID" : """""", "published" : """0""" },
;;     { "categoryID" : """42""", "categoryName" : """Planetary Resources""", "description" : """These are Items that can be extracted from a planet. """, "graphicID" : """""", "published" : """1""" },
;;     { "categoryID" : """43""", "categoryName" : """Planetary Commodities""", "description" : """""", "graphicID" : """""", "published" : """1""" },
;; ]

;; Retrieve a list of planetary interaction items
"select * from invgroups as i join invcategories as j on i.categoryID = j.categoryID 
						join invtypes as t on i.groupID = t.groupID
									 and i.categoryID = 43"

;; [
;;     { "groupID" : """0""", "categoryID" : """0""", "groupName" : """#System""", "description" : """""", "graphicID" : """""", "useBasePrice" : """0""", "allowManufacture" : """1""", "allowRecycler" : """1""", "anchored" : """0""", "anchorable" : """0""", "fittableNonSingleton" : """0""", "published" : """0""" },
;; ]

;; Only the most interesting rows
"select typeID, groupName, typeName  from invgroups as i join invcategories as j on i.categoryID = j.categoryID 
						join invtypes as t on i.groupID = t.groupID
									 and i.categoryID = 43"

;; Getting the typeIDs above allows us to check how they are reacted together
"select * from invtypereactions limit 1"
;; [
;;     { "reactionTypeID" : """16868""", "input" : """0""", "typeID" : """3645""", "quantity" : """95""" },
;; ]

;; This will be enough for PI. For ship manufacturing, understanding of blueprints is required.
