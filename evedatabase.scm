(use sql-de-lite)
(use coops)
(use aql)

;; (define-class model () (id))

(define *database-url* #f)
(define *connection* #f)

(define-class <table> ()
  (name columns (relationships initform: '())))

(define-class <model> ()
  (table))

(define-class <invcategories> (<model>)
  (category-id category-name description graphic-id published))

(define-class <invgroups> (<model>)
  ())

(define-class <invtypereactions> (<model>)
  ())

(define-method (load-data (table )) )

;(make <table> name: "teste" columns: '(categoryID categoryName description graphicID published) relationships: '())

(define-syntax define-model
  (syntax-rules ()
    ([_ model-name database-table-name (table-columns ...)]
     (let ([table-definition (make <table> 'name 'database-table-name 'columns '(table-columns ...))])
       (define-class model-name (<model>)
	 (table-columns ... (table initform: table-definition) ))
       (define-method (list-columns ())
	 '(table-columns ...))))))

"select * from invcategories where categoryID in (41,42,43);"
[
    { "categoryID" : """41""", "categoryName" : """Planetary Interaction""", "description" : """Stuff for planetary interaction""", "graphicID" : """""", "published" : """0""" },
    { "categoryID" : """42""", "categoryName" : """Planetary Resources""", "description" : """These are Items that can be extracted from a planet. """, "graphicID" : """""", "published" : """1""" },
    { "categoryID" : """43""", "categoryName" : """Planetary Commodities""", "description" : """""", "graphicID" : """""", "published" : """1""" },
]

;; Retrieve a list of planetary interaction items
"select * from invgroups as i join invcategories as j on i.categoryID = j.categoryID 
						join invtypes as t on i.groupID = t.groupID
									 and i.categoryID = 43"

;; Only the most interesting rows
"select typeID, groupName, typeName  from invgroups as i join invcategories as j on i.categoryID = j.categoryID 
						join invtypes as t on i.groupID = t.groupID
									 and i.categoryID = 43"

;; Getting the typeIDs above allows us to check how they are reacted together
"select * from invtypereactions limit 1"
[
    { "reactionTypeID" : """16868""", "input" : """0""", "typeID" : """3645""", "quantity" : """95""" },
]

;; This will be enough for PI. For ship manufacturing, understanding of blueprints is required.