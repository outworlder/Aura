(include "evecentral")

(use test)

(define marketstat-response
  '(*TOP* (*PI* xml "version=\"1.0\" encoding=\"utf-8\" ")
       (evec_api
         (@ (version "2.0") (method "marketstat_xml"))
         (marketstat
           (type (@ (id "34"))
                 (all (volume "250774577172.00")
                      (avg "9.24")
                      (max "19999.00")
                      (min "0.20")
                      (stddev "314.07")
                      (median "2.23"))
                 (buy (volume "139962279865.00")
                      (avg "1.80")
                      (max "3.00")
                      (min "0.20")
                      (stddev "0.45")
                      (median "1.84"))
                 (sell (volume "110812297307.00")
                       (avg "2.57")
                       (max "19999.00")
                       (min "1.39")
                       (stddev "365.31")
                       (median "2.30")))))))

(test "Deve criar market items a partir do sxml de entrada"
      (make-market-stat )
