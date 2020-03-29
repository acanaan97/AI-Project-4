; Classes

(defclass DEST (is-a USER)
    (slot temp)
    (slot geography)
    (slot activity)
)

(defclass PRICEPOINT (is-a USER)
    (slot price_type)
)

; Default instances
(definstances DEST-INSTANCES
    (firstdest of DEST (geography land) (temp warm))
    (seconddest of DEST (geography land) (temp warm))
    (thirddest of DEST (geography land) (temp cold))
    (fourthdest of DEST (geography water) (temp cold))
    (fifthdest of DEST (geography water) (temp warm))
    (sixthdest of DEST (geography water) (temp warm))
)

(definstances PRICEPOINT-INSTANCES
    (stay-at-home of PRICEPOINT
    (price_type free))
)

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)


;;;***************
;;;* QUERY RULES *
;;;***************

(defrule price 
    ?ins <- (object (is-a PRICEPOINT) )
=> 
    (send ?ins put-price-type (ask-question "Do you want the activity to be free, cheap, or expensive"  free cheap expensive )) )

(defrule location
    ?ins <- (object (is-a DEST) )
=> 
    (send ?ins put-geography (ask-question "Do you want the activity to be on land or water?"  land water )) )

(defrule temperature
    ?ins <- (object (is-a DEST) )
=> 
    (send ?ins put-temp (ask-question "Do you want the activity to be warm or cold?"  warm cold )) )

;;;***************
;;;* RULES *
;;;***************
(defrule is_hiking (declare (salience -50))
    ?ins <- (object (is-a DEST) (temp warm) (geography land))
    ?ins <- (object (is-a PRIECEPOINT) (price_type free))
=> 
    (send ?ins (object (is-a DEST) put-activity hiking) ))

(defrule is_museum 
    ?ins <- (object (is-a DEST) (temp warm) (geography land))
    ?ins <- (object (is-a PRIECEPOINT) (price_type cheap))
=> 
    (send ?ins (object (is-a DEST) put-activity Museum) ))

(defrule is_wine-tasting 
    ?ins <- (object (is-a DEST) (temp warm) (geography land))
    ?ins <- (object (is-a PRIECEPOINT) (price_type expensive))
=> 
    (send ?ins (object (is-a DES) put-activity wine-tasting) ))

(defrule is_ice-skating 
    ?ins <- (object (is-a DEST) (temp cold) (geography land))
    ?ins <- (object (is-a PRIECEPOINT) (price_type free))
=> 
    (send ?ins (object (is-a DES) put-activity ice-skating) ))

(defrule is_hockey-game 
    ?ins <- (object (is-a DEST) (temp cold) (geography land))
    ?ins <- (object (is-a PRIECEPOINT) (price_type cheap))
=> 
    (send ?ins (object (is-a DES) put-activity hockey-game) ))

(defrule is_snow-resort 
    ?ins <- (object (is-a DEST) (temp cold) (geography land))
    ?ins <- (object (is-a PRIECEPOINT) (price_type expensive))
=> 
    (send ?ins (object (is-a DES) put-activity snow-resort) ))

(defrule is_waterpark 
    ?ins <- (object (is-a DEST) (temp warm) (geography water))
    ?ins <- (object (is-a PRIECEPOINT) (price_type expensive))
=> 
    (send ?ins (object (is-a DES) put-activity waterpark) ))

(defrule is_white-water-rafting 
    ?ins <- (object (is-a DEST) (temp cold) (geography water))
    ?ins <- (object (is-a PRIECEPOINT) (price_type expensive))
=> 
    (send ?ins (object (is-a DES) put-activity white-water-rafting) ))

(defrule is_lap-swimming 
    ?ins <- (object (is-a DEST) (temp warm) (geography water))
    ?ins <- (object (is-a PRIECEPOINT) (price_type free))
=> 
    (send ?ins (object (is-a DES) put-activity lap-swimming) ))

(defrule is_surfing 
    ?ins <- (object (is-a DEST) (temp cold) (geography water))
    ?ins <- (object (is-a PRIECEPOINT) (price_type cheap))
=> 
    (send ?ins (object (is-a DES) put-activity surfing) ))

(defrule decision 
    (object (is-a DEST) (temp ?t) (geography ?g) (activity ?a))
    (object (is-a PRICE) (price-type ?pt))
=>
    (printout "" ?a " would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)
