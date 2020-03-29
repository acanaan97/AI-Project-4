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

(defrule price ""
    ?ins <- (object (is-a PRICEPOINT) )
=> 
    (send ?ins put-price_type (ask-question "Do you want the activity to be free, cheap, or expensive"  free cheap expensive )) )

(defrule location ""
    ?ins <- (object (is-a DEST) )
=> 
    (send ?ins put-geography (ask-question "Do you want the activity to be on land or water?"  land water )) )

(defrule land ""
    ?ins <- (object (is-a DEST) (geography land))
=> 
    (send ?ins put-temp (ask-question "Do you want the activity to be warm or cold?"  warm cold )) )

(defrule water ""
    ?ins <- (object (is-a DEST) (geography water))
=> 
    (send ?ins put-temp (ask-question "Do you want the activity to be warm or cold?"  warm cold )) )

;rules
(defrule is_hiking (declare (salience -50))
    ?ins <- (object (is-a DEST) (temp warm) (geography land))
    ?acs <- (object (is-a PRICEPOINT) (price_type free))
=> 
    (send ?ins put-activity hiking) )

(defrule is_museum (declare (salience -50))
    ?ins <- (object (is-a DEST) (temp warm) (geography land))
    ?acs <- (object (is-a PRICEPOINT) (price_type cheap))
=> 
    (send ?ins put-activity Museum) )

(defrule is_wine-tasting (declare (salience -50))
    ?ins <- (object (is-a DEST) (temp warm) (geography land))
    ?acs <- (object (is-a PRICEPOINT) (price_type expensive))
=> 
    (send ?ins put-activity wine-tasting) )

(defrule is_ice-skating (declare (salience -50))
    ?ins <- (object (is-a DEST) (temp cold) (geography land))
    ?acs <- (object (is-a PRICEPOINT) (price_type free))
=> 
    (send ?ins put-activity ice-skating) )

(defrule is_hockey-game (declare (salience -50))
    ?ins <- (object (is-a DEST) (temp cold) (geography land))
    ?acs <- (object (is-a PRICEPOINT) (price_type cheap))
=> 
    (send ?ins put-activity hockey-game) )

(defrule is_snow-resort (declare (salience -50))
    ?ins <- (object (is-a DEST) (temp cold) (geography land))
    ?acs <- (object (is-a PRICEPOINT) (price_type expensive))
=> 
    (send ?ins put-activity snow-resort) )

(defrule is_waterpark (declare (salience -50))
    ?ins <- (object (is-a DEST) (temp warm) (geography water))
    ?acs <- (object (is-a PRICEPOINT) (price_type expensive))
=> 
    (send ?ins put-activity waterpark) )

(defrule is_white-water-rafting (declare (salience -50))
    ?ins <- (object (is-a DEST) (temp cold) (geography water))
    ?acs <- (object (is-a PRICEPOINT) (price_type expensive))
=> 
    (send ?ins put-activity white-water-rafting) )

(defrule is_lap-swimming
    ?ins <- (object (is-a DEST) (temp warm) (geography water))
    ?acs <- (object (is-a PRICEPOINT) (price_type free))
=> 
    (send ?ins put-activity lap-swimming) )

(defrule is_surfing (declare (salience -50))
    ?ins <- (object (is-a DEST) (temp cold) (geography water))
    ?acs <- (object (is-a PRICEPOINT) (price_type cheap))
=> 
    (send ?ins put-activity surfing) )


(defrule decision (declare (salience -100))
    (object (is-a DEST) (temp ?t) (geography ?g) (activity ?a))
    (object (is-a PRICEPOINT) (price_type ?pt))
=>
    (printout "" ?a " would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)
