; Classes

(defclass DEST (is-a USER)
    (slot temp)
    (slot geography)
    (slot activity)
)

(defclass PRICEPOINT (is-a USER)
    (slot price_type)
)

// OBJECT DEFINITIONS
(definstances DEST-INSTANCES
    (firstdest of DEST (geography land) (temp warm))
    (seconddest of DEST (geography land) (temp warm))
    (thirddest of DEST (geography land) (temp cold))
    (fourthdest of DEST (geography water) (temp cold))
    (fifthdest of DEST (geography water) (temp warm))
    (sixthdest of DEST (geography water) (temp warm))
)

(definstances PRICEPOINT-INSTANCES
    stay-at-home of PRICEPOINT
    price_type free
)

// FUNCTION FOR QUESTIONS
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

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))

// QUERY RULES
(defrule price (declare (salience 10000))
    ?ins <- (object (is-a PricePoint) )
=> 
    (send ?ins put-price-type (ask-question "Do you want the activity to be free, cheap, or expensive"  free cheap expensive )) )

(defrule location (declare (salience 10000))
    ?ins <- (object (is-a DEST) )
=> 
    (send ?ins put-geography (ask-question "Do you want the activity to be on land or water?"  land water )) )

(defrule temperature (declare (salience 10000))
    ?ins <- (object (is-a DEST) )
=> 
    (send ?ins put-temp (ask-question "Do you want the activity to be warm or cold?"  warm cold )) )

;rules
(defrule hiking (declare (salience -50))
    (object (is-a DEST) (temp ?t) (geography ?g))
    (object (is-a PRICE) (price-type ?pt))
=>
    (printout "Hiking would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)

(defrule museum (declare (salience -50))
    (object (is-a DEST) (temp ?t) (geography ?g))
    (object (is-a PRICE) (price-type ?pt))
=>
    (printout "Visiting a museum would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)

(defrule wine-tasting (declare (salience -50))
    (object (is-a DEST) (temp ?t) (geography ?g))
    (object (is-a PRICE) (price-type ?pt))
=>
    (printout "Wine tasting would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)

(defrule ice-skating (declare (salience -50))
    (object (is-a DEST) (temp ?t) (geography ?g))
    (object (is-a PRICE) (price-type ?pt))
=>
    (printout "Ice Skating would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)

(defrule hockey-game (declare (salience -50))
    (object (is-a DEST) (temp ?t) (geography ?g))
    (object (is-a PRICE) (price-type ?pt))
=>
    (printout "Going to see a hockey game would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)

(defrule snowresort (declare (salience -50))
    (object (is-a DEST) (temp ?t) (geography ?g))
    (object (is-a PRICE) (price-type ?pt))
=>
    (printout "A snow resort would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)

(defrule waterpark (declare (salience -50))
    (object (is-a DEST) (temp ?t) (geography ?g))
    (object (is-a PRICE) (price-type ?pt))
=>
    (printout "A water park would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)

(defrule rafting (declare (salience -50))
    (object (is-a DEST) (temp ?t) (geography ?g))
    (object (is-a PRICE) (price-type ?pt))
=>
    (printout "White water rafting would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)

(defrule lap-swimming (declare (salience -50))
    (object (is-a DEST) (temp ?t) (geography ?g))
    (object (is-a PRICE) (price-type ?pt))
=>
    (printout "Lap Swimming would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)

(defrule surfing (declare (salience -50))
	(object (is-a DEST) (temp ?t) (geography ?g))
	(object (is-a PRICE) (price-type ?pt))
=>
	(printout "Surfing would be a good activity since it's " ?pt " and it is " ?t " and involves " ?g crlf)
)