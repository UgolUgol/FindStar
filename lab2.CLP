(deftemplate artist 
	(slot fn)
	(slot sn)
	(slot junre)
	(slot sex)
	(slot age)
	(slot country)
	(slot height)
	(slot weight)
	(slot alive)
)

(deffunction ask-question (?question $?allowed-values)
	(printout t ?question)
	(bind ?answer (read))
	(if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
	(while (not(member ?answer ?allowed-values)) do
	(printout t ?question)
	(bind ?answer (read))
	(if (lexemep ?answer)
		then (bind ?answer(lowcase ?answer))))
	?answer
)


(deffunction yes-no (?question)
(bind ?response (ask-question ?question yes no y n))
(if (or (eq ?response yes) (eq ?response y))
then TRUE 
else FALSE)) 

(defrule gender
=>
(if (yes-no "Are you personage male ?")
then (assert (sex male))
else (assert (sex woman))
)
)

(defrule alive
=>
(if (yes-no "Are you personage alive ?")
then (assert (alive yes))
else (assert (alive no))
)
)

(defrule not_alive 
(alive ?stat) (sex ?gen)
(test (and (= (str-compare ?stat no) 0) (= (str-compare ?gen male) 0) ))
=>
(if (yes-no "You personage was known as Russian rock start ? ")
then (assert (junre rock))
else 
(if (yes-no "Maybe shanson ?") 
then (assert (junre shanson))
else (assert (junre unknown))
)
)
)

(defrule jr_alive
(alive ?stat1)
(test (= (str-compare ?stat1 yes) 0))
=>
(if (yes-no "Are you personage rap artist ?")
then (assert (junre rap))
else 
(if (yes-no "Maybe it is pop artist ? ")
then (assert (junre pop))
else 
(if (yes-no "Oh, it is shanson singer ? ")
then (assert (junre shanson))
else (assert (junre unknown))
)
)
)
)

(defrule rapper_info1
(junre ?stat1) (alive ?stat2)
(test (= (str-compare ?stat1 rap) 0) )
(test (= (str-compare ?stat2 yes) 0) )
=>
(if (yes-no "This personage take part in Versus battle?")
then (assert (info1 Versus))
else 
(if (yes-no "The best friend of your personage is Vladimir Putin ?")
then (assert (info1 Putin))
else
(if (yes-no "Maybe this personage take part in Voice show ?")
then (assert (info1 Voice))
else 
(if (yes-no "This personage is crazy ?")
then (assert (info1 crazy))
else (assert (info1 unknown))
))))
)


(defrule rap_age
(junre ?stat1) (alive ?stat2)
(test (= (str-compare ?stat1 rap) 0) )
(test (= (str-compare ?stat2 yes) 0) )
=>
(if (yes-no "This personage is older than 45 ? ")
then (assert (age >45))
else 
(if (yes-no "This personage is approximatly 40?")
then (assert (age >37))
else (assert (age <37))
))
)

(defrule born_rapper
(junre ?stat1) (alive ?stat2)
(test (= (str-compare ?stat1 rap) 0) )
(test (= (str-compare ?stat2 yes) 0) )
=>
(if (yes-no "This personage now living in London ? ")
then (assert (country England))
else 
(if (yes-no "Maybe he was born in Ukraine ?")
then (assert (country Ukraine))
else (assert (country Russia))
))
)

(defrule pop_info1
(junre ?stat1) (alive ?stat2)
(test (= (str-compare ?stat1 pop) 0) )
(test (= (str-compare ?stat2 yes) 0) )
=>
(if (yes-no "This artist like colorful suits?")
then (assert (info1 colorful_suits))
else
(if (yes-no "Maybe this personage is known as Russia Madonna ?")
then (assert (info1 madonna))
else
(if (yes-no "This personage is look like he made a lot of plastic surgery ?")
then (assert (info1 plastic_surgery))
else
(if (yes-no "This personage sing song about barrel organ ?")
then (assert (info1 barrel))
else 
(if (yes-no "This personage appears under pseudonym?")
then (assert (info1 pseudonym))
else
(if (yes-no "The husband of this personage is producer ?")
then (assert (info1 hus_producer))
else (assert (info1 unknown))
))))))
)


(defrule pop_info2
(junre ?stat1) (alive ?stat2) (info1 ?stat3)
(test (= (str-compare ?stat1 pop) 0) )
(test (= (str-compare ?stat2 yes) 0) )
(test (<> (str-compare ?stat3 unknown) 0) )
=>
(if (and (= (str-compare ?stat3 colorful_suits) 0) (yes-no "This personage had relations with Alla Pugacheva ?") )
then (assert (info2 relations_Pugacheva))
else
(if (and (= (str-compare ?stat3 plastic_surgery) 0)  (yes-no "His suits can concurate with Filip Kirkorov suits ?") )
then (assert (info2 concurate_suits))
else 
(if (and (= (str-compare ?stat3 barrel) 0) (yes-no "This personage is natural blond ?" ) ) 
then (assert (info2 blond))
else (assert (info2 unknown))
)))
)


(defrule pop_age
(junre ?stat1) (alive ?stat2)
(test (= (str-compare ?stat1 pop) 0) )
(test (= (str-compare ?stat2 yes) 0) )
=>
(if (yes-no "This personage not so long ago celebrated his fiftieth anniversary ?")
then (assert (age 50))
else 
(if (yes-no "This personage is younger than 40 ? ")
then (assert (age <40))
else 
(if (yes-no "This personage is younger than 50 ? ")
then (assert (age <50))
else (assert (age >50))
)))
)

(defrule pop_country
(junre ?stat1) (alive ?stat2)
(test (= (str-compare ?stat1 pop) 0) )
(test (= (str-compare ?stat2 yes) 0) )
=>
(if (yes-no "This personage from Ukraine ?")
then (assert (country Ukraine))
else 
(if (yes-no "This personage from Bolgaria ?")
then (assert (country Bolgaria))
else (assert (country Russia))
)
)
)

(defrule kirkorov 
(junre ?stat1) (alive ?stat2) (info1 ?stat3) (info2 ?stat4) (country ?stat5) (age ?stat6) (sex ?stat7)
?art <- (artist (sn Kirkorov))
(test (= (str-compare ?stat1 pop) 0) )
(test (= (str-compare ?stat2 yes) 0) )
(test (= (str-compare ?stat7 male) 0) )
(test (or (and (= (str-compare ?stat3 colorful_suits) 0)    (= (str-compare ?stat4 relations_Pugacheva) 0))
	  (and (= (str-compare ?stat3 colorful_suits) 0)    (= (str-compare ?stat5 (fact-slot-value ?art country)) 0))
          (and (= (str-compare ?stat3 colorful_suits) 0)    (= ?stat6 (fact-slot-value ?art age)))  ))
=>
(printout t (fact-slot-value ?art fn) " " (fact-slot-value ?art sn) )
)


(defrule leontiev
(junre ?stat1) (alive ?stat2) (info1 ?stat3) (info2 ?stat4) (country ?stat5) (age ?stat6) (sex ?stat7)
?art <- (artist (sn Leontiev))
(test (= (str-compare ?stat1 pop) 0) )
(test (= (str-compare ?stat2 yes) 0) )
(test (= (str-compare ?stat7 male) 0) )
(test (or (and (= (str-compare ?stat3 plastic_surgery) 0)    (= (str-compare ?stat4 concurate_suits) 0))
	  (and (= (str-compare ?stat3 plastic_surgery) 0)    (= (str-compare ?stat5 (fact-slot-value ?art country)) 0))
          (and (= (str-compare ?stat3 plastic_surgery) 0)    (= (str-compare ?stat6 >50) 0) )  ))
=>
(printout t (fact-slot-value ?art fn) " " (fact-slot-value ?art sn) )
)

(defrule baskov
(junre ?stat1) (alive ?stat2) (info1 ?stat3) (info2 ?stat4) (country ?stat5) (age ?stat6) (sex ?stat7)
?art <- (artist (sn Baskov))
(test (= (str-compare ?stat1 pop) 0) )
(test (= (str-compare ?stat2 yes) 0) )
(test (= (str-compare ?stat7 male) 0) )
(test (or (and (= (str-compare ?stat3 barrel) 0)    (= (str-compare ?stat4 blond) 0))
	  (and (= (str-compare ?stat3 barrel) 0)    (= (str-compare ?stat5 (fact-slot-value ?art country)) 0))
          (and (= (str-compare ?stat3 barrel) 0)    (= (str-compare ?stat6 <50) 0) )  ))
=>
(printout t (fact-slot-value ?art fn) " " (fact-slot-value ?art sn) )
)


(defrule oxxxymiron
(junre ?stat1) (alive ?stat2) (info1 ?stat3) (age ?stat4) (country ?stat5) (sex ?stat6)
?art <- (artist (sn Fedorov))
(test (= (str-compare ?stat6 (fact-slot-value ?art sex)) 0) ) 
(test (= (str-compare ?stat2 (fact-slot-value ?art alive)) 0))
(test (= (str-compare ?stat1 (fact-slot-value ?art junre)) 0))
(test (or (= (str-compare ?stat3 Versus) 0) 
(and (= (str-compare ?stat4 <37) 0) (= (str-compare ?stat5 (fact-slot-value ?art country) ) 0) ) ) )
=>
(printout t (fact-slot-value ?art fn) " " (fact-slot-value ?art sn) )
)

(defrule basta
(junre ?stat1) (alive ?stat2) (info1 ?stat3) (age ?stat4) (country ?stat5) (sex ?stat6)
?art <- (artist (sn Vakulenko))
(test (= (str-compare ?stat6 (fact-slot-value ?art sex)) 0) )
(test (= (str-compare ?stat2 (fact-slot-value ?art alive)) 0))
(test (= (str-compare ?stat1 (fact-slot-value ?art junre)) 0))
(test (or (= (str-compare ?stat3 Voice) 0) 
(and (= (str-compare ?stat4 >37) 0) (= (str-compare ?stat5 (fact-slot-value ?art country) ) 0) ) ) )
=>
(printout t (fact-slot-value ?art fn) " " (fact-slot-value ?art sn) )
)


(defrule timati
(junre ?stat1) (alive ?stat2) (info1 ?stat3) (age ?stat4) (country ?stat5) (sex ?stat6)
?art <- (artist (sn Unysov))
(test (= (str-compare ?stat6 (fact-slot-value ?art sex)) 0) )
(test (= (str-compare ?stat2 (fact-slot-value ?art alive)) 0))
(test (= (str-compare ?stat1 (fact-slot-value ?art junre)) 0))
(test (or (= (str-compare ?stat3 Putin) 0) 
(and (= (str-compare ?stat4 <37) 0) (= (str-compare ?stat5 (fact-slot-value ?art country) ) 0) ) ) )
=>
(printout t (fact-slot-value ?art fn) " " (fact-slot-value ?art sn) )
)

(defrule titomir
(junre ?stat1) (alive ?stat2) (info1 ?stat3) (age ?stat4) (country ?stat5) (sex ?stat6)
?art <- (artist (sn Titomir))
(test (= (str-compare ?stat6 (fact-slot-value ?art sex)) 0) )
(test (= (str-compare ?stat2 (fact-slot-value ?art alive)) 0))
(test (= (str-compare ?stat1 (fact-slot-value ?art junre)) 0))
(test (or (= (str-compare ?stat3 crazy) 0) 
(and (= (str-compare ?stat4 >45) 0) (= (str-compare ?stat5 (fact-slot-value ?art country) ) 0) ) ) )
=>
(printout t (fact-slot-value ?art fn) " " (fact-slot-value ?art sn) )
)

(defrule csoy 
(alive ?stat1) (sex ?stat2) (junre ?stat3)
?art <- (artist (sn Csoy))
(test (= (str-compare ?stat1 (fact-slot-value ?art alive)) 0) )
(test (= (str-compare ?stat2 (fact-slot-value ?art sex)) 0) )
(test (= (str-compare ?stat3 (fact-slot-value ?art junre)) 0) )
=>
(printout t (fact-slot-value ?art fn) " " (fact-slot-value ?art sn) )
)


(defrule Krug 
(alive ?stat1) (sex ?stat2) (junre ?stat3)
?art <- (artist (sn Krug))
(test (= (str-compare ?stat1 (fact-slot-value ?art alive)) 0) )
(test (= (str-compare ?stat2 (fact-slot-value ?art sex)) 0) )
(test (= (str-compare ?stat3 (fact-slot-value ?art junre)) 0) )
=>
(printout t (fact-slot-value ?art fn) " " (fact-slot-value ?art sn) )
)


		