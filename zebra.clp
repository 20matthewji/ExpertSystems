;;;======================================================
;;;   Who Drinks Water? And Who owns the Zebra?
;;;     
;;;     Another puzzle problem in which there are five 
;;;     houses, each of a different color, inhabited by 
;;;     men of different nationalities, with different 
;;;     pets, drinks, and cigarettes. Given the initial
;;;     set of conditions, it must be determined which
;;;     attributes are assigned to each man.
;;;
;;;     Jess version 4.1 example
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

;(set-node-index-hash 13) ; Defines the amount of memory that will be used in this program. Larger values use more memory and have better performance.
    
/*
* Defines the avh template that contains three slots: a, v, and h.
* a represents the attribute of the fact. The attributes used in this program include nationality, color, pet, drink, and smokes.
* v represents the version (type) of the attribute. For example, Englishman is a type of nationality and red is a type of color.
* h represents the house and must be an integer. 
*/
(deftemplate avh  (slot a)  (slot v)  (slot h (type INTEGER)))

/*
* The find-solution rule uses pattern matching to determine the attributes of the inhabitants in each of the five houses. It translates the printed clues into patterns that can be
* used to determine which attributes correspond to which house.
*
* The houses are labeled from the left to the right: Left [1 2 3 4 5] Right
*/
(defrule find-solution
  ; The Englishman lives in the red house.

  (avh (a nationality) (v englishman) (h ?n1)) 
  (avh (a color) (v red) (h ?c1 & ?n1))                                                            ; The red house number is equal to the Englishman's house number.

  ; The Spaniard owns the dog.

  (avh (a nationality) (v spaniard) (h ?n2 & ~?n1))                                                ; The Spaniard's house number is not equal to the Englishman's house number.
  (avh (a pet) (v dog) (h ?p1 & ?n2))                                                              ; The dog's house number is equal to the Spaniard's house number.

  ; The ivory house is immediately to the left of the green house,
  ; where the coffee drinker lives.

  (avh (a color) (v ivory) (h ?c2&~?c1))                                                           ; The ivory house number is not equal to the red house number.
  (avh (a color) (v green) (h ?c3&~?c2&~?c1&:(= (+ ?c2 1) ?c3)))                                   ; The green house number is not equal to the ivory and red house numbers and is
                                                                                                   ; equal to one more than the ivory house number. (The colon precedes a function
                                                                                                   ; call; in this case, the function call is =)
  (avh (a drink) (v coffee) (h ?d1&?c3))                                                           ; The coffee house number is equal to the green house number.

  ; The milk drinker lives in the middle house.

  (avh (a drink) (v milk) (h ?d2&~?d1&3))                                                          ; The milk house number is not equal to the coffee house number and is house 3.

  ; The man who smokes Old Golds also keeps snails.

  (avh (a smokes) (v old-golds) (h ?s1))
  (avh (a pet) (v snails) (h ?p2&~?p1&?s1))                                                        ; The snails' house number is not equal to the dog's house number and is equal to
                                                                                                   ; the Old Golds house number. 

  ; The Ukrainian drinks tea.

  (avh (a nationality) (v ukrainian) (h ?n3&~?n2&~?n1))                                            ; The Ukrainian's house number is not equal to the Spaniard's and Englishman's
                                                                                                   ; house numbers. 
  (avh (a drink) (v tea) (h ?d3&~?d2&~?d1&?n3))                                                    ; The tea house is not equal to the milk and coffee house numbers and is equal to
                                                                                                   ; the Ukranian's house number.

  ; The Norwegian resides in the first house on the left.

  (avh (a nationality) (v norwegian) (h ?n4&~?n3&~?n2&~?n1&1))                                     ; The Norwegian's house number is not equal to the Ukranian's, Spaniard's, and
                                                                                                   ; Englishman's house numbers and is house 1.

  ; Chesterfields smoker lives next door to the fox owner.

  (avh (a smokes) (v chesterfields) (h ?s2&~?s1))                                                  ; The Chesterfields house number is not equal to the Old Golds house number.
  (avh (a pet) (v fox) (h ?p3&~?p2&~?p1&:(or (= ?s2 (- ?p3 1)) (= ?s2 (+ ?p3 1)))))                ; The fox's house number is not equal to the snails' and dog's house numbers
                                                                                                   ; and is equal to one more or one less than the Chesterfields house number.

  ; The Lucky Strike smoker drinks orange juice.

  (avh (a smokes) (v lucky-strikes) (h ?s3&~?s2&~?s1))                                             ; The Lucky Strike house number is not equal to the Chesterfields and Old Golds
                                                                                                   ; house numbers.
  (avh (a drink) (v orange-juice) (h ?d4&~?d3&~?d2&~?d1&?s3))                                      ; The orange juice house number is not equal to the tea, milk, and coffee house 
                                                                                                   ; numbers and is equal to the Lucky Strike house number. 

  ; The Japanese smokes Parliaments.

  (avh (a nationality) (v japanese) (h ?n5&~?n4&~?n3&~?n2&~?n1))                                   ; The Japanese's house number is not equal to the Norwegian's, Ukranian's,
                                                                                                   ; Spaniard's, and Englishman's house numbers. 
  (avh (a smokes) (v parliaments) (h ?s4&~?s3&~?s2&~?s1&?n5))                                      ; The Parliaments house number is not equal to the Lucky Strike, Chesterfields,
                                                                                                   ; and Old Golds house numbers and is equal to the Japanese's house number.

  ; The horse owner lives next to the Kools smoker, 
  ; whose house is yellow.

  (avh (a pet) (v horse) (h ?p4&~?p3&~?p2&~?p1))                                                   ; The horse's house number is not equal to the fox's, snails', and dog's house
                                                                                                   ; numbers
  (avh (a smokes) (v kools) (h ?s5&~?s4&~?s3&~?s2&~?s1&:(or (= ?p4 (- ?s5 1)) (= ?p4 (+ ?s5 1))))) ; The Kools house number is not equal to the Parliaments, Lucky Strike,
                                                                                                   ; Chesterfields, and Old Golds house numbers and is equal to one more or one less
                                                                                                   ; than the horse's house number.
  (avh (a color) (v yellow) (h ?c4&~?c3&~?c2&~?c1&?s5))                                            ; The yellow house number is not equal to the green, ivory, and red house numbers
                                                                                                   ; and is equal to the Kools house number.

  ; The Norwegian lives next to the blue house.

  (avh (a color) (v blue) (h ?c5&~?c4&~?c3&~?c2&~?c1&:(or (= ?c5 (- ?n4 1)) (= ?c5 (+ ?n4 1)))))   ; The blue house number is not equal to the yellow, green, ivory, and red house
                                                                                                   ; numbers and is equal one more or one less than the Norwegian's house number.
  
  ; Who drinks water?  And Who owns the zebra?

  (avh (a drink) (v water) (h ?d5&~?d4&~?d3&~?d2&~?d1))                                            ; The water house number is not equal to the orange juice, tea, milk, coffee
                                                                                                   ; house numbers.
  (avh (a pet) (v zebra) (h ?p5&~?p4&~?p3&~?p2&~?p1))                                              ; The zebra's house number is not equal to the horse's, fox's, snails', and dog's
                                                                                                   ; house numbers. 

  =>                                                                                               ; Asserts the solution facts which contains the attribute, version, and house
                                                                                                   ; number. 
  (assert (solution nationality englishman ?n1)
          (solution color red ?c1)
          (solution nationality spaniard ?n2)
          (solution pet dog ?p1)
          (solution color ivory ?c2)
          (solution color green ?c3)
          (solution drink coffee ?d1)
          (solution drink milk ?d2) 
          (solution smokes old-golds ?s1)
          (solution pet snails ?p2)
          (solution nationality ukrainian ?n3)
          (solution drink tea ?d3)
          (solution nationality norwegian ?n4)
          (solution smokes chesterfields ?s2)
          (solution pet fox ?p3)
          (solution smokes lucky-strikes ?s3)
          (solution drink orange-juice ?d4) 
          (solution nationality japanese ?n5)
          (solution smokes parliaments ?s4)
          (solution pet horse ?p4) 
          (solution smokes kools ?s5)
          (solution color yellow ?c4)
          (solution color blue ?c5)
          (solution drink water ?d5)
          (solution pet zebra ?p5))
  )

/*
* This rule will activate once all 25 solution facts are asserted. Then, it will store each of the facts in a variable from ?f1 to ?f25 and binds the versions of each attribute
* to their respective variable. The facts are retracted and the versions of each house's attributes are printed out in table format.
*
* ?n1 to ?n5 stores the nationalities of houses 1 to 5. 
* ?c1 to ?c5 stores the colors of houses 1 to 5
* ?p1 to ?p5 stores the pets of houses 1 to 5
* ?d1 to ?d5 stores the drinks of houses 1 to 5
* ?s1 to ?s5 stores the smokes of houses 1 to 5.
*/
(defrule print-solution
  ?f1 <- (solution nationality ?n1 1)
  ?f2 <- (solution color ?c1 1)
  ?f3 <- (solution pet ?p1 1)
  ?f4 <- (solution drink ?d1 1)
  ?f5 <- (solution smokes ?s1 1)
  ?f6 <- (solution nationality ?n2 2)
  ?f7 <- (solution color ?c2 2)
  ?f8 <- (solution pet ?p2 2)
  ?f9 <- (solution drink ?d2 2)
  ?f10 <- (solution smokes ?s2 2)
  ?f11 <- (solution nationality ?n3 3)
  ?f12 <- (solution color ?c3 3)
  ?f13 <- (solution pet ?p3 3)
  ?f14 <- (solution drink ?d3 3)
  ?f15 <- (solution smokes ?s3 3)
  ?f16 <- (solution nationality ?n4 4)
  ?f17 <- (solution color ?c4 4)
  ?f18 <- (solution pet ?p4 4)
  ?f19 <- (solution drink ?d4 4)
  ?f20 <- (solution smokes ?s4 4)
  ?f21 <- (solution nationality ?n5 5)
  ?f22 <- (solution color ?c5 5)
  ?f23 <- (solution pet ?p5 5)
  ?f24 <- (solution drink ?d5 5)
  ?f25 <- (solution smokes ?s5 5)
  =>
  (retract ?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7 ?f8 ?f9 ?f10 ?f11 ?f12 ?f13 ?f14
           ?f15 ?f16 ?f17 ?f18 ?f19 ?f20 ?f21 ?f22 ?f23 ?f24 ?f25)
  (printout t "HOUSE | Nationality Color Pet Drink Smokes" crlf)
  (printout t "--------------------------------------------------------------------" crlf)
  (printout t "  1   |" ?n1 " " ?c1 " " ?p1 " " ?d1 " " ?s1 crlf)
  (printout t "  2   |" ?n2 " " ?c2 " " ?p2 " " ?d2 " " ?s2 crlf)
  (printout t "  3   |" ?n3 " " ?c3 " " ?p3 " " ?d3 " " ?s3 crlf)
  (printout t "  4   |" ?n4 " " ?c4 " " ?p4 " " ?d4 " " ?s4 crlf)
  (printout t "  5   |" ?n5 " " ?c5 " " ?p5 " " ?d5 " " ?s5 crlf)
  (printout t crlf crlf))

/*
* Since this rule does not have any patterns, its actions will be executed first.
*
* The startup rule prints out the characteristic clues and the question. Then, it asserts facts that represent the attributes available in the clues.  The first slot of the value
* fact contains the attributes: color, nationality, pet, drink, and smokes. The second slot contains the specific version of the attribute; each attribute has five unique versions.
*/
(defrule startup
   =>
   (printout t
    "There are five houses, each of a different color, inhabited by men of"  crlf
    "different nationalities, with different pets, drinks, and cigarettes."  crlf
    crlf
    "The Englishman lives in the red house.  The Spaniard owns the dog."     crlf
    "The ivory house is immediately to the left of the green house, where"   crlf
    "the coffee drinker lives.  The milk drinker lives in the middle house." crlf
    "The man who smokes Old Golds also keeps snails.  The Ukrainian drinks"  crlf
    "tea.  The Norwegian resides in the first house on the left.  The"       crlf)
   (printout t
    "Chesterfields smoker lives next door to the fox owner.  The Lucky"      crlf
    "Strike smoker drinks orange juice.  The Japanese smokes Parliaments."   crlf
    "The horse owner lives next to the Kools smoker, whose house is yellow." crlf
    "The Norwegian lives next to the blue house."			     crlf
    crlf
    "Now, who drinks water?  And who owns the zebra?" crlf crlf)
   (assert (value color red) 
           (value color green) 
           (value color ivory)
           (value color yellow)
           (value color blue)
           (value nationality englishman)
           (value nationality spaniard)
           (value nationality ukrainian) 
           (value nationality norwegian)
           (value nationality japanese)
           (value pet dog)
           (value pet snails)
           (value pet fox)
           (value pet horse)
           (value pet zebra)
           (value drink water)
           (value drink coffee)
           (value drink milk)
           (value drink orange-juice)
           (value drink tea)
           (value smokes old-golds)
           (value smokes kools)
           (value smokes chesterfields)
           (value smokes lucky-strikes)
           (value smokes parliaments)) 
   )

/*
* This rule will check whether a value fact exists in the memory. If it exists, the rule will store the value fact in the variable ?f. Then, it will retract that fact and assert
* its attribute and version into the avh template five times, once for each house number.
*/
(defrule generate-combinations
   ?f <- (value ?s ?e)                                          ; Binds the value fact to the ?f variable so it can be retracted.
   =>
   (retract ?f)
   (assert (avh (a ?s) (v ?e) (h 1))
           (avh (a ?s) (v ?e) (h 2))
           (avh (a ?s) (v ?e) (h 3))
           (avh (a ?s) (v ?e) (h 4))
           (avh (a ?s) (v ?e) (h 5))))



(defglobal ?*time* = (time))                                    ; Stores the starting time as a global variable
(set-reset-globals FALSE)                                       ; Prevents the (reset) command from affecting global variables

/*
* Runs the rule engine ?n times.
*/
(deffunction run-n-times (?n)
  (while (> ?n 0) do
         (reset)
         (run)
         (bind ?n (- ?n 1))))

(run-n-times 1)

(printout t "Elapsed time: " (integer (- (time) ?*time*)) crlf) ; Prints the time the program takes by substracting the starting time from the ending time.

