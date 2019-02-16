/*
* This program asks the user for a four letter word and prints out the anagrams
* of that word.
* 
* @author Matthew Jin
* @version 09/13/2018
*/

(reset) (clear)
(batch programs/toolbox.clp)

/*
* Defines the template for the positions of the character in the letter.
*/
(deftemplate letPos (slot c) (slot p))

/*
* Hardcoded rules for printing the four letter anagrams.
*/
(defrule 4LetterRules "rules for the 4 letter anagram"
   (letPos (c ?c1) (p ?p1))
   (letPos (c ?c2) (p ?p2 &~?p1))
   (letPos (c ?c3) (p ?p3 &~?p2 &~?p1))
   (letPos (c ?c4) (p ?p4 &~?p3 &~?p2 &~?p1))
=>
   (printout t ?c1 ?c2 ?c3 ?c4 crlf)
)

/*
* Checks whether the input has the appropriate length. If it does, the function
* returns true. Otherwise it will return false.
*/
(deffunction valid (?string ?length)
   (return (= (str-length ?string) ?length))
)

/*
* Asserts each character in the list into the template using a for loop.
*/
(deffunction charAssert (?list)
   (for (bind ?i 1) (<= ?i (length$ ?list)) (++ ?i)
      (assert (letPos (c (nth$ ?i ?chars)) (p ?i)))
   )

   (return)
)

/*
* Gets the input from the user and checks whether it satisfies the required word length.
*/
(deffunction getWord (?num)
   (bind ?word (askStatement (str-cat "Please enter a " ?num " letter word")))

   (while (not (valid ?word ?num)) do
      (printout t (str-cat "Invalid input. Input must be a " ?num " letter word.") crlf)
      (bind ?word (askStatement (str-cat "Please enter a " ?num " letter word")))
   )

   (return ?word)
)

/*
* This function acts as the main method. It gets the input, slices it, and asserts the
* characters. Then it will run the rule engine to print the anagrams.
*/
(deffunction anagram (?num)
   (charAssert (bind ?chars (slice$ (getWord ?num))))
   (run)
   (return)
)

(anagram 4)
