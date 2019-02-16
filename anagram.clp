/*
* This program asks the user for a word and prints out the anagrams of
* that word. The length of the word is limited to 10 letters maximum as 
* any words with 11 or more letter will crash.
* 
* @author Matthew Jin
* @version 09/17/2018
*/

(reset) (clear)
(batch programs/toolbox.clp)

/*
* Defines the template for the positions of the character in the letter.
* Each letPos contains a slot for a character and its corresponding position
* in the word.
*/
(deftemplate letPos (slot c) (slot p))

/*
* Generates the rules for the anagrams of a word with ?num letters. The rules
* consists of the left side, which contains the patterns, and the right side,
* which contains the printout call. These rules are stored in a string and
* returned so they can be built by the build function.
*/
(deffunction generateRules (?num)
   (bind ?rules "(defrule anagramRules
")                                                                        ; Defines the rules

   (for (bind ?i 1) (<= ?i ?num) (++ ?i)
      (bind ?rules (str-cat ?rules "   (letPos (c ?c" ?i ") (p ?p" ?i))   ; Starts a rule pattern

      (for (bind ?j (- ?i 1)) (> ?j 0) (-- ?j)
         (bind ?rules (str-cat ?rules " &~?p" ?j))                        ; Adds "and nots" (&~) to each pattern in order to prevent duplicate characters from appearing.
      )

      (bind ?rules (str-cat ?rules "))
"))                                                                       ; Ends the rule pattern
   )

   (bind ?rules (str-cat ?rules "=>
"))                                                                       ; Generates the "then" symbol (=>) which seperates the left and right sides of the rule
   
   (bind ?rules (str-cat ?rules "   (printout t"))                        ; Generates the printout call

   (for (bind ?i 1) (<= ?i ?num) (++ ?i)
      (bind ?rules (str-cat ?rules " ?c" ?i))                             ; Adds each character to the printout call
   )

   (bind ?rules (str-cat ?rules " crlf)
)"))                                                                      ; Adds a newline and ends the printout call

   (return ?rules)
)

/*
* Checks whether the input is less then the limit length. If it is, the function
* returns true. Otherwise it will return false.
*/
(deffunction valid (?string ?limit)
   (return (< (str-length ?string) ?limit))
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
* Gets the input from the user and checks whether it satisfies the length requirements. If the word
* does not satisfy the requirements, the function will ask the user for a valid input again. Once
* a valid input has been entered, it will return the input as a string.
*/
(deffunction getWord (?limit)
   (bind ?word (askStatement (str-cat "Please enter a word with less than " ?limit " letters")))

   (while (not (valid ?word ?limit)) do
      (printout t (str-cat "Invalid input. Input must be less than " ?limit " letters.") crlf)
      (bind ?word (askStatement (str-cat "Please enter a word with less than " ?limit " letters")))
   )

   (return ?word)
)

/*
* This function acts as the main method. It gets the input, slices it, and asserts the
* characters. Then it will build the rules and run the rule engine to print the anagrams.
*/
(deffunction anagram (?limit)
   (charAssert (bind ?chars (slice$ (getWord ?limit))))
   (build (generateRules (length$ ?chars)))
   (run)
   (return)
)

(anagram 11) ; Running anagram on an eleven letter word will cause Jess to crash.
