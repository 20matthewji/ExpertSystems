/*
* This program tries to guess an animal that the user is thinking of by asking the user questions about the various traits of the user's
* animal. The user can only respond with yes (Y), no (N), or I don't know (?) and wins if the computer either gives up (could not find the
* animal) or times out (uses more than 20 questions). The computer wins if it guesses the user's animal.
*
* @author Matthew Jin
* @version 10/16/2018
*/

(reset) (clear)
(batch programs/toolbox.clp)

(defglobal ?*questions* = 0) 
(defglobal ?*asked* = 0)

/*
* List of traits that an animal can have.
*/
(defglobal ?*facts* = (create$ mammal reptile bird fish insect land water carnivore herbivore omnivore 4legs 2legs 0legs 8legs 10legs wings swims feline domesticated))

/*
* List of traits that belong in a group, which means that if one of them is asserted as yes, the rest will be asserted as no.
*/
(defglobal ?*g-type* = (create$ mammal reptile bird fish insect))
(defglobal ?*g-food* = (create$ carnivore herbivore omnivore))
(defglobal ?*g-legs* = (create$ 4legs 2legs 0legs 8legs 10legs))

/*
* List of restrictions for each fact. (e.g. mammals don't have 8 or 10 legs)
*/
(defglobal ?*r-mammal* = (create$ mammal 0legs N 8legs N 10legs N))
(defglobal ?*r-reptile* = (create$ reptile 2legs N 8legs N 10legs N wings N feline N))
(defglobal ?*r-bird* = (create$ bird 2legs Y wings Y feline N))
(defglobal ?*r-insect* = (create$ insect 0legs N 2legs N 4legs N 8legs N 10legs N feline N))
(defglobal ?*r-fish* = (create$ fish 0legs Y wings N feline N))

/*
* List of facts that can be backwards chained.
*/
(do-backward-chaining mammal)
(do-backward-chaining reptile)
(do-backward-chaining bird)
(do-backward-chaining fish)
(do-backward-chaining insect)
(do-backward-chaining land)
(do-backward-chaining water)
(do-backward-chaining carnivore)
(do-backward-chaining herbivore)
(do-backward-chaining omnivore)
(do-backward-chaining 4legs)
(do-backward-chaining 2legs)
(do-backward-chaining 0legs)
(do-backward-chaining 8legs)
(do-backward-chaining 10legs)
(do-backward-chaining wings)
(do-backward-chaining swims)
(do-backward-chaining feline)
(do-backward-chaining domesticated)

/*
* Dynamically creates the backwards chaining rules for asking and determining traits.
* It will only ask the question if the number of questions is less than the maximum
* number of questions allowed.
*
* @param ?prompt the question asked for the trait
* @param ?name the name of the trait
*
* @return the rule for the trait as a string
*
* Example of a trait rule for mammals:
*
* (defrule ask-mammal
*    (need-mammal ?)
* =>
*    (if (not (endOfGame)) then
*       (bind ?ans (getAns \"Is it a mammal\"))
*       (assertTrait mammal ?ans)
*    )
* )
*/
(deffunction generateTrait (?prompt ?name)
   (bind ?trait (str-cat "(defrule ask-" ?name "
"))
   (bind ?trait (str-cat ?trait "   (need-" ?name " ?)
"))

   (bind ?trait (str-cat ?trait "=>
"))

   (bind ?trait (str-cat ?trait "   (if (not (endOfGame)) then
"))
   (bind ?trait (str-cat ?trait "      (bind ?ans (getAns \"" ?prompt "\"))
"))
   (bind ?trait (str-cat ?trait "      (assertTrait " ?name " ?ans)
"))
   (bind ?trait (str-cat ?trait "   )
"))
   (bind ?trait (str-cat ?trait ")
"))
   (return ?trait)
)

/*
* Dynamically creates the rules for determining the animal. The right hand side of the
* rule contains all the traits associated with the animal and the left hand side asks
* whether the animal is the user's animal.
*
* @precondition: ?*facts* and ?binans are lists with the same length
*
* @param name the name of the animal
* @binans a list of answers to the traits associated with the animal
*
* @return the rule for the animal as a string

* Example of an animal rule for a whale:
* 
* (defrule isWhale
*    (mammal Y)
*    (reptile N)
*    (bird N)
*    (insect N)
*    (land N)
*    (water Y)
*    (carnivore Y)
*    (herbivore N)
*    (omnivore N)
* ;  etc.
* =>
*    (if (not (endOfGame)) then
*       (bind ?ans (getAns "Is it a whale"))
*       (if (eq ?ans "Y") then
*          (endGame "I win!")
*       )
*    )
* )
*/
(deffunction generateAnimal (?name ?binans)
   (bind ?animal (str-cat "(defrule is-" ?name "
"))

   (for (bind ?i 1) (<= ?i (length$ ?*facts*)) (++ ?i)
      (bind ?animal (str-cat ?animal "   (" (nth$ ?i ?*facts*) " " (nth$ ?i ?binans) ")
"))
   )

   (bind ?animal (str-cat ?animal "=>
"))

   (bind ?animal (str-cat ?animal "   (if (not (endOfGame)) then
"))
   (bind ?animal (str-cat ?animal "      (bind ?ans (getAns \"Is it a/an " ?name "\"))
"))
   (bind ?animal (str-cat ?animal "      (if (eq ?ans \"Y\") then
"))
   (bind ?animal (str-cat ?animal "         (endGame \"I win!\")
"))
   (bind ?animal (str-cat ?animal "      )
"))
   (bind ?animal (str-cat ?animal "   )
"))
   (bind ?animal (str-cat ?animal ")
"))
   (return ?animal)
)

/*
* Builds the rules for the traits associated to the animals that the expert system knows.
*/
(deffunction buildTraits ()
   (build (generateTrait "Is it a mammal" "mammal"))
   (build (generateTrait "Is it a reptile" "reptile"))
   (build (generateTrait "Is it a bird" "bird"))
   (build (generateTrait "Is it a fish" "fish"))
   (build (generateTrait "Is it an insect" "insect"))
   (build (generateTrait "Does it live on land" "land"))
   (build (generateTrait "Does it live in water" "water"))
   (build (generateTrait "Is it a carnivore" "carnivore"))
   (build (generateTrait "Is it a herbivore" "herbivore"))
   (build (generateTrait "Is it a omnivore" "omnivore"))
   (build (generateTrait "Does it have 4 legs" "4legs"))
   (build (generateTrait "Does it have 2 legs" "2legs"))
   (build (generateTrait "Does it have 0 legs" "0legs"))
   (build (generateTrait "Does it have 8 legs" "8legs"))
   (build (generateTrait "Does it have 10 legs" "10legs"))
   (build (generateTrait "Does it have wings" "wings"))
   (build (generateTrait "Can it swim" "swims"))
   (build (generateTrait "Is it feline" "feline"))
   (build (generateTrait "Is it domesticated" "domesticated"))
)  

/*
* Builds the rules for the animals.
*/
(deffunction buildAnimals ()
   (build (generateAnimal ant (create$ N N N N Y Y N N N Y N N N N N N N N N)))
   (build (generateAnimal bear (create$ Y N N N N Y N N N Y Y N N N N N Y N N)))
   (build (generateAnimal bee (create$ N N N N Y Y N N Y N N N N N N Y N N Y)))
   (build (generateAnimal chicken (create$ N N Y N N Y N N N Y N Y N N N Y N N Y)))
   (build (generateAnimal crab (create$ N N N N N Y Y N N Y N N N N Y N N N Y)))
   (build (generateAnimal crocodile (create$ N Y N N N Y Y Y N N Y N N N N N Y N N)))
   (build (generateAnimal eagle (create$ N N Y N N Y N Y N N N Y N N N Y Y N Y)))
   (build (generateAnimal eel (create$ N N N Y N N Y Y N N N N Y N N N Y N Y)))
   (build (generateAnimal elephant (create$ Y N N N N Y N N Y N Y N N N N N Y N Y)))
   (build (generateAnimal lizard (create$ N Y N N N Y N Y N N Y N N N N N Y N N)))
   (build (generateAnimal penguin (create$ N N Y N N Y Y Y N N N Y N N N Y Y N N)))
   (build (generateAnimal shark (create$ N N N Y N N Y Y N N N N Y N N N Y N N)))
   (build (generateAnimal snake (create$ N Y N N N Y Y Y N N N N Y N N N Y N N)))
   (build (generateAnimal spider (create$ N N N N N Y N Y N N N N N Y N N N N N)))
   (build (generateAnimal whale (create$ Y N N N N N Y Y N N N N Y N N N Y N N)))
   (build (generateAnimal cat (create$ Y N N N N Y N Y N N Y N N N N N Y Y Y)))
   (build (generateAnimal lion (create$ Y N N N N Y N Y N N Y N N N N N Y Y N)))
   (build (generateAnimal duck (create$ N N Y N N Y Y N N Y N Y N N N Y Y N Y)))
   (build (generateAnimal dog (create$ Y N N N N Y N Y N N Y N N N N N Y N Y)))
)

/*
* First rule that activates. It prints the startup message for the game.
*/
(defrule startup "prints the start message"
   (declare (salience 100))
=>
   (printout t "You are playing " ?*questions* " questions." crlf)
)

/*
* Activates when all the trait and animal rules have fired, which means the expert system cannot
* match the answers to the trait questions to an animal it knows.
*/
(defrule giveup "the expert system does not know any animals that fit the traits described"
   (declare (salience -100))
=>
   (endGame "I give up. I do not know any animals that fit the traits described.")
)

/*
* Prints the prompt and asks the user for a yes (Y), no (N), I don't know (?) answer. It takes
* the first letter of the answer and checks to make sure it is either Y, N, or ?. If it isn't,
* an error message will be printed and the user will be asked to answer the question again.
*
* @param ?prompt the question being asked
*
* @return the answer to the prompt
*/
(deffunction getAns (?prompt)
   (bind ?ans (upcase (sub-string 1 1 (askQuestion (str-cat ?prompt " (Y/N/?)")))))
   (while (not (valid ?ans))
      (printout t "Please enter a valid input." crlf)
      (bind ?ans (upcase (sub-string 1 1 (askQuestion ?prompt)))) 
   )
   (return ?ans)
)

/*
* Checks whether answer to a is Y, N, or ?.
*
* @param ?ans the answer to the question
*
* @return true of ?ans is Y, N, or ?; otherwise, false
*/
(deffunction valid (?ans)
   (return (or (eq ?ans "Y") (eq ?ans "N") (eq ?ans "?")))
)

/*
* Takes the user's answer to the question about whether the animal has the trait ?name and
* asserts Y if the answer is "yes", N if the answer is "no", and both if the answer is "?".
*
* @param ?name the name of the trait
* @param ?ans the user's answer to the question about the trait
*/
(deffunction assertTrait (?name ?ans)
   (bind ?yes (str-cat "(" ?name " Y)"))
   (bind ?no (str-cat "(" ?name " N)"))
   (if (eq ?ans "?") then
      (assert-string ?yes)
      (assert-string ?no)
   else
      (assert-string (str-cat "(" ?name " " ?ans ")"))
   )
   (if (eq ?ans "Y") then
      (singular ?name)
      (restrict ?name)
   )
   (return)
)

/*
* Calls assertRestriction on each restriction list for the trait ?name.
*
* @param ?name the name of the trait
*/
(deffunction restrict (?name)
   (assertRestriction ?name ?*r-mammal*)
   (assertRestriction ?name ?*r-reptile*)
   (assertRestriction ?name ?*r-bird*)
   (assertRestriction ?name ?*r-fish*)
   (assertRestriction ?name ?*r-insect*)
   (return)
)

/*
* Checks whether the trait ?name is present in the restriction list ?rlist. If it is,
* this function will assert all other traits in ?rlist to no. If it isn't, this function
* does nothing.
*
* @param ?name the name of the trait
* @param ?rlist the restriction list
*/
(deffunction assertRestriction (?name ?rlist)
   (if (eq ?name (nth$ 1 ?rlist)) then
      (bind ?others (rest$ ?rlist))
      (for (bind ?i 1) (<= ?i (length$ ?others)) (bind ?i (+ ?i 2))
         (bind ?fact (nth$ ?i ?others))
         (bind ?val (nth$ (+ ?i 1) ?others))
         (assert-string (str-cat "(" ?fact " " ?val ")"))
         (if (eq ?val Y) then
            (singular ?fact)
         )
      )
   )
   (return)
)

/*
* Calls assertSingular on each group list for the trait ?name.
*
* @param ?name the name of the trait
*/
(deffunction singular (?name)
   (assertSingular ?name ?*g-type*)
   (assertSingular ?name ?*g-food*)
   (assertSingular ?name ?*g-legs*)
   (return)
)

/*
* Checks whether the trait ?name is present in the restriction list ?glist. If it is,
* this function will assert all other traits in ?glist to no. If it isn't, this function
* does nothing.
*
* @param ?name the name of the trait
* @param ?glist the group list
*/
(deffunction assertSingular (?name ?glist)
   (if (integerp (member$ ?name ?glist)) then
      (bind ?others (complement$ (create$ ?name) ?glist))
      (for (bind ?i 1) (<= ?i (length$ ?others)) (++ ?i)
         (assert-string (str-cat "(" (nth$ ?i ?others) " N)"))
      )
   )
   (return)
)

/*
* Checks whether the game has ended, which occurs when the number of questions asked
* equals the maximum number of questions allowed.
*
* @return true if ?*asked*==?*questions*; otherwise, false
*/
(deffunction endOfGame ()
   (bind ?eog TRUE)
   (if (<= ?*asked* ?*questions*) then
      (++ ?*asked*)
      (bind ?eog FALSE)
   else
      (endGame "I lose!")
   )
   (return ?eog)
)

/*
* Ends the game and prints out a message along with the number of questions the game lasted.
*
* @param ?msg the end of game message
*/
(deffunction endGame (?msg)
   (printout t ?msg crlf)
   (printout t "This round lasted " ?*asked* " questions." crlf)
   (halt)
   (return)
)

/*
* Plays the 20 questions game.
*
* @param ?q the maximum number of questions allowed (default 20)
*/
(deffunction play (?q)
   (reset)

   (bind ?*questions* ?q)
   (bind ?*asked* 0)

   (buildTraits)
   (buildAnimals)   

   (run)
   (return)
)

(play 20)
