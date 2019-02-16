/*
* The fibonacci class returns a list of the first n fibonacci numbers. It
* contains the fib function which calculates the numbers and the valid
* function which checks whether the user input is a positive integer. If 
* the input isn't, the program will print out an error message and exit.  
*
* @author Matthew Jin
* @version 09/05/2018
*/

(reset) (clear)
(batch programs/toolbox.clp)

/*
* The fib function calculates the first n fibonacci numbers by building up 
* from (0, 1) using a for loop. The results are stored in a list and returned.  
*/
(deffunction fib (?num)
   (bind ?fib (create$))
   (bind ?a 0l)
   (bind ?b 1l)
   (bind ?c 0l)
   (for (bind ?i 0) (< ?i ?num) (++ ?i)
      (bind ?c ?b)
      (bind ?b (+ ?a ?b))
      (bind ?a ?c) 
      (bind ?fib (insert$ ?fib (+ (length$ ?fib) 1) ?a))
   )
   (return ?fib)
)

/*
* The valid function checks whether the input is a positive integer. If it
* is, the method will return true. Otherwise it will return false.
*/
(deffunction valid (?num)
   (return (and (integerp ?num) (> ?num 0)))
)

/*
* Gets the input from the user and checks whether it is valid or not.
*/
(deffunction getNum ()
   (bind ?num (askStatement "Enter a number"))
   (while (not (valid ?num)) do
      (printout t "Invalid input. Input must be a positive integer." crlf)
      (bind ?num (askStatement "Enter a number"))
   )
   (return ?num)
)

(fib (getNum)) 
