/*
* This program asks the user for a number and calculates the factorial of that
* number. It uses the ask function from the toolbox to ask the prompt.
*
* @author Matthew Jin
* @version 09/05/2018
*/

(reset) (clear)
(batch programs/toolbox.clp)

/*
* This function finds the factorial of a number. It uses recursion and will
* return 1 if the input is less than or equal to 1.
*/
(deffunction fact (?num)
   (if (<= ?num 1) then
      (return 1)
   else
      (return (* ?num (fact (- ?num 1))))
   )
)

(bind ?num (ask "Enter a number"))

(fact ?num)

