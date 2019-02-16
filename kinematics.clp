/*
* Identifies which of the five kinematics equations are required given the available variables. It uses a global variable to keep count of the number of variables.
*
* @author Matthew Jin
* @version 10/02/18
*/

(reset) (clear)
(batch programs/toolbox.clp)

(deftemplate count (slot num))

(defglobal ?*vars* = (create$))

/*
*
*/
(defrule getValue
   ?c <- (count {num < 3}) 
=>
   (printout t ?c.num crlf)
   (if (> (length$ ?*vars* 0)) then
      (bind ?var (first$ ?*vars*))
      (bind ?ans (askVar ?var))
   
      (if (eq ?ans Y) then
         (modify ?c (num ++?c.num))
      )

      (bind ?*vars* (rest$ ?*vars*))
   else
      (printout t "Insufficient information." crlf)
      (retract ?c)
      (halt)
   )
)

/*
* Asks if the user knows the value of a variable by calling the askQuestion function.
*/
(deffunction askVar (?var)
   (askQuestion (str-cat "Do you know the value of " ?var " (Y/N)"))
   (return)
)

/*
* Prints the arguments.
*/
(deffunction println ($?args)
   (foreach ?text $?args (printout t ?text))
   (printout t crlf)
   (return)
)

/*
* Runs the rule engline ?n times.
*/
(deffunction run-n-times (?n)
   (while (> ?n 0) do
      (reset)

      (bind ?*vars* (create$ t v v0 a ∆s))
      (assert (count (num 0)))

      (run-until-halt)
      (bind ?n (- ?n 1)) 
      (facts)
   )
   (return)
)

(run-n-times 1)
