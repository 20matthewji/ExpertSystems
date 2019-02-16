/*
* The toolbox file contains a list of general use functions. Currently, it
* contains the ask, askQuestion, askStatement, and slice$ functions.
*
* @author Matthew Jin
* @version 08/31/18
*/

/*
* The ask method takes in a prompt, prints it, and asks for user input. It
* returns the user input.
*/
(deffunction ask (?prompt)
   (printout t ?prompt) 
   (bind ?response (read))
   (return ?response)
)

/*
* The askQuestion method takes in a prompt and adds question mark to it. It
* calls the ask function to print the prompt and read the user input.
*
* @precondition the prompt does not have punctuation 
*/
(deffunction askQuestion (?prompt)
   (return (ask (str-cat ?prompt "? ")))
)

/*
* The askStatement method takes in a prompt and adds period mark to it. It
* calls the ask function to print the prompt and read the user input.
*
* @precondition the prompt does not have punctuation
*/
(deffunction askStatement (?prompt)
   (return (ask (str-cat ?prompt ". ")))
)

/*
* The slice$ function takes in a string and returns a list containing the
* characters of the string.
*/
(deffunction slice$ (?string)
   (bind ?chars (create$))

   (for (bind ?i 1) (<= ?i (str-length ?string)) (++ ?i)
      (bind ?chars (insert$ ?chars (+ (length$ ?chars) 1) (sub-string ?i ?i ?string)))
   )

   (return ?chars) 
)
