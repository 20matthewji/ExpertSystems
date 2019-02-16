/*
*
*
* @author Matthew Jin
* @version 10/11/2018
*/

(reset) (clear)
(batch programs/toolbox.clp)

(defglobal ?*MIN_JOINTS* = 0)
(defglobal ?*MAX_JOINTS_L* = 10)    ; shoulders, elbows, hips, knees, and ankles
(defglobal ?*MAX_JOINTS_Meta* = 10) ; five metacarpophalangeal joints on each hand/foot
(defglobal ?*MAX_JOINTS_Inter* = 8) ; four interphalangeal (proximal/distal) joints on each hand/foot

(do-backward-chaining large)
(do-backward-chaining mcp)
(do-backward-chaining mtp)
(do-backward-chaining piphand)
(do-backward-chaining pipfoot)
(do-backward-chaining diphand)
(do-backward-chaining dipfoot)

(deftemplate arthritis (slot type) (slot score))

/*
* Determined through symptoms such as stiffness/tenderness, swelling, and pain.
*/
(defrule askLargeJoints "asks which large joints are affected"
   (need-large ?)
=>
   (bind ?ans (getInt "How many large joints (shoulders, elbows, hips, knees, and ankles) are affected" MIN_JOINTS MAX_JOINTS_L))
   (assert large ?ans)
)

/*
* Determined through symptoms such as stiffness/tenderness, swelling, and pain.
*/
(defrule askMCPJoints "ask which MCP joints are affected"
   (need-mcp ?)
=>
   (bind ?ans (getInt "How many metacarpophalangeal joints (joints connecting the fingers to the palm) are affected" MIN_JOINTS MAX_JOINTS_Meta)
   (assert mcp ?ans)
)

(defrule askMTPJoints "ask which MTP joints are affected"
   (need-mtp ?)
=>
   (bind ?ans (getInt "How many metatarsophalangeal joints (joints connecting the toes to the foot) are affected" MIN_JOINTS MAX_JOINTS_Meta)
   (assert mtp ?ans)
) 

(defrule askPIPHandJoints "ask which PIP hand joints are affected"
   (need-piphand ?)
=>
   (bind ?ans (getInt "How many proximal interphalangeal joints (joints in the middle of fingers 2 to 5) are affected" MIN_JOINTS MAX_JOINTS_Inter)
   (assert piphand ?ans)
)

(defrule askPIPFootJoints "ask which PIP foot joints are affected"
   (need-pipfoot ?)
=>
   (bind ?ans (getInt "How many proximal interphalangeal joints (joints in the middle of toes 2 to 5) are affected" MIN_JOINTS MAX_JOINTS_Inter)
   (assert pipfoot ?ans)
)


;-------------------------------;
; Rheumatoid Arthritis Criteria ;
;-------------------------------;

(defrule  

(deffunction getInt (?prompt ?lower ?upper)
   (bind ?ans (askQuestion ?prompt))
   (while (or (not (integerp ?ans)) (< ?ans ?lower) (> ?ans ?upper))
      (printout t (str-cat "Please enter an integer between " ?lower " and " ?upper ".") crlf)
      (bind ?ans (askQuestion ?prompt))
   )
   (return ?ans)
)

(deffunction start
   (reset)
   
   (assert (arthritis (type rheumatoid) (score 0)))

   (run)
   (return)
)
