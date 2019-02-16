/*
* This program determines whether the user has rheumatoid arthritis, gout, or osteoarthritis. Both rheumatoid arthritis and gout have 
* follwo the American College of Rheumatology (ACR) classification criteria. Each arthritis has a likelihood score that increases as 
* the user satisfies various criteria relating to various symptoms and lab tests. Once the score for a specific arthritis  passes a 
* certain threshold, the user can be positively diagnosed with that arthritis.
* The questions are all backwards chained while the criteria calculations are forward chained.
* 
* @author Matthew Jin
* @version 10/11/2018
*/

(reset) (clear)
(batch programs/toolbox.clp)

(defglobal ?*AGE* = 0)
(defglobal ?*SEX* = 0)
(defglobal ?*MIN_JOINTS* = 0)
(defglobal ?*MAX_JOINTS_L* = 10)       ; maximum number of large joints
(defglobal ?*MAX_JOINTS_Meta* = 10)    ; maximum number of metacarpophalangeal/metatarsophalangeal joints
(defglobal ?*MAX_JOINTS_Inter* = 8)    ; maximum number of proximal/distal interphalangeal joints for the hands/feet
(defglobal ?*MAX_JOINTS_Sing* = 2)     ; maximum number of singular joints (thumb interphalangeal, wrist joints, etc.)
(defglobal ?*RF_NORM* = 15)            ; normal limit for RF, test results higher than this are low positives
(defglobal ?*RF_HIGH* = 45)            ; high limit for RF, test results higher than this are high positives
(defglobal ?*ACCP_NORM* = 20)          ; normal limit for anti-CCP, test results higher than this are low positives
(defglobal ?*ACCP_HIGH* = 60)          ; high limit for anti-CCP, test results higher than this are high positives
(defglobal ?*CRP_NORM* = 3)            ; normal limit for CRP
(defglobal ?*ESR_NORM* = 0)            ; normal limit for ESR in mm/hr that depends on the age and gender of the patient, defaults to 0
(defglobal ?*SU_NORM* = 4)             ; normal limit for serum urate

(do-backward-chaining large)           ; shoulders, elbows, hips, knees, and ankles
(do-backward-chaining small)           ; metacarophalangeal, metatarsophalangeal, proximal, distal, and thumb interphalangeal, and wrist joints
(do-backward-chaining rf)              ; rheumatoid factor
(do-backward-chaining accp)            ; anti-cyclic citrullinated peptide
(do-backward-chaining esr)             ; erythrocyte sedimentation rate
(do-backward-chaining crp)             ; c-reactive protein
(do-backward-chaining duration)
(do-backward-chaining goutep)          ; gout episode (entry criterion)
(do-backward-chaining msu)             ; monosodium urate
(do-backward-chaining anklemidfoot)
(do-backward-chaining firstmtp)        ; first metatarsophalangeal joint
(do-backward-chaining goutchars)
(do-backward-chaining epnum)           ; total number of gout episodes
(do-backward-chaining tophi)           ; crystalline uric acid deposits
(do-backward-chaining serumurate)      ; uric acid levels
(do-backward-chaining uratedeposition) ; deposits of urate present in gout
(do-backward-chaining jointdamage)     ; joint damage such as joint deformity and joint erosion
(do-backward-chaining firstcmc)        ; first carpometacarpal joint
(do-backward-chaining usagepain)       ; joint pain after usage
(do-backward-chaining bonyswelling)
(do-backward-chaining osteophytes)     ; bony outgrowth that occurs in osteoarthritis
(do-backward-chaining symmetry)

/*
* The startup rule will fire first and print a message that gives instructions to the user. It also asks for the age and sex of the user 
* and uses it to calculate the normal erythrocyte sedimentation rate of the user.
*/
(defrule startup "asks for the age and sex of the user"
   (declare (salience 100))
=>
   (printout t "This machine will ask you questions and determine if you have rheumatoid arthritis, gout, or osteoarthritis." crlf)
   (printout t "Use the (restart) function after you have answered all questions to restart the diagnosis.")

   (bind ?ans (askQuestion "What is your age"))
   (while (not (integerp ?ans))
      (printout t "Please enter a valid integer." crlf)
      (bind ?ans (askQuestion "What is your age"))
   )
   (bind ?*AGE* ?ans)

   (bind ?ans (askQuestion "What is your sex (M/F)"))
   (while (not (or (eq (upcase ?ans) M) (eq (upcase ?ans F))))
      (printout t "Please enter either M or F." crlf)
      (bind ?ans (askQuestion "What is your sex (M/F"))
   )
   (bind ?*SEX* (upcase ?ans))

   (if (eq ?*SEX* M) then
      (bind ?*ESR_NORM* (/ ?*AGE* 2))
   else
      (bind ?*ESR_NORM* (/ (+ ?*AGE 10) 2))
   )
)

;----------------------;
; Diagnostic Questions ;
;----------------------;

/*
* Asks the user how many large joints (shoulders, elbows, hips, knees, and ankles) are affected and asserts the response as a fact. The 
* large fact represents the number of large joints affected.
*/
(defrule askLargeJoints "asks which joints are affected"
   (need-large ?)
=>
   (bind ?large (getNumBtw "How many large joints (shoulders, elbows, hips, knees, and ankles) are affected" ?*MIN_JOINTS* ?*MAX_JOINTS_L*))
   (assert (large ?large))
)

/*
* Asks the user how many small joints (metacarophalangeal, metatarsophalangeal, proximal, distal, and thumb interphalangeal, and wrist joints) 
* are affected and asserts the response as a fact. The small fact represents the total number of joints affected. The specific type of joints 
* each have their own facts.
*/
(defrule askSmallJoints "asks which small joints are affected"
   (need-small ?)
=>
   (bind ?mcp (getNumBtw "How many metacarpophalangeal joints (joints connecting the fingers to the palm) are affected" ?*MIN_JOINTS* ?*MAX_JOINTS_Meta*))
   (bind ?mtp (getNumBtw "How many metatarsophalangeal joints (joints connecting the toes to the foot) are affected" ?*MIN_JOINTS* ?*MAX_JOINTS_Meta*))
   (bind ?piphand (getNumBtw "How many hand proximal interphalangeal joints (joints in the middle of fingers 2 to 5) are affected" ?*MIN_JOINTS* ?*MAX_JOINTS_Inter*))
   (bind ?pipfoot (getNumBtw "How many foot proximal interphalangeal joints (joints in the middle of toes 2 to 5) are affected" ?*MIN_JOINTS* ?*MAX_JOINTS_Inter*))
   (bind ?diphand (getNumBtw "How many hand distal interphalangeal joints (joints at the end of fingers 2 to 5) are affected" ?*MIN_JOINTS* ?*MAX_JOINTS_Inter*))
   (bind ?dipfoot (getNumBtw "How many foot distal interphalangeal joints (joints at the end of toes 2 to 5) are affected" ?*MIN_JOINTS* ?*MAX_JOINTS_Inter*))
   (bind ?thumb (getNumBtw "How many thumb interphalangeal joints (joints in the middle of the thumb) are affected" ?*MIN_JOINTS* ?*MAX_JOINTS_Sing*))
   (bind ?wrist (getNumBtw "How many wrist joints are affected" ?*MIN_JOINTS* ?*MAX_JOINTS_Sing*))
   (assert 
      (small (+ ?mcp ?mtp ?piphand ?pipfoot ?diphand ?dipfoot ?thumb ?wrist))
      (mcp ?mcp)
      (mtp ?mtp)
      (piphand ?piphand)
      (pipfoot ?pipfoot)
      (diphand ?diphand)
      (dipfoot ?dipfoot)
      (thumb ?thumb)
      (wrist ?wrist)
   )
)

/*
* Asks the user for their rheumatoid factor (RF) test result as a float. If the user has not taken this test, 
* they can input "?" and the machine will assume that their RF is normal.
*/
(defrule askRF "asks for the RF test result"
   (need-rf ?)
=>
   (bind ?ans (getResult "What is your rheumatoid factor test result (enter ? if you do not know)"))
   (if (eq (sub-string 1 1 ?ans) "?") then
      (assert (rf ?*RF_NORM*))
   else
      (assert (rf ?ans))
   )
)

/*
* Asks the user for their anti-cyclic citrullinated protein (anti-CCP) test result as a float. If the user has not taken this test,
* they can input "?" and the machine will assume that their anti-CCP level is normal.
*/
(defrule askAntiCCP "asks for the anti-CCP test result"
   (need-accp ?)
=>
   (bind ?ans (getResult "What is your anti-cyclic citrullinated peptide antibody level (enter ? if you do not know)"))
   (if (eq (sub-string 1 1 ?ans) "?") then
      (assert (accp ?*ACCP_NORM*))
   else
      (assert (accp ?ans))
   )
)

/*
* Asks the user for their erythrocyte sedimentation rate (ESR) test result as a float. If the user has not taken this test,
* they can input "?" and the machine will assume that their ESR level is normal.
*/
(defrule askESR "asks for the ESR test result"
   (need-esr ?)
=>
   (bind ?ans (getResult "What is your erythrocyte sedimentation rate (enter ? if you do not know)"))
   (if (eq (sub-string 1 1 ?ans) "?") then
      (assert (esr ?*ESR_NORM*))
   else
      (assert (esr ?ans))
   )
)

/*
* Asks the user for their c-reactive protein (CRP) test result as a float. If the user has not taken this test,
* they can input "?" and the machine will assume that their CRP level is normal.
*/
(defrule askCRP "asks for the CRP test result"
   (need-crp ?)
=>
   (bind ?ans (getResult "What is your c-reactive protein level (enter ? if you do not know)"))
   (if (eq (sub-string 1 1 ?ans) "?") then
      (assert (crp ?*CRP_NORM*))
   else
      (assert (crp ?ans))
   )
)

/*
* Asks the user for the duration of their arthritis symptoms in days.
*/
(defrule askDuration "asks for the duration of the arthritis"
   (need-duration ?)
=>
   (bind ?ans (getNumAbove "For approximately how long have you had these arthritis symptoms (in days)" 1))
   (assert (duration ?ans))
)

/*
* Asks the user if they have ever experienced a gout episode, which is characterized by sudden joint swelling and pain. 
* This question serves as the entry criterion for gout diagnosis. If they have not experienced a gout episode, they will 
* not be diagnosed with gout.
*/
(defrule askGoutEpisode "asks if the user has experienced a gout episode"
   (need-goutep ?)
=>
   (bind ?ans (getBool "Have you ever had a symptomatic episode of swelling, tenderness, or pain in a joint"))
   (assert (goutep ?ans))
)

/*
* Asks the user whether monosodium urate (MSU) crystals are present in their synovial fluid (joint fluid). The presence 
* of monosodium urate serves as an indicator that the user has gout. This question will only be asked if the user passes 
* the gout entry criterion.
*/
(defrule askMSU "asks if there are msu crystals in synovial fluid"
   (need-msu ?)
   (possibleGout)
=>
   (bind ?ans (getBool "Are there monosodium urate crystals present in your synovial fluid"))
   (assert (msu ?ans))
)

/*
* Asks the user if their first metatarsophalangeal joint (big toe) is affected. This rule requires that the at least one 
* metatarsophalangeal joint is involved.
*/
(defrule askFirstMTP "asks whether the first metatarsophalangeal joint is involved"
   (need-firstmtp ?)
   (mtp ?x&:(> ?x 0))
=>
   (bind ?ans (getBool "Is the first metatarsophalangeal joint (big toe) affected during symptomatic episodes"))
   (assert (firstmtp ?ans))
)

/*
* Asks the user if they have experienced arthritis symptoms in their ankle or midfoot. This question will only be asked 
* if the user passes the gout entry criterion.
*/
(defrule askAnkleMidfoot "asks whether the ankle or midfoot are involved"
   (need-anklemidfoot ?)
   (possibleGout)
=>
   (bind ?ans (getBool "Are the ankle or midfoot affected during symptomatic episodes"))
   (assert (anklemidfoot ?ans))
)

/*
* Asks the user if they have experienced any of the following gout symptoms: erythema over the affected joint (skin redness), 
* joint pain when touched/under pressure, and difficulty using affected joint. This question will only be asked if the user 
* passes the gout entry criterion.
*/
(defrule askGoutChars "asks whether the user has experienced any gout related symptoms"
   (need-goutchars ?)
   (possibleGout)
=>
   (bind ?ans (getNumBtw "How many of the following characteristics (erythema over affected joint, joint is painful to touch/pressure, great difficulty walking/using affected joint) have you observed during a symptomatic episode" 0 3))
   (assert (goutchars ?ans))
)

/*
* Asks how many gout episodes the user has experienced. Gout episodes are often characterized with a sharp pain that occurs 
* within 24 hours and slowly fades over the next two weeks. The symptoms of the gout episodes usually disappear between episodes. 
* This question will only be asked if the user passes the gout entry criterion.
*/
(defrule askEpisodeNum "asks for the occurrences of gout related episodes"
   (need-epnum ?)
   (possibleGout)
=>
   (bind ?ans (getNumAbove "How many gout episodes (max pain within 24 hours, symptoms alleviate within 14 days, symptoms disappear between episodes) have you experienced" 0))
   (assert (epnum ?ans))
)

/*
* Asks whether the user has any tophi, which are urate deposits that occur during gout. This question will only be asked if 
* the user passes the gout entry criterion.
*/
(defrule askTophi "asks for the presence of tophi"
   (need-tophi ?)
   (possibleGout)
=>
   (bind ?ans (getBool "Are there any tophi (uric acid crystal deposit)"))
   (assert (tophi ?ans))
)

/*
* Asks the user for their serum urate level as a float If the user does not know their serum urate level,
* they can input "?" and the machine will assume that level is normal. This question will only be asked if 
* the user passes the gout entry criterion.
*/
(defrule askSerumUrate "asks for the serum urate level (mg/dl)"
   (need-serumurate ?)
   (possibleGout)
=>
   (bind ?ans (getResult "What is your serum urate level (enter ? if you do not know)"))
   (if (eq (sub-string 1 1 ?ans) "?") then
      (assert (serumurate ?*SU_NORM*))
   else
      (assert (serumurate ?ans))
   )
)

/*
* Asks if the user has imaging evidence of urate deposits which are present in gout. This question will only 
* be asked if the user passes the gout entry criterion.
*/
(defrule askUrateDeposition "asks for imaging evidence of urate deposition"
   (need-uratedeposition ?)
   (possibleGout)
=>
   (bind ?ans (getBool "Do you have evidence of urate deposits"))
   (assert (uratedeposition ?ans))
)

/*
* Asks if the user has imaging evidence of joint damage. This question will only be asked if the user passes 
* the gout entry criterion.
*/
(defrule askJointDamage "asks for imaging evidence of joint damage"
   (need-jointdamage ?)
=>
   (bind ?ans (getBool "Do you have evidence of gout-related joint deformity or erosion"))
   (assert (jointdamage ?ans))
)

/*
* Asks the user if their first carpometacarpal joint (wrist) is involved. This rule requires that the at least 
* one wrist joint is involved.
*/
(defrule askfirstCMC "asks whether the thumb carpometacarpal joint is involved"
   (need-firstcmc ?)
   (wrist ?x&:(> ?x 0))
=>
   (bind ?ans (getBool "Is the first carpometacarpal joint (wrist joint near your thumb) affected"))
   (assert (firstcmc ?ans))
)

/*
* Asks the user if their affected become more painful after use which is a characteristic of osteoarthritis.
*/
(defrule askJointUsagePain "asks whether arthritis pain/stiffness gets worse with joint use"
   (need-usagepain ?)
=>
   (bind ?ans (getBool "Does your joint get stiffer or more painful with joint usage"))
   (assert (usagepain ?ans))
)

/*
* Asks if the joint swelling is bony which is another characteristic of osteoarthritis.
*/
(defrule askBonySwelling "asks whether the joint swelling is bony"
   (need-bonyswelling ?)
=>
   (bind ?ans (getBool "Is the swelling on the joint hard or bony"))
   (assert (bonyswelling ?ans))
)

/*
* Asks if the user has evidence of osteophytes, bony outgrowths that appear during osteoarthritis.
*/
(defrule askOsteophytes "asks whether there is evidence of osteophytes"
   (need-osteophytes ?)
=>
   (bind ?ans (getBool "Do you have imaging evidence of cartilage loss or osteophytes"))
   (assert (osteophyte ?ans))
)

/*
* Asks if the affected joints are symmetric. Rheumatoid arthritis usually affects joints symmetrically 
* while osteoarthritis affects them asymmetrically.
*/
(defrule askSymmetry "asks whether the joints affected are symmetric"
   (need-symmetry ?)
=>
   (bind ?ans (getBool "Are the joints affected on one hand are also affected on the other hand"))
   (assert (symmetry ?ans))
)

/*
* Each criteria rule will only be fired once.
*/
;-------------------------------;
; Rheumatoid Arthritis Criteria ;
;-------------------------------;

/*
* This rule increases the rheumatoid arthritis likelihood score based on the number and type of joints 
* that show arthritis symptoms.
* The criteria is as follows:
*     +5 points if more than 10 joints are involved
*     +3 points if 4 to 10 small joints are involved
*     +2 points if 1 to 3 small joints are involved
*     +1 point if 2 to 10 large joints are involved
*/
(defrule affectedJoints "alters rheumatoid score based on the affected joints"
   (not (exists (ra-affectedJoints)))
   ?ra <- (rheumatoid ?s)
   (large ?x)
   (small ?y)
=>
   (retract ?ra)
   (if (> (+ ?x ?y) 10) then
      (assert (rheumatoid (+ ?s 5)))
   elif (and (>= ?y 4) (<= ?y 10)) then
      (assert (rheumatoid (+ ?s 3)))
   elif (and (>= ?y 1) (<= ?y 3)) then
      (assert (rheumatoid (+ ?s 2)))
   elif (and (>= ?x 2) (<= ?x 10)) then
      (assert (rheumatoid (+ ?s 1)))
   else
      (assert (rheumatoid ?s))
   )
   (assert (ra-affectedJoints))
)

/*
* This rule increases the rheumatoid arthritis likelihood score based on the user's RF and ACCP levels.
* The criteria is as follows:
*     +3 points if either RF or ACCP levels are above the high limit
*     +2 points if either RF or ACCP levels are above the normal limit
*/
(defrule serologicalTests "alters rheumatoid score based on the results of RF and anti-CCP tests"
   (not (exists (ra-serologicalTests)))
   ?ra <- (rheumatoid ?s)
   (rf ?x)
   (accp ?y)
=>
   (retract ?ra)
   (if (or (> ?x ?*RF_HIGH*) (> ?y ?*ACCP_HIGH*)) then
      (assert (rheumatoid (+ ?s 3)))
   elif (or (> ?x ?*RF_NORM*) (> ?y ?*ACCP_NORM*)) then
      (assert (rheumatoid (+ ?s 2)))
   else
      (assert (rheumatoid ?s))
   )
   (assert (ra-serologicalTests))
)

/*
* This rule increases the rheumatoid arthritis likelihood score based on the user's ESR and CRP levels.
* The criteria is as follows:
*     +1 point if either ESR or CRP levels are above the normal limit
*/
(defrule acutePhaseResponse "alters rheumatoid score based on the results of ESR and CRP tests"
   (not (exists (ra-acutePhaseResponse)))
   ?ra <- (rheumatoid ?s)
   (esr ?x)
   (crp ?y)
=>
   (retract ?ra)
   (if (or (> ?x ?*ESR_NORM*) (> ?y ?*CRP_NORM*)) then
      (assert (rheumatoid (+ ?s 1)))
   else
      (assert (rheumatoid ?s))
   )
   (assert (ra-acutePhaseResponse))
)

/*
* This rule increases the rheumatoid arthritis likelihood score based on the duration of the user's symptoms.
* The criteria is as follows:
*     +1 point if the duration is longer than six weeks (42 days)
*/
(defrule symptomDuration "alters rheumatoid score based on the duration of the symptoms"
   (not (exists (ra-symptomDuration)))
   ?ra <- (rheumatoid ?s)
   (duration ?x)
=>
   (retract ?ra)
   (if (>= ?x 42) then
      (assert (rheumatoid (+ ?s 1)))
   else
      (assert (rheumatoid ?s))
   )
   (assert (ra-symptomDuration))
)

;---------------;
; Gout Criteria ;
;---------------;
/*
* The rules that affect the gout likelihood score will only be activated if the user satisfies the entry criterion 
* and does not satisfy the sufficient criterion.
*/

/*
* This rule checks if the user satisfies the entry criterion for gout: having at least one gout episode. 
* If the user satisfies this criteria, it will assert the (possibleGout) fact which allows further diagnostic 
* questions to be asked.
*/
(defrule entryCriterion "entry criterion for the diagnosis of gout"
   (not (exists (ga-entryCriterion)))
   (goutep ?x)
=>
   (if (eq ?x "Y") then
      (assert (possibleGout))
   )
   (assert (ga-entryCriterion))
)

/*
* This rules checks if the user satisfies the sufficient criterion for gout: the presence of MSU crystals.
* If the user satisfies the criteria, he can be immediately diagnosed with gout by setting the gout score 
* to its maximum value, 23. Otherwise, the other diagnostic questions will be asked. 
*/
(defrule sufficientCriterion "sufficient criterion for the diagnosis of gout"
   (not (exists (ga-sufficientCriterion)))
   (possibleGout)
   ?ga <- (gout ?s)
   (msu ?x)
=>
   (retract ?ga)
   (if (eq ?x "Y") then
      (assert (gout 23))
   else
      (assert (gout 0))
      (assert (insufficientCriterion))
   )
   (assert (ga-sufficientCriterion))
)

/*
* This rule increases the gout likelihood score based on the joints involved.
* The criteria is as follows:
*     +2 points if the first metatarsophalangeal joint is involved
*     +1 point if either the ankle or the midfoot is involved
*/
(defrule jointInvolvement "alters gout score based on joint involvement"
   (not (exists (ga-jointInvolvement)))
   (possibleGout)
   (insufficientCriterion)
   ?ga <- (gout ?s)
   (anklemidfoot ?x)
   (firstmtp ?y)
=>
   (retract ?ga)
   (if (eq ?y "Y") then
      (assert (gout (+ ?s 2)))
   elif (eq ?x "Y") then
      (assert (gout (+ ?s 1)))
   else
      (assert (gout ?s))
   )
   (assert (ga-jointInvolvement))
)

/*
* This rule increases the gout likelihood score based on the characteristics of the symptoms.
* The criteria is as follows:
*     +X points based on the number of characteristics met
*        - Erythema (redness) over affected joint
*        - Joint is painful to touch/pressure
*        - Difficulty using joint 
*/
(defrule goutCharacteristics "alters gout score based on characteristic symptoms"
   (not (exists (ga-goutCharacteristics)))
   (possibleGout)
   (insufficientCriterion)
   ?ga <- (gout ?s)
   (goutchars ?x)
=>
   (retract ?ga) 
   (assert (gout (+ ?s ?x)))
   (assert (ga-goutCharacteristics))
)

/*
* This rule increases the gout likelihood score based on the number of gout episodes that the user has experienced.
* The criteria is as follows:
*     +2 points if the user has experienced more than 1 gout episode
*     +1 point if the user has experienced 1 gout episode
*/
(defrule goutEpisodes "alters gout score based on the number of gout episodes"
   (not (exists (ga-goutEpisodes)))
   (possibleGout)
   (insufficientCriterion)
   ?ga <- (gout ?s)
   (epnum ?x)
=>
   (retract ?ga)
   (if (> ?x 1) then
      (assert (gout (+ ?s 2)))
   elif (eq ?x 1) then
      (assert (gout (+ ?s 1)))
   else
      (assert (gout ?s))
   )
   (assert (ga-goutEpisodes))
)

/*
* This rule increases the gout likelihood score based on presence of tophi in the area of the affected joint.
* The criteria is as follows:
*     +4 points if there is evidence of tophi
*/
(defrule tophiPresence "alters gout score based on the presence of tophi"
   (not (exists (ga-tophiPresence)))
   (possibleGout)
   (insufficientCriterion)
   ?ga <- (gout ?s)
   (tophi ?x)
=>
   (retract ?ga)
   (if (eq ?x "Y") then
      (assert (gout (+ ?s 4)))
   else
      (assert (gout ?s))
   )
   (assert (ga-tophiPresence))
)

/*
* This rule increases the gout likelihood score based on the user's serum urate level.
* The criteria is as follows:
*     -4 points if the serum urate level is <4 mg/dl
*     +0 points if the serum urate level is >=4 mg/dl and <6 mg/dl
*     +2 points if the serum urate level is >=6 mg/dl and <8 mg/dl
*     +3 points if the serum urate level is >=8 mg/dl and <10 mg/dl
*     +1 points if the serum urate level is >=10 mg/dl
*/
(defrule serumUrateLevel "alters gout score based on the serum urate level"
   (not (exists (ga-serumUrateLevel)))
   (possibleGout)
   (insufficientCriterion)
   ?ga <- (gout ?s)
   (serumurate ?x)
=>
   (retract ?ga)
   (if (< ?x 4) then
      (assert (gout (- ?s 4)))
   elif (and (>= ?x 6) (< ?x 8)) then
      (assert (gout (+ ?s 2)))
   elif (and (>= ?x 8) (< ?x 10)) then
      (assert (gout (+ ?s 3)))
   elif (>= ?x 10) then
      (assert (gout (+ ?s 4)))
   else
      (assert (gout ?s))
   )
   (assert (ga-serumUrateLevel))
)

/*
* This rule increases the gout likelihood score based on the presence of MSU crystals in synovial fluid. 
* The criteria is as follows:
*     -2 points if there are no MSU crystals in synovial fluid
*/
(defrule synovialFluidAnalysis "alters gout score based on synovial fluid analysis"
   (not (exists (ga-synovialFluidAnalysis)))
   (possibleGout)
   (insufficientCriterion)
   ?ga <- (gout ?s)
   (msu ?x)
=>
   (retract ?ga)
   (if (eq ?x "N") then
      (assert (gout (- ?s 2)))
   else
      (assert (gout ?s))
   )
   (assert (ga-synovialFluidAnalysis))
)

/*
* This rule increases the gout likelihood score based on the presence of urate deposits.
* The criteria is as follows:
*     +4 points if urate deposits are present
*/
(defrule urateDepositionEvidence "alters gout score based on evidence of urate deposition"
   (not (exists (ga-urateDepositionEvidence)))
   (possibleGout)
   (insufficientCriterion)
   ?ga <- (gout ?s)
   (uratedeposition ?x)
=>
   (retract ?ga)
   (if (eq ?x "Y") then
      (assert (gout (+ ?s 4)))
   else
      (assert (gout ?s))
   )
   (assert (ga-urateDepositionEvidence))
)

/*
* This rule increases the gout likelihood score based on the presence of joint damage.
* The criteria is as follows:
*     +4 points if there is evidence of joint damage
*/
(defrule jointDamageEvidence "alters gout score based on evidence of joint damage"
   (not (exists (ga-jointDamageEvidence)))
   (possibleGout)
   (insufficientCriterion)
   ?ga <- (gout ?s)
   (jointdamage ?x)
=>
   (retract ?ga)
   (if (eq ?x "Y") then
      (assert (gout (+ ?s 4)))
   else
      (assert (gout ?s))
   )
   (assert (ga-jointDamageEvidence))
)

;-------------------------;
; Osteoarthritis Criteria ;
;-------------------------;
/*
* Since osteoarthritis does not have a ACR classification criteria, every characteristic will add 1 point 
* to the osteoarthritis likelihood score.
*/

/*
* This rule increases the osteoarthritis likelihood score by 1 if the distal joints are involved.
*/
(defrule affectedDIPJoints "increases osteoarthritis score based on affected DIP joints"
   (not (exists (oa-affectedDIPJoints)))
   ?oa <- (osteoarthritis ?s)
   (dipfoot ?x)
   (diphand ?y)
=>
   (retract ?oa)
   (if (or (> ?x 0) (> ?y 0)) then
      (assert (osteoarthritis (+ ?s 1)))
   else
      (assert (osteoarthritis ?s))
   )
   (assert (oa-affectedDIPJoints))
)

/*
* This rule increase the osteoarthritis likelihood score by 1 if the first carpometacarpal joint is involved.
*/
(defrule firstCMCInvolved "increases osteoarthritis score based on the involvement of the first CMC joint"
   (not (exists (oa-firstCMCInvolved)))
   ?oa <- (osteoarthritis ?s)
   (firstcmc ?x)
=>
   (retract ?oa)
   (if (eq ?x "Y") then
      (assert (osteoarthritis (+ ?s 1)))
   else
      (assert (osteoarthritis ?s))
   )
   (assert (oa-firstCMCInvolved))
)

/*
* This rule increases the osteoarthritis likelihood score by 1 if the first metatarsophalangeal joint is involved
*/
(defrule firstMTPInvolved "increases osteoarthritis score based on involvement of the first MTP joint"
   (not (exists (oa-firstMTPInvolved)))
   ?oa <- (osteoarthritis ?s)
   (firstmtp ?x)
=>
   (retract ?oa)
   (if (eq ?x "Y") then
      (assert (osteoarthritis (+ ?s 1)))
   else
      (assert (osteoarthritis ?s))
   )
   (assert (oa-firstMTPInvolved))
)

/*
* This rule increases the osteoarthritis likelihood score by 1 if the swelling of joints is hard and bony.
*/
(defrule bonyJointSwelling "increases osteoarthritis score based on joint swelling"
   (not (exists (oa-bonyJointSwelling)))
   ?oa <- (osteoarthritis ?s)
   (bonyswelling ?x)
=>
   (retract ?oa)
   (if (eq ?x "Y") then
      (assert (osteoarthritis (+ ?s 1)))
   else
      (assert (osteoarthritis ?s))
   )
   (assert (oa-bonyJointSwelling))
)

/*
* This rule increases the osteoarthritis likelihood score by 1 if joint pain increases after usage.
*/
(defrule jointUsagePain "increases osteoarthritis score based on evidence of joint usage pain"
   (not (exists (oa-jointUsagePain)))
   ?oa <- (osteoarthritis ?s)
   (usagepain ?x)
=>
   (retract ?oa)
   (if (eq ?x "Y") then
      (assert (osteoarthritis (+ ?s 1)))
   else
      (assert (osteoarthritis ?s))
   )
   (assert (oa-jointUsagePain))
)

/*
* This rule increases the osteoarthritis likelihood score by 1 if there is evidence of osteophytes.
*/
(defrule osteophytePresence "increases osteoarthritis score based on the presence of joint deformation"
   (not (exists (oa-osteophytePresence)))
   ?oa <- (osteoarthritis ?s)
   (osteophyte ?x)
=>
   (retract ?oa)
   (if (eq ?x "Y") then
      (assert (osteoarthritis (+ ?s 1)))
   else
      (assert (osteoarthritis ?s))
   )
   (assert (oa-osteophytePresence))
)

/*
* This rule increases the osteoarthritis likelihood score by 1 if the rheumatoid factor level is normal.
*/
(defrule rheumatoidFactorAbsence "increases osteoarthritis score based on normal levels of rheumatoid factors"
   (not (exists (oa-rheumatoidFactorAbsence)))
   ?oa <- (osteoarthritis ?s)
   (rf ?x)
=>
   (retract ?oa)
   (if (< ?x ?*RF_NORM*) then
      (assert (osteoarthritis (+ ?s 1)))
   else
      (assert (osteoarthritis ?s))
   )
   (assert (oa-rheumatoidFactorAbsence))
)

/*
* This rule increases the osteoarthritis likelihood score by 1 if both the ESR and CRP levels are normal.
*/
(defrule normalACR "increases osteoarthritis score based on normal levels of acute phase reactants"
   (not (exists (oa-normalACR)))
   ?oa <- (osteoarthritis ?s)
   (esr ?x)
   (crp ?y)
=>
   (retract ?oa)
   (if (and (< ?x ?*ESR_NORM*) (< ?y ?*CRP_NORM*)) then
      (assert (osteoarthritis (+ ?s 1)))
   else
      (assert (osteoarthritis ?s))
   )
   (assert (oa-normalACR))
)

/*
* This rule increases the osteoarthritis likelihood score by 1 if the affected joints are asymmetric.
*/
(defrule asymmetricJoints "increases osteoarthritis score if the affected joints are asymmetric"
   (not (exists (oa-asymmetricJoints)))
   ?oa <- (osteoarthritis ?s)
   (symmetry ?x)
=>
   (retract ?oa)
   (if (eq ?x "N") then
      (assert (osteoarthritis (+ ?s 1)))
   else
      (assert (osteoarthritis ?s))
   )
   (assert (oa-asymmetricJoints))
)

;---------------------;
; Arthritis Diagnosis ;
;---------------------;

/*
* This rule executes after all other rules that can be executed are executed. It diagnoses decides whether the user 
* has rheumatoid arthritis, gout, or arthritis based on the likelihood score of each arthritis. Both rheumatoid 
* arthritis and gout have specific approved criteria with rheumatoid arthritis having a maximum score of 10 and gout 
* having a maximum score of 23. In order for the user to be diagnosed with rheumatoid arthritis, they need a rheumatoid 
* arthritis score of 6 or higher. In order to be diagnosed with gout, the user must have a gout score of 8 or higher.
* Since osteoarthritis does not have a criteria, the machine will diagnose the user with osteoarthritis if the score 
* is more than half of the total possible score (>= 5 out of 9 total). If none of the thresholds are met, the rule  
* will print out a message stating that the user does not have any of the three arthritis.
*/
(defrule arthritisDiagnosis "diagnosis which types of arthritis that the user has"
   (declare (salience -100))
   (rheumatoid ?ra)
   (gout ?ga)
   (osteoarthritis ?oa)
=>
   (bind ?arthritisfree TRUE)
   (bind ?rheumatoidScoreLimit 6)
   (bind ?goutScoreLimit 8)
   (bind ?osteoarthritisScoreLimit 5)
   (if (>= ?ra ?rheumatoidScoreLimit) then
      (printout t "There is a high chance that you have rheumatoid arthritis." crlf)
      (bind ?arthritisfree FALSE)
   )
   (if (>= ?ga ?goutScoreLimit) then
      (printout t "There is a high chance that you have gout." crlf)
      (bind ?arthritisfree FALSE)
   )
   (if (>= ?oa ?osteoarthritisScoreLimit) then
      (printout t "There is a high chance that you have osteoarthritis." crlf)
      (bind ?arthritisfree FALSE)
   )
   (if (eq ?arthritisfree TRUE) then
      (printout t "Congratulations! You do not have rheumatoid arthritis, gout, or osteoarthritis." crlf)
   )
)

;------------------;
; Helper Functions ;
;------------------;

/*
* The getNumBtw function asks a prompt and only accepts integer responses between ?lower and ?upper. 
* Answering with an invalid input will cause the function to print an error message and ask the question again.
*/
(deffunction getNumBtw (?prompt ?lower ?upper)
   (bind ?ans (askQuestion ?prompt))
   (while (or (not (integerp ?ans)) (< ?ans ?lower) (> ?ans ?upper))
      (printout t (str-cat "Please enter an integer between " ?lower " and " ?upper ".") crlf)
      (bind ?ans (askQuestion ?prompt))
   )
   (return ?ans)
)

/*
* The getNumAbove function asks a prompt and only accepts integer response above ?lower.
* Answering with an invalid input will cause the function to print an error message and ask the question again.
*/
(deffunction getNumAbove (?prompt ?lower)
   (bind ?ans (askQuestion ?prompt))
   (while (or (not (integerp ?ans)) (< ?ans ?lower))
      (printout t (str-cat "Please enter an integer above " ?lower ".") crlf)
      (bind ?ans (askQuestion ?prompt))
   )
   (return ?ans)
)

/*
* The getResult function is used to get the results of the various lab tests. It only accepts floats 
* and the "?" character, which is used when the user has not taken the test or does not no know the result.
* Answering with an invalid input will cause the function to print an error message and ask the question again.
*/
(deffunction getResult (?prompt)
   (bind ?ans (askQuestion ?prompt))
   (while (not (or (eq (sub-string 1 1 ?ans) "?") (floatp ?ans)))
      (printout t "Please enter a valid float or ? if you do not know the answer." crlf)
      (bind ?ans (askQuestion ?prompt))
   )
   (return ?ans)
)

/*
* The getBool function is used in yes/no questions. It accepts "Yes (Y)," "No (N)," and "I don't know (?)."
* Answering with an invalid input will cause the function to print an error message and ask the question again.
*/
(deffunction getBool (?prompt)
  (bind ?ans (upcase (sub-string 1 1 (askQuestion (str-cat ?prompt " (Y/N/?)")))))
   (while (not (or (eq ?ans "Y") (eq ?ans "N") (eq ?ans "?")))
      (printout t "Please enter either Y, N, or ?." crlf)
      (bind ?ans (upcase (sub-string 1 1 (askQuestion ?prompt)))) 
   )
   (return ?ans)
)

/*
* Restarts the program by calling (clear) and (reset) and batches the arthritis.clp file again.
*/
(deffunction restart ()
   (reset)
   (clear)

   (batch programs/arthritis.clp)
   (return)
)

/*
* Starts the arthritis diagnosis and asserts the likelihood scores of each arthritis which defaults to 0.
*/
(deffunction start ()
   (reset)
   
   (assert (rheumatoid 0))
   (assert (gout 0))
   (assert (osteoarthritis 0))

   (run)
   (return)
)

(start)
