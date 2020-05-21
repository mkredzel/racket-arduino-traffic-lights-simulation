#lang racket

(require "AsipMain.rkt" "AsipButtons.rkt")

;; ASIP PROGRAM FOR TRAFFIC LIGHTS

(define start 
  (λ ()
    (open-asip)
    (set-pin-mode! 13 INPUT_MODE)
    (for ((i (in-range 2 13)))
    (set-pin-mode! i OUTPUT_MODE))))

; DEFINITION OF PINS - GIVING NAMES TO IDENTIFY LED'S COLOUR

(define red1 2)
(define amber1 3)
(define green1 4)
(define red2 5)
(define amber2 6)
(define green2 7)
(define red3 8)
(define amber3 9)
(define green3 10)
(define pedestrianRed 11)
(define pedestrianGreen 12)

; DEFINITION OF STATES AND TRANSITIONS USING A FOR LOOP

(define state1 (λ ()
    (for ((i '(2 3 6 7 9 10 12)))
    (digital-write i LOW))
    (for ((i '(4 5 8 11)))
    (digital-write i HIGH))
    (sleep 7)))

(define transition1 (λ ()
    (for ((i '(2 4 6 7 9 10 12)))
    (digital-write i LOW))
    (for ((i '(3 5 8 11)))
    (digital-write i HIGH))
    (sleep 1)
    (for ((i '(3 4 7 9 10 12)))
    (digital-write i LOW))
    (for ((i '(2 5 6 8 11)))
    (digital-write i HIGH))
    (sleep 1)))

(define state2 (λ ()
    (for ((i '(3 4 5 6 9 10 12)))
    (digital-write i LOW))
    (for ((i '(2 7 8 11)))
    (digital-write i HIGH))
    (sleep 7)))

(define transition2 (λ ()
    (for ((i '(3 4 5 7 9 10 12)))
    (digital-write i LOW))
    (for ((i '(2 6 8 11)))
    (digital-write i HIGH))
    (sleep 1)
    (for ((i '(3 4 6 7 10 12)))
    (digital-write i LOW))
    (for ((i '(2 5 8 9 11)))
    (digital-write i HIGH))
    (sleep 1)))

(define state3 (λ ()
    (for ((i '(3 4 6 7 8 9 12)))
    (digital-write i LOW))
    (for ((i '(2 5 10 11)))
    (digital-write i HIGH))
    (sleep 7)))

(define transition3 (λ ()
    (for ((i '(4 6 7 9 10 12)))
    (digital-write i LOW))
    (for ((i '(2 3 5 8 11)))
    (digital-write i HIGH))
    (sleep 1)))

(define state4 (λ ()
    (for ((i '(3 4 6 7 9 10 11)))
    (digital-write i LOW))
    (for ((i '(2 5 8 12)))
    (digital-write i HIGH))
    (sleep 7)))


(define transitionP1 (λ ()
    (for ((i '(2 4 6 7 9 10 12)))
    (digital-write i LOW))
    (for ((i '(3 5 8 11)))
    (digital-write i HIGH))
    (sleep 1)))

(define transitionP2 (λ ()
    (for ((i '(3 4 7 9 10 12)))
    (digital-write i LOW))
    (for ((i '(2 5 6 8 11)))
    (digital-write i HIGH))
    (sleep 1)))

; EXTENSION FOR LAST SECONDS OF GREEN PEDESTRIAN LIGHT STARTING TO BLINK

(define blinkingstate4 (λ ()
    (for ((i '(3 4 6 7 9 10 11 12)))
    (digital-write i LOW))
    (for ((i '(2 5 8)))
    (digital-write i HIGH))                         
    (sleep 0.5)
    (for ((i '(3 4 6 7 9 10 11)))
    (digital-write i LOW))
    (for ((i '(2 5 8 12)))
    (digital-write i HIGH))
    (sleep 0.5))) 

(define blinkingout (λ ()
    (for ((i '(3 4 6 7 9 10 11 12)))
    (digital-write i LOW))
    (for ((i '(2 5 8)))
    (digital-write i HIGH))                         
    (sleep 0.5)))
                    
; LOOP WHICH IS USING CURRENT STATE AS AN INITIAL ONE AND IT'S BEING UPDATED DURING EVERY AMENDMENT OF A STATE

(define currentstate state1)

(define loop (λ ()
               (cond
                 ((equal? currentstate state1) (state1) (buttonCheck1))
                 ((equal? currentstate transition1) (transition1) (set! currentstate state2) (loop))
                 ((equal? currentstate state2) (state2) (set! currentstate transition2) (loop))
                 ((equal? currentstate transition2) (transition2) (set! currentstate state3) (loop))
                 ((equal? currentstate state3) (state3) (set! currentstate transition3) (loop))
                 ((equal? currentstate transition3) (transition3) (set! currentstate state1) (loop)))))

; BUTTON SETTINGS WITH INITIAL VARIABLE 0, CHANGES TO 1 WITH A PRESS AND IT'S BEING SET TO 0 AFTER EVERY USE

(define button_var 0)

(define buttonCheck1 (λ ()
                      (cond
                        ((equal? button_var 1)
                         (transitionP1)
                         (state4)
                         (blinkingstate4)
                         (blinkingstate4)
                         (blinkingstate4)
                         (blinkingout)
                         (transitionP2)
                         (set! currentstate transition1)
                         (set! button_var 0)
                         (loop))
                        (#t (set! currentstate transition1)(loop)))))

(on-button-pressed 13 (λ () (set! button_var 1)))


; NECESSARY STEPS TO RUN THE PROGRAM
(start)
(loop)