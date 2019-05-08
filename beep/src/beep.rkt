#lang racket
; https://dev.to/goober99/learn-racket-by-example-gui-programming-3epm

(require  racket/gui)

; Main Window
(define frame (new frame% [label "Bleep"]))

;scale used by slider
(define *min-position* 0)
(define *max-position* 2000)
;Range of frequencies
(define *min-frequency* 1)
(define *max-frequency* 19999)

;Notes -> frequency
;
(define notes (hash "A" 440.00
                    "B" 493.88
                    "C" 261.63
                    "D" 293.66
                    "E" 329.63
                    "F" 349.23
                    "G" 292.00))
(define (generate-tone button event)
  (system (format "beep -f ~a -l ~a"
                  (send frequency-field get-value)
                  (send duration-field get-value))))

; Logarithmic scale frequency, so that the slider is in the middle
(define min-freq (log *min-frequency*))
(define max-freq (log *max-frequency*))
(define frequency-scale (/ (- max-freq min-freq) (- *max-position* *min-position*)))

;;extend the textfield to validate data as a numeric
(define number-field%
  (class text-field%
    ;Add instance variables to define allowed range
    (init min-value max-value)
    (define min-allowed min-value)
    (define max-allowed max-value)
    (super-new)
    (define/override (on-focus on?)
      (unless on?
        (define current-value (string->number (send this get-value)))
        (unless (and (>= current-value min-allowed)
                     (<= current-value max-allowed))
        (send this set-value (~a min-allowed))
        ; reset slider position to make sure it still matches display
        (send slider set-value (string->number (send frequency-field get-value))))))))

; convert slider position to frequency
(define (position->frequency position)
  (inexact->exact (round
     (exp (+ min-freq (* frequency-scale (- position *min-position*)))))))
; convert frequency to slider position
(define (frequency->position freq)
  (inexact->exact (round
                     (/ (- (log freq) min-freq) (+ frequency-scale *min-position*)))))
;set the frequency of the slider and display
(define (set-frequency freq)
  (send slider set-value (frequency->position freq))
  (send frequency-field set-value(~a freq)))

; Link slider to text field display of frequency
; ~a converts a number to a string
; Nowdefine the callback function for the particular instance of the object, the slider
; and the frequency field
(define (adjust-frequency  widget event)
  (send frequency-field set-value
        (~a (position->frequency (send widget get-value)))))

(define (adjust-slider  entry event)
  (define new-freq (string->number (send entry get-value)))
  (send slider set-value
        (frequency->position (if new-freq new-freq *min-frequency*))))

;create a slider
(define slider (new slider% [label #f]
                            [min-value *min-position*]
                            [max-value *max-position*]
                            [parent frame]
                            [init-value (frequency->position 440)]
                            [style '(horizontal plain)]
                            [vert-margin 25]
                            [callback adjust-frequency]
                            [horiz-margin 10]))

; define a text field showing the current frequency

(define frequency-pane (new horizontal-pane% [parent frame]
                                             [border 10]
                                             [alignment '(center center)]))
(define lower-buttton (new button% [parent frequency-pane]
                                   [label "<"]))

(define frequency-field (new text-field% [label #f]
                                         [parent frequency-pane]
                                         [init-value "440"]
                                         [min-width 64]
                                         [callback adjust-slider]
                                         [stretchable-width #f]))
(define frequency-label (new message% [parent frequency-pane]
                                      [label "Hz"]))
(define higher-button (new button% [parent frequency-pane]
                                    [label ">"]))

; General Controls
(define control-pane (new horizontal-pane% [parent frame]
                                           [border 25]
                                           [spacing 25]))

(define duration-pane (new horizontal-pane% [parent control-pane]))

(define duration-field (new number-field% [label "Duration "]
                                          [parent duration-pane]
                                          [min-value 1]
                                          [max-value 600000] ; 10 minutes
                                          [init-value "200"]
                                          [min-width 120]))

(define play-button (new button% [parent control-pane]
                                 [label "Play"]
                                 [callback generate-tone]))

;set frequency to the specific note
(define (set-note choice event)
  (set-frequency (hash-ref notes(send choice get-string-selection))))

(define note (new choice% [label "♪"]
                         [choices '("A" "B" "C" "D" "E" "F" "G")]
                         [parent control-pane]
                         [callback set-note]))
;(define slider (new slider% [label #f]
;Diaplay window
(send frame show #t)
