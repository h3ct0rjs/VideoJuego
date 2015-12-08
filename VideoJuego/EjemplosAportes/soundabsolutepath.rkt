#lang racket
(require 2htdp/image
         (only-in racket/gui/base play-sound))
;Documentation will be in english language just to improve our writing skills.
;From the library 2htdp/image if we're going to use another inside function you just
;need to delete the line (only-in racket/gui/base play-sound), but if you're
;going to use just play-sound inside function please it's not necesary to import all
;performance porpouse.

;ToDo:
; Make sounds function with boolean Argument, like this disparo #f , to stop playing the sound, and disparo #t to start playing the sound
; Test inside another function, and with the main program.

;Sound Functions.
(define (disparo) (play-sound "Sounds/shoot.wav" #t))  ;This is going to reproduct the file name, take care of the time of playing.
(define (invadercerca) (play-sound "Sounds/invcerca.wav" #t))  ;
(define (invaderlejos) (play-sound "Sounds/invlejos.wav" #t))  ;
(define (menu) (play-sound "Sounds/menu.wav" #t))              ;We need to cut this file to 10 seconds of reproduction.
;if we don't cut this file, playing sound won't stop until it finish, and it will delay the other functions.
(define (menus argumento) (play-sound "Sounds/menu.wav" argumento))
