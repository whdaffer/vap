;+
; NAME:  dirs.pro
; $Id$
; PURPOSE:  part of dirs/pushd/popd unholy trinity
;
; AUTHOR:  Someone at RSI
;
; CATEGORY:  utilty
;
; CALLING SEQUENCE:dirs  
; 
; INPUTS:  none
;
; CALLING SEQUENCE:
;       DIRS
;
; OUTPUTS:
;       DIRS lists the contents of the directory stack on the default
;       output device.
;
; COMMON BLOCKS:
;       DIR_STACK:  Contains the stack.
;
; MODIFICATION HISTORY:
;
;       Mon Sep 22 14:26:36 1997, William Daffer <daffer@rainy>
;         Changed name to dirs and took out annoying error.
;
;       July, 1989, Written by AB, RSI.
;       
;
;-
; MODIFICATION HISTORY:
;
; $Log$
;
;
PRO dirs

COMMON DIR_STACK, DEPTH, STACK

on_error,2                      ;Return to caller if an error occurs
if (n_elements(DEPTH) eq 0) then depth = 0

CD, CURRENT=current
print, 'Current Directory: ', current


IF (DEPTH ne 0) THEN BEGIN
  PRINT, 'Directory Stack Contents:'
  for i = 0, DEPTH-1 do print,format='(I3,") ", A)', I, STACK(I)
ENDIF ELSE print,'Stack is empty'


end
