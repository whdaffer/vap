;+
; NAME: plotlinestyles.pro
; $Id$
; PURPOSE: Given the idl linestyle, return a string useful in
;          annotating plots
;
;
; AUTHOR: William Daffer 
;
; CATEGORY:  plot annotation
;
; CALLING SEQUENCE:  linestylestring=plotlinestyles(linestyle)
; 
; INPUTS:  linestyle, a number between 0 and 5.
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
; OUTPUTS:  A string useful in annotation
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  plsnames, plsnames, plsnamesdef
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1999/04/08 00:06:57  vapuser
; Initial revision
;
;
;Copyright (c) 1995, William Daffer
;-
; No Warranties
;
FUNCTION Plotlinestyles, linestyle


COMMON plsnames, plsnames, plsnamesdef

IF n_elements( linestyle ) EQ 0 THEN linestyle = !p.linestyle
IF n_elements( plsnamesdef ) EQ 0 THEN BEGIN
  plsnames = [ 'solid', '...', '---', '-.', '-...', '__ __' ]
  plsnamesdef =1
ENDIF
RETURN, plsnames( linestyle )
END


