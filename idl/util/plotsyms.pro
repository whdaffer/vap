;+
; NAME: PlotSyms.pro  
; $Id$
; PURPOSE:  Given the value of psym, return a string suitable for plot
;          annotation
;
; AUTHOR:  William Daffer
;
; CATEGORY:  plot annotation
;
; CALLING SEQUENCE:  symstring=plotsyms(!psym)
; 
; INPUTS:  psym : the symbol you want an annotation string for 
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  A string for annotation
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:   pnames, psymnames, psymnamesdef
;
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1995, William Daffer
;-
;
; No Warranties!
;

FUNCTION Plotsyms, sym
COMMON pnames, psymnames, psymnamesdef
IF n_elements( sym ) EQ 0 THEN sym = !psym
IF n_elements( psymnamesdef ) EQ 0 THEN BEGIN 
  psymnames = [ 'no sym', '+', '*', '.', '!9V!X', '!4D!X', '!9B!X','!9X!X']
  psymnamesdef =1
ENDIF 
RETURN, psymnames( sym )
END
