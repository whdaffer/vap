;+
; NAME:  printenv
; $Id$
; PURPOSE:  print the value of input environmental variable 
; 
; AUTHOR: William Daffer
; 
; CATEGORY:  Utility
; 
; CALLING SEQUENCE: printenv,variable 
; 
; INPUTS:  variable: the environmental variable
;
; OPTIONAL INPUTS:  non
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  the value is printed to stdout
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  none
;
; PROCEDURE:  none
;
; EXAMPLE:  
;
; MODIFICATION LOG:
;
; $Log$
;
;Copyright (c) 1999, William Daffer
;-
;
; No Warranties!
;
PRO printenv, variable
   IF n_elements(variable) EQ 0 THEN print,''
   IF NOT isa(variable,/string) THEN print,''
   print,getenv(variable)
END
