;+
; NAME:  exist.pro
; $Id$
; PURPOSE:  check for existance of input item
;
; AUTHOR:  William Daffer
;
; CATEGORY:  General argument processing
;
; CALLING SEQUENCE:  result=exist(item)
; 
; INPUTS:  item : the thing whose existance we want to check
;
; OPTIONAL INPUTS:  none
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  1 if item exists, 0 if not
;
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS: none 
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
; PROCEDURE:  return, n_elements(item) ne 0
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1999, William Daffer
;-
FUNCTION exist, item
return, n_elements( item ) NE 0
END
