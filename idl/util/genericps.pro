;+
; NAME:  GenericPs
; $Id$
; PURPOSE:  Returns a 'ps' structure, suitable for use with PS_FORM
;
; AUTHOR:  William Daffer
;
; CATEGORY:  
;
; CALLING SEQUENCE:  ps_structure = genericPS()
; 
; INPUTS:  none
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  ps: a structure that can be used in a device,_extra=ps
;          call to setup postscript printing. This code is just a way
;          to create a generic postscipt plotting environment.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;William Daffer
;Copyright (c) 1999
;-
;
; No Warranties!
;
FUNCTION genericps

    ps= { $
         XSIZE           : 8.7, $
         XOFF            : 1.3,$
         YSIZE           : 6.5,$
         YOFF            : 10,$
         FILENAME        : 'generic.ps', $
         INCHES          : 1, $
         COLOR           : 1, $
         BITS_PER_PIXEL  : 8, $
         ENCAPSULATED    : 0, $
         LANDSCAPE       : 1 }
  return,ps

END
