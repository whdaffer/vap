;+
; NAME:  Pseudo
; $Id$
; PURPOSE:  Set the Pseudo device. Report an error if you can't!
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Screen Utility
;
; CALLING SEQUENCE: pseudo 
; 
; INPUTS:  none
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  If successful: X device set to pseudo color. 
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
; Copyright (c) 1999, William Daffer
; No Warranties!
;-

PRO pseudo
  device,pseudo=8
  device,get_visual_name=name
  IF name NE 'PseudoColor' THEN print,"Can't set Pseudo=8! Visual is " + name
END
