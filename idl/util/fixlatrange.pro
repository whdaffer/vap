;+
; NAME:  FixLatRange
; $Id$
; PURPOSE:  Assure the latitude is in range -90 to 90.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: fixedlatrange=fixlatrange(latrange) 
; 
; INPUTS:  latrange: 2-vector of floats. 
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
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
;
;Copyright (c) 1999, William Daffer
;-
; No Warranties!
;
FUNCTION FixLatRange, LatRange
    ; Returns a copy of the  input Latitude range that is assured of
    ; being >= -90 and <= 90.

    ; Assumes that LatRange(0) is the Southern terminus and
    ; LatRange(1) is the Northern termininus, i.e. LatRange(0) <=
    ; LatRange(1). It will swap them if this isn't true.
 
  FixedLatRange = LatRange
  IF FixedLatRange(0) GT FixedLatRange(1) THEN $
    FixedLatRange[ [0,1] ] =  FixedLatRange[ [1,0] ]
  IF abs(FixedLatRange(1)-FixedLatRange(0)) GT 180. THEN BEGIN 
    REPEAT BEGIN 
      b1 = where(FixedLatRange GT 90, nb1 )
      IF nb1 NE 0 THEN FixedLatRange(b1) = 90 - (FixedLatRange(b1) -90)
      b2 = where(FixedLatRange LT -90, nb2 )
      IF nb2 NE 0 THEN FixedLatRange(b2) = -90 + abs(FixedLatRange(b2) + 90)
    ENDREP UNTIL (nb1 + nb2) EQ 0
  ENDIF 
  Return, FixedLatRange

END
