;+
; NAME:  fixlonrange
; $Id$
; PURPOSE:  Makes sure the input longitude range is that, in fact.
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  fixed_range= fixlonrange( lonrange)
;
;
; 
; INPUTS:  
;  Lonrange: a 2-vector with longitude arranged as [minlon, maxlon]
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;
;
; OUTPUTS:  
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;
;
; SIDE EFFECTS:  
;
;
;
; RESTRICTIONS:  
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION fixlonrange, lonrange, west=west
; Returns a lonrange assured of having lonrange(0) <= lonrange(1), so
; that lonrange(1)-lonrange(0) will correctly report the angular
; extent of the range. It handles all the evilness that arrises from
; the fact that longitude has a discontinuity at 0 or 180, depending
; on whether you're in east or west long. If the longitude range
; crosses the prime meridian, it puts it in West long, if it crosses
; 180, it puts it in east. If it crosses neither, it leaves it in
; whatever it was in.  It's assumed that lonrange(0) is supposed to be
; the min and lonrange(1) is supposed to be the max.
;
;
; Note that it DOES NOT change the input longitude range but returns a
; copy that is fixed.

; start out with east long

  rcsid = "$Id$"
  FixedLonRange = LonRange
  REPEAT BEGIN 
    x =  where( FixedLonRange LT 0, nx )
    IF nx NE 0 THEN FixedLonRange(x) =  FixedLonRange(x) + 360.
  ENDREP UNTIL nx EQ 0 

  REPEAT BEGIN 
    x =  where( FixedLonRange gt 360, nx )
    IF nx NE 0 THEN FixedLonRange(x) =  FixedLonRange(x) - 360.
  ENDREP UNTIL nx EQ 0 

  ; check to see if the max is < min
  IF FixedLonRange(0) GT FixedLonRange(1) THEN BEGIN 
    ; the choses are 
    ;   switch the two, i.e. FixedLonRange([1,0]) = FixedLonRange([0,1])
    ;     tantamount to the assumption that they weren't sorted when they
    ;     when they came in
    ;   or assume that the region crosses the prime meridian, so we
    ;   should put it all in West longitude. i.e. 
    ;   FixedLonRange(0) = FixedLonRange(0) - 360. this is what I'll do.

    FixedLonRange(0) = FixedLonRange(0) - 360. 
  ENDIF 

  junk = where(FixedLonRange LE 0, njunk )
  west =  njunk NE 0 
  Return, FixedLonRange

END






