;+
; NAME:  RegularizeVapTime
; $Id$
; PURPOSE:  Take a possibly incomplete vaptime and complete it
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  Qscat/Seawinds Vap Processing
;
;
;
; CALLING SEQUENCE:  regularized_vaptime=RegularizeVapTime(vaptime,
;                   Min=Min | Max=Max)
;
;
; 
; INPUTS:  
;
;   Vaptime: A string of the form yyyy/mm/dd(/hh/mi)
;
;
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:  
;
;     Min: (I) Flag, if set, set unspecified fields to 0.
;     Max: (I) Flag, if set, set unspecified fields to max.
;     Note, only on may be set at a time. Setting both is an ERROR!
;     If neither is set, Min is assumed.
;
;
;
; OUTPUTS:  
;
;   The regularized Vaptime with the unspecified fields set according
; to the min/max flags.
;
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  None
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
;
; $Log$
; Revision 1.1  1998/10/17 00:26:33  vapuser
; Initial revision
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION RegularizeVapTime, vaptimes, Min=Min, MAX=MAX

  IF n_params() NE 1 THEN BEGIN 
    Message,'Usage: regularizedVapTime=RegularizeVapTime( vaptimes, [Min=Min|Max=Max])',/cont
    return,0
  END

  mn = keyword_set(Min)
  mx = keyword_set(Max)
  IF mn AND mx THEN BEGIN 
    Message,'ERROR: Only one of Min/Max may be set',/cont
    return,0
  ENDIF ELSE BEGIN 
    IF NOT (mn OR mx) THEN mn = 1
  ENDELSE 
  nn = n_Elements(vaptimes)
  IF nn GT 1 THEN $
    regularizedVapTime = strarr(nn) ELSE $
    regularizedVapTime = ''
  FOR i=0,nn-1 DO BEGIN 
    tmp = strsplit(vaptimes[i],'/',/extract)
    nt = n_Elements(tmp)
    CASE nt OF 
      3: BEGIN 
        IF mn THEN BEGIN 
          min = '00'
          hour = '00'
        ENDIF ELSE BEGIN 
          min = '59'
          hour = '23'
        ENDELSE 
        regularizedVapTime[i] = vaptimes[i] + '/' + hour + '/' + min
      END
      4: BEGIN 
        IF mn THEN BEGIN 
          min = '00'
        ENDIF ELSE BEGIN 
          min = '59'
        ENDELSE 
        regularizedVapTime[i] = vaptimes[i] + '/' + min
      END
      ELSE: BEGIN 
        IF nt LT 3 THEN BEGIN 
          Message,'ERROR:, Vaptime should be minimally yyyy/mm/dd',/cont
          return,0
        ENDIF 
        regularizedVapTime[i] = vaptimes[i]
      END
    ENDCASE
  ENDFOR 
  return, regularizedVapTime
END
