;+
; NAME:  ProfTimer
; $Id$
; PURPOSE:  Provide some minimal timing capabilities
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: ProfTimer,/init (at beginning) and 
;                   Proftimer, ET, UNIT (at end) 
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  init: Boolean. If set, record the current
;                     system time and store in a common
;
; OUTPUTS:  
;
;  et: the elapsed time. The units of this delta are given in...
;  Unit: The units ('Days', 'Hours', 'Minutes' or 'Seconds') 
;        of the elapsed time. 
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  proftimer.
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  none
;
; PROCEDURE:  
;
;   if init
;    store current time t0
;   else
;    calculate delta from t0, put in largest units and return.
;  end
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 2000, William Daffer
;-
; No warranties
PRO proftimer, et,unit, init=init
  COMMON proftimer, t0
  IF keyword_set(init) THEN BEGIN 
    t0 = systime(1)
  ENDIF ELSE BEGIN 
    et = systime(1)-t0
    CASE 1 OF 
      et GE 86400: BEGIN 
        et = et/86400
        unit = ' Days '
      END 
      et GE 3600: BEGIN 
        et = et/3600.
        unit = ' Hours '
      END 
      et GE 60:  BEGIN 
        et = et/60.
        unit = ' Minutes '
      END 
      ELSE: unit = ' Seconds '
    ENDCASE 
  ENDELSE 

END 
