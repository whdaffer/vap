;+
; NAME:  chechvaptime
; $Id$
; PURPOSE:  Check the validity of a Vaptime
;
; AUTHOR:  whd
;
; CATEGORY:  Vap Utility
;
; CALLING SEQUENCE:  0|1 = checkVaptime(vaptime)
; 
; INPUTS:  vaptime: A scalar string. 
;          A vaptime is a string of the form 
;          "yyyy/mm/dd/hh/mm[/ss]"
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  0: the input thing is either not a string or not a vaptime
;           1: the input thing is a vaptime.
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
;Jet Propulsion Laboratory
;Copyright (c) 2000, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
FUNCTION checkVaptime, vaptime

  test = 0
  IF n_Params() EQ 1 THEN BEGIN 
    IF isa(vaptime,/string,/nonempty) THEN BEGIN 
        ; A vaptime is a string of the form yyyy/mm/dd/hh/mm[/ss]
      tmp = str_sep(vaptime,'/')
      IF n_elements(tmp) GE 5 THEN BEGIN 
        ss = strlen(tmp)
        ss0 = ss[0]
        ss = ss[1:*]
        ss = ss[uniq(ss,sort(ss))]
        IF ss0 EQ 4 AND $
           n_elements(ss) EQ 1 AND $
           ss[0] EQ 2 THEN BEGIN 
          !quiet = 1L
          tmp = fix(tmp)
          !quiet = 0
          IF !Error_state.code EQ 0 THEN BEGIN 
            IF tmp[0] GT 0 AND $
               tmp[1] GE 1 AND tmp[1] LE 12 AND $ ; month
               tmp[2] GE 1 AND tmp[2] LE 31 AND $ ; day
               tmp[3] GE 0 AND tmp[3] LE 23 AND $ ; hour
               tmp[4] GE 0 AND tmp[4] LE 59 THEN BEGIN ; min
              IF n_elements(tmp) EQ 6 THEN BEGIN 
                IF tmp[5] GE 0 AND tmp[5] LE 59 THEN test = 1 ;sec
              ENDIF ELSE test = 1
            ENDIF 
          ENDIF 
        ENDIF 
      ENDIF 
    ENDIF 
  ENDIF 
  return, test
END
