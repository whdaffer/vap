;+
; NAME:   eatime__define
; $Id$
; PURPOSE:   Defines and object of type eatime
;
; AUTHOR; William Daffer
;
;
; CATEGORY:   OO
;
; CALLING SEQUENCE:   eatime = obj_new('eatime',...)
; 
; INPUTS:  
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
; $Log$
; Revision 1.1  1999/10/06 23:15:04  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
;============================================

FUNCTION eatime::Init, fts = fts, _extra=_extra, codeA=codeA

  IF n_elements(fts) NE 0 THEN BEGIN 
    self.time.fts =  fts
    self.time.dt = (fts2idldt(self.time.fts))[0]
    tmp = (idldt2codea(self.time.dt))[0]
    self.time.codeA = tmp
  ENDIF 
  IF n_elements(codeA) NE 0 THEN BEGIN 
    IF isa(codeA,/string,/nonempty) THEN BEGIN 
      self.time.CodeA =  codeA
      self.time.dt = codeA2idldt(codeA)
      self.time.fts = idldt2fts(self.time.dt)
    ENDIF ELSE Message,'CodeA time must be non-empty string',/info
  ENDIF 

  return,1
END


;============================================
; Cleanup
;============================================

PRO eatime::Cleanup
END


;============================================
; Set
;============================================
PRO eatime::Set, vaptime = vaptime
  IF n_elements(vaptime) NE 0 THEN $
    self-> SetTime, vaptime2fts( vaptime )
END


;============================================
; SetTime Routine
;============================================

PRO eatime::SetTime,fts
  IF n_elements(fts) NE 0 THEN BEGIN 
    self.time.fts =  fts
    self.time.dt = (fts2idldt(self.time.fts))[0]
    tmp = (idldt2codea(self.time.dt))[0]
    self.time.codeA = tmp
    self.time.vaptime = fts2vaptime(self.time.fts)
  ENDIF 

END


;============================================
; Get Routine
;============================================

PRO eatime::GetTime, fts     = fts, $
                 dt      = dt, $
                 codea   = codea, $
                 vaptime = vaptime

   IF arg_present(fts) THEN $ 
     fts = self.time.fts
    

   IF arg_present(dt) THEN $ 
     dt = self.time.dt
    

   IF arg_present(CodeA) THEN $ 
     CodeA = self.time.CodeA

   IF arg_present(vaptime) THEN $ 
     vaptime = self.time.vaptime

END

;============================================
; 3 functions to get the 3 forms of time.
; 
;============================================

FUNCTION eatime::getFts
  return, self.time.fts
END

FUNCTION eatime::getCodeA
  return, self.time.CodeA
END

FUNCTION eatime::getDt
  return, self.time.dt
END

FUNCTION eatime::getVapTime
  return, self.time.VapTime
END


;============================================
; GetL1ATime 
;  Convert time to L1A Time which has form
;    yyyy-doy hh:mm:ss.ccc
;============================================
FUNCTION eatime::GetL1ATime
  dt_to_var,self.time.dt,year=year,hour=hour,minute=minute,sec=sec
  doy = day_OF_year(self.time.dt)
  l1atime = strtrim( year, 2) + '-' + $
    PadAndJustify(doy,3,pad='0',/right) + ' ' + $
      PadAndJustify(hour,2,pad='0',/right) + ':' + $
       PadAndJustify(minute,2,pad='0',/right) + ':' + $
        PadAndJustify(sec,2,pad='0',/right) + '.000'
  return,l1atime
end


;============================================
; L1ATime2FTS
;  Convert L1A Time to FTS time
;    yyyy-doy hh:mm:ss.ccc
;============================================
FUNCTION  eatime::L1ATime2FTS,L1ATime
  l1at = strtrim(l1atime,2)
  ;tt = str_sep(l1at,' ')
  tt = strsplit(l1at,' ',/extract)
  year = fix(strmid(TT[0],0,4))
  doy = fix(strmid(tt[0],5,3))
  ;tt = str_sep(tt[1],':')
  tt = strtrim(tt[1],':',/extract)
  hour = fix(tt[0])
  minute = fix(tt[1])
  second = fix(tt[2])
  date = doy2date(year,doy)
  month = fix(date[0])
  day = fix(date[1])
  dt = var_to_dt(year,month,day,hour,minute,second)
  fts = idldt2fts(dt)
  return,fts
end

;============================================
; Definition Routine
;============================================

PRO eatime__define
  junk = {TimeSTR,   $
          fts   : 0.0d,  $
          dt    : {idldt}, $
          CodeA : '', $
          VapTime: '' }
  junk = {eatime, time: {TimeSTR} }

END
