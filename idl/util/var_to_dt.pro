;+
; NAME:
;      VAR_TO_DT
;
; $Id$
;
;
; PURPOSE:
;      Convert values representing date and time to an IDLDT date/time
;      variable.
;
;
; CATEGORY:
;      Misc
;
;
; CALLING SEQUENCE:
;      dtvar = VAR_TO_DT( yyyy, mm, dd, hh, min, ss)
;
; 
; INPUTS:
;
; OPTIONAL INPUTS:
;      yyyy: A scalar or array containing integer year(s).
;      mm:   A scalar or array containing integer month(s)
;      dd:   A scalar or array containing integer day(s) of the month.
;      hh:   A scalar or array containing integer hour(s) of the day.
;      min:  A scalar or array containing integer minute(s).
;      ss:   A scalar or array containing the float seconds.
;
;	
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;      VAR_TO_DT returns the IDLDT date/time structure containing the
;      converted date(s).
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;      NONE
;
;
; SIDE EFFECTS:
;      The result is a named IDLDT date/time structure.  If the
;      structure named IDLDT has not been defined, IDL invokes the
;      IDLDT__DEFINE procedure to create it.  IDLDT__DEFINE creates
;      a number of system variables.
;
;
;
; RESTRICTIONS:
;      If any of the inputs are arrays, all of the inputs, if present,
;      must be arrays of the same dimension.
;
;
;
; PROCEDURE:
;
;
; EXAMPLES:
;      date = VAR_TO_DT( 1997, 12, 21 ) ; Create an IDLDT with default
;                                       ; Values for hh, mm, ss.
;      PRINT, date
;
; The result is
;      { 1997 12 21 0 0.00000 0.0000000 0 }
;
;
;      date = VAR_TO_DT( [1997, 1998], [12, 1], [1, 1] ) 
;
; MODIFICATION HISTORY:
;
; 1.0 - Fixed a scalar .vs. 1-element array confusion below. WHD 15-oct-1998
;
; $Log$
;-

FUNCTION VAR_TO_DT, yyyy, mm, dd, hh, min, ss

rcsid = "$Id$"
nElements = n_elements( yyyy )

if nElements eq 0 then begin
   
    retVal = !dt_base
endif else if nElements eq 1 then begin
    retVal = {IDLDT}
    ; Defaults
    if n_elements( mm ) eq 0 then mm = 1B
    if n_elements( dd ) eq 0 then dd = 1B
    if n_elements( hh ) eq 0 then hh = 0B
    if n_elements( min ) eq 0 then min = 0B
    if n_elements( ss ) eq 0 then ss = 0.
    retVal = {IDLDT, yyyy[0], mm[0]<12>1, dd[0]<31>1, hh[0]<23>0, min[0]<59>0, $
              ss[0]<60>0, 0.d, 0b}
    retVal.Julian = julday( mm[0]<12>1, dd[0]<31>1, yyyy[0], hh[0]<23>0, $
                            min[0]<59>0, ss[0]<60>0 )
endif else begin
    ; Defaults
    if n_elements( mm ) eq 0 then mm = replicate( 1B, nElements )
    if n_elements( dd ) eq 0 then dd = replicate( 1B, nElements )
    if n_elements( hh ) eq 0 then hh = replicate( 0B, nElements )
    if n_elements( min ) eq 0 then min = replicate( 0B, nElements )
    if n_elements( ss ) eq 0 then ss = replicate( 0., nelements )
    retVal = replicate( {IDLDT}, nElements )
    for i = 0, nElements-1 do begin
        retVal[i] = VAR_TO_DT( yyyy[i], mm[i], dd[i], $
                               hh[i], min[i], ss[i] )
    endfor
endelse
return, retVal
end
