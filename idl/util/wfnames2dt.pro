;+
; NAME:  wfnames2dt
; $Id$
; PURPOSE:  Converts a vector Wind File names to a vector if IDLDT
;          structures (i.e. times)
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  times=wfnames2dt(filenames [,nscat=nscat)
;
;
; 
; INPUTS:  
;
;     filenames - vector of filenames
;
;
;
; OPTIONAL INPUTS:  none
;
;
;	
; KEYWORD PARAMETERS:  
;
;      nscat : flag, if set, expect nscat naming conventions,
;              i.e. Nyymmdd.Shhmm.Ehhmm instead of Hyyyymmdd.S....
;
;
;
; OUTPUTS:  Vector of IDLDT structures
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
; Revision 1.2  1998/10/17 00:26:22  vapuser
; Changed
;   retstruct[ff].start_time = var_to_dt( year,month,day,start_hour,start_min)
;      to
;   t = var_to_dt( year,month,day,start_hour,start_min)
;   retstruct[ff].start_time = t
; IDL not core dump anymore.
;
; Revision 1.1  1998/10/05 18:53:39  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION wfnames2dt, windfiles, nscat=nscat

  rcsid = "$Id$"
  nscat = keyword_set(nscat)

  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    message,!err_string,/cont
    return,-1
  ENDIF 

  IF n_params() EQ 0 THEN BEGIN 
    Message,'Usage: startstoptime_struct=wfnames2dt( windfiles )',/cont
    return,-1
  END

  nf = n_elements(windfiles)
  ff = 0
  ;junk = { name: '', start_time:{IDLDT}, end_time:{IDLDT} }
  ;retstruct =  replicate( junk, nf )
  retstruct =  wfdt_str( nf )
  baselen = 9
  yearlen = 4
  IF nscat THEN BEGIN 
    baselen = 7
    yearlen = 2
  ENDIF 
  FOR f=0,nf-1 DO BEGIN 
    file = windfiles[f]
    s = rstrpos( file,'/') + 1
    file = strmid( file, s, strlen( file )-s )
    tmp = str_sep( file, '.' )
    IF n_elements(tmp) eq 3 THEN BEGIN 
      basetime = strmid( tmp[0], strlen(tmp[0])-baselen+1, baselen-1 )
      start_hour = fix(strmid( tmp[1], 1, 2 ))
      start_min  = fix(strmid( tmp[1], 3, 2 ))
      end_hour   = fix(strmid( tmp[2], 1, 2 ))
      end_min    = fix(strmid( tmp[2], 3,2 ))
      year       = fix( strmid( basetime, 0, yearlen ) )
      month      = fix( strmid( basetime, 4, 2 ) )
      day        = fix( strmid( basetime, 6, 2 ) )
      IF nscat THEN year =  year + 1900
      retstruct[ff].name = windfiles[f]
      t = var_to_dt( year,month,day,start_hour,start_min)
      retstruct[ff].start_time = t
      t =var_to_dt( year,month,day,end_hour,end_min)
      retstruct[ff].end_time = t
      ff = ff+1
    END
  ENDFOR 

  retstruct =  retstruct[0:ff-1]

  return, retstruct
END

