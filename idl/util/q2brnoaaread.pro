;+
; NAME:  
; $Id$
; PURPOSE:  
;
;
; AUTHOR:
;
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  
;
;
; 
; INPUTS:  
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
; COMMON BLOCKS:  q2b_rnoaa_cmn 
;
;   Containing the items: 
;
;            q2b_rnoaa_nheader_recs - Number of header records
;            q2b_rnoaa_size         - size of record (it's real size)
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
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION q2bRnoaaRead, filename, $
                       StartTime=Starttime, $
                       EndTime=EndTime, $
                       Verbose=Verbose

COMMON q2b_rnoaa_cmn, q2b_rnoaa_nheader_recs, $
                      q2b_rnoaa_size


  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,-1
  ENDIF 

  Verbose = keyword_set(verbose)


  retstruct = -1
  t1 = systime(1)
  t0 = t1
  IF n_params() LT 1 THEN BEGIN 
    message,' Usage: retstruct=Q2BRnoaaRead(filename) '
    return, -1
  ENDIF 

  tfilename = DeEnvVar(filename,/isfile)

  openr, lun, tfilename, /get, error=err
  IF err NE 0 THEN BEGIN 

    ff = fstat(lun)
    nrecs = ff.size/q2b_rnoaa_size-q2b_rnoaa_nheader_recs
    point_lun, lun, q2b_rnoaa_size*q2b_rnoaa_nheader_recs
    retstruct = q2b_rnoaa_str(nrecs)
    readu,lun,retstruct

  ENDIF ELSE Message,!error_State.msg,/cont
  
  return, retstruct

END
