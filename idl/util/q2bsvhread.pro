;+
; NAME:  Q2bsvhread
; $Id$
; PURPOSE:  Read a file of SVH Q2B data.
;
;
; AUTHOR; WHD
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
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION q2bsvhread, filename
  rcsid = "$Id$"
  retdata = 0
;  on_error, 1
  IF n_params() EQ 0 THEN $
    message,' Usage: data=q2bsvhread( filename )'

  openr,lun, filename,/get_lun, error=err
  IF err EQ  0 THEN BEGIN 
    ff = fstat(lun)
    data = Q2bSvh_Str(1)
    size = N_Tags(data,/length)
    nrecs = ff.size/size
    data = Replicate(data,nrecs)
    Readu, lun, data
    free_lun, lun

    nrecs = max(data.idx(0))
    ncells =  max(data.idx(1))
    IF ncells eq 76 THEN BEGIN 
      q = q2b_str( nrecs )
      Nscat_GetUV, data.dir, data.speed, u, v
      Nscat_GetUV, data.sdir, data.sspeed, su, sv
      Nscat_GetUV, data.mdir, data.mspeed, mu, mv

      FOR i=min(data.idx(1)),max(data.idx(1)) DO BEGIN 
        x = where( data.idx(1) EQ i, nx )
        IF nx NE 0 THEN BEGIN 
          tdata = data(x)

          q( tdata.idx(0) ).u     (*,i-1) = u(*,x)
          q( tdata.idx(0) ).v     (*,i-1) = v(*,x)
          q( tdata.idx(0) ).su    (  i-1) = su(x)    
          q( tdata.idx(0) ).sv    (  i-1) = sv(x)    
          q( tdata.idx(0) ).mu    (  i-1) = mu(x)    
          q( tdata.idx(0) ).mv    (  i-1) = mv(x)    
          q( tdata.idx(0) ).sel   (  i-1) = tdata.sel
          q( tdata.idx(0) ).qual  (  i-1) = tdata.qual
          q( tdata.idx(0) ).nambig(  i-1) = tdata.nambig
          q( tdata.idx(0) ).lon   (  i-1) = tdata.lon
          q( tdata.idx(0) ).lat   (  i-1) = tdata.lat

        ENDIF 
      ENDFOR 
      retdata = q
    ENDIF ELSE $
      Message,'Corrupted data, ncells != 76, filename: ' + filename,/cont
  ENDIF ELSE message,!err_string,/cont
  
  return, retdata
  
END
