;+
; NAME:  
; $Id$
; PURPOSE:  
;
; AUTHOR:  
;
; CATEGORY:  
;
; CALLING SEQUENCE:  
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
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
PRO readpvout, file, u,v,lon,lat
  IF n_params() lt 5 THEN BEGIN 
    Usage,"readpvout,file,u,v,lon,lat"
    return
  ENDIF 
  IF NOT isa(file,/string,/nonempty) THEN BEGIN 
    Message,"Parameter 1 <FILE> must be NON-EMPTY STRING!",/cont
    return
  ENDIF 
  
  openr,lun, file, error=err,/get_lun
  IF err ne 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return
  ENDIF 
  
  nrecs =(x=(y=(a=0L)))
  WHILE NOT eof(lun) DO BEGIN 
    readu,lun,x,y,a
    nrecs =  nrecs + y
    ff = fstat(lun)
    skip =  4l*(4l*(x*y))
    point_lun, lun, ff.cur_ptr + skip
  ENDWHILE 
  u = (v=(lon=(lat=fltarr(x,nrecs))))
  point_lun, lun, 0
  ii = 0
  WHILE NOT eof(lun) DO BEGIN 
    readu,lun,x,y,a
    uu = (vv=(llon=(llat=fltarr(x,y))))
    readu,lun,uu,vv,llon,llat
    u[*,ii:ii+y-1] = temporary(uu)
    v[*,ii:ii+y-1] = temporary(vv)
    lon[*,ii:ii+y-1] = temporary(llon)
    lat[*,ii:ii+y-1] = temporary(llat)
    ii = ii+y
  ENDWHILE 
  free_lun, lun
END
