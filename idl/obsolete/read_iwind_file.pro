FUNCTION read_iwind_file, file, uu,vv

status =  0 ; dress for failure
IF n_params() NE 3 THEN BEGIN 
  message,' Usage: read_iwind_field, filename, u, v ',/cont
  return, status
ENDIF 

IF vartype( file ) NE 'STRING' THEN BEGIN
  message,' Parameter 1 must be STRING',/cont
  return, status
ENDIF 


openr, lun, file,/get,error=err
IF err EQ 0 THEN BEGIN 
  uu =  fltarr( 360, 121 )
  vv =  uu
  readu, lun, uu,vv
  free_lun, lun
  status = 1
ENDIF ELSE message, !err_string,/cont

return, status
END
