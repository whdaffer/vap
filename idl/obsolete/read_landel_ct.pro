PRO read_landel_ct, red,green,blue
  openr,rlun, '$VAP_ROOT/land_elevations.ct',/get_lun,error=err
  IF err NE 0 THEN BEGIN 
    message,!err_string,/cont
    return
  ENDIF 
  spawn,'wc -l  $VAP_ROOT/land_elevations.ct',nlines
  nlines = nlines(0)
  tmp = bytarr(3,nlines)
  readf,rlun,tmp
  free_lun,rlun
  tmp = transpose(tmp)
  red   = tmp(*,0)
  green = tmp(*,1)
  blue  = tmp(*,2)
  ncolors = n_elements(red)

END
