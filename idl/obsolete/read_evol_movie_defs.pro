FUNCTION read_evol_movie_defs, desig
; return the evolution movie parameter structure that corresponds to the
; Region of interest (ROI) designator 'desig'
; Currently the only ROIs defined are NWPAC, NEPAC NPAC, NWATL and
; INDIAN

IF n_elements( desig ) EQ 0 THEN desig = 'nepac'
desig =  strupcase(desig)
OPENR, rlun,'$VAP_ROOT/evolution_movie_defs.dat',/get_lun, error=err
found =  0
IF err eq 0 THEN BEGIN 
  rec =  ''
  REPEAT BEGIN 
    readf, rlun, rec
    found =  (strpos( rec, desig ) NE -1 )
  ENDREP UNTIL found OR eof( rlun )
  IF eof(rlun)  AND NOT found THEN BEGIN 
    ret =  {desig:'ERRORNOSUCHROI'}
  ENDIF ELSE BEGIN 
    
    s =  execute( 'ret=' +  rec )
    IF NOT(s) THEN BEGIN 
      message,!err_string,/cont
      ret =  {desig:"ERRORINEXECUTE"}
    ENDIF 
      ; Since the minimum number of files is a function of the time
      ; increment (because some ROIs only make 1 interpolated field
      ; per day and some (e.g. NPAC) make 2, the field 'min_nfiles' as
      ; it appears in evol_movie_defs.dat has a string which refers to
      ; 'time_inc.' So we have rebuild the structure with the value in
      ; place of the reference

    s = execute( 'min_nfiles = ret.' + ret.min_nfiles ) ; get the value
    IF NOT(s) THEN BEGIN 
      message,!err_string,/cont
      ret = {desig:"ERRORINEXECUTE2"}
    ENDIF 

      ; rebuild the structure.
    tags =  tag_names( ret )
    ntags =  n_elements(tags)
    ss =  CREATE_STRUCT( tags(0), ret.(0) )
    FOR i=1,ntags-2 DO ss =  CREATE_STRUCT( ss, tags(i), ret.(i) )
    ss =  create_struct( ss,'min_nfiles',min_nfiles)
    ret = ss
  ENDELSE 
  free_lun, rlun
  
ENDIF ELSE BEGIN
  message,!err_string,/cont
  ret =  {desig:'ERROROPENFAILURE'}
ENDELSE 
RETURN, ret
END
