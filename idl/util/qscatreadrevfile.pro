FUNCTION qscatreadrevfile, file

  IF n_params() LT 1 THEN return,0

  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    message,!error_state.msg,/cont
    IF n_elements(revs) NE 0 THEN BEGIN 
      x = where(revs.rev NE -1, nx )
      IF nx NE 0 THEN BEGIN 
        revs = revs[x]
        save,revs,file='qscat_revs.save'
        return, revs
      ENDIF ELSE return, 0
    ENDIF ELSE return,0
  ENDIF 

  openr,lun, file, /get,error=err
  IF err NE 0 THEN $
    message,!error_state.msg,/noprint
   
  nlines =  nlines(file)

  revs =  replicate( {qscat_rev_table},nlines)
  revs.rev =  -1l
  rec =  ''
  FOR l=0,nlines-1 DO BEGIN 
    readf, lun, rec
    rec = strcompress(rec)
    tmp = strsplit(rec,' ',/extract)
    revs[l].rev =  long(tmp[0])
    dt = ccsds2idldt(tmp[1])
    revs[l].startjd = dt.julian
    revs[l].eqxlon =  double(tmp[2])
    dt = ccsds2idldt(tmp[3])
    revs[l].eqxjd = dt.julian
  ENDFOR 
  free_lun, lun
  save,revs,file='qscat_revs.save'
  return,revs
END


  
  
