PRO filechop,files
nf = n_elements(files)
mgdr_size =  12972l
rmgdr_size =  1236l

FOR i=0,nf-1 DO BEGIN
  openr,lun,files(i),/get_lun, error=err
  IF err EQ 0 THEN BEGIN
    message,' working on file ' + files(i),/cont
    ff =  fstat(lun)
    nrecs =  ff.size/mgdr_size
    mgdr =  bytarr( mgdr_size, nrecs)
    rmgdr =  bytarr( rmgdr_size, nrecs )
    readu,lun,mgdr
    rmgdr =  mgdr(0:rmgdr_size-1,*)

;    mgdr =  mgdr_str( nrecs )
;    rmgdr =  rmgdr_str(nrecs)
;    readu,lun,mgdr
    
;    rmgdr.time =  mgdr.time
;    rmgdr.rev =  mgdr.rev
;    rmgdr.lat =  mgdr.lat
;    rmgdr.lon =  mgdr.lon
;    rmgdr.col =  mgdr.col
;    rmgdr.wvcqualflag =  mgdr.wvcqualflag
;    rmgdr.mean_wind =  mgdr.mean_wind
;    rmgdr.nambig =  mgdr.nambig
;    rmgdr.wvc_sel =  mgdr.wvc_sel
;    rmgdr.windspd =  mgdr.windspd
;    rmgdr.errspd =  mgdr.errspd
;    rmgdr.winddir =  mgdr.winddir
;    rmgdr.errdir =  mgdr.errdir
;    rmgdr.mle_like =  mgdr.mle_like
;    rmgdr.l_wind_flg =  mgdr.l_wind_flg(0)
;    rmgdr.h_wind_flg =  mgdr.h_wind_flg(0)

    openw,lun2,files(i)+'.new',/get_lun,error=err
    IF err EQ 0 THEN $
      writeu,lun2,rmgdr $
    ELSE message, !err_string,/cont
    free_lun,lun,lun2
  ENDIF ELSE BEGIN
    message,!err_string,/cont
  ENDELSE 
ENDFOR 

return
END
