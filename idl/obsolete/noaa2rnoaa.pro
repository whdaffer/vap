PRO Noaa2Rnoaa, filename
  

COMMON q2b_rnoaa_cmn, q2b_rnoaa_nheader_recs, $
                      q2b_rnoaa_size, $
                      q2b_rnoaa_defined, $
                      q2b_rnoaa


COMMON q2b_noaa_cmn, q2b_noaa_nheader_recs, $
                      q2b_noaa_size, $
                      q2b_noaa_defined, $
                      q2b_noaa

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return
  ENDIF 
  IF N_elements(ncells) EQ 0 THEN ncells = 76

  IF NOT exist(q2bh_rnoaa_size) THEN q = q2b_rnoaa_str(1)
  IF NOT exist(q2bh_noaa_size) THEN q = q2b_noaa_str(1)

  q2b = -1
  t1 = systime(1)
  t0 = t1
  IF n_params() LT 1 THEN $
    message,' Usage: Noaa2Rnoaa, filename',/cont

  tfilename = DeEnvVar(filename,/isfile)

  openr, lun, tfilename, /get, error=err
  IF err EQ 0 THEN BEGIN 
    ff = fstat(lun)
    nrecs = ff.size/q2b_noaa_size-q2b_noaa_nheader_recs
    point_lun, lun, q2b_noaa_size*q2b_noaa_nheader_recs
    noaa = q2b_noaa_str(nrecs, ncells=ncells)
    readu,lun,noaa
    close,lun
    rnoaa = q2b_rnoaa_str(nrecs, ncells=ncells)
    openw, lun, tfilename + '.R'
    rnoaa.row_time     = noaa.row_time 
    rnoaa.rev          = noaa.rev      
    rnoaa.wvc_lon      = noaa.wvc_lon  
    rnoaa.wvc_lat      = noaa.wvc_lat  
    rnoaa.wvcqual_flag = noaa.wvcqual_flag
    rnoaa.model_speed  = noaa.model_speed
    rnoaa.model_dir    = noaa.model_dir
    rnoaa.nambig       = noaa.nambig
    rnoaa.windspd      = noaa.windspd
    rnoaa.winddir      = noaa.winddir
    rnoaa.errspd       = noaa.errspd
    rnoaa.errdir       = noaa.errdir   
    rnoaa.mle_like     = noaa.mle_like
    rnoaa.wvc_sel      = noaa.wvc_sel 

    noaa = 0
    writeu, lun, rnoaa
    free_lun, lun


  ENDIF ELSE Message,!error_state.msg,/cont

END

