PRO spinup_wind_field, qmodelfile, $
                       animpar=animpar, $
                       lonpar=lonpar,$
                       latpar=latpar,$
                       vlonpar=vlonpar,$
                       vlatpar=vlatpar, $
                       path_inc=path_inc

  t0 = systime(1)
  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg
    return
  END

  q = obj_new('qmodel',file=qmodelfile)
  IF NOT object_valid(q) THEN $
    Message,"Error initializing qmodel object!",/noname

  s = q-> getplotdata(u,v,lon,lat)
  IF n_elements(animpar) NE 3 THEN animpar = [800,60,60]
  IF n_elements(lonpar) NE 2 THEN lonpar =  minmax(lon)
  IF n_elements(latpar) NE 2 THEN latpar =  minmax(lat)

  IF n_elements(vlonpar) NE 3 THEN vlonpar= [lonpar,1]
  IF n_elements(vlatpar) NE 3 THEN vlatpar= [latpar,1]

  lonpar = fixlonrange(lonpar)
  IF lonpar[0] LT 0 AND lonpar[1] GT 0 THEN BEGIN 
    vlonpar =  fixlonrange(vlonpar[0:1])
  ENDIF 
  

  time = systime(1)-t0
  print, 'Done!'
  print, 'Process took ', time, ' seconds'

END

