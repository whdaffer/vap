PRO qmodelvelfield, filename, u,v,lon,lat,s,lonrange=lonrange, latrange=latrange, minspeed=minspeed,maxspeed=maxspeed
  IF n_params() LT 1 THEN BEGIN 
    Usage,"qmodelvelfield,filename [,lonrange,latrange,minspeed=minspeed,maxspeed=maxspeed]"
  ENDIF

  IF NOT isa(filename,/string,/nonempty) THEN BEGIN 
    Message,"FILENAME must be non-empty string",/cont
    return
  ENDIF 

  
  q = obj_new('qmodel',file=filename)
  IF NOT obj_valid(q) THEN BEGIN 
    Message,!error_state.msg + " Can't Read File " + filename,/cont
    return
  ENDIF 

  s = q-> getplotdata(u,v,lon,lat)
  obj_destroy,q

  nn = n_elements(lonrange)
  IF nn GT 0 THEN BEGIN 
    IF nn NE 2 THEN BEGIN 
      Message,"lonrange must be 2-vector",/cont
      return
    ENDIF 
  ENDIF ELSE lonrange = [0,360]
  nn = n_elements(latrange)
  IF nn GT 0 THEN BEGIN 
    IF nn NE 2 THEN BEGIN 
      Message,"latrange must be 2-vector",/cont
      return
    ENDIF 
  ENDIF ELSE latrange = [-90,90]

  IF n_elements(minspeed) EQ 0 THEN minspeed = 1
  IF n_elements(maxspeed) EQ 0 THEN maxspeed = 30

  s = minspeed> sqrt(u^2+v^2) < maxspeed


  ctfile = '$VAP_COLORTABLES/vap-animation.ct'
  ptc = ReadColorTable(ctfile)
  ct = *ptc &  ptr_free,ptc

  
  bottom = 1
  ncolors = 29

  savedevice = !d.name
  set_plot,'x'
  device,pseudo=8
  window,colors=51,title='Velocity field for ' + filename
  tvlct,transpose(ct)


  minv = minspeed
  maxv = maxspeed

  loncent = mean(lonrange)
  map_set,0,loncent,lim=[latrange[0],lonrange[0],latrange[1],lonrange[1]]
  contour,s,lon,lat,$
     levels=findgen(ncolors)/(ncolors-1)*(maxv-minv)+minv,$
     c_colors=findgen(ncolors)+bottom,/cell_fill,/overplot
  map_continents,/fill,color=!d.n_colors-1

END

    
