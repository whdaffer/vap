FUNCTION get_l2a, file, limits
  
  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    IF n_elements(sd) NE 0 THEN hdf_sd_end,sd
    message,!error_state.msg,/cont
    return,0
  ENDIF 

  IF n_params() LT 2 THEN BEGIN 
    Message,"Usage: data=get_l2a(file,limits)",/cont
    return,0
  ENDIF 

  stuff = ['cell_lat','cell_lon','cell_sigma0']

  recompress = 0
  IF uncompress(file,recompress) NE 1 THEN BEGIN 
    Message,"Couldn't uncompress " + file,/cont
    return,0
  ENDIF 

  sd =  hdf_sd_start( file,/read)
  IF sd GE 0 THEN BEGIN 
    r = hdf_sd_nametoindex(sd,'cell_lat')
    sdsid = hdf_sd_select(sd,r)
    hdf_sd_getinfo,sdsid,caldata=caldata
    hdf_sd_getdata,sdsid,lat
    hdf_sd_endaccess,sdsid
    lat = caldata.cal*lat+caldata.offset

    r = hdf_sd_nametoindex(sd,'cell_lon')
    sdsid = hdf_sd_select(sd,r)
    hdf_sd_getinfo,sdsid,caldata=caldata
    hdf_sd_getdata,sdsid,lon
    hdf_sd_endaccess,sdsid
    lon = caldata.cal*lon+caldata.offset

    x = where( lon GE limits[0] AND lon LE limits[2] AND $
               lat GE limits[1] AND lat LE limits[3], nx )
    IF nx NE 0 THEN BEGIN 
      unpack_where,lon,x,cc,rr
      rr = minmax(rr)
      start = rr[0]
      count = rr[1]-rr[0]+1

      r = hdf_sd_nametoindex(sd,'sigma0')
      sdsid = hdf_sd_select(sd,r)
      hdf_sd_getinfo,sdsid,caldata=caldata, dims=dims
      hdf_sd_getdata,sdsid,sigma0, start=[0,start], count=[dims[0],count]
      hdf_sd_endaccess,sdsid
      sigma0 = caldata.cal*sigma0+caldata.offset

      r = hdf_sd_nametoindex(sd,'sigma0_mode_flag')
      sdsid = hdf_sd_select(sd,r)
      hdf_sd_getinfo,sdsid,caldata=caldata, dims=dims
      hdf_sd_getdata,sdsid,mode, start=[0,start], count=[dims[0],count]
      hdf_sd_endaccess,sdsid

      r = hdf_sd_nametoindex(sd,'cell_index')
      sdsid = hdf_sd_select(sd,r)
      hdf_sd_getinfo,sdsid,caldata=caldata, dims=dims
      hdf_sd_getdata,sdsid,index, start=[0,start], count=[dims[0],count]
      hdf_sd_endaccess,sdsid

      r = hdf_sd_nametoindex(sd,'surface_flag')
      sdsid = hdf_sd_select(sd,r)
      hdf_sd_getinfo,sdsid,caldata=caldata, dims=dims
      hdf_sd_getdata,sdsid,surface, start=[0,start], count=[dims[0],count]
      hdf_sd_endaccess,sdsid

      hdf_sd_end, sd


      lon = lon[*,rr[0]:rr[1]]
      lat = lat[*,rr[0]:rr[1]]
      x = where(index,nx)
      index = 0;
      IF nx LT n_elements(sigma0) THEN BEGIN 
        lon = lon[x]
        lat = lat[x]
        sigma0 = sigma0[x]
        mode = mode[x]
        surface =  surface[x]
      ENDIF 
      junk =  {lon: 0., lat:0., sig0: 0., mode:0, surface:0}
      data =  replicate( junk, n_elements(sigma0) )
      data.lon = temporary(lon)
      data.lat = temporary(lat)
      data.sig0 = temporary(sigma0)
      data.mode = temporary(mode)
      data.surface = temporary(surface)
      
    ENDIF ELSE BEGIN 
      Message,"No data in proscribed limits",/cont
      print,"limits = ",limits
      return,0
    ENDELSE 

  ENDIF ELSE Message,"Can't open HDF interface to " + file

  IF recompress THEN BEGIN 
    lf = string(10b)
    IF compress(file) NE 1 THEN Message,"Couldn't compress " + lf + file,/info
  ENDIF 
  return, data
END 


