FUNCTION qscatorbit, oe, npts, lat=lat, lon=lon, ra=ra, time=time, $
                     rev=rev, startjd=startjd, eqxlon=eqxlon, $
                     eqxtime=eqxtime, full=full

  IF n_elements(rev) NE 0 THEN BEGIN 
    IF keyword_set(full) THEN BEGIN 
      qs =  qswath( rev.eqxlon )
      eqxlon = rev.eqxlon
      lon = qs[*,*,0]
      lat = qs[*,*,1]
      dim =  size(lon,/dim)
      ncols = dim[0]
      nrows = dim[1]
      nn =  nrows
      period = (rev.eqxjd-rev.startjd)*4
      IF n_elements(startjd) EQ 0 THEN startjd =  rev.startjd
      time1 =  startjd + findgen(nn)/(nn-1) * period
      dt =  mean(time1[1:*]-time1)

      dt = (findgen(ncols)/(ncols-1)*dt)#(replicate(1.,nrows))
      time1 =  replicate(1,ncols)#time1
      
      time =  time1 + dt
      time1 = (dt=0)
      R_earth =  make_array(/double,dim=[dim,3])
      FOR col=0L,ncols -1 DO BEGIN 
        tlon = reform(lon[col,*])
        tlat = reform(lat[col,*])
        ttime = reform(time[col,*])
        rr =  geocentric2equatorial(tlon,tlat,ttime)
        r_earth[col,*,*] =  rr
      ENDFOR 
      x = min(abs(lon - eqxlon),ii)
      eqxtime =  interpol( time[ii-2:ii+2], lon[ii-2:ii+2],eqxlon )

    ENDIF ELSE BEGIN 
      qs = qswathextent( rev.eqxlon)
      eqxlon = rev.eqxlon
      qs = reform(qs[1,*,*])
      lon = qs[*,0]
      lat = qs[*,1]
      qs = 0
      nn =  n_elements(lon)
      period = (rev.eqxjd-rev.startjd)*4
      IF n_elements(startjd) EQ 0 THEN startjd =  rev.startjd
      time =  startjd + findgen(nn)/(nn-1) * period
      r_earth =  geocentric2equatorial(lon,lat,time)
      x = min(abs(lon - eqxlon),ii)
      eqxtime =  interpol( time[ii-2:ii+2], lon[ii-2:ii+2],eqxlon )

    ENDELSE 

  ENDIF ELSE BEGIN 
    tt =  2*!pi/oe.period
    t0 =  (oe.begindt.julian-oe.eqxdt.julian )*24*60

    time =  findgen(npts)/(npts-1)*oe.period+t0
    theta =  time*tt

    rx  = rotation_matrix(1,!pi-oe.inclination)
    rz1 = rotation_matrix(3,-oe.eqxlongitude,/deg)
    gst = ut2gst(oe.eqxdt.julian)*2*!pi
    rz2 = rotation_matrix(3,gst)

    totrot = rz2##rz1##rx

    a = oe.semimajor
    X =  a*[ [cos(theta)], [sin(theta)],[theta*0]]

    r_earth =  totrot##X

    ra = atan(r_earth[*,1],r_earth[*,0])

    lat =  atan( r_earth[*,2],sqrt(total(r_earth[*,0:1]^2,2)))*!radeg

    rr = rz1##rx##X

    lon =  atan(rr[*,1], rr[*,0])*!radeg
    tt = 0
  ENDELSE 

  return, r_earth
END
