FUNCTION qscat_get_oe, file
  IF n_params() LT 1 THEN return,0


  sd = hdf_sd_start(file)

  index =  hdf_sd_attrfind( sd, 'orbit_semi_major_axis')
  hdf_sd_attrinfo, sd, index, data=semi_major_axis
  tmp = strsplit(semi_major_axis, string(10b),/extract)
  semi_major_axis =  float(tmp[2])

  index =  hdf_sd_attrfind( sd, 'orbit_eccentricity')
  hdf_sd_attrinfo, sd, index, data=eccentricity
  tmp = strsplit(eccentricity, string(10b),/extract)
  eccentricity =  float(tmp[2])


  index =  hdf_sd_attrfind( sd, 'orbit_inclination')
  hdf_sd_attrinfo, sd, index, data=inclination
  tmp = strsplit(inclination, string(10b),/extract)
  inclination =  float(tmp[2])*!dtor

  index =  hdf_sd_attrfind( sd, 'rev_orbit_period')
  hdf_sd_attrinfo, sd, index, data=period
  tmp = strsplit(period, string(10b),/extract)
  period =  float(tmp[2])/60. ; in minutes.

  index =  hdf_sd_attrfind( sd, 'rev_number')
  hdf_sd_attrinfo, sd, index, data=rev
  tmp = strsplit(rev, string(10b),/extract)
  rev =  long(tmp[2])


  index =  hdf_sd_attrfind( sd, 'RangeBeginningDate')
  hdf_sd_attrinfo, sd, index, data=beginningdate
  tmp = strsplit(beginningdate, string(10b),/extract)
  beginningdate =  tmp[2]

  tmp = strsplit(beginningdate,'-',/extract)
  tmp2 =  doy2date(tmp[0], tmp[1])
  beginningdate =  strjoin([tmp[0],tmp2],'/')

  index =  hdf_sd_attrfind( sd, 'RangeBeginningTime')
  hdf_sd_attrinfo, sd, index, data=beginningtime
  tmp = strsplit(beginningtime, string(10b),/extract)
  beginningtime =  tmp[2]

  tmp = strsplit(beginningtime,":",/extract)
  tmp = strjoin(tmp,"/")
  begindt = vaptime2idldt(beginningdate + '/' + tmp)

  index =  hdf_sd_attrfind( sd, 'RangeEndingDate')
  hdf_sd_attrinfo, sd, index, data=endingdate
  tmp = strsplit(endingdate, string(10b),/extract)
  endingdate =  tmp[2]
  tmp = strsplit(endingdate,'-',/extract)
  tmp2 =  doy2date(tmp[0], tmp[1])
  endingdate =  strjoin([tmp[0],tmp2],'/')

  index =  hdf_sd_attrfind( sd, 'RangeEndingTime')
  hdf_sd_attrinfo, sd, index, data=endingtime
  tmp = strsplit(endingtime, string(10b),/extract)
  endingtime =  tmp[2]

  tmp = strsplit(endingtime,":",/extract)
  tmp = strjoin(tmp,"/")
  enddt = vaptime2idldt(endingdate + '/' + tmp)

  index =  hdf_sd_attrfind( sd, 'StartOrbitNumber')
  hdf_sd_attrinfo, sd, index, data=startorbitnumber
  tmp = strsplit(startorbitnumber, string(10b),/extract)
  startorbitnumber =  long(tmp[2])

  index =  hdf_sd_attrfind( sd, 'StopOrbitNumber')
  hdf_sd_attrinfo, sd, index, data=stoporbitnumber
  tmp = strsplit(stoporbitnumber, string(10b),/extract)
  stoporbitnumber =  long(tmp[2])


  index =  hdf_sd_attrfind( sd, 'EquatorCrossingDate')
  hdf_sd_attrinfo, sd, index, data=equatorcrossingdate
  tmp = strsplit(equatorcrossingdate, string(10b),/extract)
  equatorcrossingdate =  tmp[2]

  tmp = strsplit(equatorcrossingdate,'-',/extract)
  tmp2 =  doy2date(tmp[0], tmp[1])
  equatorcrossingdate =  strjoin([tmp[0],tmp2],'/')

  index =  hdf_sd_attrfind( sd, 'EquatorCrossingTime')
  hdf_sd_attrinfo, sd, index, data=equatorcrossingtime
  tmp = strsplit(equatorcrossingtime, string(10b),/extract)
  equatorcrossingtime =  tmp[2]
  tmp = strsplit(equatorcrossingtime,":",/extract)
  tmp = strjoin(tmp,"/")
  eqxdt =  vaptime2idldt(equatorcrossingdate + '/' + tmp)

  index =  hdf_sd_attrfind( sd, 'EquatorCrossingLongitude')
  hdf_sd_attrinfo, sd, index, data=equatorcrossinglongitude
  tmp = strsplit(equatorcrossinglongitude, string(10b),/extract)
  equatorcrossinglongitude =  float(tmp[2])*!dtor


  hdf_sd_end, sd

  
  oe = { startorbit: startorbitnumber, $
         stoporbit: stoporbitnumber, $
         rev: rev, $
         semimajor: semi_major_axis , $
         eccentricity :eccentricity ,$
         inclination  : inclination , $
         period : period, $
         begindt : begindt, $
         enddt : enddt, $
         eqxdt : eqxdt, $
         eqxlongitude : equatorcrossinglongitude}
         
  return, oe
END
