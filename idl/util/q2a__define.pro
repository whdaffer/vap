;+
; NAME:   q2a__define
; PURPOSE:   Defines and object of type q2a
;
; AUTHOR; William Daffer
;
; CATEGORY:   OO
;
; CALLING SEQUENCE:   q2a = obj_new('q2a',...)
; 
; METHODS:
;   Init:
;   Set:
;   Get:
;   Cleanup:
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.1.1.1  2001/11/30 23:57:08  vapuser
; Initial Checkin
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  

;============================================

FUNCTION q2a::Init,file, lon = lon, lat=lat
  status = 0
  self.lon = n_elements(lon) EQ 0 ? [0.,360.]:lon
  self.lat = n_elements(lat) EQ 0 ? [-90,90.]:lat
  ;;self.eqx =  !values.f_nan

  status = self->read(file)
  return,status
END


;============================================
; Cleanup
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO q2a::Cleanup
  ptr_free, self.data
END


;============================================
; Set Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

;PRO q2a::Set
;END


;============================================
; Get Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO q2a::Get, rev = rev, $
       lon=lon, $
       lat=lat, $
       sig0=sig0, $
       azimuth=azimuth, $
       flag=flag, $
       data=data, $
       file=file

   IF arg_present(rev) THEN rev = self.rev

   lon = (lat= (sig0= (flag= (azimuth = !values.f_nan))))

   IF arg_present(lon) AND $
      ptr_valid( self.data) THEN $
      lon = (*self.data).lon 

   IF arg_present(lat) AND $
      ptr_valid( self.data) THEN $
      lat = (*self.data).lat

   IF arg_present(sig0) AND $
      ptr_valid( self.data) THEN $
      sig0 = (*self.data).sig0 

   IF arg_present(flag) AND $
      ptr_valid( self.data) THEN $
      flag = (*self.data).flag


   IF arg_present(azimuth) AND $
      ptr_valid( self.data) THEN $
      azimuth = (*self.data).azimuth

   IF arg_present(data) AND $
      ptr_valid(self.data) THEN data = (*self.data)
   
   IF arg_present(file) THEN file = self.file


END

;=====================================================
;
;=====================================================
PRO q2a::getselect, $
       lon=lon, lat=lat, $
       sig0=sig0, azimuth=azimuth, $
       row=row, index=index, beam=beam, pos=pos, $
       land=land

   lon = (lat= (sig0= (flag= (azimuth = !values.f_nan))))
   IF ptr_valid( self.data) THEN BEGIN 
     goodmask =  2l^2 OR 2l^6 ; always check for measured and usable
     flag =  (*self.data).flag
     good = where( (flag AND goodmask ) EQ goodmask, ngood)
     IF ngood NE 0 THEN BEGIN 
       IF n_elements(beam) NE 0 THEN BEGIN
         beam = strupcase(beam)
         IF beam EQ 'INNER' THEN $
           good1 =  where((ishft(flag[good],-3) AND 1) EQ 0,ngood1) $
         ELSE $
           good1 =  where((ishft(flag[good],-3) AND 1),ngood1)
         IF ngood1 NE 0 THEN BEGIN 
           good = good[good1] 
           ngood = ngood1
         ENDIF ELSE BEGIN 
           ngood = 0
           good = -1
         ENDELSE 
       ENDIF 
       IF ngood NE 0 THEN BEGIN 
         IF n_elements(pos) NE 0 THEN BEGIN 
           pos = strupcase(pos)
           IF pos EQ 'FORE' THEN $
             good1 =  where((ishft(flag[good],-4) AND 1) EQ 0,ngood1) $
           ELSE $
             good1 =  where((ishft(flag[good],-4) AND 1),ngood1)
         ENDIF 
         IF ngood1 NE 0 THEN BEGIN 
           good = good[good1] 
           ngood = ngood1
         ENDIF ELSE BEGIN 
           ngood = 0
           good = -1
         ENDELSE 
       ENDIF 
       IF ngood NE 0 THEN BEGIN 
         IF keyword_set(land) eq 0 THEN BEGIN 
           good1 = where( (flag[good] AND 3 ) EQ 0, ngood1)
           IF ngood1 NE 0 THEN BEGIN 
             good = good[good1]
             ngood = ngood1
           ENDIF ELSE BEGIN 
             good = -1
             ngood = 0
           ENDELSE 
         ENDIF 
       ENDIF 
     ENDIF 
     IF ngood NE 0 THEN BEGIN 
       IF arg_present(lon) THEN lon = ((*self.data).lon)[good]
       IF arg_present(lat) THEN lat = ((*self.data).lat)[good]
       IF arg_present(sig0) THEN sig0 = ((*self.data).sig0)[good]
       IF arg_present(azimuth) THEN azimuth = ((*self.data).azimuth)[good]
       IF arg_present(index) THEN index = ishft(((*self.data).flag)[good],-8)
       IF arg_present(row) THEN BEGIN 
         unpack_where,flag,good,cc,rr
         rr = rr[uniq(rr,sort(rr))]
         row = ((*self.data).row)[rr]
       ENDIF 
     ENDIF 
  ENDIF 
END

;============================================
; Read
;============================================

FUNCTION q2a::read, file
  status = 0
  tfile = file[0]
  catch, error
  IF error NE 0 THEN BEGIN 
    message,!error_state.msg,/cont,/noname
    return,0
  ENDIF 

  IF NOT uncompress(tfile, recompress) THEN Message,"Can't uncompress " + file
  self.was_compressed =  recompress
  self.file = tfile
  basename =  basename(tfile)
  self.rev =  long(strmid(basename,6,5))

  ;attr =  hdfgetattr(tfile,attr= "EquatorCrossingLongitude")
  ;IF ptr_valid(attr.value) THEN self.eqx = float((*attr.value)[2])

  sd =  hdf_sd_start(tfile,/read)

  
  index =  hdf_sd_nametoindex(sd,'cell_lon')
  sds =  hdf_sd_select( sd, index)
  hdf_sd_getinfo, sds, caldata=cal
  hdf_sd_getdata, sds, data
  hdf_sd_endaccess, sds
  data =  data*cal.cal + cal.cal_err
  lon =  temporary(data)


  index =  hdf_sd_nametoindex(sd,'cell_lat')
  sds =  hdf_sd_select( sd, index)
  hdf_sd_getinfo, sds, caldata=cal
  hdf_sd_getdata, sds, data
  hdf_sd_endaccess, sds
  data =  data*cal.cal + cal.cal_err
  lat =  temporary(data)

  index =  hdf_sd_nametoindex(sd,'cell_index')
  sds =  hdf_sd_select( sd, index)
  hdf_sd_getinfo, sds, caldata=cal
  hdf_sd_getdata, sds, data
  hdf_sd_endaccess, sds
  cell_index =  temporary(data)

  west = self.lon[0] LT 0
  IF west THEN BEGIN 
    x = where(lon GT 180, nx )
    IF nx NE 0 THEN lon[x] =  lon[x] - 360
  ENDIF 

  
  x = where( cell_index GT 0 AND $
             lon GT self.lon[0] AND lon LE  self.lon[1] AND $
             lat GT self.lat[0] AND lat LE  self.lat[1], nx )

  IF nx GT 50 THEN BEGIN 
    unpack_where,lon,x,cc,rr
    rr = minmax(rr)
    start = rr[0]
    count = rr[1]-rr[0]+1

    lon = lon[*,rr[0]:rr[1]]
    lat = lat[*,rr[0]:rr[1]]
    cell_index =  cell_index[*,rr[0]:rr[1]]

    ncols = (size(lon,/dim))[0]


    index =  hdf_sd_nametoindex(sd,'sigma0')
    sds =  hdf_sd_select( sd, index)
    hdf_sd_getinfo, sds, caldata=cal
    hdf_sd_getdata, sds, data,start=[0,start], count=[ncols,count]
    hdf_sd_endaccess, sds
    data =  data*cal.cal + cal.cal_err
    sigma0 =  temporary(data)


    index =  hdf_sd_nametoindex(sd,'cell_azimuth')
    sds =  hdf_sd_select( sd, index)
    hdf_sd_getinfo, sds, caldata=cal
    hdf_sd_getdata, sds, data,start=[0,start], count=[ncols,count]
    hdf_sd_endaccess, sds
    data =  data*cal.cal + cal.cal_err
    azimuth =  temporary(data)

    index =  hdf_sd_nametoindex(sd,'sigma0_qual_flag')
    sds =  hdf_sd_select( sd, index)
    hdf_sd_getinfo, sds, caldata=cal
    hdf_sd_getdata, sds, data,start=[0,start], count=[ncols,count]
    hdf_sd_endaccess, sds
    qual =  temporary(data)

    index =  hdf_sd_nametoindex(sd,'sigma0_mode_flag')
    sds =  hdf_sd_select( sd, index)
    hdf_sd_getinfo, sds, caldata=cal
    hdf_sd_getdata, sds, data,start=[0,start], count=[ncols,count]
    hdf_sd_endaccess, sds
    mode =  temporary(data)

    index =  hdf_sd_nametoindex(sd,'surface_flag')
    sds =  hdf_sd_select( sd, index)
    hdf_sd_getinfo, sds, caldata=cal
    hdf_sd_getdata, sds, data,start=[0,start], count=[ncols,count]
    hdf_sd_endaccess, sds
    surface =  temporary(data)


    index =  hdf_sd_nametoindex(sd,'row_number')
    sds =  hdf_sd_select( sd, index)
    hdf_sd_getinfo, sds, caldata=cal
    hdf_sd_getdata, sds, data,start=[start], count=[count]
    hdf_sd_endaccess, sds
    row =  temporary(data)

    quasi_mode = ((mode AND 3) EQ 0) OR $
                  ishft( (mode AND 12), -1) OR $
                  ishft( ((mode AND 48) EQ 0), 3)

   flag =  ishft(uint(cell_index),8) OR $
           ishft((qual AND 1) EQ 0,6) OR $
           ishft(quasi_mode, 2) OR $
           (surface AND 3)

    
    data =  replicate({q2a_data}, count)
    data.lon  = temporary(lon)
    data.lat  = temporary(lat)
    data.sig0 = temporary(sigma0)
    data.azimuth = temporary(azimuth)
    data.flag = temporary(flag)
    data.row =  temporary(row)

    self.data =  ptr_new(data,/no_copy)    
    status = 1
  ENDIF ELSE BEGIN 
    Message,'No data in lon/lat range.',/info
  ENDELSE 
  hdf_sd_end, sd
  IF self.was_compressed THEN s = compress(self.file)

  return, status
END

;=====================================================
;
;=====================================================
FUNCTION q2a::selfcheck
  good = ptr_valid(self.data)
  return, good
END

;============================================
; Definition Routine
;============================================

PRO q2a__define
  junk = {q2a, $
          file: '', $
          was_compressed: 0, $
          rev: 0l, $
;          eqx: 0.0, $
          lon: fltarr(2), $
          lat: fltarr(2), $
          data: ptr_new() }
END
