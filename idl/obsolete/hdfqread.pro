FUNCTION hdfqread, filename,su1=su1,sv1=sv1

  t1 = systime(1)
  t0 = t1
  IF hdf_ishdf(filename) NE 0 THEN BEGIN 
  lf =  byte(10)
  IF n_params() LT 1 THEN BEGIN 
       message,' Usage: retstruct=hdfqread(filename) '
       return, -1
    ENDIF 
    sds_id = hdf_sd_start(filename,/read) 
    IF sds_id gt 0 THEN BEGIN 
      hdf_sd_fileinfo,sds_id,datasets,attributes
      print,'Number of SD data sets: ',datasets
      print,'Number of attributes:   ',attributes
      eqx = { date:'', time:'', long:0.}
      FOR ai =0,attributes-1 DO BEGIN 
        hdf_sd_attrinfo,sds_id,ai,name=name,type=type,count=count,data=data
        tmp = str_sep( data, lf )
        tmp = tmp(where(strlen(tmp)))
        data =  tmp(n_elements(tmp)-1)
        CASE name OF 
        'EquatorCrossingDate': eqx.date = data
        'EquatorCrossingTime': eqx.time = data
        'EquatorCrossingLongitude': eqx.long = float(data)
        ELSE:
        ENDCASE

      ENDFOR

      r = hdf_sd_nametoindex(sds_id, 'wvc_row')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, row
      row =  fix(row*cal.cal + cal.offset)
      

      r = hdf_sd_nametoindex(sds_id, 'wvc_lat')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, lat
      lat =  float(lat*cal.cal + cal.offset)

      r = hdf_sd_nametoindex(sds_id, 'wvc_lon')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, lon
      lon =  float(lon*cal.cal + cal.offset)

      r = hdf_sd_nametoindex(sds_id, 'wvc_quality_flag')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, qual
      qual =  fix(qual*cal.cal + cal.offset)

      r = hdf_sd_nametoindex(sds_id, 'wind_speed')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, speed
      speed =  float(speed*cal.cal + cal.offset)

      r = hdf_sd_nametoindex(sds_id, 'wind_dir')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, dir
      dir =  float(dir*cal.cal + cal.offset)

      r = hdf_sd_nametoindex(sds_id, 'wvc_selection')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, sel
      sel =  fix(sel*cal.cal + cal.offset)

      r = hdf_sd_nametoindex(sds_id, 'wvc_index')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, idx
      idx =  fix(idx*cal.cal + cal.offset)

      r = hdf_sd_nametoindex(sds_id, 'num_ambigs')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, namb
      namb =  fix(namb*cal.cal + cal.offset)

      t2 = systime(1)
      print, 'Time to Extract data from HDF file ',t2-t1
      t1 = t2
      NSCAT_GETUV, dir,speed,u,v
      t2 = systime(1)
      print,'Time in NSCAT_GETUV ',t2-t1

      speed = 0
      dir = 0
      s = size( u )

      su = fltarr( s(2), s(3) )
      sv =  su

      t11 = systime(1)
      WVCSEL, u,v,sel,namb,su,sv
      su1 = su*0.
      sv1 = sv*0.
      sel1 = sel-1
      nn = n_elements(sel(0,*))
      FOR i=0,75 DO BEGIN 
        t1 = systime(1)
        g = where( sel1(i,*) GE 1 AND namb(i,*) GT 0, ng )
        t2 = systime(1)
;          print,'To find G     :       ',t2-t1
        IF ng NE 0 THEN BEGIN 
          ix = indgen(ng)
          ix = ix + ng*ix
          s = reform(sel1(i,g))
          t3 = systime(1)
;          print,'  To reform s        :',t3-t2
;          t = reform(u(s,i,g))
          t4 = systime(1)
;          print,'  To reform u(s,i,g) :',t4-t3
           su1(i,g) = (u(s,i,g))(ix)
          t5 = systime(1)
;          print,'  To extract t(ix)   :',t5-t4
;          t = reform(v(s,i,g))
          t6 = systime(1)
;          print,'  To reform v(s,i,g) :',t6-t5
           sv1(i,g) = (v(s,i,g))(ix)
          t7 = systime(1)
;          print,'  To extract t(ix)(2):',t7-t6
        ENDIF 
      ENDFOR 


      t2 = systime(1)
      print,'Time to get selected vectors ',t2-t11
      t1 = t2
      mu = su*0.
      mv = sv*0.
      retstruct = { $
                   u    : u  ,$
                   v    : v  ,$
                   lon  : lon,$
                   lat  : lat,$
                   sel  : sel  ,$
                   sel2 : sel*0,$
                   idx  : idx  ,$
                   row  : row  ,$
                   qual : qual ,$
                   namb : namb ,$
                   eqx  : eqx       ,$
                   mu   : mu        ,$
                   mv   : mv        ,$
                   su   : su        ,$ ; Selected 'u'
                   sv   : sv   }    ; Selected 'v'

      print,'Time to load structure ',systime(1)-t1
      print,'Total time ', systime(1)-t0

      return, retstruct
    ENDIF ELSE message,!err_string,/cont
  ENDIF ELSE message,'Not an HDF file!',/cont

  return, -1
END






