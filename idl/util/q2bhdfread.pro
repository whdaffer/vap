;+
; NAME:  Q2bHdfRead
; $Id$
; PURPOSE:  Reads 1 HDF file containing Qscat data.
;
;
; AUTHOR;
;
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  data=q2bhdfread( filename $
;                         [, eqx=eqx [,StartTime=StartTime $
;                           [,EndTime=EndTime]]] )
;
;
; 
; INPUTS:  
;        filename: fully qualified file name
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;                     eqx : (O) output structure (see
;                           ~vapuser/idl/qscat/eqx_str.pro for
;                           details) eontaining information about the
;                           equator crossing time

;                     StartTime: (O) String 'yyyy/mon/day/hour/min'
;                                the start time of the data
;                     EndDate  : (O) String 'yyyy/mon/day/hour/min'
;                                then end time of the data.
;
;
;
; OUTPUTS:  
;
;    Success: 1 structure of type q2b. See ~vapuser/id/qscat/q2b_str.pro
;    Failure: a scalar -1.
;
;    One should always check that the variable type of the returned
;    data is 'STRUCTURE'. If it isn't, the call failed.
;
;
; OPTIONAL OUTPUTS:  the keywords eqx,StartTime and EndData are
;                   optional outputs.
;
; COMMON BLOCKS:  None
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
; Revision 1.12  1999/11/12 19:56:23  vapuser
; Added code to support new DIRTH selected vectors in the
; new L2B data product. (per Bryan's request)
;
; Revision 1.11  1999/08/23 17:44:52  vapuser
; Now reads VD wvc_row_time.
;
; Revision 1.10  1999/08/12 16:33:17  vapuser
; Fixed short int  problem in directions, added some
; keywords and such.
;
; Revision 1.9  1999/07/07 20:12:18  vapuser
; fixed a sign problem in Q2B Longitude.
; May have to revisit this when we upgrade to 5.2
;
; Revision 1.8  1998/11/06 15:19:57  vapuser
; Changed a message
;
; Revision 1.7  1998/10/29 22:33:57  vapuser
; added /Isfile to DeEnvVar call.
;
; Revision 1.6  1998/10/28 23:37:51  vapuser
; Adde verbose keyword and some print statements.
;
; Revision 1.5  1998/10/23 22:19:24  vapuser
; Use DeEnvVar to protect HDF_... code from itself.
;
; Revision 1.4  1998/10/22 21:32:56  vapuser
; Corrected definition of 'lf', protected some
; string only code
;
; Revision 1.3  1998/10/12 22:07:27  vapuser
; Changed StartDate in command line to StartTime, ditto End...
;
; Revision 1.2  1998/10/05 23:30:48  vapuser
; added wvc_row_time to list of parameters to be retrieved.
;
; Revision 1.1  1998/10/05 22:43:27  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION q2bhdfread, filename, $
                     eqx=eqx, $
                     StartTime=StartTime, $
                     EndTime=EndTime, $
                     Verbose=Verbose

  rcsid = "$Id$"

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,-1
  ENDIF 

  Verbose = keyword_set(verbose)

  retstruct = -1
  t1 = systime(1)
  t0 = t1
  IF n_params() LT 1 THEN BEGIN 
    message,' Usage: retstruct=q2bhdfqread(filename) '
    return, -1
  ENDIF 

  tfilename = DeEnvVar(filename,/isfile)
  IF hdf_ishdf(tfilename) NE 0 THEN BEGIN 
    lf =  string(10b)
    fileid = hdf_sd_start(tfilename,/read) 
    IF fileid gt 0 THEN BEGIN 
      hdf_sd_fileinfo,fileid,datasets,attributes
      ; print,'Number of SD data sets: ',datasets
      ; print,'Number of attributes:   ',attributes
      eqx = eqx_str(1)
      t1 = systime(1)
      FOR ai =0,attributes-1 DO BEGIN 
        hdf_sd_attrinfo,fileid,ai,name=name,type=type,count=count,data=data
        name = strupcase(name)
        ; print,'Working on ', name
        IF VarType(data) EQ 'STRING' THEN BEGIN 
          tmp = str_sep( data, lf )
          tmp = tmp(where(strlen(tmp)))
          data =  tmp(n_elements(tmp)-1)
        ENDIF 
        CASE name OF 
        'EQUATORCROSSINGDATE': eqx.date = data
        'EQUATORCROSSINGTIME': eqx.time = data
        'EQUATORCROSSINGLONGITUDE': eqx.lon = float(data)
        'RANGEBEGINNINGDATE': StartDate = data
        'RANGEBEGINNINGTIME':StartTime = data
        'RANGEENDINGDATE':   EndDate = data
        'RANGEENDINGTIME':   EndTime = data
        ELSE:
        ENDCASE

      ENDFOR
      IF verbose THEN $
        print,'Time to extract attributes: ', t1-systime(1)
      t1 = systime(1)
      IF exist( StartDate ) AND exist( StartTime) THEN BEGIN 
          ; Date has yyyy-DDD format
          ; Time has hh:mm:ss.ccc format
        tmp = str_sep(StartDate,'-')
        StartYear = tmp[0]
        StartDoy = fix(tmp[1])
        date = doy2date( fix(StartYear),StartDoy)
        Month = date[0]
        day = date[1]
        StartDate = StartYear+'/'+Month+'/'+day
        tmp = str_sep(strcompress(StartTime,/remove_all),':')
        StartDate = StartDate+'/'+tmp[0]+'/'+tmp[1]
        StartTime = temporary(StartDate)
      ENDIF ELSE StartTime = '0000/00/00/00/00'

      IF exist( EndDate ) AND exist( EndTime) THEN BEGIN 
        tmp = str_sep(EndDate,'-')
        EndYear = tmp[0]
        EndDoy = fix(tmp[1])
        date = doy2date( fix(EndYear),EndDoy)
        Month = date[0]
        day = date[1]
        EndDate = EndYear+'/'+Month+'/'+day
        tmp = str_sep(strcompress(EndTime,/remove_all),':')
        EndDate = EndDate+'/'+tmp[0]+'/'+tmp[1]
        EndTime = temporary(enddate)
      ENDIF ELSE EndTime = '0000/00/00/00/00'
      
      IF Verbose THEN $
        print, 'Time to get start/stop/creation time: ',systime(1)-t1
     t1  = systime(1)

      r = hdf_sd_nametoindex(fileid, 'wvc_lat')
      r = hdf_sd_select( fileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, lat
      hdf_sd_endaccess,r
      lat =  float(lat*cal.cal + cal.offset)

      retstruct  =  Q2B_STR( dims[1], ncells=dims[0] )

      retstruct.lat =  float(lat) &  lat=0
      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to extract wvc_lat: ', t2-t1
      t1 = systime(1)

      r = hdf_sd_nametoindex(fileid, 'wvc_row')
      r = hdf_sd_select( fileid, r )
      
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, row
      hdf_sd_endaccess,r
      retstruct.row =  temporary(fix(row*cal.cal + cal.offset))
      
      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to extract wvc_row: ', t2-t1
      t1 = systime(1)


      r = hdf_sd_nametoindex(fileid, 'wvc_lon')
      r = hdf_sd_select( fileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, lon
      hdf_sd_endaccess,r
      x = where(lon LT 0, nx )
      IF nx GT 0 THEN BEGIN 
        lon = long(lon)
        lon[x] = lon[x] + 2l^16-1
      ENDIF 
      lon =  float(lon*cal.cal + cal.offset)

      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to extract wvc_lon: ', t2-t1
      t1 = systime(1)

        ; Make it East Longitude.
      bad = where( Lon LT 0, nbad)
      IF nbad NE 0 THEN Lon(bad) = lon(bad)+360.
        
      retstruct.lon =  temporary(lon)

      r = hdf_sd_nametoindex(fileid, 'wvc_quality_flag')
      r = hdf_sd_select( fileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, qual
      hdf_sd_endaccess,r
      qual =  fix(qual*cal.cal + cal.offset)

      retstruct.qual = qual
      retstruct.rain_flag = ((ishft(temporary(qual),-12) AND 3 ) EQ 2)

      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to extract wvc_qual: ', t2-t1
      t1 = systime(1)


      r = hdf_sd_nametoindex(fileid, 'wind_speed')
      r = hdf_sd_select( fileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, speed
      hdf_sd_endaccess,r
      speed =  float(speed*cal.cal + cal.offset)
      
      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to extract wvc_speed: ', t2-t1
      t1 = systime(1)

      r = hdf_sd_nametoindex(fileid, 'wind_dir')
      r = hdf_sd_select( fileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, dir
      hdf_sd_endaccess,r
      x = where( dir LT 0, nx )
      IF nx NE 0 THEN BEGIN 
        dir = long(dir)
        dir[x] =  dir[x] + 2L^16-1
      ENDIF 
      dir =  float(dir*cal.cal + cal.offset)


      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to extract wvc_dir: ', t2-t1
      t1 = systime(1)

      r = hdf_sd_nametoindex(fileid, 'wvc_selection')
      r = hdf_sd_select( fileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, sel
      hdf_sd_endaccess,r
      sel =  fix(sel*cal.cal + cal.offset)

      retstruct.sel = sel
      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to extract wvc_selection: ', t2-t1
      t1 = systime(1)

      r = hdf_sd_nametoindex(fileid, 'wvc_index')
      r = hdf_sd_select( fileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, idx
      hdf_sd_endaccess,r
      idx =  fix(idx*cal.cal + cal.offset)

      retstruct.idx =  temporary(idx)
      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to extract wvc_index: ', t2-t1
      t1 = systime(1)

      r = hdf_sd_nametoindex(fileid, 'num_ambigs')
      r = hdf_sd_select( fileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, nambig
      hdf_sd_endaccess,r
      nambig =  fix(nambig*cal.cal + cal.offset)

      retstruct.nambig =  nambig
      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to extract num_ambigs: ', t2-t1
      t1 = systime(1)


      r = hdf_sd_nametoindex(fileid, 'wvc_row_time')
      IF r GE 0  THEN BEGIN 
        r = hdf_sd_select( fileid, r )
        hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
        hdf_sd_getdata,r, rowtime
        hdf_sd_endaccess,r
      ENDIF 

      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to extract wvc_row_time: ', t2-t1
      t1 = systime(1)


      r = hdf_sd_nametoindex(fileid, 'model_speed')
      r = hdf_sd_select( fileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, mspeed
      hdf_sd_endaccess,r
      mspeed =  float(mspeed*cal.cal + cal.offset)
      
      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to extract wvc_speed: ', t2-t1
      t1 = systime(1)

      r = hdf_sd_nametoindex(fileid, 'model_dir')
      r = hdf_sd_select( fileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, mdir
      hdf_sd_endaccess,r
      x = where(mdir LT 0, nx )
      IF nx NE 0 THEN BEGIN 
        mdir = long(mdir)
        mdir[x] =  mdir[x] + 2l^16-1
      ENDIF 
      mdir =  float(mdir*cal.cal + cal.offset)


      DIRTHFOUND = 0
      r1 = hdf_sd_nametoindex(fileid, 'wind_speed_selection')
      r2 = hdf_sd_nametoindex(fileid, 'wind_dir_selection')

      IF r1 GT 0 AND r2 GT 0 THEN BEGIN 
        DIRTHFOUND = 1
        r = hdf_sd_select( fileid, r1 )
        hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
        hdf_sd_getdata,r, dirth_speed
        hdf_sd_endaccess,r
        dirth_speed =  float(dirth_speed*cal.cal + cal.offset)

        t2 = systime(1)
        IF Verbose THEN $
          print,'Time to extract wvc_speed_selection: ', t2-t1
        t1 = systime(1)


        r = hdf_sd_select( fileid, r2 )
        hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
        hdf_sd_getdata,r, dirth_dir
        hdf_sd_endaccess,r
        x = where( dirth_dir LT 0, nx )
        IF nx NE 0 THEN BEGIN 
          dirth_dir = long(dirth_dir)
          dirth_dir[x] =  dirth_dir[x] + 2L^16-1
        ENDIF 
        dirth_dir =  float(dirth_dir*cal.cal + cal.offset)

        t2 = systime(1)
        IF Verbose THEN $
          print,'Time to extract wvc_dir_selection: ', t2-t1
        t1 = systime(1)
      ENDIF 

      HDF_SD_END, fileid

      t2 = systime(1)
      IF Verbose THEN $
        print, 'Time to Extract data from HDF file ',t2-t1
      t1 = t2

      NSCAT_GETUV, dir,speed,u,v
      t2 = systime(1)
      IF Verbose THEN $
        print,'Time in NSCAT_GETUV ',t2-t1


      NSCAT_GETUV, mdir,mspeed,mu,mv
      t2 = systime(1)
      IF Verbose THEN $
        print,'Time in NSCAT_GETUV ',t2-t1


      speed = 0
      dir = 0

      s = size( u )
      su = fltarr( s(2), s(3) )
      sv =  su

      t11 = systime(1)
      tsel = sel-1
      bad_sel = where( tsel EQ -1 OR nambig LT 1, nx )
      tsel(bad_sel) =  0
      
      tsel = tsel + lindgen( s(2)*s(3) )*4
      su = u(tsel)
      sv = v(tsel)
      su(bad_sel) =  0.
      sv(bad_sel) =  0.

      IF DIRTHFOUND THEN BEGIN 
        NSCAT_GETUV, dirth_dir,dirth_speed,su2,sv2
        t2 = systime(1)
        IF Verbose THEN $
         print,'Time in NSCAT_GETUV ',t2-t1
      ENDIF ELSE BEGIN 
        su2 = su
        sv2 = sv
      ENDELSE 




      retstruct.su =  temporary(su)
      retstruct.sv =  temporary(sv)
      retstruct.u = temporary(u)
      retstruct.v = temporary(v)
      retstruct.mu = temporary(mu)
      retstruct.mv = temporary(mv)
      retstruct.su2 = temporary(su2)
      retstruct.sv2 = temporary(sv2)

      IF exist(rowtime) THEN $
        retstruct.rowtime = temporary(rowtime)

      t2 = systime(1)
      IF Verbose THEN $
        print,'Time to get selected vectors ',t2-t11
      t1 = t2


      fileid = hdf_open(tfilename,/read) 
      vidx = hdf_vd_find(fileid,'wvc_row_time')
      vid = hdf_vd_attach(fileid,vidx)
      nn = hdf_vd_read(vid,row_time)
      retstruct.rowtime = string(row_time)

      hdf_vd_detach,vid
      hdf_close,fileid

      
      print,'Time to load structure ',systime(1)-t1
      print,'Total time ', systime(1)-t0

      return, retstruct
    ENDIF ELSE message,!err_string,/cont
  ENDIF ELSE message,'Not an HDF file!',/cont

  return, -1
END






