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
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  
;
;
;
; RESTRICTIONS:  
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
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
                     EndTime=EndTime

  rcsid = "$Id$"

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,-1
  ENDIF 

  retstruct = -1
  t1 = systime(1)
  t0 = t1
  IF n_params() LT 1 THEN BEGIN 
    message,' Usage: retstruct=hdfqread(filename) '
    return, -1
  ENDIF 

  IF hdf_ishdf(filename) NE 0 THEN BEGIN 
    lf =  byte(10)
    sds_id = hdf_sd_start(filename,/read) 
    IF sds_id gt 0 THEN BEGIN 
      hdf_sd_fileinfo,sds_id,datasets,attributes
      ; print,'Number of SD data sets: ',datasets
      ; print,'Number of attributes:   ',attributes
      eqx = eqx_str(1)
      FOR ai =0,attributes-1 DO BEGIN 
        hdf_sd_attrinfo,sds_id,ai,name=name,type=type,count=count,data=data
        name = strupcase(name)
        tmp = str_sep( data, lf )
        tmp = tmp(where(strlen(tmp)))
        data =  tmp(n_elements(tmp)-1)
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

      IF exist( StartDate ) AND exist( StartTime) THEN BEGIN 
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


      r = hdf_sd_nametoindex(sds_id, 'wvc_lat')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, lat
      lat =  float(lat*cal.cal + cal.offset)

      retstruct  =  Q2B_STR( dims[1], ncells=dims[0] )

      retstruct.lat =  float(lat) &  lat=0


      r = hdf_sd_nametoindex(sds_id, 'wvc_row')
      r = hdf_sd_select( sds_id, r )
      
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, row
      row =  fix(row*cal.cal + cal.offset)
      

      retstruct.row =  fix(row)
      


      r = hdf_sd_nametoindex(sds_id, 'wvc_lon')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, lon
      lon =  float(lon*cal.cal + cal.offset)

        ; Make it East Longitude.
      bad = where( Lon LT 0, nbad)
      IF nbad NE 0 THEN Lon(bad) = lon(bad)+360.
        
      retstruct.lon =  lon &  lon=0

      r = hdf_sd_nametoindex(sds_id, 'wvc_quality_flag')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, qual
      qual =  fix(qual*cal.cal + cal.offset)

      retstruct.qual =  qual &  qual=0

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

      retstruct.sel =  sel

      r = hdf_sd_nametoindex(sds_id, 'wvc_index')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, idx
      idx =  fix(idx*cal.cal + cal.offset)

      retstruct.idx =  idx &  idx=0

      r = hdf_sd_nametoindex(sds_id, 'num_ambigs')
      r = hdf_sd_select( sds_id, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, nambig
      nambig =  fix(nambig*cal.cal + cal.offset)

      retstruct.nambig =  nambig


      r = hdf_sd_nametoindex(sds_id, 'wvc_row_time')
      IF r GE 0  THEN BEGIN 
        r = hdf_sd_select( sds_id, r )
        hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
        hdf_sd_getdata,r, rowtime
      ENDIF 


      r = hdf_sd_nametoindex(sds_id, 'wvc_row')
      IF r GE 0  THEN BEGIN 
        r = hdf_sd_select( sds_id, r )
        hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
        hdf_sd_getdata,r, row
      ENDIF 
      
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

      tsel = sel-1
      tnambig = nambig
      bad_sel = where( tsel EQ -1 OR tnambig LT 1, nx )
      tsel(bad_sel) =  0
      
      tsel = tsel + lindgen( s(2)*s(3) )*4
      su = u(tsel)
      sv = v(tsel)
      su(bad_sel) =  0.
      sv(bad_sel) =  0.

      retstruct.su =  temporary(su)
      retstruct.sv =  temporary(sv)
      retstruct.u = temporary(u)
      retstruct.v = temporary(v)
      IF exist(rowtime) THEN $
        retstruct.rowtime = temporary(rowtime)
      retstruct.row = temporary(row)

      t2 = systime(1)
      print,'Time to get selected vectors ',t2-t11
      t1 = t2

      retstruct.mu = 0.
      retstruct.mv = 0.

      print,'Time to load structure ',systime(1)-t1
      print,'Total time ', systime(1)-t0

      return, retstruct
    ENDIF ELSE message,!err_string,/cont
  ENDIF ELSE message,'Not an HDF file!',/cont

  return, -1
END






