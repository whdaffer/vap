; Mods.
;
; $Log$
;
;
PRO svh2hdf, files, date=date
  rcsid = "$Id$"

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_State.msg,/cont
    return
  ENDIF 

  IF n_params() EQ 0 THEN BEGIN 
    message,'Usage: svh2hdf, files ',/cont
    return
  ENDIF 

  Extensions = [ $
               "S0000.E0140",$
               "S0141.E0321",$
               "S0322.E0503",$
               "S0504.E0643",$
               "S0644.E0825",$
               "S0826.E1006",$
               "S1007.E1147",$
               "S1148.E1327",$
               "S1328.E1508",$
               "S1509.E1650",$
               "S1651.E1830",$
               "S1831.E2012",$
               "S2013.E2153",$
               "S2153.E2359"] ;

  StartTimes = [ $
               '00/00',$
               '01/41',$
               '03/22',$
               '05/04',$
               '06/44',$
               '08/26',$
               '10/07',$
               '11/48',$
               '13/28',$
               '15/09',$
               '16/51',$
               '18/31',$
               '20/13',$
               '21/53'] ;

  EndTimes = [$
               '01/40',$
               '03/21',$
               '05/03',$
               '06/43',$
               '08/25',$
               '10/06',$
               '11/47',$
               '13/27',$
               '15/08',$
               '16/50',$
               '18/30',$
               '20/12',$
               '21/53',$
               '23/59'] ;



  IF n_elements(date) EQ 0 THEN date = strmid(TodayAsString(), 0,8)
  vdate = strmid( date, 0,4 ) + '/' + $
           strmid( date, 4,2 ) + '/' + $
            strmid( date, 6,2 )

  basename =  'QS' + date + '.'

  year  =  fix(strmid( date, 0, 4 ))
  month =  fix(strmid( date,4,2))
  day   =  fix(strmid(date,6,2))
  doy = date2doy(year,month,day)

  nf = n_elements(files)
  Nn = n_Elements(extensions)

  caldata = {cal:1.0d, cal_err: 0.0d, offset:0.0d, offset_err:0.0d, Num_Type:0l }
  caldata.Num_Type = IdlType2HdfType('FLOAT')

  FOR n=0,nn-1 DO BEGIN 
    
    tmp = str_sep( StartTimes[n], '/')
    start_hour = tmp[0]
    start_min = tmp[1]

    tmp = str_sep( EndTimes[n], '/')
    end_hour = tmp[0]
    end_min = tmp[1]

    file = files[n MOD nf]
    print,' reading file ' + file
    q = obj_new('q2b',file=file )
    IF obj_valid(q) THEN BEGIN 

      q-> GetAll,U=u,v=v,lon=lon,lat=lat,sel=sel,row=row,$
          idx=idx,rtime=row_time,nambig=nambig, Qual=qual

      obj_destroy,q
      UV2DirSpeed,u,v,Dir,Speed

      u = 0
      v = 0

      LongName = "HDF_FROM_SVH_DATA"
      ShortName =  "QSCATL2B"
      version = "<no version>"

          ; Date has yyyy-DDD format
          ; Time has hh:mm:ss.ccc format

      
      StartTime = vdate + '/'+ starttimes[n]
      EndTime = vdate + '/' + endtimes[n]
      st = vaptime2idldt( startTime )
      et = vaptime2idldt( endTime )
      diff = (et.Julian-st.Julian)*12.
      diff_hours = fix(diff)
      diff_mins = fix((diff-diff_hours)*60)
      Eqx = dt_add(st, hour=diff_hours, min=diff_mins)
      dt_to_var,Eqx,hour=xhour,min=xmin
      IF xhour LT 10 THEN $
        xhour = '0'+strtrim(xhour,2) ELSE $
        xhour =  strtrim(xhour,2) 
      IF xmin LT 10 THEN $
        xmin = '0'+strtrim(xmin,2) ELSE $
        xmin =  strtrim(xmin,2) 
      
      secs =  ':00.000'
      hdfdate = strtrim( year,2 ) + '-' +strtrim(doy,2 )
      EQUATORCROSSINGDATE = hdfdate
      EQUATORCROSSINGTIME = xhour+':'+xmin+secs
      EQUATORCROSSINGLONGITUDE =  0.0
      RANGEBEGINNINGDATE = hdfdate
      RANGEBEGINNINGTIME = start_hour+':'+start_min+secs
      RANGEENDINGDATE =  hdfdate
      RANGEENDINGTIME =  end_hour+':'+end_min+secs

      ofilename =  basename + Extensions[n]


      catch,/cancel

      catch, error
      IF error NE 0 THEN BEGIN 
        Message,!error_State.msg,/cont
        ; Message,'Name = ' + !error_State.name,/cont
        IF !error_State.name EQ 'IDL_M_HDF_SD_START' THEN BEGIN 
          CATCH,/cancel
          catch, error
          IF error NE 0 THEN BEGIN 
            Message,!error_State.msg,/cont
            return
          ENDIF 
          GOTO, Try_again
        ENDIF ELSE return
      ENDIF 

      TRY_AGAIN: 
      fileid = Hdf_Sd_Start(ofilename,/create)

      Hdf_sd_AttrSet,fileId,'LONGNAME',strtrim(LongName[0],2)
      Hdf_sd_AttrSet,fileId,'VERSION',strtrim(Version[0],2)
      Hdf_sd_AttrSet,fileId,'SHORTNAME',ShortName
      Hdf_sd_AttrSet,fileId,'RANGEBEGINNINGTIME',RANGEbeginningTime
      Hdf_sd_AttrSet,fileId,'RANGEBEGINNINGDATE',RANGEbeginningDate
      Hdf_sd_AttrSet,fileId,'RANGEENDINGTIME',RANGEendingTime
      Hdf_sd_AttrSet,fileId,'RANGEENDINGDATE',RANGEendingDate
      Hdf_sd_AttrSet,fileId,'EQUATORCROSSINGTIME',EquatorcrossingTime
      Hdf_sd_AttrSet,fileId,'EQUATORCROSSINGDATE',EquatorcrossingDate
      Hdf_sd_AttrSet,fileId,'EQUATORCROSSINGLONGITUDE',EquatorcrossingLongitude

        ; All items except speed/dir, row and row_time have dimension of lon
      dims = size(lon,/dimensions)
      ID = HDF_SD_CREATE(FILEID,'wvc_lon',dims,/float);
      HDF_SD_ADDDATA,id, lon
      HDF_SD_SETINFO, Id, caldata=caldata      
      HDF_SD_ENDACCESS, Id

      ID = HDF_SD_CREATE(FILEID,'wvc_lat',dims,/float);
      HDF_SD_ADDDATA,id, lat
      HDF_SD_SETINFO, Id, caldata=caldata      
      HDF_SD_ENDACCESS, Id

      ID = HDF_SD_CREATE(FILEID,'wvc_quality_flag',dims,/float);
      HDF_SD_ADDDATA,id, qual
      HDF_SD_SETINFO, Id, caldata=caldata      
      HDF_SD_ENDACCESS, Id

      ID = HDF_SD_CREATE(FILEID,'wvc_selection',dims,/float);
      HDF_SD_ADDDATA,id, sel
      HDF_SD_SETINFO, Id, caldata=caldata      
      HDF_SD_ENDACCESS, Id

      ID = HDF_SD_CREATE(FILEID,'wvc_index',dims,/float);
      HDF_SD_ADDDATA,id, idx
      HDF_SD_SETINFO, Id, caldata=caldata      
      HDF_SD_ENDACCESS, Id

      ID = HDF_SD_CREATE(FILEID,'num_ambigs',dims,/float);
      HDF_SD_ADDDATA,id, nambig
      HDF_SD_SETINFO, Id, caldata=caldata      
      HDF_SD_ENDACCESS, Id

      dims = size( row, /dimensions )
      ID = HDF_SD_CREATE(FILEID,'wvc_row',dims,/long);
      HDF_SD_ADDDATA,id, row
      HDF_SD_SETINFO, Id, caldata=caldata      
      HDF_SD_ENDACCESS, Id

      ID = HDF_SD_CREATE(FILEID,'wvc_row_time',dims,/string);
      HDF_SD_ADDDATA,id, row_time
      HDF_SD_SETINFO, Id, caldata=caldata      
      HDF_SD_ENDACCESS, Id

      dims = size( dir, /dimensions )
      ID = HDF_SD_CREATE(FILEID,'wind_dir',dims,/float);
      HDF_SD_ADDDATA,id, dir
      HDF_SD_SETINFO, Id, caldata=caldata      
      HDF_SD_ENDACCESS, Id

      ID = HDF_SD_CREATE(FILEID,'wind_speed',dims,/float);
      HDF_SD_ADDDATA,id, speed
      HDF_SD_SETINFO, Id, caldata=caldata      
      HDF_SD_ENDACCESS, Id

      HDF_SD_END, fileid
    ENDIF ELSE Message,'Error Reading Q2b Object with file ' + file,/cont
  ENDFOR 

END

