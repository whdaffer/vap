;+
; NAME:  RecrateHdfData
; $Id$
; PURPOSE:  
;
;
; AUTHOR:
;
;
; CATEGORY:  Qscat VAP testing
;
;
;
; CALLING SEQUENCE:  
;
;
; 
; INPUTS:  
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;
;
; OUTPUTS:  
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
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
; Revision 1.3  1998/10/23 22:16:33  vapuser
; Took out calls to Q2B object and did it by hand.
;
; Revision 1.2  1998/10/23 14:43:12  vapuser
; Added comments, worked on ::version
;
; Revision 1.1  1998/10/22 21:20:31  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO RecrateHDFData, newfile, oldfile, startDt, EndDt

   lf =  string(10b)
   IF n_params() NE 4 THEN begin
     Message,"Usage: RecrateHdfData, newfile, oldfile, startDt, EndDt",/cont
     return
   ENDIF 
   IF VarType( newfile ) NE 'STRING' OR $
      VarType( oldfile ) NE 'STRING' THEN BEGIN 
     Message,"Both NEWFILE or OLDFILE must be strings",/cont
     return
   ENDIF 

   IF VarType( StartDt ) NE 'STRUCTURE' OR $
      VarType( EndDt ) NE 'STRUCTURE' THEN BEGIN 
     Message,"Both StartDt or EndDt must be Structure of type IDLDT",/cont
     return
   ENDIF ELSE BEGIN 
     IF tag_names( StartDt, /structure_Name) NE 'IDLDT' OR  $
        tag_names( EndDt, /structure_Name) NE 'IDLDT' THEN BEGIN 
       Message,"Both StartDt or EndDt must be Structure of type IDLDT",/cont
       return
     ENDIF 
   ENDELSE 

   catch,error
   IF error NE 0 THEN BEGIN 
     message,!error_State.msg,/cont
     return
   ENDIF 

   caldata = {cal:1.0d, cal_err: 0.0d, offset:0.0d, offset_err:0.0d, Num_Type:0l }
   caldata.Num_Type = IdlType2HdfType('FLOAT')

   IF Hdf_IsHdf( oldfile ) THEN BEGIN 

            ; Date has yyyy-DDD format
            ; Time has hh:mm:ss.ccc format

      ifileid = Hdf_Sd_Start(oldfile,/Read)
      ofileid = Hdf_Sd_Start(newfile,/create)
      

      hdf_sd_fileinfo,ifileid,datasets,attributes
      ; print,'Number of SD data sets: ',datasets
      ; print,'Number of attributes:   ',attributes
      eqx = eqx_str(1)
      FOR ai =0,attributes-1 DO BEGIN 
        hdf_sd_attrinfo,ifileid,ai,name=name,type=type,count=count,data=data
        name = strupcase(name)
        
        ; print,'Working on ', name
        IF VarType(data) EQ 'STRING' THEN BEGIN 
          tmp =  strsplit(  data, lf ,/extract) 
          tmp = tmp(where(strlen(tmp)))
          data =  tmp(n_elements(tmp)-1)
        ENDIF 
        CASE name OF 
        'EQUATORCROSSINGLONGITUDE': equatorCrossinglongitude = float(data)
        ELSE:
        ENDCASE

      ENDFOR


      r = hdf_sd_nametoindex(ifileid, 'wvc_lat')
      r = hdf_sd_select( ifileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, lat
      hdf_sd_endaccess,r
      lat =  float(lat*cal.cal + cal.offset)

      dims = size(lat,/dimension)
      ID = HDF_SD_CREATE(OFILEID,'wvc_lat',dims,/float);
      HDF_SD_ADDDATA,id, lat
      HDF_SD_SETINFO, Id, caldata=cal      
      HDF_SD_ENDACCESS, Id

      r = hdf_sd_nametoindex(ifileid, 'wvc_row')
      r = hdf_sd_select( ifileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, row
      hdf_sd_endaccess,r

      dims = size( row, /dimensions )
      ID = HDF_SD_CREATE(OFILEID,'wvc_row',dims,/long);
      HDF_SD_ADDDATA,id, row
      caldata.Num_Type = IdlType2HdfType('LONG')
      HDF_SD_SETINFO, Id, caldata=cal      
      HDF_SD_ENDACCESS, Id

      


      r = hdf_sd_nametoindex(ifileid, 'wvc_lon')
      r = hdf_sd_select( ifileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, lon
      hdf_sd_endaccess,r
      lon =  float(lon*cal.cal + cal.offset)

      dims = size(lon,/dimensions)
      ID = HDF_SD_CREATE(OFILEID,'wvc_lon',dims,/float);
      HDF_SD_ADDDATA,id, lon
      HDF_SD_SETINFO, Id, caldata=cal      
      HDF_SD_ENDACCESS, Id

      r = hdf_sd_nametoindex(ifileid, 'wvc_quality_flag')
      r = hdf_sd_select( ifileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, qual
      hdf_sd_endaccess,r
      qual =  fix(qual*cal.cal + cal.offset)

      dims = size(qual,/dimension)
      ID = HDF_SD_CREATE(OFILEID,'wvc_quality_flag',dims,/float);
      HDF_SD_ADDDATA,id, qual
      HDF_SD_SETINFO, Id, caldata=cal      
      HDF_SD_ENDACCESS, Id


      r = hdf_sd_nametoindex(ifileid, 'wind_speed')
      r = hdf_sd_select( ifileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, speed
      hdf_sd_endaccess,r
      speed =  float(speed*cal.cal + cal.offset)
      
      dims = size( speed, /dimensions )
      ID = HDF_SD_CREATE(OFILEID,'wind_speed',dims,/float);
      HDF_SD_ADDDATA,id, speed
      HDF_SD_SETINFO, Id, caldata=cal      
      HDF_SD_ENDACCESS, Id

      r = hdf_sd_nametoindex(ifileid, 'wind_dir')
      r = hdf_sd_select( ifileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, dir
      hdf_sd_endaccess,r
      dir =  float(dir*cal.cal + cal.offset)


      dims = size( dir, /dimensions )
      ID = HDF_SD_CREATE(OFILEID,'wind_dir',dims,/float);
      HDF_SD_ADDDATA,id, dir
      HDF_SD_SETINFO, Id, caldata=cal      
      HDF_SD_ENDACCESS, Id

      r = hdf_sd_nametoindex(ifileid, 'wvc_selection')
      r = hdf_sd_select( ifileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, sel
      hdf_sd_endaccess,r
      sel =  fix(sel*cal.cal + cal.offset)


      dims = size(sel,/dimension)
      ID = HDF_SD_CREATE(OFILEID,'wvc_selection',dims,/float);
      HDF_SD_ADDDATA,id, sel
      HDF_SD_SETINFO, Id, caldata=cal      
      HDF_SD_ENDACCESS, Id


      r = hdf_sd_nametoindex(ifileid, 'wvc_index')
      r = hdf_sd_select( ifileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, idx
      hdf_sd_endaccess,r
      idx =  fix(idx*cal.cal + cal.offset)


      dims = size(idx,/dimension)
      ID = HDF_SD_CREATE(OFILEID,'wvc_index',dims,/float);
      HDF_SD_ADDDATA,id, idx
      HDF_SD_SETINFO, Id, caldata=cal      
      HDF_SD_ENDACCESS, Id


      r = hdf_sd_nametoindex(ifileid, 'num_ambigs')
      r = hdf_sd_select( ifileid, r )
      hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
      hdf_sd_getdata,r, nambig
      hdf_sd_endaccess,r
      nambig =  fix(nambig*cal.cal + cal.offset)

      dims = size(nambig,/dimension)
      ID = HDF_SD_CREATE(OFILEID,'num_ambigs',dims,/float);
      HDF_SD_ADDDATA,id, nambig
      HDF_SD_SETINFO, Id, caldata=cal      
      HDF_SD_ENDACCESS, Id



      r = hdf_sd_nametoindex(ifileid, 'wvc_row_time')
      IF r GE 0  THEN BEGIN 
        r = hdf_sd_select( ifileid, r )
        hdf_sd_getinfo, r, ndims=nd, dims=dims, type=ty, unit=un, caldata=cal
        hdf_sd_getdata,r, rowtime
        hdf_sd_endaccess,r

        dims = size(rowtime,/dimension)
        ID = HDF_SD_CREATE(OFILEID,'wvc_row_time',dims,/string);
        HDF_SD_SETINFO, Id, caldata=cal      
        HDF_SD_ADDDATA,id, row_time
        HDF_SD_ENDACCESS, Id


      ENDIF 

      hdf_sd_end,ifileid


      year = strtrim( startDt.year,2 )
      month = PadAndJustify( startdt.month, 2 )
      day = PadAndJustify( startdt.day, 2 )
      shour = PadAndJustify( StartDt.hour,2 )
      smin = PadAndJustify( StartDt.minute,2 )

      ehour = PadAndJustify( EndDt.hour,2 )
      emin = PadAndJustify( EndDt.minute,2 )


      diff = (EndDt.Julian-StartDt.Julian)*12.
      diff_hours = fix(diff)
      diff_mins = fix((diff-diff_hours)*60)
      Eqx = dt_add(startDt, hour=diff_hours, min=diff_mins)
      dt_to_var,Eqx,hour=xhour,min=xmin

      xhour = PadAndJustify( xhour,2 )
      xmin = PadAndJustify( xmin,2 )

      doy = PadAndJustify( date2doy( StartDt.year, StartDt.month, StartDt.day ), 3)

      secs =  ':00.000'
      hdfdate = year + '-' + doy

      EQUATORCROSSINGDATE = hdfdate
      EQUATORCROSSINGTIME = xhour+':'+xmin+secs
      EQUATORCROSSINGLONGITUDE =  0.0
      RANGEBEGINNINGDATE = hdfdate
      RANGEBEGINNINGTIME = shour+':'+smin+secs
      RANGEENDINGDATE =  hdfdate
      RANGEENDINGTIME =  ehour+':'+emin+secs

      SHORTNAME = 'QSCATL2B'
      version = '<no version>'
      LongName = 'Q2B_FAKE_HDF_DATA'

      Hdf_sd_AttrSet,ofileid,'LONGNAME',strtrim(LongName[0],2)
      Hdf_sd_AttrSet,ofileid,'VERSION',strtrim(Version[0],2)
      Hdf_sd_AttrSet,ofileid,'SHORTNAME',ShortName
      Hdf_sd_AttrSet,ofileid,'RANGEBEGINNINGTIME',RANGEbeginningTime
      Hdf_sd_AttrSet,ofileid,'RANGEBEGINNINGDATE',RANGEbeginningDate
      Hdf_sd_AttrSet,ofileid,'RANGEENDINGTIME',RANGEendingTime
      Hdf_sd_AttrSet,ofileid,'RANGEENDINGDATE',RANGEendingDate
      Hdf_sd_AttrSet,ofileid,'EQUATORCROSSINGTIME',EquatorcrossingTime
      Hdf_sd_AttrSet,ofileid,'EQUATORCROSSINGDATE',EquatorcrossingDate
      Hdf_sd_AttrSet,ofileid,'EQUATORCROSSINGLONGITUDE',equatorCrossinglongitude

      HDF_SD_END, ofileid
    ENDIF ELSE Message,"OLDFILE Not an HDF file!",/cont

END
