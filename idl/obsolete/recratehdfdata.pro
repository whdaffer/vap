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
; Revision 1.1  1998/10/22 21:20:31  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO RecrateHDFData, newfile, oldfile, startDt, EndDt

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
      q = obj_new('q2b',file=oldfile)
      IF obj_Valid(q) THEN BEGIN 
        q-> GetAll,U=u,v=v,lon=lon,lat=lat,sel=sel,row=row,$
            idx=idx,rtime=row_time,nambig=nambig, Qual=qual

        UV2DirSpeed,u,v,Dir,Speed

        u = 0
        v = 0

        LongName = "HDF_FROM_SVH_DATA"
        ShortName =  "QSCATL2B"
        version = "<no version>"

            ; Date has yyyy-DDD format
            ; Time has hh:mm:ss.ccc format


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
        fileid = Hdf_Sd_Start(newfile,/create)

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

        dims = size( dir, /dimensions )
        ID = HDF_SD_CREATE(FILEID,'wind_dir',dims,/float);
        HDF_SD_ADDDATA,id, dir
        HDF_SD_SETINFO, Id, caldata=caldata      
        HDF_SD_ENDACCESS, Id

        ID = HDF_SD_CREATE(FILEID,'wind_speed',dims,/float);
        HDF_SD_ADDDATA,id, speed
        HDF_SD_SETINFO, Id, caldata=caldata      
        HDF_SD_ENDACCESS, Id

        dims = size( row, /dimensions )
        ID = HDF_SD_CREATE(FILEID,'wvc_row',dims,/long);
        HDF_SD_ADDDATA,id, row
        caldata.Num_Type = IdlType2HdfType('LONG')
        HDF_SD_SETINFO, Id, caldata=caldata      
        HDF_SD_ENDACCESS, Id
        
        caldata.num_type = idltype2hdftype('STRING')
        ID = HDF_SD_CREATE(FILEID,'wvc_row_time',dims,/string);
        HDF_SD_SETINFO, Id, caldata=caldata      
        HDF_SD_ADDDATA,id, row_time
        HDF_SD_ENDACCESS, Id


        HDF_SD_END, fileid
      ENDIF ELSE Message,"Can't create Q2B object ",/cont
    ENDIF ELSE Message,"OLDFILE Not an HDF file!",/cont

END
