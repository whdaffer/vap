;+
; NAME:  qmodelhdfwrite
; $Id$
; PURPOSE:  Writes out a model field (most likely, output from a
;          succor run) into an HDF file.
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY: Qscat Data I/O
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
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION qmodelhdfwrite, filename, u,v,$
                         LongName  = LongName, $
                         Version   = Version, $
                         CreationTime= CreationTime,$
                         StartTime = StartTime, $
                         EndTime   = EndTime,$
                         LonPar    = LonPar,$    
                         LatPar    = LatPar, $   
                         Region    = region
                         
   status = 0
   rcsid = "$Id$

   Catch,error
   IF error NE 0 THEN BEGIN 
     Message,!err_string,/cont
     return,0
   ENDIF 


   IF N_elements(region) EQ 4 THEN BEGIN 
      s=size(U)
      nlon = s[1]
      nlat = s[2]

      lonpar = fltarr(3)
      lonpar[0,1] =  region[0,2]
      lonpar[2] =  (region[2]-region[1])/nlon

      latpar[0,1] =  region[1,3]
      latpar[2] =  (region[3]-region[1])/nlat

   ENDIF ELSE BEGIN 
     IF n_elements(Lonpar) ne 3 AND $
        n_elements(Latpar) ne 3 THEN BEGIN 
       Message,'either region must be set (and a 4 vector) or lonpar/latpar must be set',/cont
       return,0
     ENDIF ELSE BEGIN 
       region = [ lonpar[0], latpar[0], lonpar[1], latpar[1] ]
     ENDELSE 
     
   ENDELSE 

   caldata = {cal:1.0d, cal_err: 0.0d, offset:0.0d, offset_err:0.0d, Num_Type:0l }

   tfilename = filename[0]
   IF n_Params() eq 3 THEN BEGIN 
     IF vartype(tfilename) EQ 'STRING' THEN BEGIN 
       s = size(u)
       IF s[0] eq 2 AND n_elements(region) EQ 4 THEN BEGIN 

         nlon = s[1]
         nlat = s[2]
         fileid = Hdf_Sd_Start(tfilename,/create)
         IF fileid GT 0 THEN BEGIN 
           IF N_Elements(LongName) EQ 0 THEN LongName = ''
           IF N_Elements(Version) EQ 0 THEN Version = ''
           IF N_Elements(StartTime) EQ 0 THEN StartTime = ''
           IF N_Elements(EndTime) EQ 0 THEN EndTime = ''
           IF N_Elements(CreationTime) EQ 0 THEN CreationTime = ''
           Hdf_sd_AttrSet,fileId,'SHORTNAME','QSCATVAPMODEL'
           Hdf_sd_AttrSet,fileId,'NLON',nlon,/short
           Hdf_sd_AttrSet,fileId,'NLAT',nlat,/short
           Hdf_sd_AttrSet,fileId,'LONGNAME',strtrim(LongName,2)
           Hdf_sd_AttrSet,fileId,'VERSION',strtrim(Version,2)
           Hdf_sd_AttrSet,fileId,'STARTTIME',strtrim(StartTime,2)
           Hdf_sd_AttrSet,fileId,'ENDTIME',strtrim(EndTime,2)
           Hdf_sd_AttrSet,fileId,'CreationTime',strtrim(CreationTime,2)
           Hdf_sd_AttrSet,fileId,'REGION',region,/float
           Hdf_sd_AttrSet,fileId,'LONPAR',lonpar,/float
           Hdf_sd_AttrSet,fileId,'LATPAR',latpar,/float
           USdsId =  Hdf_sd_create( fileid, "U",[nlon,nlat],/float)
           VSdsId =  Hdf_sd_create( fileid, "V",[nlon,nlat],/float)
           Hdf_Sd_AddData,USdsId, U
           Hdf_Sd_AddData,VSdsId,V
           caldata.Num_type = idltype2hdftype('FLOAT')
           Hdf_sd_setinfo, UsdsId, caldata=caldata
           Hdf_sd_setinfo, VsdsId, caldata=caldata
           HDF_SD_ENDACCESS, USdsId
           HDF_SD_ENDACCESS, VSdsId
           HDF_SD_END, fileid
           status = 1
         ENDIF ELSE $
           Message,"Can't open file " + tfilename,/cont
       ENDIF ELSE $
          Message,'U/V must be 2 dimentional and region must be 4-vector',/cont
     ENDIF ELSE $
        Message,'tfilename must be of type STRING',/cont
   ENDIF ELSE BEGIN 
     str =  'Usage: s= qmodelwrite(u,v,longname=longname, ' + $
      ' version=version, starttime=starttime, endtime=endtime,' + $
       ' creationtime=creationtime, lonpar=lonpar, ' + $
        'latpar=latpar, region=region)'
     Message,str,/cont
   ENDELSE 
   return,status
END
