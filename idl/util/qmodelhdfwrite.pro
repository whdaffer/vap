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
;  status=QModelHdfWrite( filename, u,v,$
;                         ShortName = ShortName,$
;                         LongName  = LongName, $
;                         Version   = Version, $
;                         CreationTime= CreationTime,$
;                         StartTime = StartTime, $
;                         EndTime   = EndTime,$
;                         LonPar    = LonPar,$    
;                         LatPar    = LatPar, $   
;                         Region    = region )
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
; Revision 1.4  1998/10/13 18:24:41  vapuser
; Correct misspelling in ShortName
;
; Revision 1.3  1998/10/12 22:06:53  vapuser
; Fixed some bugs
;
; Revision 1.2  1998/10/07 18:31:34  vapuser
; don't remember.
;
; Revision 1.1  1998/10/07 00:07:48  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION qmodelhdfwrite, filename, u,v,$
                         ShortName = ShortName,$
                         LongName  = LongName, $
                         Version   = Version, $
                         CreationTime= CreationTime,$
                         StartTime = StartTime, $
                         EndTime   = EndTime,$
                         LonPar    = LonPar,$    
                         LatPar    = LatPar, $   
                         Region    = region
                         
   status = 0
   rcsid = "$Id$"

   Catch,error
   IF error NE 0 THEN BEGIN 
     Message,!err_string,/cont
     return,0
   ENDIF 


   IF N_elements(region) EQ 4 THEN BEGIN 
      s=size(U)
      nlon = s[1]
      nlat = s[2]

      IF n_elements(lonpar) NE 3 THEN BEGIN 
        lonpar = fltarr(3)
        lonpar[0,1] =  region[[0,2]]
        lonpar[2] =  (region[2]-region[1])/nlon
      ENDIF 
      IF n_elements(latpar) NE 3 THEN BEGIN 
        latpar[0,1] =  region[[1,3]]
        latpar[2] =  (region[3]-region[1])/nlat
      ENDIF 

   ENDIF ELSE BEGIN 
     IF n_elements(Lonpar) ne 3 AND $
        n_elements(Latpar) ne 3 THEN BEGIN 
       Message,$
         'Either region must be set (a 4-vector) or lonpar/latpar must be set',/cont
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

           IF N_Elements(LongName) EQ 0 THEN $
             LongName = '<no longname>' $
           ELSE IF strlen(LongName) EQ 0 THEN $
             LongName = '<no longname>' 

           IF N_Elements(Version) EQ 0 THEN $
            Version = '<no version>' $ 
           ELSE IF strlen(Version) EQ 0 THEN $
            Version = '<no version>' 

           IF N_Elements(StartTime) EQ 0 THEN $
             StartTime = '0000/00/00/00/00' $
           ELSE IF strlen(StartTime) EQ 0 THEN  $
             StartTime = '0000/00/00/00/00' 

           IF N_Elements(EndTime) EQ 0 THEN  $
             EndTime = '0000/00/00/00/00' $
           ELSE IF strlen(EndTime) EQ 0 THEN $
             EndTime = '0000/00/00/00/00' 

           IF N_Elements(CreationTime) EQ 0 THEN $
             CreationTime = '0000/00/00/00/00' $
           ELSE IF strlen( CreationTime ) EQ 0 THEN $
             CreationTime = '0000/00/00/00/00' 

           IF N_Elements(ShortName) EQ 0 THEN $
             ShortName = 'QSCATVAPMODEL' $
           ELSE IF strlen(ShortName) EQ 0 THEN $
             ShortName = 'QSCATVAPMODEL' 

           Hdf_sd_AttrSet,fileId,'LONGNAME',strtrim(LongName[0],2)
           Hdf_sd_AttrSet,fileId,'VERSION',strtrim(Version[0],2)
           Hdf_sd_AttrSet,fileId,'STARTTIME',strtrim(StartTime[0],2)
           Hdf_sd_AttrSet,fileId,'ENDTIME',strtrim(EndTime[0],2)
           Hdf_sd_AttrSet,fileId,'CREATIONTIME',strtrim(CreationTime[0],2)
           Hdf_sd_AttrSet,fileId,'SHORTNAME',ShortName
           Hdf_sd_AttrSet,fileId,'NLON',nlon,/short
           Hdf_sd_AttrSet,fileId,'NLAT',nlat,/short
           Hdf_sd_AttrSet,fileId,'REGION',region,/float
           Hdf_sd_AttrSet,fileId,'LONPAR',lonpar,/float
           Hdf_sd_AttrSet,fileId,'LATPAR',latpar,/float
           USdsId =  Hdf_sd_create( fileid, "U",[nlon,nlat],/float)
           VSdsId =  Hdf_sd_create( fileid, "V",[nlon,nlat],/float)
           Hdf_Sd_AddData,USdsId, U
           Hdf_Sd_AddData,VSdsId,V
;           caldata.Num_type = idltype2hdftype('FLOAT')
;           Hdf_sd_setinfo, UsdsId, caldata=caldata
;           Hdf_sd_setinfo, VsdsId, caldata=caldata
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
