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
;                         InterpTime = InterpTime, $
;                         LonPar    = LonPar,$    
;                         LatPar    = LatPar, $   
;                         Region    = region, $
;                         rainf     = rainf, $
;                         ermax     = ermax, $
;                         crdecimate = crdecimate, $
;                         decimate   = decimate, $
;                         exclude_cols = exclude_cols, $
;                         wfiles=wfiles )
;
;
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
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
; Revision 1.6  2000/01/11 20:45:43  vapuser
; In line with the addition of metadata to the qmodel object and file, I
; added code to this module to read and transmit same.
;
; Revision 1.5  1999/04/09 15:37:53  vapuser
; Added some argument checking
;
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
                         ShortName    = ShortName,   $
                         LongName     = LongName,    $
                         Version      = Version,     $
                         CreationTime = CreationTime,$
                         StartTime    = StartTime,   $ 
                         EndTime      = EndTime,     $ 
                         InterpTime   = InterpTime,  $
                         LonPar       = LonPar,      $     
                         LatPar       = LatPar,      $    
                         Region       = region,      $    
                         rainf        = rainf,       $     
                         ermax        = ermax,       $     
                         crdecimate   = crdecimate,  $
                         decimate     = decimate,    $
                         exclude_cols = exclude_cols, $
                         wfiles       = wfiles
                         
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

           IF N_Elements(ShortName) EQ 0 THEN $
             ShortName = 'QSCATVAPMODEL' $
           ELSE IF strlen(ShortName) EQ 0 THEN $
             ShortName = 'QSCATVAPMODEL' $
           ELSE IF vartype(ShortName) EQ 'BYTE' THEN $
             ShortName = string(ShortName)

           IF N_Elements(LongName) EQ 0 THEN $
             LongName = '<no longname>' $
           ELSE IF strlen(LongName) EQ 0 THEN $
             LongName = '<no longname>' $
           ELSE IF vartype(LongName) EQ 'BYTE' THEN $
             LongName = string(LongName)

           IF N_Elements(Version) EQ 0 THEN $
            Version = '<no version>' $ 
           ELSE IF strlen(Version) EQ 0 THEN $
            Version = '<no version>' $
           ELSE IF vartype(Version) EQ 'BYTE' THEN $
             Version = string(Version)

           IF N_Elements(StartTime) EQ 0 THEN $
             StartTime = '0000/00/00/00/00' $
           ELSE IF strlen(StartTime) EQ 0 THEN  $
             StartTime = '0000/00/00/00/00' $
           ELSE IF vartype(StartTime) EQ 'BYTE' THEN $
             StartTime = string(StartTime)

           IF N_Elements(EndTime) EQ 0 THEN  $
             EndTime = '0000/00/00/00/00' $
           ELSE IF strlen(EndTime) EQ 0 THEN $
             EndTime = '0000/00/00/00/00' $
           ELSE IF vartype(EndTime) EQ 'BYTE' THEN $
             EndTime = string(EndTime)

           IF N_Elements(CreationTime) EQ 0 THEN $
             CreationTime = '0000/00/00/00/00' $
           ELSE IF strlen( CreationTime ) EQ 0 THEN $
             CreationTime = '0000/00/00/00/00' $
           ELSE IF vartype(creationtime) EQ 'BYTE' THEN $
             CreationTime = string(creationtime)

           IF N_Elements(InterpTime) EQ 0 THEN $
             InterpTime = '0000/00/00/00/00' $
           ELSE IF strlen( InterpTime ) EQ 0 THEN $
             InterpTime = '0000/00/00/00/00' $
           ELSE IF vartype(InterpTime) EQ 'BYTE' THEN $
             InterpTime = string(InterpTime)



           IF n_elements(rainf) EQ 0 THEN rainf = 0.
           IF n_elements(ermax) EQ 0 THEN ermax =  replicate(0.,n_elements(rainf))
           IF n_elements(crdecimate) EQ 0 THEN crdecimate = [-1,-1]
           IF n_elements(decimate) EQ 0 THEN decimate =  -1
           IF n_elements(exclude_cols) EQ 0 THEN $
             exclude_cols =  "<Don't know>" $
           ELSE IF strlen(exclude_cols) EQ 0 THEN $
             exclude_cols =  "<Don't know>" $
           ELSE IF vartype(exclude_cols) EQ 'BYTE' THEN $
             exclude_cols =  strtrim(string(exclude_cols),2)
             ; Set Global Attributes.

           IF n_elements(wfiles) EQ 0 THEN $
             wfiles = "<Don't know>" ELSE $
             wfiles = strjoin(wfiles,',')
           Hdf_sd_AttrSet,fileId,'LONGNAME',strtrim(LongName[0],2)
           Hdf_sd_AttrSet,fileId,'VERSION',strtrim(Version[0],2)
           Hdf_sd_AttrSet,fileId,'STARTTIME',strtrim(StartTime[0],2)
           Hdf_sd_AttrSet,fileId,'ENDTIME',strtrim(EndTime[0],2)
           Hdf_sd_AttrSet,fileId,'CREATIONTIME',strtrim(CreationTime[0],2)
           Hdf_sd_AttrSet,fileId,'INTERPTIME',strtrim(InterpTime[0],2)
           Hdf_sd_AttrSet,fileId,'SHORTNAME',ShortName
           Hdf_sd_AttrSet,fileId,'NLON',nlon,/long
           Hdf_sd_AttrSet,fileId,'NLAT',nlat,/long
           Hdf_sd_AttrSet,fileId,'REGION',region,/float
           Hdf_sd_AttrSet,fileId,'LONPAR',lonpar,/float
           Hdf_sd_AttrSet,fileId,'LATPAR',latpar,/float
           Hdf_sd_AttrSet,fileId,'RAINF',rainf,/float
           Hdf_sd_AttrSet,fileId,'ERMAX',ermax,/float
           Hdf_sd_AttrSet,fileId,'CRDECIMATE',crdecimate,/long
           Hdf_sd_AttrSet,fileId,'DECIMATE',decimate,/long
           Hdf_sd_AttrSet,fileId,'EXCLUDE_COLS',exclude_cols
           Hdf_sd_AttrSet,fileId,'WFILES',wfiles

             ; Create the SDS 
           USdsId =  Hdf_sd_create( fileid, "U",[nlon,nlat],/float)
           VSdsId =  Hdf_sd_create( fileid, "V",[nlon,nlat],/float)

             ; Add data to the SDS
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
       ' creationtime=creationtime, interptime=interptime, lonpar=lonpar, ' + $
        'latpar=latpar, region=region)'
     Message,str,/cont
   ENDELSE 
   return,status
END
