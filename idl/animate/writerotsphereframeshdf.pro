PRO writerotsphereframeshdf, infile, outfile, status
   IF n_params() LT 1 THEN BEGIN 
     Usage,"WriteRotSphereFramesHDF,infile [,outfile]",/cont
     return
   ENDIF 

   catch, error
   IF error NE 0 THEN BEGIN 
     catch,/cancel
     Message,!error_state.msg,/cont
     return
   ENDIF 

   fileid = Hdf_Sd_Start(outfile,/create)
   IF fileid LE 0 THEN BEGIN 
     Message,"Error opening HDF output file ",/cont
     return
   ENDIF 

   imid = hdf_sd_create( fileid, 'Frame', [600,600,3,360], /byte )
   
   
   IF n_elements(outfile) EQ 0 THEN outfile = 'RotSphereFrames.hdf'
   openr, lun, infile, /get, error=err
   IF err NE 0 THEN BEGIN 
     Message,!error_state.msg,/cont
     return
   ENDIF 
   r = 0.
   wim = bytarr(600,600,3)
   WHILE NOT eof(lun) DO BEGIN 
     readu, lun, r, wim
     ii = long(r-1)
     FOR i=0,2 DO hdf_sd_adddata, imid, wim[*,*,i], start=[0,0,i,ii]
   ENDWHILE 
   free_lun, lun
   hdf_sd_endAccess, imid
   hdf_sd_end, fileid
END
