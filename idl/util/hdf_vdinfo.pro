;+
; NAME:  hdf_vdinfo
; $Id$
; PURPOSE:  Print information about HDF Vdatas
;
; AUTHOR:  William Daffer
;
; CATEGORY:  HDF Utility
;
; CALLING SEQUENCE:  hdf_vdinfo, filename
; 
; INPUTS:  filename: scalar string
;
; OPTIONAL INPUTS:  none
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
;
; $Log$
;
;Copyright (c) 1999, William Daffer
;No Warranties!
;
;-

PRO hdfvd_info, file
  ; Open the file
fileid=hdf_Open(file)
  ; iterate over the Vdatas in the HDF file
  vdid = -1l
  repeat begin
    vdid=hdf_vd_getid(fileid,vdid)  
    print,'vdid = ', vdid
    IF vdid NE 0 THEN BEGIN 
      vdh=hdf_vd_attach(fileid, vdid) 
      IF vdh GT 0 THEN BEGIN 
        hdf_vd_get,vdh,nfields=n, Name= name
        print,'Information for Vdata: ' + name
        for i=0,n-1 do begin  
          hdf_vd_getinfo, vdh, i, name=n, type=t, order=o, size=s 
          print,i,': ',n,' type= ',t,' order= ',o, ' size = ',s 
        end 
        hdf_vd_detach,vdh
      ENDIF 
    ENDIF 
  ENDREP UNTIL vdid EQ 0

  hdf_close,fileid
END

