;+
; NAME: QmodelWrite  
; $Id$
; PURPOSE:  Write out Qmodel data ( most likely output from a Succor run)
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  status=QmodelWrite(filename, u,v,$
;                         LongName  = LongName, $
;                         Version   = Version, $
;                         CreationTime= CreationTime,$
;                         StartTime = StartTime, $
;                         EndTime   = EndTime,$
;                         LonPar    = LonPar,$    
;                         LatPar    = LatPar, $   
;                         Region    = region
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

FUNCTION qmodelwrite, filename, u,v,$
                      LongName  = LongName, $
                      Version   = Version, $
                      CreationTime= CreationTime,$
                      StartTime = StartTime, $
                      EndTime   = EndTime,$
                      LonPar    = LonPar,$    
                      LatPar    = LatPar, $   
                      Region    = region, $
                      Native    = Native

  rcsid = "$Id$"
  status = 0

  IF NOT keyword_set( Native) THEN BEGIN 
      ; HDF file
    status = qmodelHdfWrite( filename, u,v,$
                        LongName  = LongName, $
                        Version   = Version, $
                        CreationTime= CreationTime,$
                        StartTime = StartTime, $
                        EndTime   = EndTime,$
                        LonPar    = LonPar,$    
                        LatPar    = LatPar, $   
                        Region    = region, $
                        Native    = Native )
  ENDIF ELSE BEGIN 
    nd = size(u,/n_dimensions)
    s = size(u,/dimensions)
    IF nd eq 2 THEN BEGIN 
      nlon = s[0]
      nlat = s[1]
      IF n_Elements(Region) EQ 4 THEN BEGIN 
        loninc =  (Region[2]-Region[0])/nlon
        latinc =  (Region[3]-Region[1])/nlat
        LonPar = [ Region[0], Regiion[2], LonInc ]
        LatPar = [ Region[1], Regiion[3], LatInc ]
      ENDIF ELSE BEGIN 
        IF n_Elements(LonPar) NE 3 OR $
           N_Elements(LatPar) NE 3 THEN BEGIN 
          Message,'If Native, must have Lonpar/Latpar or Region set',/cont
          print,'  Lonpar/Latpar must be [Min,Max,Incr]'
          print,'  Region=[MinLon,MinLat,MaxLon,MaxLat]'
          return,0
        ENDIF 
      ENDELSE 
      q = qmodel_str(nlon,nlat)
      IF N_Elements(CreationTime) NE 0 THEN $
         q.CreationTime = CreationTime
      IF N_Elements(StartTime) NE 0 THEN $
         q.StartTime = StartTime
      IF N_Elements(EndTime) NE 0 THEN $
         q.EndTime = EndTime
      q.Lonpar = Lonpar
      q.Latpar = Latpar
      q.U = U
      q.V = V
      openw, lun, filename, /get, error=err
      IF err eq 0 THEN BEGIN 
        writeu, lun, q
        free_lun, q
        status = 1
      ENDIF ELSE $
        Message,!error_State.msg,/cont

  ENDELSE 

  return,status

END

