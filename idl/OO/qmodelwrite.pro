;+
; NAME: QmodelWrite  
; $Id$
; PURPOSE:  
;
;           Write out Qmodel data ( most likely output from a Succor
;           run) Will write it to HDF file unless the 'native' flag is
;           set, in which case it will write it out as a flat
;           file. This introduces a complexity, in that the Qmodel
;           structure doesn't have a fixed format, i.e. the U/V data
;           members are, in fact, pointers, to allow for different
;           resolutions of model data (see qmodel_str.pro).  This
;           allows the structur to be a 'named' structure, useful
;           within the q2b object, as well as the 'pv' object, while
;           maintaining a maximum of flexibility.
;
;           I would recommend writting to an HDF file, but I put this
;           in just in case.
;
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  
;
;       status = QmodelWrite(filename, u,v,$
;                            LongName  = LongName, $
;                            Version   = Version, $
;                            CreationTime= CreationTime,$
;                            StartTime = StartTime, $
;                            EndTime   = EndTime,$
;                            LonPar    = LonPar,$    
;                            LatPar    = LatPar, $   
;                            Region    = region, $
;                            Native     =native
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
; Revision 1.1  1998/10/07 18:29:45  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION qmodelwrite, filename, u,v,$
                      LongName  = LongName, $
                      ShortName = ShortName ,$
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
                             ShortName = ShortName, $
                             CreationTime= CreationTime,$
                             StartTime = StartTime, $
                             EndTime   = EndTime,$
                             LonPar    = LonPar,$    
                             LatPar    = LatPar, $   
                             Region    = region )
  ENDIF ELSE BEGIN 

      ; Write the data out in 'native' format. A little more tricky.

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
      q = Qmodel_Str(nlon,nlat)
      hdr = q.hdr

        ; Because of the way the Qmodel structure works, namely the
        ; presence of ptrs, you can't write the whole thing out in
        ; one move, you have to write it (and read it) in two stages.

      hdr.Lonpar = Lonpar
      hdr.Latpar = Latpar
      hdr.nlon = nlon
      hdr.nlat = nlat

      IF N_Elements(ShortName) EQ 0 THEN ShortName =  "QMODEL"
      IF N_Elements(CreationTime) NE 0 THEN BEGIN 
        nn = strlen( CreationTime )
        tmp = byte( CreationTime )
        hdr.CreationTime[0:nn-1] = tmp
      ENDIF 

      IF N_Elements(StartTime) NE 0 THEN BEGIN 
        nn = strlen( StartTime )
        tmp = byte( StartTime )
        hdr.StartTime[0:nn-1] = tmp
      ENDIF 

      IF N_Elements(EndTime) NE 0 THEN BEGIN 
        nn = strlen( EndTime )
        tmp = byte( EndTime )
        hdr.EndTime[0:nn-1] = tmp
      ENDIF 

      openw, lun, filename, /get, error=err
      IF err eq 0 THEN BEGIN 
        writeu, lun, hdr
        writeU, lun, U, V
        free_lun, lun
        status = 1
      ENDIF ELSE $
        Message,!error_State.msg,/cont
    ENDIF ELSE BEGIN 
      Message,'U is not 2 dimensional!',/cont
    ENDELSE 
  ENDELSE 

  return,status

END



