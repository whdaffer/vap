;+
; NAME:   q1b__define
; $Id$
; PURPOSE:   Defines and object of type q1b
;
; AUTHOR; William Daffer
;
; CATEGORY:   OO
;
; CALLING SEQUENCE:   q1b = obj_new('q1b',...)
; 
; METHODS:
;   Init:
;   Set:
;   Get:
;   Cleanup:
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
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  

;============================================

FUNCTION q1b::Init,file, alwayscompress = alwayscompress
   status = 0
   IF n_params() LT 1 THEN BEGIN 
     Message,'Param 1 <file> is required',/cont
     return,0
   ENDIF 
   IF n_elements(alwayscompress) NE 0 THEN self.alwayscompress = alwayscompress
   status = self-> read(file)   
   self.status = status
  return,self.status
END


;============================================
; Cleanup
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO q1b::Cleanup
  ptr_free, self.lon, self.lat, self.time, self.sig0, self.beam
END

;============================================
; Read
;============================================
FUNCTION q1b::read, file

  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!error_state.msg,/noname
    self.status = 0
    self.file = ''
    self.nscans = -1l
    ptr_free,self.time,self.lon,self.lat, self.sig0, self.beam
    return, self.status
  ENDIF 
    
  self.status = 0
  self.file = ''
  self.nscans = -1l

  IF NOT isa(file,/string,/nonempty) THEN BEGIN 
    Message,'<' +file + '> must be a STRING',/cont
    self.status = 0
    return,0
  ENDIF 


  IF NOT isa(file,/string,/nonempty) THEN $
    Message,"Can't read file " + file
    
  IF NOT hdf_ishdf(file) THEN $
    Message,'<' +file + '> is not an HDF file!'

  lat = hdfgetsds(file,'cell_lat')
  IF isa(lat,/string) THEN $
    Message,"Can't read SDS CELL_LAT"

  lon = hdfgetsds(file,'cell_lon')
  IF isa(lon,/string) THEN $
    Message,"Can't read SDS CELL_LON"

  sigma0 = hdfgetsds(file,'cell_sigma0')
  IF isa(sigma0,/string) THEN $
    Message,"Can't read SDS CELL_SIGMA0"

  sigma0_mode_flag = hdfgetsds(file,'sigma0_mode_flag')
  IF isa(sigma0_mode_flag,/string) THEN $
    Message,"Can't read SDS SIGMA0_MODE_FLAG"

  beam = ishft(sigma0_mode_flag,-2) AND 1

  frame_time = hdfgetvd(file,'frame_time')
  IF isa(frame_time,/string) THEN $
    Message,"Can't read VD FRAME_TIME"

  time = ccsds2idldt(string(frame_time))
  self.nscans = n_elements(lat[0,*])
  self.lat = ptr_new(lat,/no_copy)
  self.lon = ptr_new(lon,/no_copy)
  self.sig0 = ptr_new(0.01*sigma0,/no_copy)
  self.beam = ptr_new(beam,/no_copy)
  self.time = ptr_new(time,/no_copy)
  self.status = 1

  return, self.status   
END



;============================================
; Set Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO q1b::Set, file = file, alwayscompress=alwayscompress
   IF n_elements(alwayscompress) NE 0 THEN self.alwayscompress = alwayscompress
   IF n_elements(file) NE 0 THEN self.status = self-> read(file)
END


;============================================
; Get Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO q1b::Get, lon = lon, lat=lat, sig0=sig0, $
       time=time, beam=beam, file=file, nscans=nscans, status=status
   IF arg_present(lon) THEN lon = self.lon
   IF arg_present(lat) THEN lat = self.lat
   IF arg_present(sig0) THEN sig0 = self.sig0
   IF arg_present(time) THEN time = self.time
   IF arg_present(beam) THEN beam = self.beam
   IF arg_present(file) THEN file = self.file
   IF arg_present(nscans) THEN nscans = self.nscans
   IF arg_present(status) THEN status = self.status
END



;============================================
; Definition Routine
;============================================

PRO q1b__define
  junk = {q1b, $
          status: 0, $
          wascompressed: 0, $
          alwayscompress: 0, $
          file: '', $
          nscans: 0l, $
          lon: ptr_new(), $
          lat: ptr_new(), $
          sig0: ptr_new(), $
          beam: ptr_new(), $
          time: ptr_new() }
END
