;+
; NAME:   rsf__define
; $Id$
; PURPOSE:   Defines and object of type rsf. This object keeps track
;          of the Rotating Satellite Movie Frames. This way, it only
;          takes 1.5 hours to make one of these movies instead of 7.
;
; AUTHOR; William Daffer
;
; CATEGORY:   OO
;
; CALLING SEQUENCE:   rsf = obj_new('rsf',...)
; 
; Nota Bene: Talk to William. 
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
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
; 
;     INPUTS:  Basename: The 'basename' of the movie. A scalar
;             string. This should be *everything* but the frame number
;             and the extention. At the moment, the only movies we
;             have data for are 600 by 600 by 720 frames starting at
;             180.0 longitue. Therefore the basename is 
;                "/disk4/vap/animate/RSF-600,600,180.0," 
;     OPTIONAL INPUTS:  None
;     KEYWORD PARAMETERS:  None
;     OUTPUTS:  An Object of type RSF
;     OPTIONAL OUTPUTS:  None
;     COMMON BLOCKS:  None

;============================================

FUNCTION rsf::Init,basename
  IF n_elements(basename) NE 0 THEN self.basename= basename
  return,1
END


;============================================
; Cleanup
;============================================

PRO rsf::Cleanup
  ptr_free, self.retdata.landi
  ptr_free, self.retdata.landeli
  ptr_free, self.retdata.mappedPixels
END

;============================================
FUNCTION rsf::ReadFile, framenumber

  IF n_elements(framenumber) EQ 0 THEN framenumber = 0

  catch, error
  IF error NE 0 THEN BEGIN 
    message,!error_state.msg,/cont
    return,0
  ENDIF 


  frame = padAndJustify(framenumber,4,pad='0',/right)
  file = self.basename + frame + '.hdf'
  
  fileid =  hdf_sd_start( file,/read)
  IF fileid LE 0 THEN $
    Message,"Can't open file <" + file + ">"

  index = HDF_SD_NAMETOINDEX(fileid, 'land')
  id = HDF_SD_SELECT(fileid, index)
  HDF_SD_GETDATA,id,landi
  hdf_sd_endaccess, id 

  index = HDF_SD_NAMETOINDEX(fileid, 'landel')
  id = HDF_SD_SELECT(fileid, index)


  HDF_SD_GETDATA,id,landeli
  hdf_sd_endaccess, id

  animpar = lonarr(3)
  idx =  hdf_sd_attrfind(fileid, 'num_x_pixels')
  hdf_sd_attrinfo, fileid, idx, data=data
  animpar[0] =  data[0]

  idx =  hdf_sd_attrfind(fileid, 'num_y_pixels')
  hdf_sd_attrinfo, fileid, idx, data=data
  animpar[1] =  data[0]

  idx =  hdf_sd_attrfind(fileid, 'num_frames')
  hdf_sd_attrinfo, fileid, idx, data=data
  animpar[2] =  data[0]

  idx =  hdf_sd_attrfind(fileid, 'center_longitude')
  hdf_sd_attrinfo, fileid, idx, data=data
  loncent =  data[0]
  


  hdf_sd_end, fileid

  self.retdata.loncent = loncent
  self.retdata.animpar = animpar

  self.retdata = self.retdata
  self.retdata.landi = ptr_new(landi,/no_copy)
  self.retdata.landeli = ptr_new(landeli,/no_copy)
  return, self.retdata
END


;============================================
FUNCTION rsf::WriteFile, framenumber, landi, landeli, animpar, loncent

  IF n_params() LT 5 THEN BEGIN 
    Message,$
      "All 5 parameters <landi, landel, animpar, loncent> are REQUIRED!",/cont
    return,0
  ENDIF 
  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,0
  ENDIF 
  file =  self.basename +  $
    PadAndJustify( framenumber, 4, pad='0', /right ) + '.hdf'
  fileid =  hdf_sd_start( file, /create )
  IF fileid LE 0 THEN $
    Message,"Can't open file <" + file + ">"

  land = 'land' 
  landel =  'landel' 
  id = hdf_sd_create(fileid, land, [n_elements(landi)],/long )
  hdf_sd_adddata, id, landi
  hdf_sd_endaccess,id
  id = hdf_sd_create(fileid, landel, [n_elements(landi)],/long )
  hdf_sd_adddata, id, landeli
  hdf_sd_endaccess,id

  hdf_sd_attrset, fileid, 'num_x_pixels', animpar[0], /long
  hdf_sd_attrset, fileid, 'num_y_pixels', animpar[1], /long
  hdf_sd_attrset, fileid, 'num_frames', animpar[2], /long
  hdf_sd_attrset, fileid, 'center_longitude', loncent,/float

  hdf_sd_end, fileid
  return,1
END


;============================================
FUNCTION rsf::ReadMappedPixels

  
  IF ptr_valid( self.retdata.mappedPixels) THEN $
    return, *(self.retdata.mappedpixels)

  catch,error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,0
  ENDIF 

  file = self.basename + 'mappedPixels.hdf'
  fileid = hdf_sd_start( file,/read)
  IF fileid LE 0 THEN $
    Message,"Can't open file <" + file + ">"


  index = HDF_SD_NAMETOINDEX(fileid, 'mappedpixels')
  id = HDF_SD_SELECT(fileid, index)
      
  HDF_SD_GETDATA,id,mappedpixels, start=0
  self.retdata.mappedpixels = ptr_new(mappedpixels,/no_copy)

  animpar = lonarr(3)
  idx =  hdf_sd_attrfind(fileid, 'num_x_pixels')
  hdf_sd_attrinfo, fileid, idx, data=data
  animpar[0] =  data[0]

  idx =  hdf_sd_attrfind(fileid, 'num_y_pixels')
  hdf_sd_attrinfo, fileid, idx, data=data
  animpar[1] =  data[0]

  idx =  hdf_sd_attrfind(fileid, 'num_frames')
  hdf_sd_attrinfo, fileid, idx, data=data
  animpar[2] =  data[0]

  idx =  hdf_sd_attrfind(fileid, 'center_longitude')
  hdf_sd_attrinfo, fileid, idx, data=data
  loncent =  data[0]

  self.retdata.animpar = animpar
  self.retdata.loncent = loncent

  HDF_SD_ENDACCESS, id
  HDF_SD_END, fileid

  return, self.retdata
END

;============================================
FUNCTION rsf::WriteMappedPixels, mappedPixels, animpar, loncent

  IF n_params() LT 3 THEN BEGIN 
    Message,"All 3 parameters <mappedPixels, animpar, loncent> are REQUIRED!",/CONT
    return,0
  ENDIF 

  catch,error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,0
  ENDIF 

  file = self.basename + 'mappedPixels.hdf'
  fileid = hdf_sd_start( file,/create)
  IF fileid LE 0 THEN $
    Message,"Can't open file <" + file + ">"

  id = hdf_sd_create(fileid, 'mappedpixels',$
                       [n_elements(mappedPixels)],/long)
  hdf_sd_adddata, Id, mappedPixels
  hdf_sd_endaccess, Id

  hdf_sd_attrset, fileid, 'num_x_pixels', animpar[0], /long
  hdf_sd_attrset, fileid, 'num_y_pixels', animpar[1], /long
  hdf_sd_attrset, fileid, 'num_frames', animpar[2], /long
  hdf_sd_attrset, fileid, 'center_longitude', loncent,/float


  hdf_sd_end,fileid
  return, 1
END


;============================================
PRO rsf::destroy_retdata, retdata
  ptr_free, retdata.landi
  ptr_free, retdata.landeli
  IF ptr_valid(retdata.mappedPixels) THEN $
    ptr_free, retdata.mappedPixels
  retdata = 0
END

;============================================
; Definition Routine
;============================================

PRO rsf__define
   junk = {rsf_retdata, $
           animpar      : lonarr(3), $
           loncent      : 0.0, $
           landi        : ptr_new(),$
           landeli      : ptr_new(), $
           mappedPixels : ptr_new() }
           
  junk = {rsf, $
          retdata        : {rsf_retdata}, $
          basename       : '', $
          animpar        : lonarr(3), $
          loncent        : 0.0 }
END
