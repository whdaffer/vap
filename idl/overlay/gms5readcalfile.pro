;+
; NAME:  Gms5ReadCalFile.pro
; $Id$
;
; PURPOSE:  Given a name of a cal file, read it.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Qscat Vap Gms 5 image processing
;
; CALLING SEQUENCE:  calstruct = Gms5ReadCalFile(filename)
; 
; INPUTS:  filename: a name of a GMS 5 cal file
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:   None
;
; OUTPUTS:  
;
;    Success: a structure containing the cal data. 
;    Failure: A non-structure containing nothing of interests beyond
;             it's non-structure-ness.
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  None
;
; RESTRICTIONS:  None
;
; PROCEDURE:  None
;
; EXAMPLE:  calstruct=Gms5ReadCalFile('9912301020.cal')
;
; MODIFICATION LOG:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION Gms5ReadCalFile, file


  COMMON gms5_cmn, gms5initialized, gms5_data_topdir, $
    gms5_hdftemplates_saveset_file

  IF n_params() LT 1 THEN BEGIN 
    Usage,'caldataStruct = read_GmsCalFile(file)'
    return,''
  ENDIF 

  IF NOT (isa(file,/string,/nonempty)) THEN BEGIN
    Message,"Parameter 'file' must be a scalar non-empty STRING",/cont
    return,''
  ENDIF 

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_State.msg,/cont
    return,''
  ENDIF 

  IF n_elements(gms5Initialized) EQ 0 THEN Gms5Init

  file = file[0]
  retstruct = ''
  openr, lun, file, /get_lun,error=err
  IF err EQ 0 THEN BEGIN 

    retstruct =  gms5cal_str(1)
;     IrData = { $
;                valid    : 0, $
;                beta     : fltarr(7), $
;                Gradient : 0., $
;                Intercept: 0., $
;                Bias     : 0., $
;                Temps    : fltarr(256) }



;     retstruct = { id:0, $
;                 date:0.0d, $
;                 sensel: 0, $
;                 Ir: replicate(IrData,3), $
;                 VisAlbedo: fltarr(64) }

    rec = ''
    l = 0l
    f = 0.
    d =  0.d

    iform = '(a44,i4)'
    fform = '(a44, f10.3)'
    dform = '(a44, d20.1)'
    readf, lun, rec,l, format=iform
    retstruct.id = l
    readf, lun, rec,d, format=dform
    retstruct.date = d
    readf, lun, rec,l, format=iform
    retstruct.sensel = l

    FOR ir=0,2 DO BEGIN 

      readf, lun, rec, l, format=iform
      retstruct.Ir[ir].valid = l

      FOR i=0,6 DO BEGIN 
        readf, lun, rec, f, format=fform
        retstruct.Ir[ir].beta[i] = f
      ENDFOR 

      readf, lun, rec, f, format=fform
      retstruct.Ir[ir].gradient = f

      readf, lun, rec, f, format=fform
      retstruct.Ir[ir].Intercept = f

      readf, lun, rec, f, format=fform
      retstruct.Ir[ir].Bias = f
      
    ENDFOR 

    FOR i=0,63 DO BEGIN 
      readf, lun, rec, f, format=fform
      retstruct.visAlbedo[i] = f
    ENDFOR 
    
    FOR ir=0,2 DO BEGIN 
      FOR i=0,255 DO BEGIN 
        readf, lun, rec, f, format=fform
        retstruct.Ir[ir].temps[i] =  f
      ENDFOR 
    ENDFOR 

    free_lun, lun
  ENDIF ELSE Message,!error_State.msg,/cont

  return, retstruct
 
END
