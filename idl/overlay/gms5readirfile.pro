;+
; NAME:  Gms5ReadIrFile.pro
; $Id$
; PURPOSE:  Read IR 1/2/3 data from Scott Genari's GMS 5 files.
; 
; AUTHOR: William Daffer
; 
; CATEGORY:  Qscat VAP/GMS 5 image processing
; 
; CALLING SEQUENCE:  irdata_struct = Gms5ReadIRFile(filename)
; 
; INPUTS:  Filename: scalare string, Name of file to read
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  
;  Success: irdata_struct. Structure containing data to read.
;  Failure: a scalar 0.
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  A compressed HDF file may get uncompressed
;
; RESTRICTIONS: 
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION LOG:
;
; $Log$
; Revision 1.3  1999/10/05 17:28:22  vapuser
; Cosmetic work
;
; Revision 1.2  1999/06/24 18:45:57  vapuser
; Put in a wayward 'EQ 0'
;
; Revision 1.1  1999/04/02 18:03:59  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION Gms5ReadIRFile, filename

  COMMON gms5_cmn, gms5initialized, gms5_data_topdir, $
    gms5_hdftemplates_saveset_file

  COMMON gms5_templates_cmn, gms5gridtemplate, gms5imagetemplate

  IF n_params() LT 1 THEN BEGIN 
    Usage,'IrDataStruct=Gms5ReadIrfile(filename)'
    return,0
  ENDIF 

  retstruct = 0
  IF NOT (isa( filename,/string,/nonempty)) THEN BEGIN 
    Message,'Input Parameter "FILENAME" must be a non-empty STRING',/cont
    return,retstruct
  ENDIF 


  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,0
  ENDIF 

  IF n_elements(gms5initialized) EQ 0 THEN gms5Init
;  IF n_elements(gms5imagetemplate) EQ 0 THEN  $
;    restore,gms5_hdftemplates_saveset_file

;  irdata = Hdf_Read(filename, template=gms5imagetemplate )

  hdf_dfr8_getimage,filename,image,palette 
  irdata =  { filename: filename, image: image, palette: palette, date: systime() }

  return,irdata
END








