;+
; NAME:  Gms5ReadVisFile.pro
; $Id$
; PURPOSE:  Read Visible from Scott Genari's GMS 5 files.
; 
; AUTHOR: William Daffer
; 
; CATEGORY:  Qscat VAP/GMS 5 image processing
; 
; CALLING SEQUENCE:  irdata_struct = Gms5ReadVisFile(filename)
; 
; INPUTS:  Filename: scalar string, Name of file to read
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  
;  Success: visdata_struct. Structure containing data to read.
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
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION Gms5ReadVisFile, filename

  COMMON gms5_cmn, gms5initialized, gms5_data_topdir, $
    gms5_hdftemplates_saveset_file

  COMMON gms5_templates_cmn, gms5gridtemplate, gms5imagetemplate

  IF n_params() LT 1 THEN BEGIN 
    Usage,'VisDataStruct=Gms5ReadVisFile(filename)'
    return,0
  ENDIF 

  retstruct = 0
  IF NOT (isa( filename,/string,/nonempty)) THEN BEGIN 
    Message,'Input Parameter "FILENAME" must be a non-empty STRING',/cont
    return,retstruct
  ENDIF 


  IF n_elements(gms5initialized) THEN gms5Init
  IF n_elements(gms5gridtemplate) EQ 0 THEN  restore,gms5_hdftemplates_saveset_file

  visdata = Hdf_Read(filename, template=gms5imagetemplate )

  return,visdata
END








