;+
; NAME: Read_Auto_Movie_Defs  
; $Id$
; PURPOSE: Reads the file $VAP_LIB/auto_movie_defs.dat
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  defs=Read_auto_movie_defs( roi )
;
;
; 
; INPUTS:  
;
;     desig - designation of the ROI, currently the roi's are
;             'NEPAC','NPAC','NWPAC','NWATL' and 'INDIAN'
;
;
;
; OPTIONAL INPUTS:   None
;
;
;	
; KEYWORD PARAMETERS:   None
;
;
;
; OUTPUTS:  A structure containing information about the ROI. Fields
;           The structures format is.
;
;           Roi={ desig       : '',$         ; Roi Designation
;                 alonpar     : fltarr(3), $ ; [minlon, minlat, lon_inc ]
;                 alatpar     : fltarr(3), $ ; [minlat, minlat, lat_inc ]
;                 wpath       : '' ,$        ; path to wind files
;                 interp_path :'' ,$;        ; Where to put the
;                                            ; interpolated file
;                 anim_path   : '', $        ; path to output frames and movie
;                 min_nvect   : 0l }         ; minimum number of WVC needed to do
;                                            ; animation
;
;  Example:
;
;   Nepac={ desig       : 'NEPAC'                 , $        
;           alonpar     : [195.,245,1.5]          , $
;           alatpar     : [30., 60,1.5]           , $
;           wpath       : "$VAP_WINDS"            , $
;           interp_path : "$VAP_ANIM/nepac"       , $
;           anim_path   : "$VAP_ANIM/nepac/daily" , $
;           min_nvect   : 4000                    }
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

FUNCTION read_auto_movie_defs, desig

  ; return the auto movie parameter structure that corresponds to the
  ; Region of interest (ROI) designator 'desig'
  ; Currently the only ROIs defined are NWPAC, NEPAC NPAC, NWATL and
  ; INDIAN

   rcsid = "$Id$"
  IF n_elements( desig ) EQ 0 THEN desig = 'nepac'
  desig =  strupcase(desig)
  OPENR, rlun,'$VAP_LIB/auto_movie_defs.dat',/get_lun, error=err
  found =  0
  IF err eq 0 THEN BEGIN 
    rec =  ''
    REPEAT BEGIN 
      readf, rlun, rec
      found =  (strpos( rec, desig ) NE -1 )
    ENDREP UNTIL found OR eof( rlun )
    IF eof(rlun)  AND NOT found THEN BEGIN 
      ret =  {desig:'ERRORNOSUCHROI'}
    ENDIF ELSE BEGIN 
      s =  execute( 'ret=' +  rec )
      IF NOT(s) THEN BEGIN 
        message,!err_string,/cont
        rec =  {desig:"ERRORINEXECUTE"}
      ENDIF 
    ENDELSE 
    free_lun, rlun

  ENDIF ELSE BEGIN
    message,!err_string,/cont
    ret =  {desig:'ERROROPENFAILURE'}
  ENDELSE 
RETURN, ret
END
