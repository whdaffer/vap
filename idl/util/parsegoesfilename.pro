;+
; NAME:  ParseGoesFilename
; $Id$
; PURPOSE:  Parse the filename of a Goes Gridded file
;
; AUTHOR:  WHD
;
; CATEGORY:  Qscat/Seawinds Vap
;
; CALLING SEQUENCE:  parsedFilenameStruct=ParseGoesFileName(filename)
; 
; INPUTS:  
;
;  Filename : The filename (with or without path)
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  
;   Failure: '', a null string
;   Success: A structure having the following form.
;
;     retstruct =  { SatName: '', $
;                    Satnum: 0L, $
;                    SensorNum: 0l, $
;                    year: 0L, $
;                    mm:0L, $
;                    dd:0L, $
;                    hh:0L, $
;                    limits: fltarr(4) } ; [lonmin, latmin, lonmax,latmax]
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  Parse a file having the following template.
;             1         2         3         4
;   012345678901234567890123456789012345678901
;   GOESUUV-YYYYMMDDHH-%xxx0,yy0,xxx1,yy1%.dat
;
; where...
;
;   UU = 08 or 10, for goes 8/10, respectively
;   V = sensor number, 1=vis, 2=ir2, 3=ir3, 4=ir4
;   all of the xxx{0,1},yy{0,1} are right justified signed 0 padded
;   integers, with
;
;   xxx0 is a 4 digit starting longitude
;   yy0  is a 3 digit starting latitude
;   xxx1 is a 4 digit ending longitude
;   yy1  is a 3 digit ending latitude
; 

;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1998/11/20 20:11:41  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION ParseGoesFileName, filename
  retstruct = ''
  catch,error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!err_string + " (filename/template mismatch??)",/cont
    Message,'   Template: GOESUUV-YYYYMMDDHH-%xxx0,yy0,xxx1,yy1%.dat',/cont
    return,''
  ENDIF 

  IF N_Params() EQ 0 THEN BEGIN 
    Message,'Usage: retstruct=ParseGoesFileName(filename)',/cont
  ENDIF ELSE IF VarType(filename) NE 'STRING' THEN BEGIN 
    Message,'Filename must be of type STRING',/cont
  ENDIF ELSE BEGIN 
      ; Strip off leading path, if any
    junk = strpos( filename,'/',/reverse_search)
    IF junk NE -1 THEN $
      basename = strmid(filename,junk+1,strlen(filename)-junk-1) ELSE $
      basename = filename
     
      ;           1         2         3         4
      ; 012345678901234567890123456789012345678901
      ; GOESXXY_YYYYMMDDHH_%xxx0,yy0,xxx1,yy1%.dat
      ;
    SatName   = strmid( basename, 0, 4)
    SatNum    = long( strmid( basename, 4,2 ))
    Sensornum = long( strmid( basename, 6,1 ))
    year      = long( strmid( basename, 8,4 )) 
    mm        = long( strmid( basename, 12,2 ))
    dd        = long( strmid( basename, 14,2 ))
    hh        = long( strmid( basename, 16,2 ))
    lonmin    = float(strmid( basename, 20,4 ))
    latmin    = float(strmid( basename, 25,3 ))
    lonmax    = float(strmid( basename, 29,4 ))
    latmax    = float(strmid( basename, 34,3 ))

    retstruct =  { SatName: SatName, Satnum: satnum, SensorNum: SensorNum, $
                   year:year, mm:mm, dd:dd, hh:hh, $
                   limits: [ lonmin, latmin, lonmax, latmax] }
  ENDELSE 
  
  RETURN,retstruct
END 
