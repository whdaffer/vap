;+
; NAME:  Gms5ReadAll.pro
; $Id$
;
; PURPOSE: Given a datetime and possible lonpar array, read
;          *all* the data needed to create a gms5 overlay and munge it
;          accordingly.
;
; AUTHOR: William Daffer
;
; CATEGORY:  Qscat VAP/ GMS 5 image processing
;
; CALLING SEQUENCE:  allDataStruct = Gms5ReadAll(datetime[,
;                                                lonpar=lonpar ])
; 
; INPUTS:  
;
;   datetime: scalar string of form yymmddhhmm.
;
; OPTIONAL INPUTS:  None
;       
; KEYWORD PARAMETERS:  
;
;   lonpar : float 2 vector [minLon, maxLon]
;
; OUTPUTS:  
;
;   Success: Structure containing all data pertanent to this
;            datetime/type/longitude range. See the file
;            gms5data_str.pro for a description of the output data
;            structure.
;
;   Failure: a scalar 0.
;
; 
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS: Some of the files (the grid file and the image data
;               file, most likely) may be uncompressed. 
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
FUNCTION Gms5ReadAll, datetime, type, lonpar=lonpar

  IF n_Params() LT 1 THEN BEGIN 
    Usage,'AllData=Gms5ReadAll(datetime[,type[,lonpar=lonpar]])'
    return,''
  ENDIF 
  IF NOT isa( datetime, /string,/nonempty) THEN BEGIN 
    Message,'Parameter DATETIME must be NON-EMPTY STRING',/cont
    return,''
  ENDIF 

  IF exist( type ) THEN BEGIN 
    IF NOT isa( type, /string,/nonempty) THEN BEGIN 
      Message,'Parameter TYPE must be NON-EMPTY STRING',/cont
      return,''
    ENDIF 
  ENDIF ELSE type =  'ir1'

  IF n_elements(lonpar) GE 2 THEN BEGIN 
    tlonpar = fixlonrange(lonpar)
    IF tlonpar[0] LT 80 THEN BEGIN 
      grid = 'grida'
      vetted =  Gms5VetMe( type, /grida)
    ENDIF ELSE BEGIN 
      grid = 'grid'
      vetted =  Gms5VetMe( type, /grid)
    ENDELSE 
  ENDIF ELSE BEGIN 
    grid = 'grid'
    vetted = Gms5VetMe( type, /grid)
  ENDELSE 
  datastruct = 0

  IF vetted THEN BEGIN 
    caldata = Gms5ReadItem(datetime,'Cal')
    IF NOT isa(caldata,/struct,name='GMS5CALDATA') THEN BEGIN 
      Message,"Can't get CAL Data!",/cont
      return,0
    ENDIF 
      
    ;DocData = Gms5ReadItem(datetime,'Doc')
    Grid    = Gms5ReadItem(datetime,grid)
    IF NOT isa(grid,/struct,name='GMS5GRID') THEN BEGIN 
      Message,"Can't get GRID Data!",/cont
      return,0
    ENDIF 

    image    = Gms5ReadItem(datetime,type)
    IF NOT isa(image,/struct,name='GMS5IMAGE') THEN BEGIN 
      Message,"Can't get IMAGE Data!",/cont
      return,0
    ENDIF 

      ;Create and load the output struct!

    datastruct = gms5Data_Str()

    datastruct.caldata   = temporary(caldata)
    datastruct.griddata  = temporary(grid)
    datastruct.imagedata = temporary(image)


  ENDIF 

RETURN, datastruct
END


