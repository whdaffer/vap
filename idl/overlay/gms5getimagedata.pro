;+
; NAME:  Gms5GetImageData.pro
; $Id$
;
; PURPOSE: Given a 'datetime,' a type and a top directory, read all the data
;          necessary to produce an overlay using the GMS5 data for
;          that 'datetime' 
;
; AUTHOR: William Daffer
;
; CATEGORY:  Qscat VAP/GMS 5 image processing
;
; CALLING SEQUENCE: 
;
;    image_data = Gms5GetImageData( datetime, type, topdir =topdir,
;                                lonpar=lonpar, latpar=latpar )
; 
; INPUTS:  
;
;    DateTime: String of for yymmddhhmm. Required
;
; OPTIONAL INPUTS:  Type: ir1/ir2/ir3/vis. Default=ir3.
;       
; KEYWORD PARAMETERS: 
;
;     Topdir: A scalar string. The root directory of the GMS
;             directories. The directory structure is a simplified
;             version of the directory structure at Scott Genari's
;             archive site. The required subdirectories are.
;
;                   Grid:  Contains the Grid information.
;                   GridA: A second grid, shifted slightly westward.
;                   Cal:   Contains the calibration information
;                   Doc:   Some Documentation.
;                   Ir1:   The Ir1 Data
;                   Ir2:   The Ir2 Data
;                   Ir3:   The Ir3 Data
;                   Vis:   The Vis Data
;
;    LonPar: a float 3-vector. [MinLon, MaxLon, Lon Increment] This
;            determines the section of the image you want
;            returned. Must be in the range of [75,200]. Will default
;            to [??,??,??]
;
;    LatPar: a float 3-vector. [MinLat, MaxLat, Lat Increment]. The
;            Latitudinal version of LonPar. Will default to [??,??,??]
;
; OUTPUTS: A structure containing the image data, as well as other
;          things I haven't thought of yet, interpolated according to
;          the (possibly defaulted) LonPar and LatPar.
;   
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  One or more of the HDF files may be uncompressed. See
;               the routine 'GetUncompressedGms5File.pro' 
;
; RESTRICTIONS:  Longitude must be in the range of [75,200]
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

FUNCTION Gms5GetImageData, datetime, type, $
                           topdir=topdir, $
                           LonPar=LonPar, $
                           LatPar=LatPar

  COMMON gms5_cmn, gms5initialized, gms5_data_topdir, $
    gms5_hdftemplates_saveset_file

  retstruct = 0
  IF n_Params() LT 1 THEN BEGIN 
    str =  "retstruct = Gms5GetImageData(datetime [,type, topdir=topdir, " + $
           "LonPar=LonPar, LatPar=LatPar])"
    Usage,str
    return,0
  ENDIF 

  IF NOT isa( datetime, /string,/nonempty) THEN BEGIN 
    Message,"Input parameter 'datetime' must be a non-empty STRING",/CONT
    return,0
  ENDIF 

  IF n_elements(gms5initialized) EQ 0 THEN gms5init

  cd, current=startDir  

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    cd,startDir
    return,retstruct
  ENDIF 
  
  IF n_Elements(type) EQ 0 THEN BEGIN 
    Message,'Defaulting to Ir1',/info
    type = 1
  ENDIF 
  
  CASE type OF 
    1: gmsTypeString = 'IR1'
    2: gmsTypeString = 'IR2'
    3: gmsTypeString = 'IR3'
    4: gmsTypeString = 'VIS'
    ELSE: BEGIN 
      MESSAGE,'Unknown GmsType! ',/cont
      return,retstruct
    END
  ENDCASE 

  IF n_Elements(topdir) EQ 0 THEN topdir = gms5_data_topdir

  cd, topdir
  whole_enchilada = Gms5ReadAll(datetime, Lonpar=Lonpar, Latpar=Latpar )

  cd,startDir

  retstruct = data
  return,retstruct
END

    
