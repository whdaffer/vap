  pro GMSGOES, infile
;
; Author: Marion Legg, October 1994, adapted from regular GMSGOES.pro
; 
; Mods:
; 17 Ocotober 1994: Rennie Selkirk - directories now read in from .in file
;                                  - get_GOES streamlined
;
; 24 October 1994  Marion Legg for expanded longitude to 155 E.
;
; 16 March    1995: Marion Legg    - just gms data
  common combdat,  bytimage
  common colortab, red,green,blue
  common limits, blatmin, blatmax, blonmin, blonmax
;
  bytimage = intarr(1201,1201)
;
  TIMEGMS  = ''  &  GMSIMGDIR = ''  
  GMSMAPDIR = '' &  GMSCALDIR = ''
;
  CLUT_file  = ''
;
  IF n_params(0) LT 1 THEN BEGIN
    message,' need input file',/cont
    return
  ENDIF 
  openr,dlun,infile,/get_lun,error= err
  IF err THEN BEGIN
    message,!err_string,/cont
    return
  ENDIF 
;
; read time of GMS image
; Limits of big image
;
  blatmin  = -60.  &   blatmax  =  60.
  blonmin  =  80.  &   blonmax  = 200.
;
; read time of image
  readf,dlun,TIMEGMS
; read in image directory
  readf,dlun,GMSIMGDIR
; read in grid file directory
  readf,dlun,GMSMAPDIR
; read in calibration directory
  readf,dlun,GMSCALDIR
; read in lat lon limits of image you want to produce
  readf,dlun,minLat,maxLat
  readf,dlun,minLon,maxLon
; read in color table file
  readf,dlun, resdeg
  readf,dlun,CLUT_file
  print,CLUT_file
;
  free_lun, dlun
  print,'Year/month/date/time of GMS image: ', TIMEGMS
;
; Read in GMS image
;
  get_GMS, TIMEGMS, GMSIMGDIR, GMSMAPDIR,minlat,maxlat,minlon,maxlon,resdeg
;
; Composite the GMS images and deliver image in byte form
;
  combine_GMSGOES, TIMEGMS, GMSCALDIR, file=CLUT_file
;
; Call routine to save subsector of composited image
;
  print,minLat,maxLat, minlon, maxlon
  SAVE_GMSGOES,TIMEGMS,maxLat,minLat, minlon, maxlon, resdeg
;
  END

