;+
; NAME:  gms5_overlay
; $Id$
; PURPOSE:  Overlay Wind data on GMS 5 data
;
;
; CALLING SEQUENCE:  gms5_overlay, datetime, gmstype
;                    windfiles   = windfiles, $ 
;                    xsize       = xsize, $     
;                    ysize       = ysize, $     
;                    CRDecimate  = CRDecimate,$
;                    Decimate    = Decimate, $
;                    ExcludeCols = ExcludeCols, $
;                    verbose     = verbose, $
;                    minpix      = minpix, $
;                    minspeed    = minspeed, $
;                    maxspeed    = maxspeed,$
;                    length      = length,$
;                    thick       = thick, $
;                    title       = title,$
;                    BrightMin   = BrightMin, $
;                    BrightMax   = BrightMax, $
;                    SatMin      = SatMin, $
;                    SatMax      = SatMax, $
;                    LandHue     = LandHue,$
;                    WaterHue    = WaterHue,$
;                    outfile     = outfile,$
;                    gif         = gif,$        
;                    ps          = ps, $        
;                    scalefac    = scalefac,$
;                    jpeg        = jpeg,$
;                    quality     = quality, $
;                    MapLimits   = MapLimits, $
;                    status      = status, $
;                    thumbnail   = thumbnail, $
;                    config      = config 
;
;
;
; 
; INPUTS:  
;
;   DateTime: String of form YYMMDDHHMM (e.g. 9812042322 ) detailing
;             the basename of the files needed to do the overlay.
;
;     gmsType     : scalar string, one of 'ir1','ir2','ir3', 'vis' (def='ir1')
;
; OPTIONAL INPUTS:  None
;
;
;       
; KEYWORD PARAMETERS:  
;
;     windfiles   : Vector of Strings, List of wind files  to
;                   read. Must be fully qualified filenames
;     xsize       : Xsize of resulting picture (def=640)
;     ysize       : ditto, y size (def=480)
;     CRDecimate  : Column/Row Decimate, 2 vector, CRdecimate=[2,3]
;                   means take every 2nd column, every 3rd
;                   row. (def=[1,1], i.e. take every vector.)
;     Decimate    : Decimate: scalare. decimate=m means take every
;                   m-th vector.
;     ExcludeCols : String, A comma seperated list of columns or
;                   ranges of columns to exclude, independently of 
;                   whatever is excluded by CRDecimate and Decimate. 
;                   Exclude='0,38:42,75' will exclude columns 0, 38 ;
;                   through 42, inclusive and column 75. Note, the
;                   string should be ; input using single quotes, as
;                   the IDL interpreter has problems with ; "n where n
;                   is a number. It thinks it's an octal number, and
;                   fails ; with a syntax error when it sees a comma
;                   or colon.
;
;     minpix      : pixels below this number are set to 0 in cloud mask
;     minspeed    : minimum speed (in Meters/sec) to display for wind vectors
;     maxspeed    : maximum speed (in Meters/sec) to display for wind vectors
;     length      : length of vectors (in IDL 'character' units)
;     thick       : Thickness of vectors (in IDL 'character' units)
;     title       : title for plot. This string is prepended to one 
;                   built by the routine, containing the date of the
;                   GMS5 file used.
;     Subtitle    : A subtitle. No subtitle will appear, if this
;                   string is absent.
;     outfile     : (I/O) if set to a non-empty string on input, the
;                   output file will have this name. If available for
;                   output, but not set to non-empty string, outfile
;                   will return the name of the output file
;
;     BrightMin   : The abscissa at which the brightness transfer
;                   function starts it's linear ramp to 1. The
;                   ordinates of the transfer function for abscissa
;                   less than this abscissa will be set to 0. Input as
;                   a float between 0 and 1 (default=0)
;     BrightMax   : The abscissa at which the brightness function
;                   reaches 1. The ordinates of the transfer function
;                   for abscissa greater than this one will be set to
;                   1. Input as float between 0 and 1. 
;                   (Default = 0.8)
;     SatMin      : The abscissa at which the Saturation transfer
;                   function starts its linear ramp to 0. Ordinates of
;                   the transfer function less than this one will be
;                   set to 1. Input as a float between 0 and 1. (Default=0.)
;     SatMax      : The abscissa at whith the saturation transfer
;                   function reachs 0. Ordinates of the transfer
;                   function for abscissa greater than this one are ;
;                   set to 0. Input as a float between 0 and
;                   1. (Default=0.55)
;     LandRGB     : The 3-vector containing the RGB values for the
;                   Land color. 
;     WaterRGB    : The 3-vector containing the RGB values for the
;                   Water color. 
;     LandHue     : the Hue value for land (from a Hue/Brightness/Saturation
;                   triple ) default will be whatever Hue results when
;                   the default LandRGB of [25, 110,   0] is converted
;                   to Hue
;     WaterHue    : the Hue value for water (from a
;                   Hue/Brightness/Saturation triple) Default will be
;                   whatever hue results when the default WaterRGB,
;                   triple of [31, 33, 129] is converted to Hue
;     gif         : output as Gif file 
;     scalefac    : scale factor to be used in making Postscript files.
;                   (def=0.05)
;     ps          : output as Postscript file
;     jpeg        : output file as Jpeg file (the default)
;     Quality     : Use this quality when making Jpeg files (def=75)
;     verbose     : flag, if set, emit many messages
;     MapLimits   : float 4-vector. Map limits having the form
;                   [lonmin, latmin, lonmax, latmax] where the first
;                   two are the lower left corner and the 2nd two the
;                   upper right. Default will be to grid the entire
;                   file, but this will be at a reduced resolution. 
;                   The maximum limits are:
;
;                    70<lon<200
;                    -60<lat<60
;
;                   If the Lon is in [80,200] the grid file in the
;                   'grid' subdirectory is used, otherwise the grid
;                   file in the 'grida' subdirectory will be used.
;
;     status      : 1 if successful, 0 otherwise
;     thumbnail   : if present for output, create a thumbnail that is
;                   30% the size of the original and output its name
;                   in this variable
;     config      : flag, if set, will call a overlay configurator
;
;
; OUTPUTS:  A file, either a .gif, .jpeg (the default) or a .ps file,
;          depending on the status of these three flags having either
;          a name built from the input goes file name (the default) or
;          whatever is passed in via the 'outfile' keyword. The
;          default output format is Gif and the default name is :
; 
;          GMS_5_mmm_yyyymmddThh:mm-%aaaa,bbb,cccc,ddd%.gif
;
;          where 
;          mmm = VIS, IR1,IR2,IR3
;          yyyymmdd = year, month, day, e.g. 19981202
;          hh:mm  = hour:minute
;          aaaa = minimum longitude, 4 digit, signed 
;          bbb  = minimum latitude, 3 digit, signed 
;          cccc = maximum longitude, 4 digit, signed 
;          ddd  = maximum latitude, 3 digit, signed 
;
;          The keyword 'outfile' will contain the output file name if
;          it (i.e. outfile) is present for output. If this quantity
;          is empty, there has been a failure. 
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  gms_overlay_cmn
;
;
;
; SIDE EFFECTS:  Vast reduction if memory, as this program uses ALOT
;
;
;
; RESTRICTIONS: A word about input file restrictions. This program
;               uses 4 files. Each file has the input parameter
;               'datetime' as it's basename. Thereafter they are
;               distinguished by their location within a preset directory
;               structure and, possibly, an extention. Assuming the
;               'datetime' is 9812042322 and the top of the GMS files
;               tree is /dir1/gms5, the four files are:
;
;               /dir1/gms5/doc/9812042322.txt.Z
;               /dir1/gms5/cal/9812042322.cal.Z
;               /dir1/gms5/grid/9812042322.hdf.Z 
;                  ( -or- /dir1/gms5/grida/9812042322.hdf.Z depending
;                     on the longitude limits)
;               /dir1/gms5/xxx/9812042322.hdf.Z 
;                   where xxx = ir1, ir2, ir3 or vis, depending on the
;               value of the 'gmstype' keyword.
;
;               If any of these files are missing, the routine will
;               fail.
;
;               
;
;
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.8  1999/06/23 22:00:23  vapuser
; Move the map_set until after config.
;
; Revision 1.7  1999/06/23 18:11:27  vapuser
; Added CONFIG keyword
;
; Revision 1.6  1999/06/21 14:50:52  vapuser
; Updated documentation
;
; Revision 1.5  1999/04/08 22:02:43  vapuser
; Replaced Colorbar with ColBar
;
; Revision 1.4  1999/04/08 20:19:12  vapuser
; Changed color24 to rgb2true
;
; Revision 1.3  1999/04/06 18:35:43  vapuser
; Changed default x/y size to defaulting xsize to 960 and
; ysize to whatever has same ratio as (long_range)/(lat_range)
; Also biased `bright' vector (bi) to >= 0.2.
;
; Revision 1.2  1999/04/02 17:50:44  vapuser
; Removed reference to calibration data. It's not required for
; what we're doing.
;
; Revision 1.1  1999/04/02 17:49:47  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-


PRO gms5_overlay, datetime, gmsType, $
                  windFiles   = WindFiles, $
                  xsize       = xsize, $     
                  ysize       = ysize, $     
                  CRDecimate  = CRDecimate,$
                  Decimate    = Decimate, $
                  ExcludeCols = ExcludeCols, $
                  verbose     = verbose, $
                  minpix      = minpix, $
                  minspeed    = minspeed, $
                  maxspeed    = maxspeed,$
                  length      = length,$
                  thick       = thick, $
                  title       = title,$
                  subtitle    = subtitle, $
                  outfile     = outfile,$
                  BrightMin   = BrightMin, $
                  BrightMax   = BrightMax, $
                  SatMin      = SatMin, $
                  SatMax      = SatMax, $
                  LandHue     = LandHue,$
                  WaterHue    = WaterHue,$
                  LandRGB     = LandRGB, $
                  WaterRGB    = WaterRGB,$
                  gif         = gif,$        
                  ps          = ps, $        
                  jpeg        = jpeg,$
                  scalefac    = scalefac, $
                  quality     = quality, $
                  mapLimits   = mapLimits,$
                  status      = status, $
                  thumbnail   = thumbnail, $
                  config      = config





  status = 1

  IF n_Params() EQ 0 THEN BEGIN 
    Usage, 'gms5_overlay, datetime, windFiles = WindFiles, gmsType = gmsType,windfiles = windfiles, xsize = xsize, ysize = ysize, CRDecimate = CRDecimate, Decimate = Decimate, ExcludeCols = ExcludeCols, verbose = verbose, minpix  = minpix, minspeed  = minspeed, maxspeed  = maxspeed,length  = length,thick = thick, title = title,subtitle = subtitle, outfile = outfile,BrightMin = BrightMin, BrightMax = BrightMax, SatMin = SatMin, SatMax = SatMax, LandHue = LandHue,WaterHue = WaterHue,LandRGB=landRGB, WaterRGB=WaterRGB, gif = gif, ps = ps, scalefac = scalefac, jpeg = jpeg,quality = quality'
    tvlct,orig_red,orig_green,orig_blue
    genv,/restore
    status = 0
    return
  ENDIF 

  config =  keyword_set(config)

  IF NOT isa(datetime,/string,/nonempty) THEN BEGIN 
    Message,"Input parameter DATETIME must be a nonempty STRING!",/cont
    status = 0
    return
  ENDIF 

  Default_WaterRGB = [31,  33, 129]
  Default_LandRGB =  [25, 110,   0]

  Color_Convert, Default_WaterRGB[0],Default_WaterRGB[1],Default_WaterRGB[2],$
   Default_WaterHue,l,s,/rgb_hls
  Color_Convert, Default_LandRGB[0],Default_LandRGB[1],Default_LandRGB[2],$
   Default_LandHue,l,s,/rgb_hls


  datetime_str = Gms5ParseDatetime(Datetime)
  IF NOT isa(datetime_str,/structure,name='GMS5DATETIME') THEN BEGIN 
    Message,'Bad Datetime! Datetime format= YYMMDDHHMM',/cont
    status = 0
    return
  ENDIF 
  IF n_elements(gmsType) EQ 0 THEN gmsType =  'ir1'

  IF n_elements(maplimits) LT 4 THEN $
     MapLimits =  [80,-60,200,60]

  mapLimits = 1.0*mapLimits
  lonlim = MapLimits[ [0,2] ]
  latlim = MapLimits[ [1,3] ]

  lonlim = fixlonrange(lonlim)
  
  IF lonlim[0] LT 70 OR lonlim[1] GT 200 OR $
     latlim [0] LT -60 OR latlim[1] GT 60 THEN BEGIN 
    Message,'MapLimits out of range!',/cont
    print,' mapLimits must be bound by [70,-60,200,60]'
    status = 0
    return
  ENDIF 

  IF n_elements(xsize ) EQ 0 AND $
     n_elements(ysize) EQ 0 THEN BEGIN 
    
    lonrange = lonlim[1]-lonlim[0]
    latrange = latlim[1]-latlim[0]
    aspectratio = float(latrange)/lonrange
    xsize = 960
    ysize = fix(xsize*AspectRatio)
    Message,"picture size defaulting to [" + $
      string( [xsize,ysize], form='(i4,",",i4)') + ']',/info
  ENDIF 


  ps = keyword_set(ps)
  gif = keyword_set(gif)
  jpeg = keyword_set(jpeg)

  verbose = keyword_set(verbose)

  IF ps AND gif  OR $
     ps AND jpeg OR $
     jpeg AND gif THEN BEGIN 
    Message,'Only one of PS or GIF or JPEG may be set',/cont
    tvlct,orig_red,orig_green,orig_blue
    genv,/restore
    status = 0
    return
  ENDIF 

  IF NOT (ps OR gif OR jpeg) THEN BEGIN 
    Message,' Defaulting to Jpeg output ',/info
    jpeg = 1
  ENDIF 

  IF n_elements(scalefac) EQ 0 THEN BEGIN 
    IF ps THEN scalefac = 0.05 ELSE scalefac = 1
  ENDIF


  IF n_Elements(minspeed) EQ 0 THEN minspeed = 2
  IF n_Elements(maxspeed) EQ 0 THEN maxspeed = 25
  IF n_elements(length)   EQ 0 THEN length = 2
  IF n_elements(thick)    EQ 0 THEN thick = 1
  IF n_elements(xsize)    EQ 0 THEN xsize = 960
  IF n_elements(ysize)    EQ 0 THEN ysize = 720
  IF n_elements(BrightMin) EQ 0 THEN BrightMin = 0.
  IF n_elements(BrightMax) EQ 0 THEN BrightMax = 0.8
  IF n_elements(SatMin) EQ 0 THEN SatMin = 0.0
  IF n_elements(SatMax) EQ 0 THEN SatMax = 0.55

  BrightMin = 0> float(BrightMin) < 1.
  BrightMax = BrightMin+.01> BrightMax < 1.

  SatMin = 0> float(SatMin) < 1.
  SatMax = SatMin+.01> SatMax < 1.

  IF n_elements(LandHue) EQ 0 THEN BEGIN 
    IF n_Elements(LandRGB) NE 3 THEN BEGIN 
      IF n_Elements(LandRGB) NE 0 THEN $
        Message,'LandRGB must be a 3-vector',/cont
      Message,'  Taking default Land Hue = ' + $
       '[' + strjoin( strtrim( Default_LandRGB,2)," ") + ']',/cont
      LandRGB = Default_LandRGB
    ENDIF 
    Color_Convert, LandRGB[0], LandRGB[1], LandRGB[2], LandHue,l,s,/rgb_hls
  ENDIF 

  IF n_elements(WaterHue) EQ 0 THEN BEGIN 
    IF n_Elements(WaterRGB) NE 3 THEN BEGIN 
      IF n_Elements(WaterRGB) NE 0 THEN $
        Message,'WaterRGB must be a 3-vector',/cont
      Message,'  Taking default Water Hue= ' + $
       '[' + strjoin( strtrim( Default_WaterRGB,2)," ") + ']',/cont
      WaterRGB = Default_WaterRGB
    ENDIF 
    Color_Convert, WaterRGB[0], WaterRGB[1], WaterRGB[2], WaterHue,l,s,/rgb_hls
  ENDIF 
  
  LandHue =  0> LandHue < 360.
  WaterHue =  0> WaterHue < 360.

  genv,/save
  tvlct,orig_red,orig_green,orig_blue,/get
  loadct,0,/silent
;  catch, error
;  IF error NE 0 THEN BEGIN 
;    catch,/cancel
;    Message,!error_state.msg,/cont
;    tvlct,orig_red,orig_green,orig_blue
;    genv,/restore
;    status = 0
;    return
;  END

  FOR i=0,1 DO BEGIN 
    device,get_visual_name= this_visual
    this_visual =  strupcase(this_visual)
    IF this_visual NE 'DIRECTCOLOR' AND  $
       this_visual NE 'TRUECOLOR' THEN BEGIN 
      IF i EQ 0 THEN device,true=24 ELSE BEGIN 
        Message,'Visual Class MUST be either DIRECTCOLOR or TRUECOLOR',/cont
        print,'  You must Exit and restart IDL'
        print,'  If you continue getting this error, look at you initilization'
        print," files and make sure you're not setting device,pseudo=8"
        Message,'ERROR',/cont
        status = 0
        return
      ENDELSE
    ENDIF 
  ENDFOR 



  start_time = systime(1)

  ptr = ReadColorTable($
       '$VAP_COLOR_TABLES/goes_overlay24.ct')
  IF NOT Ptr_Valid(ptr) THEN BEGIN 
    Message,'Error Reading ColorTable!',/cont
    tvlct,orig_red,orig_green,orig_blue
    genv,/restore
    status = 0
    return
  ENDIF 
  CT = *ptr
  ptr_free,ptr

  WIND_START = 1
  N_WIND_COLORS = 25
  N_COLORS = 27

  lonpar =  [ lonlim, ( lonlim[1]-lonlim[0] +1)/xsize ]
  latpar =  [ latlim, ( latlim[1]-latlim[0] +1)/ysize ]
  

  IF NOT ps  THEN BEGIN 
    set_plot,'x'
    window,/free, /pixmap, xsize=xsize, ysize=ysize
  ENDIF ELSE BEGIN 
    set_plot,'PS'
    ps_form = { XSIZE          : xsize   ,$ 
                XOFF           : xoffset ,$ 
                YSIZE          : ysize   ,$ 
                YOFF           : yoffset ,$ 
                INCHES         : 1       ,$  
                COLOR          : 1       ,$  
                BITS_PER_PIXEL : 8       ,$
                ENCAPSULATED   : 0       ,$
                LANDSCAPE      : 1        }
    device,_extra=ps_form
  ENDELSE 

  gmsType = strupcase(gmsType)
  CASE gmsType OF 
    'IR1':
    'IR2':
    'IR3':
    'VIS':
    ELSE: BEGIN 
      Message,$
       "Unknown gmsType!, must be one of 'IR1','IR2','IR3' or 'VIS'",/cont
      status = 0
      return
    END
  ENDCASE 

  allData = Gms5ReadAll(datetime,gmsType,lonpar=lonpar)
  IF NOT isa(alldata,/structure) THEN BEGIN 
    Message,"Failure Reading GMS5 data",/cont
    return
  ENDIF 
  image = allData.imagedata.image
  ;calTemps = allData.CalData.ir[0].Temps
  xloc = allData.griddata.xloc
  yloc = allData.griddata.yloc
  minlon = allData.griddata.minlon
  
  newGrid = gms5idx(lonpar,latpar)
  loni=newGrid.loni
  lati=temporary(newGrid.lati)
  newx=interpolate(temporary(xloc),loni,lati,/grid)
  newy=interpolate(temporary(yloc),$
                   temporary(loni),$
                   temporary(lati),/grid)

;  tempim= temporary( caltemps($
;                              temporary($
;                                        image($
;                                              temporary(newx),$
;                                              temporary(newy)))) )

  cloudmask = temporary(image($
                          temporary(newx),$
                          temporary(newy) ))


;  Map_Set, 0, mean(lonpar[0:1]), /noborder, $
;     limit=[ latpar[0], lonpar[0], latpar[1], lonpar[1] ],$
;      Ymargin=[4.,4];

    ; These two number come from the file
    ; spectrum06.256.950.350.325.CLUT which gmsgoes.pro and subsidiary
    ; .pro files use.

;  Tmin = -95.0000 + 273.15     
;  Tmax = -32.5000 + 273.15

;  cloudmask = bytscl(temporary(tempim),min=tmin,max=tmax)

;  mapim=map_image(tempim,xs,ys,$
;                  lonmin=lonpar[0],$
;                  latmin=latpar[0],$
;                  lonmax=lonpar[1],$
;                  latmax=latpar[1],$
;                  /bilinear,/compress)


  time_string = $
    datetime_str.year + $
      datetime_str.month + $
       datetime_str.day + "T" + $
        datetime_str.hour+':'+ $
          datetime_str.min


  IF n_Elements(outfile) EQ 0 THEN BEGIN 

    lim_str =  "%" + $
       PadAndJustify(lonlim[0],4,pad='0',/right) + ','+$
        PadAndJustify(latlim[0],3,pad='0',/right) + ','+$
         PadAndJustify(lonlim[1],4,pad='0',/right) + ','+$
          PadAndJustify(latlim[1],3,pad='0',/right) + "%"
    dlm =  '_'


    ofileroot = 'GMS5' +  dlm + $
     gmsType + '_' + time_string + $
       '-' + lim_str

    CASE 1 OF 
      gif:  OutputFilename = ofileroot+'.gif'
      jpeg: OutputFilename = ofileroot+'.jpeg'
      ps:   OutputFilename = ofileroot+'.ps'
      ELSE: Message,"Job security!",/info
    ENDCASE 

  ENDIF ELSE outputFilename =  outfile

  IF ps THEN device,filename=OutputFilename

  outfile = OutputFilename

  sz = Size(cloudmask,/dim)

  lon0 = lonpar[0]
  lon1 = lonpar[1]
  loninc = lonpar[2]


  lat0 = latpar[0]
  lat1 = latpar[1]
  latinc = latpar[2]
  nlon =  fix((lon1-lon0)/loninc+1)
  nlat =  fix((lat1-lat0)/latinc+1)
  loni = (findgen(nlon)*loninc+lon0)#replicate(1.,nlat)
  lati = replicate(1.,nlon)#(findgen(nlat)*latinc+lat0)

  tt0 = systime(1)
  landmask = runLandMask(loni,lati)
  land = where(landmask)
  water = where(temporary(landmask) EQ 0)
  tt1 = systime(1)

  IF verbose THEN print,'Time for Landmask : ',tt1-tt0, ' Seconds '

  nlon = sz[0]
  nlat = sz[1]


 cloudmask = scale( temporary(cloudmask) )*99

 Hue = fltarr(nlon,nlat)+WaterHue
 Hue[land] = LandHue
   
 IF config THEN $
    CLOUD_OVERLAY_CONFIG, $
     landwater=hue, $
       cloudmask=cloudmask, $
        brightmin=brightmin, $
         brightmax=brightmax, $
          satmin=satmin, $
           satmax=satmax, $
            lonmin=lonlim[0], $
             lonmax=lonlim[1], $
              latmin=latlim[0],$
                latmax=latlim[1]


  Map_Set, 0, mean(lonpar[0:1]), /noborder, $
     limit=[ latpar[0], lonpar[0], latpar[1], lonpar[1] ],$
      Ymargin=[4.,4]

    ; Define the new Brightness/Saturation mappings
  xx=findgen(100)/99.

  bi = 0.2> interpol( [0.,1], [BrightMin, BrightMax], xx ) < 1
  si = 0> interpol( [1.,0], [SatMin,    SatMax],xx ) < 1

    ; Use 'cloudmask' to create new Brightness/Saturation values

  b2=bi[cloudmask]
  s2=si[temporary(cloudmask)]
  cloudmask = 0

    ; Substitute these new Brightness/Saturation values in for those in
    ; mapIm and convert back to RGB 

  Color_Convert, Hue,b2,s2, imr, img, imb, /hls_rgb
  Hue = 0
  b2 = 0
  s2 = 0
  Im = [ [[temporary(imr)]], [[temporary(img)]], [[temporary(imb)]] ]

  FOR i=0,2 DO BEGIN 
    tmpIm = Map_Image( Im[*,*,i], $
                       xs,ys,xsize,ysize,$
                       lonmin=lonlim[0],$
                       latmin=latlim[0],$
                       lonmax=lonlim[1],$
                       latmax=latlim[1],$
                       scale=scalefac, /compress, /bilinear )


    IF i EQ 0 THEN BEGIN 
      dim = size(tmpIm,/dim)
      mapIm = bytarr(dim[0], dim[1], 3)
    ENDIF 
    mapIm[*,*,i] =  temporary(tmpIm)
  ENDFOR 
  im = 0


    ; Tv the final PS image
  IF ps THEN BEGIN 
    Tv,mapIm,xs,ys,xsize=xsize,ysize=ysize,true=3 
  ENDIF ELSE BEGIN 
    Tv,mapIm,xs,ys,true=3
  ENDELSE 


    ; Calculate where to put Colorbar
  mapIm = temporary(mapIm[*,*,0])
  sz = size( mapIm,/dim)
  xyz = Convert_Coord( 0, ys+sz[1]/scalefac,/device,/to_normal)
  y = xyz[1]
  y = [3*y+2, 2*y+3]/5

  mapIm = 0; free memory


  IF n_elements(Windfiles) NE 0 THEN BEGIN 
    tt0 = systime(1)
    windData = Read_Wind_Files(windFiles,$
                               CRDecimate=CRDecimate,$
                               Decimate=Decimate,$
                               ExcludeCols=ExcludeCols)
    tt1 = systime(1)
    IF verbose THEN print,' Read_wind_Files took: ', tt1-tt0,$
      ' Seconds '

    ndims = size(windData,/n_dim)
    IF ndims NE 2 THEN BEGIN 
      Message,'Error reading data from windfiles ',/cont
      print,'Input Windfiles are ', transpose(windFiles)
      print,'Continuing with overlay without the Wind Data!'
    ENDIF ELSE BEGIN 
      u = windData[*,0]
      v = windData[*,1]
      lon = windData[*,2]
      lat = windData[*,3]
      good = where( finite(u) AND finite(v), ngood )
      IF ngood NE 0 THEN BEGIN 
        u = u[good]
        v = v[good]
        lon = lon[good]
        lat = lat[good]
        speed = sqrt( u^2+v^2)
        good = where( speed NE 0, ngood )
        IF ngood NE 0 THEN BEGIN 
          IF ngood NE n_elements(u) THEN BEGIN 
            u = u[good]
            v = v[good]
            lon = lon[good]
            lat = lat[good]
            speed = seepd[good]
          ENDIF 
          veccol = BytScl( speed, min=minspeed, $
                           max=maxspeed, $
                           top=N_WIND_COLORS-1) + $
                           WIND_START
          col24 = rgb2true( veccol, colortable=ct)
          t0 = systime(1)

        ENDIF 
      ENDIF 
    ENDELSE 

  ENDIF   

    ; Plot the vectors (if there are any.)
  nn = n_Elements(u)
  IF nn GT 1 THEN BEGIN 
    IF ps THEN BEGIN 
      tvlct,orig_red,orig_green,orig_blue,/get
      tvlct,transpose(ct)
      PlotVect,u,v,lon,lat,len=length,$
        thick=thick,start_index=WIND_START,ncolors=N_WIND_COLORS
      tvlct,orig_red,orig_green,orig_blue
    ENDIF ELSE $
      PlotVect, u,v,lon,lat, color=col24, len=length, thick=thick
    t1 = systime(1)
    IF verbose THEN print,' Plotvect took: ', t1-t0,' Seconds '
    t0 = t1
  ENDIF 


  IF ps THEN BEGIN 
    ColBar, bottom=Wind_Start, nColors=N_Wind_Colors,$
     position=[0.25,y[0], 0.75, y[1]], $
     Title='Wind Speed (knots)',Min=minspeed, $
     max=maxspeed,divisions=4, format='(f5.0)', $
     pscolor=ps, /true, table=ct, charsize=0.75
  ENDIF ELSE BEGIN 
    ColBar, bottom=Wind_Start, nColors=N_Wind_Colors,$
     position=[0.25,y[0], 0.75, y[1]], $
     Title='Wind Speed (knots)',Min=minspeed, $
     max=maxspeed,divisions=4, format='(f5.0)', $
     pscolor=ps, /true, table=ct, charsize=0.75, $
     color='ffffff'x 
  ENDELSE 


  gms5_string =  'GMS5 Overlay Test'
  gms5_string =  'Gms5' + gmsType + '(' + time_string + ')'
  IF n_elements(title) NE 0 THEN $
    Title= title + ' ' + gms5_string ELSE $
    Title= gms5_string 

  IF ps THEN text_color = '000000'x ELSE text_color = 'ffffff'x
  xyz = Convert_Coord(0,ys,/device,/to_normal)
  y = xyz[1]
  xyouts, 0.5, y/2., title, align=0.5, $
      /normal, charsize=1.05, color=text_color



  CASE 1 OF 
    gif: BEGIN 
      ;If output is 'gif' quantize it down to 175 colors.
      tt0 = systime(1)
      tmpim = tvrd(true=3)
      im = color_Quan( tmpim, 3, r,g,b, colors=175 )
      tt1 = systime(1)
      IF verbose THEN print,' Color_Quan took: ', tt1-tt0,' Seconds '
      tt0 = tt1

      Write_Gif, OutputFilename, im, r,g,b
      tt1 = systime(1)
      IF arg_present(thumbnail) THEN  BEGIN 
        dims = size(im,/dimension)
        thumbnail = OutputFilename + '.TN'
        thumbnailIm =  congrid( im, dims[0]*0.3, dims[1]*0.3)
        Write_Gif, thumbnail, temporary(thumbnailIm), r,g,b
      ENDIF 
      IF verbose THEN print,' Write_Gif took: ', tt1-tt0,' Seconds '
      tt0 = tt1
      Wdelete,!d.window

    END

    jpeg: BEGIN 
      tmpim = tvrd(true=3)
      Write_Jpeg, OutputFilename, tmpim, $
             quality=quality, true=3
      IF arg_present(thumbnail) THEN  BEGIN 
        dims = size(tmpim,/dimen)
        thumbnailIm = congrid( tmpim,  dims[0]*0.3, dims[1]*0.3,dims[2] )
        thumbnail = OutputFilename + '.TN'
        Write_Jpeg, thumbnail, temporary(thumbnailIm), $
         quality=quality, true=3
      ENDIF 
      Wdelete,!d.window

    END

    ps: device,/close

  ENDCASE
  Outfile = OutputFileName

   
END


