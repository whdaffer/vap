;+
; NAME:  goes_overlay24
; $Id$
; PURPOSE:  does Goes overlays in true color
;
;
; AUTHOR:  William Daffer
;
;
; CATEGORY:  Qscat Vap
;
;
;
; CALLING SEQUENCE:  goes_overlay24, goesfile, 
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
;                    LandRGB     = LandRGB,$
;                    WaterRGB    = WaterRGB,$
;                    LandHue     = LandHue,$
;                    WaterHue    = WaterHue,$
;                    outfile     = outfile,$
;                    gif         = gif,$        
;                    ps          = ps, $        
;                    scalefac    = scalefac,$
;                    jpeg        = jpeg,$
;                    quality     = quality
;
;
;
; 
; INPUTS:  
;
;   goesfile: A gridded Goes file, as output by Paul Chang's Goes
;             Gridding program.
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
;                   built by the routine, containing the data of the
;                   Goes file used.
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
;                   Land color
;     WaterRGB    : The 3-vector containing the RGB values for the
;                   Water color
;
;     LandHue     : the Hue value for land (from a Hue/Brightness/Saturation
;                   triple )
;     WaterHue    : the Hue value for water (from a
;                   Hue/Brightness/Saturation triple
;
;
;     gif         : output as Gif file 
;     scalefac    : scale factor to be used in making Postscript files.
;                   (def=0.05)
;     ps          : output as Postscript file
;     jpeg        : output file as Jpeg file (the default)
;     Quality     : Use this quality when making Jpeg files (def=75)
;     verbose     : flag, if set, emit many messages
;     thumbnail   : if present, this will contain the bytarr with a
;                   thumbnail (30% of the size of the full image)
;                   Not applicable in the case of Postscript output.
;
;
;
;
;
; OUTPUTS:  A file, either a .gif, .jpeg (the default) or a .ps file,
;          depending on the status of these three flags having either
;          a name built from the input goes file name (the default) or
;          whatever is passed in via the 'outfile' keyword. The
;          default output format is Gif and the default name is :
; 
;          GOES_nn_mmm_yyyymmddThh:mm-%aaaa,bbb,cccc,ddd%.ext (ext=gif,jpeg,ps)
;
;          where 
;          nn = 8 or 10
;          mmm = VIS, IR1,IR2,IR3, OR IR4 
;          yyyymmdd = year, month, day, e.g. 19981202
;          hh:mm  = hour:minute
;          aaaa = minimum longitude, 4 digit, signed 
;          bbb  = minimum latitude, 3 digit, signed 
;          cccc = maximum longitude, 4 digit, signed 
;          ddd  = maximum latitude, 3 digit, signed 
;
;          The keyword 'outfile' will contain the output file name if
;          it (i.e. outfile) is  present for output.
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  goes_overlay_cmn
;
;
;
; SIDE EFFECTS:  Vast reduction if memory, as this program uses ALOT
;
;
;
; RESTRICTIONS:  
;
;
;
; PROCEDURE:  Way to complicated to describe here.
;
;
;
; EXAMPLE:  
;
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.5  1999/03/03 17:26:00  vapuser
; Took out landel stuff (don't need it anymore and it
; uses a *lot* of memory. Made some other memory usage
; optimizations. Test status after read_pcgoes.
;
; Revision 1.4  1998/12/07 20:10:31  vapuser
; Implemented new method of making overlays, using Lightness/Saturation. Added
; new keywords to control this, as well as some other keywords. Updated
; documentation.
;
; Revision 1.3  1998/12/02 00:32:33  vapuser
; New cloudmask for non-IR data.
;
; Revision 1.2  1998/11/25 22:37:04  vapuser
; Use Errorf to scale cloudmask
;
; Revision 1.1  1998/11/20 19:49:34  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
PRO goes_overlay24, goesfile, $
                    windfiles   = windfiles, $ 
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
                    LandRGB     = LandRGB,$
                    WaterRGB    = WaterRGB,$
                    LandHue     = LandHue,$
                    WaterHue    = WaterHue,$
                    gif         = gif,$        
                    ps          = ps, $        
                    scalefac    = scalefac, $
                    jpeg        = jpeg,$
                    quality     = quality, $
                    thumbnail   = thumbnail

; COMMON goes_overlay_cmn, landel


  Default_WaterRGB = [31,  33, 129]
  Default_LandRGB =  [25, 110,   0]

  Color_Convert, Default_WaterRGB[0],Default_WaterRGB[1],Default_WaterRGB[2],$
   Default_WaterHue,l,s,/rgb_hls
  Color_Convert, Default_LandRGB[0],Default_LandRGB[1],Default_LandRGB[2],$
   Default_LandHue,l,s,/rgb_hls

  genv,/save
  tvlct,orig_red,orig_green,orig_blue,/get
  loadct,0,/silent
;  catch, error
;  IF error NE 0 THEN BEGIN 
;    catch,/cancel
;    Message,!error_state.msg,/cont
;    tvlct,orig_red,orig_green,orig_blue
;    genv,/restore
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
        Message,'ERROR'
      ENDELSE
    ENDIF 
  ENDFOR 
  IF n_elements(goesfile) EQ 0 THEN BEGIN 
    Message,'Usage: goes_overlay24, goesfile [,windfiles=windfile, xsize=xsize, ysize=ysize, ps=ps | gif=gif | jepg=jpeg,title=title,subtitle=subtitle, CRDecimate=CRDecimate,Decimate=Decimate,ExcludeCols=ExcludeCols, verbose=verbose, minspeed=minspeed, maxspeed=maxspeed, length=length, thick=thick,BrightMin=BrightMin,BrightMax=BrightMax,SatMin=SatMin,SatMax=SatMax,LandRGB=LandRGB,WaterRGB=WaterRGB,LandHue=LandHue,WaterHue=WaterHue ] ',/cont
    tvlct,orig_red,orig_green,orig_blue
    genv,/restore
    return
  ENDIF 

  ps = keyword_set(ps)
  gif = keyword_set(gif)
  jpeg = keyword_set(jpeg)
  verbose = keyword_set(verbose)

  IF ps AND gif OR $
     ps AND jpeg OR $
     gif AND jpeg THEN BEGIN 
    Message,'Only one of PS, GIF  or JPEG may be set',/cont
    tvlct,orig_red,orig_green,orig_blue
    genv,/restore
    return
  ENDIF 

  IF NOT (ps OR gif OR jpeg) THEN BEGIN 
    Message,' Defaulting to jpeg output ',/info
    jpeg = 1
  ENDIF 

  IF n_elements( Quality ) EQ 0 THEN Quality = 75
  Quality =  1 > Quality < 100

  IF n_elements(scalefac) EQ 0 THEN BEGIN 
    IF ps THEN scalefac = 0.05 ELSE scalefac = 1
  ENDIF ELSE BEGIN 
    IF gif THEN BEGIN 
      Message,'Keyword SCALEFAC is ignored when creating GIFs/JPEGs',/cont
      scalfac = 1
    ENDIF 
  ENDELSE 

  IF n_Elements(minspeed) EQ 0 THEN minspeed = 2
  IF n_Elements(maxspeed) EQ 0 THEN maxspeed = 25
  IF n_elements(length)   EQ 0 THEN length = 2.5
  IF n_elements(thick)    EQ 0 THEN thick = 1
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
  
  start_time = systime(1)
  Read_PCGoes,goesfile,limits,GoesData,hdr=hdr, status=status

  IF NOT status THEN BEGIN 
    Message,'ERROR Reading Goesfile ' + goesfile,/cont
    return
  ENDIF 

    ; Make sure the longitude range is monotonic
  lonrange = FixLonRange( [ limits[0],limits[2] ])
  
  IF n_Elements(xsize) EQ 0 THEN BEGIN 
    IF ps THEN xsize = 8.4 ELSE xsize = 640 
    xoffset = 1.2
  ENDIF 

  IF n_Elements(ysize) EQ 0 THEN BEGIN 
    IF ps THEN ysize = 6.5 ELSE ysize = 480 
    yoffset = 9.5
  ENDIF 


  IF status THEN BEGIN 
    GoesFilenameStruct = ParseGoesFileName( goesfile )
    IF VarType( GoesFilenameStruct ) NE 'STRUCTURE' THEN BEGIN 
      Message," Trouble parsing " + Goesfile ,/cont
      tvlct,orig_red,orig_green,orig_blue
      genv,/restore
      return
    ENDIF ELSE BEGIN 
      sat_name = GoesFilenameStruct.SatName + " " + $
       strtrim(GoesFilenameStruct.SatNum,2 )
    ENDELSE 
    sensornum = GoesFilenameStruct.Sensornum
    sensors = ['VIS','IR2','IR3','IR4']
    sensor    =  sensors[ sensornum-1 ]
    goes_date = PadAndJustify(GoesFilenameStruct.year, 4, /right ) + $
                PadAndJustify(GoesFilenameStruct.mm, 2, /right ) + $
                PadAndJustify(GoesFilenameStruct.dd, 2, /right ) + $
                'T' + $
                PadAndJustify(GoesFilenameStruct.hh, 2, /right ) + $
                PadAndJustify(GoesFilenameStruct.mm, 2, /right )
    goes_string =  sat_name + ' ' + sensor + ' ('  + goes_date + ')'

    IR =  ( sensornum GT 1 )

    IF ir THEN GoesData = 1024-temporary(GoesData)
    IF N_elements(minpix) EQ 0 THEN minpix = 0

    ptr = ReadColorTable($
       '/usr/people/vapuser/Qscat/Resources/Color_Tables/goes_overlay24.ct')
    IF NOT Ptr_Valid(ptr) THEN BEGIN 
      Message,'Error Reading ColorTable!',/cont
      tvlct,orig_red,orig_green,orig_blue
      genv,/restore
      return
    ENDIF 
    CT = *ptr
    ptr_free,ptr

    WIND_START = 1
    N_WIND_COLORS = 25
    N_COLORS = 27

    IF n_elements(Windfiles) NE 0 THEN BEGIN 
      t0 = systime(1)
      windData = Read_Wind_Files(windFiles,$
                                 CRDecimate=CRDecimate,$
                                 Decimate=Decimate,$
                                 ExcludeCols=ExcludeCols)
      t1 = systime(1)
      IF verbose THEN print,' Read_wind_Files took: ', t1-t0,$
        ' Seconds '
      t0 = t1

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
              speed = speed[good]
            ENDIF 
            good = where( lon GE lonrange[0] AND $
                          lon LE lonrange[1] AND $
                          lat GE limits[1] AND $
                          lat LE limits[3], ngood )
            IF ngood NE 0 THEN BEGIN 
              u = u[good]
              v = v[good]
              lon = lon[good]
              lat = lat[good]
              speed = speed[good]
              veccol = BytScl( speed, min=minspeed, $
                               max=maxspeed, $
                               top=N_WIND_COLORS-1) + $
                                  WIND_START
              col24 = Rgb2True( veccol, colortable=ct)
            ENDIF ELSE BEGIN 
              u = 0
              v = 0
              lon = 0
              lat = 0
              speed = 0
              col24 = 0
            ENDELSE 
            t0 = systime(1)
            
          ENDIF 
        ENDIF 
      ENDELSE 

    ENDIF   

      ; read the land elevation file, if it isn't in the common
;    IF n_elements(landel) EQ 0 THEN BEGIN 
;      openr,lun,'$VAP_ROOT/animate/land_elevations.bin',/get,error=err
;      IF err NE 0 THEN BEGIN 
;        Message,'Error reading Land Elevation Database ',/cont
;        tvlct,orig_red,orig_green,orig_blue
;        genv,/restore
;        return
;      ENDIF 
;      landel =  intarr( 12l*360, 12l*180 + 1 )
;      readu,lun, landel
;      free_lun,lun
;    ENDIF 


    sz = size(GoesData,/dimensions)
    nlon = 1.0*sz[0]
    nlat = 1.0*sz[1]

    lonmin = lonrange[0]
    latmin = limits[1]
    loninc = (lonrange[1]-lonrange[0])/nlon
    latinc = (limits[3]-limits[1])/nlat
    loni = (findgen(nlon)*loninc+lonmin)#(replicate(1.,nlat))
    lati = replicate(1.,nlon)#(findgen(nlat)*latinc+latmin)

    t0 = systime(1)
    land = where( runLandMask(loni,lati) )
    t1 = systime(1) 
    IF verbose THEN print,'Time for Landmask : ',t1-t0, ' Seconds '
    t0 = t1

;    x = where(loni LT 0, nx )
;    IF nx NE 0 THEN loni[x] =  loni[x] + 360.
;    topoIm = landel[ loni*12, (lati+90)*12 ]

    t1 = systime(1) 
    IF verbose THEN print,'Time for TopoIm : ',t1-t0, ' Seconds '
    t0 = t1

    loni = 0
    lati = 0

    ; cloudmask = bytscl(sqrt(1.0*(minpix>data<1024 )/1024),top=200) 
;    IF IR THEN $
;      cloudmask = byte(255*(0.5*(errorf(4.d*scale(data,max=1024,/double)-2)+1))) $
;    ELSE $
;      cloudmask =  bytscl(data,min=0)


    IF ps THEN BEGIN 
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
      ; device,font='
    ENDIF ELSE BEGIN 
      set_plot,'x'
      ; device,set_resolution=[xsize,ysize],z_buff=0
      window,/free, /pixmap, xsize=xsize, ysize=ysize
      pixmap = !d.window
      !p.font = 0
      device,font='7x14bold'
    ENDELSE 

    loncent = mean(lonrange)
    Map_Set,0,loncent,$
     limit=[ limits[1],lonrange[0],limits[3],lonrange[1] ],/noborder,$
      Ymargin=[4.,4]


    IF n_Elements(outfile) EQ 0 THEN BEGIN 

      t = long([lonrange[0],limits[1],lonrange[1],limits[3]])
      lim_str =  '%'+ StrJoin(t,',') +  '%'
      dlm =  '_'

      year =  PadAndJustify(hdr.year, 4 )
      tmp = doy2date(hdr.year,hdr.doy)
      month = tmp[0]
      dom = tmp[1]
      hh = hdr.hhmm/100
      mm = hdr.hhmm-hh*100
      hh = PadAndJustify(hh,2,/right)
      mm = PadAndJustify(mm,2,/right)


      time_string = year + month + dom + "T" + hh+':'+mm


      ofileroot = strtrim( sat_name, 2 ) +  dlm + $
       sensor + '_' + time_string


      sp =  strpos( ofileroot,' ' )
      strput, ofileroot, '_', sp
      ofileroot =  ofileroot + '-' + lim_str
      IF keyword_set( file_str ) THEN BEGIN 
         IF strlen( file_str[0] ) GT 0 THEN BEGIN 
           s =  str_sep( file_str,' ' )
           tt =  '_' + s(0)
           FOR i=1,n_elements(s)-1 DO tt =  tt + '_' + s(i)
           ofileroot =  ofileroot + tt
         ENDIF 
      ENDIF 

      CASE 1 OF 
        gif : ext = '.gif'
        jpeg: ext = '.jpeg'
        ps  : ext = '.ps'
      ENDCASE  
      OutputFilename = ofileroot + ext

    ENDIF ELSE outputFilename =  outfile

    IF ps THEN device,filename=OutputFilename

;    ;tmpGoesData = bytscl(GoesData)
;    cloudmask =   scale( map_image( temporary(GoesData), $
;                           xs,ys,xsize,ysize,$
;                            lonmin=lonrange[0],$
;                             latmin=limits[1],$
;                               lonmax=lonrange[1],$
;                                 latmax=limits[3],$
;                                   scale=scalefac, /compress, /bilinear ))

;    FOR channel=0,2 DO BEGIN 
;      image = intarr(nlon,nlat)+WaterRGB[channel]
;      image[land] =  LandRGB[channel]
;;      image[land] = reform( $
;;          
;;           CT[ channel, $
;;                       LAND_START>(topoIm[land]+LAND_START)<(WIND_START-1) ] )
;;      commented out, 98/11/25, whd
;;      image = 0> (image-cloudmask)
;;      image = image + cloudmask
;;      image =  byte(image <  255)

;;      m = max(cloudmask)
;;      image = image-m/4.
;;      image = byte( 0> (image + cloudmask) < 255)

;      tmpIm =   map_image( image, xs,ys,xsize,ysize,$
;                         lonmin=lonrange[0],$
;                         latmin=limits[1],$
;                         lonmax=lonrange[1],$
;                         latmax=limits[3],$
;                         scale=scalefac, /compress, /bilinear )
;      IF channel EQ 0 THEN BEGIN 
;        imsz = size(tmpIm,/dim)
;        mapIm = bytarr(imsz[0],imsz[1],3)
;      ENDIF 
;      mapIm[*,*,channel] = tmpIm
;      tmpIm = 0l
;    ENDFOR 

;      ; scale cloud mask into [0,1]
;;    cloudmask =  scale( data, min=0, max=1024)*100
;      ; Convert mapIm to HLS
;    Color_Convert, mapIm[*,*,0],mapIm[*,*,1],mapIm[*,*,2],h,l,s,/rgb_hls
;    mapIm = 0; free some memory


   cloudmask = scale( temporary(GoesData) )*99
   Hue = fltarr(nlon,nlat)+WaterHue
   Hue[land] = LandHue

      ; Define the new Brightness/Saturation mappings
    xx=findgen(100)/99.

    bi = 0> interpol( [0.,1], [BrightMin, BrightMax], xx ) < 1
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
                         lonmin=lonrange[0],$
                         latmin=limits[1],$
                         lonmax=lonrange[1],$
                         latmax=limits[3],$
                         scale=scalefac, /compress, /bilinear )

      
      IF i EQ 0 THEN BEGIN 
        dim = size(tmpIm,/dim)
        mapIm = bytarr(dim[0], dim[1], 3)
      ENDIF 
      mapIm[*,*,i] =  temporary(tmpIm)
    ENDFOR 
    im = 0

      ; Tv the final PS image 
    IF ps THEN $
      Tv,mapIm,xs,ys,xsize=xsize,ysize=ysize,true=3 ELSE $
      Tv,mapIm,xs,ys,true=3
      

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

      ; Calculate where to put Colorbar
    sz = size( mapIm[*,*,0],/dim)
    xyz = Convert_Coord( 0, ys+sz[1]/scalefac,/device,/to_normal)
    y = xyz[1]
    y = [3*y+2, 2*y+3]/5

    IF ps THEN BEGIN 
      ColorBar, bottom=Wind_Start, nColors=N_Wind_Colors,$
             position=[0.25,y[0], 0.75, y[1]], $
               Title='Wind Speed (knots)',Min=minspeed, $
                 max=maxspeed,divisions=4, format='(f5.0)', $
                  pscolor=ps, /true, table=ct, charsize=0.75
    ENDIF ELSE BEGIN 
      ColorBar, bottom=Wind_Start, nColors=N_Wind_Colors,$
             position=[0.25,y[0], 0.75, y[1]], $
               Title='Wind Speed (knots)',Min=minspeed, $
                 max=maxspeed,divisions=4, format='(f5.0)', $
                  pscolor=ps, /true, table=ct, charsize=0.75, $
                    color='ffffff'x 
    ENDELSE 



    IF ps THEN text_color = '000000'x ELSE text_color = 'ffffff'x

    IF n_elements(title) NE 0 THEN $
      Title= title + ' ' + goes_string ELSE $
      Title= goes_string 

      ; Lay down the title
    xyz = Convert_Coord(0,ys,/device,/to_normal)
    y = xyz[1]/2.
    xyouts, 0.5, y, title, align=0.5, $
        /normal, charsize=1.05, color=text_color

    IF n_Elements(subtitle) NE 0 THEN BEGIN 
      xyz = Convert_Coord( 0, fix(1.5*!d.Y_CH_Size), /device, /to_normal )
      y1 = xyz[1]
      xyouts, 0.5, y-y1, subtitle, align=0.5, $
         /normal, charsize=1.0, color=text_color
    ENDIF 



    CASE 1 OF 
      gif: BEGIN 
        ;If output is 'gif' quantize it down to 175 colors.
        t0 = systime(1)
        tmpim = tvrd(true=3)
        im = color_Quan( tmpim, 3, r,g,b, colors=175 )
        t1 = systime(1)
        IF verbose THEN print,' Color_Quan took: ', t1-t0,' Seconds '
        t0 = t1

        Write_Gif, OutputFilename, im, r,g,b
        t1 = systime(1)
        IF arg_present(thumbnail) THEN  BEGIN 
          dims = size(im,/dimension)
          thumbnail = OutputFilename + '.TN'
          thumbnailIm =  congrid( im, dims[0]*0.3, dims[1]*0.3)
          Write_Gif, thumbnail, temporary(thumbnailIm), r,g,b
        ENDIF 
        IF verbose THEN print,' Write_Gif took: ', t1-t0,' Seconds '
        t0 = t1
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
      END

      ps: device,/close

    ENDCASE

  ENDIF ELSE BEGIN 
    Message,'Error Reading Goesfile ' + goesfile
  ENDELSE 
  
  end_time = systime(1)
  IF Verbose THEN print,'Total Time: ', (end_time-start_time)/60. ,' Minutes'
  IF Arg_Present(Outfile) THEN Outfile =  OutputFilename
  IF NOT ps THEN Wdelete,pixmap
  genv,/restore

END

