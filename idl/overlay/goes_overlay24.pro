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
;                    gif         = gif,$        
;                    ps          = ps, $        
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
;                    thick       = thick
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
;     gif         : output as Gif file
;     ps          : out as Postscript file
;     verbose     : flag, if set, emit many messages
;
;

;
;
; OUTPUTS:  A file named ????
;
;
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
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
PRO goes_overlay24, goesfile, $
                    windfiles   = windfiles, $ 
                    gif         = gif,$        
                    ps          = ps, $        
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
                    outfile     = outfile,$
                    scalefac    = scalefac

COMMON goes_overlay_cmn, landel


  genv,/save
  tvlct,orig_red,orig_green,orig_blue,/get
  loadct,0,/silent
  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!error_state.msg,/cont
    tvlct,orig_red,orig_green,orig_blue
    genv,/restore
    return
  END

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
    Message,'Usage: goes_overlay24, goesfile [,windfiles=windfile, xsize=xsize, ysize=ysize, ps=ps | gif=gif,CRDecimate=CRDecimate,Decimate=Decimate,ExcludeCols=ExcludeCols, verbose=verbose, minspeed=minspeed, maxspeed=maxspeed, length=length, thick=thick ] ',/cont
    tvlct,orig_red,orig_green,orig_blue
    genv,/restore
    return
  ENDIF 

  ps = keyword_set(ps)
  gif = keyword_set(gif)
  verbose = keyword_set(verbose)

  IF ps AND gif THEN BEGIN 
    Message,'Only one of PS or GIF may be set',/cont
    tvlct,orig_red,orig_green,orig_blue
    genv,/restore
    return
  ENDIF 

  IF NOT (ps OR gif) THEN BEGIN 
    Message,' Defaulting to GIF output ',/info
    gif = 1
  ENDIF 

  IF n_elements(scalefac) EQ 0 THEN BEGIN 
    IF ps THEN scalefac = 0.05 ELSE scalefac = 1
  ENDIF ELSE BEGIN 
    IF gif THEN BEGIN 
      Message,'Keyword SCALEFAC is ignored when creating GIFs',/cont
      scalfac = 1
    ENDIF 
  ENDELSE 

  IF n_Elements(minspeed) EQ 0 THEN minspeed = 2
  IF n_Elements(maxspeed) EQ 0 THEN maxspeed = 37
  IF n_elements(length)   EQ 0 THEN length = 2
  IF n_elements(thick)    EQ 0 THEN thick = 2

  start_time = systime(1)
  Read_PCGoes,goesfile,limits,data,hdr=hdr, status=status


    ; Make sure the longitude range is monotonic
  lonrange = FixLonRange( [ limits[0],limits[2] ])
  
  IF n_Elements(xsize) EQ 0 THEN BEGIN 
    IF gif THEN xsize = 640 ELSE xsize = 8.4
    xoffset = 1.2
  ENDIF 

  IF n_Elements(ysize) EQ 0 THEN BEGIN 
    IF gif THEN ysize = 480 ELSE ysize = 6.5
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

    IF ir THEN data = 1024-data
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

    WATER_INDEX = 1
    LAND_START = 2
    WIND_START = 22
    N_WIND_COLORS = 26
    N_COLORS = 47

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
              speed = seepd[good]
            ENDIF 
            veccol = BytScl( speed, min=minspeed, $
                             max=maxspeed, $
                             top=N_COLORS-N_WIND_COLORS-1) + $
                             WIND_START
            col24 = color24( veccol, colortable=ct)
            t0 = systime(1)
            
          ENDIF 
        ENDIF 
      ENDELSE 

    ENDIF   

      ; read the land elevation file, if it isn't in the common
    IF n_elements(landel) EQ 0 THEN BEGIN 
      openr,lun,'$VAP_ROOT/animate/land_elevations.bin',/get,error=err
      IF err NE 0 THEN BEGIN 
        Message,'Error reading Land Elevation Database ',/cont
        tvlct,orig_red,orig_green,orig_blue
        genv,/restore
        return
      ENDIF 
      landel =  intarr( 12l*360, 12l*180 + 1 )
      readu,lun, landel
      free_lun,lun
    ENDIF 


    sz = size(data,/dimensions)
    nlon = 1.0*sz[0]
    nlat = 1.0*sz[1]

    lonmin = lonrange[0]
    latmin = limits[1]
    loninc = (lonrange[1]-lonrange[0])/nlon
    latinc = (limits[3]-limits[1])/nlat
    loni = (findgen(nlon)*loninc+lonmin)#(replicate(1.,nlat))
    lati = replicate(1.,nlon)#(findgen(nlat)*latinc+latmin)

    t0 = systime(1)
    landmask = runLandMask(loni,lati)
    land = where(landmask)
    water = where(landmask EQ 0)
    landmask = 0
    t1 = systime(1) 
    IF verbose THEN print,'Time for Landmask : ',t1-t0, ' Seconds '
    t0 = t1

    x = where(loni LT 0, nx )
    IF nx NE 0 THEN loni[x] =  loni[x] + 360.
    topoIm = landel[ loni*12, (lati+90)*12 ]

    t1 = systime(1) 
    IF verbose THEN print,'Time for TopoIm : ',t1-t0, ' Seconds '
    t0 = t1

    loni = 0
    lati = 0

    cloudmask = bytscl(sqrt(1.0*(minpix>data<1024 )/1024),top=200) 
    data = 0


    IF gif THEN BEGIN 
      set_plot,'x'
      ; device,set_resolution=[xsize,ysize],z_buff=0
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

      IF gif THEN BEGIN 
        OutputFilename = ofileroot+'.gif'
      ENDIF ELSE BEGIN 
        OutputFilename = ofileroot+'.ps'
      ENDELSE 

    ENDIF ELSE outputFilename =  outfile

    IF ps THEN device,filename=OutputFilename

    FOR channel=0,2 DO BEGIN 
      image = intarr(nlon,nlat)+CT[channel,water_index]
      image[land] = reform( $
                     CT[ channel, $
                       LAND_START>(topoIm[land]+LAND_START)<(WIND_START-1) ] )
      image = 0> (image-cloudmask)
      image = image + cloudmask


      image =  byte(image <  255)
      tmpIm =   map_image( image, xs,ys,xsize,ysize,$
                         lonmin=lonrange[0],$
                         latmin=limits[1],$
                         lonmax=lonrange[1],$
                         latmax=limits[3],$
                         scale=scalefac, /compress, /bilinear )
      IF channel EQ 0 THEN BEGIN 
        imsz = size(tmpIm,/dim)
        mapIm = bytarr(imsz[0],imsz[1],3)
      ENDIF 
      mapIm[*,*,channel] = tmpIm
      tmpIm = 0l
    ENDFOR 

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

    IF gif THEN $
      ColorBar, bottom=Wind_Start, nColors=N_Wind_Colors,$
             position=[0.25,y[0], 0.75, y[1]], $
               Title='Wind Speed (knots)',Min=minspeed, $
                 max=maxspeed,divisions=4, format='(f5.0)', $
                  pscolor=ps, /true, table=ct, charsize=0.75, $
                    color='ffffff'x $
    ELSE $
      ColorBar, bottom=Wind_Start, nColors=N_Wind_Colors,$
             position=[0.25,y[0], 0.75, y[1]], $
               Title='Wind Speed (knots)',Min=minspeed, $
                 max=maxspeed,divisions=4, format='(f5.0)', $
                  pscolor=ps, /true, table=ct, charsize=0.75
     



    IF n_elements(title) NE 0 THEN $
      Title= title + ' ' + goes_string ELSE $
      Title= goes_string 

    IF ps THEN text_color = '000000'x ELSE text_color = 'ffffff'x
    xyz = Convert_Coord(0,ys,/device,/to_normal)
    y = xyz[1]
    xyouts, 0.5, y/2., title, align=0.5, $
        /normal, charsize=1.05, color=text_color



      ;If output is 'gif' quantize it down to 100 colors.
    IF gif THEN BEGIN 
      t0 = systime(1)
      tmpim = tvrd(true=3)
      im = color_Quan( tmpim, 3, r,g,b, colors=150 )
      t1 = systime(1)
      IF verbose THEN print,' Color_Quan took: ', t1-t0,' Seconds '
      t0 = t1

      Write_Gif, OutputFilename, im, r,g,b
      t1 = systime(1)
      IF verbose THEN print,' Write_Gif took: ', t1-t0,' Seconds '
      t0 = t1

    ENDIF ELSE device,/close ; else close the Postscript file.
  ENDIF ELSE BEGIN 
    Message,'Error Reading Goesfile ' + goesfile
  ENDELSE 
  
  end_time = systime(1)
  IF Verbose THEN print,'Total Time: ', (end_time-start_time)/60. ,' Minutes'
  IF Arg_Present(Outfile) THEN Outfile =  OutputFilename
  genv,/restore

END

