; IDL Procedure GOES_OVERLAY - read in a GOES file (one created by the
; goes program obtained from Paul Chang ) run it through map_image,
; put on pretty colors and overlay winds, if there are any.
; $Id$
; $Log$
; Revision 1.3  1998/10/30 22:12:02  vapuser
; Worked on image processing, not done yet.
;
; Revision 1.2  1998/10/17 00:14:39  vapuser
; worked on CRDecimate and ExcludeCols keywords.
; Killed a few bugs.
;
; Revision 1.1  1998/09/09 17:34:51  vapuser
; Initial revision
;
;
;
;
PRO GOES_OVERLAY, goesfile, $ ; (I) name of goes file to read
                  bimage, $   ; (o) resulting byte imate
                  wdata,$     ; (O) output of wind
                              ; data, if any was read, 
                              ; using the 'wfiles'
                              ; keyword. A 4 X nn array
                              ; where nn is the number
                              ; of data. wdata(0,*)
                              ; = u, (1,*)=v, (2,*)=lon ...
                  image,$     ; (O) int. image warped to map
                  data,$      ; (0) data read from this file
                  wfiles = wfiles,$         ; (I) vector containing the
                                           ; fully qualified filenames
                                           ; OF windfiles to read 
                  minpix    = minpix,$     ; -1, minpix=avg-sigma of data
                                           ; -2, call widget
                                           ; configurator.
                                           ; 0 is illegal value.
                                           ; Otherwise, take number as
                                           ; given. In anycase, minpix
                                           ; it is the minimum image 
                                           ; value that will remain as 
                                           ; clouds. Pixels with
                                           ; values below this  
                                           ; are set to be land OR
                                           ; water. 
                  getminpix =  getminpix, $ ; returns minpix if it changed
                  minspeed  = minspeed,$   ; minimum wind speed
                  maxspeed  = maxspeed,$   ; max wind speed
                  thick     = thick ,$     ; thickness of vectors (default=2)
                  length    = length ,$    ; length of vector (def=2)
                  uu        = uu   ,$      ; (I) u value of vectors
                  vv        = vv   ,$      ; (I) v value
                  llon      = llon ,$      ; (I) long of u/v
                  llat      = llat ,$      ; (I) lat of u/v
                  save      = save ,$      ; (IF) save to save set
                  Z         = Z    ,$      ; (IF) work in Z buffer
                  nogrid    = nogrid ,$    ; (IF) Don't put down grid
                  gif       = gif  ,$      ; (IF) write gif file
                  ps        = ps   ,$      ; (IF) write postscript file
                  title     = title ,$     ; (I) string to add onto title string
                  file_str  = file_str ,$  ; (I) string to add onto file string
                  thumbnail = thumbnail,$  ; (IF) if set, will make a 1/4 size
                                           ; (160x120) thumbnail image.
                  watcolor    = watcolor ,$    ; use only this color index
                                           ; for water (allowable
                                           ; range: 1<watcolor<11)
                  windcolor    = windcolor ,$ ; either a color index
                                              ; (int) in which case use only
                                              ; this color index
                                              ; for wind (allowable
                                              ; range: 73<windcolor<97)
                                              ; or a color keyword.
                                              ; available keywords are
                                              ; 'red', 'green','blue','white'
                  l2        =  l2    ,$    ; (I) if set, wind data is in 
                                           ; nscat level 2 files
                  xsize     = xsize  ,$    ; number of pixels in x dir(def=640)
                  ysize     = ysize  ,$    ; number of pixels in y dir(def=480)
                  windowid  = windowid ,$  ; (I) if set, plot to specified window.
                  outpath   = outpath ,$   ; (I) output directory
                  getoutfile= getoutfile,$ ; (O) get output file name
                  debug     =  debug ,$    ; 
                  limits    = limits,$     ; (I) 4 vector (minlon, minlat, 
                                           ; maxlon,maxlat).
                                           ; map will be set to these
                                           ; limits. NOTE. care must
                                           ; be used with this keyword
                                           ; since these limits may
                                           ; bear little relation to
                                           ; the limits of the data in
                                           ; the goes file. It's best
                                           ; to map the whole file. If
                                           ; you want different
                                           ; limits, use the 'goes'
                                           ; program to create a
                                           ; different 'goesfile' with
                                           ; those limits.
                  nscat = nscat ,$         ; flag: if set, this routine 
                                           ; expects nscat 
                                           ; style data files.

                                           ; These next three keywords
                                           ; are Qscat/Seawinds
                                           ; specific.

                  decimate = decimate ,$   ; Take every n-th vector, 
                                           ; i.e. 2 means take every
                                           ; 2nd, 3 = take every
                                           ; 3rd...


                  CRDecimate=CRDecimate,$  ; two element array, 
                                           ; CRD[0] =n, take every nth
                                           ; column, CRD[1]=m, take every
                                           ; mth row, e.g. [2,3] means
                                           ; take every 2nd col, 3rd
                                           ; row. crdecimate=[0,0] =
                                           ; CRDecimate=[1,1] = take
                                           ; every vector.
                                           ; CRDecimate takes
                                           ; precidence over decimate.

                  excludecols=excludecols  ; String suitible for 
                                           ; an 'execute' call,
                                           ; e.g. "2,3,23:35,71,72". 
                                           ; These columns will be
                                           ; excluded, in addition to
                                           ; the ones excluded by
                                           ; decimate and CRDecimate


                  


                                         
                  
                  


; Calling sequence
;
; IDL> goes_overlay, goesfile, bimage
;
;      will read specified goes file and map the entire image 
;      It will not plot any wind vectors. It will return the
;      final image used in the variable bimage.
;      
;
; IDL> goes_overlay, goesfile, bimage, wfiles = vector_of_files, wdata = wdata
;
;      map all data in goesfile, read wind data from input vector of
;      wind files, return wind data in variable wdata as a 4 X nn
;      array and the warped byte image in bimage
; 
; IDL> goes_overlay, goesfile, bimage, uu=uu, vv=vv, llon=llon,$
;      llat=llat, /write_gif
;
;      map all data in goesfile, plot uu/vv vectors at specified
;      llon/llat locations, write output to gif file.
;
;
COMMON goes_overlay_cmn, landel
COMMON colors, r_curr, g_curr, b_curr, r_orig, g_orig, b_orig

catch, error
IF error NE 0 THEN BEGIN 
  catch,/cancel
  Message,!err_string,/cont
  return
END

lf =  string(10B)
debug =  keyword_set( debug )
; IF NOT debug THEN on_error, 2 ; return to caller
GENV,/save ; save graphics environment 
nparams =  n_params(0);
IF nparams EQ 0 OR n_elements(goesfile) EQ 0 THEN BEGIN 
  str =  'Usage: Goes_Overlay, goesfile, $ ' + lf + $
         '   [wfiles=wfiles | [uu=uu,vv=vv,llon=llon,llat=llat]], $' + lf + $
         '   [,bimage, wdata, image, data, $ ' +lf + $
         '   minpix=minpix, minspeed=minspeed, maxspeed=maxspeed, $ ' + lf + $
         '    thick=thick, length=length, Z=Z, gif=gif, ps=ps, $' + lf + $
         '     title=title, file_str=file_str, $ ' + lf + $
         '      thumbnail=thumbnail, watcolor=watcolor, $ ' + lf + $
         '       windcolor=windcolor, decimate=decimate, $' + lf + $
         '        CRDecimate=CRDecimate, ExcludeCols=ExcludeCols] '
  Message,str,/cont
  return
ENDIF 

gif =  keyword_set(gif)
ps  =  keyword_set(ps)
l2  =  keyword_set(l2)
thumbnail =  keyword_set( thumbnail )
ofile = ''

IF ( ( keyword_set(xsize) OR keyword_set(ysize) ) AND ps ) THEN BEGIN 
  str = "keywords PS and any combination of XSIZE/YSIZE disallowed"
  message,str,/cont
  message,' XSIZE/YSIZE only allowed with GIF/X/Z outputs/devices ',/cont
  return
ENDIF 


IF n_elements( minspeed ) EQ 0 THEN minspeed =  0
IF n_elements( maxspeed ) EQ 0 THEN maxspeed =  30

IF n_elements( xsize ) EQ 0 THEN xsize =  640
IF n_elements( ysize ) EQ 0 THEN ysize =  480

IF n_elements( thick ) EQ 0 THEN thick =  2

IF n_elements( length ) EQ 0 THEN length = 2

IF n_elements( outpath ) NE 0 THEN BEGIN
  IF strmid( outpath, strlen(outpath)-1, 1 ) NE '/' THEN $
   outpath =  outpath + '/'
ENDIF ELSE outpath =  './'

IF n_Elements(ExcludeCols) NE 0 THEN BEGIN 
  IF VarType(ExcludeCols) eq 'STRING' THEN BEGIN 
    IF strlen(ExcludeCols) EQ 0 THEN BEGIN 
      Message,'ExcludeCols must be of non-zero length, ignored ',/cont
      exclude = ''
    ENDIF ELSE exclude = ExcludeCols
  ENDIF ELSE BEGIN 
    Message,'ExcludeCols must be of a string, ignored ',/cont
    exclude = ''
  ENDELSE 
ENDIF 

IF exist(CRDecimate) AND N_Elements(CRDecimate) NE 2 THEN BEGIN 
  Message,'CRDecimate must be a 2-vector, ignored',/cont
  CRD = [1,1]
ENDIF ELSE IF NOT exist(CRDecimate) THEN CRD = [2,2] ELSE CRD = CRDecimate


IF N_Elements(Decimate) EQ 0 THEN Decimate = 1

cblables =  strtrim( [ minspeed, maxspeed ], 2 )
cbtitle = 'Wind Speed (M/S)'

IF gif AND ps THEN BEGIN
  message," Only one of 'gif' or 'ps' allowed ",/cont
  return
ENDIF 

IF ps THEN BEGIN 
  set_plot,'ps'
  device, /color, bits=8,/landscape 
  scalef =  0.02
ENDIF ELSE scalef =  1.

;IF ps THEN Z =  1
PLOTVECT = 0;
;
; Handle watcolor requests.
;
IF keyword_set( watcolor ) THEN BEGIN
  IF watcolor LT 1 OR watcolor GT 11 THEN BEGIN 
    message,' keyword WATCOLOR must be in range [1, 11] ',/cont
    return
  ENDIF 
ENDIF 
goesfile =  goesfile(0)

sensors =  [ 'Visible','IR2','IR3','IR4','IR5']
;tmp       =  str_sep( goesfile, '/' )
;base_name =  tmp(n_elements(tmp)-1)
;sensornum =  strmid( base_name, 5,1)
;sensor    =  sensors( sensornum-1 )
;sat_num =  fix( strmid( base_name, 4,1 ) )
; sat_name =  strmid( base_name, 0, 4 ) + ' ' + strmid( base_name, 4, 1 )

GoesFilenameStruct = ParseGoesFileName( goesfile )
IF VarType( GoesFilenameStruct ) NE 'STRUCTURE' THEN BEGIN 
  Message," Trouble parsing " + Goesfile ,/cont
  return
ENDIF ELSE BEGIN 
  sat_name = GoesFilenameStruct.SatName + " " + $
   strtrim(GoesFilenameStruct.SatNum,2 )
ENDELSE 
sensornum = GoesFilenameStruct.Sensornum
sensor    =  sensors[ sensornum-1 ]
;dash =  strpos( base_name, '_' )
;jday =  strmid( base_name, dash+1, 3 )
;time =  strmid( base_name, dash+4, 4 )
;tmp =  strtrim( doy2date( fix(year), fix(jday) ), 2 ) 
;date_str =  tmp(0) + '/' + tmp(1) + '/' + $
; strmid( strtrim( year,2 ), 2,2 )

IR =  ( sensornum GT 1 )
; check for wind data input .
;

NullPtr = Ptr_New()
CASE 1 OF 
  keyword_set( wfiles ) : BEGIN
    plotvect =  1
    IF keyword_set( nscat ) THEN BEGIN 
      IF L2 THEN READ_L2_DATA,    wfiles, uu,vv,llon,llat,$
                              mintime= mintime, maxtime=maxtime ELSE $
                 READ_RMGDR_DATA, wfiles, uu,vv,llon,llat, mintime=mintime,$
                                           maxtime=maxtime 
    ENDIF ELSE BEGIN 
        ; Data is assumed to be Qscat or Seawinds data
      retptr = ReadQ2BData( wfiles, $
                            decimate=decimate, $
                            CRDecimate=CRD, $
                            ExcludeCols=exclude) ; take every other vector
      IF retptr EQ NullPtr THEN BEGIN 
        Message,'Error Reading Quikscat/Seawinds data',/cont
        return
      ENDIF ELSE BEGIN 
        wdata =  temporary( *retptr )
        uu    = wdata[*,*,0] 
        vv    = wdata[*,*,1]
        llon  = wdata[*,*,2]
        llat  = wdata[*,*,3]
        wdata = 0;
        plotvect = 1
      ENDELSE 
    ENDELSE 
    IF n_elements( uu ) EQ 0 THEN BEGIN 
      message,' No data found for input data files ',/cont
      print, wfiles
      print,' Are the filenames fully qualified ?'
      print,' *** Returning *** '
      return
    ENDIF 
    IF nparams GE 3 THEN wdata =  [[uu],[vv],[llon],[llat]]

  END 
  keyword_set( vv )   AND keyword_set( uu ) AND $
  keyword_set( llon ) AND keyword_set( llat ) : BEGIN
    plotvect =  1
  END
  ELSE : BEGIN 
    ; if one is set, they all must be!
    IF keyword_set( uu ) THEN BEGIN
      IF NOT keyword_set(vv) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llon) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llat) THEN $
        message,' If you set one wind keyword, you must set them all '
    ENDIF 

    IF keyword_set( vv ) THEN BEGIN
      IF NOT keyword_set(uu) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llon) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llat) THEN $
        message,' If you set one wind keyword, you must set them all '
    ENDIF     

    IF keyword_set( llon ) THEN BEGIN
      IF NOT keyword_set(vv) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(uu) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llat) THEN $
        message,' If you set one wind keyword, you must set them all '
    ENDIF 

    IF keyword_set( llat ) THEN BEGIN
      IF NOT keyword_set(vv) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llon) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(uu) THEN $
        message,' If you set one wind keyword, you must set them all '
    ENDIF 
    IF NOT( keyword_set( uu ) AND keyword_set( vv ) AND $
            keyword_set( llon) AND keyword_set( llat ) ) THEN BEGIN 
       ; if none are set, then don't plot the vectors
       plotvect = 0
    ENDIF 
  END 
ENDCASE 


IF exist( mintime ) AND exist( maxtime ) THEN BEGIN 
    ;             1         2
    ;   012345678901234567890    
    ;  'yyyy-dddThh:mm:ss.ccc'
  mintime =  strmid( mintime, 0, 8) + '/' + $
   strmid( mintime, 9, 2) + strmid( mintime, 12,2 )
  maxtime =  strmid( maxtime, 0, 8) + '/' + $
   strmid( maxtime, 9, 2) + strmid( maxtime, 12,2 )

  wind_time_str =  ' Wind Data: ' + strtrim( mintime ,2 ) + ' - ' + $
  strtrim( maxtime, 2 )

ENDIF 
save       =  keyword_set( save )
Z          =  keyword_set( Z )
grid       =  NOT keyword_set( nogrid )

;IF NOT keyword_set( minpix ) THEN BEGIN
;  IF NOT IR THEN minpix =  150
;ENDIF ELSE BEGIN 
;  IF minpix EQ  -1 AND NOT ir THEN minpix =  150
;ENDELSE 

BACKGROUND  = 0b
WATER_START = 1b
WATER_MAX   = 12b
LAND_START  = 13b
CLOUD_START = 33b
WIND_START  = 73b
WHITE       = 98b
N_WIND_COLORS =  WHITE-WIND_START+1

IF getenv('VAP_RESOURCES') NE '' THEN BEGIN 
  colorTableFile = getenv('VAP_RESOURCES')+ $
    '/Color_Tables/goes_overlay.ct'
ENDIF ELSE $
  colorTableFile = $
     '/usr/people/vapuser/Qscat/Resources/Color_Tables/goes_overlay.ct'

PtrToColorTable = ReadColorTable(colorTableFile)
IF NOT Ptr_Valid(PtrToColorTable) THEN BEGIN 
  Message,"ERROR: Can't open " + colorTablefile,/cont
  return
ENDIF ELSE BEGIN 
  CT = *PtrToColorTable
  Red = reform(CT[0,*])
  Green = reform(CT[1,*])
  Blue = reform(CT[2,*])
  Ptr_Free,PtrToColorTable
  CT = 0
ENDELSE 

;openr,rlun,'$VAP_/goes_overlay.ct',/get,error=err
;IF err NE 0 THEN BEGIN 
;  message,!err_string,/cont
;  return
;ENDIF 
;spawn,' wc -l $VAP_ROOT/goes-vis-overlay-ct.txt', nlines
;nlines =  nlines(0)
;tmp =  bytarr(3,nlines)
;top_color_index =  nlines-1
;readf,rlun, tmp
;free_lun,rlun
;tmp =  transpose( tmp )
;red   =  tmp( *,0)
;green =  tmp(*,1)
;blue  =  tmp(*,2) &  tmp=0



; Handle windcolor requests
IF n_elements( windcolor ) NE 0 THEN BEGIN
  input_windcolor = windcolor
  s =  size( windcolor )
  IF type(windcolor) EQ 'STRING' THEN BEGIN
    color =  GETCOLOR( windcolor )
    windcolor = wind_start
    tvlct,color,wind_start
    red(windcolor) = color(0)
    green(windcolor) = color(1)
    blue(windcolor) = color(2)
  ENDIF ELSE BEGIN
    IF windcolor LT  wind_start OR $
       windcolor GT white THEN BEGIN
      str =  ' Out of range wind color, keep 73 <=windcolor,98!'
      message,str,/cont
      return
    ENDIF 
  ENDELSE 
ENDIF ELSE windcolor = -1

r_curr = red 
g_curr = green
b_curr = blue
r_orig = red
g_orig = green
b_orig = blue

message,' Reading Goes file ' + goesfile,/cont

IF NOT keyword_set( file_str ) THEN file_str =  ''
IF thumbnail THEN BEGIN
  ; GOTN = Goes Overlay ThumbNail
  IF keyword_set( watcolor ) THEN $
    GOTN, goesfile=goesfile, data=data, limits=limits, $
       uu=uu, vv=vv,  llon=llon, llat=llat, minpix=minpix, $
       minspeed=minspeed, maxspeed=maxspeed, date_str= date_str, $
         file_str= file_str,watcolor=watcolor, $
         windcolor=input_windcolor  ELSE $
    GOTN, goesfile=goesfile, data=data, limits=limits, $
       uu=uu, vv=vv, llon=llon, llat=llat, minpix=minpix, $
       minspeed=minspeed, maxspeed=maxspeed, date_str= date_str, $
         file_str= file_str, windcolor=input_windcolor
ENDIF ELSE BEGIN
  ; read the goes file
  READ_PCGOES, goesfile, limits, data, image, year, jday, $
              time, nlon,nlat, lonsize, latsize, info_string, $
              area_file = area_file, hdr=hdr
  IF year EQ 0 THEN BEGIN
    ; older version of the grid file don't
    ; have the year info in them. so set current year.
    ;spawn,'date',ret_str
    ;tmp =  str_sep( ret_str(0), ' ' )
    ;year =  tmp( n_elements(tmp)-1 )
    year =  (bin_date())[0]
  ENDIF 

  tmp =  strtrim( doy2date( fix(year), fix(jday) ), 2 )
  date_str =  tmp(0) + '/' + tmp(1) + '/' + $
     strmid( strtrim( year,2 ), 2,2 )
  jday = strtrim( fix(jday),2 )
  tmp =  '000'
  strput, tmp, jday, 3-strlen(jday)
  jday = tmp
ENDELSE 
IF n_elements( data ) EQ 0 THEN BEGIN
  message,' Error reading file ' + goesfile ,/cont
  return
ENDIF 

date = doy2date( year, jday )

month =  date[0]
dom = date[1]

IF NOT keyword_set( windowid ) THEN BEGIN 
  IF NOT Z AND NOT ps THEN BEGIN 
    set_plot,'x'
    window, xsize=xsize,ysize=ysize,colors=n_elements(red),/free 
  ENDIF ELSE IF NOT ps THEN BEGIN
    set_plot,'z'
    device, set_resolution=[xsize,ysize]
  ENDIF 
ENDIF ELSE WSET, windowid

tvlct,red,green,blue



IF n_elements( landel ) EQ 0 THEN BEGIN 
  ; read the land elevation file if it isn't in the common
  openr,1,'$VAP_ROOT/animate/land_elevations.bin'
  landel =  intarr( 12l*360, 12l*180 + 1 )
  readu,1, landel
  close,1
ENDIF 

; get the section of the land el file we need
lonpar =  limits([0,2])
latpar =  limits([1,3])
x = where( lonpar LT 0., nx )
IF nx GT 0 THEN lonpar(x) =  lonpar(x) + 360.
landlon   = fix( [lonpar(0),lonpar(1)]*12. )
landlat   = fix( ([latpar(0),latpar(1)]+90)*12 )
nlandlon  = landlon(1)-landlon(0)+1 
nlandlat  = landlat(1)-landlat(0)+1 
landlon   = findgen(nlandlon)/12. + lonpar(0); #(fltarr(nlandlat)+1)   
landlat   = findgen(nlandlat)/12. + latpar(0); ##(fltarr(nlandlon)+1)

tlimits =  limits ; copy to protect limits variable
minlat =  limits(1) &  minlon= limits(0) &  maxlon= tlimits(2)

latcent =  0
loncent = 0
y = [0,2]
x =  where( tlimits(y) LT 0,nx )
IF nx NE 0 THEN tlimits(y(x)) =  tlimits(y(x)) + 360.

loncent =  avg( tlimits([0,2]) )

IF exist( wind_time_str ) THEN $
 tit =  wind_time_str ELSE $
 tit =  ''

If keyword_set( title ) THEN tit = tit + ' ' + title 


title_str =  sat_name + ' '+ $
  sensor +  ' ' +          $
   '('+ date_str + ') = '  + $
    jday + '/' + time  + ' UT '

IF strlen(tit) GT 0 THEN $
 title_str =  title_str + ' ' + tit

; define and load color bar for winds
;cbar =  bytarr( 10, 10, n_wind_colors ) 
;FOR i=0,n_wind_colors-1 DO cbar(*,*,i) =  byte(i) + wind_start
;cbar =  transpose( reform( cbar, 10, 10*n_wind_colors ) )



s = size(data)
nlon =  s[1]
nlat =  s[2]

latinc =  (tlimits(3)-tlimits(1))/nlat 
loninc =  (tlimits(2)-tlimits(0))/nlon 


IF ir THEN  data =  temporary(1024-data) ; reverse for IR

moments = moment(data)
a = moments[0]
s = sqrt(moments[1])
oneSigma = a-s

  ; This may not work for visual images!
IF n_Elements( minpix ) EQ 0 THEN minpix = -1
CASE minpix OF 
  -1: BEGIN 
    minpix =  oneSigma
    message,' Minpix set to ' + strtrim( minpix, 2 ),/cont
  END
  -2: BEGIN 
    ret = Cloud_Overlay_Config( data=data, limits=limits)
    minpix = ret.cutoff
    Message,' Minpix set to ' + strtrim( minpix, 2 ),/cont
  END
  ELSE:
ENDCASE

smallpix =  where( data LE minpix, nsmallpix )
IF nsmallpix EQ 0 THEN BEGIN 
  Message,'No pixels smaller than ' + strtrim(minpix,2),/cont
  print,' Setting minpix to ' + oneSigma
  minpix = OneSigma
  smallpix =  where( data LE minpix, nsmallpix )
  IF nsmallpix EQ 0 THEN BEGIN 
    Message,' Still no pixels smaller than minpix (=' + strtrim( minpix,2) + ')',/cont
    print,'    Returning'
    return
  ENDIF 
ENDIF 

dlon =  (findgen( nlon )*loninc + minlon ) # replicate(1.,nlat)
dlat =  replicate(1.,nlon) # (findgen( nlat )*latinc + minlat ) 


; Scale data array to 40 indices starting at 33
;bimage =  bytscl( data, MIN=minpix, top=40 ) + CLOUD_START

bimage = hist_equal( data, min=minpix, max=1024, $
                     top=WIND_START-CLOUD_START-1)+CLOUD_START
UNPACK_WHERE, bimage, smallpix, col, row

lons =  dlon[ smallpix ]
lats =  dlat[ smallpix ]
mask =  lonarr( nsmallpix )
t7 =  systime(1)
xx =  where( lons LT 0, nxx )
IF nxx NE 0 THEN lons(xx) =  lons(xx)+360.
; Call the linkimage routine 'land_mask' to calculate which of each
; point is land and which is water. Set the appropriate color index
; in each.
print,' Going into land_mask '
t =  systime(1)
LAND_MASK, lons, lats, mask
print,' Time to do land_mask = ', $
 systime(1)-t, ' seconds for ', nsmallpix, ' pixels '
;lon =  0
;lat =  0

print,' Going into land section '
t1 =  systime(1)
land =  where( mask EQ 1, nland )
IF nland NE 0 THEN BEGIN 
  ix =  lons( land )*12.
  iy =  (lats( land )+90.)*12.
  bimage( col(land), row(land) ) =  $
   land_start >  (landel( ix, iy ) + LAND_START) <  (cloud_start-1)
ENDIF 
t2 =  systime(1)
print,' land section took ', (t2-t1)/60., ' minutes '

sea =  where( mask EQ 0, nsea )
IF nsea NE 0 THEN BEGIN 
  IF keyword_set( watcolor ) THEN $
    bimage( smallpix( sea ) ) =  watcolor $ 
  ELSE $
    bimage( smallpix( sea ) ) =  $
      bytscl( bimage( smallpix( sea ) ), min= cloud_start, $
           top= WATER_START + 11 )   + water_start

ENDIF 

xx =  where( bimage EQ 0, nxx )
IF nxx NE 0 THEN BEGIN
  IF keyword_set( watcolor ) THEN $
   bimage( xx ) =  watcolor ELSE $
   bimage( xx ) =  water_start+2
ENDIF 

t3 =  systime(1)
print,' sea section took ', (t3-t2)/60., ' minutes '

MAP_SET, latcent, loncent,  $
 limit=tlimits[ [1,0,3,2] ],/noborder, ymargin=[4.,4.]


t1 =  systime(1)
print, 'Going into map_image '
bimage = Map_Image( bimage,xs,ys,xsiz,ysiz, $
                    lonmin = tlimits(0), $
                    latmin = tlimits(1),$
                    lonmax = tlimits(2),$
                    latmax = tlimits(3), $
                    /whole_map,compress=1)
t2 =  systime(1)
print, 'map_image took ', (t2-t1)/60., ' Minutes '

IF ps OR gif  OR save THEN BEGIN 
  tmp =  '0000'
  strput, tmp, time, 4-strlen(time)
  time =  tmp
  lim_str   =  strtrim( long(limits), 2 )
  lim_str =  '%' + lim_str(1) + ',' + lim_str(0) + ',' + $
                   lim_str(3) + ',' + lim_str(2) +  '%'
  dlm =  '_'

  
  year =  strtrim( year, 2 )
  IF strlen(year) EQ 2 THEN year =  strtrim( fix(year)+1900,2)
  time_string = year + month + dom + "T" + $
    strmid(time,0,2) + ":" + strmid(time,2,2)
  ofileroot = outpath + strtrim( sat_name, 2 ) +  dlm + $
   sensor + '_' + time_string


  sp =  strpos( ofileroot,' ' )
  strput, ofileroot, '_', sp
  ofileroot =  ofileroot + '-' + lim_str
  IF keyword_set( file_str ) AND $
     strlen( file_str(0) ) GT 0 THEN BEGIN 
    s =  str_sep( file_str,' ' )
    tt =  '_' + s(0)
    FOR i=1,n_elements(s)-1 DO tt =  tt + '_' + s(i)
    ofileroot =  ofileroot + tt
  ENDIF 
ENDIF 
IF ps THEN BEGIN 
  ofile =  ofileroot + '.ps'
  message,' Output file = ' + ofile,/cont
  device, filename = ofile
ENDIF ELSE IF gif THEN  ofile =  ofileroot + '.gif'


tv,bimage,xs,ys
;s =  size( bimage ) &  ny= s(2)
IF ps THEN BEGIN 
; bimage(*,ny-40:ny-1) =  top_color_index ; ps looks best in white
 TV, bimage, xs,ys, XSIZ=xsiz, YSIZ=ysiz 
ENDIF ELSE BEGIN 
; bimage(*,ny-40:ny-1) =  0 ; gif/x looks best in black
 TV, bimage, xs,ys
ENDELSE 

;xy =  convert_coord( [ xs + xsiz/2, (ny-40)/scalef ] , /dev, /to_data )
;y =  xy[1]

sz = size(bimage,/dimensions)
xyz = Convert_Coord( 0, ys+sz[1]/scalef, /device,/to_data )
y = xyz[1]
  ;
  ; over plot the wind vectors, if there are any.

IF plotvect THEN BEGIN 
  good1 = where( finite(uu) AND finite(vv), ngood1)
  IF ngood1 NE 0 THEN BEGIN 
    xx =  where( llat[good1] LT y, nxx )
    IF nxx NE 0 THEN BEGIN 
      IF windcolor EQ -1 THEN $
        PLOTVECT, uu[good1[xx]],vv[good1[xx]],$
           llon[good1[xx]],llat[good1[xx]],$
           length      = length ,$
           start_index = wind_start, $
           ncolors     = n_wind_colors, $
           minspeed    = minspeed ,$
           maxspeed    = maxspeed, $
           thick       =  thick $     
     ELSE $
        PLOTVECT, uu[good1[xx]],vv[good1[xx]],$
           llon[good1[xx]],llat[good1[xx]],$
           length = length ,$
           thick  = thick ,$
           color  = windcolor
    ENDIF 
  ENDIF 
ENDIF 
;IF NOT ps THEN BEGIN
;  xsiz =  !d.x_size
;  ysiz =  !d.y_size
;ENDIF

;IF ps THEN $
;  cbloc = convert_coord( [(xsiz-(10*27)/scalef)/2,$
;                          ysiz-32/scalef], /device,/to_normal ) ELSE $
;  cbloc = convert_coord( [(xsiz-(10*27)/scalef)/2,$
;                          ysiz-40/scalef], /device,/to_normal )
;; color bar x location 
;cbx =  cbloc(0)
;; color bar y location
;cby =  cbloc(1)

;; color bar title
;  IF ps THEN $
;   cbtitloc = convert_coord( [xsiz/2,ysiz-22/scalef], $
;                             /device,/to_normal ) ELSE $
;   cbtitloc = convert_coord( [xsiz/2,ysiz-27/scalef], $
;                             /device,/to_normal ) 

;cbtitx =  cbtitloc(0) &  cbtity= cbtitloc(1)
;; end of color bar
;cbend =  convert_coord( [(xsiz-(10*27)/scalef)/2 + (10*27 + 2)/scalef, $
;                         ysiz-35/scalef], /dev, /to_norm )
;cbendx =  cbend(0)
;; title location
;titloc =  convert_coord( [xsiz/2,ysiz - !d.y_ch_size-2],  $
;                         /device, /to_normal )   
;titx =  titloc(0) &  tity =  titloc(1)
;IF ps THEN text_color = 0 ELSE text_color = 255

;; Annotate the image
;; First blank out the top 40 rows of the image
;;blank =  bytarr( n_elements( bimage(*,0) ), 40 )
;;tv, blank, xs, ysiz-40/scalef, xsiz=xsiz, ysiz=40/scalef

;; Put down title string
;xyouts, titx, tity, title_str, align=0.5, /normal, charsize=1.05, color=text_color
;IF (windcolor EQ -1) THEN BEGIN 
;  ; Put down color bar
;  tv, cbar, cbx*xsiz, ysiz-35/scalef,xsiz=270/scalef,ysiz=10/scalef
;  ; Annotate the color bar

;  xyouts, cbtitx, cbtity, cbtitle,     align = 0.5, /normal, color=text_color
;  xyouts, cbx,    cby,    cblables(0), align = 1.0, /normal, color=text_color
;  xyouts, cbendx, cby,    cblables(1), align = 0.0, /normal, color=text_color
;ENDIF 

IF grid THEN BEGIN 
  latrange =  limits(3)-limits(1)
  lonrange =  limits(2)-limits(0)
  IF latrange/5. GT 20 THEN latdel = 10 ELSE latdel = 5.
  IF lonrange/5. GT 20 THEN londel = 10 ELSE londel = 5.

  map_grid, latdel = latdel, londel = londel, $
   lonlab = minlat-.3,latlab = minlon, $
   charsize=.8, latalign=1, color = 255
ENDIF 


  ; Calculate where to put the Colorbar.
  ; 
sz = size(bimage,/dimensions)
xyz = Convert_Coord( 0, ys+sz[1]/scalef,/device,/to_normal)
y = xyz[1]
y = [3*y+2, 2*y+3]/5

ColorBar, bottom=Wind_Start, nColors=N_Wind_Colors,$
            position=[0.25,y[0], 0.75, y[1]], $
              Title='Wind Speed (knots)',Min=minspeed, $
               max=maxspeed,divisions=4, format='(f5.0)', $
                pscolor=ps


  ; Now put the title at the bottom
IF ps THEN text_color = 0 ELSE text_color = 255  
xyz = Convert_Coord(0,ys,/device,/to_normal)
y = xyz[1]
xyouts, 0.5, y/2., title_str, align=0.5, $
    /normal, charsize=1.05, color=text_color


IF gif THEN BEGIN 
  im =  tvrd()
  write_gif, ofile, im, red, green, blue
  message,' Output file = ' + ofile,/cont
ENDIF 

IF save THEN BEGIN
  window_size =  [ [!d.x_size], [!d.y_size]]
  device =  !d.name
  filename =  ofileroot + '.save'
  save,bimage,xs,ys,window_size,xsiz,ysiz,device, f=filename
ENDIF 

IF ps THEN device,/close

GENV,/restore ; restore graphics environment 
getminpix =  minpix
getoutfile =  ofile

RETURN
END

