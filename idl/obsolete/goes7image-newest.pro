pro Goes7image, fileroot,minlat,maxlat,minlon,maxlon,resdeg,$
                     do_winds = do_winds, windfile = windfile, ir= ir,z= z

do_winds =  keyword_set( do_winds )
IF do_winds AND NOT (keyword_set( windfile ) ) THEN $
 windfile =  'current_winds.txt'


t1 =  systime(1)
txtfile =  fileroot + '.txt'
openr,lun,txtfile,/get_lun
done =  0
rec =  ''
REPEAT BEGIN
  readf,lun, rec
  rec =  strupcase( rec )
  done  =  ( strpos( rec, 'EQUATOR') NE -1 )
ENDREP UNTIL done
free_lun,lun

blank =  strpos( rec, ' ' )
equator =  fix( strmid( rec, blank, strlen( rec ) - blank ) )
equator_offset =  equator - 836

gridfile = fileroot + '-grid.hdf'
IF keyword_set( ir ) THEN imagefile = fileroot + '-ir1.hdf' $ 
                     ELSE imagefile = fileroot + '.hdf'

dfsd_getinfo,gridfile,type=t,dims=d
grid = fltarr( d(0), d(1) )
dfsd_getdata,gridfile,grid
dfsd_getinfo,imagefile,type=t,dims=d
image = intarr( d(0), d(1) )
dfsd_getdata,imagefile,image

grid = reform( grid, 2,241,241) 
x = reform( grid( 1, *,* ), 241, 241 )
y = reform( grid( 0, *,* ), 241, 241 ) - equator_offset
grid =  0b; free some memory, lord know we need it!

; Limits of big image
;
blatmin  = -60.  &   blatmax  =  60.
blonmin  =  165.  &   blonmax  = 285.
;
;
; clean up the image, take the middle line, find all zeros at
; beginning and end of column.

tol =  700
midcol =  n_elements( image(*,0) )/2
midcol =  image( midcol, *) 

; This will find the rows at the beginning of the col that 
; have zero intensities in them
z =  where( midcol EQ 0, nz )
IF nz NE 0 THEN BEGIN
  zdiff =  z(1:nz-1) - z(0:nz-2)
  split =  where( zdiff ne 0, nsplit )
  IF nsplit EQ 0 THEN zb =  nz ELSE zb =  split(0) + 1
ENDIF 

; This will find the rows at the end of the col that 
; have zero intensities.
z =  where( reverse(midcol) EQ 0, nz )
IF nz NE 0 THEN BEGIN
  zdiff =  z(1:nz-1) - z(0:nz-2)
  split =  where( zdiff ne 0, nsplit )
  IF nsplit EQ 0 THEN ze =  nz ELSE ze =  split(0) + 1
ENDIF 

zdiff =  0b
split =  0b

; clean up noisy data 
nc =  n_elements( image(*,0) )
nr =  n_elements( image(0,*) )

numnew = nr-zb-ze
numnewb = numnew-1
val4 = image(*,zb:nr-ze)
locbadp = where(val4 eq 0 or val4 ge tol,countbadp)
t2 =  systime(1)
print,' Elapsed time from start to t2 = ', t2-t1
if countbadp ne 0 then begin
    locbadub = where(locbadp le nr-1,countbadub)
    locbadua = where(locbadp ge long(numnew-1)*nc,countbadua)
    locbad = locbadp(countbadub:countbadp-countbadua-1)
    countbad = countbadp-countbadub-countbadua
    help, countbadp
    help, countbadub
    help, countbadua
    help, countbad
    if countbad ne 0 then begin
	val4(locbad) = (val4(locbad-nc) + val4(locbad+nc))*.5
    endif
endif
image(*,zb:zb+numnew-1) = temporary( val4(*,0:numnew-1) )
val4 = 0
IF ze NE 0 THEN image(*,zb+numnew:zb+numnew+ze-1) = 0
IF zb NE 0 THEN image(*,0:zb-1) = 0

t21 =  systime(1)
print,'time to get rid of bad points = ',t21-t2
; Now do the interpolations
;
; not so fast, let's ajust a couple of things first
;
findstep = 2.*resdeg
numfind  = fix(240/findstep) + 1
locy   = findgen(numfind)*findstep
locx   = findgen(numfind)*findstep
;
; Limits of requested cdsubimage
;
slatmin = minlat    &   slatmax = maxlat
slonmin = minlon   &   slonmax = maxlon
;
print, 'Lat/lon limits of subimage: ', slatmin, slatmax, slonmin, slonmax
;
; images start at the top
;
ml = fix(1./resdeg)
latstart  = (blatmax-slatmax)*ml    &  lonstart  = (slonmin-blonmin)*ml
latlen    = (slatmax-slatmin)*ml+1  &  lonlen    = (slonmax-slonmin)*ml+1
latend    =  latstart + latlen -1      &  lonend    =  lonstart + lonlen -1
;
print, 'lonstart,lonend',lonstart,lonend
print, 'latstart,latend',latstart,latend
;
locxn  = locx(lonstart:lonend)
locyn  = locy(latstart:latend)
locx = 0
locy = 0
;
print,'Interpolate Wenxias locations to finer grid'
;
t3 =  systime(1)
newx  = interpolate(x,locxn,locyn,/grid)
newy  = interpolate(y,locxn,locyn,/grid)
locxn = 0
lonyn = 0
t4 =  systime(1)
print,' time to interpolate new row/cols = ', t4-t3
;
print, 'Find values of image at finer locations'
;
subimage = image( newx, newy )
;
image =  0b

; define color tables

red = [   0, 0, 3, 6,10,13,17,20,23,27,30,34,30,47,51,55,60,64,68,73,77,81,$
   86,90,94,99,103,107,112,116,120,125, 129,133,138,142,146,151,155,$
   159,164,168,172,177,181,185,190,194,198,203,207,211,216,220,224,229,$
   233,237,242,246,250,255, 0, 0,  0, 0, 0, 0, 0,255,255,79,255 ]

green = [ 0, 0, 7,14,21,29,36,43,51,58,65,73,30,47,51,55,60,64,68,73,$
	77,81,86,90,94,99,103,107,112,116,120,125, 129,133,138,142,$
	146,151,155,159,164,168,172,177,181,185,190,194,198,203,207,$
	211,216,220,224,229,233,237,242,246,250,255,109,157, 206,255,$
	118,118,255,255, 0,136,255 ]

blue =[0,255,240,226,212,198,184,170,156,142,128,114,30,47,51,55,60,64,$
	68,73,77,81,86,90,94,99,103,107,112,116,120,125, 129,133,138,142,$
	146,151,155,159,164,168,172,177,181,185,190,194,198,203,207,211,$
	216,220,224,229,233,237,242,246,250,255, 0, 0,0, 0,126,218,255, 0, 0,62,255 ]

  LAND_COLOR_INDEX =  71
  OCEAN_INDICES    =  [1, 11 ]
  CLOUD_INDICES    =  [12, 61 ]
  WIND_INDICES     =  [62, 70 ]

IF keyword_set( ir ) THEN begin

  temparray =  fltarr(1024 )
  openr,tlun,'GOES.table',/get_lun
  readf,tlun,temparray
  goestemp =  subimage*0.
  free_lun,tlun
  ;
  ; ---  convert to temperatures
  ;

  goestemp =  temparray( subimage ) &  subimage= 0b

;  red =  bytarr(256) &  green=red &  blue=red
  t = fltarr(256)
  ;
  ;
  ;
  clut_file =  'spectrum06.256.950.350.325.CLUT'
  openr,clut,CLUT_file,/get_lun
  readf,clut,N_colors
  readf,clut,Tmin,stepTmax,Tmax
  readf,clut,T
  ; uncomment the following lines as
  ; well as the one above defining
  ; the red,green and blue arrays if 
  ; you want to use the UofH color table
;  readf,clut,red
;  readf,clut,green
;  readf,clut,blue
  
  ;
  free_lun,clut
  TK    = T    + 273.15
  TKmax = Tmax + 273.15
  TKmin = Tmin + 273.15

  ;
  ; bytscl image between Tkmin and Tkmax
  ;
  goestemp =  bytscl( goestemp, min=TKmin,max=TKmax,$
                      top= CLOUD_INDICES(1)-CLOUD_INDICES(0) ) + CLOUD_INDICES(0)

  goestemp =  CLOUD_INDICES(1) - goestemp + CLOUD_INDICES(0)
ENDIF ELSE BEGIN

  goestemp = bytscl( subimage/4, top= CLOUD_INDICES(1) )
  
ENDELSE 



;
IF keyword_set( z ) THEN BEGIN 
  set_plot,'Z'
  ;  print, 'reds:   ', red
  ;  print, 'greens: ', green
  ;  print, 'blues:  ', blue
  ;
  ; window size for 40S to 40N and 130E to 110W 
  ;
  device,set_resolution=[800,790], set_colors=n_elements(red)
  help,/device
ENDIF ELSE BEGIN
  window,0,xsiz=800,ysiz=790,colors=n_elements(red)
ENDELSE 
tvlct,red,green,blue

;
; Limits of requested cdsubimage
;
;
;subimage    =  goestemp(lonstart:lonend,latstart:latend) ; put test around this
subimage = goestemp
print, 'Pixel limits of subimage: ', lonstart,lonend, latstart,latend
latcent   = 0.
loncent   = (slonmax+slonmin)*0.5
; 
; set to 0 for left justification
;
latalign  = 0.0
!order=1
;

MAP_SET,latdel=10,londel=10,/grid,/continent,/cyl,latcent,loncent,/usa, $

    limit=[slatmin,slonmin,slatmax,slonmax],xmargin=[0,0],ymargin=[0,2],$
    latalign=latalign, /noborder
t5 =  systime(1)
MIMAGE = map_image(subimage,startx,starty,latmin=slatmin,latmax=slatmax, $
    lonmin=slonmin,lonmax=slonmax,compress=1)
t6 =  systime(1)
print,' time to make mimiage = ',t6-t5
; -- get the lat/lon of the image using the coordiante conversions
;
repvals = where( mimage GE 10 AND mimage LE 23, nrepvals )

IF nrepvals NE 0 THEN BEGIN 
  unpack_where, mimage, repvals, col, row 
  sz =  size( mimage )
  ncols =  sz(1)
  nrows =  sz(2)
  diff =  !d.y_vsize- nrows- startx
  im_coords =  convert_coord( col+startx,!d.y_vsize - (row+diff), /device, /to_data )
  im_coords =  im_coords(0:1,*) ; discard the 'z' value
  gg = where( im_coords(0,*) LT 0,ngg )
  IF ngg NE 0 THEN im_coords(0,gg) =  im_coords(0,gg)+360.
  t7 =  systime(1)
  lon =  transpose( im_coords(0,*) )
  lat =  transpose( im_coords(1,*) )
  im_coords = 0;
  mask =  long( lon*0 )
 ; openr,maplun,'tendeg.mask',/get_lun
 ; tendegmap =  lonarr(36,18)
 ; readu,maplun,tendegmap
 ; free_lun,maplun
 ; lon10 =  long(lon)/10l
 ; lat10 =  long(lat+90)/10l
 ; ulon =  lon10( uniq( lon10, sort(lon10) ) )
 ; ulat =  lat10( uniq( lat10, sort(lat10) ) )

  ; the mask used in the ten degree map follows the Navy tradition of
  ; '1 if by land, 2 if by sea'. The results from the land_mask program 
  ; are 1=land, 0=sea

;  FOR i=0, n_elements( ulon )-1 DO BEGIN 
;    FOR  j= 0, n_elements( ulat )-1 DO BEGIN
;      x =  where( lon10 EQ ulon(i) AND lat10 EQ ulat(j),nx )
;      IF nx NE 0 THEN mask(x) =  tendegmap( ulon(i), ulat(j) )
;   ENDFOR
;  ENDFOR 

;  mixed =  where( mask NE 1 AND mask NE 2, nmixed )

;  IF nmixed NE 0 THEN  BEGIN
;    IF nmixed LT n_elements( lon ) THEN BEGIN
;      lon =  lon( mixed )
;      lat =  lat( mixed )
;      tmask =  long( lon*0 )
;      land_mask, lon, lat, tmask
;      mask(mixed) =  tmask
;    ENDIF ELSE BEGIN
      land_mask, lon, lat, mask
;    ENDELSE 
;  ENDIF 
  lon =  0
  lat =  0

  land =  where( mask EQ 1, nland )
  IF nland NE 0 THEN mimage( repvals( land ) ) =  LAND_COLOR_INDEX
  sea =  where( mask EQ 0, nsea )
  IF nsea NE 0 THEN mimage( repvals( sea ) ) =  $
   bytscl( mimage( repvals( sea ) ) >  OCEAN_INDICES(0), $
           top= OCEAN_INDICES(1) )

   t8 =  systime(1)
   print,' time to do land masking = ', t8-t7
ENDIF 
!order = 1;
tv,mimage,startx,starty
plot_title=''
plot_title =   'GOES ' + fileroot
map_set,latdel=10,londel=10,/grid,/cyl,latcent,loncent,$
    limit=[slatmin,slonmin,slatmax,slonmax],xmargin=[0,0],ymargin=[0,2],$
    latalign=latalign, /noerase,color=!d.n_colors-1, /label, title=plot_title, /noborder

IF do_winds THEN BEGIN 
  ; read the winds file 
  t9 =  systime(1)
  openr,windslun,windfile,/get_lun, error= err
  IF err EQ  0 THEN begin
    exe_str =  'wc -l ' + windfile 
    spawn, exe_str, retval
    nrecs =  long((str_sep( retval(0), ' ' ))(0))
    ii =  0
    rec =  fltarr(5, nrecs)
    readf,windslun,rec
    free_lun,windslun
    lon =  rec(1,*)
    lat =  rec(0,*)
    gg =  where( lon LT 0, ngg )
    IF ngg NE 0 THEN lon(g) = lon(g) + 360.
    gg = 0
    u = rec(2,*)
    v =  rec(3,*)
    landflag = rec(4,*)
    rec =  0b
    gg =  where( landflag EQ 0 AND $
                 lon GE minlon AND lon LE maxlon AND $
                 lat GE minlat AND lat LE maxlat, ngg )
    u =  u(gg)
    v = v(gg)
    lat = lat(gg)
    lon = lon(gg)
    landflag =  0b
    gg =  0b
    ; plot the wind vectors
    ploters_mws,u,v,lon,lat,2, length=0.011, start_index = WIND_INDICES(0)

    t10 =  systime(1)
    print,' time spent adding winds ',t10-t9
  ENDIF ELSE BEGIN
    message," Couldn't open winds file " + windfile,/cont
  ENDELSE 
   t9 =  -1;
   t10 =  -1
ENDIF 

IF keyword_set(z) THEN BEGIN
  !order = 0
  T      = tvrd()
  ;  srfdir = '/home/gennari/srf/'
  ;
  ;  srfile =  GOESTIME  + '.srf'
  ;  srfile = srfdir + srfile
  ;  print,'Writing srf file to ', srfile
  ;  write_srf, srfile, T, red, green, blue
  gifdir =  './'
  giffile =  fileroot + '.gif'
  print,'Writing gif file to ', giffile
  write_gif, giffile, T,red,green,blue

  device,/close
ENDIF 
t11 =  systime(1)
print,' time from start to just before save = ', t11-t1
alltimes =  [t1,t2,t21,t3,t4,t5,t6,t7,t8,t9,t10,t11] - t1

print,' all times = ' , alltimes
save,t,mimage,subimage,slatmin,slonmin,slatmax,slonmax, $
        latalign,latcent,loncent,red,green,blue,resdeg,f=fileroot + '-new.save'

return
end
