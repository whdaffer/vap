PRO map_goes, filenm, idata, limits
; 8/8/96   for both GOES8 and GOES9, WHOLE or SUB area    

; display one GOESx_xxxxxxx.dat to .gif 
; data has been switched in GOESx_xxxxxxx.dat for IDL display !!

; input GOESx_xxxxxxx.dat  (f77 unformatted)
;     1. Head line: array size (lin,ele), jday, time, lat/lon boundary,
;              and  lat/lon precision
;        -- 8 integers and 2 real numbers
;     2. image data (ima_e,ima_l)  
;        -- 16-bit integers
;---------------------------------------------
;***
;spawn,'date'
limits=intarr(4)
outp=lonarr(8)
outpp=fltarr(2)
IF n_elements( filenm  ) EQ 0 THEN begin
  buf='  '
  print
  print,'>> Available .dat files: '
  spawn,'ls GOES*.dat'
  print
  read,buf,prompt='>> which *.dat? '
  filenm=buf+'.dat'
ENDIF 

openr,unit,filenm,/f77_unformatted,/get_lun
print,'>> read ',filenm,' head info:'
;** readu,unit,siza,sizo,jday,time,minlat,minlon,maxlat,maxlon,preci_l,preci_e
readu,unit,outp,outpp

print,outp,outpp
siza=outp(0)
sizo=outp(1)
jday=outp(2)
time=outp(3)
minlat=outp(4)
minlon=outp(5)
maxlat=outp(6)
maxlon=outp(7)
preci_l=outpp(0)
preci_e=outpp(1)

idata=intarr(sizo,siza)
readu,unit,idata
free_lun,unit

indx=where(idata gt 0,nn)
print,'>> non-zero values in array',siza,sizo,': ', nn

device, get_screen = xsize             ;Size to machine
xsize = xsize(0) * 2 / 4
ysize = xsize * 4 / 5

;set_Plot,'z'
set_Plot,'x'
window,xsize=800,ysize=600
;   DEVICE, DECOMPOSED=0, BYPASS_TRANSLATION=0, pseudo_color=8
;  loadct,39, file='/home/dataman/idl/colors2.tbl'
;loadct,0     
lat1=0
rot0=0

lon1=0
limits(1)=minlon

if (minlon gt maxlon) then begin
lon1=-180
limits(1)=-360+minlon   
endif

;***
limits(0)=minlat  
limits(2)=maxlat 
limits(3)=maxlon   
print,'>> limits: ',limits
; coordinates set up
map_set, lat1,lon1, rot0,  grid=0, /noerase,  $
 limit = limits, color=1, /noborder
print,'>> max image data: ',max(idata)

if (minlon lt maxlon) then begin
  resimag=map_image(idata,xs1,ys1,latmax=(limits(2)), $
      latmin=(limits(0)),lonmax=(limits(3)),lonmin=(limits(1)), $
      /whole_map,compress=1)
;  resimag=bytscl(resimag,min=0,max=255)
endif

if (minlon gt maxlon) then begin
; imag=bytscl(idata/10.,min=0,max=255)
 resimag=map_patch(idata,lat0=limits(0),lat1=limits(2), $
   lon0=limits(1),lon1=limits(3)+0.01,xstart=xs1,ystart=ys1)
endif

tvscl,resimag,xs1,ys1

map_continents,/coasts,color=1

if (siza lt 2500) then begin
 map_grid, latdel = 5, londel = 5, color=255, $
        lonlab=minlat-.3,latlab=minlon, $
         charsize=.8, latalign=1
endif

if (siza ge 2500) then begin
 map_grid, latdel = 10, londel = 10, color=255, $
        lonlab=minlat-.3,latlab=minlon, $
         charsize=.8, latalign=1
endif

b=string(jday)
c=string(time)
d=string(preci_l)
e=string(preci_e)
 xyouts,!d.x_vsize-700,!d.y_vsize-20,buf+'.dat'+  $
 '  Jday: '+strmid(b,9,3)+ '   time '+strmid(c,8,2)+':'+strmid(c,10,2)+  $
 '   lat/lon preci: '+strmid(d,4,4)+' '+strmid(e,4,4),/device,color=255,charsize=.9

; t=tvrd(channel=0)
;***
; write_gif,buf+'.gif',t
print,'>> written in '+buf+'.gif'

;spawn,'date'
endit:
return

end


