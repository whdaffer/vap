  PRO COMBINE_GMSGOES,GMSTIME,GMSCALDIR, FILE=CLUT_file
;
; This program is called by GMSGOES, the compositing program
;
; Author: Marion Legg, 1 March 1994
;
; Modified, 10 May 1994: T values in combdat are no longer bytscl'd to get
;           the color map indices stored in byteimage;  
;           instead byteimage is loaded in module "lookup_T"
;
  common combdat,  bytimage
  common gmsdat,   gmsimage
  common limits, blatmin, blatmax, blonmin, blonmax
  common colortab, red,green,blue
;
;
  print,'Read GMS calibration info'
;
  val = intarr(256) & temp = fltarr(256)
;
; GMS calibration file obtained from C-Space; JTWC


cal_file = GMSTIME + '.cal'
cal_file = gmscaldir + cal_file
openr,clun,cal_file,/get_lun
a=' '
;skip to start of cal for IR1
for i = 0,99 do begin
readf,clun,a
endfor
b=0.
for i = 0, 255 do begin
readf,clun,b,format='(44x,f10.3)'
temp(i)=b
ENDFOR
free_lun, clun

;  openr,1,'gms.cal'
;  for i =0, 255 do begin
;      readf,1,temp1,temp2
;      val(i)  = temp1
;      temp(i) = temp2
;  endfor
;
  print, 'Converts GMS byte values to temperatures (REAL FAST!!)'
;
  l                   = size(gmsimage)
  gmstemp             = fltarr(l(1),l(2))
  gmstemp             = temp(gmsimage)
;
; JUST GMS data
;  combimage           = fltarr(1201,1201)
; convert gms image values to temperatures
  combimage = gmstemp
;
; Open color lookup table file and read in temperature key and color map
;
  red = bytarr(256) & green = red & blue = green
  T   = fltarr(256)
;
  openr,clut,CLUT_file,/get_lun
  readf,clut,N_colors
  readf,clut,Tmin,stepTmax,Tmax
  readf,clut,T
  readf,clut,red
  readf,clut,green
  readf,clut,blue
;
  TK    = T    + 273.15
  TKmax = Tmax + 273.15
  TKmin = Tmin + 273.15
;
;
; bytscl combimage between Tmin and Tmax
;
  bytimage = bytscl(combimage,min=TKmin,max=TKmax)
  help,bytimage
;
;  print,'Write out composite GMS/GOES image 60N to 60S and 80E to 310E'
;; here we save new hdf image file so that we don't have to
;; reprocess original image if we want another area
;; 2/27/96 write out hdf files
; p1 = ''
; p1= 'J5_______IR1_'
;  bigfile   = p1 + GMSTIME +  '.hdf'
;;  openw, flun,bigfile,/get_lun
;;  writeu,flun,combimage
;  print, 'Writing out hdf file'
;;
;  gmsdir = '/q/satprod/gms-5/ir1/'
;  bigfile = gmsdir + bigfile
;  print, bigfile
;; open hdf file
;  res1 = hdf_open(bigfile, /create, /write)
;; a is string of when file was created
;a = today(hour=1)
;fid1='There are 7 fid records including this one.'
;fid2 ='file created:' + a
;hdf_dfan_addfid,bigfile,fid1
;hdf_dfan_addfid,bigfile,fid2
;fid3= 'Produced by L. Pfister, H. Selkirk and M. Legg, NASA Ames Research Center'
;hdf_dfan_addfid,bigfile,fid3
;fid4='pfister@telsci.arc.nasa.gov'
;hdf_dfan_addfid,bigfile,fid4
;fid5='File contains 4 Scientific Data Sets and a Color Palette'
;hdf_dfan_addfid,bigfile,fid5
;fid6='set 1 lon/lat gridded image(byte), set 2 longitude locations(float) '
;hdf_dfan_addfid,bigfile,fid6 
;fid7='set 3 latitude locations(float) ; set 4 temperature table(float)'
;hdf_dfan_addfid,bigfile,fid7 
;;openw, flun, bigfile, /get_lun
;;  nsat = 2
;  bstep = .1
;;  writeu, flun, nsat
;;  print,nsat
;  sat_name='GMS-5'
;;  n_sat_name = strlen(sat_name)
;;  writeu,flun,n_sat_name
;;  writeu,flun,sat_name
;  print,sat_name
;;  n_GMSTIME = strlen(GMSTIME)
;;  writeu,flun,n_GMSTIME
;;  writeu,flun,GMSTIME
;  print,GMSTIME
;  sat_type='SVISSR-IR1'
;  pt1 = 'Satellite:'
;  pt2 = 'Time:'
;  pt3 = 'Wavelength:'
;  pt4 = 'Source:UofH/hdf'
;  label1=pt1+sat_name+';'+pt2+GMSTIME+';'+pt3+sat_type+';'+pt4
;  help,label1
;  print,label1
;;  n_sat_type = strlen(sat_type)
;;  writeu,flun,n_sat_type
;;  writeu,flun,sat_type
;  print,sat_type
;  sg = size(combimage)
;;  writeu,flun,sg(1), sg(2)
;  print,sg(1), sg(2)
;;  writeu,flun,blatmin,blatmax,bstep
;   nlats = long((blatmax-blatmin)/bstep) + 1
;   nlons = long((blonmax-blonmin)/bstep) + 1
;   help,nlats,nlons
;   lats = findgen(nlats)*bstep + blatmin
;   lons = findgen(nlons)*bstep + blonmin
;   print,lats
;   print,lons
;   pal = bytarr(3,256)
;   pal(0,*) = red
;   pal(1,*) = green
;   pal(2,*) = blue
;   hdf_dfsd_setinfo,/clear,/byte,dims=[nlons,nlats],label=label1
;   hdf_dfsd_adddata,bigfile,bytimage 
;   print,min(lons)
;   print,max(lons)
;   mlon=min(lons)
;   malon=max(lons)
;   hdf_dfsd_setinfo,/float,dims=[nlons],range=[malon,mlon],label="grid longitudes"
;   hdf_dfsd_adddata,bigfile,lons
;   mlat=min(lats)
;   malat=max(lats)
;   hdf_dfsd_setinfo,/float,dims=[nlons],range=[malat,mlat],label="grid latitudes"
;   hdf_dfsd_adddata,bigfile,lats
;   help,TK
;   print,min(TK)
;   print,max(TK)
;   mintk=min(TK)
;   maxtk=max(TK)
;   help,maxtk
;   help,mintk
;   hdf_dfsd_setinfo,/float,dims=[256],range=[maxtk,mintk],$
;    label="temperature translation table (K)"
;   hdf_dfsd_adddata,bigfile,TK
;   hdf_dfp_addpal,bigfile,pal
;;  print,blatmin,blatmax,bstep
;;  writeu,flun,blonmin,blonmax,bstep
;;  print,blonmin,blonmax,bstep
;;  n_CLUT_file = strlen(CLUT_file)
;;  writeu,flun,n_CLUT_file
;;  writeu,flun,CLUT_file
;;  writeu,flun,bytimage
;;  help,bytimage
;  hdf_close,res1
  return
  end
