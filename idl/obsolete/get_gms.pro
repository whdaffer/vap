  pro get_GMS, yymmddhhhh, imgdir, mapdir, minlat,maxlat,minlon,maxlon,resdeg
;
  common gmsdat,gmsimage
  common limits, blatmin, blatmax, blonmin, blonmax
;
;   This program reads hdf image files that contain
;   the mapping info, image, palette, and navigation parameters
;
;   Called by GMSGOES.pro
;
;   Author: Marion Legg, 1 March 1994
;   Modified: Marion Legg, March 1996  to use new IDL 4.0.1 HDF calls
;
;  !!!!!!!!!!!!!!!!!!!!!!!   this version assumes you have some GMS data  !!!! 10/18/94 mjl
;  gmsimage=bytarr(1201,1201)
  files  = '.hdf'
  file   = yymmddhhhh + files
  file   = imgdir + '/' + file
; use Torbin's data
  print,'GMS input file: ', file
;
; most of the stuff read in here is not used by us
  rf=hdf_open(file,/read)
  vgid   = -1
  vgid   = hdf_vg_getid(rf,vgid)
  vg     = hdf_vg_attach(rf,vgid,/read)
  lone   = hdf_vd_lone(rf)
  hdf_vg_getinfo,vg,class=c,name=nm,nentries=n
  vsid   = -1
;
  print,'Read in mapping info'
;
  for ij=0,63 do begin
      vsid = hdf_vg_getnext(vg,vsid)
      res4 = hdf_vd_attach(rf,vsid,/read)
      hdf_vd_get,res4,class=cl,count=col,fields=fl,interlace=interlace,name=nl,$
          size=sl,nfields=nfs,tag=tags,ref=refs
      nrec = hdf_vd_read(res4,nl)
      hdf_vd_detach,res4
  endfor
;
  hdf_vg_detach,vg
;
  vsid   = 67
;
  print,'Read in rest of the mapping info'
;
  for ji = 0,40 do begin
      res4 = hdf_vd_attach(rf,vsid,/read)
      hdf_vd_get,res4,class=cl,count=col,fields=fl,interlace=interlace,name=nl,$
          size=sl,nfields=nfs,tag=tags,ref=refs
      nrec  = hdf_vd_read(res4,nl)
;      print,'vsid = ',vsid,' fl = ',fl,'   ', nl
      hdf_vd_detach,res4
      vsid  = vsid+1
  endfor
  print,vsid
;
  print, 'Got all the nav stuff, Now read in image and color palette info'
; Dec 1993 pal is grey scale
  hdf_dfr8_getimage,file,image,pal
;
  print, 'Read in grid information stored in Scientific Data sets'
;  Grid is pixel values at .5 degree increments from 60N to 60S
;      and 80E to 160W
;
  file2= yymmddhhhh + '.hdf'
  file2=mapdir + '/' + file2
  print,'GMS map input file: ', file2
  hdf_dfsd_getinfo,file2,nsds=n
;
;   2/94  changed to floating point
; 
  hdf_dfsd_setinfo,/restart,/float
  range  = 0L
  hdf_dfsd_getinfo,file2,dims=dims,range=range
  print,'GMS range= ', range
  print,'GMS dims=  ', dims
  hdf_dfsd_getdata,file2,val
  help,val
  val      = reform( val, 2, 241, 241 )            
  x     = reform( val(1,*,*), 241, 241 ) 
  y     = reform( val(0,*,*), 241, 241 )
  val = 0; clear memory
;
; create a .1 resolution grid of locations we want
; to extract from original image
; result is a square image going +60 to -60 and 80-200
  ;

  ; not so fast, let's adjust a couple of things first
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
  x =  0l
  y =  0l
  t4 =  systime(1)
  print,' time to interpolate new row/cols = ', t4-t3
  ;
  print, 'Find values of image at finer locations'
  ;
  gmsimage = image( newx, newy )

;  locx    = indgen(1201)*.2 
;  locy    = indgen(1201)*.2
;  index1   = indgen(241)*2
;  index2   = indgen(241)*2 +1 
;  val1     = val(index1,*)    
;  val2     = val(index2,*)    
;  val2b    = interpolate(val2,locx,locy,/grid)
;  val1b    = interpolate(val1,locx,locy,/grid)
  return
  end
