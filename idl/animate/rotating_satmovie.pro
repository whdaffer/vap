;+
; NAME:  SatMovie
; $Id$
;
;  PURPOSE: Make Nscat/Qscat/Seawinds animations using the satellite
;           projection
;
; AUTHOR:  William
;
; CATEGORY:  Nscat/Qscat/Seawinds animations
;
; CALLING SEQUENCE: 
;
;        rotating_satmovie, interp_file, $
;                           loncent=loncent,$
;                           vlonspace=vlonspace, $
;                           vlatspace=vlatspace
;                           animpar=animpar, $
;                           rsfbase=rsfbase, $
;                           saveframes=saveframes
;
; INPUTS:  
;    interp_file - HDF file containing 
;                  interpolated wind field. No Default!
;
; OPTIONAL INPUTS:  None

;       
; KEYWORD PARAMETERS:  
;        loncent - Longitude of the center of the projection
;                  Default=180.
;        vlonspace - The longitude spacing between vectors
;                    Default=3. degrees
;        vlatspace - The latitude spacing between vectors
;                    Default=3. degrees
;        animpar - 3 vector, xsize, ysize, nframes of animation. 
;                  NB, since this is a satellite projection, the area
;                  of the  window taken up with the actual animation 
;                  will be considerably smaller than actual size 
;                  of the window. The window size should be
;                  modified accordingly. Default size=[600,600,720]
;
;        rsfbase - scalar string. The basename  of the
;                       hdf files containing the image frames used in
;                       making the animation. Thes files must contain
;                       *precicely* the right size and number of
;                       frames for this particular animation.
;        saveframes - flag. If set, save the frames to an hdf file. 
;                     WARNING. This file can get VERY large. The
;                     600,600,720 frame file is 700 MB. So, use this
;                     option carefully!
;
; OUTPUTS:  nframes worth of Gif files in the default directory.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO ROTATING_SATMOVIE, interp_file, $
                       loncent=loncent, $
                       vlonspace=vlonspace, $
                       vlatspace=vlatspace, $
                       animpar=animpar, $
                       path_inc=path_inc, $
                       rsfbase=rsfbase, $
                       saveframes=saveframes, maxcnt=maxcnt


COMMON prs, long_sel, lats_sel, lons, lats, uu, vv, uu_sel, vv_sel, $
           ileft, iright, itop, ibot, dist_left, dist_right, dist_top, $
           dist_bot, dist, weights, invdist, xfinc, xf0, yfinc, yf0, eps




genv,/save
IF n_params() LT 1 THEN BEGIN 
  Usage,"rotating_satmovie, interp_file[,loncent=loncent, vlonspace=vlonspace, vlatspace=vlatspace, animpar=animpar, path_inc=path_inc, rsfbase=rsfbase]"
  return
END

IF NOT isa(interp_file,/string,/nonempty) THEN BEGIN 
  Message,'INTERP_FILE must be a nonempty string!',/cont
  return
ENDIF 

IF n_elements(path_inc) EQ 0 THEN path_inc =  0.04
IF n_elements(loncent) EQ 0 THEN loncent = 180.
IF n_elements(vlonspace) EQ 0 THEN vlonspace = 3
IF n_elements(vlatspace) EQ 0 THEN vlatspace = 3
IF n_elements(animpar) EQ 0 THEN animpar = [600,600,720]

;catch, error
;IF error NE 0 THEN BEGIN
;  Message,!error_state.msg,/cont
;  return
;ENDIF 

interp_file = interp_file[0]
read_frames = 0

IF n_elements(rsfbase) NE 0 THEN BEGIN 
  IF NOT isa(rsfbase,/string,/nonempty) THEN BEGIN 
    Message,'Rsfbase must be NON-EMPTY STRING',/cont
    Message,'Frames will be calculated',/cont
  ENDIF ELSE BEGIN 
    rsf = obj_new('rsf',rsfbase)
    IF NOT obj_valid( rsf ) THEN BEGIN 
      Message,$
         'Error creating Rotating Satellite Frames object using file ' + $
          rsfbase,/cont
      return
    ENDIF 
    read_frames = 1
  ENDELSE 
ENDIF 

saveframes = keyword_set(saveframes)

IF saveframes THEN BEGIN 
  xs_str = padandjustify(animpar[0],3,pad='0',/right)
  ys_str = PadAndJustify(animpar[1],3,pad='0',/right)
  nframes_str= PadAndJustify(animpar[2],3,pad='0',/right)
  loncent_str = string(loncent,form='(f5.1)')
  loncent_str = padAndJustify(loncent_str,5,pad='0',/right)
  rsfbase = 'RSF-' + xs_str + ',' + ys_str + ',' + loncent_str + ','
  rsf = obj_new('rsf',rsfbase)
  IF NOT obj_valid( rsf ) THEN BEGIN 
    Message,$
     'Error creating Rotating Satellite Frames object using file ' + $
     rsfbase,/cont
    return
  ENDIF 
ENDIF 

eps =  0.000102
nframes = animpar[2]

  ;
  ; Define the color table. 
  ;

PtrToColorTable = ReadColorTable( $
     "$VAP_RESOURCES/Color_Tables/vap-animation.ct")
IF NOT ptr_valid(ptrToColorTable) THEN BEGIN 
  Message,"Can't Read Color table",/cont
  return
ENDIF 
CT = *PtrToColorTable &  ptr_free, ptrToColorTable
Red   = reform(CT[0,*])
Green = reform(CT[1,*])
Blue  = reform(CT[2,*])
r_curr =  red &  g_curr= green &  b_curr= blue
r_orig =  red &  g_orig= green &  b_orid= blue

n_colors = n_elements(red)


  ; ------- Read the data ------------

qmodel = qmodelhdfread(interp_file)
uu = *qmodel.u
vv = *qmodel.v
loni = *qmodel.lon
lati =  *qmodel.lat
xf0 = qmodel.hdr.lonpar[0]
xf1 = qmodel.hdr.lonpar[1]
xfinc = qmodel.hdr.lonpar[2]

yf0 = qmodel.hdr.latpar[0]
yf1 = qmodel.hdr.latpar[1]
yfinc = qmodel.hdr.latpar[2]

set_plot,'z'

 ; First, set the resolution to exactly
 ; match the lon/lat grid of the interpolated data. This grid will
 ; then be the input to map_patch.

sz = size( loni, /dim)
device, set_resolution=[sz[0],sz[1]]

ss = 1>sqrt( uu^2+vv^2)<30  

CONTOUR,ss,loni,lati,levels=findgen(30),$
 c_colors=bindgen(30),/cell_fill, xstyle=13,ystyle=13 ,position=[0.,0.,1.,1]


contour_im=tvrd() 

  ; Now set the resolution we really want.

device, set_resolution=[animpar[0], animpar[1] ]

openr,lun,'$VAP_LIB/land_elevations.bin',/get,error=err
IF err NE 0 THEN BEGIN 
  Message,!error_state.msg,/cont
  return
ENDIF 
landel =  intarr( 12*360, 12*180 + 1 ) 
readu,lun, landel 
free_lun, lun

lonrotinc = 360./nframes


erase


;  wim, continents in black, ocean white
;  wim2, continents in colors, oceans in black
;  wim3, filled contour plot of wind field
;  wim4, vectors on wim3
;  All are 600 by 600



;
;This piece does the animation and interpolation stuff
;

nvlon = 360/vlonspace
nvlat = 120/vlatspace+1
nn = nvlon*nvlat
dist        = fltarr( nn,4 )   
weights     = dist               

long_sel1 = reform( (findgen(nvlon)*vlonspace)#replicate(1.,nvlat), nn )
lats_sel1 = reform( replicate(1.,nvlon) # (findgen(nvlat)*vlatspace-60.)  ,nn ) 
long_sel  = long_sel1
lats_sel  = lats_sel1

time_mov  = fix( 60.0*randomu( seed,nn ) )
njunk = n_elements(long_sel)
;writeu, tlun, njunk
;

;
real_start_time =  systime(1)
tottime =  0.
iter =  0l
times = fltarr(nframes)

n_runup_frames = 60

t0_runup = systime(1)
FOR frame=0L,nframes+n_runup_frames-1 DO BEGIN 
  CALCWINDFIELD
  IF frame GE n_runup_frames THEN BEGIN 
    frm = frame-n_runup_frames
    t0 = systime(1)
    print,'working on frame ',frame-n_runup_frames+1
    t1_runup = systime(1)

    IF frame EQ n_runup_frames THEN BEGIN 
      print,'Runup took :', (systime(1)-t0_runup)/60., ' minutes'
    ENDIF 

    frmcnt = frame-n_runup_frames;
    MAP_SET,0,(loncent-frmcnt*lonrotinc) MOD 360.,$
      sat_p=[20,0,0],/noborder,/satellite



    IF frm EQ 0 THEN BEGIN 
      IF saveframes THEN BEGIN 

        tmp = contour_im*0+255b
        tmpim = map_patch(tmp, $
                          lon0=min(loni),lon1=max(loni),$
                          lat0=min(lati),lat1=max(lati),$
                          xsiz=xsz,ysiz=ysz,xstart=xs,ystart=ys )
        tv,tmpim,xs,ys
        tmpim = tvrd()
        mappedPixels = where( tmpim NE 0, nmappedPixels )
        s = rsf->WriteMappedPixels(mappedPixels,animpar,loncent)
        IF s NE 1 THEN BEGIN 
          Message,"Couldn't write MappedPixels!",/cont
          return
        ENDIF 
      ENDIF 
    ENDIF 
    contour_im2 = map_patch(contour_im,$
                            lon0=min(loni),lon1=max(loni),$
                            lat0=min(lati),lat1=max(lati),$
                            xsiz=xsz,ysiz=ysz,xstart=xs,ystart=ys)
    tv,contour_im2,xs,ys
    wim =  tvrd()


    IF read_frames THEN BEGIN 
      IF frm EQ 0 THEN BEGIN 
        retdata = rsf-> ReadMappedPixels()
        IF NOT isa(retdata,/structure) THEN BEGIN 
          Message,"Couldn't read Mapped Pixels!",/cont
        ENDIF 
        mappedPixels = *(retdata.mappedPixels)
        rsf-> destroy_retdata,retdata
      ENDIF 

      frame_data = rsf->ReadFile(frm)
      IF NOT isa(frame_data,/structure) THEN BEGIN 
         Message,"Couldn't read Mapped Pixels!",/cont
      ENDIF 

      landi  = *(frame_data.landi)
      landeli  = *(frame_data.landeli)
      tmp = bytarr(animpar[0],animpar[1])+255b
      tmp[mappedPixels] = 0b
      tmp[landi] =  255b
      wateri = where(tmp EQ 0, nw )
      tmp = 0
      rsf-> Destroy_retdata,frame_data
    ENDIF ELSE BEGIN 

      x =  where( wim ) 
      unpack_where, wim, x, cc, rr 

      oo = convert_coord( cc,rr,/dev,/to_data) 
      yy = where( oo(0,*) lt 360. and oo(1,*) lt 360.,nyy) 
      cc=cc(yy) & rr=rr(yy) & oo=oo(0:1,yy)  

      tlon  = reform(oo(0,*)) 
      tlat  = reform(oo(1,*))
      tmask = long(tlon*0) 
      Land_Mask,tlon,tlat,tmask 
      land=where(tmask eq 1) 
      water = where( tmask EQ 0, nw )

      t = where( tlon lt 0., nt ) 
      IF nt GT 0 THEN tlon(t) = tlon(t)+360. 
      ix = long(tlon(land)*12.)
      iy = long((tlat(land)+90)*12. )

      landi = cc[land] + animpar[0]*rr[land]
      wateri = cc[water]+animpar[0]*rr[water]
      sz = size(landel,/dim)
      landeli = ix + sz[0]*iy

      ix=(iy=(cc=(rr=(land=(water=0)))))

      IF saveframes THEN BEGIN 
        s = rsf-> WriteFile( frm, landi, landeli, animpar, loncent )
        IF s NE 1 THEN BEGIN 
          Message,"Couldn't write Frame <" + strtrim(frm,2) + ">",/cont
          return
        ENDIF 
      ENDIF 

    ENDELSE 

    
    wim3 = (wim2= wim)

;    wim(cc(land),rr(land)) = 0b
;    wim2(cc(land),rr(land)) = (landel(ix,iy)+31) < 50   
;    IF nw NE 0 THEN BEGIN 
;      wim2(cc(water),rr(water)) = 0
;      wim(cc(water),rr(water)) = 255
;    ENDIF 

    wim[landi] = 0b
    wim2[landi] = (landel[landeli]+31) < 50   
    IF nw NE 0 THEN BEGIN 
      wim2[wateri] = 0
      wim [wateri] = 255
    ENDIF 

    TV,wim3
    frm = frame-n_runup_frames+1L
    PLOTVECT, uu_sel, vv_sel, long_sel, lats_sel, $
       length=4, color=n_colors-1, thick=2
    wim4 = tvrd()
    tv,(wim4 AND wim) + wim2
    gifim =  tvrd()
    frame_str =  PadAndJustify(strtrim( frm, 2 ), 3,pad='0',/right)
    file = 'gwind.' + frame_str

    write_gif,file,gifim, red,green,blue

  ENDIF 
  long_sel = long_sel + path_inc*uu_sel
  x = where( long_sel LT 0, nx )
  IF nx NE 0 THEN BEGIN 
    long_sel[x] =  long_sel[x] + 360.
  ENDIF 

  x = where( long_sel GT 360, nx )
  IF nx NE 0 THEN BEGIN 
    long_sel[x] =  long_sel[x] - 360.
  ENDIF 

  lats_sel = lats_sel + path_inc*vv_sel
  x = where( abs(lats_sel) GT 90,nx )
  IF nx NE 0 THEN BEGIN 
    long_sel[x] =  (long_sel[x] + 180) MOD 360.
    lats_sel[x] =  sign(lats_sel[x])*90 + $
     (sign(lats_sel[x])*90 - lats_sel[x]) 
  ENDIF 
  time_mov = time_mov+1
  

  x =  where( time_mov GE 60, nx )
  IF nx NE 0 THEN BEGIN
    long_sel(x) =  long_sel1(x)
    lats_sel(x) =  lats_sel1(x)
    time_mov(x) =  0
  ENDIF 
  IF frame GE n_runup_frames THEN $
    times(frame -n_runup_frames) =  systime(1)-t0

ENDFOR 

m = moment(times)
print, 'Avg Time for 1 Frame: ', m[0],' +/- ', sqrt(m[1])


  ; If reading frames, destroy the reader object
IF read_frames THEN destroy,rsf

  ; reset to previous graphic environment
GENV,/restore

END
