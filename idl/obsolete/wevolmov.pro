PRO incrementpath, u,v,ui,vi,loni,lati,loni0,lati0,pathinc,nframes,timei,$
                    dlonpar,dlatpar,vloni,vlati,init=init


  IF keyword_set(init) THEN BEGIN 
    vloni = (loni-dlonpar[0])/dlonpar[2] 
    vlati = (lati-dlatpar[0])/dlatpar[2]

    ui = bilinear(u,vloni,vlati)
    vi = bilinear(v,vloni,vlati)
  ENDIF ELSE BEGIN 

    loni = (loni + cos(lati/!radeg)*ui*pathinc) MOD 360
    ;loni = (loni + ui*pathinc) MOD 360
    x = where( loni LT 0, nx)
    IF nx NE 0 THEN loni[x] =  loni[x] + 360.

    lati = lati + vi*pathinc
    x = where( abs(lati) GT 90, nx )
    IF nx NE 0 THEN BEGIN 
      loni[x] =  (loni[x] + 180) MOD 360.
      lati[x] =  180*sign(lati[x]) - lati[x]
    ENDIF 

    vloni = (loni-dlonpar[0])/dlonpar[2]
    vlati = (lati-dlatpar[0])/dlatpar[2]

    x = where( abs(lati) GT 90, nx )
    IF nx NE 0 THEN stop
    ui = bilinear(u,vloni,vlati)
    vi = bilinear(v,vloni,vlati)

    timei = timei+1
    x = where(timei GT nframes-1,nx)
    IF nx NE 0 THEN BEGIN 
      loni[x] =  loni0[x]
      lati[x] =  lati0[x]
      timei[x] =  0;
    ENDIF 

  ENDELSE 


END

;+
; NAME:  wevolmov.pro
; $Id$
; PURPOSE:  Create an 'evolution' movie
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Animation
;
; CALLING SEQUENCE:  Wevolmov, files, nframes, filetimes,
;                   object=object, starttime=starttime,
;                   endtime=endtime, lonpar=lonpar,$
;                   latpar=latpar,pathinc=pathinc, $
;                   vloninc=vloninc,vlatinc=vlatinc,
;                   xsize=xsize,ysize=ysize, runup=runup, $
;                   minv=minv, maxv=maxv, bottom=bottom, $
;                  ncolors=ncolors, length=length
; 
;
; INPUTS:  
;
;   What constitutes *required* and *optional* input depends on the
;   value of the keyword 'object.' If this is an object of type
;   'GEOANIM' then `files,' `nframes,' and `filetimes' are all
;   optional, since all the information they would convey should
;   already be contained in the object. If, however, `object' is a
;   string or is simple absent, then `files,' and `nframes' are
;   *required* and `filetimes' is optional, depending on whether the
;   object used to read `files' is able to determine the `time' of
;   the data in each file.
;
;  
;   In short, this program is a wrapper to the object which reads the
;   data and calculates each successive frame. The parameters
;   `files',`nframes' (and `filetimes' optionally) are just passed to
;   this object. You can either load the object separately, prior to
;   calling this routine, and then pass in the, now fully loaded,
;   object, or you can pass these parameters to the object
;   through the interface of this routine. It's just a matter of when
;   the object gets created. Either it's created  prior to calling
;   this routine and passed in or you pass the arguments needed to
;   create the object and it's created inside this routine.
;
;   Files: (I). Array of strings. Fully qualified names of the  files
;          to be used.  
;   nframes: (I), scalar integer. The number of frames 
;
;
; OPTIONAL INPUTS:  
;
;   filetimes: (I), array of vaptimes or IDLDTs giving the 'times' of
;             each of the fields in each of the files in FILES.
;
;             VAPTIME is a string with format 'yyyy/mm/dd/hh/mm'
;
;   Note Bene: If `filetimes' isn't present, the `object' must be able
;              to determine the `time' of the data it is reading. This
;              is a REQUIREMENT on the object created to read this data.
;
; KEYWORD PARAMETERS:  
;
;   object: (I).
;
;           This can be one of two creatures. 
;
;           An Object of type 'GEOANIM': In this case, it already
;           contains all information about the animation, the number
;           of frames the files to read ... etc.
;
;           A scalar string. In this case, it's the name of the object
;           to be use in reading the data, in which case, 'files' and
;           `nframes' must be present. If this object isn't designed
;           to determine the `time' of the field from the data itself,
;           the keyword `filetimes' must also be present.  If this
;           keyword is absent, it will default to the string 'qmodela', the
;           QuikSCAT interpolated field model object.
;
;
;   starttime: The starttime of the animation. Must lie between the
;              time of the first and last files, inclusive. May be
;              specified as VAPTIME or IDLDT. If object is a GEOANIM
;              then this keyword is ignored and the starttime
;              specified by the GEOANIM object is used.
;
;   endtime: The endtime of the animation. Must lie between the
;              time of the first and last files, inclusive.
;              May be specified as VAPTIME or  IDLDT. If `object' is a
;              GEOANIM, this keyword is ignored and the endtime
;              specified by the GEOANIM object is used.
;
;   lonpar: float 2-vector, [min,max]. The Longitude start and end
;           points of the `view', NOT of the data!
;   latpar: float 2-vector, [min,max]. "   Latitude   "     "  "
;           points of the `view', NOT the data. 
;
;
;     lonpar/latpar are the endpoints of the 'view,' i.e. what the
;     user will see when the final animation is made. An attempt will
;     be made to extend the portion used in the animations so that
;     there is a 10 degree border around this view, so that the
;     vectors enter and exit the field gracefully. However, the extent
;     of the data is determined by the data, naturally enough.
;     
;
;   pathinc: Used to control the amount each 'vector' moves in a
;            frame. Quite simply, it's the constant of proportionality
;            in the expression 
;
;            location[frame+1] = location[frame] + pathinc*location[frame]
;
;                          -- that is --
;
;            So loni[frame+1] = loni[frame] + pathinc*ui.
;
;            Where `loni' are the locations of the vectors in any given
;            frame and `ui' is the U component for that frame. Simile
;            for lati.
;
;            If this keyword is not set, the routine will increment
;            them 'naturally,' i.e. it will move them however far they
;            would move on the surface of the planet in the time that
;            would elapse between the frames. Of course, this depends
;            on the units of the data. Here I'm assuming that we're
;            using the QuikSCAT data, so the units will be in
;            meters/second.
;            
;            If you pass this keyword in, it must be in the correct
;            units for the data that you are using and the amount of
;            time between frames. You may determine the delta time
;            between frames in the following manner.
;
;            Time[1st  frame] = starttime | filetimes[first]
;            Time[last frame] = endtime   | filetimes[last]
;
;            time_between_frames = 
;              (Time[1st Frame] - Time[last frame])/(nframes-1)
;
;            The 'unit' of this quantity depends on how big it is. You
;            may determine the unit by the following calculation
;
;            Unit='day'
;            If time_between_frames < 1 day then unit='hour'
;            If unit='hour' and time_between_frames < 1 then unit='minute'
;            If unit='minute' and time_betwee_frames < 1 then unit='second'
;   
;            'Hour' will be the most likely unit.
;
;            One last word on this subject. If you construct a GEOANIM
;            object and pass it in, you will have been able to find
;            what this unit is from the GEOANIM object itself and use
;            that information to construct the correct `pathinc.' Just
;            do the following, once you have the GEOANIM object.
;
;            idl> goeanim->get,delta_t=delta_t
;            idl> print,delta_t.units
;
;            This is a string, telling what units the 'delta' time
;            between frames is
;
;
;            Truth to tell, I'm assuming that we will usually be using
;            the QuikSCAT data and calculating `pathinc' in the
;            natural way, if you are using different data, I suggest
;            you construct the GEOANIM object and pass it and a
;            calculated `pathinc' in  yourself. 
;
;
;   xsize: the number of pixels in the X direction.
;   ysize:  ------------- " ---------- Y ---------
;
;   Vloninc: The longitude increment of the initial vector field. The
;            extent of the 'view' will be the same as above,
;            i.e. [lonpar[0]-10, latpar[0]-10, lonpar[1]+10,
;            latpar[1]+10], provided this is possible.
;
;   Vlatinc: The latitude  increment of the initial vector field.
;
;   MinV: The minimum 'velocity' to be used in the underlying contour
;         field. (default=1)
;   MaxV: The maximum 'velocity' to be used in the underlying contour
;         field. (default=25)
;
;   bottom: the minimum index into the color table, to be used in
;           making the background color contour of the speed.
;   
;   ncolors: the number of colors to be used for this speed contour.
;    
;   length: the length of the arrows
;
; OUTPUTS:  This routine will output NFRAMES of gif files.
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
; Revision 1.1.1.1  2001/12/04 19:19:30  vapuser
; Imported sources
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO wevolmov, files, nframes, filetimes, object=object, $
                   starttime=starttime, endtime=endtime, lonpar=lonpar,$
                   latpar=latpar,vloninc=vloninc,vlatinc=vlatinc, $
                   xsize=xsize,ysize=ysize, runup=runup, $
                   minv=minv, maxv=maxv, bottom=bottom, $
                   ncolors=ncolors, ctfile=ctfile, colortable=colortable, $
                   length=length

  IF chkidlver('5.1.1') NE 1 THEN BEGIN 
    Message,"Need at least IDL version 5.1.1!",/cont
    return
  ENDIF 


  save_device = !d.name
;  catch, error
;  IF error NE 0 THEN BEGIN 
;    catch,/cancel
;    Message,!error_state.msg,/cont
;    IF obj_valid(animobj) THEN obj_destroy,animobj
;    set_plot,save_device
;    return
;  ENDIF 

  vetted = 1
  
  IF n_elements(object)  EQ 0 THEN BEGIN 
    object =  'qmodela'
    IF n_elements(files) EQ 0 OR $
       n_elements(nframes) EQ 0 THEN vetted = 0
  ENDIF ELSE BEGIN 
    CASE 1 OF 
      isa(object,/string,/nonempty): BEGIN 
        IF n_elements(files) EQ 0 OR $
           n_elements(nframes) EQ 0 THEN vetted = 0
      END 
      isa(object,/object,objname='GEOANIM'): BEGIN 
        object-> get,nframes = nframes, data0=data0
        data0-> get,extent=extent
        obj_lonpar = extent[[0,2]]
        obj_latpar = extent[[1,3]] &  extent=0
        IF nframes EQ 0 THEN vetted = 0
        animobj = object
      END 
      ELSE: vetted = 0
    ENDCASE 
  ENDELSE 
  IF NOT vetted THEN BEGIN 
    lf = string(10b)
    ustr =  lf + "Usage: wevolmov, files, nframes ,object=object [, filetimes, " + lf 
    ustr =  ustr + $
      "starttime=starttime, endtime=endtime, lonpar=lonpar, latpar=latpar, " + lf 
    ustr =  ustr + $
    "xsize=xsize, ysize=ysize, vloninc=vloninc, vlatinc=vlatinc,minv=minv," + lf 
    ustr =  ustr + $
     "maxv=maxv, bottom=bottom, ncolors=ncolors, ctfile=ctfile, " + lf 
    ustr = ustr + "colortable=colortable" + lf
    ustr = ustr + lf + $
      " Either you must pass in already loaded GEOANIM object " + lf 
    ustr = ustr  + " via the 'object=' keyword" + lf
    ustr =  ustr + " Or you MUST pass in the positional parameters " + lf
    ustr =  ustr + " FILES, NFRAMES" + lf + lf
    ustr =  ustr + " If the object (default=qmodela) that reads FILES can not" + lf
    ustr =  ustr + " determine the time for each field from the data itself, " + lf 
    ustr =  ustr + " you must additionally pass in the parameter FILETIMES, " + lf 
    ustr =  ustr + " which give the time for each field" + lf 
    Message,"Not enough or incorrect input arguments!",/cont
    print,ustr
    return
  ENDIF 


  IF n_elements(lonpar)  EQ 0 THEN BEGIN 
    IF n_elements(obj_lonpar) NE 0 THEN $
      lonpar = obj_lonpar ELSE $
      lonpar = [0.,360]
  ENDIF 
  IF n_elements(latpar)  EQ 0 THEN BEGIN 
    IF n_elements(obj_latpar) NE 0 THEN $
      latpar=obj_latpar ELSE $
      latpar = [-60,60]
  ENDIF 
  IF n_elements(vloninc) EQ 0 THEN vloninc = 1.
  IF n_elements(vlatinc) EQ 0 THEN vlatinc = 1.
  IF N_elements(xsize)   EQ 0 THEN xsize = 640
  IF N_elements(ysize)   EQ 0 THEN ysize = 480
  IF n_elements(runup)   EQ 0 THEN runup = 60
  IF n_elements(minv)    EQ 0 THEN minv = 1
  IF n_elements(maxv)    EQ 0 THEN maxv = 25
  IF n_elements(bottom)  EQ 0 THEN bottom = 0
  IF n_elements(length)  EQ 0 THEN length = 3
  IF n_elements(colortable) EQ 0 THEN BEGIN 
    IF n_elements(ctfile) EQ 0 THEN BEGIN 
      ctfile = '/usr/people/vapuser/Qscat/Resources/Color_Tables/vap-animation.ct'
      bottom = 1
      ncolors = 28
      Message,'No Colortable, CTFILE = ' +  ctfile,/info
    ENDIF 
    ptc = ReadColorTable(ctfile)
    ct = *ptc &  ptr_free,ptc
    dim = size(ct,/dim)
    nc = dim[1]
  ENDIF ELSE BEGIN 
    dim = size(colortable,/dim)
    IF dim[1] EQ 3 THEN BEGIN 
      ct =  transpose(colortable)
      nc = dim[0]
    ENDIF ELSE BEGIN 
      ct = colortable
      nc = dim[1]
    ENDELSE 
  ENDELSE 
  IF n_elements(ncolors) EQ 0 THEN ncolors = nc-1
  IF ncolors GT nc-1 THEN BEGIN 
    Message,"ncolors too big! Colortable has only " + $
      strtrim(nc,2) + " colors",/cont
    return
  ENDIF 

  
  vlonpar = lonpar + [-10,10]
  
  IF vlonpar[1] GT 360 THEN vlonpar[1] =  vlonpar[1] - 360.

  vlatpar = -90>  (latpar+ [-10,10]) <  90



  IF n_elements(starttime) EQ 0 THEN object-> get,starttime = starttime
  IF n_elements(endtime) EQ 0 THEN object-> get,endtime = endtime

  Message,"Object  : " + obj_class(object),/info
  Message,"Lonpar  : " + string(lonpar,form='(2(f7.2,:,","))'),/info
  Message,"Latpar  : " + string(latpar,form='(2(f7.2,:,","))'),/info
  Message,"vloninc : " + strtrim(vloninc,2),/info
  Message,"vlatinc : " + strtrim(vlatinc,2),/info
  Message,"xsize   : " + strtrim(xsize,2),/info
  Message,"ysize   : " + strtrim(ysize,2),/info
  Message,"nframes : " + strtrim(nframes,2),/info
  Message,"runup   : " + strtrim(runup,2),/info
  Message,"MinV    : " + strtrim(minv,2),/info
  Message,"MaxV    : " + strtrim(maxv,2),/info
  Message,"Length    : " + strtrim(length,2),/info
  Message,"Vlonpar  : " + string(Vlonpar,form='(2(f7.2,:,","))'),/info
  Message,"VLatpar  : " + string(Vlatpar,form='(2(f7.2,:,","))'),/info
  Message,"Starttime: " + idldt2vaptime(starttime),/info
  Message,"Endtime : " + idldt2vaptime(endtime),/info

  set_plot,'z'
  device,set_resolution=[xsize,ysize]
  tvlct,transpose(ct) 
  red = reform(ct[0,*])
  green = reform(ct[1,*])
  blue = reform(ct[2,*])

  IF NOT exist(animobj) THEN $
    animobj=obj_new('geoanim',nframes,files,object,$
                    lonpar=Vlonpar,latpar=Vlatpar)
    

  IF NOT obj_valid(animobj) THEN BEGIN 
    Message,"Can't create animation objection!",/cont
    return
  ENDIF 


  ;mps2kmpday = 86.4 ; Convert meters/sec to Km/day.
  ;mps2kmphour = 3.6 ; Convert meters/sec to Km/hour.
  ;mps2kmpmin = 0.06 ; Convert meters/sec to Km/min
  
  IF n_elements(path_inc) EQ 0 THEN BEGIN 

    animobj-> get,delta_t = delta_t
    dt_unit  =  delta_t.units
    dt       = delta_t.dt
    degperkm = 1./111.31736 

    CASE dt_unit OF 
      "DAY" : pathinc = 86.4 
      "HOUR": pathinc = 3.6
      "MIN" : pathinc = 0.06
      "SEC" : pathinc = 1.e-3
    ENDCASE 
    pathinc = pathinc*dt*degperkm ; convert m/s to degs/frame
  ENDIF 

  Message,"pathinc : " + string(pathinc,form='(g10.6)'),/info

  latcent = mean(latpar)
  loncent = mean(lonpar)

  nx =  (vlonpar[1]-vlonpar[0])/vloninc +1
  ny =  (vlatpar[1]-vlatpar[0])/vlatinc +1

  loni = (loni0= (dindgen(nx)*vloninc+vlonpar[0])#replicate(1.d,ny))
  lati = (lati0= (replicate(1.d,nx)#(findgen(ny)*vlatinc+vlatpar[0])))

    ; Get the data lat and lon. 
  animobj->get,data1 =data
  data-> get,lon = lon,lat=lat

  dim = size(lon,/dim)
  nlon = dim[0] &  nlat=dim[1]
  loninc = (max(lon)-min(lon))/(nlon-1)
  latinc = (max(lat)-min(lat))/(nlat-1)

  dlonpar = [min(lon),max(lon),loninc]
  dlatpar = [min(lat),max(lat),latinc]

    ; Calculate the 'virtual indices'
  timei =  randomu(s,nx,ny)*(nframes-1)

  range = maxv-minv+1
  levels = findgen(30)/29*range+(minv-1)
  c_colors = findgen(30)/29*ncolors + bottom

  l = 0
   ;=========== Main Processing Loop ================

  FOR frm=0,nframes-1 DO BEGIN 

    curdat = animobj-> getframe(frm)
    MAP_SET,0,loncent,$
     lim=[latpar[0],lonpar[0],latpar[1],lonpar[1]],/noborder, $
       xmargin=[0,0], ymargin=[0,0]
    

    u = curdat[*,*,0]
    v = curdat[*,*,1]


    IF frm EQ 0 THEN BEGIN 

      IncrementPath,u,v,ui,vi,loni,lati,loni0,lati0,$
          pathinc,nframes,timei,dlonpar,dlatpar, vloni,vlati,/init
      ;print,loni[20,15],lati[20,15]
        ; Read in the land elevation file;
        ; Find the subarray which applies for this run and extract it.
      openr,lun,'$VAP_LIBRARY/land_elevations.bin',/get,error=err
      IF err NE 0 THEN BEGIN 
        Message,!error_state.msg,/cont
        return
      ENDIF 
      landel =  intarr( 12*360, 12*180 + 1 )
      readu,lun, landel
      free_lun, lun

        ; First frame. Run up `runup' frames
      FOR i=0,runup-1 DO BEGIN 
        IncrementPath,u,v,ui,vi,loni,lati,loni0,lati0,$
          pathinc,nframes,timei,dlonpar,dlatpar, vloni,vlati
          print,loni[20,15],lati[20,15]
        l = 1
      ENDFOR 


      xx = lindgen(xsize)#replicate(1l,ysize)
      yy = replicate(1l,xsize)#lindgen(ysize)
      xyz =  convert_coord( temporary(xx),temporary(yy),/device,/to_data)
      xx = reform(xyz[0,*])
      b = where(xx LT 0,nb)
      IF nb NE 0 THEN xx[b] =  xx[b] + 360.
      yy = reform(temporary(xyz[1,*]))
      mask = RunLandMask(xx,yy)      
      land =  where( mask EQ 1b,nland )
      water = where( mask EQ 0b,nwater)
      watermask = (landmask=replicate(255b,xsize,ysize))
      landIm = landMask*0b
      IF nland NE 0 THEN BEGIN 
        watermask[land] = 0b
        xx = round(temporary(xx)*12.)
        yy = round( (temporary(yy)+90.)*12.)
        landIm[land] = (landel[ xx[land],yy[land] ]+31b) < 50b
      ENDIF 

      IF nwater NE 0 THEN landmask[water] = 0b

    ENDIF 


    s = minv> sqrt(u^2+v^2) <  maxv
    CONTOUR,s,lon,lat,levels=levels,min=minv,max=maxv,$
      c_colors=c_colors,/fill,/overplot,/closed

    IncrementPath,u,v,ui,vi,loni,lati,loni0,lati0,$
      pathinc,nframes,timei,dlonpar,dlatpar, vloni,vlati
      print,loni[20,15],lati[20,15]
    Plotvect,ui,vi,loni,lati,length=length,$
      min=minv,max=maxv,color=!d.n_colors-1,/scale

    im = (tvrd()  AND watermask ) + landIm

    ofilename = 'frame.' + string( frm, format='(i3.3)') + '.gif'
    WRITE_GIF, ofilename, im, red, green, blue


  ENDFOR 

  obj_destroy, animobj
  set_plot,save_device
END


