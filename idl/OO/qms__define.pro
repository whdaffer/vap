;+
; $Id$
;
; NAME:   qms__define
; PURPOSE:   Defines and object of type qms. Used in displaying
;          QuikSCAT Interpolated Models.
;
; Note Bene: This routine assumes that you've already set up the color
;            environment the way you want it, that you are using 8 bit
;            color and that you've already loaded the color table you
;            want to use! Caveat User!
;
; AUTHOR; William Daffer
;
; CATEGORY:   OO
;
; CALLING SEQUENCE:   qms = obj_new('qms',...)
; 
; METHODS:
;
;   Init: file [, $
;         path=path, $               ; Path for subsequent reads
;         lonrange = lonrange, $     ; min/max longitude of view
;         latrange=latrange, $       ; simile for latitude
;         minspeed=minspeed, $       ; one way of specifying min speed
;                                    ;  (m/s)
;         maxspeed=maxspeed, $       ; one way of specifying max speed
;                                    ;  (m/s)
;         speedrange=speedrange, $   ; The other way to specify both.
;         xsize=xsize, $             ; size of window in X dir
;         ysize=ysize, $             ; and Y dir
;         bottom=bottom, $           ; Bottome color index
;         ncolors=ncolors, $         ; Number of colors to use
;         plotvect=plotvect, $       ; 1 => show vectors
;         showvoids=showvoids, $     ; 1 => don't chop speed array at minspeed
;         groupid=groupid ]          ; in case you want to tie this to
;                                    ; another widget.
;
;   Set: All are keywords 
;       (e.g. you set the min speed by sayin qms->set,minspeed=2)
;       I've left some out, which are only usefulto the program itself.
;       minspeed 
;       maxspeed
;       speedrange
;       lonrange
;       latrange
;       bottom
;       ncolors
;       file
;       path
;       groupid
;       xsize
;       ysize
;       plotvect
;       showvoids
;
;   Get: (All keywords)
;       I've left some out, which are only usefulto the program
;       itself.
;
;       minspeed 
;       maxspeed
;       speedrange
;       lonrange
;       latrange
;       bottom
;       ncolors
;       file
;       path
;       xsize
;       ysize
;
;   Cleanup: No params or keywords
;
;
; SIDE EFFECTS:  Creates an object of type 'qms' which uses memory in
;               a magical way. If you don't destroy the object
;               (i.e. by using IDL> Obj_destroy,qms) but rather you
;               only say qms=0, you won't have destroyed the memory
;               it's using. In actuality, you will have made it
;               impossible to free that memory without exiting IDL!
;
;
; RESTRICTIONS:  
;
;            This routine assumes that you've already set up the color
;            environment the way you want it, that you are using 8 bit
;            color and that you've already loaded the color table you
;            want to use!
;
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 2000, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
; 
;     INPUTS:   file: FQFN
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS: 
;         path: The path to use when opening new files
;         lonrange: [min,max] the longitudinal extent of the view.
;         latrange: [min,max] the latitudinal extent of the view.
;         minspeed: min speed
;         maxspeed: max speed
;            -- or --
;         speedrange: [min,max][
;         xsize: size of window in x direction in pixels
;         ysize: size of window in y direction in pixels
;         ncolors: the number of colors to use
;         bottom: the bottom color index.
;         plotvect: flag. If set, plot the vectors as well as the
;                   speed array.
;         showvoids: Allow the speed array to have values less than
;                    minspeed.
;         groupid: in case you want to tie this in to another widget
;         
;     OUTPUTS:  An object of type qms.
;
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

FUNCTION qms::Init, file, $
            path=path, $
            lonrange = lonrange, $
            latrange=latrange, $
            minspeed=minspeed, $
            maxspeed=maxspeed, $
            speedrange=speedrange, $
            xsize=xsize, $
            ysize=ysize, $
            bottom=bottom, $
            ncolors=ncolors, $
            plotvect=plotvect, $
            showvoids=showvoids, $
            groupid=groupid
            

  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    message,!error_state.msg,/cont
    return,0
  ENDIF 

  IF n_elements(file) NE 0 THEN BEGIN 
    IF self->read(file) NE 1 THEN return,0
  ENDIF 

  IF n_elements(path) NE 0 THEN self.path = path

  IF n_elements(lonrange) NE 2 THEN BEGIN 
    self.lonrange =  [0,360] 
  ENDIF ELSE self.lonrange = lonrange

  IF n_elements(latrange) NE 2 THEN BEGIN 
    self.latrange =  [-90,90] 
  ENDIF ELSE self.latrange = latrange
  
  
  self.info.xsize  = n_elements(xsize)    EQ 0? 640: xsize
  self.info.ysize  = n_elements(ysize)    EQ 0? 480: ysize
  self.bottom = n_elements(bottom)   EQ 0? 0: bottom
  self.ncolors = n_elements(ncolors)  EQ 0? !d.n_colors-1: ncolors <  (!d.n_colors-1)
  IF n_elements(speedrange) EQ 2 THEN BEGIN 
    self.speedrange = speedrange
  ENDIF ELSE BEGIN 
    self.speedrange[0] = n_elements(minspeed) EQ 0? 1: minspeed
    self.speedrange[1] = n_elements(maxspeed) EQ 0? 25: maxspeed 
  ENDELSE 
  self.info.groupid = n_elements(groupid) EQ 0? 0: groupid
  self.plotvect = keyword_set(plotvect)
  self.showvoids = keyword_set(showvoids)
  self-> draw
  return,1
END

;============================================
; READ
;
;============================================

FUNCTION qms::read, file
   IF n_params() LT 1 THEN return,0
   q = obj_new('qmodel',file=file)
   IF NOT obj_valid(q) THEN return,0
   self.file = file
   self.path = path(self.file)
   s = q-> getplotdata(u,v,lon,lat)
   obj_destroy,q
   ptr_free, self.u, self.v, self.speed, $
     self.lon, self.lat
   speed = sqrt(u^2 + v^2)
   self.u = ptr_new(u,/no_copy)
   self.v = ptr_new(v,/no_copy)
   
   self.speed =  ptr_new(speed,/no_copy)
   self.lon = ptr_new(lon,/no_copy)
   self.lat =  ptr_new(lat,/no_copy)
   return,1
END

;============================================
; Cleanup
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO qms::Cleanup
   ptr_free, self.u, self.v, self.speed, self.lon, self.lat
   widget_control, self.info.tlb, /destroy, bad_id=id
END


;============================================
; Set Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO qms::Set, $
       minspeed = minspeed,$ 
       maxspeed=maxspeed,$ 
       speedrange=speedrange, $
       lonrange=lonrange,$ 
       latrange=latrange,$ 
       bottom=bottom,$ 
       ncolors=ncolors,$ 
       file=file,$
       path=path, $
       info=info, $
       cfginfo=cfginfo, $
       groupid=groupid,$ 
       xstatic = xstatic, ystatic=ystatic, $
       xsize=xsize, ysize=ysize, $
       tlbxsize=tlbxsize, tlbysize=tlbysize, $
       plotvect=plotvect, $
       showvoids=showvoids

  IF n_elements(minspeed) NE 0 THEN self.speedrange[0] =  minspeed
  IF n_elements(maxspeed) NE 0 THEN self.speedrange[1] =  maxspeed
  IF n_elements(speedrange) EQ 2 THEN self.speedrange = speedrange
  IF n_elements(lonrange) EQ 2 THEN self.lonrange = lonrange
  IF n_elements(latrange) EQ 2 THEN self.latrange = latrange
  IF n_elements(xsize) NE 0 THEN self.info.xsize =  xsize
  IF n_elements(ysize) NE 0 THEN self.info.ysize =  ysize
  IF n_elements(bottom) NE 0 THEN self.bottom =  bottom
  IF n_elements(ncolors) NE 0 THEN self.ncolors = ncolors <  (!d.n_colors-1)
  IF n_elements(file) NE 0 THEN BEGIN 
    oldFile = self.file
    IF self-> read(file) NE 1 THEN BEGIN 
      r = dialog_message(["Can't read file " + file, $
                          "Restoring old file!"],/error)
      s = self-> read(oldFile)
    ENDIF 
  ENDIF 

  IF n_elements(path) NE 0 THEN self.path = path
  IF n_elements(info) NE 0 THEN self.info = info
  IF n_elements(cfginfo) NE 0 THEN self.cfginfo = cfginfo
  IF n_elements(xstatic) NE 0 THEN self.info.xstatic = xstatic
  IF n_elements(ystatic) NE 0 THEN self.info.ystatic = ystatic
  IF n_elements(xsize) NE 0 THEN self.info.xsize = xsize
  IF n_elements(ysize) NE 0 THEN self.info.ysize = ysize

  IF n_elements(tlbxsize) NE 0 THEN self.info.tlbxsize = tlbxsize
  IF n_elements(tlbysize) NE 0 THEN self.info.tlbysize = tlbysize

  IF n_elements(plotvect) NE 0 THEN self.plotvect = keyword_set(plotvect)
  IF n_elements(showvoids) NE 0 THEN self.showvoids = keyword_set(showvoids)


END


;============================================
; Get Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO qms::Get, info = info, $
       cfginfo=cfginfo, $
       minspeed = minspeed,$ 
       maxspeed=maxspeed,$ 
       speedrange=speedrange, $
       lonrange=lonrange,$ 
       latrange=latrange,$ 
       bottom=bottom,$ 
       ncolors=ncolors,$ 
       groupid=groupid,$ 
       file=file, $
       path=path, $
       xstatic=xstatic,$
       ystatic=ystatic, $
       pixid=pixid, $
       wid = wid, $
       xsize=xsize, $
       ysize=ysize, $
       tlbxsize=tlbxsize, $
       tlbysize=tlbysize

       
  IF arg_present(info)       THEN info = self.info
  IF arg_present(cfginfo)    THEN cfginfo = self.cfginfo
  IF arg_present(minspeed)   THEN minspeed = self.speedrange[0]
  IF arg_present(maxspeed)   THEN maxspeed=self.speedrange[0]
  IF arg_present(speedrange) THEN speedrange=self.speedrange
  IF arg_present(lonrange)   THEN lonrange=self.lonrange
  IF arg_present(latrange)   THEN latrange=self.latrange
  IF arg_present(xsize)      THEN xsize=self.info.xsize
  IF arg_present(ysize)      THEN ysize=self.info.ysize
  IF arg_present(bottom)     THEN bottom=self.bottom
  IF arg_present(ncolors)    THEN ncolors=self.ncolors
  IF arg_present(groupid)    THEN groupid=self.info.groupid
  IF arg_present(file)       THEN file=self.file
  IF arg_present(path)       THEN path=self.path
  IF arg_present(xstatic)         THEN xstatic = self.info.xstatic
  IF arg_present(ystatic)         THEN ystatic = self.info.ystatic
  IF arg_present(pixid)      THEN pixid = self.info.pixid
  IF arg_present(wid)        THEN wid = self.info.wid
  IF arg_present(xsize) THEN xsize = self.info.xsize
  IF arg_present(ysize) THEN ysize = self.info.ysize
  IF arg_present(tlbxsize) THEN tlbxsize = self.info.tlbxsize
  IF arg_present(tlbysize) THEN tlbysize = self.info.tlbysize


END

;=====================================================================
; resize routine
;=====================================================================

PRO qms::resize, x,y
  Catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    ok =  Dialog_Message( !error_state.msg )
    Message,!error_state.msg,/cont
    return
  ENDIF 

  oldxsize = self.info.tlbxsize
  oldysize = self.info.tlbysize
  
  percent_delta_x = 1.0*x/oldxsize
  percent_delta_y = 1.0*y/oldysize

  draw_x_size = round(self.info.xsize*percent_delta_x)
  draw_y_size = round(self.info.ysize*percent_delta_y)

  Widget_Control, self.info.tlb, update=0
  Widget_Control, self.info.DrawId, draw_Xsize=draw_x_size, Draw_Ysize=draw_y_size
  Widget_Control, self.info.tlb, /update

  geom = widget_info(self.info.tlb,/geom)
  self.info.tlbxsize = geom.xsize
  self.info.tlbysize = geom.ysize

  geom = widget_info(self.info.drawId,/geom)
  self.info.xsize = geom.xsize
  self.info.ysize = geom.ysize
  
  wset, self.info.wid
  WDelete, self.info.pixId
  Window,/Free,/Pixmap, XSize=self.info.xsize, YSize=self.info.ysize
  
  self-> Draw

END
;=====================================================================
; Widget cleanup
;=====================================================================
PRO qms_cleanup, tlb
   Widget_control, tlb, get_uvalue=qms
   qms-> get,info = info
    IF info.groupid NE 0 THEN BEGIN 
      qms-> get,file = file
      event = {qms_destroy_event}
      event.id = -1L
      event.handler = (event.top= info.groupid)
      event.file = file
      widget_control, info.groupid, $
         send_event=event, /no_copy
    ENDIF ELSE BEGIN 
      obj_destroy, qms
    ENDELSE 
END

;=====================================================================
; main event routine
;=====================================================================

PRO qms_event, event
  Widget_control, event.handler, get_uvalue=qms
  qms-> get,info = info
  CASE event.id OF 
    info.tlb: qms-> Resize, event.x, event.y
    info.fileid: BEGIN 
      qms-> get,path = path,file=oldFile
      file = dialog_pickfile(path=path)
      IF strlen(file) NE 0 THEN BEGIN 
        IF qms-> read(file)  NE 1 THEN BEGIN 
          r = dialog_message(["Can't read file " + file,$
                              "Resetting old file!"],/error)
          s = qms-> read(oldFile)
        ENDIF 
        qms-> get,file = file
        widget_control, info.labelid, set_value=file
        qms-> draw
      ENDIF 
    END 
    info.configid: qms-> config
    info.quitid: widget_control, info.tlb,/destroy
  ENDCASE 
  
END

;====================================================
; Event in the Draw Widget.
;====================================================
PRO qms_draw_events,event

  ButtonTypes = ['LEFT', 'MIDDLE', 'RIGHT' ]
  Widget_Control, event.top, get_uval=qms
  qms->Get,XStatic = xs ,YStatic=ys
  EventTypes =  [ 'DOWN','UP','MOTION','SCROLL']
  IF EventTypes( event.type ) NE 'DOWN' THEN return
;  print,'press = ',event.press
  Button = ButtonTypes[ (event.press AND 7)/2 ]

  qms->get,lonrange=lon, latrange=lat

  qms-> Get,PixId = PixId, Wid=Wid
    ; Have to assure that the current plotting transformation IS
    ; the mapping transformation
  Wset, PixId ; Set the Pixmap
    ; Set the Map coordinate transformation
  center = [ mean(lon), mean(lat) ]
  Map_Set,center[1], center[0], limit=[Lat[0],Lon[0],Lat[1],Lon[1]]
    ; Copy the data back to the Pixmap
  qms-> CopyToPixMap
    ; Set Window ID back
  Wset, Wid
    ; if Config widget is open, send a
    ; message to it to redo the list of map dimensions.
;  CASE Button OF 
;    'LEFT': BEGIN 
      xs =  event.x
      ys =  event.y
      qms->Set,XStatic = event.x,YStatic=event.y
      Widget_Control, Event.id, /Draw_Motion_Events, $
         Event_pro='qms_motion_events'

;    END
;    'MIDDLE': BEGIN 
;    END
;    'RIGHT': BEGIN 
;    END
;  ENDCASE 

  Widget_Control, event.top, set_uval=qms
END


;====================================================
; Motion Events
;====================================================
PRO QMS_Motion_Events, event
   
  ;print,'pv_motion_events'
  Widget_Control, event.top, get_uval=qms
  qms->Get,xsize = xsize,ysize=ysize,$
    pixId=PixId, XStatic=xs, YStatic=ys, wid=wid

  EventTypes =  [ 'DOWN','UP','MOTION','SCROLL']
  ThisEvent =  EventTypes(event.type)
  WSET, wid

  IF ThisEvent eq 'UP' THEN BEGIN 
    ;print,'Up Event!'
    qms-> get,lonrange = lon,latrange=lat
    Device, Set_Graphics_Function=3 ; copy source to destination
    Device, copy=[0,0,xsize, ysize, 0, 0, pixID ]
    Widget_Control, event.id, Draw_Motion_Events=0, $
     event_pro= 'qms_draw_events'
    x =  [ min([ xs, event.x ]), max( [ xs, event.x ] ) ]
    y =  [ min([ ys, event.y ]), max( [ ys, event.y ] ) ]
    Coords =  Convert_Coord( x, y, /device,/to_data)
    bad =  where( abs(Coords) GT 360., nbad )
    xx =  coords(0,*)
    yy =  coords(1,*)
    IF nbad GT 0 THEN BEGIN 
      str =  'Bad coordinate conversion, limiting pick to < current ROI '
      Message, str,/cont

      IF xx(0) GT 360. THEN xx(0) =  Lon(0)
      IF xx(1) GT 360. THEN xx(1) =  Lon(1)

      IF yy(0) GT 360. THEN yy(0) =  Lat(0)
      IF yy(1) GT 360. THEN yy(1) =  Lat(1)

    ENDIF 
      ; use fixlonrange to deternime whether this range should be
      ; east/west long
    xx =  FixLonRange(xx)
    IF (abs(xx(0)-xx(1)) GT 0.001 AND $
        abs(yy(0)-yy(1)) GT 0.001) THEN BEGIN 


      limits1 = [xx[0],yy[0],xx[1],yy[1]]
      qms-> set,lonrange = [ xx[0], xx[1] ], latrange=[ yy[0],yy[1] ]
      qms->Draw
    ENDIF ELSE BEGIN
      str =  'Selected area too small, only 0.001 by 0.001 degrees '
      Message,str,/cont
      ok = Dialog_Message(str)
    ENDELSE 
    return
  ENDIF 
  ;print,' doing device copy '
  DEVICE,set_graphics_funct=3 ; copy
  DEVICE, copy=[0,0,xsize, ysize, 0,0, pixId ]
  xd = event.x
  yd = event.y
  Coords =  Convert_Coord( xd, yd, /device,/to_data)

;  qms->Set, XDynamic = XD, YDynamic=YD

  val = 'X: ' + string( Coords(0), form='(f7.3)') + $
        'Y: ' + string( Coords(1), form='(f7.3)') 

  DEVICE,set_graphics_funct=6 ; xor the source and destination


  PlotS, [xs, xs, xd, xd, xs], $
         [ys, yd, yd, ys, ys], $
            /Device, Color=!d.n_colors-1

  DEVICE, set_graphics_funct=3
  Widget_Control, event.top, set_uval=qms
END

;====================================================
;
; Pro qms::CopyToPixmap. Copies the contents of the current draw window
; to a pixmap.
;
;====================================================
PRO qms::CopytoPixmap
  WSet, self.info.pixID
  Device, Copy=[0, 0, self.info.xsize, self.info.ysize, 0, 0, self.info.wid]
  Wset, self.info.wid
END


;=====================================================================
; config event routine
;=====================================================================
PRO qms_config_event, event
  redraw = 0
  widget_control, event.handler, get_uvalue=qms
  qms-> get,cfginfo = cfginfo
  IF event.id EQ cfginfo.cancelid THEN BEGIN 
    widget_control, cfginfo.tlb,/destroy
    return
  ENDIF 
    
  IF event.id NE cfginfo.applyid AND   $
     event.id NE cfginfo.doneid THEN return

  qms-> get,speedrange = speedrange, lonrange=lonrange, $
        latrange=latrange, bottom=bottom, ncolors=ncolors

  widget_control, cfginfo.speedid, get_value=v
  x = where( abs(v-speedrange) GT 0.1, nx)
  IF nx NE 0 THEN BEGIN 
    qms-> set,speedrange = v
    redraw = 1
  ENDIF 

  widget_control, cfginfo.lonid, get_value=v
  x = where( abs(v-lonrange) GT 0.1, nx )
  IF nx NE 0 THEN BEGIN 
    qms-> set,lonrange = v
    redraw = 1
  ENDIF 

  widget_control, cfginfo.latid, get_value=v
  x = where( abs(v-latrange) GT 0.1, nx )
  IF nx NE 0 THEN BEGIN 
    qms-> set,latrange = v
    redraw = 1
  ENDIF 

  widget_control, cfginfo.colorid, get_value=v
  IF v[0] NE bottom THEN BEGIN 
    qms-> set,bottom = v[0]
    redraw = 1
  ENDIF 
  IF v[1] NE ncolors THEN BEGIN 
    qms-> set,ncolors = v[1]
    redraw = 1
  ENDIF 

  IF event.id EQ cfginfo.doneid THEN $
      widget_control, cfginfo.tlb,/destroy

  IF redraw THEN qms-> draw
  

END

;============================================
; qms::config
;============================================

PRO qms::config
   ;forward_FUNCTION cw_doubleslide

   self.cfginfo.tlb = widget_base(group_leader=self.info.tlb, /col, $
                                  title='Qmodel Show Config',/map)
   junk = widget_base(self.cfginfo.tlb,/col)
   self.cfginfo.speedid = cw_doubleslide(junk,/titlerc,$
                                         TitleDs="Speed (m/s)", $
                                         format='(f7.2)',$
                                         min=0, max=50,$
                                         valu=self.speedrange,$
                                         /drag)
   self.cfginfo.lonid = cw_doubleslide(junk,$
                                            /titlerc,$
                                            TitleDS='Longitude Range', $
                                            format='(f7.2)',$
                                            min=-180,max=360.,$
                                            value=self.lonrange,$
                                            /drag)
   self.cfginfo.latid = cw_doubleslide(junk,$
                                            /titlerc,$
                                            TitleDS='Latitude Range', $
                                            min=-90.,max=90.,$
                                            format='(f7.2)',$
                                            value=self.latrange,$
                                            /drag)
   self.cfginfo.colorid = cw_doubleslide(junk, $
                                           format='(f5.0)',$
                                           TitleDS='Bottom/Ncolors',$
                                           min=0,max=!d.n_colors-1,$
                                           value=[self.bottom,self.ncolors],$
                                           /drag)
   self.cfginfo.cancelid = widget_button(self.cfginfo.tlb,value='Cancel')
   self.cfginfo.applyid = widget_button(self.cfginfo.tlb,value='Apply')
   self.cfginfo.doneid = widget_button(self.cfginfo.tlb,value='Done')
   widget_control, self.cfginfo.tlb,/realize,set_uvalue=self

   xmanager,'qms_config',self.cfginfo.tlb,/no_block
END

;============================================
; qms::draw
;============================================
PRO qms::draw, plotvect = plotvect, showvoids=showvoids
  IF NOT widget_info( self.info.tlb,/valid ) THEN self-> create_widget
  wset,self.info.wid
  IF keyword_set(plotvect) THEN $
    plotvect =  1 ELSE plotvect= self.plotvect

  IF keyword_set(showvoids) THEN $
    showvoids =  1 ELSE showvoids= self.showvoids

  loncent = mean(self.lonrange)
  minv = self.speedrange[0]
  maxv = self.speedrange[1]
  nc = self.ncolors
  bottom = self.bottom
  
  map_set,0,loncent,lim=[self.latrange[0],self.lonrange[0],$
                         self.latrange[1],self.lonrange[1]]
  nc1 = nc < 30
  IF showvoids THEN minv = 0
  s = minv> *(self.speed) < maxv

  ln = *(self.lon) 
  la = *(self.lat)
  range = (maxv-minv)+1
  levels = findgen(nc1)/(nc1-1)*range+minv-1
  c_colors = findgen(nc1)/(nc1-1)*(nc-1-bottom)+bottom

  contour,s,ln,la,levels=levels,c_colors=c_colors,min=minv,$
   max=maxv,/cell_fill,/overplot

;  contour,ss,*(self.lon),*(self.lat),$
;     levels=findgen(nc1)/(nc1-1)*(nc-1)+self.bottom,$
;      /cell_fill,/overplot
;  contour,ss,*(self.lon),*(self.lat),nlev=nc,/cell_fill,/overplot
;   contour,minv> *(self.speed)<maxv,*(self.lon),*(self.lat),levels=levels,$
;     /cell_fill,/overplot, min_v=minv,max_v=maxv

  IF plotvect THEN $
    plotvect, *(self.u), *(self.v), ln, la, color=!d.n_colors-1, len=3

  map_continents,/fill,color=!d.n_colors-1
  self-> copytopixmap
END

;============================================
; Create the widget
;
;============================================
PRO qms::create_widget
   IF NOT widget_info( self.info.tlb, /valid ) THEN BEGIN 
     self.info.tlb =  widget_base( /tlb_size_events,row=3)
     junk = widget_base(self.info.tlb,/row)
     self.info.fileid   = Widget_Button(junk, value='File')
     self.info.configid =  widget_button(junk,value='Config')
     self.info.quitid =  widget_button(junk,value='Quit')
     junk = widget_base(self.info.tlb,/row)
     self.info.labelid = widget_label(junk,value=self.file,/align_center,$
                                      /dynamic_resize)
     self.info.DrawId =  widget_Draw( self.info.tlb, scr_xsize=self.info.xsize, $
                                 scr_ysize=self.info.ysize,/button_events, $
                                 retain=2, event_PRO='qms_draw_events')
     widget_control, self.info.tlb,/realize
     widget_control, self.info.drawid, get_value=wid
     self.info.wid = wid
     window,/free,/pixmap,xsize=self.info.xsize,ysize=self.info.ysize
     self.info.pixid = !d.window
     wset, self.info.wid
     geom = Widget_info( self.info.tlb,/geometry)
     self.info.tlbxsize = geom.xsize
     self.info.tlbysize = geom.ysize

     Widget_Control, self.info.tlb, set_uval=self
                                ; Call Xmanager to Manage the Widget.
     XManager, 'QMS', self.info.TLB, /no_block,cleanup='qms_cleanup'
     
   ENDIF 
END

;============================================
; qms_destroy_event__define
;============================================

PRO qms_destroy_event__define
  junk = {QMS_DESTROY_EVENT, $
          Id: 0L, $
          Top: 0L, $
          Handler: 0L, $
          file:''}
END

;============================================
; qms_info define
;============================================
PRO qms_info__define
  junk = {qms_info,$
          tlb: 0L, $
          groupid: 0L, $
          fileid: 0l, $
          quitID: 0L, $
          configid: 0l, $
          labelid: 0l,$
          drawid: 0l, $
          wid: 0l, $
          pixid: 0l, $
          xsize: 0l, $
          ysize: 0l, $
          xstatic: 0L, $
          ystatic: 0L, $
          tlbxsize: 0l, $
          tlbysize: 0l }
END


;============================================
; qmscfg_info define
;============================================

PRO qmscfg_info__define
   junk = {qmscfg_info, $
           tlb: 0L, $
           speedId: 0L, $
           lonId: 0l, $
           latid: 0l, $
           colorId: 0l, $
           applyid: 0l, $
           cancelId: 0l, $
           doneid: 0L}
END


;============================================
; Definition Routine
;============================================

PRO qms__define
  junk = {qms, $
          file: '', $
          path: '', $
          u: ptr_new(), $
          v: ptr_new(), $
          speed: ptr_new(), $
          lon: ptr_new(), $
          lat: ptr_new(), $
          speedrange: fltarr(2),$
          lonrange: fltarr(2), $
          latrange: fltarr(2), $
          bottom: 0l, $
          ncolors: 0l, $
          plotvect: 0l, $
          showvoids: 0l, $
          info: { qms_info }, $
          cfginfo: { qmscfg_info} $

                     
  }
END
