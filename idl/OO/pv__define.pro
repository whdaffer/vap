;+
; NAME:  pv__define
; PURPOSE:  Defines the PV (PlotVector) Object
;
;
; AUTHOR; william Daffer
;
;
; CATEGORY:  OO
;
;
;
; CALLING SEQUENCE:  pv=obj_new('PV',files=files|data=data,...)
;
;                    See Init Method for more details
;
;
; 
; INPUTS:  
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  Read the Init Method
;           files      : list of files to read
;           data       : the Data itself, must be object of type 
;                         Q2BDATA, QMODEL or RQ2BDATA
;           xsize      : xsize of window
;           ysize      : ysize of window
;           color      : don't use this one
;           ambiguities : which ambiguities to plot, either a flag
;                         array of 6 elements (speed, ambig 1, ambig2,
;                         ambig3,ambig4, model) or a number between 0
;                         and 5.
;
;           decimate_by : n means plot every n-th vector
;           CRDecimate_by: [p,q] plot every pth column in every qth row
;           ExcludeCols  : string, e.g. "0,31:33,72" exclude colums
;                          0,31,32,33 and 75
;           InputPath    : Path to input files
;           InputFilter  : Filter to apply in open file widget
;           OutputPath   : Path to output files
;           HCType       : type of Hardcopy (ps,gif,jpeg)
;           HCFile       : base filename
;           LonRange     : Open view using this longitude range
;           LatRange     : Open view using this latitude range
;           Length       : Length of Vector (0.1 to 5)
;           Thickness    : Thickness of Vector (0.1 to 5)
;           MinSpeed     : Minimum speed to plot (0,37)
;           MaxSpeed     : Maximum speed to plot (0,37)
;           Help         : a Message 
;
;
;
;
; OUTPUTS:  If successful, and object of type 'PV'
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  Creation of many subsidiary objects which will hang
;                around if PV doesn't take care of them correctly. At
;                the moment, it is, but the user should occasionally
;                check to make sure and do a 'heap_gc' if any objects
;                are left overa after the pv object has been destroyed.
;
;
;
; RESTRICTIONS:  
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.17  1999/08/30 15:46:01  vapuser
; Took Colorbar out of Annotation. Put it in it's own
; object and put an instance of that object in the PV object.
; Changed supporting code.
;
; Revision 1.16  1999/08/26 20:59:47  vapuser
; Let See....
;   Misc code fixes.
;   Improved support for postscript
;   output.
;   Color Bar support
;
; Revision 1.15  1999/07/01 15:25:28  vapuser
; Change inputpath/outputpath defaults
;
; Revision 1.14  1999/06/30 23:07:43  vapuser
; Changed Xdisplay to XdisplayFile
;
; Revision 1.13  1999/06/29 20:59:28  vapuser
; Changed the default input filter
;
; Revision 1.12  1999/01/24 19:59:11  vapuser
; Changed to accomidate new linkedlist object.
;
; Revision 1.11  1998/11/20 20:02:01  vapuser
; Accomidate 24bit color
;
; Revision 1.10  1998/10/29 22:36:46  vapuser
; Added verbose keyword/member
;
; Revision 1.9  1998/10/28 23:28:12  vapuser
; More work on adding pvplotobject.
; More stable now.
;
; Revision 1.8  1998/10/26 22:09:37  vapuser
; In process of changing Datalist to list of PvPlotObjects. Not done yet.
;
; Revision 1.7  1998/10/23 22:18:46  vapuser
; Incoporated DeEnvVar to handle HDF_... inability to parse
; environmental variables. Handled a problem with SpeedHisto object
;
; Revision 1.6  1998/10/21 17:40:57  vapuser
; Added a 'print' statement while reading files.
;
; Revision 1.5  1998/10/21 16:28:06  vapuser
; Free attr.value PTR after call to hdfgetattr
;
; Revision 1.4  1998/10/12 22:06:36  vapuser
; Worked on Qmodel stuff
;
; Revision 1.3  1998/10/01 17:52:35  vapuser
; Modified 'version' method so that it will report
; the versions of member classes. Put in some error handling
; so that it'll ignore calls to undefined 'version' methods.
;
; Revision 1.2  1998/10/01 15:41:26  vapuser
; Eliminated rcsid member. Added 'version' method that returns rcsid string, which
; is now local to version method.
;
; Revision 1.1  1998/09/30 23:38:15  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;====================================================
;
; Main Event Handler.
;
;====================================================

PRO pv_events, event
  Widget_Control, event.top, get_uval=self
  thisEvent = Tag_Names(event, /structure_name )
  IF thisEvent EQ 'WIDGET_BASE' THEN BEGIN 
    self-> Resize, event.x, event.y
    return
  ENDIF 
  
  Widget_Control, event.id, Get_UValue=thisEvent
  CASE thisEvent OF
    'QUIT'           : self->Quit, event.top  
    'READ'           : self->WidgetRead       
    'WRITE'          : self->WidgetWrite      
    'PLOTOPTIONS'    : self->PlotOptions      
    'CONFIGHARDCOPY' : self->HardCopy
    'CLOUDOVERLAY'   : self->Overlay
    'OVERPLOTLOC'    : self->OverPLot
    'PROPAGATEORBITS': self->Propagate
    'RESETSENS'      : self-> Set, Sensitivity = 1
    'REDRAW'         : self->draw,/force
    'QUIT2'          : self->Quit, event.top  
    ELSE:
  ENDCASE
  Widget_Control, event.top, set_uval=self, bad_id=bad
END



;====================================================
;
; Event in the Draw Widget.
;
;====================================================


PRO pv_draw_events,event
;  print,'pv_draw_events'
  ButtonTypes = ['LEFT', 'MIDDLE', 'RIGHT' ]
  Widget_Control, event.top, get_uval=self
  self->Get,XStatic = xs ,YStatic=ys,firstClick=firstClick
  EventTypes =  [ 'DOWN','UP','MOTION','SCROLL']
  IF EventTypes( event.type ) NE 'DOWN' THEN return
;  print,'press = ',event.press
  Button = ButtonTypes[ (event.press AND 7)/2 ]

  Dims= self-> GetCurrentDimensions()
  Dims-> Get,Lon = Lon,Lat=Lat,Center=Center

  self-> Get,PixId = PixId, Wid=Wid
    ; Have to assure that the current plotting transformation IS
    ; the mapping transformation
  Wset, PixId ; Set the Pixmap
    ; Set the Map coordinate transformation
  Map_Set,center[1], center[0], limit=[Lat[0],Lon[0],Lat[1],Lon[1]]
    ; Copy the data back to the Pixmap
  self-> CopyToPixMap
    ; Set Window ID back
  Wset, Wid
    ; if Config widget is open, send a
    ; message to it to redo the list of map dimensions.
  CASE Button OF 
    'LEFT': BEGIN 
      xs =  event.x
      ys =  event.y
      self->Set,XStatic = event.x,YStatic=event.y
      Widget_Control, Event.id, /Draw_Motion_Events, $
         Event_pro='pv_motion_events'

    END
    'MIDDLE': BEGIN 
      xy = Convert_Coord( event.x, event.y, /Device, /To_Data )
        ; Get the XYLabel Id
      self->Get, XYLabId=XYLabId
      IF xy[0] LT 0 THEN xy[0] =  xy[0] + 360.
      val = 'X: ' + string( xy[0], form='(f7.3)') + $
       ' Y: ' + string( xy[1], form='(f7.3)') 
      Widget_Control,XYLabId, Set_Value=val
      
    END
    'RIGHT': BEGIN 
      ;ok = Dialog_Message('Right Button Selections are not
      ;implemented yet!')
      
    END
  ENDCASE 

;  CASE firstClick OF 
;    0: BEGIN 
;      print,'firstclick=0, set to 1'
;      firstClick =  1
;      xs =  event.x
;      ys =  event.y
;    END 
;    1: BEGIN 
;      IF event.clicks EQ 2 THEN BEGIN 
;        ; Selection Event, Turn Motion Events off.
;        Widget_Control, event.id, Draw_Motion_Events=0
;        ok = Dialog_Message( $
;          "Thanks for testing the double-click functionality!")
;        self->Get,StatusID = StatusId
;        Widget_Control, StatusID, Set_Value='Selection...'
;      ENDIF  ELSE BEGIN 
;        ; Movement Event, Turn Motion events on
;        print,' turning Motion Events on'
;        Widget_Control, Event.id, /Draw_Motion_Events, $
;          Event_pro='pv_motion_events'
;      ENDELSE 
;      firstClick = 0
;    END
;    ELSE:
;  END
;  self->Set,XStatic = xs,YStatic=ys,firstClick=firstClick
  Widget_Control, event.top, set_uval=self
END


;====================================================
;
; PV::MotionEvents
;
;====================================================
PRO Pv_Motion_Events, event
   
  ;print,'pv_motion_events'
  Widget_Control, event.top, get_uval=self
  self->Get,xsize = xsize,ysize=ysize,$
    pixId=PixId, XStatic=xs, YStatic=ys, wid=wid,CurrentDims=CurrentDims

  EventTypes =  [ 'DOWN','UP','MOTION','SCROLL']
  ThisEvent =  EventTypes(event.type)
  WSET, wid

  IF ThisEvent eq 'UP' THEN BEGIN 
    ;print,'Up Event!'
    CurrentDims->Get,lon = lon,lat=lat
    Device, Set_Graphics_Function=3 ; copy source to destination
    Device, copy=[0,0,xsize, ysize, 0, 0, pixID ]
    Widget_Control, event.id, Draw_Motion_Events=0, $
     event_pro= 'pv_draw_events'
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
      Dims = self->GetCurrentDimensions()
      Dims-> Get,Lon = oldLonrange, Lat=OldLatRange
      limits2 = [ OldLonRange[0], OldLatRange[0],$
                  OldLonRange[1], OldLatRange[1]]
      NewDims = Obj_New('MapDims',xx,yy,/fix )
      self-> Set,Dimensions = NewDims
      str =  'new dimensions: lon ' + $
       string( xx, form='(2(f7.3,:,","))') + ' lat ' + $
       string( yy, form='(2(f7.3,:,","))')
      Message,str,/cont
        ; Draw the new area.
      IF NOT CompletelyWithin( limits1, limits2 ) THEN $
        self->ResetPlotObjects,/All ELSE $
        self->ResetPlotObjects,/AlreadyPlotted
      
      self->Draw
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

  self->Set, XDynamic = XD, YDynamic=YD
  self->Get, XYLabId=XYLabId

  val = 'X: ' + string( Coords(0), form='(f7.3)') + $
        'Y: ' + string( Coords(1), form='(f7.3)') 
  Widget_Control,XYLabId, Set_Value=val

  DEVICE,set_graphics_funct=6 ; xor the source and destination


  PlotS, [xs, xs, xd, xd, xs], $
         [ys, yd, yd, ys, ys], $
            /Device, Color=!d.n_colors-1

  DEVICE, set_graphics_funct=3
  Widget_Control, event.top, set_uval=self
END



;====================================================
;
; Quit Event
;
;====================================================

PRO pv::quit,id
   Widget_Control, id,/Destroy
   Obj_Destroy, self
END


;====================================================
;
; Files->Read  event
;
;====================================================

PRO Pv::WidgetRead
  files = Mpickfile( path=self.InputPath, filter=self.InputFilter, $
                     Get_Path=NewPath, Get_Filter=NewFilter)
  cnt = n_elements( files )
  IF cnt EQ 1 AND strlen(files(0)) EQ 0 THEN cnt = 0
  self.InputPath = DeEnvVar(NewPath)
  self.InputFilter = NewFilter
  IF cnt NE 0 THEN BEGIN 
    s = self->Read(files)
  ENDIF 
END


;====================================================
;
; Files->Write  event
;
;====================================================

PRO Pv::WidgetWrite
  ; ok = Dialog_Message("Not Currently Implemented -- Sorry!")
;    print,'Pv::WidgetWrite'
  Catch, Error
  IF Error NE 0 THEN BEGIN 
    ok = Dialog_Message(!Error_State.msg)
    Message,!error_state.msg,/cont
    IF exist( SaveSensitivity) THEN $
       self-> set, Sensitivity = saveSensitivity ELSE $
       self-> set, Sensitivity = 1
    return 
  ENDIF 
  saveSensitivity = self.Sensitivity
  self-> ChangeSensitivity,/Off
  Message,'Preparing to Write data ... ',/info
  ;Widget_Control, self.StatusId, Set_Value='Writing...'
  self->WriteToStatusBar, 'Writing ... '

  file = Dialog_Pickfile( filter=self.OutputPath,Path=self.OutputPath, $
                          Get_path=OutputPath,/Write)
  IF strlen(file[0]) NE 0 THEN BEGIN 
    self.OutputPath =  OutputPath
    OpenW, Lun, file, /get_lun, error=err 
    IF Err EQ 0 THEN BEGIN 
      CurrentDimNode =  self.DimsList->GetCurrent()
      Dim = *CurrentDimNode
      Dim->Get,lon = lon,lat=lat,center=center
      limit = [  lon[0], lat[0], lon[1], lat[1]  ]

      ;junk = self.DataList->GetHead()
      CurrentPlotDataPtr =  self.DataList->GetHead()
      ; CurrentPlotDataPtr = self.datalist->GetCurrentDataPtr()
      IF Ptr_Valid( CurrentPlotDataPtr ) THEN BEGIN 
        WHILE Ptr_Valid(CurrentPLotDataPtr) DO BEGIN 

          po =  *(CurrentPlotDataPtr)
          po-> get, Data = q2b, SelectedOnly=SelectedOnly
          s = q2b-> Get(Filename=filename)
          junk = rstrpos(filename,'/')+1
          basename = strmid( filename, junk, strlen(filename)-junk)
          self->WriteToStatusBar, "Writing (Extracting from " + basename + ")"
;          Widget_Control, self.StatusId, $
;            Set_Value="Writing (Extracting from " + filename + ")"
          s = q2b-> Set( Decimate    = self.Decimate_by, $
                         CRDecimate  = self.CRDecimate_by, $
                         ExcludeCols = self.ExcludeCols )
          PlotAmbiguities =  where( self.ambiguities NE 0, nPlots)
            ; self.ambiguities[0]=1 => plot 'selected' ambiguities.
            ; self.ambiguities[1]=1 => plot 1st ambiguity ... etc.
          IF SelectedOnly THEN BEGIN 
            status = q2b-> getplotdata(u,v,lon,lat,limit=limit,/Silent)
            IF status THEN BEGIN 
              dims = size(U,/Dimension)
              WriteU, Lun, dims[1],dims[2], 0L
              WriteU, Lun, u,v,lon,lat
              Message,'Writing file to' + file,/info
              Message,'File Dims are: ' + $
                  string(dims[1:2], form='(i2,1x,i5)' ) + ' Ambig: Selected ',/info
              self-> WriteToStatusBar,"Data Written to " + file
            ENDIF 
          ENDIF ELSE BEGIN 
            FOR p=0,nPlots-1 DO BEGIN 
                ; GetPlotData likes limit as [lon0, lat0, lon1,lat1 ]
              t1 = systime(1)
              status = q2b->GetPLotData(u,v,lon,lat, PlotAmbiguities[p], $
                                        limit = limit, /Silent  )
  ;            print,'Time to Extract: ' , systime(1)-t1

              IF status THEN BEGIN 
                s = size(U)
                pp = PlotAmbiguities[p]
                WriteU, Lun, s[1],s[2], PlotAmbiguities[p]
                WriteU, Lun, u,v,lon,lat
                Message,'File Dims are: ' + $
                  string( s[1:2], form='(i2,1x,i5)') + ' Ambig: ' + strtrim(pp,2),/cont

              ENDIF 
            ENDFOR 
          ENDELSE 
          ;CurrentDataNode =  self.DataList->GetNext()
          ;CurrentPlotDataPtr = self.datalist->GetCurrentDataPtr()
          CurrentPlotDataPtr = self.DataList->GetNext() 
        ENDWHILE 
      ENDIF ELSE Message," There's No Data!",/cont
      Free_Lun, lun
    ENDIF ELSE Message,!err_string,/cont
     
  ENDIF 

  self-> ChangeSensitivity, On =  SaveSensitivity
  ; Widget_Control, self.StatusId, Set_Value='Done Writing!'
  self->WriteToStatusBar, 'Done Writing '
  junk = self.DataList->GetHead()
END
;====================================================
;
; Config Menu Events
;
;====================================================


PRO pv::WidgetConfig
  ok = Dialog_Message("Not Currently Implemented -- Sorry!")
;  print,'pv_config_events'
END


;====================================================
;
; Make HardCopies
;
;====================================================


PRO pv::Hardcopy
   ; ok = Dialog_Message("Not Currently Implemented -- Sorry!")
   pv_hardcopy, parent=self.tlb, Cancel=cancel
   IF NOT cancel THEN BEGIN 
     fullFilename = self.OutputPath+self.HCFile + '.' + strlowcase(self.HCType)
     self-> ChangeSensitivity,/OFF
     CASE strupcase(self.HcType) OF 
       'GIF' : BEGIN 
         set_plot,'x'
         wset, self.wid
         im = tvrd()
         tvlct,red,green,blue,/get
         Write_Gif,fullFilename,im,red,green,blue
       END 
       'JPEG': BEGIN 
         set_plot,'x'
         wset, self.wid
         im = tvrd()
         tvlct,red,green,blue,/get
         s = size(im)
         JPeg = bytarr( s[1], S[2], 3 )
         Jpeg[*,*,0] =  red(im)
         Jpeg[*,*,1] =  green(im)
         Jpeg[*,*,2] =  blue(im)
         Write_Jpeg,fullFilename,jpeg, true=3
       END 
       'PICT': BEGIN 
         set_plot,'x'
         wset, self.wid
         im = tvrd()
         tvlct,red,green,blue,/get
         Write_Pict,fullFilename,im,red,green,blue
       END 
       'PS' : BEGIN 
        set_plot,'ps'
        self.PsInfo-> Get,ps = ps
        device,_extra=ps
        self.ColorBar->Calc
        self-> draw,/force
        device,/close
        set_plot,'x'
        self.ColorBar->Calc
       END 
       ELSE: message,'Unknown Hardcopy Type!!',/cont
     ENDCASE 
     self-> ChangeSensitivity,/ON
  ENDIF 
;   print,'pv_hardcopy_events'
END




;====================================================
;
; Make Overlays
;
;====================================================


PRO pv::Overlay
  ok = Dialog_message( "Not Implemented, yet")
END



;====================================================
; Overplot Data
;====================================================
PRO pv::Overplot
  pv_oplot, GROUP=self.tlb
END



;====================================================
; Propagate
;====================================================
PRO pv::Propagate
  pv_propagate, self.tlb
END



;====================================================
;
; Plot Options
;
;====================================================


PRO pv::PlotOptions
     ; self-> ChangeSensitivity, /off
     ; self.plotcfg_widget_open = 1
   PV_CONFIG,group=self.tlb
     ; self.plotcfg_widget_open = 0
     ; self-> ChangeSensitivity, /on
     ; print,'pv_plotopt_events'
END


;====================================================
;
; Resize Events
;
;====================================================


PRO Pv::Resize, x,y
  Catch, error
  IF error NE 0 THEN BEGIN 
    ok =  Dialog_Message( !error_state.msg )
    Message,!error_state.msg,/cont
    self-> set, Sensitivity = 1
    return
  ENDIF 

  self-> ResetPlotObjects,/AlreadyPlotted
  y = y-40
  Widget_Control, self.tlb, update=0
  Widget_Control, self.DrawId, draw_Xsize=x, Draw_Ysize=y
  Widget_Control, self.tlb, /update
  wset, self.wid
  WDelete, self.pixId
  self.xsize = x
  self.ysize = y
  Window,/Free,/Pixmap, XSize=x, YSize=y
  self.ColorBar->Calc
  self-> Draw
  ;self.pixId = !d.Window
  ;self-> CopyToPixmap
END


;====================================================
;
; Pro Pv::CopyToPixmap. Copies the contents of the current draw window
; to a pixmap.
;
;====================================================
PRO Pv::CopytoPixmap
  IF !d.Name NE 'PS' THEN BEGIN 
    WSet, self.pixID
    Device, Copy=[0, 0, self.xsize, self.ysize, 0, 0, self.wid]
    Wset, self.wid
  ENDIF 
END



;====================================================
;
; Create_help_msg: Creates the Help message.
;
;====================================================

PRO Pv::SelfHelp
    ; Create the help array 
  IF NOT Ptr_Valid( self.help ) THEN BEGIN 
    help_array =  strarr(500) &  i=0
    help_array[i] =  ' ---------- Help for PlotVector Object  --------------'
    i = i+1 &  help_array[i] =  '' 
    i = i+1 &  help_array[i] =  ' This Object has the following data members' 
    i = i+1 &  help_array[i] =  ' Data member may be Set using "obj->Set,item=item" if ' 
    i = i+1 &  help_array[i] =  ' "(Set..." is included in description. '
    i = i+1 &  help_array[i] =  ' Data Member may be retrieved using "obj->Get,item=item" if '
    i = i+1 &  help_array[i] =  '  "(Get..." is included in description.'
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  ' Xsize - X Size of display in pixels (Get/Set)'
    i = i+1 &  help_array[i] =  '    Default = 640. This quantity can also be set '
    i = i+1 &  help_array[i] =  '    by "dragging" the corner of the window. The '
    i = i+1 &  help_array[i] =  '    widget will resize itself accordingly '
    i = i+1 &  help_array[i] =  ' Ysize - Y Size of display in pixels (Get/Set)'
    i = i+1 &  help_array[i] =  '    Default = 480. May be set by dragging window '
    i = i+1 &  help_array[i] =  '    corner, as with Xsize '
    i = i+1 &  help_array[i] =  ' Ambiguities - The Ambiguities to plot (Get/Set)'
    i = i+1 &  help_array[i] =  '    This is an array, where ...'
    i = i+1 &  help_array[i] =  '     Ambiguities[0] =1 means plot the "selected" ambiguitiy '
    i = i+1 &  help_array[i] =  '     Ambiguities[1] = 1 means plot the first ambiguitiy '
    i = i+1 &  help_array[i] =  '    ... Etc. '
    i = i+1 &  help_array[i] =  '    default = Ambiguities[0]=1 (the Selected Ambiguity) '
    i = i+1 &  help_array[i] =  '  Ncolors -  Number of colors in the color table '
    i = i+1 &  help_array[i] =  '     Currently equals 94, or which 87 are for mapping '
    i = i+1 &  help_array[i] =  '       speed into color table and the last 6 are solid '
    i = i+1 &  help_array[i] =  '        colors for plotting the individual ambiguities '
    i = i+1 &  help_array[i] =  '          not modifiable. '
    i = i+1 &  help_array[i] =  '  Length - the "Length" of the vectors (Get/Set)'
    i = i+1 &  help_array[i] =  '     Default = 2.0 '
    i = i+1 &  help_array[i] =  '  Thickness - The "Thickness" of the vectors (Get/Set)'
    i = i+1 &  help_array[i] =  '     Default=1.0 '
    i = i+1 &  help_array[i] =  '  Decimate_by  - Take Every "n-th" Vector (Get/Set)'
    i = i+1 &  help_array[i] =  '     2 means take every other vector '
    i = i+1 &  help_array[i] =  '     Default = 2.0 '
    i = i+1 &  help_array[i] =  '  Min_Speed - If color=-1, the minimum speed to plot (Get/Set) '
    i = i+1 &  help_array[i] =  '     Default = 2'
    i = i+1 &  help_array[i] =  '  Max_Speed - - If color=-1, the maximum speed to plot (Get/Set) '
    i = i+1 &  help_array[i] =  '     Default = 37.0'
    i = i+1 &  help_array[i] =  '  Ambig_Colors - 5 element array to store the colors used '
    i = i+1 &  help_array[i] =  '     to plot the individual ambiguities'
    i = i+1 &  help_array[i] =  '      Not Modifiable through Get/Set'
    i = i+1 &  help_array[i] =  '  DataList - A linked list containing the Data to be '
    i = i+1 &  help_array[i] =  '     plotted (Get/Set) '
    i = i+1 &  help_array[i] =  '     Nominally, the data member would be modified through the '
    i = i+1 &  help_array[i] =  '     widget. Provision is made to modify it through the ' 
    i = i+1 &  help_array[i] =  '     "Set" object method call. '
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  '     This is considered and "Expert" option and should '
    i = i+1 &  help_array[i] =  '     be used with due caution!'
    i = i+1 &  help_array[i] =  '  '
    i = i+1 &  help_array[i] =  ' Example of setting color, ambiguities and length '
    i = i+1 &  help_array[i] =  '   IDL> pv_obj->Set, color=-1, ambguity=1, length=1.5 '
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  ' ---------- Initialization ----------------- '
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  ' The "constructor" may be called with a filename, a structure'
    i = i+1 &  help_array[i] =  '   of type Q2B containing the data or a linkedlist of structures'
    i = i+1 &  help_array[i] =  '   of type Q2B containing the data '
    i = i+1 &  help_array[i] =  '   For Example: '
    i = i+1 &  help_array[i] =  '     IDL> PV_OBJ = Obj_New("pv" '
    i = i+1 &  help_array[i] =  '         [, filename=filename | ,data=data ] ) '
    i = i+1 &  help_array[i] =  ' -------------- Methods ----------------------- '
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  ' Get/Set - already covered '
    i = i+1 &  help_array[i] =  ' Help - Prints out this method '
    i = i+1 &  help_array[i] =  ' whatever else I come up with' 
    help_array = help_array[0:i]
    self.help =  ptr_new(help_array) &  help_array=0
  ENDIF 
  SaveDevice = !d.Name
  set_plot,'X'
  XDisplayFile,'',*self.help
  Set_plot,  SaveDevice

END
;====================================================
;
;  Init: Initializes the PV object
;
;====================================================

FUNCTION pv::Init, $
           files         = files, $
           data          = data, $ 
           xsize         = xsize, $
           ysize         = ysize, $
           color         = color, $
           ambiguities   = ambiguities, $
           decimate_by   = decimate_by,$
           CRDecimate_by = CRDecimate_by,$
           ExcludeCols   = ExcludeCols,$
           InputPath     = InputPath ,$     
           InputFilter   = InputFilter,$    
           OutputPath    = OutputPath,$     
           HCType        = HCType, $    
           HCFile        = HCFile,$
           LonRange      = LonRange, $      
           LatRange      = LatRange ,$      
           Length        = Length, $        
           Thickness     = Thickness,$      
           MinSpeed      = MinSpeed,$       
           MaxSpeed      = MaxSpeed,$       
           Help          = Help, $
           Verbose       = Verbose, $
           doColorBar    = doColorBar, $
           doAnnotations = doAnnotations
   


  Catch, Error
  IF Error NE 0 THEN BEGIN 
    ok = Dialog_Message(!Error_State.msg)
    Message,!error_state.msg,/cont
    self-> set, Sensitivity = 1
    return,0
  ENDIF 


  IF keyword_set( help ) THEN self-> SelfHelp


    ; Set the pseudo 8 bit class. It may not take, since the visual
    ; class may already been set and a connection to the X-server may
    ; already have been created, in which case, we'll find out when we
    ; get the visual name in the 2nd call.

  Device, pseudo=8
  device,get_visual_name= visual_name
  self.visual = strupcase(visual_name)
  

  self.Sensitivity = 1 ; Make sure we can operate on the widget.

  self.MinMaxSpeed = [ 1.e10, -1.e10 ] 

  self.Verbose = keyword_set( Verbose )

  self.doAnnotations =  keyword_set(doAnnotations)
  self.doColorBar =  keyword_set(doColorBar)
  IF N_Elements(xsize)       EQ 0 THEN xsize = 640 
  IF N_Elements(ysize)       EQ 0 THEN ysize = 480 
  
  n_ambigs = N_Elements(self.ambiguities)
  IF N_Elements(ambiguities) EQ 0 THEN BEGIN 
    ambiguities = lonarr(n_ambigs)
    ambiguities[0] = 1
   ENDIF ELSE BEGIN 
     IF N_Elements(ambiguities) EQ 1 THEN BEGIN 
       self.ambiguities[ambiguities < (n_ambigs-1) ] =  1
     ENDIF ELSE $
       IF N_Elements(ambiguities) EQ n_ambigs THEN $
         self.ambiguities = ambiguities $
       ELSE BEGIN 
         Message,"Ambiguities must be either 1 or " + $
            strtrim( n_ambigs,2) + " elements, if anything",/cont
         Message,"Using 'Selected' Ambiguity",/cont
         self.ambiguities[0] = 1
       ENDELSE 
  ENDELSE 
  IF N_Elements(decimate_by) EQ 0 THEN decimate_by = 5
  IF N_Elements(CRDecimate_by) NE 2 THEN CRDecimate_by = [0,0] 
  IF N_Elements(ExcludeCols) EQ 0 THEN ExcludeCols = '' ELSE BEGIN 
    IF VarType(ExcludeCols) NE 'STRING' THEN BEGIN 
      Message,'ExcludeCols must be of type STRING',/cont
      ExcludeCols = '' 
    ENDIF 
  ENDELSE 
  IF N_Elements(InputPath)   EQ 0 THEN BEGIN 
    path = getenv('VAP_WINDS') 
    IF path NE '' THEN InputPath = path ELSE InputPath ='./' 
  ENDIF ELSE BEGIN 
    IF VarType(InputPath) NE 'STRING' THEN BEGIN 
      Message,'InputPath must be of type STRING',/cont
      InputPath = './' 
    ENDIF 
  ENDELSE 
  IF N_Elements(InputFilter) EQ 0 THEN InputFilter = 'QS*S*E* *.hdf' ELSE BEGIN 
    IF VarType(InputFilter) NE 'STRING' THEN BEGIN 
      Message,'InputFilter must be of type STRING',/cont
      InputFilter = 'QS*S*E* *.hdf' 
    ENDIF 
  ENDELSE 
  IF N_Elements(OutputPath)  EQ 0 THEN OutputPath = './' ELSE BEGIN 
    IF VarType(OutputPath) NE 'STRING' THEN BEGIN 
      Message,'OutputPath must be of type STRING',/cont
      OutputPath = './' 
    ENDIF 
    IF rstrpos( OutputPath,'/') LT strlen(OutputPath)-1 THEN $
      OutputPath = OutputPath + '/'
  ENDELSE 
  IF N_Elements(OutputFilter) EQ 0 THEN OutputFilter = '*.dat' ELSE BEGIN 
    IF VarType(OutputFilter) NE 'STRING' THEN BEGIN 
      Message,'OutputFilter must be of type STRING',/cont
      OutputFilter = '*.dat'
    ENDIF 
  ENDELSE 
  IF N_Elements(HCType)  EQ 0 THEN HCType = 'ps' ELSE BEGIN 
    IF VarType(HCType) NE 'STRING' THEN BEGIN 
      Message,'HCType must be of type STRING',/cont
      HCType = 'ps'
    ENDIF 
  ENDELSE 

  IF N_Elements(HCFile)  EQ 0 THEN $
    HCFile = 'pv_out' +self.HCType ELSE BEGIN 
      IF VarType(HCFile) NE 'STRING' THEN BEGIN 
        Message,'HCFile must be of type STRING',/cont
        HCFile = 'pv_out'+self.HCType
      ENDIF 
  ENDELSE 

  junk = '.' + self.HCType &  junklen=strlen(junk)
  IF strpos( self.HCFile, junk ) EQ -1 THEN $
   self.HCFile = self.HCFile+'.' + self.HCType

  IF N_Elements(LonRange)   NE 2 THEN LonRange = [0.,359] ELSE BEGIN 
    IF VarType(LonRange) NE 'FLOAT' THEN LonRange = float(LonRange)  
  ENDELSE 
  IF N_Elements(LatRange)    EQ 0 THEN LatRange = [-90,90.] ELSE BEGIN 
    IF VarType(LatRange) NE 'FLOAT' THEN LatRange = float(LatRange)  
  ENDELSE 

  IF N_Elements(Length)      EQ 0 THEN Length = 1
  IF N_Elements(Thickness)   EQ 0 THEN Thickness = 1
  IF N_Elements(MinSpeed)   EQ 0 THEN MinSpeed = 2
  IF N_Elements(MaxSpeed)   EQ 0 THEN MaxSpeed =  37

  self.xsize         = xsize      
  self.ysize         = ysize      
  self.ExcludeCols   = ExcludeCols
  self.ambiguities   = ambiguities 
  self.InputPath     = DeEnvVar(InputPath)
  self.InputFilter   = InputFilter   
  self.OutputPath    = OutputPath  
  self.HCType        = HCType  
  self.HCFile        = HCFile  
  self.Length        = Length      
  self.Thickness     = Thickness   
  self.MinSpeed      = MinSpeed    
  self.MaxSpeed      = MaxSpeed    
  self.decimate_by   = decimate_by
  self.CRdecimate_by = CRdecimate_by

  junk = where(CRDecimate_by, njj )
  self.Decimate_flag = (njj NE 0)

;   ColorMapping =  { Name: ' ', $
;                      ColorTriple: bytarr(3),$
;                      ColorIndex : 0l }

     ; These are the possible plotting colors available in the
     ; colortable file loaded (pv.ct). If that file is changed, these
     ; will have to change also. I have yet to come up with a general
     ; way of doing this.
   colors = replicate({ColorMapping},8)

   colors[0].Name = 'Speed'
   colors[0].colorTriple =  [-1b, -1,  -1  ]
   colors[0].ColorIndex = -1 

   colors[1].Name = 'Red'
   colors[1].ColorTriple =   [255b, 0,   0  ]

   colors[2].Name = 'Green'
   colors[2].ColorTriple =   [0b,   255, 0  ]

   colors[3].Name = 'Blue'
   colors[3].ColorTriple =    [0b,   0,   255]

   colors[4].Name        = 'Yellow'
   colors[4].ColorTriple =  [255b, 255, 0  ]

   colors[5].Name        = 'Magenta'
   colors[5].ColorTriple = [255b, 0,   255]

   colors[6].Name         = 'Aqua'
   colors[6].ColorTriple  = [0b,   255, 255]

   colors[7].Name         = 'White'
   colors[7].ColorTriple  =   [255b, 255, 255]


   self.Water0    = 1         ; Start of Water indices
   self.Land0     = 7         ; Start of Land Colors in Color table
   self.Cloud0    = 27        ; Start of Cloud Indicies
   self.Wind0     = 47        ; Start of Wind Indicies
   self.Ambig0    = 92        ; Start of Ambiguity Color Indicies
                              ; (Red is the top of the Wind colors, so
                              ; there is some overlap)
   self.NWind     = 45        ; Number of Wind Colors.

  FOR i=1,7 DO colors[i].ColorIndex = self.Ambig0+i-1

  self.AmbigColorMapping = colors

    ; These are the ACTUAL colors we will use.
  self.ambigColorNames = [ 'Speed','Red','Green','Blue','Yellow','Magenta']

    ; find the indices from the ColorMapping array of structures.
  FOR i=0,N_Elements(self.AmbigColorNames)-1 DO $
      self.AmbigColors[i] =  self->GetColorIndex( self.AmbigColorNames[i] )


  dims                 = Obj_New('MapDims',LonRange,LatRange,/fix )
    ; Linked List of Plot Dimensions
  self.DimsList        = Obj_New('linkedlist',dims) 
    ; Linked List of Data Objects (roughly of 'files')
  self.DataList        = Obj_New('linkedlist')      
    ; 'Speed' Histogram object
  self.SpeedHisto      = Obj_New('ObHisto', nBins=200, Min=0, Max=37)  


    ; Get Pointer to color table.
  CT = GetEnv('PV_COLORTABLE')
  IF strlen(CT) NE 0 THEN $
    PtrToColorTable = ReadColorTable(CT[0]) $
  ELSE $
    PtrToColorTable = ReadColorTable($
                     '/usr/people/vapuser/Qscat/Resources/Color_Tables/pv.ct.2')
;  PtrToColorTable = ReadColorTable($
;                     '/home/daffer/idl5/OO/pv.ct')
  self.PtrToCt = PtrToColorTable
  CT =  *PtrToColorTable
  self.Ncolors = N_elements( CT[0,*] )
  self.PsInfo = Obj_New('PSFORM') ;
  self.Annotation = Obj_new('ANNOTATION')
  self.ColorBar =  Obj_New('ColorBar', $
                       table=ct,ncolors=self.nwind,title='Wind Speed (m/s)',$
                            bottom=self.wind0,$
                            true= (self.visual NE 'PSEUDOCOLOR'), $
                           min=self.minspeed, max=self.maxspeed, $
                            divisions=4,position=[0.25,0.93,0.75,0.95], $
                           format='(f5.0)', color=n_elements(ct[0,*]) )

  self.Oplot =  obj_new('ObPlot',psym=2,/plots)

  status = 1
  IF n_elements( files ) NE 0 THEN BEGIN 
    status = self-> Read( files )
  ENDIF ELSE IF n_elements( data ) NE 0 THEN BEGIN 
    IF Obj_Valid( data ) THEN BEGIN 
      status = self.DataList-> Append(data)
    ENDIF ELSE BEGIN 
      Structure_Name = STRUPCASE(Tag_Names( data, /structure_name) )
      CASE Structure_Name OF 
        'Q2BDATA'    : q = Obj_New('Q2B'   , data=data)
        'RQ2BDATA'   : q = Obj_New('Q2B'   , data=data)
        'QMODELDATA' : q = Obj_New('QMODEL', data=data)
        ELSE     : BEGIN 
          Message,"Can't identify data type ",/cont        
          return,0
        END
      ENDCASE 
       p =  Obj_new('PvPlotObject',q)
      status = self.DataList-> Append(p)
    ENDELSE 
  ENDIF 

  status =  status AND $
            Ptr_Valid(PtrToColorTable) AND $
            Obj_Valid(Self.Annotation) AND $
            Obj_Valid( self.PsInfo)


  IF status THEN BEGIN
    self->Draw
  ENDIF 
  RETURN,status
END

;====================================================
;
; CreateWidget - Creates a new widget window
;
;====================================================


FUNCTION Pv::CreateWidget

  Catch, Error
  IF Error NE 0 THEN BEGIN 
    ok = Dialog_Message(!Error_State.msg)
    Message,!error_state.msg,/cont
    return,0
  ENDIF 

  IF NOT Widget_Info( self.tlb, /Valid) THEN BEGIN 
     ; Reset all the plotObjects
    self-> ResetPlotObjects,/all
     ; Create the widget hierarchy.
    set_plot,'x'
    self.tlb = Widget_Base( mbar=MenuID, $
                            /tlb_size_events, col=1, $
                          Title='Plot Vectors')
    self.MenuId =  MenuId
    self.FileId =  Widget_Button( MenuID, Value='File',/menu)
    self.ReadId =  Widget_Button( self.FileID, Value='Read', $
                                  UValue='READ') 
    self.WriteId =  Widget_Button( self.FileID, Value='Write', $
                                  UValue='WRITE')
    self.QuitId =  Widget_Button( self.FileID, Value='Quit',UValue='QUIT')
    junk  =  Widget_Button( MenuID, Value='Config',/Menu, $
                                    UValue='CONFIG')
    self.ConfigId =   Widget_Button( junk, Value='Plotting Options', $
                                  UValue='PLOTOPTIONS')
    junk =  Widget_Button( MenuId, Value='HardCopy',/Menu, $
                                    UValue='HARDCOPY')
    self.HardCopyId =  Widget_Button( junk, Value='Configure Hard Copy', $
                                      UValue='CONFIGHARDCOPY')
;    junk = Widget_Button( MenuId, Value='Overlay',/Menu,Uvalue='OVERLAY')
;    self.OverlayId = Widget_Button( junk,Value='Cloud Overlay', $
;                                    Uvalue='CLOUDOVERLAY')

    junk = Widget_Button( MenuId, Value='OverPlot',/Menu,Uvalue='OVERPLOT')
    self.OverPlotId = Widget_Button( junk,Value='Plot Locations', $
                                    Uvalue='OVERPLOTLOC')

    junk = Widget_Button( MenuId, Value='Propagate',/Menu,Uvalue='PROPAGATE')
    self.PropagateId = Widget_Button( junk,Value='Propagate Orbits', $
                                    Uvalue='PROPAGATEORBITS')

    ; put a MENU below the menubar one, just in case  
    junk = Widget_Base(self.tlb,/row)
    self.MiscId =  Widget_Button( junk, Value='Misc',Uvalue='MISC',$
                                  /menu)
    self.SetSensId = Widget_Button( self.MiscId, Value='Reset Sensitivity',$
                                    Uvalue='RESETSENS')
    self.RedrawId = Widget_Button( self.MiscId, Value='Redraw', $
                                   Uvalue='REDRAW' )

    self.Quit2Id = Widget_Button( self.MiscId, Value='Quit', $
                                   Uvalue='QUIT2' )

    IF self.visual EQ 'PSEUDOCOLOR' THEN BEGIN 
      self.DrawId =  Widget_Draw( self.tlb, scr_xsize=self.xsize, $
                                  scr_ysize=self.ysize, $
                                  /button_events, retain=2, $ 
                                  colors=self.Ncolors,$
                                  event_pro='pv_draw_events')
    ENDIF ELSE BEGIN
      self.DrawId =  Widget_Draw( self.tlb, scr_xsize=self.xsize, $
                                  scr_ysize=self.ysize, $
                                  /button_events, retain=2, $ 
                                  event_pro='pv_draw_events')
    ENDELSE 
    junk = Widget_Base( self.tlb, col=1 )
  ;  self.StatusId = Widget_Label( junk,Scr_XSize=self.xsize/2-4,$
  ;                                   Scr_YSize=20, Frame=2, $
  ;                                /Dynamic_Resize,/Align_Left)
  ;  self.xyLabId =  Widget_Label( junk,Scr_XSize=self.xsize/2-2,$
  ;                                Scr_YSize=20, Frame=2, /Dynamic_Resize, $
  ;                                 Value='X:      ,Y:    ',/Align_Left );, $
  ;                                  Font="helvetica-medium-o-normal--10")
    self.StatusId = Widget_Label( junk, Scr_XSize=self.xsize,$
                                     Scr_YSize=20, Frame=2, $
                                  /Dynamic_Resize,/Align_Left)
    self.xyLabId =  Widget_Label( junk, Scr_XSize=self.xsize,$
                                  Scr_YSize=20, Frame=2, /Dynamic_Resize, $
                                   Value='X:      ,Y:    ',/Align_Left );, $
  ;                                  Font="helvetica-medium-o-normal--10")


    Widget_Control,self.tlb,/Realize
    Widget_Control, self.DrawId, get_value=wid
    self.wid = wid
    IF self.visual EQ 'PSEUDOCOLOR' THEN BEGIN 
      Window,/Free,/Pixmap,colors=self.Ncolors,$
        XSize=self.XSize, YSize=self.YSize
    ENDIF ELSE BEGIN 
      Window,/Free,/Pixmap,colors=self.Ncolors,$
        XSize=self.XSize, YSize=self.YSize
    ENDELSE 
    self.PixId = !d.Window
    Wset, wid
    IF self.visual EQ 'PSEUDOCOLOR' THEN $
      TvLct, transpose(*self.PtrToCT) ELSE loadct,0
    Widget_Control, self.tlb, set_uval=self
      ; Call Xmanager to Manage the Widget.
    XManager, 'PV', self.TLB, event_handler='pv_events', /no_block
  ENDIF 
  return,1
END

;====================================================
;
; Draw: Draw the contents of the datalist
;
;====================================================


PRO Pv::Draw, $
      NoErase = NoErase, $
      AlreadyPlotted=AlreadyPlotted, $
      Force=Force, $
      NO_OPLOT=NO_OPLOT

  COMMON PrevDims, oldLimit
  no_oplot = keyword_set(no_oplot)



  Catch, Error
  IF Error NE 0 THEN BEGIN 
    ok = Dialog_Message(!error_State.Msg)
    Message,!error_State.Msg,/cont
    self-> set, Sensitivity = 1
    return
  ENDIF 

  force = keyword_set(Force)
  IF keyword_set(AlreadyPlotted) THEN self->ResetPlotObjects,/AlreadyPlotted
  IF force THEN self-> ResetPlotObjects,/AlreadyPlotted
  Erase = keyword_set( NoErase) NE 1

  IF NOT exist(CT) THEN CT = *self.PtrToCt
  status = 1
  nTotPlots = 0
  noData = -2 ; A particular failure mode from q2b->getplotdata
  DontKnow = -1 ; Setting for PvPlotObject.InRegion

  IF NOT Widget_Info( self.Tlb,/Valid ) THEN BEGIN 
    status =  self-> CreateWidget()
    force = 1
  ENDIF 

  postscriptDevice =  !d.name EQ 'PS'
  IF NOT PostscriptDevice THEN Wset, self.wid

  IF status THEN BEGIN 
    Widget_Control,/HourGlass
    SaveSensitivity = self.Sensitivity
    self-> ChangeSensitivity, /OFF
    ; Widget_Control, self.StatusId, Set_Value='Drawing...'
    self->WriteToStatusBar,' Drawing ... '

    ;CurrentDimNode =  *(self.DimsList->GetCurrent())
    ;Dim = *CurrentDimNode.data
    Dim = *(self.DimsList->GetCurrent())
    Dim->Get,lon = lon,lat=lat,center=center
    limit = [  lon[0], lat[0], lon[1], lat[1]  ]
    maplimit = [ lat[0], lon[0], lat[1], lon[1] ]

    IF NOT exist( oldLimit ) THEN oldLimit =  limit

    map_extent = abs(lon[1]-lon[0])*abs(lat[1]-lat[0])
    dots =  map_extent GE (140*110)
      ; Map_set likes limit as [lat0, lon0, lat1, lon1]

    limitChange = where( limit-oldLimit, nlimitChange )



    ;junk = self.DataList->GetHead()
    ;CurrentPlotDataPtr = self.datalist->GetCurrentDataPtr()
    CurrentPlotDataPtr = self.DataList->GetHead()
    IF Ptr_Valid( CurrentPlotDataPtr ) THEN BEGIN 
      first = 1
      WHILE Ptr_Valid(CurrentPLotDataPtr) DO BEGIN 


        po = *(CurrentPlotDataPtr)
        po-> Get,Data=Q2b, $
                 SelectedOnly=SelectedOnly, $
                 PlotFlag=PlotFlag, $
                 AlreadyPlotted=AlreadyPlotted, $
                 InRegion=InRegion
        PlotAmbiguities =  where( self.ambiguities NE 0, nPlots)
          ; self.ambiguities[0]=1 => plot 'selected' ambiguities.
          ; self.ambiguities[1]=1 => plot 1st ambiguity ... etc.

        IF PlotFlag AND NOT (AlreadyPlotted) AND $
           InRegion NE 0 THEN BEGIN 

          IF first THEN BEGIN 
            self-> CreateMap, Erase = Erase, Force=Force, $
                   center = center, limit=maplimit
            first = 0
          ENDIF 

          s = q2b-> Get( Filename=filename)
          junk = rstrpos(filename,'/')+1
          basename = strmid( filename, junk, strlen(filename)-junk)
          str = "Drawing (Extracting from " + basename + ")"
          self->WriteToStatusBar, str
          Message,str,/info
          s = q2b-> Set( Decimate    = self.Decimate_by, $
                         CRDecimate  = self.CRDecimate_by, $
                         ExcludeCols = self.ExcludeCols)
          IF SelectedOnly THEN BEGIN 
            t1 = systime(1)
            status = q2b-> GetPlotData(u,v,lon,lat,limit=limit,/Silent)
            t2 = systime(1)
            print,'  Time to extract ', t2-t1
            IF Status THEN BEGIN 
              self-> UpdateSpeedHisto,u,v,nTotPlots
              self->WriteToStatusBar,'Drawing...'
              IF self.visual NE 'PSEUDOCOLOR' AND PostscriptDevice THEN BEGIN 
                ; True color, but plotting to Postscript file!
                tvlct,r,g,b,/get
                tvlct,transpose(CT)
                PlotVect,u,v,lon,lat,$
                  Length = self.Length, $
                    thick=self.Thickness, $
                      Color=self.AmbigColors[PlotAmbiguities[p]],$
                        MinSpeed=self.MinSpeed, $
                         MaxSpeed=self.MaxSpeed, $
                           Start_index=self.Wind0,$
                            NColors=self.NWind, dots=dots
                tvlct,r,g,b
              ENDIF ELSE BEGIN 
                PlotVect,u,v,lon,lat,$
                  Length = self.Length, $
                    thick=self.Thickness, $
                      Color=self.AmbigColors[PlotAmbiguities[p]],$
                        MinSpeed=self.MinSpeed, $
                         MaxSpeed=self.MaxSpeed, $
                           Start_index=self.Wind0,$
                            NColors=self.NWind, dots=dots,$
                             truecolor=self.visual ne 'PSEUDOCOLOR' , $
                              table=CT
              ENDELSE 
              
              po->InRegion,1
              nTotPlots =  nTotPlot+1
            ENDIF ELSE IF status EQ NoData THEN BEGIN 
              po->InRegion,0
              GOTO, Set_AlreadyPlotted
            ENDIF 
          ENDIF ELSE BEGIN 
            FOR p=0,nPlots-1 DO BEGIN 
                ; GetPlotData likes limit as [lon0, lat0, lon1,lat1 ]
              t1 = systime(1)
              status = q2b->GetPLotData(u,v,lon,lat, PlotAmbiguities[p], $
                                        limit = limit, /Silent  )
              t2 = systime(1)
              print,'  Time to extract ', t2-t1
              IF status THEN BEGIN 
                self-> UpdateSpeedHisto,u,v,nTotPlots
                self->WriteToStatusBar,'Drawing...'
                  ; Plot the vectors
                t1 = systime(1)
                IF PostscriptDevice AND $
                   self.visual NE 'PSEUDOCOLOR' THEN BEGIN 
                  tvlct,r,g,b,/get
                  tvlct,transpose(CT)
                  PlotVect,u,v,lon,lat,$
                    Length = self.Length, $
                      thick=self.Thickness, $
                        Color=self.AmbigColors[PlotAmbiguities[p]],$
                          MinSpeed=self.MinSpeed, $
                           MaxSpeed=self.MaxSpeed, $
                             Start_index=self.Wind0,$
                              NColors=self.NWind, dots=dots
                  tvlct,r,g,b
                ENDIF ELSE BEGIN 
                  PlotVect,u,v,lon,lat,$
                    Length = self.Length, $
                      thick=self.Thickness, $
                        Color=self.AmbigColors[PlotAmbiguities[p]],$
                          MinSpeed=self.MinSpeed, $
                           MaxSpeed=self.MaxSpeed, $
                             Start_index=self.Wind0,$
                              NColors=self.NWind, dots=dots,$
                             truecolor=self.visual ne 'PSEUDOCOLOR' , $
                              table=CT
                ENDELSE 
                po->InRegion,1
                nTotPlots =  nTotPlots + 1
              ENDIF ELSE IF status EQ NoData THEN BEGIN 
                po->InRegion,0
                GOTO, Set_AlreadyPlotted
              ENDIF 
            ENDFOR 
          ENDELSE 
        ENDIF 
          ; Mark this node as already plotted.
        Set_AlreadyPlotted: 
        po-> SetAlreadyPlotted, 1
        ;CurrentDataNode =  self.DataList->GetNext()
        ;CurrentPlotDataPtr = self.datalist->GetCurrentDataPtr()
        CurrentPlotDataPtr =  self.DataList->GetNext()
      ENDWHILE 
        ; Copy new plot to pix map
      self->CopyToPixMap

    ENDIF ELSE BEGIN 
      Message," There's No Data!",/cont
      self-> CreateMap, Erase = Erase, Force=Force,$
           center = center, limit=maplimit
      xyouts, 0.5, 0.6, 'No Data!!', align=0.5, $
       Charsize=1.3,charthick=2,/normal
    ENDELSE 

      

    IF NOT no_oplot THEN self.oplot-> draw,/doAnnot
    self-> ChangeSensitivity, On =  SaveSensitivity
    ; Widget_Control, self.StatusId, Set_Value='Done Drawing!'
    self-> WriteToStatusBar, 'Done Drawing '
  ENDIF ELSE Message,"Can't Create Top Level Widget!",/cont

END

;========================================
;
; pv::ChangeSensitivity
;
;========================================
PRO Pv::ChangeSensitivity, On = On, Off=Off
  IF Keyword_set( On ) THEN BEGIN 
    IF Widget_Info( self.MenuId, /Valid) THEN $
      Widget_Control, self.MenuId, Sensitive=1
    IF Widget_Info( self.DrawId, /Valid) THEN $
      Widget_Control, self.DrawId, Draw_Button_Events=1
    self.sensitivity = 1
  ENDIF ELSE BEGIN 
    IF Widget_Info( self.MenuId, /Valid) THEN $
      Widget_Control, self.MenuId, Sensitive=0
    IF Widget_Info( self.DrawId, /Valid) THEN $
      Widget_Control, self.DrawId, Draw_Button_Events=0
    self.sensitivity = 0
  ENDELSE 
END


;========================================
;
; pv::Read
;
;========================================

FUNCTION Pv::Read,files
  IF Widget_Info( self.StatusId, /valid ) THEN BEGIN 
    ; Widget_Control, self.StatusId,Set_Value='Reading Data ... '
    self-> WriteToStatusBar,' Reading Data '
    Widget_Control,/HourGlass
  ENDIF 
  status = 0
  nf = N_Elements(files)
  IF nf NE 0 THEN BEGIN 
    nfiles_string = strtrim( nf, 2 )
    FOR f=0,nf-1 DO BEGIN 
      print,'Attempting to read ', files[f]
      IF Widget_Info( self.StatusId, /valid ) THEN BEGIN 
        junk = rstrpos(files[f],'/')+1
        basename = strmid( files[f], junk[0], strlen(files[f])-junk[0] )
        self-> WriteToStatusBar,'Reading file ' +  basename + $
         ' (' + strtrim(f+1,2) + ' of ' + nfiles_string + ')'
      ENDIF 
;         Widget_Control, self.StatusId,Set_Value='Reading File ' + $
;            files(f) + ' (' + strtrim(f+1,2) + ' of ' + nfiles_string + ')'

      IF Hdf_IsHdf( files(f) ) THEN BEGIN 
        ; It may be a q2b file or a model file.
        attr = hdfgetattr( files(f), attr='SHORTNAME' )
        IF VarType( attr ) EQ 'STRUCTURE' THEN BEGIN 
          CASE (*attr.value)[0] OF 
            'QSCATVAPMODEL': q = Obj_New('qmodel',filename=files(f) )
            'QSCATL2B'     : q = Obj_New('q2b',$
                                         filename=files(f),$
                                         verbose=self.verbose)
            ELSE           : BEGIN 
              Message,"Can't identify type of HDF file ",/cont
              print,'  Attribute "ShortName" must be either QSCATL2B or QSCATVAPMODEL'
              status = 0
            END
          ENDCASE 
          Ptr_Free, attr.value
        ENDIF ELSE BEGIN 
          Message,"Can't get ShortName attribute from HDF file",/cont
          status = 0
        ENDELSE 
      ENDIF ELSE BEGIN 
          ; q2b can read some non-HDF formats, too.
        q = Obj_New('q2b',filename=files(f), verbose=self.verbose)
      ENDELSE 
       
      IF Obj_Valid( q ) THEN BEGIN 
        s =  q-> Set(Decimate = self.decimate_by,$
                     CRDecimate = self.CRdecimate_by, $
                     ExcludeCols= self.ExcludeCols )
        s = q-> Get(filename=filename)
        po = Obj_New('PvPlotObject',q,file=filename)
        IF Obj_Valid( po ) THEN BEGIN 
          status =  self.DataList->append(po)
          n = self.DataList-> GetCount()
          IF n EQ 1 THEN $
            self->Draw ELSE $
            self->Draw,/NoErase
        ENDIF ELSE status = 0
      ENDIF ELSE status = 0

    ENDFOR 
  ENDIF 
  IF Widget_Info( self.StatusId, /valid ) THEN $
   self-> WriteToStatusBar, 'Done Reading '
;    Widget_Control, self.StatusId,Set_Value='Done Reading '

  RETURN,status
END ; End pv::Read

;====================================================
;
; Cleanup: Destroy's whatever needs to be destroyed.
;
;====================================================

PRO Pv::Cleanup
  IF Obj_Valid(self.DataList)        THEN Obj_Destroy, self.DataList 
  IF Obj_Valid(self.DimsList)        THEN Obj_Destroy, self.DimsList
  IF Obj_Valid(self.OutputHCFiles)   THEN Obj_Destroy, self.OutputHCFiles 
  IF Obj_Valid(self.OutputFiles)     THEN Obj_Destroy, self.OutputFiles 
  If Obj_Valid(self.SpeedHisto)      THEN obj_Destroy, self.SpeedHisto
  IF Obj_valid(self.PsInfo )         THEN Obj_Destroy, self.PsInfo
  IF Obj_valid(self.Annotation )     THEN Obj_Destroy, self.Annotation
  Ptr_Free, self.help
  Ptr_Free, self.PtrToCT

  IF Widget_Info( self.tlb, /valid ) THEN $
    Widget_Control, self.tlb, /destroy
END

;====================================================
;
; Prints the Help message
;
;====================================================


PRO Pv::SelfHelp
  XdisplayFile,'',text=*self.help
END


;====================================================
;
; Set procedure
;
;====================================================

PRO Pv::Set, xsize       = xsize, $     
             ysize       = ysize, $     
             XStatic     = XStatic,$  
             YStatic     = YStatic,$  
             XDynamic    = XDynamic,$  
             YDynamic    = YDynamic,$  
             firstClick  = firstClick ,$
             MinSpeed    = MinSpeed, $  
             MaxSpeed    = MaxSpeed, $  
             Length      = Length, $    
             Thickness   = Thickness, $
             Decimate_by = Decimate_by, $
             CRDecimate_by = CRDecimate_by, $
             ExcludeCols= ExcludeCols,$
             SpeedHisto  = SpeedHisto,$
             Ambiguities = Ambiguities, $
             InputPath   = InputPath,$ 
             InputFilter = InputFilter, $
             LatRange    = LatRange ,$
             LonRange    = LonRange,$
             Dimensions  = Dimensions ,$ ; for prepending a new MAPDIMS object
             DimsList    = DimsList ,$   ; for replacing the entire list
             DataList    = DataList,$
             RedrawFlag  = RedrawFlag ,$
             AmbigColors = AmbigColors ,$
             AmbigColorNames = AmbigColorNames ,$
             Sensitivity  = Sensitivity, $
             HCType       = HCType,$
             HCFile       = HCFile,$
             OutputPath   = OutputPath,$
             OutputFile   = OutputFile,$
             PsInfo       = PsInfo,$
             Annotation   = Annotation, $
             ColorBar     = ColorBar, $
             MainTitle    = MainTitle,$
             Xtitle       = Xtitle,$
             Ytitle       = Ytitle,$
             Subtitle     = Subtitle,$
             doAnnotations=doAnnotations, $
             doColorBar=doColorBar, $
             oplot=oplot





  redraw = Keyword_Set(RedrawFlag)
  resize = 0

  IF N_Elements(xsize) NE 0 THEN BEGIN 
    self.xsize = xsize
    resize = 1
  ENDIF
   
  IF N_Elements(ysize) NE 0 THEN BEGIN 
    self.ysize = ysize
    resize = 1
  ENDIF
   
  IF N_Elements(MinSpeed) NE 0 THEN BEGIN 
    self.MinSpeed = Minspeed
    self.ColorBar-> Set,Min = MinSpeed
  ENDIF 
 
  IF N_Elements(MaxSpeed) NE 0 THEN BEGIN 
    self.MaxSpeed = MaxSpeed
    self.ColorBar-> Set,Max = MaxSpeed
  ENDIF 

  IF N_Elements(Length) NE 0 THEN BEGIN 
    self.Length = Length
  ENDIF 

  IF N_Elements(Thickness) NE 0 THEN BEGIN 
    self.Thickness = Thickness
  ENDIF 

  IF N_Elements(Decimate_by) NE 0 THEN BEGIN 
    self.Decimate_by = Decimate_by
  ENDIF 

  IF N_Elements(CRDecimate_by) EQ 2 THEN BEGIN 
    self.CRDecimate_by = CRDecimate_by
  ENDIF 

  IF N_Elements(ExcludeCols) NE 0 THEN BEGIN 
    IF VarType(ExcludeCols) EQ 'STRING' THEN $
      self.ExcludeCols = ExcludeCols
  ENDIF 

  n_Ambigs = N_Elements(self.Ambiguities)
  IF N_Elements(Ambiguities) NE 0 THEN BEGIN 
    CASE 1 OF 
      N_Elements(Ambiguities) EQ 1: BEGIN 
        self.Ambiguities[Ambiguities < n_Ambigs] =  1
      END
      N_Elements(Ambiguities) EQ n_Ambigs: BEGIN     
        self.Ambiguities = Ambiguities
      END
      ELSE : BEGIN 
        Message, "Ambiguities must have 1 or " + $
        strtrim(n_Ambigs,2) + " elements ",/cont
        Message, "Taking 'selected' ambiguity",/cont
        self.Ambiguities =  self.Ambiguities*0
        self.Ambiguities[0] =  1
      END
    ENDCASE 
  ENDIF 

    ; Change the names of the colors used in plotting the ambiguities.
  n_Ambigs = N_Elements(self.Ambiguities)
  IF N_elements(AmbigColorNames)  GT 0 THEN BEGIN 
    IF N_Elements(AmbigColorNames) EQ n_Ambigs AND $
      VarType(AmbigColorNames) EQ 'STRING' THEN BEGIN 
      self.AmbigColorNames = AmbigColorNames
    ENDIF ELSE Message,'AmbigColorNames must be a STRING ARRAY having ' + $
      strtrim( n_Ambigs,2 ) + ' elements',/cont
  ENDIF 

  n_Ambigs = N_Elements(self.AmbigColors)
  IF N_Elements(AmbigColors) GT 0 THEN BEGIN 
    IF N_Elements(AmbigColors) EQ n_Ambigs THEN BEGIN 
      self.AmbigColors = Long(AmbigColors)
    ENDIF ELSE Message,'AmbigColors must have ' + $
     strtrim(n_Ambig,2)  + ' elements '
  ENDIF 
 
    ; change region's Longitude range
  nlon = N_Elements(LonRange)
  nlat = N_Elements(LatRange)

  IF Nlon + Nlat NE 0 THEN BEGIN 
      ; One or the other has changed
    Dims = self-> GetCurrentDimensions()
    dims-> Get, Lon = OldLonRange, Lat=OldLatRange
    limits2 = [ OldLonRange[0], OldLatRange[0], $
                OldLonRange[1], OldLatRange[1] ]
    
    CASE 1 OF 
      
      Nlon EQ 2 AND Nlat EQ 2 : BEGIN 
        limits1 =  [ LonRange[0], LatRange[0], $
                     LonRange[1], LatRange[1] ]
    
        IF NOT CompletelyWithin( limits1, limits2 ) THEN $
          self->SetResetPlotObjects,/All
        
        NewDims = Obj_New('MAPDIMS',LonRange,LatRange )
        s = self.DimsList-> Prepend(NewDims)
        IF NOT s THEN $
           Message,'Unable to create new dimensions in Dimension List ',/cont
      END
      Nlon EQ 2: BEGIN 
        limits1 =  [ LonRange[0], OldLatRange[0], $
                     LonRange[1], OltLatRange[1] ]
        IF NOT CompletelyWithin( limits1, limits2 ) THEN $
          self->SetResetPlotObjects,/All
        
        NewDims = Obj_New('MAPDIMS',LonRange, OldLatRange )
        s = self.DimsList-> Prepend(NewDims)
        IF NOT s THEN $
           Message,'Unable to create new dimensions in Dimension List ',/cont
      END 

      NLat EQ 2: BEGIN 
        limits1 =  [ OldLonRange[0], LatRange[0], $
                     OldLonRange[1], LatRange[1] ]

        IF NOT CompletelyWithin( limits1, limits2 ) THEN $
          self->SetResetPlotObjects,/All

        NewDims = Obj_New('MAPDIMS',OldLonRange,LatRange )
        s = self.DimsList-> Prepend(NewDims)
        IF NOT s THEN $
           Message,'Unable to create new dimensions in Dimension List ',/cont
      END 
      ELSE: Message,' lonrange and/or LatRange must be 2-Vectors',/cont
      
    ENDCASE 
  ENDIF 

    ; Change the members of the datalist
  IF N_Elements(DataList) THEN BEGIN 
    IF obj_Valid( DataList ) THEN BEGIN 
      IF Obj_IsA( DataList, 'LINKEDLIST') THEN BEGIN 
        IF self.DataList NE DataList THEN $
          Obj_Destroy, self.DataList
        Self.DataList = DataList
      ENDIF ELSE $
       Message," DataList requires object of type 'LinkedList'",/cont
    ENDIF ELSE $
     Message," DataList requires an OBJECT (type 'LinkedList')",/cont
  ENDIF 

    ; Input path and filter
  IF N_Elements(InputPath) NE 0 THEN self.InputPath = DeEnvVar(InputPath)
  IF N_Elements(InputFilter) NE 0 THEN self.InputFilter = InputFilter

    ; used in zooming
  IF N_Elements(XStatic) THEN self.XS = Xstatic
  IF N_Elements(YStatic) THEN self.YS = Ystatic

    ; used in zooming
  IF N_Elements(Xdynamic) NE 0 THEN self.Xd = Xdynamic
  IF N_Elements(Ydynamic) NE 0 THEN self.Yd = Ydynamic


    ; change dimensions of plotting region
  IF N_Elements(Dimensions) NE 0 THEN BEGIN 
    IF Obj_ISA(Dimensions, 'MAPDIMS' ) THEN BEGIN 
      CurrentDims = self->GetCurrentDimensions()
      IF Ptr_Valid(CurrentDims)  THEN BEGIN 
        Dims = *CurrentDims
        Dims-> Get,LonRange = OldLonRange, LatRange=OldLatRange
        Dimensions-> Get,LonRange = LonRange,LatRange=LatRange
        limits1 =  [ LonRange[0], LatRange[0], $
                     LonRange[1], LatRange[1] ]
        limits2 =  [ OldLonRange[0], OldLatRange[0], $
                     OldLonRange[1], OldLatRange[1] ]
        IF NOT CompletelyWithin( limits1, limits2 ) THEN $
         self-> ResetPlotObjects,/All
      ENDIF 
      s = self.DimsList->Prepend(Dimensions)
      IF NOT s THEN $
        Message,'Unable to prepend dimension to pv.Dimslist',/cont
    ENDIF ELSE BEGIN 
      Message,'Must use Object of type MapDims to set Map Dimensions ',/cont
    ENDELSE 
  ENDIF 

    ; Change the entire Dimensions List
  IF N_Elements(DimsList) NE 0 THEN BEGIN 
    IF obj_Valid( DimsList ) THEN BEGIN 
      IF Obj_IsA( DimsList,'LINKEDLIST') THEN BEGIN 
        IF self.DimsList EQ  DimsList THEN $
          self.DimsList =  DimsList $
        ELSE BEGIN 
          Obj_Destroy, Self.DimsList
          self.DimsList = DimsList
        ENDELSE 
        self-> ResetPlotObjects,/All
      ENDIF ELSE Message,'DimsList must be an Object of type LINKEDLIST',/cont
    ENDIF ELSE Message,'DimsList must be an Object of type LINKEDLIST',/cont
  ENDIF 

    ; Change the Histogram of the Speed Object.
  IF N_Elements(SpeedHisto) NE 0 THEN BEGIN 
    IF obj_Valid( SpeedHisto ) THEN BEGIN 
      IF Obj_IsA( SpeedHisto,'OBHISTO') THEN BEGIN 
        IF self.SpeedHisto EQ  SpeedHisto THEN $
          self.SpeedHisto =  SpeedHisto $
        ELSE BEGIN 
          Obj_Destroy, Self.SpeedHisto
          self.SpeedHisto = SpeedHisto
        ENDELSE 
      ENDIF ELSE Message,'SpeedHisto must be an Object of type OBJHISTO',/cont
    ENDIF ELSE Message,'SpeedHisto must be an Object of type OBJHISTO',/cont
  ENDIF 


  IF N_Elements(firstClick) NE 0 THEN self.firstClick = firstClick
  

  IF n_elements(OutputFile) NE 0 THEN self.OutputFile = OutputFile
  IF n_elements(OutputPath) NE 0 THEN BEGIN 
    self.OutputPath = DeEnvVar(OutputPath)
    IF rstrpos(self.outputPath,'/') NE $
     strlen(self.OutputPath)-1 THEN $
     self.OutputPath =  self.OutputPath + '/'
    psfilename = self.OutputPath+self.HCFile+'.ps'
    self.psInfo-> Set,filename = psfilename
  ENDIF 
   
  IF n_elements(HCType) NE 0 THEN self.HCType = HCType
  IF n_elements(HCFile) NE 0 THEN BEGIN 
    self.HCFile = HCFile
    psfilename = self.OutputPath + self.HCFile+'.ps'
    self.psInfo-> Set,filename = psfilename
  ENDIF 

    ; N.B. Resize calls draw itself.
  IF resize THEN self-> Resize,self.xsize,self.ysize ELSE $
    IF redraw THEN self-> Draw

  IF Keyword_set(Sensitivity) then BEGIN 
    self.sensitivity=1
    self-> ChangeSensitivity,/ON
  ENDIF 
  
  IF n_elements(psInfo) NE 0 THEN BEGIN 
    IF Obj_Isa(psInfo, 'PSFORM') THEN BEGIN 
      IF psInfo NE self.psInfo THEN obj_destroy, self.psInfo
      self.psInfo = psInfo 
    ENDIF 
  ENDIF 

  IF N_elements(Annotation) NE 0 THEN BEGIN 
    IF Obj_Isa( Annotation,'ANNOTATION') THEN BEGIN 
      IF annotation NE self.annotation THEN obj_destroy, annotation
      self.annotation = annotation
    ENDIF 
  ENDIF 

  IF N_elements(ColorBar) NE 0 THEN BEGIN 
    IF Obj_Isa( ColorBar,'COLORBAR') THEN BEGIN 
      IF ColorBar NE self.ColorBar THEN obj_destroy, ColorBar
      self.ColorBar = ColorBar
    ENDIF 
  ENDIF 

  
  IF N_elements(Maintitle) NE 0 THEN BEGIN 
    IF VarType( MainTitle ) EQ 'STRING' THEN $
      self.Annotation-> Set,MainTitle = Maintitle
  ENDIF 

  
  IF N_elements(Xtitle) NE 0 THEN BEGIN 
    IF VarType( XTitle ) EQ 'STRING' THEN $
      self.Annotation-> Set,XTitle = Xtitle
  ENDIF 

  
  IF N_elements(Ytitle) NE 0 THEN BEGIN 
    IF VarType( YTitle ) EQ 'STRING' THEN $
      self.Annotation-> Set,YTitle = Ytitle
  ENDIF 

  
  IF N_elements(Subtitle) NE 0 THEN BEGIN 
    IF VarType( SubTitle ) EQ 'STRING' THEN $
      self.Annotation-> Set,SubTitle = Subtitle
  ENDIF 

  IF n_Elements(doAnnotations) NE 0 THEN self.doAnnotations = doAnnotations
  IF n_Elements(doColorBar) NE 0 THEN self.doColorBar = doColorBar

  IF n_elements(oplot) NE 0 THEN BEGIN 
    IF isa(oplot,/object,objname='obplot') THEN BEGIN 
      IF self.oplot NE oplot THEN BEGIN 
        obj_destroy,self.oplob
        self.oplot = oplot
      ENDIF 
    ENDIF 
  ENDIF 

END



;====================================================
;
; Get procedure
;
;====================================================

PRO Pv::Get, xsize             = xsize, $      
             ysize             = ysize, $      
             MinSpeed          = MinSpeed, $   
             MaxSpeed          = MaxSpeed, $   
             Length            = Length, $     
             Thickness         = Thickness, $  
             Decimate_by       = Decimate_by, $
             CRDecimate_by     = CRDecimate_by, $
;             Decimate_flag     = Decimate_flag,$
             ExcludeCols       = ExcludeCols,$
             MinMaxSpeed       = MinMaxSpeed,$       
             SpeedHisto        = SpeedHisto,$
             Ambiguities       = Ambiguities, $
             AmbigColors       = AmbigColors,$
             AmbigColorMapping = AmbigColorMapping,$
             AmbigColorNames   = AmbigColorNames,$
             InputPath         = InputPath,$      
             InputFilter       = InputFilter, $   
             PixId             = PixId,$          
             Wid               = Wid,$            
             Xstatic           = Xstatic,$        
             Ystatic           = Ystatic,$        
             Xdynamic          = Xdynamic,$       
             Ydynamic          = Ydynamic,$       
             firstClick        = firstClick ,$    
             CurrentDims       = CurrentDims, $   
             DimsList          = DimsList,$       
             DataList          = DataList,$       
             StatusId          = StatusId,$       
             XYLabId           = XYLabId, $
             HCType            = HCType,$
             HCFile            = HCFile,$
             HCCntr            = HCCntr,$
             OutputHCFiles     = OutputHCFiles,$
             OutputFiles       = OutputFiles,$
             OutputPath        = OutputPath,$
             OutputFile        = OutputFile, $
             PsInfo            = PsInfo,$
             Annotation        = Annotation, $
             ColorBar          = colorBar, $
             doAnnotations     = doAnnotations, $
             doColorBar        = doColorBar, $
             oplot             = oplot

   IF Arg_Present(xsize)             THEN xsize             = self.xsize 
   IF Arg_Present(ysize)             THEN ysize             = self.ysize 
   IF Arg_Present(PixId)             THEN PixId             = self.PixId  
   IF Arg_Present(Wid)               THEN Wid               = self.Wid    
   IF Arg_Present(Xstatic)           THEN Xstatic           = self.Xs     
   IF Arg_Present(Ystatic)           THEN Ystatic           = self.Ys     
   IF Arg_Present(Xdynamic)          THEN Xdynamic          = self.Xd     
   IF Arg_Present(Ydynamic)          THEN Ydynamic          = self.Yd     
   IF Arg_Present(DimsList)          THEN DimsList          = self.DimsList 
   IF Arg_Present(DataList)          THEN DataList          = self.DataList
   IF Arg_Present(MinMaxSpeed)       THEN MinMaxSpeed       = self.MinMaxSpeed
   IF Arg_Present(MinSpeed)          THEN MinSpeed          = self.MinSpeed     
   IF Arg_Present(MaxSpeed)          THEN MaxSpeed          = self.MaxSpeed   
   IF Arg_Present(Length)            THEN Length            = self.Length     
   IF Arg_Present(Thickness)         THEN Thickness         = self.Thickness  
   IF Arg_Present(Decimate_by)       THEN Decimate_by       = self.Decimate_by  
   IF Arg_Present(CRDecimate_by)     THEN CRDecimate_by     = self.CRDecimate_by  
;   IF Arg_Present(Decimate_Flag)     THEN Decimate_Flag     = self.Decimate_Flag  
   IF Arg_Present(ExcludeCols)       THEN ExcludeCols     = self.ExcludeCols  
   IF Arg_Present(Ambiguities)       THEN Ambiguities       = self.Ambiguities  
   IF Arg_Present(InputPath)         THEN InputPath         = self.InputPath    
   IF Arg_Present(InputFilter)       THEN InputFilter       = self.InputFilter  
   IF Arg_Present(firstClick)        THEN firstClick        = self.firstClick   
   IF Arg_Present(StatusId)          THEN StatusId          = self.StatusId     
   IF Arg_Present(XYLabId)           THEN XYLabId           = self.XYLabId      
   IF Arg_Present(AmbigColorMapping) THEN AmbigColorMapping = self.AmbigColorMapping
   IF Arg_Present(AmbigColorNames)   THEN AmbigColorNames   = self.AmbigColorNames
   IF Arg_Present(AmbigColors)       THEN AmbigColors       = self.AmbigColors
   IF Arg_Present(SpeedHisto)        THEN SpeedHisto        = self.SpeedHisto
   IF Arg_Present(HCType)            THEN HCType            = self.HCType
   IF Arg_Present(HCFile)            THEN HCFile            = self.HCFile
   IF Arg_Present(HCCntr)            THEN HCCntr            = self.HCCntr
   IF Arg_Present(OutputPath)        THEN OutputPath        = self.OutputPath
   IF Arg_Present(OutputFile)        THEN OutputFile        = self.OutputFile
   IF Arg_Present(OutputHCFiles)     THEN OutputHCFiles     = self.OutputHCFiles
   IF Arg_Present(OutputFiles)       THEN OutputFiles       = self.OutputFiles
   IF Arg_Present(PsInfo)            THEN PsInfo            = self.PsInfo
   IF Arg_Present(Annotation)        THEN Annotation        = self.Annotation
   IF Arg_Present(ColorBar)          THEN ColorBar          = self.ColorBar
   IF Arg_Present(doAnnotations)     THEN doAnnotations     = self.doAnnotations
   IF Arg_Present(doColorBar)        THEN doColorBar        = self.doColorBar
   IF Arg_Present(oplot)             THEN oplot        = self.oplot


   IF Arg_Present(CurrentDims)       THEN CurrentDims = $
      self->GetCurrentDimensions()  
   
END


;====================================================
;
; GetCurrentDimensions: Gets the current extent of the 
;   plotting region. Returns an object of type 'MapDims'
;
;====================================================
FUNCTION Pv::GetCurrentDimensions
   data = self.DimsList->GetCurrent()
   IF Ptr_Valid( data ) THEN $
     return, *data ELSE $
     return, Ptr_New()
END


;====================================================
;
; SetCurrentDimensions: Sets the current extent of the 
;   plotting region. Does not add a new MapDims Object 
;   to the DimsList 
;
;====================================================

PRO Pv::SetCurrentDimensions, LonRange = LonRange, LatRange = LatRange
   data = self.DimsList->GetCurrent()
   CurrentDims =  *data
   NLon = N_Elements(LonRange) 
   NLat = N_Elements(LatRange) 
   CurrentDims-> Get,LonRange = OldLonRange, LatRange=OldLatRange
   limits2 = [ OldLonRange[0], OldLatRange[0], $
               OldLonRange[1], OldLatRange[1] ]
   IF Nlon + Nlat NE 0 THEN BEGIN 
     CASE 1 OF 
       NLon EQ 2 AND NLat EQ 2: BEGIN 
         limits1 = [ LonRange[0], LatRange[0], $
                     LonRange[1], LatRange[1] ]
         CurrentDims-> Set, Lon= LonRange, Lat=LatRange
       END 
       NLon EQ 2 : BEGIN 
         limits1 = [ LonRange[0], OldLatRange[0], $
                     LonRange[1], OldLatRange[1] ]
         CurrentDims-> Set, Lon= LonRange
       END
       NLat EQ 2              : BEGIN 
         limits1 = [ OldLonRange[0], LatRange[0], $
                     OldLonRange[1], LatRange[1] ]
         CurrentDims-> Set, Lat= LatRange
       end
       ELSE: BEGIN 
         Message,' Lon/Lat must be 2-Vector',/cont
         limits1 = limits2
       end
     ENDCASE 
   ENDIF 
   IF NOT CompletelyWithin( limits1, limits2 ) THEN self-> ResetPlotObjects,/All

END

;====================================================
;   GetColorIndex
;   Given one of 'Speed','Red', 'Green', 'Blue', '
; 'Yellow','Magenta','Aqua' or 'White', returns the index of that
; color in the color table, provided the user hasn't changed those
; indices.
;====================================================

FUNCTION pv::GetColorIndex, ColorName
  index = -2
  ii = -1
  REPEAT BEGIN 
    ii = ii+1
    j = where( strupcase(self.AmbigColorMapping[ii].Name) EQ $
               strupcase(ColorName), nj )
  ENDREP UNTIL ii GT N_Elements(self.AmbigColorMapping)  OR nj NE 0 
  IF ii LE N_Elements(self.AmbigColorMapping) AND nj NE 0 THEN $
   index = self.AmbigColorMapping[ii].ColorIndex
  return,index
END


;====================================================
; WriteToStatusBar
;====================================================
PRO Pv::WriteToStatusBar, Msg
   Widget_Control, self.StatusId, Set_Value=Msg
END


;============================================
; Version
;============================================

FUNCTION PV::Version

     ; Version number for this class
   rcsid = "$Id$"

     ; Find version number for member objects.
   s=execute( 'tags=tag_names({' + 'pv' + '})' ) 
   n_tags = n_elements(Tags)
   i = 0
   WHILE i LE n_tags-1 DO BEGIN 

     catch, error
     IF error NE 0 THEN BEGIN 
         ; Ignore 'undefined method' errors
       IF strpos( strupcase(!Error_state.Msg), $
                  "UNDEFINED METHOD" ) NE -1 THEN BEGIN 
         error = 0
         i = i+1
       ENDIF ELSE BEGIN 
         self-> set, Sensitivity = 1
         return,''
       ENDELSE 
     ENDIF 
     
     IF VarType( self.(i) ) EQ 'OBJECT' THEN BEGIN 
       IF Obj_Valid( self.(i) ) THEN BEGIN 
         V =  Call_Method( "VERSION", self.(i) )
         nv = N_Elements(V)
         IF exist(member_versions) THEN $
            member_versions =  [ member_versions, v ] ELSE $
            member_versions =  v
       ENDIF 
     ENDIF 
     i =  i+1
   ENDWHILE 

     ; find version number for superclasses.
   super = Obj_Class(self,/Super,count=cnt)
        
   IF cnt NE 0 THEN BEGIN 
     WHILE i LE cnt-1 DO BEGIN 
       catch, error
       IF error NE 0 THEN BEGIN 
           ; Ignore 'undefined method' errors
         IF strpos( strupcase(!Error_state.Msg), $
                    "UNDEFINED METHOD" ) NE -1 THEN BEGIN 
           error = 0
           i = i+1
         ENDIF ELSE BEGIN 
           Message,!error_state.msg,/cont
           self-> set, Sensitivity = 1
           return,''
         ENDELSE 
       ENDIF 

       V  = call_method("VERSION",super[i])

       IF exist( super_versions ) THEN $
         super_versions =  [super_versions, v ] ELSE $
         super_versions =  v 
       i = i+1

     ENDWHILE 
   ENDIF

   versions =  rcsid

   IF exist(super_versions) THEN $
      versions =  [versions, super_versions]

   IF exist( member_versions ) THEN $
      versions =  [versions, member_versions ] 

   Catch,/cancel
  return,versions(uniq(versions,sort(versions)))
END

;==================================================
;
; UpdateSpeedHisto - Update the speed histogram
;
;==================================================

PRO PV::UpdateSpeedHisto, u,v, nTotPlots

  good1 = where( finite(u) AND finite(v), ngood1 )
  IF ngood1 NE 0 THEN BEGIN 
    speed = sqrt( u[good1]^2 + v[good1]^2 ) 
    good =  where( speed ,ngood)
    self-> Get, SpeedHisto = SpeedHisto
    IF ngood NE 0 THEN BEGIN 
      speed = speed(good)
      IF nTotPlots EQ 0 THEN BEGIN 
          ; First plot
        SpeedHisto-> Set, Data = speed
        self.MinMaxSpeed = MinMax(Speed) 
      ENDIF ELSE BEGIN 
          ; self.MinMaxSpeed=[0,0] means there hasn't been
          ; any good data up to this point, so disregard it.
        test = where(self.MinMaxSpeed-[0.,0],ntest)
        IF ntest NE 0 THEN BEGIN 
          self.MinMaxSpeed = [ Min( [ Self.MinMaxSpeed[0], $
                                      Min(speed,max=mx) ] ), $
                               Max( [ Self.MinMaxSpeed[1], mx ] ) ]
        ENDIF ELSE BEGIN 
          self.MinMaxSpeed = [  MinMax(speed) ]
        ENDELSE 
        SpeedHisto-> Set, Data = speed, /Append
      ENDELSE 
    ENDIF ELSE BEGIN 
       ; No good data in this plot. Don't do
       ; anything if this isn't the first plot.
      IF nTotPlots EQ 0 THEN BEGIN 
          ; First plot
        SpeedHisto-> ClearAll
        self.MinMaxSpeed = [0.,0.]
      ENDIF 
    ENDELSE 
    self-> Set,SpeedHisto = SpeedHisto
  ENDIF 

END

;====================================================
;
; ResetPlotObjects.
;  Goes through the DataList, setting the appropriate flags for
;  plotting.
;
;====================================================
PRO Pv::ResetPlotObjects,All = All, AlreadyPlotted=AlreadyPlotted

   All = keyword_set(all)
   AlreadyPlotted = keyword_set(AlreadyPlotted)
   
   n = self.Datalist-> WhichNode()
   DataPtr =  self.DataList-> GetHead()
   WHILE Ptr_Valid(DataPtr) DO BEGIN 
     po = *DataPtr
     CASE 1 OF 
       ALL: BEGIN 
         po-> set, InRegion = -1, PlotFlag=1, AlreadyPlotted=0
       END
       AlreadyPlotted: BEGIN 
         po-> Set, AlreadyPlotted=0
       END
     ENDCASE 
     DataPtr = self.DataList-> GetNext()
   ENDWHILE 
   IF n NE 0 THEN s = self.Datalist-> GotoNode(n)

END


;====================================================
;
; CreateMap
;
;====================================================

PRO pv::CreateMap, Erase = Erase, force=force, $
      center = center, limit = limit

  postscriptDevice = !d.name EQ 'PS'

   map_extent = abs(limit[3]-limit[1])*abs(limit[2]-limit[0])
   dots =  map_extent GE (140*110)
   IF postscriptdevice THEN BEGIN 
     titlecolor = !p.color
   ENDIF ELSE BEGIN 
     IF self.visual NE 'PSEUDOCOLOR' THEN BEGIN 
       titlecolor = 'ffffff'x
     ENDIF ELSE BEGIN 
       titlecolor = 255
     ENDELSE 
   ENDELSE 

   IF self.doAnnotations OR self.doColorBar THEN BEGIN 
   
     IF self.doAnnotations THEN BEGIN   

       self.Annotation-> Get,MainTitle = Mtitle, $
        Xtitle=Xtitle, $
        Ytitle=Ytitle, SubTitle=SubTitle, $
        Xmargin=Xmargin, Ymargin=Ymargin, $
        Charsize=Charsize, CharThick=CharThick

       !p.subtitle = subtitle
       IF xmargin[0] eq 0 AND xmargin[1] EQ 0 THEN xmargin = 6
       IF ymargin[0] eq 0 AND ymargin[1] EQ 0 THEN ymargin = 5

     ENDIF 

     IF Erase OR Force THEN BEGIN 
       self-> ResetPlotObjects,/AlreadyPlotted
       Map_Set, 0,center[0], /Grid,/Label,/Continent,$
        limit=limit,Title=mtitle, $
        xmargin=xmargin, ymargin=ymargin, color=titlecolor
                                ;        oldLimit = limit
     ENDIF 

     IF self.doAnnotations THEN  BEGIN 
       IF strlen( Xtitle ) NE 0 THEN $
         XYOUTS,mean(!x.window), 0.75*!y.window[0], Xtitle, Align=0.5, /normal, $
         charsize=charsize, charthick=charthick,color=titlecolor

       IF strlen( Ytitle ) NE 0 THEN $
         XYOUTS, 0.75*!x.window[0], mean(!y.window), Ytitle, Align=0.5, /normal, $
         charsize=charsize, charthick=charthick, Orient=90,color=titlecolor
        !p.subtitle = ''
     ENDIF 
     IF self.doColorBar THEN BEGIN 
       self.ColorBar-> calc,/apply
       Map_Set, 0,center[0], limit=limit, $
         xmargin=xmargin, ymargin=ymargin, /noerase,/noborder
     ENDIF 
     
   ENDIF ELSE BEGIN 
     IF Erase OR Force THEN BEGIN 
       self-> ResetPlotObjects,/AlreadyPlotted
       Map_Set, 0,center[0], /Grid,/Label,/Continent,$
        limit=limit,Title=mtitle, color=titlecolor
     ENDIF 
   ENDELSE 	


  
END

;====================================================
; GetData
;  Get data out of object for current map limits
;   Gets only 1 ambiguity! 
;   Applies current decimation scheme!
;====================================================
PRO pv::GetData, u,v,lon,lat, ambig
  IF n_elements(ambig) EQ 0 THEN ambig = 0
  Dim = *(self.DimsList->GetCurrent())
  Dim->Get,lon = lon,lat=lat,center=center
  limit = [  lon[0], lat[0], lon[1], lat[1]  ]
  CurrentPlotDataPtr = self.DataList->GetHead()
  nrecs = 0
  IF Ptr_Valid( CurrentPlotDataPtr ) THEN BEGIN 
    first = 1
    WHILE Ptr_Valid(CurrentPLotDataPtr) DO BEGIN 


      po = *(CurrentPlotDataPtr)
      po-> Get,Data=Q2b, $
               SelectedOnly=SelectedOnly, $
               PlotFlag=PlotFlag, $
               AlreadyPlotted=AlreadyPlotted, $
               InRegion=InRegion
      s = q2b-> Set( Decimate    = self.Decimate_by, $
                     CRDecimate  = self.CRDecimate_by, $
                     ExcludeCols = self.ExcludeCols)
      nrecs =  nrecs + q2b-> Nrecs()
      CurrentPlotDataPtr = self.DataList-> GetNext()
    ENDWHILE
  ENDIF 
  u = (v= (lon= (lat= fltarr( 76, nrecs ))))
  
  ii = 0
  CurrentPlotDataPtr = self.DataList->GetHead()
  nfiles = self.dataList-> GetCount()
  file_cntr = 0
  IF Ptr_Valid( CurrentPlotDataPtr ) THEN BEGIN 
    first = 1
    WHILE Ptr_Valid(CurrentPLotDataPtr) DO BEGIN 
      file_cntr = file_cntr+1
      print,'working on ',file_cntr, ' of ', nfiles

      po = *(CurrentPlotDataPtr)
      po-> Get,Data=Q2b, $
               SelectedOnly=SelectedOnly, $
               PlotFlag=PlotFlag, $
               AlreadyPlotted=AlreadyPlotted, $
               InRegion=InRegion
      s = q2b-> Set( Decimate    = 0, $
                     CRDecimate  = [0,0], $
                     ExcludeCols = '' )
      status = q2b-> GetPlotData(uu,vv,llon,llat,ambig,limit=limit,/Silent)
      nn = q2b-> Nrecs()
      IF status NE 1 THEN BEGIN 
        Message,"Error Getting Data!",/cont
        return
      ENDIF 
      u  [*,ii:ii+nn-1] =  uu
      v  [*,ii:ii+nn-1] =  vv
      lon[*,ii:ii+nn-1] =  llon
      lat[*,ii:ii+nn-1] =  llat
      ii = ii+nn
      CurrentPlotDataPtr = self.DataList-> GetNext()
    ENDWHILE 
    u = temporary(u[*,0:ii-1])
    v = temporary(v[*,0:ii-1])
    lon = temporary(lon[*,0:ii-1])
    lat = temporary(lat[*,0:ii-1])
  ENDIF 
END


;====================================================
;
; Object Definition
;
;====================================================

PRO Pv__define

   ColorMapping =  { ColorMapping, Name: ' ', $
                      ColorTriple: bytarr(3),$
                      ColorIndex: 0l }
   
   colors = replicate(ColorMapping,8)

   junk = { PV, $

 ;----------- General Object Quantities ---------------------
            Help         : Ptr_New(), $
            Redraw       : 0l ,$ ; if something changes that 
                                 ; requires redrawing.
            Sensitivity  : 0l, $ ; flag for sensitivity s
                                 ; tate of widget.
            Verbose      : 0l, $ ; Verbosity flag.
            doAnnotations: 0l, $
            doColorBar   : 0l, $ 
            Annotation   : Obj_New(),$ ; for titles, and such
            ColorBar     : Obj_New(), $

 ;------------- Widget quantities ---------------
            tlb          : 0l ,$
            MenuId       : 0l ,$
            QuitId       : 0l ,$
            FileId       : 0l ,$
            ReadId       : 0l ,$
            WriteId      : 0l ,$
            ConfigId     : 0l ,$
            HardcopyId   : 0L ,$
            OverlayId    : 0l ,$
            OverplotId   : 0l, $
            PropagateId  : 0L, $
            MiscId       : 0l ,$
            SetSensId    : 0l ,$
            RedrawId     : 0L ,$
            Quit2Id      : 0L ,$
            DrawId       : 0l, $ ; IDL Draw Widget ID
            Wid          : 0l, $ ; IDL window ID
            PixID        : 0L, $ ; pixmap id
            StatusId     : 0L, $ ; Status Label Widget ID
            xyLabId      : 0L, $ ; Mouse XY location Label Widget ID
            xsize        : 0l ,$
            ysize        : 0l ,$
            firstClick   : 0l ,$
            xd           : 0l ,$
            yd           : 0l ,$
            xs           : 0l ,$
            ys           : 0l ,$
            visual       : '' ,$ ; either DIRECT, TRUE or PSEUDO.

  ;------------ Vector plotting quantities ---------------

            Ambiguities    : intarr(6),$ ; Ambiguities[i]=1 means plot i-th 
                                       ; ambiguity. Ambiguities[0] is 
                                       ; 'selected' vector, ambiguities[1] 
                                ; is 1st ...
                                       ; the 6th element is for 'error' 

            Ncolors      : 0l ,$ ; number of colors in table.
            Length       : 0. ,$ ; Length for the vectors
            Thickness    : 0. ,$ ; Thickness of vectors
            Decimate_by  : 0  ,$ ; take every n-th vector, passed 
                                 ;   to data object.
            CRDecimate_by : intarr(2),$; Col/Row Decimate, works like 
                                 ; decimate, but on columns and rows.
            Decimate_flag: 0  ,$ ; 1=decimate by col/ro, 0 otherwise.
            ExcludeCols  : '' ,$ ; e.g. '2,3,32:35,74,75'
            MinSpeed     : 0. ,$ ; Minimum speed to plot
            MaxSpeed     : 0. ,$ ; Maximum   "   "   "
            MinMaxSpeed  : fltarr(2) ,$ ; Store minmax speed for 
                                        ; pv_config widget
            SpeedHisto   : Obj_New(),$ ; Histogram Object for Speed Histogram
            AmbigColors  : lonarr(6),$ ; Latest colors indices used for each ambig.
                                       ; AmbigColors[0] = color index
                                       ; for  selected ambiguity,
                                       ; AmbigColors[1] for 1st
                                       ; Ambig, ... AmbigColors[5] is
                                       ; color index for 'model' or
                                       ; 'error', should there be such.
           AmbigColorNames: strarr(6) ,$ ; The respective names.
           AmbigColorMapping: colors ,$ ; The color mapping (future use)
           DimsList     : Obj_New(),$ ; Linked list of map dimensions

           PtrToCT      : Ptr_New(),$ ; Pointer to 3 by ncolors array 
                                      ; containing the color table

  ; ------------- Cloud Overlay Quantities ------------

            Satellite : '',$   ; Satellite Name (currently='Goes')
            SatNum    : 0L,$   ; Satellite Number 
                               ; (10 or 8 OR -1, IF N/A)
            SensNum   : 0L,$   ; Sensor Number (1=vis, 2=ir2, 3=ir3,
                               ; 4=ir4)
            Cutoff    : 0L,$   ; Value to Cut off histogram of cloud
                               ; intensities between cloud and
                               ; water/land
            Water0    : 0,$    ; Start of Water indices (cur= 1)
            Land0     : 0,$    ; Start of Land Colors in Color table
                               ; (cur= 7)
            Cloud0    : 0,$    ; Start of Cloud Indicies (cur=27)
            Wind0     : 0,$    ; Start of Wind Indicies (cur=47)
            Ambig0    : 0,$    ; Start of Ambiguity Color Indicies (cur=92)
                               ; (Red is the top of the Wind colors, so
                               ; there is some overlap)
            NWind     : 0,$    ; Number of Wind Colors. (cur=45)
                                
                ; All of these values are set in ::init
            
  ; ------------- I/O Quantities --------------
            InputPath    : '' ,$        ; Input path for data files
            InputFilter  : '' ,$        ; Input Filter for data files
            OutputPath   : '', $        ; Default path for output hardcopies/data.
            OutputFile   : '', $        ; Default Output Data file name.
;           OutputFilter   : '', $      ; Output filter for data files
            HCFile       : '', $        ; Default Output Hardcopy file name
            HCCntr       : 0 , $        ; For use as a extension.
            HCType       : '',$         ; Type of hardcopy
            oplot        : obj_new(),$  ; To hold positions to be overplotted
            PSInfo       : Obj_New(),$  ; To hold Postscript info.
            OutputHCFiles: Obj_New(),$  ; LinkedList of Output Hardcopies
            OutputFiles  : Obj_New(),$  ; Linked ist of Output Data Files.
            Datalist     : Obj_New()}   ; List of PvPlotObjects 
            
END


