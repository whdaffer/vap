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
;           data       : the Data itself, must be object of type Q2b or Qmodel
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

  Dims= *(self-> GetCurrentDimensions())
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
      val = 'X: ' + string( xy(0), form='(f7.3)') + $
       ' Y: ' + string( xy(1), form='(f7.3)') 
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

  CurrentDims = *CurrentDims
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


      NewDims = Obj_New('MapDims',xx,yy,/fix )
      self-> Set,Dimensions = NewDims
      str =  'new dimensions: lon ' + $
       string( xx, form='(2(f7.3,:,","))') + ' lat ' + $
       string( yy, form='(2(f7.3,:,","))')
      Message,str,/cont
        ; Draw the new area.
      self->Draw
;      IF widget_info( self.config_widget_id,/valid) THEN BEGIN 
;        self-> ChangeSensitivity,/off
;          Widget_Control, self.Config_widget_id, $
;           send_event = { Id: self.Config_widget_id, Top:self.tlb, $
;                          Handler:self.config_widget_id, action:'REDO_DIMS_LIST' }
;        self-> ChangeSensitivity,/on
;      ENDIF 
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
   ; Obj_Destroy, self
END


;====================================================
;
; Files->Read  event
;
;====================================================

PRO Pv::WidgetRead
  files = Mpickfile( path=self.InputPath, filter=self.InputFilter, $
                     Get_Path=NewPath, Get_Filter=NewFilter)
  self.InputPath = NewPath
  self.InputFilter = NewFilter
  
  cnt = n_elements( files )
  IF cnt EQ 1 AND strlen(files(0)) EQ 0 THEN cnt = 0
  IF cnt NE 0 THEN BEGIN 
    s = self->Read(files)
    self->Draw
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
    Catch,/Cancel
;      ok = Dialog_Message(!Error_State.msg)
;      Message,!error_state.msg,/cont
    ok = Dialog_Message(!err_string)
    Message,!err_string,/cont
    self-> ChangeSensitivity, On =  SaveSensitivity
    return 
  ENDIF 
  saveSensitivity = self.Sensitivity
  self-> ChangeSensitivity,/Off
  ;Widget_Control, self.StatusId, Set_Value='Writing...'
  self->WriteToStatusBar, 'Writing ... '

  file = Dialog_Pickfile( filter=self.OutputPath,Path=self.OutputPath, $
                          Get_path=OutputPath,/Write)
  IF strlen(file[0]) NE 0 THEN BEGIN 
    self.OutputPath =  OutputPath
    OpenW, Lun, file, /get_lun, error=err 
    IF Err EQ 0 THEN BEGIN 
      CurrentDimNode =  *(self.DimsList->GetCurrent())
      Dim = *CurrentDimNode.data
      Dim->Get,lon = lon,lat=lat,center=center
      limit = [  lon[0], lat[0], lon[1], lat[1]  ]

      junk = self.DataList->GetHead()
      CurrentPlotDataPtr = self.datalist->GetCurrentDataPtr()
      IF Ptr_Valid( CurrentPlotDataPtr ) THEN BEGIN 
        WHILE Ptr_Valid(CurrentPLotDataPtr) DO BEGIN 

          q2b = *(CurrentPlotDataPtr)
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
          FOR p=0,nPlots-1 DO BEGIN 
              ; GetPlotData likes limit as [lon0, lat0, lon1,lat1 ]
            t1 = systime(1)
            status = q2b->GetPLotData(u,v,lon,lat, PlotAmbiguities[p], $
                                      limit = limit, /Silent  )
;            print,'Time to Extract: ' , systime(1)-t1

            IF status THEN BEGIN 
              s = size(U)
              WriteU, Lun, s[1],s[2], PlotAmbiguities[p]
              WriteU, Lun, u,v,lon,lat
            ENDIF 
          ENDFOR 
          CurrentDataNode =  self.DataList->GetNext()
          CurrentPlotDataPtr = self.datalist->GetCurrentDataPtr()
        ENDWHILE 
      ENDIF ELSE Message," There's No Data!",/cont
      Free_Lun, lun
    ENDIF ELSE Message,!err_string,/cont
     
  ENDIF 

  self-> ChangeSensitivity, On =  SaveSensitivity
  ; Widget_Control, self.StatusId, Set_Value='Done Writing!'
  self->WriteToStatusBar, 'Done Writing '

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
         Jpeg[*,*,0] =  green(im)
         Jpeg[*,*,0] =  blue(im)
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
        self-> draw
        set_plot,'x'
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
    Catch,/cancel
;    ok =  Dialog_Message( !error_state.msg )
;    Message,!error_state.msg,/cont
    ok =  Dialog_Message( !err_string )
    Message,!err_string,/cont

    return
  ENDIF 

  Widget_Control, self.DrawId, draw_Xsize=x, Draw_Ysize=y
  wset, self.wid
  self-> Draw
  WDelete, self.pixId
  self.xsize = x
  self.ysize = y
  Window,/Free,/Pixmap, XSize=x, YSize=y
  self.pixId = !d.Window
  self-> CopyToPixmap
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
           Help          = Help             
   


  Catch, Error
  IF Error NE 0 THEN BEGIN 
    Catch,/Cancel
;    ok = Dialog_Message(!Error_State.msg)
;    Message,!error_state.msg,/cont
    ok = Dialog_Message(!err_string)
    Message,!err_string,/cont
    ; Obj_Destroy,self
    return,0
  ENDIF 
  self.rcsid = "$Id$"

  IF keyword_set( help ) THEN self-> SelfHelp

  self.Sensitivity = 1 ; Make sure we can operate on the widget.

  self.MinMaxSpeed = [ 1.e10, -1.e10 ] 

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
    IF path NE '' THEN InputPath = path ELSE InputPath ='/disk3/qscat_winds' 
  ENDIF ELSE BEGIN 
    IF VarType(InputPath) NE 'STRING' THEN BEGIN 
      Message,'InputPath must be of type STRING',/cont
      InputPath = '/disk3/qscat_winds' 
    ENDIF 
  ENDELSE 
  IF N_Elements(InputFilter) EQ 0 THEN InputFilter = '*.DAT *.hdf' ELSE BEGIN 
    IF VarType(InputFilter) NE 'STRING' THEN BEGIN 
      Message,'InputFilter must be of type STRING',/cont
      InputFilter = '*.DAT *.hdf' 
    ENDIF 
  ENDELSE 
  IF N_Elements(OutputPath)  EQ 0 THEN OutputPath = '/disk5/qscat' ELSE BEGIN 
    IF VarType(OutputPath) NE 'STRING' THEN BEGIN 
      Message,'OutputPath must be of type STRING',/cont
      OutputPath = '/disk5/qscat/' 
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
  self.InputPath     = InputPath     
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

  status = 1
  IF n_elements( files ) NE 0 THEN BEGIN 
    status = self-> Read( files )
  ENDIF ELSE IF n_elements( data ) NE 0 THEN BEGIN 
    IF Obj_Valid( data ) THEN BEGIN 
      status = self.DataList-> Append(data)
    ENDIF ELSE BEGIN 
      structure_name = STRUPCASE(Tag_Names( data, /structure_name) )
      CASE structure_name OF 
        'Q2BDATA'    : q = Obj_New('Q2B'   , data=data)
        'QMODELDATA' : q = Obj_New('QMODEL', data=data)
        ELSE     : BEGIN 
          Message,"Can't identify data type ",/cont        
          return,0
        END
      ENDCASE 
      status = self.DataList-> Append(q)
    ENDELSE 
  ENDIF 

    ; Get Pointer to color table.
  CT = GetEnv('PV_COLORTABLE')
  IF strlen(CT) NE 0 THEN $
    PtrToColorTable = ReadColorTable(CT[0]) $
  ELSE $
    PtrToColorTable = ReadColorTable($
                     '/usr/people/vapuser/Qscat/Resources/Color_Tables/pv.ct.2')
;  PtrToColorTable = ReadColorTable($
;                     '/home/daffer/idl5/OO/pv.ct')
  status =  status AND Ptr_Valid(PtrToColorTable)


  IF status THEN BEGIN
    self.PtrToCt = PtrToColorTable
    CT =  *PtrToColorTable
    self.Ncolors = N_elements( CT[0,*] )
    self.PsInfo = Obj_New('PSFORM') ;
    self.Annotation = Obj_new('ANNOTATION')

      ; Only realize the widget if the status = 1
    status = self-> CreateWidget()
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
    Catch,/Cancel
;    ok = Dialog_Message(!Error_State.msg)
;    Message,!error_state.msg,/cont
    ok = Dialog_Message(!err_string)
    Message,!err_string,/cont
    ; Obj_Destroy,self
    return,0
  ENDIF 

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
  junk =  Widget_Button( MenuId, Value='Hard Copy',/Menu, $
                                  UValue='HARDCOPY')
  self.HardCopyId =  Widget_Button( junk, Value='Configure Hard Copy', $
                                    UValue='CONFIGHARDCOPY')
  junk = Widget_Button( MenuId, Value='Overlay',/Menu,Uvalue='OVERLAY')
  self.OverlayId = Widget_Button( junk,Value='Cloud Overlay', $
                                  Uvalue='CLOUDOVERLAY')
  self.DrawId =  Widget_Draw( self.tlb, scr_xsize=self.xsize, $
                              scr_ysize=self.ysize, $
                              /button_events, retain=2, $ 
                              colors=self.Ncolors,$
                              event_pro='pv_draw_events')
  junk = Widget_Base( self.tlb, col=2 )
  self.StatusId = Widget_Label( junk,Scr_XSize=self.xsize/2-4,$
                                   Scr_YSize=20, Frame=2, $
                                /Dynamic_Resize,/Align_Left)
  self.xyLabId =  Widget_Label( junk,Scr_XSize=self.xsize/2-2,$
                                Scr_YSize=20, Frame=2, /Dynamic_Resize, $
                                 Value='X:      ,Y:    ',/Align_Left );, $
;                                  Font="helvetica-medium-o-normal--10")


  Widget_Control,self.tlb,/Realize
  Widget_Control, self.DrawId, get_value=wid
  self.wid = wid
  Window,/Free,/Pixmap,colors=self.Ncolors,XSize=self.XSize, YSize=self.YSize
  self.PixId = !d.Window
  Wset, wid
  TvLct, transpose(*self.PtrToCT)
  Widget_Control, self.tlb, set_uval=self
    ; Draw whatever data there is.
  Self->Draw
    ; Call Xmanager to Manage the Widget.
  XManager, 'PV', self.TLB, event_handler='pv_events', /no_block
  return,1
END

;====================================================
;
; Draw: Draw the contents of the datalist
;
;====================================================


PRO Pv::Draw
  Catch, Error
  IF Error NE 0 THEN BEGIN 
    Catch,/Cancel
;      ok = Dialog_Message(!Error_State.msg)
;      Message,!error_state.msg,/cont
    ok = Dialog_Message(!err_string)
    Message,!err_string,/cont
  ENDIF 

;  print,'Pv::Draw'
  status = 1
  self.Annotation-> Get,MainTitle = Mtitle, Xtitle=Xtitle, $
                      Ytitle=Ytitle, SubTitle=SubTitle

  ; X/Y title must be applied using _xyouts_
  ; Mtitle gets passed in to Map_set. Subtitle will be seen only if
  ; x/y margin is set in _Map_set_

  !p.subTitle = subTitle

  IF NOT Widget_Info( self.Tlb,/Valid ) THEN status =  self-> CreateWidget()
  IF !d.name NE 'PS' THEN Wset, self.wid
  IF status THEN BEGIN 
    Widget_Control,/HourGlass
    SaveSensitivity = self.Sensitivity
    self-> ChangeSensitivity, /OFF
    ; Widget_Control, self.StatusId, Set_Value='Drawing...'
    self->WriteToStatusBar,' Drawing ... '

    CurrentDimNode =  *(self.DimsList->GetCurrent())
    Dim = *CurrentDimNode.data
    Dim->Get,lon = lon,lat=lat,center=center
    limit = [  lon[0], lat[0], lon[1], lat[1]  ]

      ; Map_set likes limit as [lat0, lon0, lat1, lon1]

    IF strlen(!p.SubTitle) NE 0 OR $
       strlen( Xtitle )    NE 0 OR $
       strlen( YTitle )    NE 0 THEN BEGIN 

      Map_Set, 0,center[0], /Grid,/Label,/Continent,$
       limit=[ lat[0], lon[0], lat[1], lon[1] ],Title=mtitle, $
          xmargin=6, ymargin=5

      IF strlen( Xtitle ) NE 0 THEN $
        XYOUTS, 0.5, 0.75*!y.window[0], Xtitle, Align=0.5, /normal, $
           charsize=1.2

      IF strlen( Ytitle ) NE 0 THEN $
        XYOUTS, 0.75*!x.window[0], 0.5, Ytitle, Align=0.5, /normal, $
           charsize=1.2, Orient=90

    ENDIF ELSE BEGIN 

      Map_Set, 0,center[0], /Grid,/Label,/Continent,$
       limit=[ lat[0], lon[0], lat[1], lon[1] ],Title=mtitle
    ENDELSE 	



    junk = self.DataList->GetHead()
    CurrentPlotDataPtr = self.datalist->GetCurrentDataPtr()
    IF Ptr_Valid( CurrentPlotDataPtr ) THEN BEGIN 
      WHILE Ptr_Valid(CurrentPLotDataPtr) DO BEGIN 

        q2b = *(CurrentPlotDataPtr)
        s = q2b-> Get( Filename=filename)
;        Widget_Control, self.StatusId, $
;          Set_Value="Drawing (Extracting from " + filename + ")"
        junk = rstrpos(filename,'/')+1
        basename = strmid( filename, junk, strlen(filename)-junk)
        self->WriteToStatusBar, "Drawing (Extracting from " + basename + ")"
        s = q2b-> Set( Decimate    = self.Decimate_by, $
                       CRDecimate  = self.CRDecimate_by, $
                       ExcludeCols = self.ExcludeCols)
        PlotAmbiguities =  where( self.ambiguities NE 0, nPlots)
          ; self.ambiguities[0]=1 => plot 'selected' ambiguities.
          ; self.ambiguities[1]=1 => plot 1st ambiguity ... etc.
        nTotPlots = 0
        FOR p=0,nPlots-1 DO BEGIN 
            ; GetPlotData likes limit as [lon0, lat0, lon1,lat1 ]
          t1 = systime(1)
          status = q2b->GetPLotData(u,v,lon,lat, PlotAmbiguities[p], $
                                    limit = limit, /Silent  )
;          print,'Time to Extract: ' , systime(1)-t1
          IF status THEN BEGIN 
            t1 = systime(1)
            good1 = where( finite(u) AND finite(v), ngood1 )
            IF ngood1 NE 0 THEN BEGIN 
              speed = sqrt( u[good1]^2 + v[good1]^2 ) 
              good =  where( speed ,ngood)
              IF ngood NE 0 THEN BEGIN 
                self-> Get, SpeedHisto = SpeedHisto

                speed = speed(good)
                IF nTotPlots EQ 0 THEN BEGIN 
                    ; First plot
                  SpeedHisto-> Set, Data = speed
                  self.MinMaxSpeed = MinMax(Speed) 
                ENDIF ELSE BEGIN 
                  self.MinMaxSpeed = [ Min( [ Self.MinMaxSpeed[0], $
                                              Min(speed,max=mx) ] ), $
                                     Max( [ Self.MinMaxSpeed[1], mx ] ) ]
                  SpeedHisto-> Set, Data = speed, /Append
                ENDELSE 
              ENDIF ELSE BEGIN 
                SpeedHisto-> Set, data = Ptr_New()
                self.MinMaxSpeed = [0.,0.]
              ENDELSE 
              self-> Set,SpeedHisto = SpeedHisto
  ;            print,'Time to extract SpeedHisto info: ', systime(1)-t1
  ;            Widget_Control, self.StatusId, $
  ;              Set_Value="Drawing "
            ENDIF 
            self->WriteToStatusBar,'Drawing...'
            
              ; Plot the vectors
            t1 = systime(1)
            PlotVect,u,v,lon,lat,$
              Length = self.Length, $
                thick=self.Thickness, $
                  Color=self.AmbigColors[PlotAmbiguities[p]],$
                    MinSpeed=self.MinSpeed, $
                     MaxSpeed=self.MaxSpeed, $
                       Start_index=self.Wind0,$
                        NColors=self.NWind
;            print,'Time to Plot: ', systime(1)-t1
              ; Copy new plot to pix map
            self->CopyToPixMap
              ; Reset to plotting window
          ENDIF 
          nTotPlots =  nTotPlots + 1
        ENDFOR 
        CurrentDataNode =  self.DataList->GetNext()
        CurrentPlotDataPtr = self.datalist->GetCurrentDataPtr()
      ENDWHILE 
    ENDIF ELSE Message," There's No Data!",/cont


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
    Widget_Control, self.MenuId, Sensitive=1
    Widget_Control, self.DrawId, Draw_Button_Events=1
    self.sensitivity = 1
  ENDIF ELSE BEGIN 
    Widget_Control, self.MenuId, Sensitive=0
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
      IF Widget_Info( self.StatusId, /valid ) THEN $
       junk = rstrpos(files[f],'/')+1
      basename = strmid( files[f], junk[0], strlen(files[f])-junk[0] )
      self-> WriteToStatusBar,'Reading file ' +  basename + $
       ' (' + strtrim(f+1,2) + ' of ' + nfiles_string + ')'
;         Widget_Control, self.StatusId,Set_Value='Reading File ' + $
;            files(f) + ' (' + strtrim(f+1,2) + ' of ' + nfiles_string + ')'

      IF Hdf_IsHdf( files(f) ) THEN BEGIN 
        ; It may be a q2b file or a model file.
        attr = hdfgetattr( files(f), attr='ShortName' )
        IF VarType( attr ) EQ 'STRUCTURE' THEN BEGIN 
          CASE *attr.value OF 
            'QSCATVAPMODEL': q = Obj_New('qmodel',filename=files(f) )
            'QSCATL2B'     : q = Obj_New('q2b',filename=files(f) )
            ELSE           : BEGIN 
              Message,"Can't identify type of HDF file ",/cont
              print,'  Attribute "ShortName" must be either QSCATL2B or QSCATVAPMODEL'
              status = 0
            END
          ENDCASE 

        ENDIF ELSE BEGIN 
          Message,"Can't get ShortName attribute from HDF file",/cont
          status = 0
        ENDELSE 
      ENDIF ELSE BEGIN 
          ; q2b can read some non-HDF formats, too.
        q = Obj_New('q2b',filename=files(f) )
      ENDELSE 
       
      IF Obj_Valid( q ) THEN BEGIN 
        s =  q-> Set(Decimate = self.decimate_by,$
                     CRDecimate = self.CRdecimate_by, $
                     ExcludeCols= self.ExcludeCols )
        status =  self.DataList->append(q)
      ENDIF ELSE status = 0

    ENDFOR 
  ENDIF 
  IF Widget_Info( self.StatusId, /valid ) THEN $
   self-> WriteToStatusBar, 'Done Reading '
;    Widget_Control, self.StatusId,Set_Value='Done Reading '

  RETURN,status
END

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
  Xdisplay,'',text=*self.help
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
             MainTitle    = MainTitle,$
             Xtitle       = Xtitle,$
             Ytitle       = Ytitle,$
             Subtitle     = Subtitle





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
  ENDIF 
 
  IF N_Elements(MaxSpeed) NE 0 THEN BEGIN 
    self.MaxSpeed = MaxSpeed
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
    Dims = *(self-> GetCurrentDimensions())
    dims-> Get, Lon = OldLonRange, Lat=OldLatRange
    CASE 1 OF 
      
      Nlon EQ 2 AND Nlat EQ 2 : BEGIN 
        NewDims = Obj_New('MAPDIMS',LonRange,LatRange )
        s = self.DimsList-> Prepend(NewDims)
        IF NOT s THEN $
           Message,'Unable to create new dimensions in Dimension List ',/cont
      END
      Nlon EQ 2: BEGIN 
        
        NewDims = Obj_New('MAPDIMS',LonRange, OldLatRange )
        s = self.DimsList-> Prepend(NewDims)
        IF NOT s THEN $
           Message,'Unable to create new dimensions in Dimension List ',/cont
      END 

      NLat EQ 2: BEGIN 
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
  IF N_Elements(InputPath) NE 0 THEN self.InputPath = InputPath
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
    self.OutputPath = OutputPath
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
             Annotation        = Annotation

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
   junk = self.DimsList->GetCurrent()
   IF Ptr_Valid( junk ) THEN $
     return, (*junk).data ELSE $
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
   junk = self.DimsList->GetCurrent()
   CurrentDims =  *(*junk).data
   NLon = N_Elements(LonRange) 
   NLat = N_Elements(LatRange) 
   IF Nlon + Nlat NE 0 THEN BEGIN 
     CASE 1 OF 
       NLon EQ 2 AND NLat EQ 2: CurrentDims-> Set, Lon= LonRange, Lat=LatRange
       NLon EQ 2              : CurrentDims-> Set, Lon= LonRange
       NLat EQ 2              : CurrentDims-> Set, Lat= LatRange
       ELSE: Message,' Lon/Lat must be 2-Vector',/cont
     ENDCASE 
   ENDIF 
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
            rcsid        : '',$
            Annotation   : Obj_New(),$ ; for titles, and such

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
            Cloud0    : 0,$   ; Start of Cloud Indicies (cur=27)
            Wind0     : 0,$   ; Start of Wind Indicies (cur=47)
            Ambig0    : 0,$   ; Start of Ambiguity Color Indicies (cur=92)
                               ; (Red is the top of the Wind colors, so
                               ; there is some overlap)
            NWind     : 0,$   ; Number of Wind Colors. (cur=45)
                                
                ; All of these values are set in ::init
            
  ; ------------- I/O Quantities --------------
            InputPath    : '' ,$        ; Input path for data files
            InputFilter  : '' ,$        ; Input Filter for data files
            OutputPath   : '', $        ; Default path for output hardcopies/data.
            OutputFile   : '', $        ; Default Output Data file name.
;           OutputFilter   : '', $      ; Output filter for data files
            HCFile       : '', $        ; Default Output Hardcopy file name
            HCCntr       : 0 , $        ; For use as a extension.
            HCType       : '',$      ; Type of hardcopy
            PSInfo       : Obj_New(),$  ; To hold Postscript info.
            OutputHCFiles: Obj_New() ,$ ; LinkedList of Output Hardcopies
            OutputFiles  : Obj_New(),$  ; Linked list of Output Data Files.
            Datalist     : Obj_New()}   ; List of data objects (Q2b,Qmodel)
            
END


