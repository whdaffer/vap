;+
; NAME:  Pv_config
; $Id$
; PURPOSE:  Popup widget under 'config' pick on PVs main menu
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Qscat/Seawinds Widget (PV)
;
;
;
; CALLING SEQUENCE:  
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
; KEYWORD PARAMETERS:  
;
;
;
; OUTPUTS:  
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;
;
; SIDE EFFECTS:  
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
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;-----------------------------------------------------
;pv_config_Cleanup
;-----------------------------------------------------

PRO Pv_Config_Cleanup, id
  Widget_Control, id, Get_UValue=info
  Ptr_Free, (*info).DataListIndex
  Ptr_Free, info
END

;-----------------------------------------------------
;DecimateCWBut_Events
;-----------------------------------------------------
FUNCTION DecimateCWBut_Events, Event
  retevent = 0
  Widget_Control, Event.top, Get_UValue=info
  IF Event.value EQ 0 THEN BEGIN 
    Widget_Control, (*info).Decimate1DBaseId,Map=1
    Widget_Control, (*info).Decimate2DBaseId,Map=0
  ENDIF ELSE BEGIN 
    Widget_Control, (*info).Decimate1DBaseId,Map=0
    Widget_Control, (*info).Decimate2DBaseId,Map=1
  ENDELSE 
  return, retevent
END

;-----------------------------------------------------
;Pv_Config_DrawLimLines
;-----------------------------------------------------
PRO pv_Config_DrawLimLines, info, self, MinSpeed, MaxSpeed
    ; Get the SpeedHisto object.
  self-> Get, SpeedHisto = SpeedHisto
    ; Get the information from the Speed Histogram Object.
  SpeedHisto-> Get, Histo = Histo, XHisto=XHisto, $
    BinSize=BinSize, NBins=Nbins, Min=Min, Max=Max

  IF Ptr_Valid( Histo ) THEN BEGIN 
    IF Ptr_Valid( XHisto ) THEN XHisto = *XHisto ELSE $
      XHisto = findgen(nBins)*Binsize + Min
    Plot, Xhisto, *Histo, Psym=10, $
        Title= 'Histogram of Data in Current Plotting Region'
;    Wset, (*info).PixId
;    Plot, Xhisto, *Histo, Psym=10, $
;        Title= 'Histogram of Data in Current Plotting Region'
    Pv_Config_CopyToPixmap, (*info).Pixid, (*info).wid, (*info).xsize,(*info).ysize

    Wset, (*info).Wid
    tmp=Convert_Coord([0,0],!y.CRange,/data,/to_normal) 
    (*info).yr=tmp(1,*)+[1,-1]*0.001
      ; Convert to Normal Coordinates
    speed = Convert_Coord( [MinSpeed,Maxspeed], [0,0], /data, /to_normal )
    speed = reform(speed[0,*]) 
     ; Redraw the two limit lines.
    Plots,[1,1]*speed[0], (*info).yr, color=(*info).ColorIndex,/Normal
    Plots,[1,1]*speed[1], (*info).yr, color=(*info).ColorIndex,/Normal
  ENDIF ELSE BEGIN 
    Plot, findgen(100), /nodata, xstyle=4, ystyle=4
    XYOuts,0.5,0.5,'No Histogram Available ',/normal,align=0.5
    (*info).yr = [0,0.]
  ENDELSE 


END


;-----------------------------------------------------
;Pv_Config_DrawEvents
;-----------------------------------------------------

PRO Pv_Config_DrawEvents, event

  ButtonTypes = ['LEFT', 'MIDDLE', 'RIGHT' ]
  EventTypes =  [ 'DOWN','UP','MOTION','SCROLL']
  IF EventTypes( event.type ) NE 'DOWN' THEN return

  Widget_Control, event.top, get_UValue=info
  Widget_Control, (*info).tlb, get_UValue=self
  Wset, (*info).Wid
  xsize = (*info).xsize
  ysize = (*info).ysize
  pixid =  (*info).pixid


  Button = ButtonTypes[ (event.press AND 7)/2 ]

;  print,'DrawEvents: Button = ',button
  (*info).Button = Button
    ; Get Min/Max Speed, as it appears in the Widget
  Widget_Control, (*info).MinSpeedId, Get_Value=MinSpeed
  Widget_Control, (*info).MaxSpeedId, Get_Value=MaxSpeed
  Device, Set_Graphics_Function=3 ; copy source to destination

    ; To keep the picture updated, copy the Pixmap to the 
    ; pv_config Draw Widget
  Device, copy=[0,0,xsize, ysize, 0, 0, pixID ]

    ; Also, redraw the data into the histogram draw widget. If the use
    ; has hit the 'Apply' button, the plot transform will be  set for
    ; the mapping coordinate system and all of our calls to
    ; Convert_Coord will produce the wronge conversions. So, redraw
    ; the whole plot to assure that we have the correct coordinate
    ; system set up. 

    ; Get the SpeedHisto object.
  self-> Get, SpeedHisto = SpeedHisto
    ; Get the information from the Speed Histogram Object.
  SpeedHisto-> Get, Histo = Histo, XHisto=XHisto, $
    BinSize=BinSize, NBins=Nbins, Min=Min, Max=Max

    ; Plot it, taking care of possibly bad pointers.
  IF Ptr_Valid( Histo ) THEN BEGIN 
    IF Ptr_Valid( XHisto ) THEN XHisto = *XHisto ELSE $
      XHisto = findgen(nBins)*Binsize + Min
    Plot, Xhisto, *Histo, Psym=10, $
        Title= 'Histogram of Data in Current Plotting Region'
    Wset, (*info).PixId
    Plot, Xhisto, *Histo, Psym=10, $
        Title= 'Histogram of Data in Current Plotting Region'
    Wset, (*info).Wid
    tmp=Convert_Coord([0,0],!y.CRange,/data,/to_normal) 
    (*info).yr=tmp(1,*) 
      ; Convert to Normal Coordinates
    speed = Convert_Coord( [MinSpeed,Maxspeed], [0,0], /data, /to_normal )
    speed = reform(speed[0,*]) 
     ; Redraw the two limit lines.
    Plots,[1,1]*speed[0], (*info).yr, color=(*info).ColorIndex,/Normal
    Plots,[1,1]*speed[1], (*info).yr, color=(*info).ColorIndex,/Normal
  ENDIF ELSE BEGIN 
    Plot, findgen(100), /nodata, xstyle=4, ystyle=4
    XYOuts,0.5,0.5,'No Histogram Available ',/normal,align=0.5
    (*info).yr = [0,0.]
  ENDELSE 


    ; Convert the button event to Normal Coordinates
  xn = Convert_Coord( event.x, event.y, /Device, /To_Normal )
  xd = Convert_Coord( event.x, event.y, /Device, /To_Data )
  xn = xn[0]
  xd = xd[0]
  yr = (*info).yr
  CASE Button OF 
    'LEFT': BEGIN 
      X = [1,1]*( 0 > xn< Speed[1])
      CX =  [1,1]*Speed[1]
      Widget_Control, (*info).MinSpeedId, Set_Value=xd
    END 
    'MIDDLE': BEGIN 
      ok = Dialog_Message('Middle Button Selections are not implemented yet!')
    END
    'RIGHT': BEGIN 
      X =  (Speed[0] > xn < (*info).xsize)*[1,1]
      CX =  [1,1]*Speed[0]
      Widget_Control, (*info).MaxSpeedId, Set_Value=xd
    END 
  ENDCASE 
  Plots,CX,YR, Color=(*info).colorIndex,/Normal
  Plots, X, YR, Color=(*info).ColorIndex, /Normal
  Widget_Control, Event.id, /Draw_Motion_Events, $
   Event_pro='pv_config_motionevents'
  Widget_Control, event.top, set_uval=info

END

;-----------------------------------------------------
;Pv_Config_MotionEvents
;-----------------------------------------------------

PRO Pv_Config_MotionEvents, event

  Widget_Control, event.top, Get_UValue=info

  yr     = (*info).yr
  Wid    = (*info).Wid
  Xsize  = (*info).xsize
  ysize  = (*info).ysize
  PixId  = (*info).PixId
  Button = (*info).Button

;  print,'MotionEvents: Button = ',Button
  EventTypes =  [ 'DOWN','UP','MOTION','SCROLL']
  ThisEvent =  EventTypes(event.type)
  WSET, wid

  Widget_Control, (*info).MinSpeedId, Get_Value=MinSpeed
  Widget_Control, (*info).MaxSpeedId, Get_Value=MaxSpeed

  speed = Convert_Coord( [MinSpeed,MaxSpeed],[0,0],/data, /To_Normal )
  speed = reform( speed[0,*] )
  xn = Convert_Coord( event.x, event.y, /device,/to_Normal)
  xd = Convert_Coord( event.x, event.y, /device,/to_Data)
  xn = xn[0]
  xd = xd[0]

  IF ThisEvent eq 'UP' THEN BEGIN 
    ;print,'Up Event!'

    Device, Set_Graphics_Function=3 ; copy source to destination
    Device, copy=[0,0,xsize, ysize, 0, 0, pixID ]
    Widget_Control, event.id, Draw_Motion_Events=0, $
     event_pro= 'pv_config_drawevents'
    CASE Button OF 
      'LEFT'  : BEGIN 
        X =  [1,1]*(0>  xn < speed[1])
        CX =  [1,1]*Speed[1]
        Widget_Control, (*info).MinSpeedId, Set_Value=xd
      END 
      'RIGHT' : BEGIN 
        X =  [1,1]*(speed[0] > xn <  xsize )
        CX =  [1,1]*Speed[0]
        Widget_Control, (*info).MaxSpeedId, Set_Value=xd
      END 
      ELSE:
    ENDCASE 
    Plots, X,YR, Color=(*info).colorIndex,/Normal
    Plots,CX,YR, Color=(*info).colorIndex,/Normal
    RETURN
  ENDIF 

  DEVICE,set_graphics_funct=3 ; copy
  DEVICE, copy=[0,0,xsize, ysize, 0,0, pixId ]
  DEVICE,set_graphics_funct=6 ; xor the source and destination


  CASE Button OF 
    'LEFT'  : BEGIN 
      X =  [1,1]*(0>  xn < speed[1])
      CX =  [1,1]*Speed[1]
      Widget_Control, (*info).MinSpeedId, Set_Value=xd
    END 
    'RIGHT' : BEGIN 
      X =  [1,1]*(speed[0] > xn <  xsize)
      CX =  [1,1]*Speed[0]
      Widget_Control, (*info).MaxSpeedId, Set_Value=xd
    END 
    ELSE:
  ENDCASE 
  Plots,  X, YR, Color=(*info).colorIndex, /Normal
  Plots, CX, YR, Color=(*info).colorIndex, /Normal
  DEVICE, set_graphics_funct=3

END


;-----------------------------------------------------
;pv_config_MakeDimsList: make a list of MapDimensions 
;                 suitable for a Widget_List
;-----------------------------------------------------

FUNCTION pv_config_MakeDimsList, pv_object
   t1 = systime(1)
  MapDimsList = ''
  IF Obj_IsA( pv_object, 'PV') THEN BEGIN 
    pv_object-> Get,DimsList=DimsList
    current = DimsList-> GetHead()
    WHILE Ptr_Valid( current ) DO BEGIN 
      Dims = *(*current).data
      Dims-> Get, Lon = lon, Lat=Lat
      dims_string = string( [ Lon[0], Lat[0], Lon[1], Lat[1] ], $
                       form='( "<", f7.2,",",f7.2,",",f7.2,",",f7.2,">" )' )

      IF N_Elements(MapDimsList) EQ 0 THEN $
        MapDimsList = dims_string ELSE $
        MapDimsList = [ dims_string, MapDimsList  ]
       current = DimsList-> GetNext()
    ENDWHILE

    ; Reset the List pointer to the head, since this is always the
    ; 'current'  dimension

  junk = DimsList-> GetHead()

  ENDIF ELSE BEGIN 
    Message,"Usage: dimslist=pv_config_MakeDimsList(pv_object)"
    MapDimsList = ''
  ENDELSE 
;   print,'Time to create dims list ',systime(1)-t1
  return, MapDimsList
END



;-----------------------------------------------------
;Pv_Config_MakeDataFileList: make a list of Data Files
;                 suitable for a Widget_List
;-----------------------------------------------------

FUNCTION Pv_Config_MakeDataFileList, pv_object

  t1 = systime(1)
  
  IF Obj_IsA( pv_object, 'PV') THEN BEGIN 

    pv_object->Get, DataList=DataList
    nFiles = DataList-> GetCount()
    ii = 0
    IF nFiles GT 0 THEN BEGIN 
      filelist = strarr(nFiles)

      junk =  DataList-> GetHead()
      CurrentDataPtr = DataList->GetCurrentDataPtr()
      WHILE ptr_valid( CurrentDataPtr ) DO BEGIN 
        t2 = systime(1)
        *CurrentDataPtr->Get,Data = Q2b
        s = q2b-> Get(filename=filename)
;        print,'Time to get filename: ', systime(1)-t2
        filelist[ii] = filename
        s = DataList-> GetNext()
        CurrentDataPtr =  DataList-> GetCurrentDataPtr()
        ii = ii+1
      ENDWHILE 

        ; Reset 'Current' Pointer to 'head'
      junk =  DataList-> GetHead()
    ENDIF ELSE filelist = '<No Data Files>'

  ENDIF ELSE BEGIN 
    Message,"Usage: datafilelist=Pv_Config_MakeDataFileList(pv_object)"
    filelist = ''
  ENDELSE 
;   print,'Time to create Data File list ',systime(1)-t1
  return, filelist
END

;-----------------------------------------------------
; PV_Config_Position_Dims_Sliders
;-----------------------------------------------------
PRO PV_Config_Position_Dims_Sliders, info, lon, lat
   FixedLon = FixLonRange(lon)
   Widget_Control, (*info).LonMinId, Set_Value=FixedLon[0]
   Widget_Control, (*info).LonMaxId, Set_Value=FixedLon[1]
   Widget_Control, (*info).LatMinId, Set_Value=Lat[0]
   Widget_Control, (*info).LatMaxId, Set_Value=Lat[1]
END

;-----------------------------------------------------
;Pv_Config_PrevDims_Events 
;-----------------------------------------------------

PRO Pv_Config_PrevDims_Events, event
   Widget_Control, event.top, Get_UValue=info
   Widget_Control, (*info).tlb,Get_Uvalue=self
   self-> Get,DimsList = DimsList
   nDims = DimsList-> GetCount()
   index = nDims-event.index
   n = DimsList-> GotoNode(index)
   IF Ptr_Valid(n) THEN BEGIN 
     Dims = *(DimsList-> GetCurrentDataPtr())
     Dims-> Get, Lon = Lon, Lat=Lat
     PV_Config_Position_Dims_Sliders, info, lon, lat
   ENDIF 
   Widget_Control, (*info).DeleteDimId,Sensitive=(event.index NE  0 )
END

;-----------------------------------------------------
;AmbigSelectBgroup_Events
;-----------------------------------------------------

FUNCTION AmbigSelectBgroup_Events, event
  retevent = 0
  Widget_Control, event.top, Get_Uvalue=info
  Widget_Control, (*info).tlb, Get_UValue=self
  (*info).redraw = 1
  self-> Set, Ambiguities = event.value
  (*info).AmbigArray = event.value
  Widget_Control, (*info).tlb, Get_UValue=self  
  Widget_Control, event.top, Set_Uvalue=info
  return,retevent
END


;-----------------------------------------------------
;ConfigChoiceBgroup_Events
;-----------------------------------------------------

FUNCTION ConfigChoiceBgroup_Events, event
  Widget_Control, event.top, Get_Uvalue=info
;  Names = [ $
;    'Map Dimensions', $
;    'Vector Configuration', $
;    'Ambiguity/Plotting Color Selection', $
;    'Data List Pruning',$
;    'Plot Annotations']

  retevent = 0
  CASE event.value OF
    'Map Dimensions'                     : map_array = [1,0,0,0,0]
    'Vector Configuration'               : BEGIN 
        ; Update the MinMax Speed Widgets
      Widget_Control, (*info).tlb, Get_UValue=self
      self-> Get, MinMaxSpeed = MinMaxSpeed
      Widget_Control, (*info).CurrentSpeedMinId, $
        Set_Value=string( MinMaxSpeed[0], form='(f7.2)')
      Widget_Control, (*info).CurrentSpeedMaxId, $
        Set_Value=string( MinMaxSpeed[1], form='(f7.2)')
      Widget_Control, (*info).MinSpeedId, Get_Value=MinSpeed
      Widget_Control, (*info).MaxSpeedId, Get_Value=MaxSpeed
      Wset,(*info).Wid

        ; Get the Speed Histogram Object
      self-> Get,SpeedHisto = SpeedHisto
        ; If the ColorIndex isnt' set, set it!
      IF (*info).ColorIndex EQ -1 THEN BEGIN 
        Colors = ['Green','Red','Aqua','Yellow','Magenta','White','Blue']
        ii = -1
        REPEAT BEGIN 
          ii = ii+1
          index = self-> GetColorIndex(Colors(ii))
        ENDREP UNTIL index GT 0 
        (*info).ColorIndex = Index
      ENDIF 
      IF (*info).ColorIndex EQ -1 THEN $
         (*info).ColorIndex = !d.N_Colors-1 

        ; Get the information from the Speed Histogram Object.
      SpeedHisto-> Get, Histo = Histo, XHisto=XHisto, $
        BinSize=BinSize, NBins=Nbins, Min=Min, Max=Max

        ; Plot it, taking care of possibly bad pointers.
      IF Ptr_Valid( Histo ) THEN BEGIN 
        IF Ptr_Valid( XHisto ) THEN XHisto = *XHisto ELSE $
          XHisto = findgen(nBins)*Binsize + Min
        Plot, Xhisto, *Histo, Psym=10, $
            Title= 'Histogram of Data in Current Plotting Region'
        Wset, (*info).PixId
        Plot, Xhisto, *Histo, Psym=10, $
            Title= 'Histogram of Data in Current Plotting Region'
        Wset, (*info).Wid
        tmp=Convert_Coord([0,0],!y.CRange,/data,/to_normal) 
        (*info).yr=tmp(1,*) 
        speed = Convert_Coord( [MinSpeed,Maxspeed], [0,0], /data, /to_normal )
        speed = reform(speed[0,*]) 
        Plots,[1,1]*speed[0], (*info).yr, color=(*info).ColorIndex,/Normal
        Plots,[1,1]*speed[1], (*info).yr, color=(*info).ColorIndex,/Normal
      ENDIF ELSE BEGIN 
        Plot, findgen(100), /nodata, xstyle=4, ystyle=4
        XYOuts,0.5,0.5,'No Histogram Available ',/normal,align=0.5
        (*info).yr = [0,0.]
      ENDELSE 
      map_array = [0,1,0,0,0]
    END 
    'Ambiguity/Plotting Color Selection' : map_array = [0,0,1,0,0]
    'Data List Pruning'                  : map_array = [0,0,0,1,0]
    'Plot Annotations'                   : map_array = [0,0,0,0,1]
    ELSE:
  endcase
  FOR i=0,n_elements(map_array)-1 DO $
   Widget_Control, (*info).bases[i], map=map_array[i]
  Widget_Control, event.top, Set_Uvalue=info
  return,retevent
END


;-----------------------------------------------------
; Pv_Config_CopytoPixMap
;-----------------------------------------------------
PRO Pv_Config_CopytoPixmap, pixid, wid, xsize, ysize
  WSet, pixID
  Device, Copy=[0, 0, xsize, ysize, 0, 0, wid]
END

;-----------------------------------------------------
; Pv_CONFIG_Events
;-----------------------------------------------------

PRO PV_CONFIG_Events, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Descript
  Widget_Control, Event.top, Get_UValue=info
  Widget_Control, (*info).tlb, Get_UValue=self

  IF Descript EQ 'CANCEL' THEN BEGIN 
    Widget_control, (*info).tlb, set_Uvalue=self
    Widget_Control, event.top,/destroy
    return 
  ENDIF 

  
  CASE Descript OF 
;    'PREVDIMS': BEGIN
;      Print, 'Event for PREVDIMS'
;      (*info).prevdimsindex = event.index
;      (*info).Redraw = 1
;    END
    'GOTODIM': BEGIN 
      selected =  Widget_info( (*info).PrevDimsId, /List_Select)
      IF selected[0] NE -1 THEN BEGIN 
        self-> Get, DimsList = DimsList
         ; The list in the widget is arranged in reverse order of the
         ; pv object, so take this into account.
        nDims = DimsList-> GetCount()
        index = nDims-Selected[0]
        n =  DimsList-> GotoNode(index)
        IF Ptr_Valid(n) THEN BEGIN 
          Widget_Control, (*info).CurrentSpeedMinId, $
            Set_Value="Push 'Apply' for"
          Widget_Control, (*info).CurrentSpeedMaxId, $
            Set_Value="Min/Max Speed Update"
        ENDIF 
        (*info).redraw = 1
      ENDIF 
    END
    'DELETEDIM': BEGIN 
      selected =  Widget_info( (*info).PrevDimsId, /List_Select)
      IF selected[0] NE -1 THEN BEGIN 
        self-> Get, DimsList = DimsList
         ; The list in the widget is arranged in reverse order of the
         ; pv object, so take this into account.
        nDims = DimsList-> GetCount()
        index = nDims-Selected[0]
        n =  DimsList-> GotoNode(index)
        IF Ptr_Valid(n) THEN BEGIN 
          DimsList-> DeleteCurrent
          MapDimsList = pv_config_MakeDimsList( self )
          Widget_Control, (*info).PrevDimsId, Set_Value=MapDimsList
          Dims = *(DimsList-> GetCurrentDataPtr())
          Dims-> Get, Lon = Lon, Lat=Lat
          PV_Config_Position_Dims_Sliders, info, Lon, Lat
          (*info).redraw = 1
        ENDIF ELSE Message,"Error deleting selected Dims ",/cont
      ENDIF 
    END
    'CLEARDIMSLIST': BEGIN 
      self-> Get, DimsList = DimsList
      Obj_Destroy, Dimslist
      Dims = Obj_New('MapDims',[0.,359.999],[-90.,90] )
      DimsList = Obj_New('linkedlist',Dims)
      self-> Set, DimsList = DimsList
      MapDimsList = pv_config_MakeDimsList(self)
      Widget_Control, (*info).PrevDimsId, Set_Value=MapDimsList
      PV_Config_Position_Dims_Sliders, info, [0.,359.999],[-90.,90]
      (*info).redraw = 1
    END
;    'DATALIST': BEGIN
;      Print, 'Event for DATALIST'
;      IF NOT Ptr_Valid( (*info).DataListIndex ) THEN BEGIN 
;        (*info).DataListIndex =  Ptr_New(event.index+1)
;      ENDIF ELSE BEGIN 
;        indexList = *(*info).DataListIndex 
;        indexList = [indexList, event.index+1]
;        indexList = indexList(sort(indexList))
;        *(*info).DataListIndex = indexList 
;      ENDELSE 
;       (*info).Redraw = 1
;      Print, 'Event for DATAFILESLIST'
;    END
    'DELETEDATAENTRY': BEGIN
      selected = Widget_Info( (*info).DataListId , /LIST_SELECT )
      IF selected[0] NE -1 THEN BEGIN 
        self-> Get, Datalist=DataList
        nFiles = DataList-> GetCount()
        IF nFiles NE 0 THEN BEGIN 
          nSelected = N_Elements(selected)
          FOR s=0, nSelected-1 DO BEGIN 
            n = DataList-> GoToNode( selected[s]+1 ) ; Nodes are '1' indexed
            IF Ptr_Valid( n ) THEN DataList-> DeleteCurrent
          ENDFOR 
          filelist = Pv_Config_MakeDataFileList( self )
          Widget_Control, (*info).DataListId, Set_Value=filelist
          (*info).Redraw = 1
        ENDIF 
      ENDIF 
        

    END
    'CLEARLIST': BEGIN
     ok = Dialog_Message("Clear Current Data?",/Question )
     ok = strupcase(ok)
     IF ok EQ 'YES' THEN BEGIN 
       self-> Get, DataList= DataList
       Obj_Destroy, DataList
       DataList = Obj_New('linkedlist' )
       self-> Set, DataList = DataList
       Widget_Control, (*info).DataListId, Set_Value='<No Data Files>'
       (*info).Redraw = 1
     ENDIF 
    END
    'MINSPEED': BEGIN
      Widget_Control, (*info).MinSpeedId, Get_Value=MinSpeed
      Widget_Control, (*info).MaxSpeedId, Get_Value=MaxSpeed
      Pv_Config_DrawLimLines, info, self, MinSpeed, MaxSpeed
    END
    'MAXSPEED':BEGIN
      Widget_Control, (*info).MinSpeedId, Get_Value=MinSpeed
      Widget_Control, (*info).MaxSpeedId, Get_Value=MaxSpeed
      Pv_Config_DrawLimLines, info, self, MinSpeed, MaxSpeed
    END
;    'EXCLUDECOLS': BEGIN
;    END 
    ELSE: BEGIN 

      IF Descript NE 'APPLY' AND Descript NE 'DISMISS' THEN RETURN
      Widget_Control, (*info).LonMinId,        Get_Value = LonMin        
      Widget_Control, (*info).LonMaxId,        Get_Value = LonMax        
      Widget_Control, (*info).LatMinId,        Get_Value = LatMin        
      Widget_Control, (*info).LatMaxId,        Get_Value = LatMax        
      Widget_Control, (*info).MinSpeedId,      Get_Value = NewMinSpeed   
      Widget_Control, (*info).MaxSpeedId,      Get_Value = NewMaxSpeed   
      Widget_Control, (*info).LengthId,        Get_Value = NewLength     
      Widget_Control, (*info).ThickId,         Get_Value = NewThick      
      Widget_Control, (*info).SelectedColorId, Get_Value = SelectedColor
      Widget_Control, (*info).FirstColorId,    Get_Value = FirstColor 
      Widget_Control, (*info).SecondColorId,   Get_Value = SecondColor
      Widget_Control, (*info).ThirdColorId,    Get_Value = ThirdColor 
      Widget_Control, (*info).FourthColorId,   Get_Value = FourthColor
      Widget_Control, (*info).Model1ColorId,   Get_Value = Model1Color
      Widget_Control, (*info).AmbigId,         Get_Value = NewAmbiguities
      Widget_Control, (*info).DecimateCwButId, Get_Value = DecimateCwButVal
      Widget_Control, (*info).ExcludeColsId,   Get_Value = NewExcludeCols
;      WidgetControl,  (*info).Decimate1D,      Get_Value = newDecimate1D
;      WidgetControl,  (*info).Decimate2DCol,   Get_Value = newDecimate2DCosl
;      WidgetControl,  (*info).Decimate2Drow,   Get_Value = newDecimate2DRows
      Widget_Control, (*info).MainTitleId, Get_Value = NewMainTitle
      Widget_Control, (*info).XTitleId, Get_Value = NewXTitle
      Widget_Control, (*info).YTitleId, Get_Value = NewYTitle
      Widget_Control, (*info).SubTitleId, Get_Value = NewSubTitle      


      colorNames = [ SelectedColor, FirstColor,SecondColor,ThirdColor,$
                     FourthColor,Model1Color]
      self-> Get, AmbigColors = AmbigColors, $
                   Ambiguities=Ambiguities,$
                     AmbigColorNames=AmbigColorNames
                
      nc = N_Elements(colorNames)
      junk = where( colorNames NE AmbigColorNames, nj )
      IF nj NE 0 THEN BEGIN 
        self-> Set, AmbigColorNames = colorNames
        FOR i=0,nc-1 DO AmbigColors[i] = self-> GetColorIndex(colorNames[i])
         self-> Set,AmbigColors = AmbigColors
        (*info).redraw = 1
      ENDIF 

      junk = where( Ambiguities NE NewAmbiguities, nj )
      IF nj NE 0 THEN BEGIN 
        self-> Set, Ambiguities=NewAmbiguities
        (*info).redraw = 1
      ENDIF 

        ; See if the Dimensions have changed
      CurDims = *(self->GetCurrentDimensions())
      CurDims-> Get, Lon = Lon, Lat=Lat
      NewLon = [LonMin, LonMax]
      NewLon = FixLonRange(NewLon,west=west)
      NewLat =  [LatMin, LatMax ] 
      junk = where( abs(NewLon-Lon) GT 0.01 OR abs(NewLat-Lat) GT 0.1, nj )
      IF nj NE 0 THEN BEGIN 
          Dims = Obj_New('MapDims',NewLon, NewLat)
          IF Obj_Valid( Dims ) THEN BEGIN 
            self->Set, Dimensions = Dims
          ENDIF 

          ; Rebuild the List of dimensions in the widget
        MapDimsList = pv_config_MakeDimsList( self )

;        self-> Get,DimsList=DimsList
;        current = DimsList-> GetHead()
;        WHILE Ptr_Valid( current ) DO BEGIN 
;          Dims = *(*current).data
;          Dims-> Get, Lon = lon, Lat=Lat
;          dims_string = string( [ Lon[0], Lat[0], Lon[1], Lat[1] ], $
;                        form='( "<", f7.2,",",f7.2,",",f7.2,",",f7.2,">" )' )

;          IF N_Elements(MapDimsList) EQ 0 THEN $
;            MapDimsList = dims_string ELSE $
;            MapDimsList = [ dims_string, MapDimsList  ]
;           current = DimsList-> GetNext()
;        ENDWHILE

;        junk = DimsList-> GetHead()

        Widget_Control, (*info).PrevDimsId, set_Value=MapDimsList
        (*info).redraw = 1
      ENDIF 

       ; Check the other quantites.
      self-> Get, MinSpeed = MinSpeed, $
        MaxSpeed=MaxSpeed, $
          Length=Length, $
            Thickness=Thick
       

      IF NewMinSpeed NE MinSpeed THEN BEGIN 
        self-> Set,MinSpeed = NewMinSpeed
        (*info).redraw = 1
      ENDIF 

      IF NewMaxSpeed NE MaxSpeed THEN BEGIN 
        self-> Set,MaxSpeed = NewMaxSpeed
        (*info).redraw = 1
      ENDIF 

      IF NewLength NE Length THEN BEGIN 
        self-> Set,Length = NewLength
        (*info).redraw = 1
      ENDIF 

      IF NewThick NE Thick THEN BEGIN 
        self-> Set,Thick = NewThick
        (*info).redraw = 1
      ENDIF 


      IF DecimateCwButVal EQ 0 THEN BEGIN 
          ; 1D decimation
        Widget_Control, (*info).Decimate1D, Get_Value=new_Decimate_by
        self-> Get,Decimate_by=Decimate_by
        IF new_Decimate_by NE Decimate_by THEN BEGIN 
           self-> Set,Decimate_by = new_Decimate_by,CRDecimate=[0,0]
           (*info).redraw = 1
        ENDIF 
      ENDIF ELSE BEGIN 
          ; 2D decimation
        Widget_Control, (*info).Decimate2DCol, Get_Value=ColDecimate
        Widget_Control, (*info).Decimate2DRow, Get_Value=RowDecimate
        self-> Get,CRDecimate_by = CRDecimate_by
        IF CRDecimate_by[0] NE ColDecimate OR $
           CRDecimate_by[1] NE RowDecimate THEN BEGIN 
          self-> Set,CrDecimate_by = [ColDecimate,RowDecimate]
          (*info).redraw = 1
        ENDIF 
      ENDELSE 

      self-> Get,ExcludeCols = ExcludeCols 
      NewExcludeCols = NewExcludeCols[0]
      IF NewExcludeCols NE ExcludeCols THEN BEGIN 
        self-> Set, ExcludeCols = NewExcludeCols
        (*info).redraw = 1
      ENDIF 


      self-> Get,Annotation = Annotation
      Annotation-> Get,MainTitle = MainTitle,Xtitle=Xtitle,$
       YTitle=YTitle,SubTitle=SubTitle

      newMaintitle = newMainTitle[0]
      newXTitle = newXTitle[0]
      newYTitle = newYTitle[0]
      newSubTitle = newSubTitle[0]

      IF newMainTitle NE MainTitle THEN BEGIN 
        Annotation-> Set,MainTitle = newMainTitle
        (*info).redraw = 1
      ENDIF 

      IF newXTitle NE XTitle THEN BEGIN 
        Annotation-> Set,XTitle = newXTitle
        (*info).redraw = 1
      ENDIF 



      IF newYTitle NE YTitle THEN BEGIN 
        Annotation-> Set,YTitle = newYTitle
        (*info).redraw = 1
      ENDIF 



      IF newSubTitle NE SubTitle THEN BEGIN 
        Annotation-> Set,SubTitle = newSubTitle
        (*info).redraw = 1
      ENDIF 

       self-> Set,Annotation = Annotation
       
;        ; Check to see if the user has requested an old map dimension
;        ; from the list of previous dimensions.
;      IF (*info).PrevDimsIndex NE -1 THEN BEGIN 
;          ; The list in the widget is arranged in reverse order of the
;          ; pv object, so take this into account.
;        print,'going to old dims '
;        self-> Get, DimsList=DimsList
;        nDims = DimsList-> GetCount()
;        index = nDims-(*info).PrevDimsIndex
;        Dims = DimsList->GotoNode( Index )
;        IF Ptr_Valid(Dims) THEN BEGIN 
;          self-> Set, DimsList = DimsList
;          (*(*dims).data)->Get,Lon = Lon,Lat=Lat
;          Widget_Control,(*info).LonMinId,Set_Value=Lon[0]
;          Widget_Control,(*info).LonMaxId,Set_Value=Lon[1]
;          Widget_Control,(*info).LatMinId,Set_Value=Lat[0]
;          Widget_Control,(*info).LatMaxId,Set_Value=Lat[1]
;          (*info).redraw = 1
;        ENDIF 
;      ENDIF 



        ; See if the user requested deletion of some (or all) of the
        ; entries in the 'datafilelist' Linked List. But Make sure
        ; there are files to be deleted!

;      self-> Get, DataList = DataList
;      nFiles = DataList-> GetCount()
;      IF nFiles GT 0 THEN BEGIN 

;        CASE 1 OF 
;          (*info).ClearDataListFlag EQ 1: BEGIN 
;            Obj_Destroy, DataList
;            DataList = Obj_New( 'LinkedList')
;            self-> Set,DataList = DataList
;            (*info).redraw = 1
;          END 
;          Ptr_Valid( (*info).DataListIndex ) :BEGIN 
;            indexList =  *(*info).DataListIndex
;            nn = n_elements( indexList)
;            FOR i=0,nn DO BEGIN 
;              s = DataList-> GotoNode( indexList[i] )
;              IF Ptr_Valid(s) THEN $
;               DataList-> DeleteCurrent
;            ENDFOR 
;            self-> Set, DataList = DataList
;            (*info).redraw = 1
;          END 
;          ELSE:
;        ENDCASE 

;      ENDIF 


    END ; End the 'else' of the outermost 'case' 

  ENDCASE
  redraw = (*info).redraw
  tlb = (*info).tlb
;  print,' About to redraw'

  IF Descript EQ 'DISMISS' THEN BEGIN 
    Widget_Control, event.top, /Destroy
  ENDIF 

  IF redraw THEN  self->Draw ; if necessary, Redraw.
  IF Ptr_Valid(info) THEN (*info).redraw = 0
  Widget_Control, tlb, Set_Uvalue=self
  Widget_Control, event.top, Set_Uvalue=info, bad_id=bad

END

;-----------------------------------------------------
;
;  pv_config: Widget Definition Routine
;
;-----------------------------------------------------


PRO pv_config, GROUP=Group


  Catch, error
  IF error NE 0 THEN BEGIN 
    Catch,/cancel
;    ok =  Dialog_Message( !error_state.msg )
;    Message,!error_state.msg,/cont
    ok =  Dialog_Message( !err_string )
    Message,!err_string,/cont

    return
  ENDIF 

  IF N_ELEMENTS(Group) EQ 0 THEN BEGIN 
     Message,'Must be called with Group="TLB of calling Widget"',/cont
     return
  ENDIF 

  Widget_Control, Group, Get_UValue=self

  TLB = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Plot Vector Configurator')

  BASE2 = WIDGET_BASE(TLB, $
      COLUMN=1, $
      FRAME=2, $
      MAP=1, $
      UVALUE='BASE2')

  junk = [ $
    'Map Dimensions', $
    'Vector Configuration', $
    'Ambiguity/Plotting Color Selection', $
    'Data List Pruning',$
    'Plot Annotations']

      ConfigureChoiceBgroupId = CW_BGROUP( BASE2, junk, $
      ROW=2, $
      EXCLUSIVE=1, $
      Event_Funct='ConfigChoiceBgroup_Events',$
      UVALUE='CONFIGURECHOICEBGROUP', $
      /Return_Name)

      Widget_Control, ConfigureChoiceBgroupId, Set_Value=0


  BulletinBoardBase = WIDGET_BASE(TLB, $
      FRAME=2, $
      MAP=1, $
      UVALUE='BulletinBoardBase')

;----------------------------------------------------------------
;
;             Map dimensions 
;
;----------------------------------------------------------------

  MapDimsId = WIDGET_BASE(BulletinBoardBase, $
      ROW=7, $
      MAP=1, $
      UVALUE='MapDimsId')

  LABEL7 = WIDGET_LABEL( MapDimsId, $
      FONT='-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1', $
      UVALUE='MAPDIMS', $
      VALUE='Map Dimensions')

  BASE36 = WIDGET_BASE(MapDimsId, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE36')


  Dims = *(self->GetCurrentDimensions())
  Dims-> Get, Lon = Lon, Lat=Lat

  LABEL38 = WIDGET_LABEL( BASE36, $
      FONT='-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1', $
      UVALUE='LABEL38', $
      VALUE='Longitude')

  BASE58 = WIDGET_BASE(BASE36, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE58')

  LonMinId = CW_FSLIDER( BASE58, $
      DRAG=1, $
      MAXIMUM=359.999, $
      MINIMUM=-180.00000, $
      TITLE='Min', $
      UVALUE='LONMIN', $
      /EDIT, $
      VALUE=lon[0])

  LonMaxId = CW_FSLIDER( BASE58, $
      DRAG=1, $
      MAXIMUM=359.999, $
      MINIMUM=0.00000, $
      TITLE='Max', $
      UVALUE='LONMAX', $
      /EDIT, $
      VALUE=Lon[1])



  BASE44 = WIDGET_BASE(MapDimsId, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE44')

  LABEL52 = WIDGET_LABEL( BASE44, $
      FONT='-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1', $
      UVALUE='LABEL52', $
      VALUE='Latitude')

  BASE59 = WIDGET_BASE(BASE44, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE59')

  LatMinId = CW_FSLIDER( BASE59, $
      DRAG=1, $
      MAXIMUM=90.0000, $
      MINIMUM=-90.0000, $
      TITLE='Min', $
      UVALUE='LATMIN', $
      /EDIT, $
      VALUE=Lat[0])

  LatMaxId = CW_FSLIDER( BASE59, $
      DRAG=1, $
      MAXIMUM=90.0000, $
      MINIMUM=-90.0000, $
      TITLE='Max', $
      UVALUE='LATMAX', $
      /EDIT, $
      VALUE=Lat[1])


    ; Make list for Widget_List of Map dimensions
  MapDimsList =  pv_config_MakeDimsList( self )


  PrevdimsId = WIDGET_LIST( MapDimsId,$
      VALUE=MapDimsList, $
      UVALUE='PREVDIMS', $
      YSIZE=3, $
      Event_Pro='Pv_Config_PrevDims_Events')

  GoToDimId =  Widget_Button( MapDimsId, VAlue='Go to This One',$
                              UValue='GOTODIM')

  DeleteDimId =  Widget_Button( MapDimsId, VAlue='Delete This One',$
                              UValue='DELETEDIM')

  ClearDimsListId =  Widget_Button( MapDimsId, VAlue='Clear This List',$
                              UValue='CLEARDIMSLIST')
;----------------------------------------------------------------
;
;                 Vector Configuration 
;
;----------------------------------------------------------------
  self-> Get,Length = Length, Thick=Thick, MinSpeed=MinSpeed, $
   MaxSpeed=MaxSpeed, Decimate=Decimate_by


  TopVecConfigId = Widget_Base(BulletinBoardBase, $
                                 row=3,map=0)

  VecConfigId = WIDGET_BASE(TopVecConfigId, $
      COLUMN=2, $
      UVALUE='VecSlidersId')

  LABEL74 = WIDGET_LABEL( VecConfigId, $
      FONT='-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1', $
      UVALUE='LABEL74', $
      VALUE='Vector Configuration')

  junkid =  Widget_Base( VecConfigId, col=1 )
  MinSpeedId = CW_FSLIDER( Junkid, $
      DRAG=1, $
      MAXIMUM=37.0000, $
      MINIMUM=0.100000, $
      TITLE='Min Speed', $
      UVALUE='MINSPEED', $
      /EDIT, $
      VALUE=MinSpeed)

  MaxSpeedId = CW_FSLIDER( Junkid, $
      DRAG=1, $
      MAXIMUM=37.0000, $
      MINIMUM=0.100000, $
      TITLE='Max Speed', $
      UVALUE='MAXSPEED', $
      /EDIT, $
      VALUE=MaxSpeed)

  LengthId = CW_FSLIDER( Junkid, $
      DRAG=1, $
      MAXIMUM=5.00000, $
      MINIMUM=0.100000, $
      TITLE='Vector Length', $
      UVALUE='VECLEN', $
      /EDIT, $
      VALUE=Length)

  ThickId = CW_FSLIDER( Junkid, $
      DRAG=1, $
      MAXIMUM=5.00000, $
      MINIMUM=0.100000, $
      TITLE='Vector Thickness', $
      UVALUE='VECTHICK', $
      /EDIT, $
      VALUE=Thick)


  self-> Get,MinMaxSpeed = MinMaxSpeed

  MinSpeedString = string( MinMaxSpeed[0], form='(f7.2)' )
  MaxSpeedString = string( MinMaxSpeed[1], form='(f7.2)' )

  junkid =  widget_Base( VecConfigId, Col=1 )
  JunkLabelid =  Widget_Label(junkid, $
        FONT='-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1',$
        Value='Speed Min/Max in Current Plotting Area')

  CurrentSpeedMinId =  Widget_Label( Junkid,$
        FONT='-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1',$
        Value=MinSpeedString )

  CurrentSpeedMaxId =  Widget_Label( Junkid,$
        FONT='-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1',$
        Value=MaxSpeedString )

  xsize = 300
  ysize = 250
  DrawId = Widget_Draw( junkid, /Button_Events, Xsize=xsize,$
                        ysize=ysize, Retain=2, $
                        Event_PRO='pv_config_drawevents')
  Window,/Free, /pixmap, xsize=xsize, ysize=ysize
  PixId =  !d.Window

    ;---------------------------------------------------------
    ; Decimation 
    ;---------------------------------------------------------
          
  self-> get,Decimate = Decimate_by, CRDecimate_by=CRDecimate_by
  jj=where( CRDecimate_by, njj )
  cwdecimatebutval = ( njj NE 0 )
  
  DecimateBaseId = Widget_Base( TopVecConfigId, Col=2 )
    ; 1st column, CW_Bgroup of 1d /versus 2d decimation
  Decimate_Choices = [ '1D Decimation', '2D Decimation']
  DecimateCwButId = CW_BGroup( DecimateBaseId, $
                               Decimate_Choices,/Exclusive, $
                               Row=2,$
                               /Return_Index, $
                               Event_Funct='DecimateCwBut_Events',$
                               Set_Value=CwDecimateButVal)

    ; 2nd column, the sliders/fields in bulletinboard base
  DecimateSlidersBaseId = Widget_Base( DecimateBaseId,/Map)
  Decimate1dBaseId = Widget_Base(DecimateSlidersBaseId,$
                                 Map=(CwDecimateButVal EQ 0), Row=1)
  Decimate1D = WIDGET_SLIDER(Decimate1DBaseId, $
      DRAG=1, $
      MAXIMUM=10, $
      VALUE=decimate_by,$
      MINIMUM=1, $
      TITLE='Decimate (1D)', $
      UVALUE='DECIMATE_1D')

  Decimate2DBaseId = Widget_Base(DecimateSlidersBaseId,$
                                 map=(CwDecimateButVal NE 0), Row=2)
  Decimate2dCol = Widget_Slider(Decimate2DBaseId,$
                                /DRAG,$
                                Minimum=0, $
                                Value=CRDecimate_BY[0],$
                                Maximum=10, $
                                Title='Columns',$
                               Uvalue='DECIMATE_2D_COLS')

  Decimate2dRow = Widget_Slider(Decimate2DBaseId,$
                                /DRAG,$
                                Minimum=0, $
                                Value=CRDecimate_BY[1],$
                                Maximum=10, $
                                Title='Rows',$
                                Uvalue='DECIMATE_2D_ROWS')

  self-> get, ExcludeCols = ExcludeCols
  Junkid = Widget_Base(TopVecConfigId,Col=1)
  ExcludeColsId = CW_Field(JunkId,$
                           title='Columns To Exclude',$
                           Value=ExcludeCols, $
                           Uvalue='EXCLUDECOLS',$
                          /String, /Return_Events,/Column)
;-------------------------------------------------------------
;
;                Ambiguities 
;
;-------------------------------------------------------------
  AmbigConfigId = WIDGET_BASE(BulletinBoardBase, $
      ROW=2, $
      MAP=0, $
      UVALUE='AmbigConfigId')

  AmbigSelectId = WIDGET_BASE(AmbigConfigId, $
      ROW=1, $
      MAP=1, $
      UVALUE='AmbigSelectId')

  Ambiguities = [ $
    'Selected', $
    'First', $
    'Second', $
    'Third', $
    'Fourth', $
    'Model1']

  AmbigId = CW_BGROUP( AmbigSelectId, Ambiguities, $
      Row=2, $
      NONEXCLUSIVE=1, $
      LABEL_TOP='Ambiguities', $
      UVALUE='AMBIGUITIES', $
      Event_Funct='AmbigSelectBgroup_Events')

  self-> Get,Ambiguities = setAmbiguities
  Widget_control, ambigId, Set_Value=setAmbiguities

  ColorBaseId = WIDGET_BASE(AmbigConfigId, $
      Col=2, $
      MAP=1, $
      UVALUE='COLORBASE')

  self->Get, AmbigColorMapping = AmbigColorMapping, $
   AmbigColorNames=AmbigColorNames

    
    Colors = AmbigColorMapping.Name
  
  SelectedColorId = Cw_ButWL( ColorBaseId, 'Selected ', Colors, $
                              Value=AmbigColorNames[0] )
  FirstColorId    = Cw_ButWL( ColorBaseId, 'First ', Colors, $
                              Value=AmbigColorNames[1])
  SecondColorId   = Cw_ButWL( ColorBaseId, 'Second ', Colors, $
                              Value=AmbigColorNames[2])
  ThirdColorId    = Cw_ButWL( ColorBaseId, 'Third ' , Colors, $
                              Value=AmbigColorNames[3])
  FourthColorId   = Cw_ButWL( ColorBaseId, 'Fourth ', Colors, $
                              Value=AmbigColorNames[4] )
  Model1ColorId   = Cw_ButWL( ColorBaseId, 'Model1 ', Colors, $
                              Value=AmbigColorNames[5])
 

;-------------------------------------------------------------------
;
;                    Data List 
;
;-------------------------------------------------------------------

  PruneDataId = WIDGET_BASE(BulletinBoardBase, $
      COLUMN=1, $
      MAP=0, $
      UVALUE='PruneDataId')

  LABEL96 = WIDGET_LABEL( PruneDataId, $
      FONT='-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1', $
      UVALUE='LABEL96', $
      VALUE='Prune Data List')

    ;Make List of Data Files for Widget_List .
  filelist = pv_Config_MakeDataFileList( self )

; idl 5.1
;  DataListId = WIDGET_LIST( PruneDataId,VALUE=FileList, $
;      UVALUE='DATALIST', $
;      YSIZE=3,/Multiple)

;  idl 5.0.x
  DataListId = WIDGET_LIST( PruneDataId,VALUE=FileList, $
      UVALUE='DATALIST', $
      YSIZE=3)

  DeleteDataEntryId = Widget_Button( PruneDataId, $
                                     Value='Delete This Entry',$
                                     Uvalue='DELETEDATAENTRY')

  ClearListId = Widget_Button( PruneDataId, Value='Clear List',$
                                Uvalue='CLEARLIST')



;-------------------------------------------------------------------
;
;              Title Base
;
;-------------------------------------------------------------------

  AnnotBaseId = WIDGET_BASE(BulletinBoardBase, $
      COLUMN=1, $
      MAP=0, $
      UVALUE='PruneDataId')

  self-> Get,Annotation = Annotation
  Annotation-> Get,MainTitle = mtitle, Xtitle=Xtitle, $
   Ytitle=Ytitle, SubTitle=Subtitle
  MainTitleId = CW_Field(AnnotBaseId, Title = 'Main Title', $
                         Value=Mtitle, /Return_Events, /string,$
                        UValue='MAINTITLE')
  XTitleId = CW_Field(AnnotBaseId, Title = 'X Title', Value=Xtitle, $
                      /Return_Events, /string,$
                        UValue='XTITLE' )
  YTitleId = CW_Field(AnnotBaseId, Title = 'Y Title', Value=ytitle, $
                      /Return_Events, /string,$
                        UValue='YTITLE' )
  SubTitleId = CW_Field(AnnotBaseId, Title = 'SubTitle', Value=Subtitle, $
                        /Return_Events, /string,$
                        UValue='SUBTITLE' )
;-------------------------------------------------------------------
;
;              Buttons
;
;-------------------------------------------------------------------


  BASE102 = WIDGET_BASE(TLB, $
      ROW=1, $
      FRAME=2, $
      MAP=1, $
      UVALUE='BASE102')

  CancelId = WIDGET_BUTTON( BASE102, $
      UVALUE='CANCEL', $
      VALUE='Cancel')

  ApplyId = WIDGET_BUTTON( BASE102, $
      UVALUE='APPLY', $
      VALUE='Apply')

  DismissId = WIDGET_BUTTON( BASE102, $
      UVALUE='DISMISS', $
      VALUE='Dismiss')

  info =  Ptr_New( {$
                    Redraw            : 0l ,$
                    ClearDataListFlag : 0l    ,$
                    bases             :  [MapDimsId, TopVecConfigId, $       
                                            AmbigConfigId, PruneDataId,$
                                         AnnotBaseId],$   
;------------------  Map dims Base -------------------------
                    LonMinId          : LonMinId,$                        
                    LonMaxId          : LonMaxId ,$                       
                    LatMinId          : LatMinId,$                        
                    LatMaxId          : LatMaxId ,$                       
                    PrevDimsId        : PrevDimsId,$                      
                    PrevDimsIndex     : -1L       ,$                      
                    GoToDimId         : GoToDimId,$                       
                    DeleteDimId       : DeleteDimId,$                     
                    ClearDimsListId   : ClearDimsListId, $  

;----------------   Vector Config Base -------------------------              
                    MinSpeedId        : MinSpeedId,$                       
                    MaxSpeedId        : MaxSpeedId,$                       
                    LengthId          : LengthID,$                         
                    ThickId           : ThickId,$                          
                    DataListId        : DataListId,$ 
                    CurrentSpeedMinId : CurrentSpeedMinId ,$
                    CurrentSpeedMaxId : CurrentSpeedMaxId ,$
                    DrawId            : DrawId,$
                    Wid               : 0L    ,$ ; won't know until realization
                    PixId             : PixId ,$
                    ColorIndex        : -1L,$
                    Button            : '' ,$
                    yr                : fltarr(2), $
                    xsize             : xsize,$
                    ysize             : ysize,$
                    DecimateCwButId   : DecimateCwButId,$
                    Decimate1DBaseId  : Decimate1DBaseId,$
                    Decimate1D        : Decimate1D,$
                    Decimate2DBaseId  : Decimate2DBaseId,$
                    Decimate2DCol     : Decimate2DCol,$
                    Decimate2DRow     : Decimate2DRow,$
                    ExcludeColsId     : ExcludeColsId,$

; --------------   Data List Base -------------------------


                    DataListIndex     : Ptr_New() ,$ ; array of indices.   
                    DeleteDataEntryId : DeleteDataEntryId, $               
                    ClearListId       : ClearListId,$                      
                    AmbigId           : AmbigId ,$                         
                    SelectedColorId   : SelectedColorId,$    
                    FirstColorId      : FirstColorId  ,$    
                    SecondColorId     : SecondColorId,$     
                    ThirdColorId      : ThirdColorId ,$     
                    FourthColorId     : FourthColorId,$     
                    Model1ColorId     : Model1ColorId,$     
                    ApplyId           : ApplyId,$            
                    DismissId         : DismissId,$          
                    CancelId          : CancelId ,$          
                    AmbigArray        : [1,0,0,0,0,0], $       
; ------------------- Titles Base ------------------------------
                    MainTitleId       : MainTitleId,$
                    XtitleId          : XtitleId,$
                    YTitleId          : YTitleId,$
                    SubTitleId        : SubTitleId,$
                    tlb               : Group } )            
                    


  WIDGET_CONTROL, TLB, /REALIZE, set_uvalue=info
  Widget_Control, (*info).DrawId, Get_Value=wid
  (*info).wid = wid
  
  XMANAGER, 'pv_config', TLB, $
    event_handler='pv_config_events', $
      cleanup='Pv_Config_Cleanup'
END
