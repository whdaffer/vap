;+
; NAME:  
; $Id$
; PURPOSE:  
;
;
; AUTHOR:
;
;
; CATEGORY:  
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
; Revision 1.1  1998/10/30 22:14:28  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;===========================
;
;===========================
FUNCTION cloud_overlay_config_histo_events, event
  retevent = 0
  ;  print,'pv_draw_events'
  Widget_Control, event.top, get_uval=info
  ButtonTypes = ['LEFT', 'MIDDLE', 'RIGHT' ]
  EventTypes =  [ 'DOWN','UP','MOTION','SCROLL']
  Button = ButtonTypes[ (event.press AND 7)/2 ]
  IF EventTypes( event.type ) NE 'DOWN' THEN return,0
  ;  print,'press = ',event.press
  (*info).xs = event.x
  (*info).ys = event.y
  Widget_Control, event.top, Set_Uvalue=info
  Widget_Control, Event.id, /draw_motion_events, $
   Event_pro='cloud_overlay_config_motion_events'
  return, retevent
END

;===========================
;
;===========================
PRO cloud_overlay_config_motion_events, event
  Widget_control, event.top, Get_Uvalue=info
  EventTypes =  [ 'DOWN','UP','MOTION','SCROLL']
  WSET, (*info).histowid
  x =  0>  event.x < !d.X_size-1
  y =  0>  event.y < !d.Y_Size-1
  junk = Convert_Coord( x,y,/device,/to_data)
  newcutoff = fix(junk[0])
  Widget_Control, (*info).cutoffId, Set_Value=newcutoff

  IF EventTypes( event.type ) EQ 'UP' THEN BEGIN 
    Device, Set_Graphics_Function=3, copy=[0,0,300,200,0,0,(*info).pixid]
    Widget_Control, event.id, Draw_Motion_Events=0, $
     event_func='CLOUD_OVERLAY_CONFIG_HISTO_EVENTS'
    (*info).cutoff = newcutoff
  ENDIF 


  Device, set_graphics_funct=3
  Device, copy=[0,0,!d.X_size, !d.Y_Size, 0,0, (*info).pixid ]

  (*info).xd = x
  (*info).yd = y 
  
  Device,set_graphics_function=6 ; xor
  Plots,[1,1]*newcutoff, !y.crange,color=!d.n_colors-1
  Device,set_graphics_function=3 ; copy

  Widget_Control, event.top, Set_Uvalue=info
   
END


;===========================
;
;===========================
PRO Cloud_overlay_config_histo_draw, info
   IF Ptr_Valid( (*info).dataPtr) THEN BEGIN 
     wset, (*info).histowid
     plot,histogram( *(*info).dataPtr,min=0,max=1024),psym=10
     cloud_overlay_config_copyToPixmap,info
     plots,(*info).cutoff*[1,1], !y.crange,color=!d.n_colors-1
   ENDIF 
END

;===========================
;
;===========================

PRO cloud_overlay_config_copyToPixmap,info
     wset, (*info).pixid
     device, copy=[0,0,300,200,0,0,(*info).histowid]
     wset, (*info).histowid     
END

;===========================
;
;===========================

PRO cloud_overlay_config_setStatus,labelid, label
   IF VarType(label) NE 'STRING' THEN label = string(label)
   Widget_Control, LabelId, Set_Value=label
END

;===========================
;
;===========================
FUNCTION cloud_overlay_cutoff_events, event
   retevent = 0
   Widget_Control, event.top, Get_Uvalue=info
   Widget_Control, event.id, Get_Value=val
   (*info).cutoff = val
   Cloud_overlay_config_histo_draw, info
   Widget_Control, event.top, Set_Uvalue=info   
   return, retevent
END
FUNCTION cloud_overlay_config_compute_image,CloudMin, CloudMax, $
                                            cutoff,dataPtr,$
                                            limits,watermin, watermax, $
                                            sx,sy

     data = *dataPtr
     MAP_SET, 0, avg(limits[ [0,2] ] ), $
      limit=limits[ [1,0,3,2] ], /grid,/lab,/cont

     junk = MAP_IMAGE( data, sx,sy, lonmin=limits[0],$
                           latmin=limits[1], lonmax=limits[2],$
                           latmax=limits[3], /bilinear, /compress )
     newimage = hist_equal( junk, min=cutoff,max=1024,$
                            top=cloudmax-1)
     water = newimage*0b+watermin
     newimage = newimage OR water
     x = where(newimage gt watermax AND newimage LT cloudmin, nx )
     IF nx NE 0 THEN newimage(x) =  bytscl(newimage[x], top=watermax-1)+1
  return,newimage

END

;===========================
;
;===========================

PRO Cloud_Overlay_Config_Draw, info

   CloudMin = (*info).CloudMin
   CloudMax = (*info).CloudMax
   WaterMin = (*info).WaterMin
   WaterMax = (*info).WaterMax
   cutoff =  (*info).cutoff
   dataPtr =  (*info).dataPtr
   limits = (*info).limits
   IF Ptr_Valid(dataPtr) THEN BEGIN 
     Cloud_overlay_config_setstatus, (*info).TextId, 'Calculating new image'
     Widget_Control,/hourglass
     wset, (*info).Wid
     newimage = cloud_overlay_config_compute_image(CloudMin, CloudMax, $
                                            cutoff,dataPtr,$
                                            limits,watermin, watermax, $
                                            sx,sy)
     Tv,newimage,sx,sy
     IF ptr_Valid( (*info).imagePtr) THEN $
       *((*info).imagePtr) = newimage $
     ELSE $
       (*info).imagePtr = Ptr_New(newimage,/no_copy)
     (*info).sx = sx
     (*info).sy = sy
     Cloud_overlay_config_setstatus, (*info).TextId, 'Done!'
   ENDIF ELSE $
     Message,'No Valid Data!',/cont

END


;===========================
;
;===========================

PRO cloud_overlay_config_events, event
   Widget_Control, event.top, Get_Uvalue=info
   Widget_Control, event.id, Get_Uvalue=descript
   CASE descript OF 
     'FILEOPEN': BEGIN 
       F = dialog_pickfile()
       IF strlen(f[0]) NE 0 THEN BEGIN 
         Cloud_overlay_config_setstatus, (*info).TextId, $
          'Reading file ' + f[0]
         (*info).filename = f[0]
         Read_PCGoes, f[0], limits, data, hdr=hdr
         sz = size( data,/Dimensions )
         IF N_Elements(Limits) NE 4 THEN $
           Limits = hdr.limits
          sensornum = fix( getChar( strcompress( $
                                    strtrim( hdr.type,2),/remove_all),/last) )
          ir =  sensornum GT 1
          IF ir THEN BEGIN 
            data = 1024-data
            Message, 'IR data, reversing data',/cont
          ENDIF 
          moments = moment(data)
          ;a = moments[0]
          ;s = sqrt(moments[1])
         (*info).dataPtr = data
         Cloud_Overlay_Config_Draw, info
       ENDIF 
     END
     'FILEQUIT': BEGIN 
       Widget_Control, (*info).CutoffId, Get_Value=v
       (*info).Cutoff = v[0]
       Widget_Control,event.top,/destroy
     END
     'APPLY': begin 
       Widget_Control, (*info).CutoffId, Get_Value=v
       (*info).Cutoff = v[0]
        Cloud_overlay_config_setstatus, (*info).TextId, $
         'Setting Cutoff to ' + $
           strtrim( (*info).cutoff,2)
       Cloud_overlay_config_draw,info
     END
     'CANCEL': BEGIN 
       Widget_Control,event.top,/destroy
     END
     'DISMISS': BEGIN 
       Widget_Control, (*info).CutoffId, Get_Value=v
       (*info).Cutoff = v[0]
       Widget_Control,event.top,/destroy
     END
     ELSE: BEGIN 
       Message,'Unknown id',/cont
     end
   ENDCASE 
END

;===========================
;
;===========================

FUNCTION cloud_overlay_config, $
                          cloud_file, $ ; 'grid_goes' output file
                          data=data, $
                          WaterMin= watermin, $
                          WaterMax= watermax, $
                          CloudMin= Cloudmin, $
                          CloudMax= Cloudmax, $
                          Xsize=Xsize, $
                          Ysize=Ysize, $
                          Limits=Limits,$
                          group_leader = group_leader ; =0 if not present
  restore = 0
  IF !d.name NE 'X' THEN BEGIN 
    restore = 1
    genv,/save
    set_plot,'x'
  ENDIF 

  IR =  keyword_set(IR)
  IF n_Elements(group_leader) EQ 0 THEN group_leader = 0
  IF N_elements(Xisze) EQ 0 THEN Xsize = 640
  IF N_elements(Yisze) EQ 0 THEN Ysize = 512

  IF !d.n_colors EQ 256 THEN BEGIN 
    Window,/free, /pixmap
    Wdelete, !d.window
  ENDIF 
  IF N_Elements(WaterMin) EQ 0 THEN WaterMin = 1b
  IF N_Elements(WaterMax) EQ 0 THEN WaterMax = 11b
  IF N_Elements(CloudMin) EQ 0 THEN CloudMin = 33b
  IF N_Elements(CloudMax) EQ 0 THEN CloudMax = 72b

  watermin = byte(watermin)
  waterMax = byte(waterMax)

  CloudMin = byte(CloudMin)
  CloudMax = byte(CloudMax)

  tlb = Widget_Base(col=1, /map, mbar=menu_bar_id, $
                    group_leader=group_leader, $
                   Title='Cloud Overlay Configurator')
  fileButtonId = Widget_Button( menu_bar_id, /menu, Value='File')
  FileId = Widget_Button( fileButtonId, Value='Open',Uvalue='FILEOPEN')
  QuitId = Widget_Button( fileButtonId, VaLUE='Quit', Uvalue='FILEQUIT')
  
  base2 = Widget_Base( tlb,col=2, /map,frame=2)
  HistoDrawId = Widget_Draw( base2, xsize=300, ysize=200, $
                             retain=2,/Button_events, $
                             event_func='Cloud_Overlay_Config_Histo_Events')

  base3 = Widget_Base( base2, /Col,/map)
  TextId = Widget_Text( base3, xsize=30, ysize=3, $
                        Value='Status:',/Wrap,/Scroll )
  CutoffId = Widget_Slider( base3, Min=0, $
                            max=1024, Value=512, Uvalue="CUTOFF", $
                          Event_func='Cloud_Overlay_Cutoff_events',$
                          Title='Cloud Image Cutoff')
  
  DrawId = Widget_Draw( tlb, Xsize=Xsize, ysize=ysize, retain=2)
;                        /app_scroll, $
;                        x_scroll_size=512, y_scroll_size=480, $
;                        Button_Events )
  
  base3 = Widget_Base(tlb, /row, /map, frame=2)
  ApplyId = Widget_Button( base3, Value="Apply", Uvalue='APPLY')
  DismissId = Widget_Button( base3, Value="Dismiss", Uvalue='DISMISS')
  CancelId = Widget_Button( base3, Value="Cancel", Uvalue='CANCEL')


  IF N_Elements(cloud_file) NE 0 THEN BEGIN 
    Cloud_overlay_config_setstatus, TextId, 'Reading file ' + cloud_file
    Read_PCGoes, cloud_file, limits, data, hdr=hdr
    sz = size( data, /dimensions )
    IF N_Elements(Limits) NE 4 THEN $
      Limits = hdr.limits
    sensornum = fix( getChar( strcompress( $
                   strtrim( hdr.type,2),/remove_all),/last) )
    ir =  sensornum GT 1
    IF ir THEN BEGIN 
      data = 1024-data
      Message, 'IR data, reversing data',/cont
    ENDIF 
    moments = moment(data)
    a = moments[0]
    s = sqrt(moments[1])
    slidermin = min(data)
    slidermax=max(data)
    dataPtr = Ptr_New(data,/no_copy)
    cutoff = a-s
    
  ENDIF ELSE BEGIN 
    IF n_elements(data) NE 0 THEN BEGIN 
      IF N_Elements(Limits) NE 4 THEN BEGIN 
        Message,$
          "Keyword 'limits' is REQUIRED when passing data by keyword!'",$
         /cont
        IF restore THEN genv,/restore
        return,0
      ENDIF 
      moments = moment(data)
      a = moments[0]
      s = sqrt(moments[1])
      slidermin = min(data)
      slidermax=max(data)
      dataPtr = Ptr_New(data) 
      cutoff = a-s

    ENDIF ELSE BEGIN 
      Message,"Assuming you'll read data using the 'file' menu!",/cont
      cutoff = 1024/2.
      slidermin = 0
      slidermax=1024
      dataPtr = Ptr_New();
    ENDELSE 
  ENDELSE 

  Widget_Control, tlb, /realize
  Widget_Control, HistoDrawId, Get_Value=histowid
  Widget_Control, DrawId, Get_Value=wid

  Window, /free, /pixmap, Xsize=300, ysize=200
  Pixid = !d.Window

  x = where(Limits LT 0, nx )
  IF nx NE 0 THEN limits[x] = limits[x]+360.


  loncent = Mean( Limits[ [0,2] ] )
  info =  Ptr_New( { $
                    tlb         : tlb,$
                    FileId      : FileId,$
                    QuitId      : QuitId,$
                    HistoDrawId : HistoDrawId,$
                    HistoWid    : HistoWid,$
                    TextId      : TextId ,$
                    CutoffId    : CutoffId,$
                    DrawId      : DrawId,$
                    PixId       : PixId,$
                    WId         : wid,$
                    ApplyId     : ApplyId,$   
                    DismissId   : DismissId,$ 
                    CancelId    : CancelId, $ 
                    filename    : '',$
                    DataPtr     : DataPtr,$
                    imagePtr    : Ptr_new(),$
                    CloudMin    : CloudMin,$
                    CloudMax    : CloudMax,$
                    WaterMin    : WaterMin,$
                    WaterMax    : WaterMax,$
                    Limits      : Limits, $
                    cutoff      : cutoff, $
                    xs          : 0l,$   
                    ys          : 0l, $  
                    xd          : 0l,$   
                    yd          : 0l, $
                   sx: 0L,$
                   sy: 0l } )
  
  

  Widget_Control, (*info).CutoffId, Set_Value=cutoff
  Widget_Control, tlb, Set_Uvalue=info
  Cloud_Overlay_Config_Histo_Draw, info
  Cloud_Overlay_Config_Draw, info
  Xmanager, 'CLOUD_OVERLAY_CONFIG', tlb, $
     event_handler='Cloud_overlay_config_events'

  retinfo = { cutoff: (*info).cutoff, limits:(*info).limits, $
              dataPtr: (*info).dataPtr, imagePtr: (*info).imagePtr,$
            sx: (*info).sx, sy: (*info).sy , cloudMin: (*info).cloudMin,$
            CloudMax: (*info).CloudMax, WaterMin: (*info).WaterMin, $
            WaterMax: (*info).WaterMax}
;  Ptr_Free, (*info).DataPtr
;  Ptr_Free, (*info).imagePtr
  Ptr_Free, info
  IF restore THEN genv,/restore
  return, retinfo
END
