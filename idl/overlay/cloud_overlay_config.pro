;+
; NAME:  
; $Id$
; PURPOSE:  
;
; AUTHOR:
;
; CATEGORY:  
;
; CALLING SEQUENCE:  
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
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
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.2  1998/11/04 19:40:12  vapuser
; Put in code to handle non IR data.
;
; Revision 1.1  1998/10/30 22:14:28  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
PRO CLOUD_OVERLAY_CONFIG_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  Widget_Control, event.handler, get_uvalue=info

  CASE ev OF 
    'CANCEL': BEGIN 
      (*info).cancelflag = 1l
      widget_control, event.handler, set_uvalue=info
      widget_control, event.handler, /destroy
      return
    END
  
    'DISMISS': BEGIN
      widget_control, (*info).BrightMinId, get_Value=brightmin
      widget_control, (*info).BrightMaxId, get_Value=brightmax
      widget_control, (*info).SatMinId, get_Value=satmin
      widget_control, (*info).SatMaxId, get_Value=satmax

      IF brightmin GT brightmax THEN BEGIN 
        Message,'Illegal BRIGHT values, min>max. Min set!'
        brightmin =  brightmax-1
      ENDIF 

      IF satmin GT satmax THEN BEGIN 
        Message,'Illegal SAT values, min>max. Min set!'
        satmin =  satmax-1
      ENDIF 

      (*info).BrightMin = brightmin
      (*info).BrightMax = brightmax
      (*info).SatMin = satmin
      (*info).SatMax = satmax
      
      Widget_Control, event.handler,/destroy

    END 

    'APPLY': BEGIN 

      widget_control, (*info).BrightMinId, get_Value=brightmin
      widget_control, (*info).BrightMaxId, get_Value=brightmax
      widget_control, (*info).SatMinId, get_Value=satmin
      widget_control, (*info).SatMaxId, get_Value=satmax

      IF brightmin GE  brightmax THEN BEGIN 
        Message,'Illegal BRIGHT values, min>max. Min set!'
        brightmin =  brightmax-1
      ENDIF 

      IF satmin GE  satmax THEN BEGIN 
        Message,'Illegal SAT values, min>max. Min set!'
        satmin =  satmax-1
      ENDIF 

      cloudmask =  *((*info).cloudmask)
      landwaterIm =  *((*info).landWaterIm)

      (*info).BrightMin = brightmin
      (*info).BrightMax = brightmax
      (*info).SatMin = satmin
      (*info).SatMax = satmax

      cloud_overlay_config_drawimage, info

    END 
    ELSE:
  ENDCASE 
    

END


PRO cloud_overlay_config_drawimage, info

   Message,"Calculating new image! Please have patience!",/info
   Print,"Using Values ..." 
     Print,"  Bright Min/Max: ", (*info).Brightmin, (*info).BrightMax 
     Print,"  Sat Min/Max: ", (*info).SatMin, (*info).SatMax
   widget_control, /hourglass
   widget_control, (*info).tlb,sensitive=0
        ; Define the new Brightness/Saturation mappings
      xx=findgen(100)/99.

      bi = 0> interpol( [0.,1], [(*info).BrightMin, (*info).BrightMax], xx ) < 1
      si = 0> interpol( [1.,0], [(*info).SatMin,    (*info).SatMax],xx ) < 1

        ; Use 'cloudmask' to create new Brightness/Saturation values

      cloudmask =  *((*info).cloudmask)
      landwaterIm =  *((*info).landWaterIm)

      b2=bi[cloudmask]
      s2=si[cloudmask]


        ; Substitute these new Brightness/Saturation values in for those in
        ; mapIm and convert back to RGB 

      Color_Convert, landWaterIm,b2,s2, imr, img, imb, /hls_rgb
      b2 = 0
      s2 = 0
      Im = [ [[temporary(imr)]], [[temporary(img)]], [[temporary(imb)]] ]

      Wset, (*info).OverlayDraw
      Map_set, /grid,/lab,/cont,limi=[(*info).latmin,$
                                      (*info).lonmin,$
                                      (*info).latmax,$
                                      (*info).lonmax]

      FOR i=0,2 DO BEGIN 
        tmpIm = Map_Image( Im[*,*,i], $
                           xs,ys,xsize,ysize,$
                           lonmin= (*info).lonmin, $
                           latmin= (*info).latmin, $
                           lonmax= (*info).lonmax, $
                           latmax= (*info).latmax, $
                           /compress, /bilinear )


        IF i EQ 0 THEN BEGIN 
          dim = size(tmpIm,/dim)
          mapIm = bytarr(dim[0], dim[1], 3)
        ENDIF 
        mapIm[*,*,i] =  temporary(tmpIm)
      ENDFOR 

      Message,"Done",/info
      
      Tv,mapIm,xs,ys,true=3
      widget_control, (*info).tlb,sensitive=1
       
END


PRO cloud_overlay_config, LandWaterIm = LandWaterIm, $
                          cloudmask   = cloudMask, $
                          brightmin   = brightmin, $
                          brightmax   = brightmax,$ 
                          satmin      = satmin,$ 
                          satmax      = satmax,$ 
                          lonmin      = lonmin,$
                          lonmax      = lonmax,$
                          latmin      = latmin,$
                          latmax      = latmax,$ 
                          group       = group

  genv,/save
  IF !d.name NE 'X' THEN set_plot,'X'

  IF n_elements(lonmin)*n_elements(latmin)*$
     n_elements(lonmax)*n_elements(latmax) EQ 0 THEN BEGIN 
    Message,"One of LONMIN,LONMAX,LATMIN,LATMAX is undefined. All are required',/cont
    return
  ENDIF 


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0
  IF n_elements(brightmin) EQ 0 THEN brightmin = 0.0
  IF n_elements(brightmax) EQ 0 THEN brightmax = 0.8

  IF n_elements(satmin) EQ 0 THEN satmin = 0.0
  IF n_elements(satmax) EQ 0 THEN satmax = 0.55

  brightmin = 0> brightmin < 1.0
  brightmax =  brightmin+0.001> brightmax <  1.0

  satmin = 0> satmin < 1.0
  satmax =  satmin+0.001> satmax <  1.0

  IF abs(brightmax-brightmin) LE 0.1 THEN BEGIN 
    Message,'Bright values too close (<0.1). Try again',/cont
    return
  ENDIF 


  IF abs(satmax-satmin) LE 0.1 THEN BEGIN 
    Message,'Sat values too close (<0.1). Try again',/cont
    return
  ENDIF 

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  base1 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=2, $
      MAP=1, $
      TITLE='Overlay Configurator', $
      UVALUE='BASE1')

  BASE2 = WIDGET_BASE(BASE1, $
      COLUMN=2, $
      MAP=1, $
      UVALUE='BASE2')

  HistoID = WIDGET_DRAW( BASE2, $
      BUTTON_EVENTS=1, $
      MOTION_EVENTS=1, $
      RETAIN=2, $
      UVALUE='HistoDraw', $
      XSIZE=300, $
      YSIZE=200)

  BASE4 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE4')

  BrightMinId = CW_FSLIDER( BASE4, $
      DRAG=1, $
      MAXIMUM=brightmax-0.1, $
      MINIMUM=0.00000, $
      TITLE='Bright Min', $
      UVALUE='BRIGHTMIN', $
      VALUE=brightmin, /edit)

  BrightMaxId = CW_FSLIDER( BASE4, $
      DRAG=1, $
      MAXIMUM=1.00000, $
      MINIMUM=brightmin+0.1, $
      TITLE='Bright Max', $
      UVALUE='BRIGHTMAX', $
      VALUE=brightmax, /edit)

  SatMinId = CW_FSLIDER( BASE4, $
      DRAG=1, $
      MAXIMUM=satmax-0.1, $
      MINIMUM=0.00000, $
      TITLE='Saturation Min', $
      UVALUE='SATMIN', $
      VALUE=satmin,/edit)

  SatMaxId = CW_FSLIDER( BASE4, $
      DRAG=1, $
      MAXIMUM=1.00000, $
      MINIMUM=satmin+0.1, $
      TITLE='Saturation Max', $
      UVALUE='SATMAX', $
      VALUE=satmax,/edit)



  BASE10 = WIDGET_BASE(BASE1, $
      COLUMN=2, $
      MAP=1, $
      UVALUE='BASE10')

  OverlayId = WIDGET_DRAW( BASE10, $
      RETAIN=2, $
      UVALUE='OVERLAY', $
      XSIZE=400, $
      YSIZE=300, $
      X_SCROLL_SIZE=300, $
      Y_SCROLL_SIZE=200)

  BASE12 = WIDGET_BASE(BASE10, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE12')

  ApplyId = WIDGET_BUTTON( BASE12, $
      UVALUE='APPLY', $
      VALUE='Apply')

  DismissId = WIDGET_BUTTON( BASE12, $
      UVALUE='DISMISS', $
      VALUE='Dismiss')

  CancelId = WIDGET_BUTTON( BASE12, $
      UVALUE='CANCEL', $
      VALUE='Cancel')


  WIDGET_CONTROL, BASE1, /REALIZE

  ; Get drawable window index

  COMMON DRAW3_Comm, DRAW3_Id
  WIDGET_CONTROL, HistoId, GET_VALUE=histoDraw

  ; Get drawable window index

  COMMON DRAW11_Comm, DRAW11_Id
  WIDGET_CONTROL, OverlayId, GET_VALUE=OverlayDraw

  dim = size(cloudmask,/dim)
  newx = dim[0]/2
  newy = dim[1]/2

  IF newx*2 EQ dim[0] AND newy*2 EQ dim[1] THEN BEGIN 
     tcloudmask = rebin(cloudmask,newx,newy)
     tlandwaterIm = rebin(landwaterIm,newx,newy)
  ENDIF ELSE BEGIN 
    tcloudmask = congrid(cloudmask,newx,newy)
    tlandwaterim = congrid(cloudmask,newx,newy)
  ENDELSE 
  info = ptr_new( { tlb:         base1, $
                    histoId:     histoid, $
                    overlayId:   overlayId, $
                    BrightMinid: BrightMinid, $
                    BrightMaxId: BrightMaxId, $
                    SatMinid:    SatMinid, $
                    SatMaxId:    SatMaxId, $
                    ApplyId:     ApplyId, $
                    DismissId:   DismissId, $
                    CancelId:    CancelId, $
                    Cancelflag:  0l, $
                    HistoDraw:   HistoDraw, $
                    OverlayDraw: OverlayDraw,$
                    brightmin:  brightmin, $
                    brightmax:  brightmax, $
                    satmin:  satmin, $
                    satmax:  satmax, $
                    lonmin: lonmin, $
                    lonmax: lonmax, $
                    latmin : latmin, $
                    latmax:  latmax, $
                    histoData:   0l, $
                    cloudmask:   Ptr_new( tcloudmask,/no_copy), $
                    landwaterim: Ptr_new( tLandWaterIm,/no_copy) } )

  Wset, OverlayDraw
  cloud_overlay_config_drawimage,info

  
  Widget_control, base1, set_uval=info
  XMANAGER, 'CLOUD_OVERLAY_CONFIG', base1, $
    event_handler='cloud_overlay_config_event'

  IF NOT (*info).cancelflag THEN BEGIN 
    BrightMin = (*info).BrightMin
    BrightMax = (*info).BrightMax
    SatMin = (*info).SatMin
    SatMax = (*info).SatMax
    print,'returning, ', brightmin,brightmax,satmin,satmax
  ENDIF 

  Ptr_Free, (*info).cloudmask, (*info).LandWaterIm, info

  genv,/restore

END
