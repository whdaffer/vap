;+
; NAME:  pv_overlay
; $Id$
; PURPOSE:  overlay goes/gms data in PV
;
; AUTHOR:  William Daffer
;
; CATEGORY:  PV
;
; CALLING SEQUENCE:  Called from within PV
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
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;=====================================================
;  pv_overlay_events: Main Event routine
;=====================================================

pv_overlay_events, events
END

;=====================================================
;  pv_overlay: Definition
;=====================================================


PRO pv_overlay, group=group

   Catch, error
   IF error NE 0 THEN BEGIN 
     Message,!error_state.msg,/cont
     return
   ENDIF 

   IF n_elements(group) EQ 0 THEN BEGIN 
     Message,'Usage, pv_overlay, group=Tlb_of_caller',/cont
     return
   ENDIF 

   Widget_Control, group, Get_Uvalue=self

   tlb = Widget_Base( group, /map,/col, Title='PV Overlay Configurator',$
                      mbar=MenuId)
   junk = Widget_Button( MenuId, Value='Cloud File',Uvalue='CLOUDFILE',/menu)
   AreaId = Widget_Button( junk, Value='Pick Area File',UValue='AREAFILE')
   GridId = Widget_Button( junk, Value='Pick Gridded File',UValue='GRIDFILE')
   MakeFinalId = Widget_Button( junk, Value='Make Final Gridded File',$
                                UValue='FINALGRIDFILE')
   ; junk = Widget_Base( tlb, /map, frame=2, title='Cloud Overlay Type')
   type = ['East Pacific (Goes 10)', 'West Atlantic (Goes 8)',/EXCLUSIVE ]
   TypeBGroupId = CW_Bgroup(tlb, type, /row, frame=2, $
                            title='Cloud Overlay Type',$
                           set_value=0)
   Sensors = ['Visible','Ir2','Ir3','Ir4']
   SensorBGroupId =  CW_Bgroup( tlb, Sensors, row=2,frame=2,$
                                title='Sensor',$
                                set_value=3 )
   DrawId = Widget_Draw( tlb, Xsize=640, ysize=512 )
   junk = Widget_Base( tlb, /row, frame=2)
   CancelId = Widget_Button( junk, value='Cancel', Uvalue='CANCEL' )
   DoneId = Widget_Button( junk, value='Done', Uvalue='DONE' )
   Widget_Control, tlb, /realize
   Widget_Control, DrawId, Get_Uvalue=Wid
   Widget_Control, tlb, Set_Uvalue=Ptr_New({ tlb: tlb, $
                                             AreaId: AreaId,$
                                             GridId: GridId,$
                                             MakeFinalId: MakeFinalId,$
                                             TypeBGroupId: TypeBGroupId,$
                                             SensorBgroupId: SensorBgroupId,$
                                             DrawId: DrawId,$
                                             Wid:Wid,$
                                             CancelId: CancelId,$
                                             DoneId: DoneId } )

END

;=====================================================
;  pv_overlay
;=====================================================
