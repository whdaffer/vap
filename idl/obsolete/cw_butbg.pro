PRO cw_ButBG_Cleanup, id
   Widget_Control, id, get_Uvalue=info
   ptr_free, info
END

FUNCTION CW_Butbg_events, event
   retevent = 0
   Widget_Control, Widget_Info( Event.handler, /child ), get_uvalue=info
   Top_Descript = (*info).top
   BGroup_Descripts =  (*info).Bgroup_Descripts

   CASE Event.id OF 
     (*info).ButtonId: BEGIN
       wait,0.1
       Widget_Control,(*Info).ButtonBase,map=0
       Widget_Control, (*info).BGroupBase,/map
     end
     (*info).BGroupID: BEGIN 
       wait,0.1
       top = Top_Descript + $
                '<' + Bgroup_Descripts[event.value] + $
                '>'
       Widget_Control, (*info).ButtonId, Set_Value=top
       Widget_Control, (*info).BGroupBase,map=0
       Widget_Control, (*Info).ButtonBase,/map
     END 
     ELSE:
   ENDCASE 
   return, retevent
END


;FUNCTION CW_Butbg_Bgroup_Get
  
;  return,value
;END


;FUNCTION CW_Butbg_Top_Get
;  return,value
;END


;PRO CW_Butbg_Bgroup_Set
;  return,value
;END


;PRO CW_Butbg_Top_Set
;  return,value
;END


;-----------------------------------------------
; Definition Routine
;-----------------------------------------------

FUNCTION CW_Butbg, parent, $
                           Value=Value, $
                           Uvalue=Uvalue

   IF N_Params() EQ 0 THEN BEGIN 
     ok = Dialog_Message("Usage, id=Cw_Butbg(parentid)")
     return, 0
   ENDIF 
   IF N_Elements(Value) LT 2 THEN BEGIN 
     Ok = Dialog_Message("Need at least 2 fields in the 'Value' keyword")
     return,0
   ENDIF 
   IF N_Elements(Uvalue) EQ 0 THEN Uvalue = ''
   
   IF vartype( Value )  NE 'STRING' THEN Value = String(Value)
   Top = Value[0]
   Bgroup_descripts = Value[1: N_Elements(Value)-1 ]

   junk =  top +'<' + BGroup_Descripts + ">"
   MaxLen =  max( strlen(Junk) )
   CW_Tlb = Widget_Base(parent,Uvalue=Uvalue, $
                        Event_func='cw_butbg_events',$
                        /map, Col=1)

   ButtonBase = Widget_Base( CW_Tlb, /map )
   buttonid = Widget_Button( ButtonBase, $
;                             Pro_Set_Value='CW_Butbg_Top_Set',$
;                             Pro_Get_Value='CW_Butbg_Top_Get',$
                             Value=Top,$
                              xsize=(MaxLen+5)*!D.X_Ch_Size)

   BgroupBase = Widget_Base( CW_Tlb, map=0)
   BgroupId =  Cw_BGroup( BgroupBase, Bgroup_Descripts, $
                          /Exclusive, $
;                          Pro_Set_Value='CW_Butbg_BGroup_Set',$
;                          Pro_Get_Value='CW_Butbg_BGroup_Get',$
                          /Return_Index)

   info = Ptr_New( { Parent: parent,$
                     Cw_Tlb           : Cw_Tlb,$      
                     ButtonBase       : ButtonBase,$  
                     ButtonId         : ButtonId,$    
                     BgroupBase       : BgroupBase,$  
                     BgroupId         : BgroupId, $   
                     Top              : Top,      $
                     BGroup_Descripts : Bgroup_Descripts } )

   Widget_Control, ButtonBase, set_uval=info
  
END

