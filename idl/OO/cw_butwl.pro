;+
; NAME: CW_BUTWL.PRO
; $Id$
;
;
; PURPOSE:  Creates a Widget which consists of a button and a
;          list. The button widget starts out mapped and is unmapped
;          when the button in pressed, revealing the list widget. The
;          user then  chooses an element from the list, whereupon the
;          list is unmapped and  the button mapped. The label of the
;          button changes to reflect the  user's choice from the
;          list.


;
;
; Author : William Daffer
;
;
; DATE:  Sometime in July.
; 
;
; CATEGORY:  Compound Widget
;
;
;
; CALLING SEQUENCE:  id=cw_butwl( topid, toplabel, list, [value=value,
;                                      uvalue=uvalue]] )
;
;
; 
; INPUTS:  
;
;    Topid    - Widget_ID of base in which this widget resides.
;    TopLabel - Label of button.
;    List     - The list for the widget list.
;
;
; OPTIONAL INPUTS: 
;
;
;	
; KEYWORD PARAMETERS: 
;
;    Value  - The 'value' of the widget, namely the choice from the
;          list widget. Equals the empty string by default. If
;          present, it must be an element of 'list'
;    Uvalue - The Uvalue of the widget. (CW_BUTWL by default)
;
;
;
;
; OUTPUTS: The ID of this Compound Widget.
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
;    Create base for which this widget will be a child.
;  topid=widget_base()
;     create but/wl
;  cwbutwlid = cw_butwl( id, 'TopLabel',
;          List=['Item1','Item2','Item3'], Value='Item2')
;
;  widget_control, topid,/realize
;  
;    To Get the 'Value' of the widget
;  Widget_control, cwbutwlid, Get_Value=Value
;
;   Note: Value will be one of 'Item1', 'Item2' or 'Item3'
;
;    To Set the 'Value' of the widget
;  Widget_control, cwbutwlid, set_Value=Value
;
;  Note: Value must be a member of the list .
;         (one of ['Item1','Item2','Item3']  in this case)
;
;
;
; MODIFICATION HISTORY: 
;
; $Log$
;
;-

;------------------------------------
; Main event processor
;------------------------------------

FUNCTION CW_ButWL_events, event
   retevent = 0
   tlb = Widget_Info( Event.handler, /child )
   Widget_Control, tlb, get_uvalue=info,/No_Copy
   Top_Descript = info.TopLabel
   List =  info.List

   CASE Event.id OF 
     info.ButtonId: BEGIN
       wait,0.1
       Widget_Control,info.ButtonBase,map=0
       Widget_Control, info.ListBase,/map
     end
     info.ListId: BEGIN 
       wait,0.1
       top = Top_Descript + $
                '<' + List[event.index] + $
                '>'
       Widget_Control, info.ButtonId, Set_Value=top, $
          Set_Uvalue=List[event.index]
       Widget_Control, info.ListBase,map=0
       Widget_Control, info.ButtonBase,/map
     END 
     ELSE:
   ENDCASE 
   Widget_Control, tlb, set_uvalue=info,/No_Copy
   RETURN, retevent
END

;------------------------------------
; Get routine
;------------------------------------

FUNCTION CW_ButWL_Get,cw_tlb
  value = 0
  tlb =  Widget_Info( cw_tlb,/child )
  Widget_Control, tlb, Get_uvalue=info,/No_Copy
  Widget_Control, info.ButtonId, Get_UValue=Value
  Widget_Control, tlb, Set_Uvalue=info,/No_Copy
  return,Value
END

;------------------------------------
; Set routine
;------------------------------------

PRO CW_ButWL_Set, id, value
  IF N_Params() NE 2 THEN BEGIN 
    Message,'Usage: cw_butwl_set, id, value ',/cont
    return
  ENDIF 
  IF VarType(value) NE 'STRING' THEN value = strtrim( value,2 )
  tlb = Widget_Info( id, /child )
  Widget_Control, tlb, Get_Uvalue=info,/No_Copy

  j = where( strupcase(info.List) EQ strupcase( value ), nj )
  IF nj EQ 1 THEN BEGIN 
    Widget_Control, info.ButtonId, Set_UValue=value
    Widget_Control, ButtonLabelId, Set_Value= info.TopLabel + '<' + value + '>'
  ENDIF ELSE Message,'Value must be among the  elements of List',/cont
  Widget_Control, tlb, Set_Uvalue=info,/No_Copy
  
END

;-----------------------------------------------
; Definition Routine
;-----------------------------------------------

FUNCTION CW_ButWL, parent, TopLabel, List, $
                           Value=Value, $
                           Uvalue=Uvalue

   IF N_Params() EQ 0 THEN BEGIN 
     Message,"Usage, id=Cw_ButWL(parentid, TopLabel, List,[Value=Value, Uvalue=Uvalue])",/cont
     return, 0
   ENDIF 
   IF N_Elements(Uvalue) EQ 0 THEN Uvalue = ''
   IF N_Elements(Value) EQ 0 THEN BEGIN 
     Message,"Usage, id=Cw_ButWL(parentid, TopLabel, List,[Value=Value, Uvalue=Uvalue])",/cont
     Message,"  Variable 'List' must be present!",/cont
     return,0
   ENDIF 
   IF VarType( TopLabel )  NE 'STRING' THEN TopLabel = StrTrim(TopLabel,2)   
   IF VarType( List )  NE 'STRING' THEN List = StrTrim(List,2)
   IF N_Elements(Value) EQ 0 THEN  Value='' ELSE BEGIN 
     IF N_Elements(Value) ne 1  THEN BEGIN 
       Message, "'Value' keyword must be scalar ",/cont
       return,0
     ENDIF ELSE BEGIN 
       s = where( strupcase(list) EQ strupcase(value), ns )
       IF ns EQ 0 THEN BEGIN 
         Message,"Value must be a member of 'List' array",/cont
         return,0
       ENDIF
     ENDELSE 
   ENDELSE 
   junk =  TopLabel +'<' + List + ">"
   MaxLen =  max( strlen(Junk) ) 
   
   CW_Tlb = Widget_Base(parent,Uvalue=Uvalue, $
                        Event_func='cw_butwl_events',$
                        Pro_Set_Value='cw_butwl_set',$
                        Func_Get_Value='cw_butwl_get',$
                        /map )

   ButtonBase = Widget_Base( CW_Tlb, /map )
   IF strlen(Value) NE 0 THEN $
     top = topLabel + '<' + Value + '>' ELSE $
     top =  TopLabel
   buttonid = Widget_Button( ButtonBase, $
                             Value=Top,$
                             UValue=Value,$
                              xsize=(MaxLen+5)*!D.X_Ch_Size)

   ListBase = Widget_Base( CW_Tlb, map=0, /col)
   LabelId =  Widget_Label( ListBase, Value='Choose Color')
   ListId = Widget_List( ListBase, Value=List, $
                          Xsize=MaxLen+2, $
                          Ysize=2)


   info = { Parent: parent,$
                     Cw_Tlb     : Cw_Tlb,$      
                     ButtonBase : ButtonBase,$  
                     ButtonId   : ButtonId,$    
                     ListBase   : ListBase,$  
                     ListId     : ListId, $   
                     TopLabel   : TopLabel,      $
                     List       : List }

   Widget_Control, ButtonBase, set_uval=info,/no_copy
   return, cw_tlb  
END


