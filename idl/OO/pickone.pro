;+
; NAME:  pickone.pro
; $Id$
; PURPOSE:  Little popup that allows user to pick something from a list.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Widget Utility.
;
; CALLING SEQUENCE:  pickone, list, index, cancel
; 
; INPUTS:  list: The list to be chosen from
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
;
;     Index: the chosen item (=-1 if cancel=1)
;     Cancel: 0 if cancel button was not clicked, 1 if it was.
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
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO PICKONE_Event, Event


  WIDGET_CONTROL,Event.top,get_uvalue=info
  

  CASE Event.id OF 

    (*info).ListId: BEGIN
      Widget_Control,(*info).PickedId,Sensitive=1
      (*info).index =  event.index
    END
    (*info).PickedID: BEGIN
      Widget_Control,(*info).tlb,/destroy
    END
    (*info).cancelId: BEGIN
      (*info).cancel = 1
      Widget_Control,(*info).tlb,/destroy
    END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END PICKONE
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



FUNCTION pickone, list,cancel,GROUP=Group


  IF n_elements(list) EQ 0 THEN BEGIN 
    ok = widget_dialog("Picking one from an empty list seems fruitless!")
    cancel = 1
    return,-1
  ENDIF 

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0


  TLB = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Pick one!', $
      UVALUE='TLB',/modal)

  ListId = WIDGET_LIST( TLB,VALUE=list, $
      UVALUE=list, $
      YSIZE=3)

  BASE3 = WIDGET_BASE(TLB, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  PickedId = WIDGET_BUTTON( BASE3, $
      UVALUE='PICKED', $
      VALUE='This one!')

  CancelId = WIDGET_BUTTON( BASE3, $
      UVALUE='CANCEL', $
      VALUE='Cancel')


  info = ptr_new({tlb:tlb,$
                  listId:listId,$
                  index:0L,$
                  cancel:0L,$
                  PickedId:PickedId,$
                  CancelId:CancelId})

  WIDGET_CONTROL, TLB, /REALIZE, set_uval=info
  Widget_Control, (*info).pickedId, Sensitive=0
  XMANAGER, 'pickone', TLB

  index = (*info).index
  cancel = (*info).cancel
  ptr_free, info
  IF cancel THEN index = -1
  return, index
END
