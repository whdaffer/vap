;+
; NAME:  cw_pvfinfo
; $Id$
; PURPOSE:  A compound widget containing a label naming a file, a
;          button which, when pushed, opens another window with more
;          information about the file, and two radio button groups
;          allowing one to plot/noplot and keep/delete the file.
;          I know, it doesn't make much sense. Just ask me and I'll explain.
;
; AUTHOR:  William
;
; CATEGORY:  PV utility
;
; CALLING SEQUENCE:  id=cw_pvfinfo( base, filename, $
;                                   infostruct, plotflag, $
;                                   uvalue=uvalue, xsize=xsize )
; 
; INPUTS:  Base :(Required) the widget ID of the base of which  this
;                 widget will be a part.
;
;         Filename: The filename. The path component is stripped off
;                   and put in with the information contained in 'infostruct'
;
;         infostruct: A structure which will be used in constructing
;                     the information that will be displayed when the
;                     'info' button is pressed. The information will
;                     be keyword: value format where the keyword will
;                     be the tag in the structure and the data will be
;                     it's value, as a string.
;
;                     E.G. If the structure is 
;                       infostruct={$
;                          starttime: '1999-10-10T00:11:22.333Z', $
;                          endtime: '1999-11-12T13:14:15.678Z' }
;
;                     Then, when the 'info' button on the widget is
;                     pushed,  the information will be presented as:
; 
;                          starttime : 1999-10-10T00:11:22.333Z
;                          endtime   : 1999-11-12T13:14:15.678Z
;
;                      
;         Plotflag: 0|1. A flag telling whether this file will be
;                   plotted.
;         
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
;    UserVal: All good widgets need a user value. Set to 'CW_PVFINFO'
;             if not passed in.
;    Xsize: The size (in ???) of the widget.
;
; OUTPUTS:  The ID of the compound widget
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
; Revision 1.1  1999/10/22 23:32:49  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

; ====================================================
;PRO cw_pvfinfo_cleanup, id
;  Widget_control, widget_info(id,/child), get_value=info
;  ptr_free, info
;END

; ====================================================
;FUNCTION cw_pvfinfo_ev,event
PRO cw_pvfinfo_ev,event
  retevent = 0l
  Widget_Control, Widget_Info( event.handler,/child), Get_Uvalue=info
  CASE event.id OF 
    info.plotid: info.plotit = event.value
    info.deleteid: BEGIN 
      info.deleteit = event.value
      Widget_Control, info.infobaseid, map= event.value EQ 0
      Widget_Control, info.deletebaseid, map=event.value EQ 1
    END 
    info.infoButId: BEGIN 
      cw_pvfinfo_popup,info.tlb
    END 
  ENDCASE 
  Widget_Control, Widget_Info( event.handler,/child), Set_Uvalue=info
;  return,retevent 
END

; ====================================================

FUNCTION cw_pvfinfo_getv,id 
  Widget_Control, widget_info(id, /child), get_uval=info
  return, [info.plotit, info.deleteit]
END

; ====================================================


PRO cw_pvfinfo_setv, id, value
  IF n_params() LT 2 THEN BEGIN 
    Usage, "cw_pvfinfo_setv, id, [plotit, deleteit]"
    return
  ENDIF 
  IF n_elements(value) NE 2 THEN BEGIN 
    Message,"value must have 2 elements [plotit_flag,deleteit_flag]",/cont
    return
  ENDIF 

  Widget_Control, widget_info(id, /child), get_uval=info
  info.plotit = value[0]
  info.deleteit = value[1]
  Widget_Control, info.plotid, set_value=value[1] 
  Widget_Control, info.deleteID, set_value=value[0] 
  Widget_Control, info.infobaseid,map=value[0] EQ 0
  Widget_Control, info.deletebaseid,map=value[0] EQ 1
  Widget_Control, widget_info(id, /child), Set_uval=info
 
END


; ====================================================
PRO cw_pvfinfo_popup_events, event
  widget_control, event.handler, /destroy
END
; ====================================================

PRO cw_pvfinfo_popup, top

  popupid = Widget_Base( group=top, /col,/map,/modal)
  widget_control, widget_info(top,/child), get_uvalue=top_info
  popuplabelid = widget_label(popupid, $
                                      value=top_info.filename)

;  tableid = Widget_Table( popupid, value=top_info.infostruct, $
;                          /row_major, ysize=1,/resizeable_col)
  tags = tag_names(top_info.infostruct)
  ntags = n_elements(tags)
  info_array = strarr(ntags)
  FOR i=0,ntags-1 DO BEGIN 
      info_array[i] =  tags[i] + ' : '      
    IF n_elements(top_info.infostruct.(i)) GT 1 THEN BEGIN 
      info_array[i] =  info_array[i] + $
        strjoin(strtrim(top_info.infostruct.(i),2),' ')
    ENDIF ELSE BEGIN 
      info_array[i] =  info_array[i] + $
        strtrim( top_info.infostruct.(i),2  )
    ENDELSE 
  ENDFOR 
  tableid =  Widget_List( popupid, value=info_array,ysize=ntags)
  dismissId = Widget_Button( popupid, value='DISMISS')
  info = { top: popupid, $
                   tableid: tableid, $
                   dismissid: dismissid}
  Widget_control, popupid, /realize, set_uvalue=info

  Xmanager, 'cw_pvfinfo_popup',popupid, $
    event_handler='cw_pvfinfo_popup_events'

END


; ====================================================

FUNCTION cw_pvfinfo, base, filename, infostruct, plotflag, $
                     uvalue=uvalue, xsize=xsize

  IF n_params() LT 4 THEN BEGIN 
    Usage,'id=cw_pvfinfo(base_tlb,filename,infostruct_ptr,plotflag)'
    return,0l
  ENDIF 

  IF NOT isa(infostruct,/structure) THEN BEGIN 
    Message,'INFOSTRUCT must be a STRUCTURE',/cont
    return, 0l
  ENDIF 
;  tags = tag_names(infostruct)
;  ntags = n_elements(tags)
;  infostructarray = strarr(2,ntags)
;  FOR i=0,ntags-1 DO BEGIN 
;    infostructarray[0,i] =  tags[i]
        
;  ENDFOR 

  IF n_elements(uvalue) EQ 0 THEN Uvalue = 'CW_PVFINFO'

  
  tlb = widget_base(base,uvalu=uvalue, frame=2, $
                  PRO_Set_Value='cw_pvfinfo_setv', $
                  func_Get_Value='cw_pvfinfo_getv',/row)


  junkid = widget_base( tlb )

  basename = basename(filename)
  path = path(filename)
  infobaseid = widget_base(junkid,/row,/map)
  fileid = widget_label(infobaseid,value=basename(filename), xsize=xsize )
  infoButId = widget_Button(infobaseid,value="Info")
  labels = ['N','Y']
  plotid = CW_Bgroup(infobaseid,labels,Label_Top='Plot?', $
                     frame=2,/exclusive,/return_index,$
                     /no_release,/row,set_value=plotflag)

  deletebaseid =  widget_base(junkid,map=0,/row)
  labelid = Widget_Label(deletebaseId,Value='Dismiss Removes from List')


  deleteId = CW_BGroup(tlb,labels,Label_top='Del?',$
                        frame=2,/exclusive,/return_index,$
                       /no_release,/row, set_value=0)

  
  infostruct =  create_struct( 'path', path, infostruct )
  info =  { base         : base, $
            tlb          : tlb, $
            filename     : filename, $
            infobaseid   : infoBaseId, $
            infoButId    : infoButId, $
            infostruct   : infostruct, $
            plotId       : PlotId, $
            Deletebaseid : DeleteBaseId, $
            DeleteId     : DeleteId, $
            plotit       : plotflag, $  
            deleteit     : 0L, $        
            popupid      : 0L, $        
            tableid      : 0l, $        
            dismissid    : 0l }


  Widget_control, junkid, set_Uvalue=info
  xmanager, 'cw_pvfinfo', tlb, $
    event_handler='cw_pvfinfo_ev',/just_reg
        

  return, tlb
END

