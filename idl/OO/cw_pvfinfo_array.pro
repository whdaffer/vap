;+
; NAME:  cw_pvfinfo_array
; $Id$
; PURPOSE:  Create an array of cw_pvfinfo compound widgets.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  PV Utility
;
; CALLING SEQUENCE:  id=cw_pvfinfo_array(tlb, $
;                               base=base, $
;                                filenames=filenames, $
;                                infostr=infostr, $
;                                plotflags=plotflags, $
;                                uvalue=uvalue)
; 
;
;    There are two ways in which this routine can be called. The
;    first, and most sensible, requires the keyword arguments
;    base,filenames,infostr and plotflags, and is the way that one
;    calls the routine to define the widget initially. The second requires only
;    the 'tlb' parameter and is used, in certain unusual
;    circumstances, to 'reset' the widget. This parameter is the base of the current
;    cw_pvfinfo_array widget, i.e. it's the number the widget
;    definition routine 'cw_pvfinfo_array RETURNS. That id can be used
;    (via WIDGET_INFO and WIDGET_CONTROL) to get the other
;    information, since it's currently stored in the user value of the
;    child of the widget base which has id =tlb.  My advice to you is,
;    Don't use this method. I put it in and I'm leaving it in because
;    I might find it useful later, but I'm planning on getting rid of
;    it and so you shouldn't use it.

; INPUTS:   tlb: The widget id of the current 'cw_pvfinfo_array'
;         base.  
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
;   base: The ID of the base in which this widget will be a part.
;   filenames: The filenames of to be used in the cw_pvfinfo widgets
;   infostr:  The info structures. May be an array of structure, an
;            array of pointers or a linked list of structures.
;   plotflags: The plotflags to be passed ot cw_pvfinf
;   uvalue: All good compound widgets need a user value. Set to
;           'CW_PVFINFO_ARRAY' if unset by user.
;
; OUTPUTS:  ID. The widget id of the array, or 0 if there is some failure
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

;===================================================
PRO cw_pvfinfo_array_reset, id, filenames=filenames, $
                            infostr=infostr, plotflags=plotflags
  forward_FUNCTION cw_pvfinfo_array_init
  Widget_Control,widget_info(id,/child),get_uvalue=info
  info = cw_pvfinfo_array_init(id, base=info.base,filenames=filenames, $
                            infostr=infostr, plotflags=plotflags)
  Widget_Control,widget_info(id,/child),set_uvalue=info
END


;===================================================

;FUNCTION cw_pvfinfo_array_ev, event
PRO cw_pvfinfo_array_ev, event
   retevent = 0l
   print,'in cw_pvfinfo_array_ev'
   help,event,/st
;   return,retevent
END

;===================================================
FUNCTION cw_pvfinfo_array_getV, id
  widget_control, widget_info( id,/child), get_uvalue=info
  nn = n_elements(info.pvfinfoids)
  value = lonarr(2,nn)
  FOR i=0,nn-1 DO BEGIN 
    widget_control, info.pvfinfoids[i], get_value=v
    value[*,i] = v
  ENDFOR 
  return, value
END

;===================================================
PRO cw_pvfinfo_array_setv, id, value
  forward_FUNCTION cw_pvfinfo_array_init
  widget_control, widget_info( id,/child), get_uvalue=info
  nn = n_elements(info.pvfinfoids)
  IF NOT isa(value,/pointer) THEN BEGIN 
    dim = size(value,/dim)
    ndims = size(value,/n_dim)
    IF ndims EQ 2 THEN BEGIN 
      IF dim[0] NE 2 OR dim[1] NE nn THEN BEGIN 
        Message,"Value must be " + $
          strtrim(dim[0],2) + $
            ' by '+ $
               strtrim(dim[1],2),/info
        ok = dialog_message( !err_string )
        return
      ENDIF 
      FOR i=0,nn-1 DO $
       widget_control, info.pvfinfoids[i], set_value=value[*,i]
    ENDIF ELSE $
       widget_control, info.pvfinfoids[0], set_value=value
  ENDIF ELSE BEGIN 
      ; The only time 'value' is a pointer is when the user wants to
      ; set the widget to 'No Files' state.
    filenames =  '<No Data Files>'
    infostr =  {No_Info_Available: '<No Information>'}
    plotflags = 0l
    info =  cw_pvfinfo_array_init(id, $
                                  filenames=filenames, $
                                  infostr=infostr, $
                                  plotflags=plotflags)
    widget_Control, Widget_Info(id,/child),set_uvalue=info
  ENDELSE 
END


;===================================================


FUNCTION cw_pvfinfo_array_init, tlb, base=base, $
                                filenames=filenames, $
                                infostr=infostr, $
                                plotflags=plotflags, $
                                uvalue=uvalue

  IF n_params() LT 1 AND $
    n_elements(filenames) EQ 0  THEN BEGIN 
    Message,'Either params TLB or keywords FILENAMES|INFOSTR|PLOTFLAGS',/cont
    return,0
  ENDIF 

  IF n_elements(tlb) NE 0 THEN Widget_Control, Widget_Info(tlb,/child), Get_Uvalue=info
  
  IF NOT isa(filenames,/string,/nonempty) THEN BEGIN 
    Message,"Filenames isn't a STRING!",/cont
    return,0l
  ENDIF 

  INFOTYPE    = 0l
  LinkedList  = 1l
  PTRARRAY    = 2l
  STRUCTARRAY = 3l
  CASE 1 OF 
    (obj_valid(infostr))[0]: BEGIN 
      IF NOT obj_isa(infostr, 'LINKEDLIST') THEN BEGIN 
        Message,"If Object, Infostr must be a LINKEDLIST",/cont
        return,0l
      ENDIF 
      INFOTYPE = LINKEDLIST
      n_info = infostr-> GetCount()
    END 
    (ptr_valid(infostr[0]))[0]: BEGIN 
      INFOTYPE = PTRARRAY
      n_info = n_elements(infostr)
    END 
    (isa(infostr[0],/structure))[0]: BEGIN 
      INFOTYPE = STRUCTARRAY
      n_info = n_elements(infostr)
    END 
    ELSE : BEGIN 
      Message,"infostructs must be a Object (linkedlist) a Ptr_Arr or a Structure!",/cont
      return,0l
    ENDIF 
  ENDCASE 

  IF n_elements(filenames) NE n_info  OR $
     n_elements(filenames) NE n_elements(plotflags)  OR $
     n_info NE n_elements(plotflags)  THEN BEGIN 
     Message,$
       'All parameters <FILENAMES, INFOSTRUCT, PLOTFLAGS> must have same # elements',/cont
     return,0l
  ENDIF

  IF n_elements(uvalue) EQ 0 THEN uvalue = 'CW_PVFINFO_ARRAY'

  y_scroll_size = !d.y_ch_size*10
  x_size =  !d.x_ch_size*($
        max(strlen(basename(filenames)))+$
         strlen('Info')+$
          strlen('plot it?')+$
            strlen("delete it?") $
                             )

  IF n_elements(tlb) EQ 0 THEN BEGIN 
    tlb = widget_base(base,/col,y_scroll_size=y_scroll_size, /scroll, $
                    xsize=x_size,$
                     x_scroll_size=x_size,/map,frame=2, $
                    PRO_set_value='cw_pvfinfo_array_setv', $
                    func_get_value='cw_pvfinfo_array_getv', $
                   uvalue=uvalue) 
    junk = widget_base( tlb,/col )
  ENDIF ELSE BEGIN 
      ; tlb exists! That means we're
      ; reseting the components of this
      ; widget. So, we have to destroy the
      ; old ones!
    nn = n_elements(info.pvfinfoids)
    FOR i=0,nn-1 DO Widget_Control, info.pvfinfoids[i], /Destroy
    nn = n_elements(filenames)
    pvfinfoids = lonarr(nn)
    base = info.base
    junk = Widget_Info(tlb,/child)
    Widget_Control, tlb, ysize=0
  ENDELSE 

  nn = n_elements(filenames)
  pvfinfoids = lonarr(nn)
  FOR i=0,nn-1 DO BEGIN 
    CASE infotype OF 
      linkedlist: info1 = *(infostr-> GoToNode(i+1))
      ptrarray:   info1 = *(infostr[i])
      structarray: info1 = infostr[i]
    ENDCASE 
    t = cw_pvfinfo(junk,filenames[i],info1,plotflags[i])
    IF t eq 0l THEN BEGIN 
      Message,"Error Creating cw_pvfinfo! for file " + filenames[i],/cont
      return,0l
    ENDIF 
    pvfinfoids[i] = t
  ENDFOR 
  info = {base: base, $
          tlb:tlb, $
          pvfinfoids: pvfinfoids}
  return,info
END
 
;===================================================


FUNCTION cw_pvfinfo_array, base, filenames, infostr,plotflags, uvalue=uvalue

  IF n_params() LT 1 THEN BEGIN 
    Usage,'id=cw_pvfinfo_array (base[, filenames, info_structs, plotflags,uvalue=uvale] )'
    return,0l
  ENDIF 

  IF n_elements(filenames) EQ 0 THEN BEGIN 
    filenames =  '<No Data Files>'
    infostr =  {No_Info_Available: '<No Information>'}
    plotflags = 0l
  ENDIF ELSE IF isa(filenames,/pointer) THEN BEGIN 
    filenames =  '<No Data Files>'
    infostr =  {No_Info_Available: '<No Information>'}
    plotflags = 0l
  ENDIF 

  info = cw_pvfinfo_array_init( base=base, filenames=filenames, $
                                infostr=infostr, $
                                plotflags=plotflags, $
                                uvalue=uvalue)

;  IF NOT isa(filenames,/string,/nonempty) THEN BEGIN 
;    Message,"Filenames isn't a STRING!",/cont
;    return,0l
;  ENDIF 

;  INFOTYPE    = 0l
;  LinkedList  = 1l
;  PTRARRAY    = 2l
;  STRUCTARRAY = 3l
;  CASE 1 OF 
;    obj_valid(infostr): BEGIN 
;      IF NOT obj_isa(infostr, 'LINKEDLIST') THEN BEGIN 
;        Message,"If Object, Infostr must be a LINKEDLIST",/cont
;        return,0l
;      ENDIF 
;      INFOTYPE = LINKEDLIST
;    END 
;    ptr_valid(infostr[0]): INFOTYPE = PTRARRAY
;    isa(infostr,/structure): INFOTYPE = STRUCTARRAY
;    ELSE : BEGIN 
;      Message,"infostructs must be a Object (linkedlist) a Ptr_Arr or a Structure!",/cont
;      return,0l
;    ENDIF 
;  ENDIF 

;  IF n_elements(filenames) NE n_elements(infostr)  OR $
;     n_elements(filenames) NE n_elements(plotflags)  OR $
;     n_elements(infostr) NE n_elements(plotflags)  THEN BEGIN 
;     Message,$
;       'All parameters <FILENAMES, INFOSTRUCT, PLOTFLAGS> must have same # elements',/cont
;     return,0l
;  ENDIF

;  IF n_elements(uvalue) EQ 0 THEN uvalue = 'CW_PVFINFO_ARRAY'

;  nn = n_elements(filenames)
;  pvfinfoids = lonarr(nn)
;  y_scroll_size = !d.y_ch_size*10
;  x_size =  2*!d.x_ch_size*($
;        max(strlen(filenames))+$
;         strlen('Info')+$
;          strlen('plot it?')+$
;            strlen("delete it?") $
;                             )
;;  IF nn GT 4 THEN BEGIN 
;    tlb = widget_base(base,/col,y_scroll_size=y_scroll_size, /scroll, $
;                     xsize=x_size,$
;                      x_scroll_size=x_size,/map,frame=2, $
;                     PRO_set_value='cw_pvfinfo_array_setv', $
;                     func_get_value='cw_pvfinfo_array_getv', $
;                    uvalue=uvalue) 
;;  ENDIF ELSE BEGIN 
;;    tlb = widget_base(base,/col,/map,frame=2)
;;  ENDELSE 
;  junk = widget_base( tlb,/col )
;  FOR i=0,nn-1 DO BEGIN 
;    CASE infotype OF 
;      linkedlist: info = infostr-> GetNode(i+1)
;      ptrarray:   info = *(infostr[i])
;      structarray: info = infostr[i]
;    ENDCASE 
;    t = cw_pvfinfo(junk,filenames[i],info,plotflags[i])
;    IF t eq 0l THEN BEGIN 
;      Message,"Error Creating cw_pvfinfo! for file " + filenames[i],/cont
;      return,0l
;    ENDIF 
;    pvfinfoids[i] = t
;  ENDFOR 
;  info = {base:base, $
;          tlb:tlb, $
;          pvfinfoids: pvfinfoids}

  widget_control, widget_info(info.tlb,/child), set_uvalue=info
  base_geometry = widget_info(info.tlb,/geometry)
  pvfinfo_geometry = widget_info(info.pvfinfoids[0],/geometry)
  nn = n_elements(info.pvfinfoids)
  widget_control,info.tlb,ysize=(3 < nn)*pvfinfo_geometry.ysize
  Xmanager, 'cw_pvfinfo_array', info.tlb, /just_reg, $
     event_handler='cw_pvfinfo_array_ev'

  return, info.tlb
END
