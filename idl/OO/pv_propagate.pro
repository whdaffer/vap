;+
; NAME:  pv_propagate
; $Id$
; PURPOSE: Widget to be called from pv. Propagate QuikSCAT orbits to
;          see where the next 'N' orbits will fall.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Pv support
;
; CALLING SEQUENCE:  pv_propagate, parent
;
; 
; INPUTS:  parent: The Top Level Base of the 
;                  PV plotting object.
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  None
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
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO pv_propagate_Event, Event


  WIDGET_CONTROL,Event.top, get_uvalue=info
  IF event.id NE (*info).applyid AND $
   event.id NE (*info).dismissid AND $
   event.id NE (*info).browseId THEN return 

  Widget_control, (*info).parent,Get_uvalue=pv_obj
  pv_obj-> get,oplot = oplot
  orbinc = 25.258
  CASE Event.id OF 
    (*info).browseId: BEGIN 
      pv_obj-> Get,DataList = DataList
      nn = dataList-> GetCount()
      files = strarr(nn)
      ii = 0
      IF nn NE 0 THEN BEGIN 
        files = strarr(nn)
        current = dataList-> GetHead()
        WHILE ptr_valid(current) DO BEGIN 
          (*current)-> Get,data = q
          s = q-> Get(file=file)
          files[ii] =  file
          ii = ii+1
          current = dataList-> GetNext()
        ENDWHILE 
        IF ii LT nn THEN files = files[0:ii-1]
        index =  pickone( files, cancel, group=event.top)
        IF cancel NE 1 THEN BEGIN 
          current =  datalist-> GoToNode(index)
          IF ptr_valid(current) THEN BEGIN 
            (*current)-> Get,data = q
            s = q-> Get(file=file,eqx=eqx)
            Widget_Control, (*info).fileId,set_value=file
            Widget_Control, (*info).lonid, set_value=eqx.lon
            eqxtime = eqx.date + 'T' + eqx.time
            Widget_Control, (*info).TimeId, set_value=eqxtime
          ENDIF ELSE BEGIN 
            message = ["I can't get the file you want!.", $
                       "Somehow the information I need is wrong",$
                       "Please try again!"]
            ok = dialog_Message(message)
          ENDELSE 
        ENDIF 
      ENDIF ELSE BEGIN 
        ok = dialog_message("No Files!")
      ENDELSE 
    END 
    (*info).APPLYid: BEGIN
      Widget_Control, (*info).lonId, get_value=lon
      widget_control, (*info).numOrbsId, get_value=numorbs
      Widget_control, (*info).InclusiveId, Get_value=inclusive
      wset,(*info).wid
      map_set,/grid,/lab,/cont
      IF Numorbs GT 1 THEN BEGIN 
        IF inclusive THEN BEGIN 
          FOR o=1,numorbs DO BEGIN 
            tt = qswathextent(lon-o*orbinc)
            ; FOR i=0,2 DO plots,tt[i,*,0],tt[i,*,1]
            FOR i=0,n_elements(tt[0,*,0])-1 DO BEGIN 
              plots,[tt[0,i,0],tt[2,i,0]],[tt[0,i,1],tt[2,i,1]]
            ENDFOR 
            plots,tt[0,*,0],tt[0,*,1],color=!d.n_colors/2
            plots,tt[2,*,0],tt[2,*,1],color=!d.n_colors/2
          ENDFOR 
        ENDIF ELSE BEGIN 
          tt = qswathextent(lon-numorbs*orbinc)
          FOR i=0,2 DO plots,tt[i,*,0],tt[i,*,1]
        ENDELSE 
      ENDIF ELSE BEGIN 
        tt = qswathextent(lon-orbinc)
        FOR i=0,2 DO plots,tt[i,*,0],tt[i,*,1]
      ENDELSE 
      oplot-> draw
    END
    (*info).DISMISSid: BEGIN
      Widget_Control, event.top, /destroy
    END
  ENDCASE
END


;=====================================================


PRO pv_propagate, parent


  IF N_ELEMENTS(parent) EQ 0 THEN BEGIN 
    Usage,"pv_propagate,parent"
    return
  ENDIF 

  widget_control,parent,get_uvalue=pv_obj
  pv_obj-> Get,DataList = DataList
  current = DataList-> GetHead()
  keeplon= 0.0
  keeptime = '1970-001T00:00:00.000'
  keepfile = '<No File>'
  WHILE ptr_valid( current ) DO BEGIN 
    po = *current
    po-> Get,data = q
    IF obj_class(q) EQ 'Q2B' THEN BEGIN 
      s = q-> Get(Eqx=Eqx, file=file)
      time = eqx.date + 'T' + eqx.time
      IF time GT keeptime THEN BEGIN 
        keeplon = eqx.lon
        keeptime = time
        keepfile = file
      ENDIF 
    ENDIF 
    current = DataList-> GetNext()
  ENDWHILE 

  
  TLB = WIDGET_BASE(GROUP_LEADER=parent, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Propagate Orbits...', $
      UVALUE='TLB')

  BASE4 = WIDGET_BASE(TLB, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  FileId = WIDGET_LABEL( BASE4, $
      UVALUE='LABEL6', $
      VALUE= keepfile, /dynamic_resize )

  BrowseId = WIDGET_BUTTON( BASE4, $
      UVALUE='BROWSEFILE', $
      VALUE='Browse')


  BASE14 = WIDGET_BASE(TLB, $
      COLUMN=1, $
      FRAME=2, $
      MAP=1, $
      UVALUE='BASE14')

  LABEL15 = WIDGET_LABEL( BASE14, $
      UVALUE='LABEL15', $
      VALUE='Equator Crossing Information', $
      /align_center)

  LonId = CW_FIELD( BASE14,VALUE=keeplon, $
      ROW=1, $
      FLOAT=1, $
      TITLE='Longitude: ', $
      /return_events, $
      UVALUE='LONGITUDE')

  TimeId = CW_FIELD( BASE14,VALUE=keeptime, $
      ROW=1, $
      STRING=1, $
      TITLE='Time: ', $
      UVALUE='TIME')

   NumOrbsId = CW_FIELD( BASE14,VALUE=1, $
      ROW=1, $
      /Long, $
     /return_events, $
      TITLE='Num Orbits: ', $
      UVALUE='NUMORBS')

  Btns1017 = [ $
    'No', $
    'Yes' ]

  InclusiveId = CW_BGROUP( BASE14, Btns1017, $
      ROW=1, $
      EXCLUSIVE=1, $
      set_value=0, $
      LABEL_TOP='Inclusive?...', $
      UVALUE='INCLUSIVE')


  BASE27 = WIDGET_BASE(TLB, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE27')

  PropDrawId = WIDGET_DRAW( BASE27, $
      RETAIN=2, $
      UVALUE='PROPDRAW', $
      XSIZE=400, $
      YSIZE=300)

  BASE31 = WIDGET_BASE(BASE27, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE31')

  ApplyId = WIDGET_BUTTON( BASE31, $
      UVALUE='APPLY', $
      VALUE='Apply')

  DismissId = WIDGET_BUTTON( BASE31, $
      UVALUE='DISMISS', $
      VALUE='Dismiss')

  WIDGET_CONTROL, TLB, /REALIZE

  ; Get drawable window index
  WIDGET_CONTROL, PropDrawId, GET_VALUE=wid

  info = Ptr_New({ tlb:tlb, $
                 parent: parent, $
                 FileId: FileId, $
                 browseId: browseId, $
                 LonId: LonId, $
                 TimeId: TimeId, $
                 NumOrbsId:NumOrbsId, $
                 InclusiveId:InclusiveId, $
                 propDrawId:propDrawId,$
                 Wid:Wid, $
                 ApplyId: ApplyId, $
                 DismissId: DismissId})
  Widget_control, tlb, set_uvalue=info
  XMANAGER, 'pv_propagate', TLB
  ptr_free, info

END
