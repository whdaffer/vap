;+
; NAME:  showqmodels
; $Id$
; PURPOSE:  Wrap the QMS (Qscat Model Show) object to make displaying
;          lots of QIF files easier.
;
;
; AUTHOR:  whd
;
; CATEGORY: Seawinds/Interp field display/ Widget 
;
; CALLING SEQUENCE:  
;
;    showqmodels, GROUP=Group, $
;                     files=files, $
;                     path=path, $
;                     filter=filter, $
;                     speedrange=speedrange, $
;                     lonrange=lonrange, $
;                     latrange=latrange, $
;                     bottom=bottom, $
;                     ncolors=ncolors, $
;                     colorfile=colorfile,$
;                     plotvect=plotvect, $
;                     showvoids=showvoids
;
; 
; INPUTS:  none
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
;   Group: The group leader ID of whatever widget hierarchy you want
;          to tie this widget to.
;
;   files: filename vector. List of QIF files you want to display.
;
;   path: The path to use if you want to read more files (defaults to
;         whatever path is on the files appearing in the 'files'
;         keyword or to './' if those aren't present.
;  
;   filter: The filter to use when the file picking widget is
;           called. Def='QIF*.hdf'
;   
;   speedrange: 2-vector: floats. [min,max] speed to consider when
;               making the filled contour. def=[1, 25]
;
;   lonrange: 2-vector: floats. [min,max] longitude. def=[0.,360]
;
;   latrange: simile. def=[-90.,90]
;
;   bottom: bottom color index, def=0
;
;   ncolors: number of colors to map speed into. default=!d.n_colors-1
;            Nota Bene! this routine only works on 8 bit color. If you
;            try to run it when the color environment has already been
;            set to 24 bits it will fail!
;
;   colorfile: file to read for color table.  This file must be
;              arranged as a 3 by ncolors file. See
;              `readcolortable.pro'  for a discussion of the format.
;
;              Default =
;              '~vapuser/Qscat/Resources/Color_Tables/vap-animation.ct'
;   
;   showvoids: flag. If set, don't chop the color contour array at the
;              min speed. this allows the user to see if there are any
;              'voids' in the interpolated field.
; 
;   plotvect: flag. If set, plot the vectors too!
;
;
; OUTPUTS:  All output is to the screen. 
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  Uses the QMS object, so qms__define.pro must be in
;               the path. Only works on 8 bit color, right now.
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  2001/02/07 20:23:50  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 2000, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;====================================================
; Cleanup
;====================================================

PRO showqmodels_cleanup, tlb
  widget_control, tlb,get_uvalue=info
  obj_destroy,(*info).linkedlist
  ptr_free,info
END 
;====================================================
; Redo List of files
;====================================================

PRO showqmodels_redo_list, info
  linkedlist = info.linkedlist
  q = linkedlist-> gethead()
  n = linkedlist-> getcount()
  IF n GT 0 THEN BEGIN 
    list = strarr(n)
    ii = 0
    WHILE ptr_valid(q) DO BEGIN 
      *q-> get,file = file
      file = basename(file)
      list[ii] = file
      ii = ii+1
      q = linkedlist-> getnext()
    ENDWHILE 
    list = list[0:ii-1]
  ENDIF ELSE list= ['']
  widget_control, info.listid, set_value=list
END 

;====================================================
; Event Routine
;====================================================

PRO Showqmodels_Event, Event


  name = tag_names(event,/structure)
  IF name EQ 'QMS_DESTROY_EVENT' THEN BEGIN 
    widget_control, event.handler, get_uvalu=info
    linkedlist = (*info).linkedlist
    q = linkedlist-> gethead()
    found = 0
    WHILE ptr_valid(q) AND NOT found DO BEGIN 
      *q-> get,file = file
      IF strupcase(file) EQ strupcase(event.file) THEN BEGIN 
        q = linkedlist-> removecurrent()
        obj_destroy,*q
        ptr_free,q
        showqmodels_redo_list,*info
        found = 1
      ENDIF ELSE $
        q = linkedlist-> getNext()
    ENDWHILE 
    return
  ENDIF 

  WIDGET_CONTROL,Event.top,Get_uvalue=info
  CASE Event.id OF 

    (*info).pathid: BEGIN
      widget_control, (*info).pathid, get_value=v
      (*info).path = v[0]
      widget_control, (*info).pathid, set_value=v[0]
    END

    (*info).filterid: BEGIN
      widget_control, (*info).filterid, get_value=v
      (*info).filter = v[0]
      widget_control, (*info).filterid, set_value=v[0]
    END

    (*info).ColorFileId: BEGIN
       ptc = readcolortable(event.value)
       IF ptr_valid(ptc) THEN BEGIN 
         (*info).colorfile = event.value
         ct = *ptc
         ptr_free,ptc
         tlvct,transpose(ct)
       ENDIF ELSE BEGIN 
         r = dialog_message(["Can't read input colortable file!",$
                             "No changes made!"],/error)
         widget_control, (*info).colorfileid, set_value=(*info).colorfile
       ENDELSE 
    END

    (*info).NewFileId: BEGIN 
      files = dialog_pickfile(path=(*info).path,filter=(*info).filter,/fix_filter,$
                              /multiple,/must_exist, get_path=newPath)
      IF strlen(newPath) NE 0 AND $
         newPath NE (*info).path THEN (*info).path =  newPath
      x =  where( strlen(files) NE 0, nn )
      IF nn NE 0 THEN BEGIN 
        files = files[x]
        FOR f=0,nn-1 DO BEGIN 
          q = obj_new('qms',files[f],$
                      speedrange=(*info).speedrange,$
                      lonrange=(*info).lonrange, $
                      latrange=(*info).latrange, $
                      bottom=(*info).bottom,$
                      ncolors = (*info).ncolors, $
                     group=(*info).tlb)
          s = (*info).linkedlist-> gettail()
          s = (*info).linkedlist-> Append(q)
          showqmodels_redo_list,(*info)
        ENDFOR 
      ENDIF 
    END 
    (*info).speedid: BEGIN
      widget_Control, (*info).speedid, get_value=v
      (*info).speedrange = v
    END
    (*info).lonid : BEGIN
      widget_Control, (*info).lonid, get_value=v
      (*info).lonrange = v
    END
    (*info).latid : BEGIN
      widget_Control, (*info).latid, get_value=v
      (*info).latrange = v
    END
    (*info).colorid : BEGIN
      widget_Control, (*info).colorid, get_value=v
      (*info).bottom = v[0]
      (*info).ncolors = v[1]
    END
    (*info).listid : BEGIN
    END
    (*info).DeleteId: BEGIN
      list = widget_info((*info).listid, /list_select)
      IF list[0] NE -1 THEN BEGIN 
        list = list+1
        nn = n_elements(list)
        FOR i=0,nn-2 DO BEGIN 
           s = (*info).linkedlist-> gotonode(list[i])
           s = (*info).linkedlist-> removecurrent()
           obj_destroy,*s
           list[i+1:*] = list[i+1:*]-1
        ENDFOR 
        s = (*info).linkedlist-> gotonode(list[nn-1])
        s = (*info).linkedlist-> removecurrent()
        obj_destroy,*s
      ENDIF 
      showqmodels_redo_list,*info
    END
    (*info).Clearid : BEGIN
      obj_destroy,(*info).linkedlist
      (*info).linkedlist = obj_new('linkedlist')
      Widget_control, (*info).listid, set_value=['']
    END
    (*info).quitid: BEGIN
      Widget_Control,(*info).tlb,/destroy
    END
  ENDCASE
END


;====================================================
; Widget Definition Routine
;====================================================


PRO showqmodels, GROUP=Group, $
                 files=files, $
                 path=path, $
                 filter=filter, $
                 speedrange=speedrange, $
                 lonrange=lonrange, $
                 latrange=latrange, $
                 bottom=bottom, $
                 ncolors=ncolors, $
                 colorfile=colorfile,$
                 plotvect=plotvect, $
                 showvoids=showvoids
                 
   


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0



  set_plot,'x'
  device,pseudo=8
  device,get_visual_name=visual
  IF strupcase(visual) NE 'PSEUDOCOLOR' THEN BEGIN 
    r = dialog_Message(["Can't create PSEUDO COLOR visual",$
                        "You'll have to exit and restart IDL.",$
                        "Then start this routine FIRST THING!",$
                        "If you still get this message, check your",$
                        "Init files (e.g. $IDL_STARTUP) for commands that", $
                        "create windows or calls to the 'device' command",$
                         "help,/device also causes problems!"],/error)
    return
  ENDIF 

  speedrange = n_elements(speedrange) NE 2 ? [1.,25] : speedrange
  lonrange   = n_elements(lonrange)   NE 2 ? [0.,360] : lonrange
  latrange   = n_elements(latrange)   NE 2 ? [-90.,90.] : latrange

  path = n_elements(path) EQ 0 ? deenvvar('$VAP_OPS_ANIM') : path
  filter =  n_elements(filter) EQ 0? "QIF*.hdf": filter

  plotvect = keyword_set(plotvect)
  cf =  $
    "/usr/people/vapuser/Qscat/Resources/Color_Tables/vap-animation.ct" 

  IF n_elements(colorfile) EQ 0 THEN BEGIN 
    colorfile =  cf
    IF n_elements(bottom) EQ 0 THEN bottom = 1
    IF n_elements(ncolors) EQ 0 THEN ncolors = 29
  ENDIF 

  ptc = readcolortable(colorfile)
  IF NOT ptr_valid(ptc) THEN BEGIN 
    r = dialog_message(["Can't read colortable file!", $
                        colorfile, "Exiting!"],/error)
    return
  ENDIF 
  ct = *ptc
  ptr_free,ptc
  nc = n_elements(ct[0,*])
  window,/free,/pixmap, colors=nc &  wdelete, !d.window
  tvlct,transpose(ct)

  bottom =  n_elements(bottom) EQ 0 ? 0: bottom
  ncolors =  n_elements(ncolors) EQ 0 ? !d.n_colors-1: ncolors

  linkedlist = obj_new('linkedlist')

  TLB = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Qmodel Velocity Field ', $
      UVALUE='TLB')

  IF n_elements(files) NE 0 THEN BEGIN 
    FOR i=0,n_elements(files)-1 DO BEGIN 
      q = obj_new('qms',files[i],bottom=bottom,ncolors=ncolors,$
                  speedrange=speedrange,lonrange=lonrange,$
                  latrange=latrange, group=tlb,plotvect=plotvect, $
                 showvoids=showvoids)
      IF obj_valid(q) THEN BEGIN 
        s = linkedlist-> append(q)
      ENDIF ELSE BEGIN 
        Message,'Error creating QMS object for File ' + $
            files[i] + ': Discarded',/info
      ENDELSE 
    ENDFOR 
  ENDIF 

  BASE3 = WIDGET_BASE(TLB, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE3')

  LABEL29 = WIDGET_LABEL( BASE3, $
      UVALUE='LABEL29', $
      VALUE='Path to Qmodel (QIF*hdf) files')

  PathId = Widget_Text( BASE3,VALUE=path, $
      editable=1, $
      UVALUE='FIELD31',Xsize=30,ysize=1,/scroll)

  LABEL29 = WIDGET_LABEL( BASE3, $
      UVALUE='LABEL29', $
      VALUE='Filter for (QIF*hdf) files')

  filterId = Widget_Text( BASE3,VALUE=filter, $
      editable=1, $
      UVALUE='FIELD31',Xsize=30,ysize=1,/scroll)

  LABEL33 = WIDGET_LABEL( BASE3, $
      UVALUE='LABEL33', $
      VALUE='Color Table File')

  ColorFileId = Widget_text( BASE3,VALUE=colorfile, $
      Editable=1, $
      UVALUE='FIELD35', $
      XSIZE=30,ysize=1,/scroll)

  NewFileId =  widget_Button( base3, Value="Get New File")


  junk = WIDGET_BASE(BASE3, $
      /col, $
      MAP=1, $
      UVALUE='BASE44')

   speedid = cw_doubleslide(junk,/titlerc,$
                            TitleDs="Speed (m/s)", $
                            format='(f7.2)',$
                            min=0, max=50,$
                            valu=speedrange,$
                            /drag)
   lonid = cw_doubleslide(junk,$
                          /titlerc,$
                          TitleDS='Longitude Range', $
                          format='(f7.2)',$
                          min=-180,max=360.,$
                          value=lonrange,$
                          /drag)
   latid = cw_doubleslide(junk,$
                          /titlerc,$
                          TitleDS='Latitude Range', $
                          min=-90.,max=90.,$
                          format='(f7.2)',$
                          value=latrange,$
                          /drag)
   colorid = cw_doubleslide(junk, $
                            /titlerc, $
                            format='(f5.0)',$
                            TitleDS='Bottom/Ncolors',$
                            min=0,max=!d.n_colors-1,$
                            value=[bottom,ncolors],$
                            /drag)                                
  

  BASE19 = WIDGET_BASE(TLB, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE19')

  
  q = linkedlist-> gethead()
  WHILE ptr_valid(q) DO BEGIN 
    *q-> get,file = file 
    file = basename(file)
    IF n_elements(filelist) EQ 0 THEN $
      filelist = file ELSE filelist = [file,filelist]
    q = linkedlist-> getnext()
  ENDWHILE 
  IF n_elements(filelist) EQ 0 THEN filelist = ''
  
  ListId = WIDGET_list( BASE19,VALUE=filelist, $
      UVALUE='LIST', $
      /multiple, $
      YSIZE=5,$
      Xsize=40)
  junk =  widget_base(base19,/row)
  DeleteId = WIDGET_BUTTON( junk, $
      UVALUE='BUTTON41', $
      VALUE='Delete')

  Clearid = WIDGET_BUTTON( junk, $
      UVALUE='BUTTON42', $
      VALUE='Clear')


  QuitId = WIDGET_BUTTON( TLB, $
      UVALUE='BUTTON53', $
      VALUE='Quit')

  info = { tlb: tlb, $
           pathId: pathid, $
           filterId: filterid, $
           ColorFileId: ColorFileId, $
           NewFileId: NewFileId, $
           speedId: Speedid, $
           lonid: lonid, $
           latid: latid, $
           colorId: colorId, $
           DeleteId: DeleteId, $
           ClearId: ClearId, $
           QuitId: QuitId, $
           ListId:  listid, $
           path: path, $
           filter:filter, $
           colorfile: colorfile, $
           speedrange: speedrange, $
           lonrange: lonrange, $
           latrange: latrange, $
           Bottom: bottom, $
           ncolors: ncolors, $
           linkedlist: linkedlist }

  WIDGET_CONTROL, TLB, set_uvalue=ptr_new(info,/no_copy), /REALIZE
  XMANAGER, 'showqmodels', tlb, cleanup='showqmodels_cleanup',/no_block


END
