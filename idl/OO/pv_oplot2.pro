;+
; NAME:  PV_Oplot
; $Id$
; PURPOSE:  overplotting of locations in PV
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
; Revision 1.1  1999/10/06 22:53:12  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-


;================================================================
; PV_OPLOT_EVENT
;================================================================
PRO pv_oplot_Event, Event

  Widget_control,event.top, get_uvalue=info

  IF event.id EQ (*info).bgroupid THEN BEGIN 
    widget_control, (*info).bgroupid, get_value=value
    x = where((*info).mapmap)
    (*info).mapmap[x[0]] =  0
    widget_control, (*info).panes[x[0]], map=0
    widget_control, (*info).panes[value[0]],/map
    (*info).mapmap[value[0]] =  1
    return
  ENDIF 

  IF event.id NE (*info).applyId AND  $
     event.id NE (*info).dismissId AND  $
     event.id NE (*info).ClearId AND  $
     event.id NE (*info).cancelId THEN return

  Widget_control, (*info).parent, Get_Uvalue=self
  self-> Get,Oplot = Oplot
  CASE event.Id OF 
    (*info).ClearId: BEGIN
      widget_Control, (*info).tlb,/destroy
      oplot-> clear
      self-> set,oplot = oplot
      self-> draw,/force
    END 
    (*info).APPLYId: BEGIN
      ;Widget_Control,(*info).XlistId, Get_Value=xlist
      ;Widget_Control,(*info).ylistId, Get_Value=ylist
      ;Widget_Control,(*info).AnnotId, Get_Value=Annot
      Widget_Control,(*info).TableId, Get_Value=Table
      xlist = reform(table[0,*])
      ylist = reform(table[1,*])
      Annot = reform(table[2,*])
      xx = where(strlen(xlist) NE 0, nxx )
      IF nxx NE 0 THEN BEGIN 
        xlist = xlist[xx]
        ylist = ylist[xx]
        Annot = Annot[xx]
        x = where( strlen(annot) NE 0, nx )
        doAnnot = nx NE 0
        Widget_Control,(*info).symId,Get_Value=psym
        Widget_Control,(*info).symsizeId,Get_Value=symsize
        Widget_Control,(*info).ColorId,Get_Value=Color
        Widget_Control,(*info).LinesId,Get_Value=Lines
        Widget_Control,(*info).LineStyleId,Get_Value=Linestyle
        widget_control, (*info).thickid, get_value=thick
        widget_control, (*info).charthickid, get_value=charthick
        widget_control, (*info).charsizeid, get_value=charsize
        color = strcompress(color[0],/remove_all) 

        IF strmid(color,0,1) EQ "'" OR  $
         strmid(color,strlen(color)-2,2) EQ "'x" THEN BEGIN 
          IF strmid(color,0,1) NE "'" THEN color =  "'" + color          
          IF strmid(color,strlen(color)-2,2) NE "'x" THEN $
           color =  strmid(color,0,strlen(color)-2) + "'x"
          s = execute( 'color = ' + color )
        ENDIF ELSE color = long(color)

        IF color GT !d.n_colors-1 THEN BEGIN 
          color = !d.n_colors-1
          Widget_Control,(*info).ColorId,Set_Value=strtrim(color,2)
        ENDIF 

        sign =  ([1,-1])[lines[0]]
        toplot = obj_new('obplot',float(ylist),xarray=float(xlist),$
                         annot=annot,symsize=symsize,color=color,$
                         charsize=charsize, charthick=charthick, $
                         thick=thick, linestyle=linestyle, $
                         psym=sign*psym, doAnnot=doAnnot,/plots)
        self-> Get,wid = wid
        wset,wid
        toplot-> draw
        destroy,toplot
      ENDIF 

    END
    (*info).DISMISSid: BEGIN
      ;Widget_Control,(*info).XlistId, Get_Value=xlist
      ;Widget_Control,(*info).ylistId, Get_Value=ylist
      ;Widget_Control,(*info).AnnotId, Get_Value=Annot
      Widget_Control,(*info).TableId, Get_Value=Table
      xlist = reform(table[0,*])
      ylist = reform(table[1,*])
      Annot = reform(table[2,*])
      xx = where(strlen(xlist) NE 0, nxx )
      IF nxx NE 0 THEN BEGIN 
        xlist = xlist[xx]
        ylist = ylist[xx]
        Annot = Annot[xx]
        x = where( strlen(annot) NE 0, nx )
        doAnnot = nx NE 0
        Widget_Control,(*info).symId,Get_Value=psym
        Widget_Control,(*info).symsizeId,Get_Value=symsize
        Widget_Control,(*info).ColorId,Get_Value=Color
        Widget_Control,(*info).LinesId,Get_Value=Lines
        Widget_Control,(*info).LineStyleId,Get_Value=Linestyle
        widget_control, (*info).thickid, get_value=thick
        widget_control, (*info).charthickid, get_value=charthick
        widget_control, (*info).charsizeid, get_value=charsize
        color = strcompress(color[0],/remove_all)
        IF strmid(color,0,1) EQ "'" OR  $
         strmid(color,strlen(color)-2,2) EQ "'x" THEN BEGIN 
          IF strmid(color,0,1) NE "'" THEN color =  "'" + color          
          IF strmid(color,strlen(color)-2,2) NE "'x" THEN $
           color =  strmid(color,0,strlen(color)-2) + "'x"
          s = execute( 'color = ' + color )
        ENDIF ELSE color = long(color)

        IF color GT !d.n_colors-1 THEN BEGIN 
          color = !d.n_colors-1
          Widget_Control,(*info).ColorId,Set_Value=strtrim(color,2)
        ENDIF 

        sign =  ([1,-1])[lines[0]]
        oplot-> Set,Yarray = float(ylist),$
                     Xarray=float(Xlist),$
                     annot=annot,$
                      symsize=symsize,$
                       color=color,$
                         psym=sign*psym,$
                           doAnnot=doAnnot, $
                            charsize=charsize, $
                             charthick=charthick, $
                              linestyle=linestyle, $
                               thick=thick
        oplot-> draw
        self-> Set,Oplot = Oplot
        Widget_control, (*info).tlb, /destroy
      ENDIF 
    END
    (*info).CANCELid: Widget_control, (*info).tlb, /destroy
  ENDCASE
END


;================================================================
; PV_OPLOT: Widget Definition
;================================================================

PRO pv_oplot, GROUP=parent_TLB, cancel=cancel, mapmap=mapmap


  
  IF n_elements(parent_TLB) EQ 0 THEN BEGIN 
    Usage,"pv_oplot,group=Parent_TLB,cancel=cancel"
    return
  ENDIF 

  IF n_elements(mapmap) EQ 0 THEN mapmap = [1,0,0,0,0]

  Widget_control, parent_TLB, get_uvalue=self

    ;; Get the oplot member object from the PV object
  self-> get,oplot=oplot


  Xarray = oplot-> getXarray()
  Yarray = oplot-> getYarray()
  AnnotArray = oplot-> GetAnnotArray()

    ;; Get the other things form the obPlot object
  oplot-> get,psym = psym,symsize=symsize,color=color, $
          thick=thick, charthick=charthick, charsize=charsize, $
           linestyle=linestyle

  XList = (Ylist = strarr(10))
  IF ptr_valid( Yarray ) THEN BEGIN 
    nn = n_elements(*xarray)
    Ylist1 = string( *Yarray, format='(f7.3)' ) 
    XList1 = string( *Xarray, format='(f7.3)' ) 
    Xlist[0:(nn-1) < 10] = Xlist1
    Ylist[0:(nn-1) < 10] = YList1
  ENDIF 
  AnnotList=strarr(10)    
  IF Ptr_Valid(AnnotArray) THEN BEGIN 
    nn = n_elements(*AnnotArray)
    AnnotList[0:(nn-1) < 10] =  *AnnotArray
  ENDIF 
   

    
    ; AnnotList might not have any values
    ; in it, so becareful to make it the
    ; same size as {XY}List

;  xx = where(strlen(AnnotList) NE 0 ,nxx )
;  nx = n_elements(Xlist)
;  IF nxx lt nx THEN BEGIN 
;    IF nxx EQ 0 THEN BEGIN 
;      AnnotList = strarr(nx) 
;    ENDIF ELSE BEGIN 
;      tt = strarr(nx)
;      tt[0:nxx-1] =  AnnotList
;      AnnotList = temporary(tt)
;    ENDELSE 
;  ENDIF ELSE IF nxx GT nx THEN AnnotList = AnnotList[0:nx-1]


  oplot_tlb = WIDGET_BASE(GROUP_LEADER=parent_tlb, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='OPLOT_TLB')


  names = ['Locations','Symbol Stuff','Line Stuff','Text Stuff', 'Color']
  x = where(mapmap,nx)
  IF nx EQ 0 THEN BEGIN 
    mapmap = [1,0,0,0,0] 
    x = 0 
  ENDIF ELSE x = x[0]

  bgroupid = cw_bgroup(oplot_tlb,names,col=2, $
                       /exclusive, /no_release, $
                      set_value=x)

  base1 = widget_base(oplot_tlb,/map,frame=2)

  tablebase = widget_base( base1, /col, map=mapmap[0] )

  table = transpose([ [XList],[YList],[AnnotList]])

  TableId = Widget_Table( tablebase, alignment=2, /edit, $
                Column_labels=['X Values', 'Y Values', 'Annotations'], $
                xsize=3, ysize=10, y_scroll_size=3,$
                /resizeable_columns, value=table)


  symbase = widget_base(base1,/col,map=mapmap[1])
  SymId = WIDGET_SLIDER( symbase, $
      MAXIMUM=8, $
      MINIMUM=0, $
      TITLE='Symbol', $
      UVALUE='PSYM', $
      VALUE=abs(psym))

  SymSizeId = CW_FSLIDER( symbase, $
      DRAG=1, $
      /edit, $
      xsize=1, $
      MAXIMUM=3.0, $
      MINIMUM=0.0, $
      TITLE='Symbol Size', $
      UVALUE='SYMSIZE', $
      VALUE=symsize)

  ; -=-=-=-=-=-=-=-=-=-=-=-= line stuff -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  linebase = widget_base(base1,/col,map=mapmap[2])
  thickId = WIDGET_SLIDER( linebase, $
      MAXIMUM=8, $
      MINIMUM=0, $
      TITLE='Thick', $
      UVALUE='THICK', $
      VALUE=thick)

  linestyleId = WIDGET_SLIDER( linebase, $
      MAXIMUM=8, $
      MINIMUM=0, $
      TITLE='Line Style', $
      UVALUE='LINESTYLE', $
      VALUE=linestyle)

  YesNo = [ 'No ', 'Yes']

   LinesId = CW_BGROUP( linebase, YesNo, $
      ROW=1, $
      EXCLUSIVE=1, $
      set_value=psym LT 0, $
      LABEL_TOP='Lines Between Symbols?', $
      UVALUE='CONNECTSYMS')


  ; -=-=-=-=-=-=-=-=-=-=-=-= text stuff -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  textbase = widget_base(base1,/col,map=mapmap[3])
  charsizeid = CW_FSLIDER( textbase, $
      DRAG=1, $
      /edit, $
      xsize=1, $
      MAXIMUM=4.0, $
      MINIMUM=0.0, $
      TITLE='Character Size', $
      UVALUE='CHARSIZE', $
      VALUE=charsize)

  charthickId = WIDGET_SLIDER( textbase, $
      MAXIMUM=8, $
      MINIMUM=0, $
      TITLE='Char Thick', $
      UVALUE='CHARTHICK', $
      VALUE=charthick)

  ; -=-=-=-=-=-=-=-=-=-=-=-= color stuff -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  colorbase = widget_base(base1,/col, map=mapmap[4])

  IF color GT 255 THEN BEGIN 
    color = "'" + $
      padandjustify(color,6,pad='0',form='(z)',/right) + $
       "'xl"
  ENDIF ELSE color = strtrim(color,2)


  color = color < (!d.n_colors-1)
  
  ColorId = CW_FIELD( colorbase,VALUE=color, $
      ROW=1, $
      /string, $
      xsize=10, $
      RETURN_EVENTS=1, $
      TITLE='Color', $
      UVALUE='COLOR')


  BASE19 = WIDGET_BASE(OPLOT_TLB, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE19')


  ClearId =  widget_Button( Base19, $
      UVALUE='CLEARFORM', $
      VALUE='Clear Form')                  
  ApplyId = WIDGET_BUTTON( BASE19, $
      UVALUE='APPLY', $
      VALUE='Apply')

  DismissId = WIDGET_BUTTON( BASE19, $
      UVALUE='DISMISS', $
      VALUE='Dismiss')

  CancelId = WIDGET_BUTTON( BASE19, $
      UVALUE='CANCEL', $
      VALUE='Cancel')


  info = ptr_new( { tlb: oplot_tlb, $
                    parent: parent_tlb, $
                    ;XlistId: XlistId, $
                    ;YlistId: YlistId, $
                    ;AnnotListId: AnnotListId, $
                    bgroupid: bgroupid, $
                    mapmap: mapmap, $
                    panes: [tablebase, symbase, linebase, $
                            textbase, colorbase], $
                    
                    TableId: TableId, $
                    SymId: SymId, $
                    SymSizeId: SymSizeId, $
                    thickid: thickid, $
                    linestyleid: linestyleid, $
                    LinesId: LinesId, $
                    charsizeid: charsizeid, $
                     charthickid: charthickid, $
                    ColorId: ColorId, $
                    ClearId: ClearId, $
                    ApplyId: ApplyId, $
                    DismissId: DismissId, $
                    CancelId: CancelId })

  WIDGET_CONTROL, OPLOT_TLB, /REALIZE, set_uval=info
  XMANAGER, 'PV_OPLOT', OPLOT_TLB, event_hand='pv_oplot_event'
  ptr_free, info
END
