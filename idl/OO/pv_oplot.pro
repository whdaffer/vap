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
                           doAnnot=doAnnot
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

PRO pv_oplot, GROUP=parent_TLB, cancel=cancel


  IF n_elements(parent_TLB) EQ 0 THEN BEGIN 
    Usage,"pv_oplot,group=Parent_TLB,cancel=cancel"
    return
  ENDIF 

  Widget_control, parent_TLB, get_uvalue=self
  self-> get,oplot=oplot

  oplot_tlb = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='OPLOT_TLB')

  BASE2 = WIDGET_BASE(OPLOT_TLB, $
      COLUMN=3, $
      MAP=1, $
      UVALUE='BASE2')


  Xarray = oplot-> getXarray()
  Yarray = oplot-> getYarray()
  AnnotArray = oplot-> GetAnnotArray()
  oplot-> get,psym = psym,symsize=symsize,color=color
  IF ptr_valid( Yarray ) THEN BEGIN 
    Ylist = string( *Yarray, format='(g20.10)' ) 
    XList = string( *Xarray, format='(g20.10)' ) 
  ENDIF ELSE $
    XList = (Ylist = strarr(10))
  IF Ptr_Valid(AnnotArray) THEN $
   AnnotList=*AnnotArray ELSE $
   AnnotList=strarr(10)

    
    ; AnnotList might not have any values
    ; in it, so becareful to make it the
    ; same size as {XY}List

  xx = where(strlen(AnnotList) NE 0 ,nxx )
  nx = n_elements(Xlist)
  IF nxx lt nx THEN BEGIN 
    IF nxx EQ 0 THEN BEGIN 
      AnnotList = strarr(nx) 
    ENDIF ELSE BEGIN 
      tt = strarr(nx)
      tt[0:nxx-1] =  AnnotList
      AnnotList = temporary(tt)
    ENDELSE 
  ENDIF ELSE IF nxx GT nx THEN AnnotList = AnnotList[0:nx-1]

  table = transpose([ [XList],[YList],[AnnotList]])

  TableId = Widget_Table( base2, alignment=2, /edit, $
                Column_labels=['X Values', 'Y Values', 'Annotations'], $
                xsize=3, ysize=10, y_scroll_size=3,$
                /resizeable_columns, value=table)

  BASE10 = WIDGET_BASE(OPLOT_TLB, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE10')

  SymId = WIDGET_SLIDER( BASE10, $
      MAXIMUM=8, $
      MINIMUM=0, $
      TITLE='Symbol', $
      UVALUE='PSYM', $
      VALUE=abs(psym))

  SymSizeId = CW_FSLIDER( BASE10, $
      DRAG=1, $
      /edit, $
      MAXIMUM=3.00000, $
      MINIMUM=0.00000, $
      TITLE='Symbol Size', $
      UVALUE='SYMSIZE', $
      VALUE=symsize)

  IF color GT 255 THEN BEGIN 
    color = "'" + $
      padandjustify(color,6,pad='0',form='(z)',/right) + $
       "'x"
  ENDIF ELSE color = strtrim(color,2)

  
  IF color GT !d.n_colors-1 THEN $
    color = !d.n_colors-1
  
  ColorId = CW_FIELD( BASE10,VALUE=color, $
      ROW=1, $
      /string, $
      RETURN_EVENTS=1, $
      TITLE='Color', $
      UVALUE='COLOR')


  BASE19 = WIDGET_BASE(OPLOT_TLB, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE19')

  YesNo = [ 'No ', 'Yes']

   LinesId = CW_BGROUP( BASE19, YesNo, $
      ROW=1, $
      EXCLUSIVE=1, $
      set_value=psym LT 0, $
      LABEL_TOP='Lines Between Symbols?', $
      UVALUE='CONNECTSYMS')

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
                    TableId: TableId, $
                    SymId: SymId, $
                    SymSizeId: SymSizeId, $
                    LinesId: LinesId, $
                    ColorId: ColorId, $
                    ClearId: ClearId, $
                    ApplyId: ApplyId, $
                    DismissId: DismissId, $
                    CancelId: CancelId })

  WIDGET_CONTROL, OPLOT_TLB, /REALIZE, set_uval=info
  XMANAGER, 'PV_OPLOT', OPLOT_TLB, event_hand='pv_oplot_event'
  ptr_free, info
END
