PRO cw_pvobj_form_element_cleanup,id
  Widget_Control, id, get_uvalu=info
  Ptr_free, info
END

FUNCTION cw_pvobj_form_element_events, event
   retevent = 0
   Widget_Control, Widget_Info(event.handler,/Child), Get_Uvalue=info
   CASE event.id OF 
     (*info).PlotFlagId: BEGIN 
       (*info).PlotFlag =   ((*info).PlotFlag +1 ) MOD 2
       p = (*info).PlotFlag
       Widget_Control, (*info).PlotFlagId, Set_Value=(*info).CheckBoxArr[*,*,p]
       print, 'flag = ',p
     END
     (*info).SelectId: BEGIN 
       Widget_Control, (*info).SelectId, Sensitive=0
       (*info).name = (*info).name + '(S)'
       Widget_Control, (*info).Textid, Set_Value=(*Info).name
     END
   ENDCASE 
   return,retevent
END

FUNCTION cw_pvobj_form_element_get_value, id
  Widget_Control, Widget_Info(id,/Child), Get_Uvalue=info
  return, [(*info).plotFlag, (*info).SelectFlag]
END


Pro cw_pvobj_form_element_set_value,id, value
  Widget_Control, Widget_Info(id,/Child), Get_Uvalue=info
  Widget_Control, (*info).PlotFlagId, Set_Value=(*info).checkboxArr[*,*,value[0] < 1]
  (*info).plotflag = value[0]
  IF (*info).SelectFlag EQ 0 THEN BEGIN 
    Widget_Control, (*info).SelectFlagId, Set_Value=(*info).checkboxArr[*,*,value[1] < 1]
    (*info).SelectFlag = value[0]
  ENDIF 
END


FUNCTION cw_pvobj_form_element, tlb, name, plotflag, selectflag, $
                                length=length, Height=Height
  lf = string(10b)
  IF n_params() NE 4 THEN BEGIN 
    Message,"Usage: " + lf + $
     "  cwtlb=cw_pvobj_form_element( tlb, name, plotflag, selectflag $" + lf + $
     "         [, length=length, height=height])",/cont
    return,0
  ENDIF 

  name =  strtrim(name,2)
  IF selectFlag THEN name =  name + '(S)'
  BitmapDir = GetEnv('PV_BITMAP_DIR')
  IF strlen(bitmapdir) EQ 0 THEN bitmapdir = '/usr/people/vapuser/Qscat/Resources/'
  checked   = cvtToBm(Read_BMP(bitmapdir + 'checked.bm'))
  unchecked = CvtToBm(Read_BMP(bitmapdir + 'unchecked.bm'))
  sz = size(checked,/dimensions)

  checkboxArr = [ [[unchecked]], [[checked]] ]

  IF n_Elements(length) EQ 0 THEN BEGIN 
    length =  strlen( name ) + 4*sz[0]
  ENDIF  
  IF n_Elements(height) EQ 0 THEN $
   height = max( [!d.y_ch_size*2, sz[1] ] )

  IF length LT strlen( name ) + 4*sz[0] THEN $
    length = strlen( name ) + 4*sz[0] 

  IF height LT max( [!d.y_ch_size*2, sz[1] ] ) THEN  $
     height = max( [!d.y_ch_size*2, sz[1] ] )

  cwtlb = Widget_Base(tlb, /row,/map,$; scr_xsize=length, scr_ysize=height,/row,/map,$
                      event_fun='cw_pvobj_form_element_events', $
                     kill_notify='cw_pvobj_form_element_cleanup')
  textid = Widget_Label( cwtlb, Value=name, frame=2)
  PlotFlagid = Widget_Button( cwtlb, Value=checkboxArr[*,*, PlotFlag < 1 ], frame=2)
  SelectId = Widget_Button( cwtlb, Value=checkboxArr[ *,*,Selectflag < 1 ], frame=2)
  
  Widget_Control, cwtlb, /realize

  Widget_Control, textid, Set_uval=Ptr_New( { tlb: tlb, $
                                              cwtlb:cwtlb, $
                                              textid: textid, $
                                              PlotFlagId: PlotFlagId,$
                                              SelectId: SelectId, $
                                              checkboxArr: CheckboxArr,$
                                              name:name ,$
                                              plotFlag:PlotFlag, $
                                              SelectFlag: SelectFlag} )
  Widget_Control, SelectId, Sensitive=SelectFlag < 1

  return, cwtlb
END

                                                       
  

