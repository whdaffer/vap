; NAME:
;	gms5configurator.pro
;
; PURPOSE:
; CATEGORY:
; CALLING SEQUENCE:
; INPUTS:
; OPTIONAL INPUTS:
; KEYWORD PARAMETERS:
; OUTPUTS:
; OPTIONAL OUTPUTS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; EXAMPLE:
; MODIFICATION HISTORY:
;
;  $Log$
;
;-

PRO gms5configurator_Events, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
  print,'event = ',ev

  IF ev NE 'APPLY' AND  ev NE 'CANCEL' THEN return

  Widget_control, Event.top, get_uvalue=info
  IF Ev EQ 'APPLY' THEN BEGIN 
      widget_control, (*info).topdirid, get_value=v
      v = v[0]
      IF v NE '<unknown>' THEN (*info).topdir =  v[0]
      widget_control, (*info).templateid, get_value=v
      v = v[0]
      IF v NE '<unknown>' THEN (*info).templatefile =  v[0]
      widget_control, event.top, set_uvalue=info
  ENDIF  ELSE (*info).cancel = 1l
  Widget_Control, event.top, /destroy
END


; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



FUNCTION gms5configurator, GROUP=Group


  COMMON gms5_cmn, gms5initialized, gms5_data_topdir, $
    gms5_hdftemplates_saveset_file

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0


  
  TLB = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Qscat Vap/GMS 5 (.qvapgms5rc) File Configurator', $
      UVALUE='TLB')

  IF n_elements( gms5_data_topdir ) NE 0 THEN $
    topdir_field = gms5_data_topdir ELSE $
    topdir_field = ''
  IF topdir_field EQ '' THEN topdir_field = '<unknown>'
  topdirid = CW_FIELD( TLB,VALUE= topdir_field, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='GMS 5 Data Topdir', $
      UVALUE='GMS5TOPDIR')

  IF n_elements( gms5_hdftemplates_saveset_file) NE 0 THEN $
    saveset_field = gms5_hdftemplates_saveset_file ELSE $
    saveset_field = ''
  IF saveset_field EQ '' THEN saveset_field = '<unknown>'
  templateid = CW_FIELD( TLB,VALUE=saveset_field,$
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='HDF Template file', $
      UVALUE='HDFTEMPLATEFILE')

  BASE4 = WIDGET_BASE(TLB, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  applyid = WIDGET_BUTTON( BASE4, $
      UVALUE='APPLY', $
      VALUE='Apply')

  cancelid = WIDGET_BUTTON( BASE4, $
      UVALUE='CANCEL', $
      VALUE='Cancel')


  info = Ptr_new( { topdirid: topdirid,$
                    templateid: templateid, $
                    cancel:0l,$
                    topdir: topdir_field,$
                    templatefile: saveset_field } )
  WIDGET_CONTROL, TLB, $
     set_uvalue=info, $
           /REALIZE

  XMANAGER, 'gms5configurator',tlb, event_handler='gms5configurator_events'

  IF (*info).cancel EQ 0 THEN BEGIN 
    config = ['gms5_data_topdir="' + (*info).topdir + '"', $
              'gms5_hdftemplates_saveset_file="' + (*info).templatefile + '"' ]
  ENDIF ELSE config = ''
  
  ptr_free, info
  return, config
END

