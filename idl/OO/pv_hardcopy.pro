;+
; NAME: pv_hardcopy
; $Id$
; PURPOSE: Defines and manages widget to accept hardcopy configuration
;          options.
;
; CATEGORY: PV utility
;
; CALLING SEQUENCE: 
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
;-

;FUNCTION addext_events, event
;  retevent = 0
;  Widget_Control, Event.top, Get_Uvalue=info
;  (*info).AddExt = (event.index EQ 0)
;  return,retevent
;END

;FUNCTION uniq_events, event
;  retevent = 0
;  Widget_Control, Event.top, Get_Uvalue=info
;  (*info).Uniqueify = (event.index EQ 0)
;  IF (*info).Uniqueify EQ 1 THEN BEGIN 
;    Widget_Control, (*info).pathId, Get_Value=path
;    Widget_Control, (*info).BasenameId, Get_Value=basename
;    Widget_Control (*info).ExtId, Get_Value = Extention
;    testfilename = path + basename + '*'
;    IF (*info).AddExt EQ 1 THEN $
;      testfilename = testfilename + Extention
;    f = findfile(testfilename,count=cnt)
;    IF count GT 0 THEN BEGIN 
;      date = todayasstring()
;      testfilename = path+basename+'.'+date
;      IF (*info).AddExt EQ 1 THEN $
;       testfilename = testfilename + Extention
;      f = findfile(testfilename,count=cnt)
;      IF count GT 0 THEN BEGIN 
      
      
    
;  return,retevent
;END

FUNCTION Pvhc_update_filelabel, LabelId, path, basename, extention
   fullfilename = Path[0] + basename[0] + '.' + extention[0]
   Widget_Control, LabelId, Set_Value=fullfilename
   return, fullfilename 
END

FUNCTION pv_hardcopy_hctype_bgroup_events,event
   IF event.select NE 1 THEN return,0
   Widget_control, event.top, get_uvalue=info
   Widget_control, (*info).parent, get_uvalue=self
   Widget_Control, (*info).basenameId, Get_Value=basename
   Widget_Control, (*info).pathId, Get_Value=Path
   CASE strupcase(event.value) OF 
     'PS': begin 
       self-> set,HCType = 'ps'
        (*info).PsSensitive = 1
        Widget_Control, (*info).ConfigPsId, Sensitive=1
        junk = pvhc_update_filelabel((*info).LabelId, path,basename,'ps')
     END 
     'GIF' : BEGIN 
       self-> Set,HCType = 'Gif'
        (*info).PsSensitive = 0
        Widget_Control, (*info).ConfigPsId, Sensitive=0
        junk = pvhc_update_filelabel((*info).LabelId, path,basename,'gif')
     END 
     'JPEG': BEGIN 
       self-> Set,HCType = 'Jpeg'
        (*info).PsSensitive = 0
        Widget_Control, (*info).ConfigPsId, Sensitive=0
        junk = pvhc_update_filelabel((*info).LabelId, path,basename,'jpeg')
     END 
     'PICT': BEGIN 
       self-> Set,HCType = 'Pict'
        (*info).PsSensitive = 0
        Widget_Control, (*info).ConfigPsId, Sensitive=0
        junk = pvhc_update_filelabel((*info).LabelId, path,basename,'pict')
     END 
     ELSE:
   ENDCASE
   Widget_Control, (*info).parent, Set_uvalue=self
   widget_control, event.top, set_uvalue=info
  return,0
END

;---------------------------------------------------------
;
; PV_Hardcopy Main Event Handler Routine
;
;---------------------------------------------------------

PRO PV_HARDCOPY_Events, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Descript
  Widget_Control, Event.top, Get_Uvalue=info
  Widget_Control, (*info).parent, get_uvalue=self
  self-> get, HCType = HCType, $
   OutputPath=OutputPath, HCFile=HCFile
  (*info).Cancel = 0
  Widget_Control, (*info).BasenameId, get_Value=basename
  Widget_Control, (*info).PathId, Get_Value=path
  ext = strlowcase(HCType)
  CASE Descript OF 
    'CANCEL': BEGIN
      (*info).Cancel = 1
      Widget_Control, Event.top, Set_Uvalue=(*info)
      Widget_Control,Event.top,/Destroy
    END
    'ACCEPT': BEGIN 
      fullfilename = pvhc_update_filelabel((*info).LabelId, path,basename,ext)
      self-> Set,HCfile = basename, OutputPath=path
      Widget_Control, (*info).parent, Set_uValue=self
      Widget_Control, Event.top, Set_Uvalue=(*info)
      Widget_Control,Event.top,/Destroy
    END
    'BASENAME': BEGIN 
      Widget_Control,(*info).BasenameId, Get_Value=basename
      fullfilename = pvhc_update_filelabel((*info).LabelId, path,basename,ext)
      p = rstrpos( fullfilename, '/')+1
      filename = strmid( fullfilename, p, strlen(fullfilename)-p)
      self-> Set,HCFile = filename
    END
    'PATH':BEGIN
      Widget_Control,(*info).PathId, Get_Value=Path
      fullfilename = pvhc_update_filelabel((*info).LabelId, path,basename,ext)
      self-> Set,OutputPath = path
    END
    'CONFIGPS': BEGIN 
      Widget_Control, (*info).BasenameId, get_Value=filename
      Widget_Control, (*info).PathId, Get_Value=path
      self-> Set,HCfile = filename, OutputPath=path
      self-> get, psInfo = psInfo
      psInfo-> set,parent = (*info).Tlb
      psInfo->setup
      psInfo-> Get, filename = filename
      junk = rstrpos( filename, '/' )+1
      IF junk EQ 0 THEN BEGIN 
        path = './'
        file = strmid( filename, 0, ext )
      ENDIF ELSE BEGIN 
        path = strmid( filename, 0, junk )
        file = strmid( filename, junk, strlen(filename)-junk)
      ENDELSE 
      ext = strpos(file,'.ps')
      file = strmid( file, 0, ext )
      self-> Set, OutputPath = path, HCFile=file, psInfo=psInfo
      Widget_Control, (*info).BasenameId, Set_Value=file
      Widget_Control, (*info).PathId, Set_Value=path
      fullfilename = pvhc_update_filelabel((*info).LabelId,path,file,'ps')  
    END 
    ELSE:
  ENDCASE 
END

;---------------------------------------------------------
;
; PV_Hardcopy Definition Routine
;
;---------------------------------------------------------

PRO pv_hardcopy, PARENT=parent, Cancel=Cancel



  IF n_elements(parent) eq 0 THEN BEGIN   
    Message,'Usage: pv_hardcopy, parent=parent_TLB, Cancel=cancel',/cont
    return
  ENDIF 

  Widget_Control, parent, Get_Uvalue=self
    
  TLB = WIDGET_BASE( GROUP_LEADER=Parent, $
      ROW=1, $
      MAP=1, $
      TITLE='Hardcopy Output Configurator.', $
      UVALUE='TLB',$
      Modal=keyword_set(parent) )

  BASE2 = WIDGET_BASE(TLB, $
      COLUMN=2, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      FRAME=2, $
      MAP=1, $
      UVALUE='BASE3')

  LABEL4 = WIDGET_LABEL( BASE3, $
      UVALUE='LABEL4', $
      VALUE='Hardcopy Type')

  HCTypes = [ $
    'Gif', $
    'Jpeg', $
    'Pict', $
    'ps' ]

  self-> Get,HCType = HCType
  junk = strupcase(HCTypes)
  junk = where( strpos( junk, strupcase(HCType) ) NE -1, njunk)
  initial_index = 0
  IF njunk NE 0 THEN initial_index = junk[0] 

  PSSensitive = (strupcase( HCTypes[ initial_index ] ) EQ 'PS' )

  HCTypeBgroupId = CW_BGROUP( BASE3, HCTypes, $
      COLUMN=1, $
      EXCLUSIVE=1, $
      UVALUE='HCTYPEBGROUP', $
      Set_Value=initial_index,$
      /Return_Name, event_funct='PV_HARDCOPY_HCTYPE_BGROUP_EVENTS')


  BASE18 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE18')

  BASE21 = WIDGET_BASE(BASE18, $
      Col=1, $
      FRAME=2, $
      MAP=1, $
      UVALUE='BASE21')


;  Base22 =  Widget_Base(base21, Col=1,/Map)
;  AddExtId = Widget_DropList( base22, TITLE='Extention Options',$
;                              Value=['Add Extention',"Don't Add Extention"],$
;                              Uvalue='ADDEXTENTION', Event_funct='AddExtId_events')


;  UniqId = Widget_DropList( base22, TITLE='Unique-ify Filename',$
;                              Value=['Make Filenames Unique',"Don't"],$
;                              Uvalue='UNIQUE-IFY',Event_func='Uniq_events')

;  date = todayasstring()
;  IF strpos( HCFile, date) EQ -1 THEN $
;    basename = HCFile + '.' + date $
;  ELSE $
;    basename =  HCFile

;  f = findfile(OutputPath + basename + '*',count=cnt)
;  IF cnt GT 0 THEN $
;   basename = basename + '.' + strtrim( cnt,2 )

  Base23 = Widget_Base( Base21, Col=1,/Map)
  self-> Get,HCfile = HCFile, OutputPath=OutputPath
  
  PathId = CW_FIELD( Base23,VALUE=OutputPath, $
      ROW=1, $
      STRING=1, $
      /Return_events, $
      TITLE='Path', $
      UVALUE='PATH')

  
  BasenameId = CW_FIELD( Base23,VALUE=HCFile, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Basename', $
      UVALUE='BASENAME')


  fullfilename = OutputPath + HCFile + '.' + HCType
  LabelId = Widget_Label( Base23, Value=fullfilename)


  BASE24 = WIDGET_BASE(BASE18, $
      ROW=1, $
      FRAME=2, $
      MAP=1, $
      UVALUE='BASE22')

  ConfigPSId = WIDGET_BUTTON( BASE24, $
      UVALUE='CONFIGPS', $
      VALUE='Configure Postscript')



  base23 = Widget_base( base18, row=1, frame=2, /map )

  CancelId = Widget_button( base23, Uvalue='CANCEL', VALUE='Cancel')
  AcceptId = Widget_button( base23, Uvalue='ACCEPT', VALUE='Accept')

  WIDGET_CONTROL, TLB, /REALIZE
  Widget_Control, ConfigPsId, Sensitive=PSSensitive
  info =  Ptr_New( { Tlb            : tlb, $
                 Parent         : Parent, $
                 PsSensitive    : PsSensitive,$
                 HCTypeBgroupId : HCTypeBgroupId,$
                 basenameId     : basenameId,$
                 PathId         : PathId,$
                 LabelId        : LabelId,$
;                 MiscConfigId   : MiscConfigId,$
;                 AddExtId       : AddExtId,$
;                 UniqId         : UniqId,$
;                 AddExt         : -1l,$
;                 Uniqueify      : -1l,$
                 ConfigPsId     : ConfigPsId,$
                 CancelId       : CancelId,$
                 Cancel         : 0L ,$
                 AcceptId       : AcceptId } )

  
  Widget_Control, Tlb, Set_Uvalue= info
      
  XMANAGER, 'PV_HARDCOPY', tlb, event_handler='PV_HARDCOPY_EVENTS'
  Cancel = (*info).Cancel

END
