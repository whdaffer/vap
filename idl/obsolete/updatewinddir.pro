;+
; NAME:  Updatewinddit
; $Id$
; PURPOSE:  
;
;
; AUTHOR:
;
;
; CATEGORY:  Qscat VAP testing
;
;
;
; CALLING SEQUENCE:  
;
;
; 
; INPUTS:  
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;
;
; OUTPUTS:  
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;
;
; SIDE EFFECTS:  
;
;
;
; RESTRICTIONS:  
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO updatewinddir

  cd,current=curdir
  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    cd,curdir
    return
  ENDIF 
  cd,'$VAP_WINDS'

  date = TodayAsString(sep='/')

  sepdate = str_sep(date,'/')
  baseyear = sepdate[0]
  basemonth = sepdate[1]
  baseday = sepdate[2]
  year = fix(baseyear)
  month = fix(basemonth)
  day = fix(baseday)

    ; find the fake files
  fake_hdf = './fake_hdf/QS*'
  fake_hdf_files = findfile(fake_hdf,count=nfakehdffiles)
  fake_hdf_wfdt = wfnames2dt( fake_hdf_files )

  IF nfakehdffiles EQ 0 THEN $
    Message,'No fake hdf files found using' + $
      fake_hdf,/noprint,/noname,/noprefix

    ; Check for the today's files 
  test_string = './QS'+baseyear+basemonth+baseday+'.S*.E*'
  hdf_files = findfile(test_string,count=cnt)
  IF cnt NE 0 THEN $
    existing_wfdt = wfnames2dt( hdf_files )
  IF cnt lt nfakehdffiles THEN BEGIN 
    FOR i=0,nfakehdffiles-1 DO BEGIN 
      print,'Working with ',  fake_hdf_files[i]
      wfdt = fake_hdf_wfdt[i]
      new_wfdt = wfdt
      new_wfdt.start_time.year = year
      new_wfdt.start_time.month = month
      new_wfdt.start_time.day = day
      new_wfdt.start_time.recalc = 1
      new_wfdt.end_time.year = year
      new_wfdt.end_time.month = month
      new_wfdt.end_time.day = day
      new_wfdt.end_time.recalc = 1
      IF new_wfdt.start_time.julian GT new_wfdt.end_time.julian THEN BEGIN 
        tmp = dt_add( new_wfdt.end_time, day=1)
        new_wfdt.end_time = temporary(tmp)
      ENDIF 
      filename = dt2wfnames(new_wfdt)
      IF cnt EQ 0 THEN BEGIN 
        RecrateHdfData, filename, fake_hdf_files[i],$
           new_wfdt.start_time, new_wfdt.end_time
      ENDIF ELSE BEGIN 
        ; If there's one with the same start/end time, skip it.
        x = where( existing_wfdt.start_time.hour EQ wfdt.start_time.hour AND $
                   existing_wfdt.start_time.minute  EQ wfdt.start_time.minute AND $
                   existing_wfdt.end_time.hour   EQ wfdt.end_time.hour AND $
                   existing_wfdt.end_time.minute    EQ wfdt.end_time.minute, nx )
        IF nx NE 0 THEN BEGIN 
          RecrateHdfData, filename, fake_hdf_files[i],$
           new_wfdt.start_time, new_wfdt.end_time
        ENDIF 
      ENDELSE 
    ENDFOR 
  ENDIF 


    ; Check for the yesterday's files 
  idldt = vaptime2idldt( date )
  idldt = dt_subtract( idldt, sec=86400 )
  year = idldt.year
  month = idldt.month
  day = idldt.day

  date =  idldt2vaptime( idldt )
  tmp = str_sep(date,'/')
  test_string = './QS'+tmp[0]+tmp[1]+tmp[2]+'.S*.E*'
  hdf_files = findfile(test_string,count=cnt)

  IF cnt NE 0 THEN $
    existing_wfdt = wfnames2dt( hdf_files )

  IF cnt lt nfakehdffiles THEN BEGIN 
    FOR i=0,nfakehdffiles-1 DO BEGIN 
      print,'Working with ',  fake_hdf_files[i]
      wfdt = fake_hdf_wfdt[i]
      new_wfdt = wfdt
      new_wfdt.start_time.year = year
      new_wfdt.start_time.month = month
      new_wfdt.start_time.day = day
      new_wfdt.start_time.recalc = 1
      new_wfdt.end_time.year = year
      new_wfdt.end_time.month = month
      new_wfdt.end_time.day = day
      new_wfdt.end_time.recalc = 1
      IF new_wfdt.start_time.julian GT new_wfdt.end_time.julian THEN BEGIN 
        tmp = dt_add( new_wfdt.end_time, day=1)
        new_wfdt.end_time = temporary(tmp)
      ENDIF 
      filename = dt2wfnames(new_wfdt)
      IF cnt EQ 0 THEN BEGIN 
        RecrateHdfData, filename, fake_hdf_files[i],$
           new_wfdt.start_time, new_wfdt.end_time
      ENDIF ELSE BEGIN 
        ; If there's one with the same start/end time, skip it.
        x = where( existing_wfdt.start_time.hour EQ wfdt.start_time.hour AND $
                   existing_wfdt.start_time.minute  EQ wfdt.start_time.minute AND $
                   existing_wfdt.end_time.hour   EQ wfdt.end_time.hour AND $
                   existing_wfdt.end_time.minute    EQ wfdt.end_time.minute, nx )
        IF nx NE 0 THEN BEGIN 
          RecrateHdfData, filename, fake_hdf_files[i],$
           new_wfdt.start_time, new_wfdt.end_time
        ENDIF 
      ENDELSE 
    ENDFOR 
  ENDIF 
    
     ; check for the day-before-yesterday's files
     ; delete any found

  lf =  string(10b)
  idldt = dt_subtract( idldt, sec=86400 )
  date =  idldt2vaptime( idldt )
  tmp = str_sep(date,'/')
  test_string = './QS'+tmp[0]+tmp[1]+tmp[2]+'.S*.E*'
  hdf_files = findfile(test_string,count=cnt)
  IF cnt NE 0 THEN BEGIN 
    exe_str =  'rm -f ' 
    FOR i=0,cnt-1 DO exe_str =  exe_str + hdf_files[i] + ' '
    print, 'Deleting files with string ' + lf + $
     exe_str
    spawn, exe_str, ret
    IF strlen(ret[0]) NE 0 THEN BEGIN 
       Message,'Error deleting files',/cont
       print,' return: ' + ret
       return
    ENDIF 
       
  ENDIF 

END


