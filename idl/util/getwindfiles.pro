;+
; $Id$
;
; NAME:   GETWINDFILES
; PURPOSE:  Given a end time, a delta or a start time, a path and a
;           filter, find all files within the timerange, in the
;           indicated directory matching the given filter.
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  files=getwindfiles("1998/10/03/02/03",delta=2, path='./',filter='Q*')
;
;
; 
; INPUTS:   
;
;   end_time - yyyy/mm/dd/hh/mm format. It is required
;              that the time must minimally have yyyy,mm,dd fields. If
;              the year field is only two characters long, 1900 will
;              be added to it.  All other fields default to maximal
;              values, i.e. start_time has hh=mm=0, end_time has hh=23
;              and mm=59

;
;
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:  
;
;
;     delta      : Subtract this time from the end
;                  time
;     start_time : time in same format as end_time 
;     path       : the standard unix style path
;     filter     : string, a filter to use in 'findfile'
;     nscat      : flag, if set, expect nscat filenaming convention.
;                  (yymmdd.Shhmm.Ehhmm instead of yyyymmdd.Shh...)
;
; OUTPUTS:  Vector of filennames.
;
;
;
;
; OPTIONAL OUTPUTS:  
;
;     Count - keyword telling howmany files are output
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
; Find wind files within the time range of end_time - delta to
; end_time or start_time to end_time. delta is in hours (a float if
; you want more precision) and is specified positive, since it is
; SUBTRACTED from the end_time. 
; Start/end_time is in yyyy/mm/dd/hh/mm format. It is required
; that the time must  minimally have yyyy,mm,dd fields. If the
; year field is only two characters long, 1900 will be added to it.
; All other
; fields default to maximal values, i.e. start_time has hh=mm=0,
; end_time has hh=23 and mm=59

; delta defaults to 24 hours.
; end_time defaults to current time.
; start_time defaults to current_time - delta.

; Path defaults to VAP_WINDS
; filter defaults to 'path/Q*'
;
; 
; Success: Returns an array of file names 
; Failure: the null string.

;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.1  1998/10/02 23:20:40  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION getwindfiles, end_time, $
                       delta      = delta, $
                       start_time = start_time, $
                       path       = path, $
                       filter     = filter,$
                       count      = count,$
                       nscat      = nscat

  rcsid = "$Id$";

  nscat = keyword_set(nscat)
  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    message,!err_string,/cont
    return,''
  ENDIF 

  now = today();
  IF n_elements(end_time) EQ 0 THEN BEGIN 
    end_time = today();
  ENDIF ELSE BEGIN 
    tmp = str_sep( end_time, '/');
    IF n_Elements(tmp) LT 3 THEN BEGIN 
      Message,'End_time must have at least yyyy/mm/dd',/cont
      return,retarray
    ENDIF 
    yyyy  = fix(tmp[0]) 
    IF yyyy LE 99 THEN yyyy =  yyyy+1900
    month = fix(tmp[1])
    day   = fix(tmp[2])
    IF n_elements(tmp) lt 4 THEN hh = 23 ELSE hh = tmp[3]
    IF n_elements(tmp) LT 5 THEN mm = 59 ELSE mm = tmp[4]
    end_time_dt = var_to_dt( yyyy,month,day,hh,mm)
  ENDELSE 

  IF n_elements(delta) EQ 0 THEN BEGIN 
    IF n_elements(start_time) EQ 0 THEN BEGIN 
      start_time_dt =  dt_subtract( end_time_dt, hour=24 )
    ENDIF ELSE BEGIN 
      tmp = str_sep( start_time, '/')
      yyyy  = fix(tmp[0]) 
      IF yyyy LE 99 THEN yyyy =  yyyy+1900
      month = fix(tmp[1])
      day   = fix(tmp[2])
      IF n_elements(tmp) lt 4 THEN hh = 0 ELSE hh = tmp[3]
      IF n_elements(tmp) LT 5 THEN mm = 0 ELSE mm = tmp[4]
      start_time_dt = var_to_dt( yyyy,month,day,hh,mm)
    ENDELSE 
  ENDIF ELSE BEGIN 
    start_time_dt =  dt_subtract( end_time_dt, hour=delta )
  ENDELSE 

  IF n_elements(path) EQ 0 THEN path =  Get_Env('VAP_WINDS')
  IF strlen(path) EQ 0 THEN BEGIN 
    message,"Can't get_env(VAP_WINDS), defaulting to '/disk3/winds'",/cont
    path = '/disk3/winds'
  ENDIF 
   
  IF n_elements(filter) EQ 0 THEN filter =  'Q*'
  IF nscat THEN filter = 'N*'
    ;-----------------------------------------
    ;
    ; Begin processing
    ;
    ;-----------------------------------------

  filespec = path + '/' + filter
  windfiles = findfile(filespec, count=cnt)
  retarray = ''

  IF cnt NE 0 THEN BEGIN 
      ; Output is {name:'', start_time:{idldt}, end_time:{idldt} }
    windfiletimes =  wfnames2dt( windfiles, nscat=nscat )

    IF VarType(Windfiletimes) EQ 'STRUCTURE' THEN BEGIN 

      files = windfiletimes.name
      file_start_times =  windfiletimes.start_time
      file_end_times = windfiletimes.end_time

      idx =  dtcompare( start_time_dt, file_start_times, 'GE') OR  $
             dtcompare( end_time_dt, file_end_times, 'LE') 

      idx=where(idx,nx)
      IF nx NE 0 THEN BEGIN 
        retarray = files(idx) 
        count = nx
      ENDIF ELSE BEGIN 
        retarray = ''
        count = 0
      ENDELSE 
    ENDIF ELSE $
     Message,"Bad return from wfnames2dt",/cont

  ENDIF ELSE $
    Message,"Can't find any wind files in " + path + "/Q*",/cont
   
return, retarray
END



