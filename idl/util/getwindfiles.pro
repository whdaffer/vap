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
; CALLING SEQUENCE:  files=getwindfiles("yyyy/mm/dd/hh/mm",$
;                                        delta=delta, path=path,$
;                                        filter=filter,/twoway)

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
;     delta      : Delta time from 'end_time' 
;                  If flag 'twoway' is set, add and subtract 
;                  this time. Otherwise, simply subtract it.
;     start_time : time in same format as end_time. Can be used to
;                  address arbitary time ranges.
;     path       : the standard unix style path
;     filter     : string, a filter to use in 'findfile'
;     nscat      : flag, if set, expect nscat filenaming convention.
;                  (yymmdd.Shhmm.Ehhmm instead of yyyymmdd.Shh...)
;     twoway    : flag. If set, add and subtract
;                  delta. I.e. 'endtime' is really a 'middletime'
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
; Find wind files within the time range of end_time - delta  (or
; end_time +/- delta, which make it a 'middletime) to
; end_time or start_time to end_time. delta is in hours (a float if
; you want more precision) and is specified positive.
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
; Revision 1.4  1999/04/09 15:39:24  vapuser
; Took out some calls to dtcompare.pro, used
; built in idldt.julian instead
;
; Revision 1.3  1998/10/17 00:16:24  vapuser
; Fixed broken dtcompare call
;
; Revision 1.2  1998/10/06 00:21:30  vapuser
; added nscat flag
;
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
                       nscat      = nscat, $
                       twoway    = twoway

  rcsid = "$Id$";

  nscat = keyword_set(nscat)
  twoway = keyword_set(twoway)
  IF n_elements(delta) EQ 0 THEN delta = 24

  delta_mins =  0
  delta_hours = fix(delta)
  IF isa(delta,/float) THEN delta_mins = fix(delta-delta(hours))*60

  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    message,!err_string,/cont
    return,''
  ENDIF 

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

  IF n_elements(start_time) EQ 0 THEN BEGIN 
    start_time_dt =  dt_subtract( end_time_dt, hour=delta_hours, min=delta_mins)
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

 IF twoway THEN end_time_dt=dt_add(end_time_dt,hour=delta_hours, min=delta_mins)

  IF n_elements(path) EQ 0 THEN path =  GetEnv('VAP_WINDS')
  IF strlen(path) EQ 0 THEN BEGIN 
    message,"Can't get_env(VAP_WINDS), defaulting to '/disk3/winds'",/cont
    path = '/disk3/winds'
  ENDIF 
   
  IF n_elements(filter) EQ 0 THEN BEGIN 
    IF nscat THEN filter = 'N*' ELSE filter =  'Q*'
  ENDIF 
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

      idx1 =  where( file_start_times.julian le end_time_dt.julian,nx)
      IF nx NE 0 THEN BEGIN 
        idx=where( file_end_times[idx1].julian ge start_time_dt.julian,nx ) 

        IF nx NE 0 THEN BEGIN 
          retarray = files(idx1[idx]) 
          count = nx
        ENDIF ELSE BEGIN 
          retarray = ''
          count = 0
        ENDELSE 
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



