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
;     path       : string(s): the standard unix style path
;                  If more than one path, the array is iterated over
;                  and the results concatenated. 
;     filter     : string(s), a filter to use in 'vfindfile'
;                  If more than one filter, the array is interated
;                  over and the results concatenated together.
;     twoway    : flag. If set, add and subtract
;                  delta. I.e. 'endtime' is really a 'middletime'
;
;     The windfiles array is arranged with path then filter, so all
;     the files in one directory will be in one block of the array.
;    
;     The user should be aware, however, that some routines can't
;     handle huge quantities of data. Linkimage has a hard coded limit
;     and concatenating together the results of too many combinations
;     of path/filter may exceed some limits.
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
; filter defaults to 'path/{QS,SW}*'
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
; Revision 1.9  2002/05/03 01:11:34  vapdev
; Changed these support files to use multiple paths/filters (in case
; we want to segregate QuikSCAT/ADEOS streams). Modifed env variables to
; reflect new schema. Just cosmetic work on the swath routines.
;
; Revision 1.8  2001/12/08 00:02:37  vapdev
; Getting rid of obsolete RSI routines and fixing ENV vars
;
; Revision 1.7  1999/10/06 16:00:07  vapuser
; Call 'vfindfile' instead of 'findfile' to handle findfile's problems
; with too many files in one directory.
;
; Revision 1.6  1999/10/05 17:18:52  vapuser
; Changed delta(hours) to delta_hours.
;
; Revision 1.5  1999/09/22 18:02:30  vapuser
; Added 'twoway' keyword to tell it to add and subtract a delta.
;
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
                       twoway    = twoway

  rcsid = "$Id$";

  twoway = keyword_set(twoway)
  IF n_elements(delta) EQ 0 THEN delta = 24

  delta_mins =  0
  delta_hours = fix(delta)
  IF isa(delta,/float) THEN delta_mins = fix(delta-delta_hours)*60

  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    message,!err_string,/cont
    return,''
  ENDIF 

  IF n_elements(end_time) EQ 0 THEN BEGIN 
    end_time = today();
  ENDIF ELSE BEGIN 
    tmp = strsplit( end_time, '/',/extract);
    IF n_Elements(tmp) LT 3 THEN BEGIN 
      Message,'End_time must have at least yyyy/mm/dd',/cont
      return,''
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
    tmp = strsplit( start_time, '/',/extract)
    yyyy  = fix(tmp[0]) 
    IF yyyy LE 99 THEN yyyy =  yyyy+1900
    month = fix(tmp[1])
    day   = fix(tmp[2])
    IF n_elements(tmp) lt 4 THEN hh = 0 ELSE hh = tmp[3]
    IF n_elements(tmp) LT 5 THEN mm = 0 ELSE mm = tmp[4]
    start_time_dt = var_to_dt( yyyy,month,day,hh,mm)
  ENDELSE 

 IF twoway THEN end_time_dt=dt_add(end_time_dt,hour=delta_hours, min=delta_mins)

  IF n_elements(path) EQ 0 THEN BEGIN 
    path =  GetEnv('VAP_DATA_TOP')
    IF strlen(path) EQ 0 THEN BEGIN 
      message,"Can't GETENV(VAP_DATA_TOP), pass path explicitly or set VAP_DATA_TOP!",/cont
      return,''
    ENDIF 
  ENDIF 

  path = deenvvar(path)
  FOR i=0,n_elements(paths)-1 DO BEGIN 
    IF strpos(path[i],'/',/reverse_search) NE strlen(path[i])-1 THEN $
       path[i] = path[i] + '/'
  ENDFOR 
   
  IF n_elements(filter) EQ 0 THEN filter =  "{QS,SW}*"


    ;-----------------------------------------
    ;
    ; Begin processing
    ;
    ;-----------------------------------------

  FOR p=0,n_elements(path)-1 DO BEGIN 
    FOR f=0,n_elements(filter)-1 DO BEGIN 
      tmpfiles = vfindfile(filter[f],path[p],count=cnt)
      IF cnt GT 0 THEN BEGIN 
        tmpfiles = deenvvar(path[p]) + tmpfiles
        IF n_elements(windfiles) EQ 0 THEN $
          windfiles = tmpfiles ELSE $
          windfiles = [windfiles,tmpfiles]
        tmpfiles = 0
      ENDIF 
    ENDFOR 
  ENDFOR 
  retarray = ''

  IF cnt NE 0 THEN BEGIN 
      ; Output is {name:'', start_time:{idldt}, end_time:{idldt} }
    windfiletimes =  wfnames2dt(windfiles)

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

  ENDIF ELSE BEGIN 
    lf =  string(10b)
    ipath = path
    ifilt = filter
    IF n_elements(path) GT 1 THEN ipath =  strjoin(path,",") 
    IF n_elements(filter) GT 1 THEN ifilter =  strjoin(filter,",")
    Message,"Can't find any wind files in " + $
            ipath + lf + 'using filter ' + ifilt,/cont
  ENDELSE 
return, retarray
END



