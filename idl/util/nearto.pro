;+
; NAME:  Nearto
; $Id$
;
; PURPOSE: Find all files within the given time range that have data
;          which comes come within a specified distance of a specific
;          location
;
; AUTHOR:  whd
;
; CATEGORY:  Qscat/SeaWinds VAP
;
; CALLING SEQUENCE:  
;
;     retstruct=nearto(lon,lat[,starttime,endtime|files=files, ofile=ofile)
; 
; INPUTS:  
;   Lon       : scalar float. (*required*)
;   Lat       : scalar float (*required*)
;   starttime : scalar string having format yyyy/mm/dd/hh/mm, but see
;               the `files' keyword!
;   endtime   : scalar string having format yyyy/mm/dd/hh/mm, but see
;               the `files' keyword!
;   tolerance : The 'nearness' criterion, in degrees. (default=6)
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  
;
;     ofile: The fully qualified name of a file to write the output
;            in, so that external programs (like PERL) can pick it up
;            and make use of it.
;
;     files: Input a list of files, instead of having this routine
;            find them by the time range.
;
; OUTPUTS:  
;
;   Failure: 0
;   Success:
;   retstruct: An array of structures having the following format
; 
;          retstruct={filename: '',$
;                     rowtime: '', $
;                     loc: [0.0d, 0.0d], $
;                     distance: 0.0d, $
;                     inswath: 0}
;
;    -- where --
;
;    filename is the name of the file being considered.
;    rowtime is the time of the row which has the closest approach to
;            the input location
;    loc  is the [lon,lat] of the location of closest approach 
;    dist is the distance in degrees from the input location and the
;         point of closest approach.
;    inswath is a flag, 1 if the point is actually in the swath, 
;            0 otherwise.
;
; OPTIONAL OUTPUTS:  
;
;   If `ofile' is given, the data will be written to the indicated
;   file in a keyword : value(s) format so that external programs
;   can read it for the results.
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  none
;
; PROCEDURE:  
;
;   Find all files in the given timerange, open each in turn and go
;   through them bruteforce computing the distance between each swath
;   location and the input location. Return those files which have
;   points less than the input (or defaulted) tolerance.
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 2000, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION nearto, longitude, latitude, starttime, endtime, tolerance, $
                      ofile=ofile, files=files

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    close,/all

    IF n_elements(ofile) NE 0 THEN BEGIN 
      openw, lun, ofile,/get,error=err
      IF err EQ 0 THEN BEGIN 
        printf,lun,'--- ERROR --- No files found!'
        free_lun, lun
      ENDIF ELSE Message, !error_state.msg,/cont
    ENDIF 
    return,0
  ENDIF 

  IF n_params() LT 2 THEN BEGIN 
    Message,$
      "Usage: restructs=nearto(lon,lat[,start,end [,tol,ofile=ofile, files=files])", /info
    return,0
  ENDIF 

  IF n_elements(tolerance) EQ 0 THEN tolerance =  8 ; degrees

  IF n_elements(files) EQ 0 THEN BEGIN 
    IF NOT isa(starttime,/string,/nonempty) THEN $
      Message,"Starttime must be a string of form YYYY/MM/DD/HH/MM",/noname

    IF NOT isa(endtime,/string,/nonempty) THEN $
      Message,"Endtime must be a string of form YYYY/MM/DD/HH/MM",/noname

    files = getwindfiles(endtime, start_time=starttime)
    nf = n_elements(files)
    IF nf EQ 1 AND strlen(files[0]) EQ 0 THEN $
      Message,'No files found',/noname


  ENDIF ELSE BEGIN 
    IF NOT isa(files,/string,/nonempty) THEN $
      Message,"Files=file, must be non Empty string!",/cont
    nf = n_elements(files)
  ENDELSE 

    
  
  retstruct= replicate( {filename: '',$
                         rowtime: '', $
                         loc: [0.0d, 0.0d], $
                         distance: 0.0d, $
                         inswath: 0}, nf )
  fi=0;
  
  
  FOR f=0,nf-1 DO BEGIN 
    q = obj_new('q2b',file=files[f],crd=[0,0],rainflag=0)
    IF NOT obj_valid(q) THEN $
      Message,"Error initializing object for file " + files[f],/noname
    
    s = q-> getplotdata(u,v,lon,lat)
    iid = u*0+1
    gg = where(abs(lon) LE 1.e-7 AND abs(lat) LE 1.e-7 AND $
               (NOT (finite(u) OR finite(v))), ngg)
    iid[gg] = 0

    g = where(iid NE 0, ng)

    IF ng NE 0 THEN BEGIN 
      s = q-> get(rowtime=rowtime)
      obj_destroy,q
      dist = abs(sphdist(lat[g],lon[g],latitude,longitude))
      x = where(dist LE tolerance,nx)

      IF nx NE 0 THEN BEGIN 
        mi = min( dist[x], ii )
        retstruct[fi].filename =  files[f]
        retstruct[fi].distance = mi
        retstruct[fi].loc = [ lon[g[x[ii]]], lat[g[x[ii]]] ]
        unpack_where,u,g[x[ii]],cc,rr
        cc = cc[0]
        rr = rr[0]
        rowtime = rowtime[rr[0]]
        ;012345678901234567890123456789
        ;yyyy-dddThh:mm:ss.ccc
        year = strmid(rowtime,0,4)
        doy = strmid(rowtime,5,3)
        hour = strmid(rowtime,9,2)
        min = strmid(rowtime,12,2)
        date = doy2date(fix(year),fix(doy))
        retstruct[fi].rowtime = whdstrjoin([year,date[0],date[1],hour,min], '/')
        test =  (cc[0] GT 0) AND (cc[0] LT 75)
        IF test THEN BEGIN 
          x = where( (finite( u[0:cc-1,rr] ) AND finite(v[0:cc-1,rr] )) OR $
                     (finite( u[cc+1:*,rr] ) AND finite(v[cc+1:*,rr] )), nx )
          
          retstruct[fi].inswath =  nx NE 0 
        ENDIF 
        fi = fi+1
      ENDIF 

    ENDIF ELSE $
      Message,'No good data in file ' + files[f],/info
  ENDFOR 

  IF fi LT nf THEN BEGIN 
    IF fi NE 0 THEN retstruct =  retstruct[0:fi-1] ELSE retstruct = 0
  ENDIF 

  IF n_elements(ofile) NE 0 THEN BEGIN 
    openw, lun, ofile, /get,error=err
    IF err NE 0 THEN BEGIN 
      Message,!error_state.msg,/info
      Message,ofile + " was NOT WRITTEN!",/info
    ENDIF ELSE BEGIN 
      IF NOT isa(retstruct,/structure) THEN BEGIN 
        printf,lun,'--- ERROR --- No data found!'
      ENDIF ELSE BEGIN 
        
        FOR i= 0, n_elements(retstruct)-1 DO BEGIN 
          printf, lun, 'FILE    : ' + retstruct[i].filename
          printf, lun, 'ROWTIME : ' + retstruct[i].rowtime
          printf, lun, 'LOCATION: ' + string(retstruct[i].loc, form='(2(f7.2,2x))')
          printf, lun, 'DISTANCE: ' + string(retstruct[i].distance,form='(f7.2)')
          printf, lun, 'INSWATH: ', retstruct[i].inswath
          printf, lun, '---------------------------------'
        ENDFOR         
        free_lun, lun
      ENDELSE 

      free_lun, lun
    ENDELSE 
  ENDIF 
  return,retstruct
END
