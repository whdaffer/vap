;+
; NAME:  
; $Id$
; PURPOSE:  
;
;
; AUTHOR:
;
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  
;
;          q2b=q2brnoaaread(file [,ncells=ncells,$
;                          header=header,$
;                          raw=raw,starttime=starttime,$
;                          endtime=endtime,verbose=verbose)
;
;
;
;
; 
; INPUTS:  
;
;     filename - name of file to read
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;         ncells: number of cells in crosstrack direction (if
;                 different from 76)
;         header: returns the header
;         raw:    returns the data as q2b_rnoaa_str instead of q2b_str.
;         StartTime: returns data range start time (unused as yet)
;         EndTime: Ditto, end time (unused as yet)
;         Verbose: will emit lots of messages (unused as yet)
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
; COMMON BLOCKS:  q2b_rnoaa_cmn 
;
;   Containing the items: 
;
;            q2b_rnoaa_nheader_recs - Number of header records
;            q2b_rnoaa_size         - size of record (it's real size)
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
; Revision 1.2  1998/11/10 00:46:04  vapuser
; More work...
;
; Revision 1.1  1998/11/05 19:25:25  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION q2bRnoaaRead, filename, $
                       ncells=ncells, $
                       header=header ,$
                       raw=raw, $
                       StartTime=Starttime, $
                       EndTime=EndTime, $
                       Verbose=Verbose

COMMON q2b_rnoaa_cmn, q2b_rnoaa_nheader_recs, $
                      q2b_rnoaa_size, $
                      q2b_rnoaa_defined, $
                      q2b_rnoaa

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,-1
  ENDIF 

  IF NOT exist(q2bh_rnoaa_size) THEN q = q2b_rnoaa_str(1)
  Verbose = keyword_set(verbose)
  IF N_elements(ncells) EQ 0 THEN ncells = 76

  q2b = -1
  t1 = systime(1)
  t0 = t1
  IF n_params() LT 1 THEN $
    message,' Usage: retstruct=Q2BRnoaaRead(filename, starttime=starttime, endtime=endtime, ncells=ncells, verbose=verbose) '

  tfilename = DeEnvVar(filename,/isfile)

  openr, lun, tfilename, /get, error=err
  IF err EQ 0 THEN BEGIN 

    ff = fstat(lun)
    nrecs = ff.size/q2b_rnoaa_size-q2b_rnoaa_nheader_recs
    rnoaa = q2b_rnoaa_str(nrecs, ncells=ncells)

    IF Arg_Present( header ) THEN BEGIN 
      header = bytarr(q2b_rnoaa_size,q2b_rnoaa_nheader_recs)
      readu, lun, header, rnoaa
    ENDIF ELSE BEGIN 
      point_lun, lun, q2b_rnoaa_size*q2b_rnoaa_nheader_recs
      readu,lun,rnoaa
    ENDELSE 
    free_lun, lun

    IF keyword_set(raw) THEN return, rnoaa

    t2 = systime(1)
    print,'Time to read Noaa file', t2-t1
    t1 = t2

    dir = rnoaa.winddir*0.01
    x = where(dir LT 0, nx )
    IF nx NE 0 THEN dir[x] =  dir[x] + 655.36

    speed = rnoaa.windspd*0.01


    mdir = rnoaa.Model_dir*0.01
    x = where(mdir LT 0, nx )
    IF nx NE 0 THEN mdir[x] =  mdir[x] + 655.36

    mspeed = rnoaa.model_speed*0.01

    lon = rnoaa.Wvc_lon*0.01
    x = where(lon LT 0, nx )
    IF nx NE 0 THEN lon[x] =  lon[x] + 655.36


;    errdir = rnoaa.errdir*0.01
;    x = where(errdir LT 0, nx )
;    IF nx NE 0 THEN errdir[x] =  errdir[x] + 655.36

    
    q2b = q2b_str( nrecs, ncells=76 )
    t2 = systime(1)
    print,'time to just before 1st nscat_getuv',t2-t1
    t1 = t2
    nscat_getuv, dir, speed,u,v
    t2 = systime(1)
    print,'time to do 1st nscat_getuv',t2-t1
    t1 = t2
    nscat_getuv, mdir, mspeed, mu, mv
    t2 = systime(1)
    print,'time to do 2nd nscat_getuv',t2-t1
    t1 = t2
    q2b.lon = temporary(lon)
    q2b.lat = rnoaa.wvc_lat*0.01
    q2b.row =  rnoaa.wvc_row

    q2b.mu = mu
    q2b.mv = mv
    
      ; reuse mu/mv, this time as the selected vectors 'su/sv'
    mu[*] = !values.F_Nan
    mv[*] = !values.F_Nan

    sel = rnoaa.wvc_sel-1
    good = where( rnoaa.nambig GT 1 AND sel GT -1, ngood)
    unpack_where, sel, good, col, row
    ucol = col[ uniq(col,sort(col)) ]
    nn = n_elements(ucol)
    t2 = systime(1)
    print,'time to just before loop',t2-t1
    t1 = t2

    FOR ii=0,nn-1 DO BEGIN 
      x = where( col EQ ucol[ii], nx )
      mu[col[x],row[x]] =  u[ sel[col[x],row[x]], col[x], row[x] ]
      mv[col[x],row[x]] =  v[ sel[col[x],row[x]], col[x], row[x] ]
    ENDFOR 

    t2 = systime(1)
    print,'time to do loop',t2-t1
    t1 = t2

    q2b.u = temporary(u)
    q2b.v = temporary(v)
    q2b.su = temporary(mu)
    q2b.sv = temporary(mv)

    t2 = systime(1)
    print,'time to do load arrays',t2-t1
    t1 = t2

  ENDIF ELSE Message,!error_State.msg,/cont

  print,' total time ',  t1-t0
  return, q2b

END

