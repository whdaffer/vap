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
; Revision 1.5  2000/02/23 21:26:12  vapuser
; General cleanup. Added rain_flag code
;
; Revision 1.4  1999/10/05 17:24:24  vapuser
; Fixed some unsigned INT bugs. Increased use of 'verbose' flag. Return
; nambig and wvc_sel. return rowtime. General optimization.
;
; Revision 1.3  1998/11/12 20:59:50  vapuser
; Added comments/documentation and header keyword
;
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


  IF n_params() LT 1 THEN BEGIN 
    message,' Usage: retstruct=Q2BRnoaaRead(filename, starttime=starttime, endtime=endtime, ncells=ncells, verbose=verbose) ',/continue
    return,-1
  ENDIF 


  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,-1
  ENDIF 

  Verbose = keyword_set(verbose)
  IF N_elements(ncells) EQ 0 THEN ncells = 76
  IF NOT exist(q2bh_rnoaa_size) THEN q = q2b_rnoaa_str(1, ncells=ncells)

  q2b = -1
  t1 = systime(1)
  t0 = t1
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
    IF verbose THEN print,'Time to read Noaa file', t2-t1
    t1 = t2

    dir = rnoaa.winddir*0.01
    x = where(dir LT 0, nx )
    IF nx NE 0 THEN dir[x] =  dir[x] + (2l^16-1)*0.01

    speed = rnoaa.windspd*0.01


    mdir = rnoaa.Model_dir*0.01
    x = where(mdir LT 0, nx )
    IF nx NE 0 THEN mdir[x] =  mdir[x] + (2l^16-1)*0.01

    mspeed = rnoaa.model_speed*0.01

    lon = rnoaa.Wvc_lon*0.01
    x = where(lon LT 0, nx )
    IF nx NE 0 THEN lon[x] =  lon[x] + (2l^16-1)*0.01


;    errdir = rnoaa.errdir*0.01
;    x = where(errdir LT 0, nx )
;    IF nx NE 0 THEN errdir[x] =  errdir[x] + (2l^16-1)*0.01

    
    q2b = q2b_str( nrecs, ncells=76 )
    t2 = systime(1)
    IF verbose THEN print,'time to just before 1st nscat_getuv',t2-t1
    t1 = t2
    nscat_getuv, dir, speed,u,v
    t2 = systime(1)
    IF verbose THEN print,'time to do 1st nscat_getuv',t2-t1
    t1 = t2
    nscat_getuv, mdir, mspeed, mu, mv
    t2 = systime(1)
    IF verbose THEN print,'time to do 2nd nscat_getuv',t2-t1
    t1 = t2
    q2b.lon = temporary(lon)
    q2b.lat = rnoaa.wvc_lat*0.01
    q2b.row =  rnoaa.wvc_row
    q2b.nambig = rnoaa.nambig
    q2b.sel = rnoaa.wvc_sel
    q2b.qual =  rnoaa.wvcqual_flag
    q2b.mu = mu
    q2b.mv = mv
    
      ; reuse mu/mv, this time as the selected vectors 'su/sv'
    mu[*] = !values.F_Nan
    mv[*] = !values.F_Nan

    sel = rnoaa.wvc_sel-1
    x = where( rnoaa.nambig EQ 1 AND sel LT 0, nx )
    IF nx NE 0 THEN sel[x] =  0
    good = where( rnoaa.nambig GE 1 AND sel GT -1, ngood)
    bad = where(  rnoaa.nambig LT 1 OR  sel le -1, nbad)
    ;unpack_where, sel, good, col, row
    ;ucol = col[ uniq(col,sort(col)) ]
    ;nn = n_elements(ucol)
    t2 = systime(1)
    IF verbose THEN print,'time to just before loop',t2-t1
    t1 = t2

    ;FOR ii=0,nn-1 DO BEGIN 
    ;  x = where( col EQ ucol[ii], nx )
    ;  mu[col[x],row[x]] =  u[ sel[col[x],row[x]], col[x], row[x] ]
    ;  mv[col[x],row[x]] =  v[ sel[col[x],row[x]], col[x], row[x] ]
    ;ENDFOR 

    
    dims = size(u,/dim)
    nx = dims[1] &  ny=dims[2]


;    FOR i=0,75 DO BEGIN 
;      FOR j=0l,ny-1 DO BEGIN 
;        IF sel[i,j] GE 0 THEN BEGIN 
;          mu[i,j] =  u[sel[i,j],i,j]
;          mv[i,j] =  v[sel[i,j],i,j]
;        ENDIF 
;      ENDFOR 
;    ENDFOR 

    sel[bad] = 0
    sel = sel+lindgen(nx,ny)*4
    t2 = systime(1)
    IF verbose THEN print,'time to do loop',t2-t1
    t1 = t2


    mu[*] = u[sel]
    mv[*] = v[sel]
    mu[bad] = !values.f_nan
    mv[bad] = !values.f_nan

    q2b.u = temporary(u)
    q2b.v = temporary(v)
    q2b.su = temporary(mu)
    q2b.sv = temporary(mv)

    q2b.mp_rain_flag = ishft(rnoaa.wvcqual_flag,-12) AND 3
    q2b.nof_rain_flag = ishft(rnoaa.wvcqual_flag,-14) AND 3

    q2b.rowtime =  string( rnoaa.row_time )
    t2 = systime(1)
    IF verbose THEN print,'time to do load arrays',t2-t1
    t1 = t2

  ENDIF ELSE Message,!error_State.msg,/cont

  print,' total time ',  t1-t0
  return, q2b

END

