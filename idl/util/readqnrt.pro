;+
; NAME:  
; $Id$
; PURPOSE:  Simple reader for QuikSCAT NRT data file.
; AUTHOR:   William Daffer
;           William.Daffer@jpl.nasa.gov
;
; CATEGORY:  QuikSCAT I/O
; CALLING SEQUENCE:  
;
;          q2b=readqnrt(file [,header=header,$
;                          raw=raw,little_endian=little_endian)
;
;
;
;
; 
; INPUTS:  
;
;     filename - Fully qualified file name
;
;
; OPTIONAL INPUTS:   None
;
; KEYWORD PARAMETERS:  
;
;         header: returns the header. Must be a named variable!
;         raw:    returns the data as q2b_rnoaa_str instead of q2b_str.
;         little_endian: Set this flag if you are running on a
;                        little_endian machine using IDL 4.x. This
;                        keyword only has effect if the IDL version <
;                        5.0. It's ignored if the version is >= 5.0
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
; RESTRICTIONS: Requires IDL 4.0 or higher, although I don't test for
;               it. I'm not sure that the the routine will fail
;               outright when run with idl 3.x, I just don't remember
;               the differences between 3.x and 4.x well enough to
;               predict this its behavior. I've tried to write it so
;               that it will be transparent but I can't really do that
;               in the case of IDL 3.x. If that's the version you're
;               using, give it a try. If you have problems, drop me a
;               line and we'll try to work something out a little less
;               drastic than you upgrading to IDL 5.5.
;
; PROCEDURE: Read the file using the rnoaa structure. If `raw' is set,
;            return, else unpack and convert the important parts,
;            (i.e. change wind speed/direction to u/v, etc...),
;            fill a q2b structure with these quantities and return.
;
;
; EXAMPLE:  
;
;  to get the conversion spoken of above, do:
; 
;  q2b=readqnrt('/foo/bar/nrt_file')
;
;
;  To get the raw QuikSCAT NRT data without any conversion, do:
;
;  nrt=readqnrt('/foo/bar/nrt_file',/raw)
;
;  In *either* case, to get the header as well, do:
;
;  foo=readqnrt(file,header=header)
;
;  If running on a little_endian machine with IDL version < 5.0, then
;  do
;
;  foo=readqnrt(file,/little_endian)
;
;
; ROUTINES CALLED:
;
;  q2b_rnoaa_str.pro; defines the structure used to read the QuikSCAT NRT data.
;  q2b_str.pro      ; defines the structure used to return the data
;                     (if /raw is not set)
;  nscat_getuv.pro  ; converts wind speed/direction to u/v.
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.2  2001/08/28 20:33:25  vapuser
; Added some code to accomodate IDL 4.x users
;
; Revision 1.1  2001/07/23 16:28:15  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 2001, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1407 is acknowledged.
;-

FUNCTION readqnrt, filename, $
                   header=header ,$
                   raw=raw, $
                   little_endian= little_endian


COMMON q2b_rnoaa_cmn, q2b_rnoaa_nheader_recs, $
                      q2b_rnoaa_size, $
                      q2b_rnoaa_defined, $
                      q2b_rnoaa


  idl4 =  !version.release LT '5.0'

  IF n_params() LT 1 THEN BEGIN 
    message,' Usage: retstruct=Readqnrt(filename [,header=header, raw=0|1, little_endian=0|1]) ',/continue
    return,-1
  ENDIF 


  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,-1
  ENDIF 

  little_endian = keyword_set(little_endian)

  ncells = 76
  IF N_elements(q2bh_rnoaa_size) EQ 0 THEN q = q2b_rnoaa_str(1, ncells=ncells)

  q2b = -1

  IF idl4 THEN $
    openr, lun, filename, /get, error=err ELSE $
    openr, lun, filename, /get, error=err,/SWAP_IF_LITTLE_ENDIAN

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

    IF idl4 AND little_endian THEN BEGIN 
      rnoaa =  swap_endian(rnoaa)
    ENDIF 

    IF keyword_set(raw) THEN return, rnoaa

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

    
    q2b = q2b_str( nrecs, ncells=76 )

    Nscat_Getuv, dir, speed,u,v


    Nscat_Getuv, mdir, mspeed, mu, mv

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
    
    IF idl4 THEN BEGIN 
      sz = size(u)
      dims = sz[1:sz[0]]
    ENDIF ELSE dims = size(u,/dim)

    nx = dims[1] &  ny=dims[2]

    sel[bad] = 0
    sel = sel+lindgen(nx,ny)*4


    mu[*] = u[sel]
    mv[*] = v[sel]
    mu[bad] = !values.f_nan
    mv[bad] = !values.f_nan

    mle = rnoaa.mle_like
    smle = q2b.smle
    smle[*] =  temporary(mle[sel])
    smle[bad] = -1.

    q2b.smle =temporary(smle)

    q2b.u = temporary(u)
    q2b.v = temporary(v)
    q2b.su = temporary(mu)
    q2b.sv = temporary(mv)

    q2b.rain_flag = ((ishft(rnoaa.wvcqual_flag,-12) AND 3) EQ 2)

    q2b.rowtime =  string( rnoaa.row_time )

  ENDIF ELSE Message,!error_State.msg,/cont

  return, q2b

END

