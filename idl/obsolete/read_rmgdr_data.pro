PRO  read_rmgdr_data, files , u,v,lon,lat, $
                      ambig = ambig, $
                      row= row ,$
                      mintime = mintime, $
                      maxtime = maxtime,$
                      times= times
;+
; NAME:  
; Time-stamp: <97/03/18 15:52:39 vapuser>
; MODIFICATION HISTORY:
;
; PURPOSE: Reads an rmgdr file 
;
;
; AUTHOR;    William Daffer
;            818-354-0161, daffer@rainy.jpl.nasa.gov;
;
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  read_rmgdr_data, files, u,v,lon,lat,ambig=ambig,
;                       mintime=mint, maxtime=maxt
;
;
; 
; INPUTS:  files - (I) vector of rmgdr file, must be fully qualified
;                  file names. The data is read in the order of the
;                  files in this input vector,i.e. if the vector is
;                  files=[file1,file2] then file1's data is read first.
;          
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  ambig - (I) keyword useful if you want to read a specific
;                              ambiguity. Otherwise the program will
;                              read the ambiguity which the wind
;                              processor considers the best, i.e. the
;                              'selected' wind vector 1 <= ambig <= 4,
;                              take specified ambiguity ambig = 0 =>
;                              take ambiguity specified in the
;                              data.wvc_sel variable,i.e. the
;                              'selected' ambiguity.
;                              Not(keyword_set(ambig)) => take wvc_sel ambiguity.
;
;                      mintime - (O) returns the minimum time tag that
;                                    appears in the processed data,
;                                    i.e the min in all the files.
;
;                      maxtime - (O) returns the maximum time tag that
;                                    appears in the processed data,
;                                     i.e. the max in all the files
;
;
;
; OUTPUTS: u - (O) output vector of the U components of the wind field
;          v - (O) output vector of the V component of the wind field
;          lon - (O) output vector of the Longitudes 
;          lat - (O) output vector of the Latitudes.

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
; RESTRICTIONS:  Currently this program is being used on a UNIX
;                machine. No attempt has been made to make it portable
;                to VAXes running VMS or DOS/Windows/OS2
;                machines. Modifications, mostly to the file handling
;                code, will be necessary to run on
;                machines running those OSs.
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  Let's say you want to read all the files in the directory
;          /a/b. (Let's say that the only files in this directory are
;          rmgdr files. If this isn't true, you have to get more
;          specific with the findfile call. You could do this.
;
;          IDL> files=findfile('/a/b/*')
;
;          IDL>
;          read_rmgdr_data,file,u,v,lon,lat,mintime=mint,maxtime=maxt
;            this would return the u components of the vector field in
;            'u', the v in 'v' and the locations in the lon/lat
;            arrays. The minimum time found in the data set woud be
;            returned in mint and the max in maxt. The vectors
;            returned would be the vector ambiguity 'selected' by the
;            wind processor as the 'best', i.e. that specified in the
;            data.wvc_sel variable.
;
;          Say you wanted the 2nd ambiguity from the same set of files.
;          
;          IDL> read_rmgdr_data,file,u,v,lon,lat,mintime=mint,maxtime=maxt,ambig=2
;
;          (the lines beginning with IDL> are just there to signify
;          which lines are to be executed in IDL. Don't type in the
;          'IDL>', just the stuff after it. These lines could easily
;          be executed at the command line or in a
;          procedure/function/'main level procedure' call.)
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1997, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

COMMON rmgdr_cmn, rmgdr_size, rmgdr_defined 
RMGDR_SIZE = 1236L ; for M. Spencer's reduced MGDR format

mintime =  '9999-999T99:99:99.999'
maxtime =  '0000-000T00:00:00.000'

IF keyword_set( ambig ) THEN amb =  ambig ELSE $
                             amb = 0
nf =  n_elements( files )
first_read =  1;
FOR f=0,nf-1 DO BEGIN 
  openr,lun,files(f),/get, error=err
  r = 0

  IF err EQ 0 THEN BEGIN

    fs =  fstat(lun)
    nrecs =  fs.size/RMGDR_SIZE
    r =  RMGDR_STR( nrecs )
    readu,lun,r
    free_lun,lun
    mintime =  min( [ mintime, string(r.time)  ] )
    maxtime =  max( [ maxtime, string(r.time) ] )
    times =  string(r.time)
    nnew =  n_elements( r )
    newlat =  r.lat*0.01
    newspeed =  r.windspd*0.01
    newrow =  r.row ## replicate(1,24)
    ; dir and lon are unsinged integers, so take 
    ; care of the case where they are negative.
    newdir =  r.winddir*0.01
    b = where( newdir LT 0.,nb )
    IF nb NE 0 THEN newdir(b) =  655.36 + newdir(b)

    newlon =  r.lon*0.01
    b = where( newlon LT 0.,nb )
    IF nb NE 0 THEN newlon(b) =  655.36 + newlon(b)

    NSCAT_GETUV, newdir, newspeed, newu, newv 
    IF first_read THEN BEGIN
      ; First read is always different.

      IF amb Ge 1 AND amb LE 4 THEN BEGIN 
        ; The user wants an ambiguity other than the one the wind
        ; processor selected.
        u       =  reform( newu( amb-1,*,*), 24l*nnew )
        v       =  reform( newv( amb-1,*,*), 24l*nnew )
        lon     =  reform( newlon, 24l*nnew )
        lat     =  reform( newlat, 24l*nnew )
        row     =  reform( newrow, 24l*nnew )
      ENDIF ELSE BEGIN 
        ; The user either didn't specify or specifically asked for the
        ; 'selected' ambiguity.
        u =  fltarr( nnew*24l ) 
        v =  u
        lon =  u
        lat =  u
        row =  u
        ii =  0
        FOR i=0,nnew-1 DO BEGIN 
          g =  where( r(i).nambig GT 0 AND r(i).wvc_sel GE  1, ng )
          IF ng NE 0 THEN BEGIN 
            sel =  r(i).wvc_sel(g)-1
            ix =  indgen(ng) + ng*indgen(ng)
            u(ii:ii+ng -1)   =  (newu(sel ,g,i ))(ix)
            v(ii:ii+ng -1)   =  (newv(sel,g,i ))(ix)
            lon(ii:ii+ng -1) =  newlon(g,i)
            lat(ii:ii+ng -1) =  newlat(g,i)
            row(ii:ii+ng -1) =  newrow(g,i)
            ii =  ii+ng
          ENDIF 
        ENDFOR 
        u   =  u  (0:ii-1)
        v   =  v  (0:ii-1)
        lon =  lon(0:ii-1)
        lat =  lat(0:ii-1)
        row =  row(0:ii-1)
      ENDELSE 
      first_read   =  0l
    ENDIF ELSE BEGIN 
      times =  [ times, string( r.time) ]

      IF amb GT 1 AND amb LE 4 THEN BEGIN 
        ; The user wants an ambiguity other than the one the wind
        ; processor selected.

        u       =  [u,  reform( newu( amb-1,*,*), 24l*nnew )]
        v       =  [v,  reform( newv( amb-1,*,*), 24l*nnew )]
        lon     =  [lon,reform( newlon, 24l*nnew )]
        lat     =  [lat,reform( newlat, 24l*nnew )]
        row     =  [row,reform( newrow, 24l*nnew )]
      ENDIF ELSE BEGIN 
        ; The user either didn't specify or specifically asked for the
        ; 'selected' ambiguity.
        tu =  fltarr( 24*nnew )
        tv =  tu
        tlon =  tu
        tlat =  tu
        trow =  tu
        ii =  0
        FOR i=0,nnew-1 DO BEGIN 
          g =  where( r(i).nambig GT 0 AND r(i).wvc_sel GE 1, ng )
          IF ng NE 0 THEN BEGIN 
            sel =  r(i).wvc_sel(g)-1
            ix =  indgen(ng) + ng*indgen(ng)
            tu(ii:ii+ng -1)   = ( newu( sel,g,i ))(ix)
            tv(ii:ii+ng -1)   = ( newv( sel,g,i ))(ix)
            tlon(ii:ii+ng -1) =  newlon(g,i)
            tlat(ii:ii+ng -1) =  newlat(g,i)
            trow(ii:ii+ng -1) =  newrow(g,i)
            ii =  ii+ng
          ENDIF 
        ENDFOR 
        tu   =  tu  (0:ii-1)
        tv   =  tv  (0:ii-1)
        tlon =  tlon(0:ii-1)
        tlat =  tlat(0:ii-1)
        trow =  trow(0:ii-1)

        u    =  [u,   tu   ]
        v    =  [v,   tv   ]
        lon  =  [lon, tlon ]
        lat  =  [lat, tlat ]
        row  =  [row, trow ]

      ENDELSE 
    ENDELSE 
  ENDIF ELSE BEGIN
    message,!err_string,/cont
  ENDELSE 
ENDFOR ; end loop over files
RETURN
END









