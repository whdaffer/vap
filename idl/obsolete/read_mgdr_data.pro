PRO read_mgdr_data, files, fore,midv,midh,aft,$
                    foreloc,midvloc,midhloc,aftloc
;
;
;



COMMON mgdr_cmn, mgdr_defined, mgdr_size, mgdr
mgdr_size =  12972; record length in bytes.

mintime =  '9999-999T99:99:99.999'
maxtime =  '0000-000T00:00:00.000'

nf =  n_elements( files )
first =  1;
FOR f=0,nf-1 DO BEGIN 
  openr,lun,files(f),/get, error=err
  r = 0
  IF err EQ 0 THEN BEGIN

    fs =  fstat(lun)
    nrecs =  fs.size/MGDR_SIZE
    r =  MGDR_STR( nrecs )
    readu,lun,r
    free_lun,lun

    cenlon =  r.cenlon*0.01
    x =  where( cenlon LT 0, nx )
    IF nx GT 0 THEN cenlon(x) =  cenlon(x)+(2l^16)*0.01
    cenlat =  r.cenlat*0.01
    sig0 =  r.sig0*0.01
    nfore =  r.nbeamfore
    nmidv =  r.nbeammidv
    nmidh =  r.nbeammidh
    naft  =  r.nbeamaft

    IF first THEN BEGIN 
      iieach =  lonarr(4)

      first = 0
      fore    =  fltarr(   total(r.nbeamfore) )
      midv    =  fltarr(   total(r.nbeammidv) )
      midh    =  fltarr(   total(r.nbeammidh) )
      aft     =  fltarr(   total(r.nbeamaft ) )
      foreloc =  fltarr(2, total(r.nbeamfore) )
      midvloc =  fltarr(2, total(r.nbeammidv) )
      midhloc =  fltarr(2, total(r.nbeammidh) )
      aftloc  =  fltarr(2, total(r.nbeamaft ) )
      
      FOR i=0,nrecs-1 DO BEGIN
        ii =  iieach(0)
        FOR j=0,23 DO BEGIN 
          n = nfore(j,i)
          IF n GT 0 THEN BEGIN 
            bp                   = r(i).beamptr( 0:n-1,0,j )
            fore(ii:ii+n-1)      = sig0(bp,j,i)
            foreloc(0,ii:ii+n-1) = cenlon(bp,j,i)
            foreloc(1,ii:ii+n-1) = cenlat(bp,j,i)
            ii = ii+n
          ENDIF 
        ENDFOR 
        iieach(0) =  ii

        ii =  iieach(1)
        FOR j=0,23 DO BEGIN 
          n = nmidv(j,i)
          IF n GT 0 THEN BEGIN 
            bp                   = r(i).beamptr( 0:n-1,1,j )
            midv(ii:ii+n-1)      = sig0(bp,j,i)
            midvloc(0,ii:ii+n-1) = cenlon(bp,j,i)
            midvloc(1,ii:ii+n-1) = cenlat(bp,j,i)
            ii = ii+n
          ENDIF 
        ENDFOR 
        iieach(1) =  ii


        ii =  iieach(2)
        FOR j=0,23 DO BEGIN 
          n = nmidh(j,i)
          IF n GT 0 THEN BEGIN 
            bp                   = r(i).beamptr( 0:n-1,2,j )
            midh(ii:ii+n-1)      = sig0(bp,j,i)
            midhloc(0,ii:ii+n-1) = cenlon(bp,j,i)
            midhloc(1,ii:ii+n-1) = cenlat(bp,j,i)
            ii = ii+n
          ENDIF 
        ENDFOR 
        iieach(2) =  ii


        ii =  iieach(3)
        FOR j=0,23 DO BEGIN 
          n = naft(j,i)
          IF n GT 0 THEN BEGIN 
            bp                  = r(i).beamptr( 0:n-1,3,j )
            aft(ii:ii+n-1)      = sig0(bp,j,i)
            aftloc(0,ii:ii+n-1) = cenlon(bp,j,i)
            aftloc(1,ii:ii+n-1) = cenlat(bp,j,i)
            ii = ii+n
          ENDIF 
        ENDFOR 
        iieach(3) =  ii

      ENDFOR 

      fore =  fore(0:iieach(0)-1) &  foreloc= foreloc(*,0:iieach(0)-1)
      midv =  midv(0:iieach(1)-1) &  midvloc= midvloc(*,0:iieach(1)-1)
      midh =  midh(0:iieach(2)-1) &  midhloc= midhloc(*,0:iieach(2)-1)
      aft  =  aft (0:iieach(3)-1) &  aftloc = aftloc (*,0:iieach(3)-1)
    ENDIF ELSE BEGIN
      tfore = fore & nforeold =  n_elements( fore ) &  fore=0
      tmidv = midv & nmidvold =  n_elements( midv ) &  midv=0
      tmidh = midh & nmidhold =  n_elements( midh ) &  midh=0
      taft =  aft  & naftold   =  n_elements( aft ) &  aft=0

      tforeloc = foreloc &  foreloc=0
      tmidvloc = midvloc &  midvloc=0
      tmidhloc = midhloc &  midh=0
      taftloc  = aftloc  &  aftloc=0
      
      fore    =  fltarr(   total(r.nbeamfore)+nforeold )
      midv    =  fltarr(   total(r.nbeammidv)+nmidvold )
      midh    =  fltarr(   total(r.nbeammidh)+nmidhold )
      aft     =  fltarr(   total(r.nbeamaft )+naftold )

      foreloc =  fltarr(2, n_elements(fore) )
      midvloc =  fltarr(2, n_elements(midv) )
      midhloc =  fltarr(2, n_elements(midh) )
      aftloc  =  fltarr(2, n_elements(aft ) )

      fore(0:iieach(0)-1)      =  tfore &  tfore=0
      foreloc(*,0:iieach(0)-1) =  tforeloc &  tforeloc=0

      midv(0:iieach(1)-1)      =  tmidv &  tmidv=0
      midvloc(*,0:iieach(1)-1) =  tmidvloc &  tmidvloc=0

      midh(0:iieach(2)-1)      =  tmidh &  tmidh=0
      midhloc(*,0:iieach(2)-1) =  tmidhloc &  tmidhloc=0

      aft(0:iieach(3)-1)       =  taft &  taft=0
      aftloc(*,0:iieach(3)-1)  =  taftloc &  taftloc=0


      FOR i=0,nrecs-1 DO BEGIN

        ii =  iieach(0)
        FOR j=0,23 DO BEGIN 
          n = nfore(j,i)
          IF n GT 0 THEN BEGIN 
            bp =  r(i).beamptr( 0:n-1,0,j )
            fore(ii:ii+n-1) =  sig0(bp,j,i)
            foreloc(0,ii:ii+n-1) =  cenlon(bp,j,i)
            foreloc(1,ii:ii+n-1) =  cenlat(bp,j,i)
            ii = ii+n
          ENDIF 
        ENDFOR 
        iieach(0) =  ii

        ii =  iieach(1)
        FOR j=0,23 DO BEGIN 
          n = nmidv(j,i)
          IF n GT 0 THEN BEGIN 
            bp =  r(i).beamptr( 0:n-1,1,j )
            midv(ii:ii+n-1) =  sig0(bp,j,i)
            midvloc(0,ii:ii+n-1) =  cenlon(bp,j,i)
            midvloc(1,ii:ii+n-1) =  cenlat(bp,j,i)
            ii = ii+n
          ENDIF 
        ENDFOR 
        iieach(1) =  ii


        ii =  iieach(2)
        FOR j=0,23 DO BEGIN 
          n = nmidh(j,i)
          IF n GT 0 THEN BEGIN 
            bp =  r(i).beamptr( 0:n-1,2,j )
            midh(ii:ii+n-1) =  sig0(bp,j,i)
            midhloc(0,ii:ii+n-1) =  cenlon(bp,j,i)
            midhloc(1,ii:ii+n-1) =  cenlat(bp,j,i)
            ii = ii+n
          ENDIF 
        ENDFOR 
        iieach(2) =  ii


        ii =  iieach(3)
        FOR j=0,23 DO BEGIN 
          n = naft(j,i)
          IF n GT 0 THEN BEGIN 
            bp =  r(i).beamptr( 0:n-1,3,j )
            aft(ii:ii+n-1) =  sig0(bp,j,i)
            aftloc(0,ii:ii+n-1) =  cenlon(bp,j,i)
            aftloc(1,ii:ii+n-1) =  cenlat(bp,j,i)
            ii = ii+n
           ENDIF 
        ENDFOR 
        iieach(3) =  ii
      
      ENDFOR 

      fore =  fore(0:iieach(0)-1) &  foreloc= foreloc(*,0:iieach(0)-1)
      midv =  midv(0:iieach(1)-1) &  midvloc= midvloc(*,0:iieach(1)-1)
      midh =  midh(0:iieach(2)-1) &  midhloc= midhloc(*,0:iieach(2)-1)
      aft  =  aft (0:iieach(3)-1) &  aftloc = aftloc (*,0:iieach(3)-1)

    ENDELSE 
  ENDIF ELSE BEGIN
    message,!err_string,/cont
  ENDELSE 
ENDFOR ; end loop over files
RETURN
END
