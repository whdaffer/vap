PRO COMPARE_IWIND, interp_file, wind_files, ou, ov, su,sv, $
                   ilon, ilat, iu,iv,uu,vv,llon,llat, saveit=saveit

  saveit =  keyword_set(saveit)
  IF n_params() LT 2 THEN BEGIN
   message,' USAGE: compare_iwind, iterp_file, wind_files ',/cont
   return
  ENDIF 

;  status =  READ_IWIND_FILE( interp_file, iu, iv)
  status =  READ_IWIND_FILE( interp_file, iu, iv, hdr )
  IF hdr EQ -1 THEN BEGIN 
    ilon =  findgen(360)#replicate(1,121)
    ilat =  replicate(1,360)#(findgen(121)-60)
  ENDIF ELSE message,' Not ready for hdr != -1 !'

  IF status THEN BEGIN 
    READ_RMGDR_DATA, wind_files, u,v,lon,lat, row=row, $
     mint=mint,maxt= maxt, times=times
    IF n_elements(u) NE 0 THEN BEGIN 
      ou = iu*0. &  ov=ou &  su= ou+1 &  sv=ou+1

      xx =  where( lat GE -60 AND lat LE 60 )
      lon =  lon(xx) &  lat= lat(xx) &  u= u(xx) &  v= v(xx)

      s =  sort(lon)
      uu =  u &  vv=v &  llon=lon &  llat=lat
      lon =  fix(lon(s)) &  lat= fix(lat(s)) &  u= u(s) &  v= v(s)
      ii =  0
      FOR i=0,359 DO BEGIN 
        nn =  n_elements(lon)
        xx =  where( lon Ge i AND lon LT i+1 , nxx )
        IF nxx NE 0 THEN BEGIN 
          lat1 =  lat(xx)
          ulat =  lat1(uniq( lat1, sort(lat1) ))
          nulat =  n_elements( ulat )
          FOR j=0,nulat-1 DO BEGIN 
            yy =  where( lat(xx) EQ ulat(j), nyy )
            k =  ulat(j) + 60
            IF nyy GT 1 THEN BEGIN 
              su(i,k) =  stdev( iu(i,k)-u(xx(yy)), a ) &  ou(i,k)= a
              sv(i,k) =  stdev( iv(i,k)-v(xx(yy)), a ) &  ov(i,k)= a
            ENDIF ELSE BEGIN
              ou(i,k)= avg(iu(i,k)-u(xx(yy)))
              ov(i,k)= avg(iv(i,k)-v(xx(yy)))
              su(i,k) = 1.e6
              sv(i,k) = 1.e6
            ENDELSE 
          ENDFOR 
          lon =  lon(max(xx)+1:nn-1)
          lat =  lat(max(xx)+1:nn-1)
          u =  u(max(xx)+1:nn-1)
          v =  v(max(xx)+1:nn-1)
        ENDIF 
      ENDFOR 
      IF saveit THEN $
        save,interp_file,wind_files,uu,vv,llon,llat,iu,iv,ilon,ilat,ou,ov,su,sv,file='compare_iwind.save'
    ENDIF ELSE BEGIN
      message,' No u data returned from read_rmgdr_data!',/cont
    ENDELSE 
  ENDIF ELSE BEGIN
    message, 'Bad status from read_iwind_file !',/cont
  ENDELSE 
END



