PRO readpvdat, filename, u, v, lon, lat
  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!error_State.msg,/cont
    catch, error
    return
  ENDIF 

  IF n_Params() NE 5         OR $
   N_Elements(filename) EQ 0 OR $
   NOT Arg_Present(U)        OR $ 
   NOT Arg_Present(V)        OR $ 
   NOT Arg_Present(Lon)      OR $ 
   NOT Arg_Present(Lat)      OR $
   strlen(filename(0)) EQ 0 THEN BEGIN 
    Message,'Usage: readpvdat, file, u,v,lon,lat '
    return
  ENDIF 
  
  OpenR,rlun, filename, /Get_lun, error=err
  IF err EQ 0 THEN BEGIN 
    ncells = 0l
    nrecs = 0l
    ambig = 0l
    first = 1
    WHILE NOT eof( rlun ) DO BEGIN 
      ReadU, rlun, ncells, nrecs, ambig
      IF first THEN BEGIN 
        Keep_Ambig = ambig
        keep_ncells =  ncells
      ENDIF 
      IF ambig EQ  keep_ambig THEN BEGIN 

        IF ncells EQ  keep_ncells THEN BEGIN 
          IF first THEN BEGIN 
            first = 0
            U = fltarr(ncells,nrecs)
            V = u
            lon = u
            lat = u
            readu, rlun, u,v,lon,lat
          ENDIF ELSE BEGIN 
            NewU   = fltarr(ncells,nrecs)
            newV   = newU
            newLon = newU
            newLat = newU
            ReadU, rlun, Newu,Newv,Newlon,Newlat          
            s = size( u )
            nrecs =  nrecs + s[2]
              ; Make new U
            tu =  fltarr(ncells, nrecs )
            tu[*,0:s[2]-1] =  U
            tu[*,s[2]:nrecs-1] =  newu
            u = tu &  newu = 0

              ; Make new V
            tv =  fltarr(ncells, nrecs )
            tv[*,0:s[2]-1] =  V
            tv[*,s[2]:nrecs-1] =  newv
            v = tv &  newv = 0

              ; Make new LON
            tLon =  fltarr(ncells, nrecs )
            tLon[*,0:s[2]-1] =  LON
            tLon[*,s[2]:nrecs-1] =  newLon
            Lon = tLon &  newLon = 0

              ; Make new LAT
            tLat =  fltarr(ncells, nrecs )
            tLat[*,0:s[2]-1] =  Lat
            tLat[*,s[2]:nrecs-1] =  newLat
            Lat = tLat &  newLat = 0

          ENDELSE 
        ENDIF ELSE BEGIN 
          Message,'Different NCELLS!',/cont
          print,'   This Ncells: ',ncells, ' First Ncells ',keep_ncells
          print,'   --- Skipping this record --- '
        ENDELSE 
          
      ENDIF ELSE BEGIN 
        Message,'Different Ambiguity!',/cont
        print,'     This Ambiguity: ',ambig, ' First Ambiguity :',keep_ambig
        print,'   --- Skipping this record --- '
      ENDELSE 
    ENDWHILE 
    free_lun, rlun 
  ENDIF ELSE Message,!error_State.msg,/cont
  
  
END

  








