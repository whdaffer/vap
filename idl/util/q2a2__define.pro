;+
; NAME:   q2a2__define
; PURPOSE:   Defines and object of type q2a2
;
; AUTHOR; William Daffer
;
; CATEGORY:   OO
;
; CALLING SEQUENCE:   q2a2 = obj_new('q2a2',...)
; 
; METHODS:
;   Init:
;   Set:
;   Get:
;   Cleanup:
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.1.1.1  2001/11/30 23:57:08  vapuser
; Initial Checkin
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  

;============================================

FUNCTION q2a2::Init,file, lon = lon, lat=lat
  status = 0
  self.file = file
  self.eqx =  !values.f_nan
  self.lon = n_elements(lon) EQ 0 ? [0.,360.]:lon
  self.lat = n_elements(lat) EQ 0 ? [-90,90.]:lat
  IF n_elements(file) NE 0 THEN BEGIN 
    status = self->read()
  ENDIF ELSE status = 1
  return,status
END


;============================================
; Cleanup
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO q2a2::Cleanup
  ptr_free, self.data
END


;============================================
; Set Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO q2a2::Set, file = file, data=data, rev=rev
  self.file = file
  self.data = ptr_new(data,/no_copy)
  self.rev = rev
  self-> geteqx
END


;============================================
; Get Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO q2a2::Get, eqx=eqx, _ref_extra=_extra

   IF arg_present(eqx) THEN eqx = self.eqx
   self-> q2a::get,_extra = ['rev','lon','lat','sig0',$
                       'azimuth','flag','data','file']

END

;=====================================================
;
;=====================================================
PRO q2a2::grid, glon, glat, lon, lat, sig0, tlon,tlat,sigma0,index,beam_pos
  IF ptr_valid( self.data) THEN BEGIN
    row =  (*self.data).row
    index =  ishft((*self.data).flag,-8)
    beam_pos = ishft((*self.data).flag,-3)
    nrows =  max(row,min=mn) - mn+1
    offset = mn
    lon = (lat= (sig0 = fltarr(76,nrows,2,2,3)))
    grid = self-> generategrid()
    glon = grid[*,row-1,0]
    glat = grid[*,row-1,1] &  grid=0


    tlon = (*self.data).lon
    tlat = (*self.data).lat
    sigma0 =  (*self.data).sig0

    g = where( index, ng )
    unpack_where,index,g,cc,rr
    IF ng NE 0 THEN BEGIN 
      FOR i=0l,ng-1 DO BEGIN 
        
        r = row[rr[i]]-offset
        b = beam_pos[g[i]] AND 1
        p = ishft(beam_pos[g[i]],-1) AND 1
        c = index[g[i]]-1
        sig0[c,r,b,p,0] = sig0[c,r,b,p,0] + sigma0[g[i]]
        sig0[c,r,b,p,1] = sig0[c,r,b,p,1] + sigma0[g[i]]^2
        sig0[c,r,b,p,2] = sig0[c,r,b,p,2] + 1

        lon[c,r,b,p,0] = lon[c,r,b,p,0] + tlon[g[i]]
        lon[c,r,b,p,1] = lon[c,r,b,p,1] + tlon[g[i]]^2
        lon[c,r,b,p,2] = lon[c,r,b,p,2] + 1

        lat[c,r,b,p,0] = lat[c,r,b,p,0] + tlat[g[i]]
        lat[c,r,b,p,1] = lat[c,r,b,p,1] + tlat[g[i]]^2
        lat[c,r,b,p,2] = lat[c,r,b,p,2] + 1

      ENDFOR 

      sig0[*,*,*,*,0] =  sig0[*,*,*,*,0]/(sig0[*,*,*,*,2]> 1)
      sig0[*,*,*,*,1] =  sqrt( $
            ((sig0[*,*,*,*,1]-sig0[*,*,*,*,2]*sig0[*,*,*,*,0]^2)/$
               ((sig0[*,*,*,*,2]-1)> 1))> 0.0)

      lon[*,*,*,*,0] =  lon[*,*,*,*,0]/(lon[*,*,*,*,2]> 1)
      lon[*,*,*,*,1] =  sqrt( $
            ((lon[*,*,*,*,1]-lon[*,*,*,*,2]*lon[*,*,*,*,0]^2)/$
               ((lon[*,*,*,*,2]-1)> 1))> 0.0)

      lat[*,*,*,*,0] =  lat[*,*,*,*,0]/(lat[*,*,*,*,2]> 1)
      lat[*,*,*,*,1] =  sqrt( $
            ((lat[*,*,*,*,1]-lat[*,*,*,*,2]*lat[*,*,*,*,0]^2)/$
               ((lat[*,*,*,*,2]-1)> 1))> 0.0)
    ENDIF 
  ENDIF 
END 

;=====================================================
;
;=====================================================
FUNCTION q2a2::generategrid
;  swath = fltarr(76,1702,2)
;  swath[*,0:38,*]       =  (qswath(self.eqx+25.25))[*,1624-39:*,*]
;  swath[*,39:1624+38,*] = qswath(self.eqx)
;  swath[*,1702-39:*,*] = (qswath(self.eqx-25.25))[*,0:38,*]
  swath =  qswath(self.eqx)
  return,swath
END

;=====================================================
;
;=====================================================
; PRO q2a2::getselect, $
;        lon=lon, lat=lat, $
;        sig0=sig0, azimuth=azimuth, $
;        row=row, index=index, beam=beam, pos=pos, $
;        land=land

;    lon = (lat= (sig0= (flag= (azimuth = !values.f_nan))))
;    IF ptr_valid( self.data) THEN BEGIN 
;      goodmask =  2l^2 OR 2l^6 ; always check for measured and usable
;      flag =  (*self.data).flag
;      good = where( (flag AND goodmask ) EQ goodmask, ngood)
;      IF ngood NE 0 THEN BEGIN 
;        IF n_elements(beam) NE 0 THEN BEGIN
;          beam = strupcase(beam)
;          IF beam EQ 'INNER' THEN $
;            good1 =  where((ishft(flag[good],-3) AND 1) EQ 0,ngood1) $
;          ELSE $
;            good1 =  where((ishft(flag[good],-3) AND 1),ngood1)
;          IF ngood1 NE 0 THEN BEGIN 
;            good = good[good1] 
;            ngood = ngood1
;          ENDIF ELSE BEGIN 
;            ngood = 0
;            good = -1
;          ENDELSE 
;        ENDIF 
;        IF ngood NE 0 THEN BEGIN 
;          IF n_elements(pos) NE 0 THEN BEGIN 
;            pos = strupcase(pos)
;            IF pos EQ 'FORE' THEN $
;              good1 =  where((ishft(flag[good],-4) AND 1) EQ 0,ngood1) $
;            ELSE $
;              good1 =  where((ishft(flag[good],-4) AND 1),ngood1)
;          ENDIF 
;          IF ngood1 NE 0 THEN BEGIN 
;            good = good[good1] 
;            ngood = ngood1
;          ENDIF ELSE BEGIN 
;            ngood = 0
;            good = -1
;          ENDELSE 
;        ENDIF 
;        IF ngood NE 0 THEN BEGIN 
;          IF keyword_set(land) eq 0 THEN BEGIN 
;            good1 = where( (flag[good] AND 3 ) EQ 0, ngood1)
;            IF ngood1 NE 0 THEN BEGIN 
;              good = good[good1]
;              ngood = ngood1
;            ENDIF ELSE BEGIN 
;              good = -1
;              ngood = 0
;            ENDELSE 
;          ENDIF 
;        ENDIF 
;      ENDIF 
;      IF ngood NE 0 THEN BEGIN 
;        IF arg_present(lon) THEN lon = ((*self.data).lon)[good]
;        IF arg_present(lat) THEN lat = ((*self.data).lat)[good]
;        IF arg_present(sig0) THEN sig0 = ((*self.data).sig0)[good]
;        IF arg_present(azimuth) THEN azimuth = ((*self.data).azimuth)[good]
;        IF arg_present(index) THEN index = ishft(((*self.data).flag)[good],-8)
;        IF arg_present(row) THEN BEGIN 
;          unpack_where,flag,good,cc,rr
;          rr = rr[uniq(rr,sort(rr))]
;          row = ((*self.data).row)[rr]
;        ENDIF 
;      ENDIF 
;   ENDIF 
; END

;============================================
; Read
;============================================

FUNCTION q2a2::read,file
  status = 0
  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    message,!error_state.msg,/cont,/noname
    return,0
  ENDIF 

  IF n_elements(file) NE 0 THEN BEGIN 
    message,'Reseting data to ' + file,/info
    ptr_free, self.data
    self.file = file
  ENDIF 

  tfile = assureuncompressed(self.file)
  IF tfile EQ '' THEN Message,"Can't uncompress " + self.file
  basename =  basename(tfile)
  self.rev =  long(strmid(basename,6,5))
  attr =  hdfgetattr(tfile,attr= "EquatorCrossingLongitude")
  IF ptr_valid(attr.value) THEN self.eqx = float((*attr.value)[2])
  ptr_free,attr.value

  status =  self-> q2a::read(tfile)
  tfile = assurecompressed(self.file)
  
  return, status
END

;=====================================================
;
;=====================================================


PRO q2a2::geteqx
  tfile = self.file
  catch, error
  IF error NE 0 THEN BEGIN 
    message,!error_state.msg,/cont,/noname
    return
  ENDIF 
  IF finite(self.eqx) EQ 0 THEN BEGIN 
    tfile = assureuncompressed(self.file)
    attr =  hdfgetattr(tfile,attr= "EquatorCrossingLongitude")
    IF ptr_valid(attr.value) THEN self.eqx = float((*attr.value)[2])
    ptr_free, attr.value
    tfile = assurecompressed(self.file)
  ENDIF 
END

;============================================
; Definition Routine
;============================================

PRO q2a2__define
  junk = {q2a2, $
          eqx: 0.0, INHERITS q2a}
END
