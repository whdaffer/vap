;+
; $Id$
; NAME:   qmodela__define
; PURPOSE:   Defines and object of type qmodela
;
; AUTHOR; William Daffer
;
; CATEGORY:   OO
;
; CALLING SEQUENCE:   qmodela = obj_new('qmodela',...)
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

FUNCTION qmodela::Init,file, $
                  data = data, $
                  lonpar=lonpar, $
                  latpar=latpar, time=time
  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    message,!error_state.msg,/cont
    return,0
  ENDIF 
    
  status = 0
  self.lonpar = (self.latpar=[!values.f_nan,!values.f_nan])
  self-> set,lonpar = lonpar,latpar=latpar
  IF n_elements(file) THEN BEGIN 
    IF self-> read(file)  EQ 0 THEN Message,"Error reading file " + file
    self->CalcTime
    status = 1
  ENDIF ELSE BEGIN 
    IF n_elements(data) NE 0 THEN $
      self-> Set,data = data,lonpar=lonpar,latpar=latpar,time=time, status=status
    status = 1
  ENDELSE 
      
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

PRO qmodela::Cleanup
   self-> qmodel::cleanup
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
; Todo: work on regions that cross the prime meridian!
;  
;============================================

PRO qmodela::Set, data = data, lonpar=lonpar, latpar=latpar, time=time, status=status

   on_error,0
   status = 1
   IF n_elements(lonpar) EQ 2 THEN self.lonpar = lonpar
   IF n_elements(latpar) EQ 2 THEN self.latpar = latpar

   IF n_elements(data) NE 0 THEN BEGIN 
     IF NOT isa(data,/structure,name='QMODELDATA') THEN BEGIN 
       IF n_elements(time) EQ 0 THEN BEGIN 
         Message,"Must have TIME if DATA is not QMODELDATA",/info
         return
       ENDIF 
       dim = size(data,/dim)
       IF n_elements(dim) NE 3 THEN BEGIN 
         Message,"Either input the data as a QMODELDATA structure, or a ",/info
         Message,'3 dim array where 2 dims are [nlon,nlat] AND the third is',/info
         Message,"U followed by V",/info
         Message,"I don't understand the format of this data",/info
         status = 0
         return
       ENDIF 
       IF n_elements(lonpar) NE 2 OR n_elements(latpar) NE 2 THEN BEGIN 
         Message,"If you input data as array, you must tell me what",/info
         Message,"the lat/lon limits are via the lonpar=lonpar,",/info
         Message,"latpar=latpar keywords!"
         status = 0
         return
       ENDIF 

       CASE 1 OF 
         dim[0] EQ 2: BEGIN 
           u = reform(data[0,*,*])
           v = reform(data[1,*,*])
         END
         dim[1] EQ 2: BEGIN 
           u = reform(data[*,0,*])
           v = reform(data[*,1,*])
         END 
         dim[2] EQ 2: BEGIN 
           u = data[*,*,0]
           v = data[*,*,1]
         END 
       ENDCASE 
         ; Set the time for this interpolated field.
       self-> setTime,time
       dim = size(u,/dim)
       nlon = dim[0]
       nlat = dim[1]
       loninc = 1.0*(self.lonpar[1]-self.lonpar[0])/(nlon-1)
       latinc = 1.0*(self.latpar[1]-self.latpar[0])/(nlat-1)
       lon = (findgen(nlon)*loninc+self.lonpar[0])#(replicate(1.,nlat))
       lat = replicate(1.,nlon)#(findgen(nlat)*latinc+self.latpar[0])

       IF ptr_valid(self.data) THEN BEGIN 
         ptr_free,(*(self.data)).u,(*(self.data)).v,$
                  (*(self.data)).lon,(*(self.data)).lat, $
                  (*(self.data)).hdr.rainf,(*(self.data)).hdr.ermax
         ptr_free,self.data
       ENDIF 
       qq = qmodel_str()
       qq.lon = ptr_new(lon,/no_copy)
       qq.lat = ptr_new(lat,/no_copy)
       qq.u =  ptr_new(u,/no_copy)
       qq.v =  ptr_new(v,/no_copy)
       qq.hdr.lonpar = [self.lonpar,loninc]
       qq.hdr.latpar = [self.latpar,latinc]
       qq.hdr.nlon = nlon
       qq.hdr.nlat = nlat
       qq.hdr.region = [self.lonpar[0],self.latpar[0],$
                        self.lonpar[1],self.latpar[1]]
       qq.hdr.interpTime = idldt2vaptime(self.time)
       self.data =  ptr_new(qq,/no_copy)

     ENDIF ELSE self.data =  ptr_new(data)
   ENDIF 

   IF ptr_valid(self.data) THEN BEGIN 
     x = where( finite([self.lonpar,self.latpar]),nx)
     IF nx EQ 4 THEN BEGIN 
       x = where( [self.lonpar-(*self.data).hdr.lonpar[0:1], $
                   self.latpar-(*self.data).hdr.latpar[0:1] ],nx )
       IF nx NE 0 THEN BEGIN 
         lon = *((*self.data).lon)
         lat = *((*self.data).lat)
         x = where( lon GE self.lonpar[0] AND lon LE self.lonpar[1] AND $
                    lat GE self.latpar[0] AND lat LE self.latpar[1], nx )
         IF nx eq 0 THEN BEGIN 
           Message,"No data in input lon/lat range!"
           status = 0
           return
         ENDIF 

         Unpack_Where,lon,x,c,r
         c = minmax(c)
         r = minmax(r)
         U = (*((*self.data).u))[c[0]:c[1],r[0]:r[1]]
         V = (*((*self.data).v))[c[0]:c[1],r[0]:r[1]]
         lon = lon[c[0]:c[1],r[0]:r[1]]
         lat = lat[c[0]:c[1],r[0]:r[1]]
         dim = size(u,/dim)
         nlon = dim[0]
         nlat = dim[1]

         loninc = 1.0*(max(lon)-min(lon))/(nlon-1)
         latinc = 1.0*(max(lat)-min(lat))/(nlat-1)
         *((*self.data).U) =  u
         *((*self.data).v) =  v
         *((*self.data).lon) = lon
         *((*self.data).lat) = lat
         (*self.data).hdr.lonpar = [lonpar, loninc ]
         (*self.data).hdr.latpar = [latpar, latinc ]
         (*self.data).hdr.nlon = nlon
         (*self.data).hdr.nlat = nlat

       ENDIF 
     ENDIF 
   ENDIF 

END

;============================================
; Read
;============================================

FUNCTION qmodela::Read, filename 
  IF ptr_valid(self.data) THEN BEGIN 
    ptr_free,(*(self.data)).u,(*(self.data)).v,$
             (*(self.data)).lon,(*(self.data)).lat, $
             (*(self.data)).hdr.rainf,(*(self.data)).hdr.ermax
    ptr_free,self.data
  ENDIF 
  status = self-> qmodel::read(filename)
  self-> calctime
  IF status THEN BEGIN 
    x = where(finite(self.lonpar) AND finite(self.latpar), nx )
    IF nx EQ 2 THEN BEGIN 
      LonPar = (*self.data).hdr.lonpar
      LatPar = (*self.data).hdr.latpar
      NLon = (*self.data).hdr.nlon
      NLat = (*self.data).hdr.nlat
      lon = ( findgen( nlon )*lonpar[2] + lonpar[0] )#replicate(1, nlat )
      lat =  replicate(1,nlon)#( findgen(nlat)*LatPar[2]+LatPar[0] )
      x = where( lon GE self.lonpar[0] AND lon LE self.lonpar[1] AND $
                 lat GE self.latpar[0] AND lat LE self.latpar[1],nx )
      IF nx eq 0 THEN BEGIN 
        Message,"No data in input lon/lat range!",/cont
        return,0
      ENDIF 
      
      unpack_where,lon,x,c,r
      c = minmax(c)
      r = minmax(r)
      U = (*((*self.data).u))[c[0]:c[1],r[0]:r[1]]
      V = (*((*self.data).v))[c[0]:c[1],r[0]:r[1]]
      lon = lon[c[0]:c[1],r[0]:r[1]]
      lat = lat[c[0]:c[1],r[0]:r[1]]
      dim = size(u,/dim)
      nlon = dim[0]
      nlat = dim[1]

      loninc = lonpar[2]
      latinc = latpar[2]
      *((*self.data).U) =  u
      *((*self.data).v) =  v
      *((*self.data).LON) =  lon
      *((*self.data).LAT) =  lat
      (*self.data).hdr.lonpar = [self.lonpar, loninc ]
      (*self.data).hdr.latpar = [self.latpar, latinc ]
      (*self.data).hdr.nlon = nlon
      (*self.data).hdr.nlat = nlat
      status = 1
    ENDIF 
  ENDIF 
  return,status
END

;============================================
; CalcTime
; 
;============================================

PRO qmodela::CalcTime, time
  IF n_elements(time) NE 0 THEN BEGIN 
    self-> SetTime,time
  ENDIF ELSE BEGIN 
    IF ptr_valid(self.data) THEN BEGIN 
      IF strlen( string((*self.data).hdr.interptime )) NE 0 THEN BEGIN 
        self.time = vaptime2idldt(string((*self.data).hdr.interptime))
      ENDIF ELSE $
      IF strlen( string((*self.data).hdr.starttime )) NE 0 AND $
         strlen( string((*self.data).hdr.endtime )) NE 0 THEN BEGIN 
        st = vaptime2idldt( string((*self.data).hdr.starttime ) )
        et = vaptime2idldt( string((*self.data).hdr.endtime ) )
        tt = mean([st.julian,et.julian])
        self.time = jul_to_dt(tt)
      ENDIF ELSE $
        Message,"Can't set TIME: Interp/Start/End Times empty in DATA structure!",/info
    ENDIF ELSE $
      Message,"Can't set TIME: Null DATA PTR",/info
  ENDELSE 
END

;============================================
; SetTime
;  
;============================================

PRO qmodela::SetTime, time

  IF n_params() LT 1 THEN return

  IF isa(time,/idldt) THEN self.time = time ELSE BEGIN 
    tmp = strsep(time,'/')
    nn = n_elements(tmp)
    IF nn LT 6 THEN ss = 0.000 ELSE ss = tmp[5]
    IF nn LT 5 THEN mm = 0 ELSE mm = tmp[4]
    IF nn LT 4 THEN hh = 0 ELSE hh = tmp[3]
    IF nn LT 3 THEN Message,"Time must have at least 3 elements!"
    tt = var_to_dt(tmp[0],tmp[1],tmp[2],hh,mm,ss)
    self.time = tt
  ENDELSE 

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

PRO qmodela::Get, data = data, time=time, lon=lon, lat=lat, $
           extent=extent
  IF arg_present(data) THEN BEGIN 
     s=self->q2b::get(data = data)
     IF ptr_valid(data) THEN BEGIN 
      U = *((*self.data).u)
      V = *((*self.data).v)
      data =  [ [[u]],[[v]] ]
    ENDIF 
   ENDIF 
   IF arg_present(time) THEN time = self.time
   IF arg_present(lon) THEN lon = *((*self.data).lon)
   IF arg_present(lat) THEN lat = *((*self.data).lat)
   IF arg_present(extent) THEN $
     extent = [ self.lonpar[0], self.latpar[0], $
                self.lonpar[1], self.latpar[1] ]

   IF arg_present(latpar) THEN latpar = self.latpar
END



;============================================
; SelfHelp routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO qmodela::SelfHelp
   ok = Message_Dialog( "qmodela: No Self help available. Sorry!")
   Message,"No Self help available. Sorry!",/cont
END


;============================================
; Version
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

FUNCTION qmodela::Version

     ; Version number for this class

   rcsid = "$Id$"

     ; Find version number for member objects.
   s=execute( 'tags=tag_names({' + 'qmodela' + '})' ) 
   n_tags = n_elements(Tags)
   i = 0
   WHILE i LE n_tags-1 DO BEGIN 

     catch, error
     IF error NE 0 THEN BEGIN 
         ; Ignore 'undefined method' errors
       IF strpos( strupcase(!Error_state.Msg), $
                  "UNDEFINED METHOD" ) NE -1 THEN BEGIN 
         error = 0
         i = i+1
       ENDIF ELSE BEGIN 
         Message,!error_state.msg,/cont
         return,''
       ENDELSE 
     ENDIF 
     
     IF VarType( self.(i) ) EQ 'OBJECT' THEN BEGIN 
       IF obj_valid( self.(i)) THEN BEGIN 
         V =  Call_Method( "VERSION", self.(i) )
         nv = N_Elements(V)
         IF exist(member_versions) THEN $
            member_versions =  [ member_versions, v ] ELSE $
            member_versions =  v
       ENDIF 
     ENDIF 
     i =  i+1
   ENDWHILE 

     ; find version number for superclasses.
   super = Obj_Class(self,/Super,count=cnt)
        
   IF cnt NE 0 THEN BEGIN 
     WHILE i LE cnt-1 DO BEGIN 
       catch, error
       IF error NE 0 THEN BEGIN 
           ; Ignore 'undefined method' errors
         IF strpos( strupcase(!Error_state.Msg), $
                    "UNDEFINED METHOD" ) NE -1 THEN BEGIN 
           error = 0
           i = i+1
         ENDIF ELSE BEGIN 
           Message,!error_state.msg,/cont
           return,''
         ENDELSE 
       ENDIF 

       V  = call_method("VERSION",super[i])

       IF exist( super_versions ) THEN $
         super_versions =  [super_versions, v ] ELSE $
         super_versions =  v 
       i = i+1

     ENDWHILE 
   ENDIF

   versions =  rcsid

   IF exist(super_versions) THEN $
      versions =  [versions, super_versions]

   IF exist( member_versions ) THEN $
      versions =  [versions, member_versions ] 

   Catch,/cancel
  return,versions(uniq(versions,sort(versions)))
END

;============================================
; getTimeFromFile
;
;============================================

FUNCTION qmodela::GetTimeFromFile,file

   interpTime = 0
   IF NOT HDF_ishdf(file) THEN BEGIN 
     Message,"file is not HDF file. QMODEL data is",/cont
     return,0
   ENDIF 

   IF NOT isqmodel(file) THEN BEGIN 
     Message,"file is not a QMODEL file",/cont
     return,0
   ENDIF 

   sds_id = HDF_SD_START(file,/read) 
   IF sds_id gt 0 THEN BEGIN 
     HDF_SD_FILEINFO,sds_id,ndatasets,nattributes
     r = hdf_sd_AttrFind(sds_id,'INTERPTIME')
     IF r NE -1 THEN BEGIN 
       hdf_sd_attrInfo,sds_id,r,data=Itime,type=type
       HDF_SD_END,sds_id
       interpTime = vaptime2idldt(Itime)
     ENDIF ELSE BEGIN 
       Message,"No INTERPTIME, will calculate from Start/End Times",/info
       r1 = hdf_sd_AttrFind(sds_id,'STARTTIME')
       r2 = hdf_sd_AttrFind(sds_id,'ENDTIME')
       IF r1 NE -1 AND r2 NE -1 THEN BEGIN 
         hdf_sd_attrInfo,sds_id,r1,data=starttime,type=type
         hdf_sd_attrInfo,sds_id,r2,data=Endtime,type=type
         st = vaptime2idldt(starttime)
         et = vaptime2idldt(endtime)
         tt = mean([st.julian,et.julian])
         interpTime = jul_to_dt(tt)
         HDF_SD_END,sds_id
       ENDIF ELSE BEGIN 
         Message,"Can't find InterpTime, StartTime/EndTime",/cont
         HDF_SD_END,sds_id
         return,0
       ENDELSE 
     ENDELSE 
   ENDIF ELSE BEGIN 
     Message,"Error opening " + file,/cont
     return,0
   ENDELSE 

return, interpTime   
end


;============================================
; Time
;============================================

FUNCTION qmodela::Time
  self-> Get,time = time
  return,time
END

;============================================
; Type
;============================================

FUNCTION qmodela::type
  return,'VECTOR'
END

;============================================
; Definition Routine
;============================================

PRO qmodela__define
  junk = {qmodela, $
          time  : {idldt},$
          lonpar: fltarr(2),$
          latpar: fltarr(2), $
           INHERITS qmodel  }
END
