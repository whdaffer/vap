;+
; NAME:  Qmodel__define
; $Id$
; PURPOSE:  Define an object of type QModel
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:   OO
;
;
;
; CALLING SEQUENCE:   qmodel=Obj_New('qmodel',data)
;
;
; 
; INPUTS:  
;
;       U : 2-dim array, U component of field
;       V : 2-dim array, V component of field
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;   Region: 4-vector , [lonmin, latmin, lonmax, latmax]. Must
;           be present if inputs U/V are or if keyword 'data=data' is.
;
;   Data: Structure of type Qmodel. 
;
;   Filename: Name of file containing model data. 
;             This file must be an HDF file with field NLON, NLAT,
;             REGION, U and V defined in the natural way.
;
; OUTPUTS:  
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
; Revision 1.1  1998/10/01 17:50:57  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
;============================================

FUNCTION qmodel::Init, U, V, $
                 Data     = Data,$
                 Filename = Filename ,$
                 Region   = Region, $
                 No_Copy  = No_Copy,$
               _extra=extra


  status = 0
  no_copy = keyword_set(no_copy)
  Catch, Error
  IF Error NE 0 THEN BEGIN 
    Catch,/Cancel
    ok = Dialog_Message(!Error_State.msg)
    Message,!error_state.msg,/cont
    return,0
  ENDIF 

  CASE 1 OF 
    N_Params() EQ 2: BEGIN 
      IF N_Elements(region) eq 4 THEN BEGIN 
        s = size(u)
        nlon = s[1]
        nlat = s[2]
        qmodel = qmodel_str(nlon,nlat)
        IF vartype( qmodel ) EQ 'STRUCTURE' THEN BEGIN 
          qmodel.u = u
          qmodel.v = v
          qmodel.nlon = nlon
          qmodel.nlat = nlat
          loninc =  (region[2]-region[0])/nlon
          latinc =  (region[3]-region[1])/nlat
          qmodel.lon =  (findgen(nlon)*loninc+region[0])#replicate(1,nlat)
          qmodel.lon =  replicate(1,nlon) # (findgen(nlat)*latinc+region[1])
          qmodel.region = region
          status = 1
        ENDIF ELSE $
          Message,"Couldn't create QMODEL structure for U/V",/cont
      ENDIF ELSE $
        Message,"When using U/V, 'REGION' keyword must be used",/cont
    END 
    N_Elements(Data) NE 0 : BEGIN 
      IF VarType( Data ) eq 'STRUCTURE' THEN BEGIN 
        IF Tag_Names( Data, /Structure_Name ) EQ 'QMODEL' THEN BEGIN 
          s           = size( data.nlon)
          region      = data.region
          self.data   =  Ptr_New( data, No_COPY=No_Copy ) 
          self.nlon   = s[1]
          self.nlat   = s[2]
          self.region = region
          status = 1
        ENDIF ELSE Message,"Data must be of type 'QMODEL'",/cont
      ENDIF ELSE Message,"Data must be a STRUCTURE",/cont
    END 
    N_Elements(Filename) NE 0: BEGIN 
      self.filename = filename
      status = self->read(filename)
    END 
    ELSE: Message,$
      "Usage, q=Obj_New('qmodel', U,V | Data =data | File =file [... )",/cont
  ENDCASE 
  return,status
END

;============================================
; Cleanup
;============================================

PRO qmodel::Cleanup
   self-> Q2b::Cleanup
END

;============================================
; Read Routine
;============================================
FUNCTION Qmodel::Read, filename 
   status = 0
   IF n_elements(filename) ne 0 THEN BEGIN 
     IF VarType( filename ) EQ  'STRING' THEN BEGIN 
       IF hdf_isHdf(filename) THEN BEGIN 
         qmodel =  qmodelhdfread(filename)
         IF vartype( qmodel ) EQ 'STRUCTURE' THEN BEGIN 
           IF Ptr_Valid( self.data ) THEN $
            Ptr_Free, self.data 
           self.data = Ptr_New( qmodel )
           status = 1
         ENDIF 
       ENDIF ELSE $
         Message,filename + ' Must be HDF file, at the moment',/cont
     ENDIF ELSE Message,'filename must be STRING',/cont
   ENDIF ELSE Message,'Usage qmodel->read,filename',/cont
   return, status
END




;============================================
; Set Routine
;============================================

FUNCTION qmodel::Set, _extra=extra
   return,1
END


;============================================
; Get Routine
;============================================

FUNCTION  qmodel::Get, $
                nlon = nlon, $
                nlat=nlat, $
                region=region, $
                filename=filename,_extra=extra
   IF Arg_Present(nlon)     THEN nlon = self.nlon
   IF Arg_Present(nlat)     THEN nlat = self.nlat
   IF Arg_Present(region)   THEN region = self.region
   IF Arg_Present(filename) THEN filename = self.filename

  return,1
END



;============================================
; GetPlotData
;============================================

FUNCTION qmodel::GetPLotData, u,v,lon,lat,ambiguity, $
               limit=limit, _extra=extra

  status = 0
  IF ptr_valid( self.data ) THEN BEGIN 
    u   = (*self.data).u
    v   = (*self.data).v
    lon = (*self.data).lon
    lat = (*self.data).lat
    IF n_elements(limit) EQ 4 THEN BEGIN 
      x = where( lon Ge limit[0] AND lon LE limit[2] AND $
                 lat Ge limit[1] AND lat LE limit[3], nx )
      IF nx NE 0 THEN BEGIN 
        u = u[x]
        v = v[x]
        lon = lon[x]
        lat = lat[x]
        status = 1
      ENDIF ELSE Message,' No data in specified range '
    ENDIF ELSE status = 1
  ENDIF 
  return,status
END


;============================================
; SelfHelp routine
;============================================

PRO qmodel::SelfHelp
   ok = Message_Dialog( "QMODEL: No Self help available. Sorry!")
   Message,"No Self help available. Sorry!",/cont
END


;============================================
; Version
;============================================

FUNCTION Qmodel::Version

     ; Version number for this class

   rcsid = "$Id$"
   versions = Q2b::Version()
   versions = [rcsid, versions]
   return, versions(uniq(versions,sort(versions)))

END


;============================================
; Definition Routine
;============================================

PRO qmodel__define
  junk = {$
          QMODEL,   $
          LatInc: 0., $
          LonInc: 0., $
          NLon : 0L , $
          Nlat: 0L  , $
          Region: fltarr(4), $ ; region of model
          INHERITS q2b }             
END

