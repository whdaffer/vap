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
; Revision 1.3  1998/10/12 22:12:42  vapuser
; Worked on Read/Init
;
; Revision 1.2  1998/10/06 00:00:54  vapuser
; Added some Version stuff.
;
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
    ; ok = Dialog_Message(!Error_State.msg)
    Message,!error_state.msg,/cont
    return,0
  ENDIF 

  CASE 1 OF 
    N_Params() EQ 2: BEGIN 
      IF N_Elements(region) eq 4 THEN BEGIN 
        s = size(u)
        nlon = s[1]
        nlat = s[2]
        qmodel = qmodel_str()
        IF vartype( qmodel ) EQ 'STRUCTURE' THEN BEGIN 
          qmodel.u = Ptr_New(u)
          qmodel.v = Ptr_New(v)
          qmodel.nlon = nlon
          qmodel.nlat = nlat
          loninc =  (region[2]-region[0])/nlon
          latinc =  (region[3]-region[1])/nlat
          qmodel.lon = Ptr_New((findgen(nlon)*loninc+region[0])#replicate(1,nlat),/no_copy)
          qmodel.lon = Ptr_New(replicate(1,nlon)#(findgen(nlat)*latinc+region[1]),/no_cop)
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
   data =  (*self.data)
   Ptr_Free, data.U
   Ptr_Free, data.V
   Ptr_Free, data.Lon
   Ptr_Free, data.Lat
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
         IF IsQmodel(filename) THEN BEGIN 
           qmodel =  QmodelHdfRead(filename)
         IF vartype( qmodel ) EQ 'STRUCTURE' THEN BEGIN 
             IF Ptr_Valid( self.data ) THEN $
               Ptr_Free, self.data 
             self.data = Ptr_New( qmodel )
             status = 1
           ENDIF 
         ENDIF ELSE BEGIN 
           Message,"file " + filename + " must be a QMODEL HDF file",/cont
           status = 0
         ENDELSE 
       ENDIF ELSE BEGIN 
         q = qmodel_str()
         hdr = q.hdr
         openr,lun, filename, error=err,/get
         IF err EQ 0 THEN BEGIN 
           readu, lun, hdr
           ShortName = string( hdr.shortname )
           CASE ShortName OF 
             'QSCATVAPMODEL': BEGIN 
               u = fltarr( hdr.nlon, hdr.nlat )
               v = u
               readu, lun, u,v
               free_lun, lon
               q.U =  Ptr_New(U,/no_copy)
               q.V =  Ptr_New(V,/no_copy)
               self.data =  Ptr_New(Q,/no_copy)
             END
             ELSE: BEGIN 
               Message,filename + $
                 ": Can't recognize format, ID = " + id,/cont
               status = 0
             END
           ENDCASE 
         ENDIF ELSE BEGIN 
           Message,!error_state.msg,/cont
           status = 0
         ENDELSE 
       ENDELSE 
     ENDIF ELSE Message,'filename must be STRING',/cont
   ENDIF ELSE Message,'Usage s=qmodel->read(filename)',/cont
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

PRO   qmodel::Get, $
                nlon         = nlon, $       
                nlat         = nlat, $       
                region       = region, $     
                filename     = filename,$    
                lonpar       = lonpar,$      
                latpar       = latpar, $     
                shortname    = shortname,$   
                longname     = longname,$    
                starttime    = StartTime,$   
                EndTime      = EndTime,$     
                CreationTime = CreationTime,$
                _extra       = extra


   IF Arg_Present(nlon)      THEN nlon = self.nlon
   IF Arg_Present(nlat)      THEN nlat = self.nlat
   IF Arg_Present(region)    THEN region = self.region
   IF Arg_Present(filename)  THEN filename = self.filename
   IF Arg_Present(lonpar)    THEN lonpar = (*self.data).hdr.lonpar
   IF Arg_Present(latpar)    THEN latpar = (*self.data).hdr.latpar
   IF Arg_Present(ShortName) THEN ShortName = (*self.data).hdr.ShortName
   IF Arg_Present(LongName)  THEN LongName = (*self.data).hdr.LongName
   IF Arg_Present(StartTime) THEN StartTime = (*self.data).hdr.StartTime
   IF Arg_Present(EndTime)   THEN EndTime = (*self.data).hdr.EndTime
   IF Arg_Present(CreationTime) THEN CreationTime = (*self.data).hdr.CreationTime
END



;============================================
; GetPlotData
;============================================

FUNCTION qmodel::GetPLotData, u,v,lon,lat,ambiguity, $
               limit=limit, _extra=extra

  status = 0
  IF ptr_valid( self.data ) THEN BEGIN 
    u   = *(*self.data).u
    v   = *(*self.data).v
    lon = *(*self.data).lon
    lat = *(*self.data).lat
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
   versions = qmodel-> Q2b::Version()
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

