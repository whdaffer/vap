;+
; NAME:  QmodelHdfRead
; $Id$
; PURPOSE:  Read a Qmodel HDF file
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  HDF Data I/O
;
;
;
; CALLING SEQUENCE:  qmodel=QmodelHdfRead(filename)
;
;
; 
; INPUTS:  
;
;    Filename : string, fully qualified HDF filename
;
;
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:   None
;
;
;
; OUTPUTS:  
;
;   Qmodel : A structure as defined by Qmodel_str.pro.
;            If failure, a 0 is returned.
;
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  
;
;  It appears the HDF interface is pretty fragile. If the open fails
;  for some reason, it interface may be left in soe odd state such
;  that subsequent attemps to open the HDF interface may lead to IDL
;  crashing. Hey, don't blame it on me.

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
; Revision 1.2  1999/03/17 20:58:15  vapuser
; fixed error in computation of qmodel.lon/.lat
;
; Revision 1.1  1998/10/12 22:21:13  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION qmodelhdfread, filename

  rcsid = "$Id:"
  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,0
  ENDIF 

  IF N_Elements(filename) EQ 0 THEN BEGIN 
    Message,'Usage: result=qmodelhdfread( filename )',/cont
    return,0
  ENDIF 
  IF vartype( filename ) NE 'STRING' THEN BEGIN 
    Message,' filename must be of type STRING',/cont
    return,0
  ENDIF 

  IF Hdf_IsHdf(filename) THEN BEGIN 

    sds_id = HDF_SD_START(filename,/read) 
    IF sds_id gt 0 THEN BEGIN 
      HDF_SD_FILEINFO,sds_id,datasets,attributes
      print,'Number of SD data sets: ',datasets
      print,'Number of attributes:   ',attributes
      qmodel = qmodel_str()
      lf =  string(10b)
      FOR ai =0,attributes-1 DO BEGIN 
        HDF_SD_ATTRINFO,sds_id,ai,name=name,$
           type=type,count=count,data=data
        IF strupcase(type) EQ 'STRING' THEN BEGIN 
          tmp = str_sep( data, lf )
          tmp = tmp(where(strlen(tmp)))
          data =  tmp(n_elements(tmp)-1)
          name =  strupcase( name )
          CASE name OF 
            'STARTTIME': BEGIN 
              n = n_elements(qmodel.hdr.StartTime)
              nn = strlen(data) <  n
              tmp = byte(data)
              qmodel.hdr.StartTime[0:nn-1] = tmp[0:nn-1]
            END 
            'ENDTIME': BEGIN 
              n = n_elements(qmodel.hdr.EndTime)
              nn = strlen(data) <  n
              tmp = byte(data)
              qmodel.hdr.EndTime[0:nn-1] = tmp[0:nn-1]
            END 
            'CREATIONTIME': BEGIN 
              n = n_elements(qmodel.hdr.CreationTime)
              nn = strlen(data) <  n
              tmp = byte(data)
              qmodel.hdr.CreationTime[0:nn-1] = tmp[0:nn-1]
            END 
            'VERSION': BEGIN 
              n = n_elements(qmodel.hdr.Version)
              nn = strlen(data) <  n
              tmp = byte(data)
              qmodel.hdr.Version[0:nn-1] = tmp[0:nn-1]
            END 
            'LONGNAME': BEGIN 
              n = n_elements(qmodel.hdr.LongName)
              nn = strlen(data) <  n
              tmp = byte(data)
              qmodel.hdr.Longname[0:nn-1] = tmp[0:nn-1]
            END 
            'SHORTNAME': BEGIN 
              n = n_elements(qmodel.hdr.ShortName)
              nn = strlen(data) <  n
              tmp = byte(data)
              qmodel.hdr.Shortname[0:nn-1] = tmp[0:nn-1]
            END 
            'EXCLUDE_COLS': BEGIN 
              n = n_elements(qmodel.hdr.Exclude_Cols)
              nn = strlen(data) <  n
              tmp = byte(data)
              qmodel.hdr.exclude_cols[0:nn-1] = tmp[0:nn-1]
            END 
            ELSE: $
              Message,'Unrecognized Attribute ' + name,/cont
          ENDCASE

        ENDIF ELSE BEGIN 
          CASE name OF 
            'NLON'       : qmodel.hdr.nlon = data
            'NLAT'       : qmodel.hdr.nlat = data
            'REGION'     : qmodel.hdr.region = data
            'LONPAR'     : qmodel.hdr.lonpar = data
            'LATPAR'     : qmodel.hdr.latpar = data
            'RAINF'      : qmodel.hdr.rainf =  ptr_new(data)
            'ERMAX'      : qmodel.hdr.ermax = ptr_new(data)
            'CRDECIMATE' : qmodel.hdr.crdecimate = data
            'DECIMATE'   : qmodel.hdr.decimate = data
            ELSE:
          ENDCASE
        ENDELSE 
      ENDFOR


      r = HDF_SD_NAMETOINDEX(sds_id, 'U')
      r = HDF_SD_SELECT( sds_id, r )
      HDF_SD_GETINFO, r, ndims=nd, dims=dims, type=ty, unit=un; , caldata=cal
      HDF_SD_GETDATA,r, U

      r = HDF_SD_NAMETOINDEX(sds_id, 'V')
      r = HDF_SD_SELECT( sds_id, r )
      HDF_SD_GETINFO, r, ndims=nd, dims=dims, type=ty, unit=un; , caldata=cal
      HDF_SD_GETDATA,r, V


        ; End Access to this file.
      HDF_SD_END, sds_id

      nlon = dims[0]
      nlat = dims[1]

      qmodel.hdr.nlon   = nlon
      qmodel.hdr.nlat   = nlat
      qmodel.u      = Ptr_New( u,/no_copy )
      qmodel.v      = Ptr_New( v, /no_copy)
      region = qmodel.hdr.region
      loninc =  (region[2]-region[0]+1)/nlon
      latinc =  (region[3]-region[1]+1)/nlat
      qmodel.lon =  Ptr_New((findgen(nlon)*loninc+region[0])#replicate(1,nlat), /no_Copy)
      qmodel.lat =  Ptr_New(replicate(1,nlon)#(findgen(nlat)*latinc+region[1]), /no_Copy)
    ENDIF ELSE Message,"Can't open " + filename,/cont
  ENDIF ELSE Message,filename  + ' Must be HDF ',/cont


  return, qmodel
END

