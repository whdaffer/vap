;+
; NAME:  qmodel_str.pro
; $Id$
; PURPOSE:  Defines the 'qmodel' structure
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:   Data I/O
;
;
;
; CALLING SEQUENCE:  structure=qmodel_str()
;
;
; 
; INPUTS:  
;
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;
;
; OUTPUTS:   1 structure of type QMODEL
;
;
;
; OPTIONAL OUTPUTS:  none
;
;
;
; COMMON BLOCKS:  
;
;   qmodel_cmn: containining 
;
;          qmodel_defined: if define the structure has been defined
;                          already, in which case this routine returns
;                          a copy of the alread defined structure. 
;           qmodel_size: The size of the structure 
;            qmodel:     The structure itself.
;
;
;
; SIDE EFFECTS:  None
;
;
;
; RESTRICTIONS:  None
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
; Revision 1.2  1998/10/07 18:29:36  vapuser
; Took out lon and lat arrays. Added some CreationTime,
; StartTime, EndTime and Lonpar, to make it like the
; HDF version.
;
; Revision 1.1  1998/10/01 17:59:17  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-


FUNCTION qmodel_str
  COMMON qmodel_cmn, qmodel_defined, qmodel

  rcsid = "$Id$"
  ;

  IF n_elements( qmodel_defined ) eq 0 THEN BEGIN

    hdr={ QMODELHDR, $
          LongName     : bytarr(48),$
          ShortName    : bytarr(24),$ 
          NLon         : 0l        ,$
          NLat         : 0l        ,$
          LonPar       : fltarr(3) ,$
          LatPar       : fltarr(3) ,$
          Region       : fltarr(4) ,$
          CreationTime : bytarr(16),$ ;vaptime, yyyy/mm/dd/hh/mm
          StartTime    : bytarr(16),$ ;yyyy/mqm/dd/hh/mm
          EndTime      : bytarr(16),$ ;yyyy/mm/dd/hh/mm
          Version      : bytarr(16) $
        }

    qmodel =  { QMODELDATA,$
                hdr : replicate(hdr,1), $
                U   : Ptr_New(), $
                V   : Ptr_New(), $
                Lon : Ptr_New(), $
                Lat : Ptr_New()  $
              }

    tmp = byte('QMODEL')
    nn = strlen('QMODEL')
    qmodel.hdr.ShortName[0:nn-1] = tmp
    qmodel_defined = 1
  ENDIF

RETURN, qmodel
end





