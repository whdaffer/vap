;+
; NAME:  qmodel_str.pro
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
; CALLING SEQUENCE:  structure=qmodel_str(nlon,nlat)
;
;
; 
; INPUTS:  
;
;      nlon : number of longitude elements
;      nlat : number of latitude elements
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;     region - a 4-vector defining the region.
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
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
FUNCTION qmodel_str, nlon, nlat, region=region
COMMON qmodel_cmn, qmodel_defined, qmodel_size, qmodel

rcsid = "$Id$"
;
IF N_Elements( nlon ) EQ 0 THEN nlon = 360.
IF N_Elements( nlat ) EQ 0 THEN nlat = 121


IF n_elements( qmodel_defined ) eq 0 THEN BEGIN

  qmodel =  { QMODELDATA,$
              U      : fltarr(nlon, nlat ),$
              V      : fltarr(nlon, nlat ),$
              lon    : fltarr(nlon, nlat ),$
              lat    : fltarr(nlon, nlat ),$
              Region : fltarr(4)           } ;[ lonmin, latmin, lonmax, latmax ]

  qmodel_defined = 1
  qmodel_size = n_Tags( qmodel, /length )
ENDIF
retmodel = qmodel

IF n_elemetns(region) EQ 4 THEN retmodel.region = region

RETURN, retmodel
end





