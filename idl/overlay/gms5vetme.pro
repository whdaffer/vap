;+
; NAME:  gms5VetMe.pro
; $Id$
;
; PURPOSE: Checks to see that the required subdirectories are present
;          for further processing, i.e. that we're at the top of the
;          required directory structure. Assumes we're interested in
;          'grid' and 'ir1'
;
; AUTHOR: William Daffer
;
; CATEGORY:  Qscat Vap/ Gms5 Image processing
;
; CALLING SEQUENCE:  good=gms5VetMe([type,/grid|/agrid )
; 
; INPUTS:  None required
;
; OPTIONAL INPUTS:  None
;       
; KEYWORD PARAMETERS:  
;
;     grid: If set, don't file on absence of agrid subdirectory.
;     agrid: If set, don't fail on absence of grid subdirectory.
;     ir1: If set, don't fail because 'ir2', 'ir3', or 'vis'
;          subdirectory is missing. This is the default.
;     ir2: If set, don't fail because 'ir1', 'ir3', or 'vis'
;          subdirectory is missing. 
;     ir3: If set, don't fail because 'ir1', 'ir2', or 'vis'
;          subdirectory is missing. 
;     vis: If set, don't fail because 'ir1', 'ir2', or 'ir3'
;          subdirectory is missing. 
;
; OUTPUTS:  
;
;  0 : failure! one of the the required subdirectories is missing!
;  1 : success! They're all there!
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  None
;
; RESTRICTIONS:  None
;
; PROCEDURE:  Do a 'findfile' on each of the subdirectories. Fail if
;            any are missing. 
;
; EXAMPLE:  
;
; MODIFICATION LOG:
;
; $Log$
; Revision 1.2  1999/04/02 17:47:53  vapuser
; Removed cal and doc subdirectories. Don't need them.
;
; Revision 1.1  1999/04/02 17:47:01  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION gms5vetme, type, agrid=agrid, grid=grid, ir1=ir1, ir2=ir2, ir3=ir3, vis=vis

  COMMON gms5_cmn, gms5initialized, gms5_data_topdir, $
    gms5_hdftemplates_saveset_file

  IF n_elements(gms5initialized) EQ 0 THEN gms5init

  grid = keyword_set(grid)
  agrid = keyword_set(agrid)
  ir1 = keyword_set(ir1)
  ir2 = keyword_set(ir2)
  ir3 = keyword_set(ir3)
  vis = keyword_set(vis)

  CASE strupcase(type ) OF 
    'IR1' : ir1 = 1
    'IR2' : ir2 = 1
    'IR3' : ir3 = 1
    'VIS' : vis = 1
    ELSE: BEGIN 
      Message,"unknown type! " + type,/cont
      return,0
    END
  ENDCASE 
  IF NOT (ir1 OR ir2 OR ir3 OR vis) THEN ir1 = 1


  ncal = 1
;  c = findfile(gms5_data_topdir + '/cal',count=ncal) 
;  IF ncal EQ 0 THEN $
;    Message,'Calibration Subdirectory is missing!',/cont
    
  ndoc = 1
;  d = findfile(gms5_data_topdir + '/doc',count=ndoc)
;  IF ncal EQ 0 THEN $
;    Message,'Calibration Subdirectory is missing!',/cont

  IF grid THEN BEGIN 
    g = findfile(gms5_data_topdir + '/grid',count=ngrid)
    IF ngrid EQ 0 THEN $
      Message,'Grid Subdirectory is missing!',/cont
  ENDIF 
  IF agrid THEN BEGIN 
    ag = findfile(gms5_data_topdir + '/grida',count=nagrid)
    IF nagrid EQ 0 THEN $
      Message,'grida Subdirectory is missing!',/cont
  ENDIF 

  IF ir1 THEN BEGIN 
    i1 = findfile(gms5_data_topdir + '/ir1',count=nir1)
    IF nir1 EQ 0 THEN $
     Message,'Ir1 Subdirectory is missing!',/cont
  ENDIF 
  IF ir2 THEN BEGIN 
    i2 = findfile(gms5_data_topdir + '/ir2',count=nir2)
    IF nir2 EQ 0 THEN $
      Message,'Ir2 Subdirectory is missing!',/cont
  ENDIF 

  IF ir3 THEN BEGIN 
    i3 = findfile(gms5_data_topdir + '/ir3',count=nir3)
    IF nir3 EQ 0 THEN $
      Message,'Ir3 Subdirectory is missing!',/cont
  ENDIF 
  
  IF vis THEN BEGIN 
    v = findfile(gms5_data_topdir + '/vis',count=nvis)
    IF nvis EQ 0 THEN $
      Message,'Vis Subdirectory is missing!',/cont
  ENDIF 

  vetted =  (ncal NE 0) AND (ndoc NE 0) 
   
  IF grid  THEN vetted =  vetted AND (ngrid  NE 0) 
  IF agrid THEN vetted =  vetted AND (nagrid NE 0) 
  IF ir1   THEN vetted =  vetted AND (nir1   NE 0) 
  IF ir2   THEN vetted =  vetted AND (nir1   NE 0)
  IF ir3   THEN vetted =  vetted AND (nir3   NE 0)
  IF vis   THEN vetted =  vetted AND (vis    NE 0) 

  return, vetted

END


