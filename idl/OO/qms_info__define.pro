;+
; NAME:  qms_info__define
; $Id$
; PURPOSE:  defines structure of qms_info, which is passed around in
;          the widgeting portion of the QMS object.
;
; AUTHOR:  whd
;
; CATEGORY:  Seawinds visualization
;
; CALLING SEQUENCE:  If one were so inclined, one could say 
;                    qms_info={qms_info}, but there should be no need
;                    for the user to ever do this. This routine exists
;                    solely to provide a bit of functionality to the
;                    QMS object.
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
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
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 2001, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO qms_info__define
  junk = {qms_info,$
          groupid: 0L, $
          tlb: 0L, $
          fileid: 0l, $
          quitID: 0L, $
          configid: 0l, $
          wid: 0l, $
          pixid: 0l, $
          tlbxsize: 0l, $
          tlbysize: 0l }
END

