;+
; NAME:  Annotation__define
; PURPOSE:  Defines an object useful in Annotations
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  OO
;
;
;
; CALLING SEQUENCE:  
;
;   Annotation=Obj_New('annotation', $
;                       MainTitle = MainTitle, $
;                       Xtitle=Xtitle, $
;                       Ytitle=Ytitle, $
;                       Ztitle=Ztitle, $
;                       SubTitle=SubTitle
;
;
;
; 
; INPUTS:  None
;
;
;
; OPTIONAL INPUTS:   None
;
;
;	
; KEYWORD PARAMETERS:  All Keywords are strings.
;
;             MainTitle : The Main Title
;              Xtitle   : The title FOR the X axis
;              Ytitle   : The title for the Y axis
;              Ztitle   : The title for the Z axis
;              SubTitle : The Sub-title
;
;
;
; OUTPUTS:  If successful, an object of type 'annotation'. 
;           If not, A null object.
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  None
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
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
;============================================
; Init
;============================================

FUNCTION Annotation::Init,$
              MainTitle = MainTitle, $
              Xtitle=Xtitle, $
              Ytitle=Ytitle, $
              Ztitle=Ztitle, $
              SubTitle=SubTitle

   IF n_Elements(MainTitle) NE 0 THEN $
     IF VarType(MainTitle) EQ 'STRING' THEN self.MainTitle = MainTitle

   IF n_Elements(XTitle) NE 0 THEN $
     IF VarType(XTitle) EQ 'STRING' THEN self.XTitle = XTitle

   IF n_Elements(YTitle) NE 0 THEN $
     IF VarType(YTitle) EQ 'STRING' THEN self.YTitle = YTitle

   IF n_Elements(ZTitle) NE 0 THEN $
     IF VarType(ZTitle) EQ 'STRING' THEN self.ZTitle = ZTitle
     
   IF n_Elements(SubTitle) NE 0 THEN $
     IF VarType(SubTitle) EQ 'STRING' THEN self.SubTitle = SubTitle

  return,1
END


;============================================
; Cleanup
;============================================

PRO Annotation::Cleanup
END


;============================================
; Set Routine
;============================================

PRO Annotation::Set, $
              MainTitle = MainTitle, $
              Xtitle=Xtitle, $
              Ytitle=Ytitle, $
              Ztitle=Ztitle, $
              SubTitle=SubTitle

   IF n_Elements(MainTitle) NE 0 THEN $
     IF VarType(MainTitle) EQ 'STRING' THEN self.MainTitle = MainTitle

   IF n_Elements(XTitle) NE 0 THEN $
     IF VarType(XTitle) EQ 'STRING' THEN self.XTitle = XTitle

   IF n_Elements(YTitle) NE 0 THEN $
     IF VarType(YTitle) EQ 'STRING' THEN self.YTitle = YTitle

   IF n_Elements(ZTitle) NE 0 THEN $
     IF VarType(ZTitle) EQ 'STRING' THEN self.ZTitle = ZTitle
     
   IF n_Elements(SubTitle) NE 0 THEN $
     IF VarType(SubTitle) EQ 'STRING' THEN self.SubTitle = SubTitle

END


;============================================
; Get Routine
;============================================

PRO Annotation::Get, $
              MainTitle = MainTitle, $
              Xtitle=Xtitle, $
              Ytitle=Ytitle, $
              Ztitle=Ztitle, $
              SubTitle=SubTitle

              MainTitle = self.MainTitle
              Xtitle    = self.Xtitle
              Ytitle    = self.Ytitle
              SubTitle  = self.SubTitle
END



;============================================
; Version
;============================================

FUNCTION Annotation::Version
   rcsid = "$Id$"
   super = Obj_Class(self,/Super,count=cnt)
   IF cnt NE 0 THEN BEGIN 
     versions = strarr(cnt+1)
     versions[0] = rcsid
     FOR i=0,cnt-1 DO versions[i] = call_method("VERSION",super[i])
     return,versions
   ENDIF ELSE return,rcsid
END


;============================================
; Definition Routine
;============================================

PRO Annotation__define
  junk = {Annotation,$
         MainTitle : '' ,$
         Xtitle    : '' ,$
         Ytitle    : '' ,$
         Ztitle    : '' ,$
         Subtitle  : '' }
END







