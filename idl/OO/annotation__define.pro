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
;                       SubTitle=SubTitle, $
;                       charsize=charsize, $
;                       charthick=charthick,$
;                       xmargin=xmargin, $
;                       ymargin=ymargin, $
;                       zmargin=zmargin)
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
; KEYWORD PARAMETERS:  
;
;             MainTitle : The Main Title
;              Xtitle   : The title FOR the X axis
;              Ytitle   : The title for the Y axis
;              Ztitle   : The title for the Z axis
;              SubTitle : The Sub-title
;              Charsize : float scalar. The character size in character units
;                         (default=1.0)
;              charThick : int scalar. The Character Thickness (!p.charthick)
;              x/y/zmargin: int 2-vectors. The margins (default=!{X,Y,Z}.margin)
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
; Revision 1.3  1999/04/09 15:47:43  vapuser
; worked on version method
;
; Revision 1.2  1998/10/01 17:53:32  vapuser
; Modified 'version' method so that it will report
; the versions of member classes. Put in some error handling
; so that it'll ignore calls to undefined 'version' methods.
;
; Revision 1.1  1998/10/01 16:39:59  vapuser
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

FUNCTION Annotation::Init,$
              MainTitle = MainTitle, $
              Xtitle=Xtitle, $
              Ytitle=Ytitle, $
              Ztitle=Ztitle, $
              SubTitle=SubTitle, $
              CharSize=charSize, $
              CharThick=CharThick,$
              Xmargin=Xmargin,$
              Ymargin=Ymargin,$
              Zmargin=Zmargin
              

   IF n_Elements(MainTitle) NE 0 THEN $
     IF VarType(MainTitle) EQ 'STRING' THEN self.MainTitle = MainTitle

   IF n_Elements(XTitle) NE 0 THEN $
     IF VarType(XTitle)  EQ 'STRING' THEN self.XTitle = XTitle

   IF n_Elements(YTitle) NE 0 THEN $
     IF VarType(YTitle)  EQ 'STRING' THEN self.YTitle = YTitle

   IF n_Elements(ZTitle) NE 0 THEN $
     IF VarType(ZTitle)  EQ 'STRING' THEN self.ZTitle = ZTitle
     
   IF n_Elements(SubTitle) NE 0 THEN $
     IF VarType(SubTitle)  EQ 'STRING' THEN self.SubTitle = SubTitle
   
   IF n_elements(charsize)  EQ 0 THEN charsize = 1.0
   IF n_elements(charthick) EQ 0 THEN charthick = 1.0

   IF n_elements(xmargin) NE 2 THEN xmargin = !x.margin
   IF n_elements(ymargin) NE 2 THEN ymargin = !y.margin
   IF n_elements(zmargin) NE 2 THEN zmargin = !z.margin

   self.charsize = charsize
   self.charthick = charthick
   self.xmargin = xmargin
   self.ymargin = ymargin
   self.zmargin = zmargin

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
              SubTitle=SubTitle, $
              CharSize=charSize, $
              CharThick=CharThick,$
              Xmargin=Xmargin,$
              Ymargin=Ymargin,$
              Zmargin=Zmargin

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

   IF n_elements(charsize) NE 0 THEN self.charsize = charsize
   IF n_elements(charthick) NE 0 THEN self.charthick = charthick
   IF n_elements(xmargin) EQ 2 THEN self.xmargin = xmargin
   IF n_elements(ymargin) EQ 2 THEN self.ymargin = ymargin
   IF n_elements(zmargin) EQ 2 THEN self.zmargin = zmargin

END


;============================================
; Get Routine
;============================================

PRO Annotation::Get, $
              MainTitle = MainTitle, $
              Xtitle=Xtitle, $
              Ytitle=Ytitle, $
              Ztitle=Ztitle, $
              SubTitle=SubTitle, $
              CharSize=charSize, $
              CharThick=CharThick,$
              Xmargin=Xmargin,$
              Ymargin=Ymargin,$
              Zmargin=Zmargin

              MainTitle = self.MainTitle
              Xtitle    = self.Xtitle
              Ytitle    = self.Ytitle
              SubTitle  = self.SubTitle
              charsize  = self.charsize
              charThick = self.charThick
              xmargin   = self.xmargin
              ymargin   = self.ymargin
              zmargin   = self.zmargin
END

;============================================
; 
;============================================
;============================================
; Definition Routine
;============================================

PRO Annotation__define
  junk = {Annotation,$
         MainTitle : '' ,$
         Xtitle    : '' ,$
         Ytitle    : '' ,$
         Ztitle    : '' ,$
         Subtitle  : '' ,$
         CharSize  : 1.0, $
         CharThick : 1.0, $
         Xmargin   : [0,0],$
         Ymargin   : [0,0], $
         Zmargin   : [0,0] }
END







