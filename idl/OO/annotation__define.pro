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

     ; Version number for this class

   rcsid = "$Id$"

     ; Find version number for member objects.
   Tags = Tag_Names(self)
   n_tags = n_elements(Tags)
   WHILE i LE n_tags-1 DO BEGIN 

     catch, error
     IF error NE 0 THEN BEGIN 
         ; Ignore 'undefined method' errors
       IF strpos( strupcase(!Error_state.Msg), $
                  "UNDEFINED METHOD" ) NE -1 THEN BEGIN 
         error = 0
         i = i+1
       ENDIF ELSE return,''
     ENDIF 
     
     IF VarType( self.(i) ) EQ 'OBJECT' THEN BEGIN 
       V =  Call_Method( "VERSION", self.(i) )
       nv = N_Elements(V)
       IF exist(member_versions) THEN $
          member_versions =  [ member_versions, v ] ELSE $
          member_versions =  v
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
         ENDIF ELSE return,''
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
  return,versions
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







