;+
; NAME:  psform__define.pro
; $Id$
; PURPOSE:  Defines the PSFORM object
;
; AUTHOR:  William Daffer
;
; CATEGORY:  General OO programming, plot/Image Output 
;
; CALLING SEQUENCE:  psform=obj_net('psform')
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  Look at the function psform::init
;
; OUTPUTS:  an object of type psform
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  Heap memory is used. The object must be destroyed
;               correctly!
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
; Revision 1.1  1999/04/09 21:36:45  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-

;============================================
; Init
;============================================

FUNCTION psform::init, $
    Xsize          = Xsize, $ 
    Ysize          = Ysize, $ 
    Xoff           = Xoff, $  
    Yoff           = Yoff, $  
    Landscape      = Landscape, $
    Exacpsulated   = Encapsulated, $
    Inches         = Inches, $
    Color          = color, $
    Bits_per_pixel = Bits_Per_Pixel, $
    Filename       = filename,$
    setup          = setup,$
    parent         = parent
               
    CD,Current=thisdir
    IF n_Elements(xsize) EQ 0 THEN xsize = 7.0
    IF N_Elements(ysize) EQ 0 THEN ysize = 5.0
    IF n_Elements(xoff)  EQ 0 THEN xoff = 1.0
    IF N_Elements(yoff)  EQ 0 THEN yoff = 3.0
    IF n_Elements(inches) EQ 0 THEN inches = 1
    IF n_elements(color) EQ 0 THEN color = 0
    IF n_elements(bits_Per_pixel) EQ 0 THEN bits_Per_pixel = 2
    IF n_elements(encapsulated) EQ 0 THEN encapsulated = 0
    IF n_elements(landscape) EQ 0 THEN landscape = 0
    IF n_elements(filename) EQ 0 THEN filename = thisdir + 'idl.ps'
    IF N_Elements(parent) EQ 0 THEN parent = 0L
    
    self.xsize = xsize
    self.ysize = ysize
    self.xoff = xoff
    self.yoff = yoff
    self.inches = inches
    self.color = color
    self.bits_per_pixel = bits_per_pixel
    self.encapsulated = encapsulated
    self.landscape = landscape
    self.filename = filename

    IF keyword_set(setup) THEN self-> setup
     

return,1
END

;============================================
; Cleanup
;============================================


PRO PsForm::Cleanup
END

;============================================
; Setup
;============================================
PRO psform::setup
  forward_FUNCTION ps_form
  ps = PS_FORM( xsize          = self.xsize, $    
                ysize          = self.ysize, $    
                xoff           = self.xoff, $     
                yoff           = self.yoff, $     
                color          = self.color, $    
                inches         = self.inches, $   
                landscape      = self.landscape, $
                encapsulated   = self.encapsulated, $
                bits_per_pixel = self.bits_per_pixel, $
                filename       = self.filename, $
                parent         = self.parent )
  
  IF VarType(ps) eq 'STRUCTURE' THEN BEGIN 
    self.xsize = ps.xsize
    self.ysize = ps.ysize
    self.xoff = ps.xoff
    self.yoff = ps.yoff
    self.inches = ps.inches
    self.color = ps.color
    self.bits_per_pixel = ps.bits_per_pixel
    self.encapsulated = ps.encapsulated
    self.landscape = ps.landscape
    self.filename = ps.filename
  ENDIF 
END



;============================================
; set
;============================================

Pro Psform::Set, $
    Xsize = Xsize, $
    Ysize = Ysize, $
    Xoff  = Xoff, $
    Yoff  = Yoff, $
    Landscape = Landscape, $
    Exacpsulated = Encapsulated, $
    Inches = Inches, $
    Color = color, $
    Bits_per_pixel = Bits_Per_Pixel, $
    Filename = filename, $
    parent=parent

   IF N_elements(Xsize) NE 0 THEN self.xsize = xsize
   IF N_elements(Xoff) NE 0 THEN self.xoff = xoff
   IF N_elements(Ysize) NE 0 THEN self.ysize = ysize
   IF N_elements(Yoff) NE 0 THEN self.yoff = yoff
   IF N_elements(Landscape) NE 0 THEN self.landscape = landscape
   IF N_elements(Encapsulated) NE 0 THEN self.encapsulated = encapsulated
   IF N_elements(Inches) NE 0 THEN self.inches = inches
   IF N_elements(Color) NE 0 THEN self.color = color
   IF N_elements(Bits_per_pixel) NE 0 THEN self.bits_per_pixel = bits_per_pixel
   IF N_elements(Filename) NE 0 THEN self.filename = filename
   IF N_elements(Parent) NE 0 THEN self.parent = parent

END


;============================================
; Get
;============================================

PRO psform::Get, $
          Xsize          = Xsize, $
          Ysize          = Ysize, $
          Xoff           = Xoff,  $
          Yoff           = Yoff,  $
          Landscape      = Landscape, $
          Exacpsulated   = Encapsulated, $
          Inches         = Inches, $
          Color          = color, $
          Bits_per_pixel = Bits_Per_Pixel, $
          Filename       = filename, $
          PS             = PS

   IF Arg_Present(xsize)     THEN Xsize     = self.Xsize 
   IF Arg_Present(xoff)      THEN Xoff      = self.Xoff   
   IF Arg_Present(ysize)     THEN Ysize     = self.Ysize 
   IF Arg_Present(yoff)      THEN Yoff      = self.Yoff   
   IF Arg_Present(landscape) THEN landscape = self.landscape
   IF Arg_Present(Inches)    THEN Inches    = self.Inches
   IF Arg_Present(Color)     THEN Color     = self.Color
   IF Arg_Present(Filename)  THEN Filename  = self.Filename   
   IF Arg_Present(encapsulated) THEN Encapsulated = self.Encapsulated
   IF Arg_Present(Bits_per_pixel) THEN Bits_per_pixel = self.Bits_per_pixel

   IF Arg_Present(ps) THEN ps = { Xsize        : self.Xsize, $
                                  ysize        : self.Ysize, $
                                  Xoff         : self.Xoff   , $
                                  Yoff         : self.Yoff   , $
                                  LandScape    : self.landscape, $
                                  Encapsulated : self.Encapsulated, $
                                  Inches       : self.Inches, $
                                  Color        : self.Color, $
                                  filename     : self.Filename, $   
                                  Bit_Per_Pixel: self.bits_per_pixel }
   

END

;============================================
; Definition
;============================================


PRO psform__define                
  junk = {psform, $
          Xsize       :0. ,$
          Ysize       :0. ,$
          Xoff        :0. ,$
          Yoff        :0. ,$
          LandScape   :0  ,$
          Encapsulated:0  ,$
          Inches      :0  ,$
          Color       :0  ,$
          filename    :'' ,$
          Bits_Per_Pixel :0,$
         parent: 0L }
END


