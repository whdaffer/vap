;+
; NAME:  SetX
; $Id$
; PURPOSE:  Define the X plotting environment.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE:  setx [,ncolors=ncolors ,/xfont]
; 
; INPUTS:  None
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  
;
;   NCOLORS: scalar. Set window with number of colors given in the
;            argument. Only works if you have not opened any windows
;
;   XFONT: calls the program Xfont to let you pick the font you want.
;          The default font is 
;          -adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1
;          if xfont isn't called or fails in some way.
;
;   PSEUDO: Do a device,pseudo=8 call. Only works if this is the first
;           connection to the Xserver.
;
; OUTPUTS:  nothing
;
; OPTIONAL OUTPUTS: none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  Sets the default device font and number of colors, if
;               this is the first window opened in the IDL session
;
; RESTRICTIONS:   Device must be 'X' (print,!d.name or help,/device to
;               determine what you output device is set to)
;
;
; PROCEDURE:  Open a small window (with colors=ncolor if that keyword
;            is set) call device, font=somefont. That font is either
;            the default font or the font returned by the call to
;            xfont.
;
;
; EXAMPLE:  Geeze, you can't figure it out?
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1999, William Daffer
;-
;
;  No Warranties
;
PRO setx, ncolors=ncolors, xfont=xfont, pseudo=pseudo

   FORWARD_FUNCTION xfont
   IF !d.name NE 'X' THEN BEGIN 
     Message,"Output Device must be set to the 'X' device!",/cont
     return
   ENDIF 
   nn = !d.n_colors
   IF keyword_set(pseudo) THEN device,pseudo=8
   IF n_elements(ncolors) NE 0 THEN BEGIN 
     window, xsize=5,ysize=5,/free,/pixmap, colors=ncolors 
     IF nn EQ !d.n_colors AND $
        nn NE ncolors THEN $
       Message,"Attempt to set ncolors failed! This isn't the first window!",/cont
   ENDIF ELSE BEGIN 
     window, xsize=5,ysize=5,/free,/pixmap
   ENDELSE 
    !p.font = 0      ; Use hardware font          
      ; This font is good for SGIs, you'll
      ; probably want to change it for other
      ; machines
    IF keyword_set(xfont) THEN BEGIN 
      t = xfont()
      IF strlen(t) NE 0 THEN $
        device, font=t ELSE $
        device, font="-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1"
    ENDIF ELSE $
      device,font="-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1"
    wdelete          ; Delete window created
END
