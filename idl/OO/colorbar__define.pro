;+
; NAME:   colorbar__define
; PURPOSE:   Defines and object of type colorbar
;
; AUTHOR: William Daffer
;
; CATEGORY:   OO
;
;   Object oriented version of my ColBar.pro which is, itself, stolen
;   shamelessly from Dave Fanning's ColorBar.pro.
;
; CALLING SEQUENCE:   colorbar = obj_new('colorbar',...)
; 
; METHODS:
;   Init:
;   Set:
;   Get:
;   Cleanup:
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
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  

;============================================

FUNCTION colorbar::Init,$
                 BOTTOM=bottom, $ ; bottom color index
                 CHARSIZE=charsize, $ ; size of annotation characters 
                 COLOR=color, $ ; color of border (def=!d.n_colors-1)
                 DIVISIONS=divisions, $ ; Numbe of divisions in colorbar
                                        ; def=2
                 FORMAT=format, $             ; format for numbering
                 POSITION=position, $        ; position of colorbar 
                                ; (in normalized coords)
                 MAX=max, $                   ; Min data value on bar
                 MIN=min, $                  ; Max data value on bar
                 NCOLORS=ncolors,  $         ; number of colors
                 PSCOLOR=pscolor, $     ; use with Postscript output
                 TITLE=title, $         ; Bar title
                 VERTICAL=vertical, $   ; Make the bar be vertical
                  TOP=top, $            ; put division markings on top, 
                                        ; if horizontal?
                                        ; Normally the title is on the top and
                                        ; the divisions on the bottom
                 RIGHT=right,$          ; put divisions on Right if vertical?
                                        ; Normally the divisions are on the
                                        ; left and the title on the right.
                 TRUE=TRUE, $           ; true color visual
                 TABLE=TABLE            ; Take the Color table from this table,  
                                        ; rather than the table one would get
                                        ; from a 'tvlct' call. (ignored when
                                        ; when true=0)


     ; Is the PostScript device selected?

  self.postScript = (!D.NAME EQ 'PS')
  true = keyword_set(true)
  ;IF true AND self.postscript THEN BEGIN 
  ;  message," Can't use keyword 'TRUE' with postscript output",/cont
  ;  return
  ;ENDIF 
    ; Check and define keywords.

  IF N_ELEMENTS(ncolors) EQ 0 THEN BEGIN

     ; Most display devices to not use the 256 colors available to
     ; the PostScript device. This presents a problem when writing 
     ; general-purpose programs that can be output to the display or
     ; to the PostScript device. This problem is especially bothersome
     ; if you don't specify the number of colors you are using in the
     ; program. One way to work around this problem is to make the
     ; default number of colors the same for the display device and for
     ; the PostScript device. Then, the colors you see in PostScript are
     ; identical to the colors you see on your display. Here is one way to
     ; do it.

     IF self.postscript THEN BEGIN
        oldDevice = !D.NAME

           ; What kind of computer are we using? SET_PLOT to appropriate
           ; display device.

        thisOS = !VERSION.OS_FAMILY
        thisOS = STRMID(thisOS, 0, 3)
        thisOS = STRUPCASE(thisOS)
        CASE thisOS of
           'MAC': SET_PLOT, thisOS
           'WIN': SET_PLOT, thisOS
           ELSE: SET_PLOT, 'X'
        ENDCASE

           ; Open a window (to make sure !D.N_COLORS is accurate).

        WINDOW, /FREE, /PIXMAP, XSIZE=10, YSIZE=10
        WDELETE, !D.WINDOW

           ; Here is how many colors we should use.

        ncolors = !D.N_COLORS
        SET_PLOT, oldDevice
     ENDIF ELSE ncolors = !d.N_COLORS-1
  ENDIF  
  self.ncolors = ncolors

  IF N_ELEMENTS(bottom)    EQ 0 THEN bottom = 0B
  IF N_ELEMENTS(charsize)  EQ 0 THEN charsize = 1.0
  IF N_ELEMENTS(format)    EQ 0 THEN format = '(F6.2)'
  IF STRLEN(format[0])     EQ 0 THEN format = '(F6.2)'
  IF N_ELEMENTS(color)     EQ 0 THEN color = ncolors - 1 + bottom
  IF N_ELEMENTS(min)       EQ 0 THEN min = 0.0
  IF N_ELEMENTS(max)       EQ 0 THEN max = FLOAT(ncolors) - 1
  IF N_ELEMENTS(divisions) EQ 0 THEN divisions = 2
  IF N_ELEMENTS(title)     EQ 0 THEN title = ''
  vertical = keyword_set(vertical)
  right = keyword_set(right)
  top = keyword_set(top)
  pscolor = KEYWORD_SET(pscolor)
  true = keyword_set(true)

  self.bottom = bottom
  self.ncolors = ncolors
  self.charsize = charsize
  self.format = format
  self.color = color
  self.min = min
  self.max = max
  self.divisions = divisions
  self.title = title
  self.pscolor = pscolor
  self.true = true
  self.vertical = vertical
  self.right = right
  self.top = top

  IF self.vertical THEN BEGIN 
    IF N_ELEMENTS(position) EQ 0 THEN $
       self.position = [0.88, 0.15, 0.95, 0.95] ELSE $
       self.position = position
  ENDIF ELSE BEGIN 
    IF N_ELEMENTS(position) EQ 0 THEN $
      self.position = [0.15, 0.88, 0.95, 0.95] ELSE $
      self.position = position
  ENDELSE 

  IF n_elements(table) NE 0 THEN $
    self.table = ptr_new(table)

  return,1
END


;============================================
; Cleanup
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO colorbar::Cleanup
  ptr_free, self.table
  ptr_free, self.bar
END


;============================================
; Set Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO colorbar::Set, $
                 BOTTOM=bottom, $ ; bottom color index
                 CHARSIZE=charsize, $ ; size of annotation characters 
                 COLOR=color, $ ; color of border (def=!d.n_colors-1)
                 DIVISIONS=divisions, $ ; Numbe of divisions in colorbar
                                        ; def=2
                 FORMAT=format, $             ; format for numbering
                 POSITION=position, $        ; position of colorbar 
                                ; (in normalized coords)
                 MAX=max, $                   ; Min data value on bar
                 MIN=min, $                  ; Max data value on bar
                 NCOLORS=ncolors,  $         ; number of colors
                 PSCOLOR=pscolor, $     ; use with Postscript output
                 TITLE=title, $         ; Bar title
                 VERTICAL=vertical, $   ; Make the bar be vertical
                 TOP=top, $             ; put title on top? if vertical
                 RIGHT=right,$          ; put title on Right if vertical
                 TRUE=TRUE, $           ; true color visual
                 TABLE=TABLE            ; Take the Color table from this table,  
                                        ; rather than the table one would get
                                        ; from a 'tvlct' call. (ignored when


  recalc = 0
  IF N_ELEMENTS(bottom)   NE 0 THEN BEGIN 
    self.bottom = bottom
    recalc = 1
  ENDIF 
  IF N_ELEMENTS(divisions)NE 0 THEN BEGIN 
    self.divisions = divisions
    recalc = 1
  ENDIF 
  IF N_ELEMENTS(format)   NE 0 THEN BEGIN 
    self.format = format
    recalc = 1
  ENDIF 
  IF N_ELEMENTS(position) EQ 4 THEN BEGIN 
    self.position = float(position)
    recalc = 1
  ENDIF 
  IF N_ELEMENTS(min)      NE 0 THEN BEGIN 
    self.min = min
    recalc = 1
  ENDIF 
  IF N_ELEMENTS(max)      NE 0 THEN BEGIN 
    self.max = max
    recalc = 1
  ENDIF 
  IF N_ELEMENTS(ncolors)  NE 0 THEN BEGIN 
    self.ncolors = ncolors
    recalc = 1
  ENDIF 
  IF N_ELEMENTS(color)    NE 0 THEN BEGIN 
    self.color = color
    recalc = 1
  ENDIF 
  IF n_elements(pscolor)  NE 0 THEN BEGIN 
    self.pscolor = pscolor
    recalc = 1
  ENDIF 
  IF n_elements(vertical) NE 0 THEN BEGIN 
    self.vertical = vertical
    recalc = 1
  ENDIF 
  IF n_elements(true)     NE 0 THEN BEGIN 
    self.true = true
    recalc = 1
  ENDIF 
  IF n_elements(table)    NE 0 THEN BEGIN 
    *(self.table) = table
    recalc = 1
  ENDIF 

  
  IF N_ELEMENTS(title)    NE 0 THEN self.title = title
  IF N_ELEMENTS(charsize) NE 0 THEN self.charsize = charsize
  IF n_elements(pscolor)  NE 0 THEN self.pscolor = pscolor
  IF n_elements(right)    NE 0 THEN self.right = right
  IF n_elements(top)      NE 0 THEN self.top = top

  IF recalc THEN self-> calc

END


;============================================
; Get Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO colorbar::Get, $
                 BOTTOM=bottom, $ ; bottom color index
                 CHARSIZE=charsize, $ ; size of annotation characters 
                 COLOR=color, $ ; color of border (def=!d.n_colors-1)
                 DIVISIONS=divisions, $ ; Numbe of divisions in colorbar
                                        ; def=2
                 FORMAT=format, $             ; format for numbering
                 POSITION=position, $        ; position of colorbar 
                                ; (in normalized coords)
                 MAX=max, $                   ; Min data value on bar
                 MIN=min, $                  ; Max data value on bar
                 NCOLORS=ncolors,  $         ; number of colors
                 PSCOLOR=pscolor, $     ; use with Postscript output
                 TITLE=title, $         ; Bar title
                 VERTICAL=vertical, $   ; Make the bar be vertical
                 TOP=top, $             ; put title on top? if vertical
                 RIGHT=right,$          ; put title on Right if vertical
                 TRUE=TRUE, $           ; true color visual
                 TABLE=TABLE            ; Take the Color table from this table,  
                                        ; rather than the table one would get
                                        ; from a 'tvlct' call. (ignored when

  IF ARG_PRESENT(bottom)   NE 0 THEN bottom = self.bottom
  IF ARG_PRESENT(charsize) NE 0 THEN charsize = self.charsize
  IF ARG_PRESENT(divisions)NE 0 THEN divisions = self.divisions
  IF ARG_PRESENT(format)   NE 0 THEN format = self.format
  IF ARG_PRESENT(position) NE 0 THEN position = self.position
  IF ARG_PRESENT(min)      NE 0 THEN min = self.min
  IF ARG_PRESENT(max)      NE 0 THEN max = self.max
  IF ARG_PRESENT(ncolors)  NE 0 THEN ncolors = self.ncolors
  IF ARG_PRESENT(color)    NE 0 THEN color = self.color
  IF ARG_PRESENT(title)    NE 0 THEN title = self.title
  IF Arg_Present(pscolor)  NE 0 THEN pscolor = self.pscolor
  IF Arg_Present(pscolor)  NE 0 THEN pscolor = self.pscolor
  IF Arg_Present(vertical) NE 0 THEN vertical = self.vertical
  IF Arg_Present(right)    NE 0 THEN right = self.right
  IF Arg_Present(top)      NE 0 THEN top = self.top
  IF Arg_Present(true)     NE 0 THEN true = self.true
  IF Arg_Present(table)    NE 0 THEN table=*(self.table) 
            
END

;============================================
; Calcuate colorbar
;============================================
PRO colorbar::Calc, apply = apply
  IF self.vertical THEN $
     bar = REPLICATE(1B,10) # BINDGEN(256) ELSE $
     bar = BINDGEN(256) # REPLICATE(1B, 10)

     ; Scale the color bar.
   bar = BYTSCL(bar, TOP=self.ncolors-1) + self.bottom

     ; Get starting locations in DEVICE coordinates.
  self.xstart = self.position[0] * !D.X_VSIZE
  self.ystart = self.position[1] * !D.Y_VSIZE

     ; Get the size of the bar in DEVICE coordinates.

  self.xsize = (self.position[2] - self.position[0]) * !D.X_VSIZE
  self.ysize = (self.position[3] - self.position[1]) * !D.Y_VSIZE

     ; For PostScript output only, draw the annotation in !P.COLOR 
     ; unless "pscolor" is set. This makes better output on grayscale 
     ; printers. 


  IF self.postscript THEN BEGIN
    IF self.true THEN BEGIN 
      IF ptr_valid( self.table) THEN BEGIN 
        table = *(self.table)
        r = reform(table[0,*])
        g = reform(table[1,*])
        b = reform(table[2,*])
      ENDIF ELSE tvlct,r,g,b,/get
      self.bar = ptr_new([ [[r[bar]]],[[g[bar]]],[[b[bar]]] ],/no_copy)
      r = (g=(b=0))
    ENDIF ELSE self.bar =  ptr_new(bar,/no_copy)


  ENDIF ELSE BEGIN 
      ; Not postscript
    bar = self.bottom > $
            CONGRID(bar, CEIL(self.xsize), CEIL(self.ysize), /INTERP)<$
         (self.bottom+self.ncolors-1)
    IF self.true THEN BEGIN    
      IF ptr_valid(self.table) THEN BEGIN 
        table = *(self.table)
        r = reform(table[0,*])
        g = reform(table[1,*])
        b = reform(table[2,*])
      ENDIF ELSE tvlct,r,g,b,/get 
      self.bar =  ptr_new([ [[r[bar]]], [[g[bar]]], [[b[bar]]] ],/no_copy)
    ENDIF ELSE self.bar =  ptr_new(bar,/no_copy)

  ENDELSE 

  IF keyword_set(apply) THEN self-> apply

END

;============================================
; Apply
;  Apply the bar to the output device.
;============================================

PRO colorbar::apply
  IF NOT ptr_valid(self.bar) THEN self-> calc
  self.postscript = !d.name EQ 'PS'
  IF self.postscript THEN BEGIN 
    self-> Calc
    IF self.true THEN $
      TV, *(self.bar), self.xstart, self.ystart,$
           XSIZE=self.xsize,YSIZE=self.ysize,true=3 ELSE $
      TV, *(self.bar), self.xstart, self.ystart, $
          XSIZE=self.xsize, YSIZE=self.ysize
  ENDIF ELSE BEGIN
    IF self.true THEN  $
      TV,*(self.bar), self.xstart, self.ystart,true=3 ELSE $
      TV,*(self.bar), self.xstart, self.ystart
  ENDELSE 

  ; Annotate bar.

     ; Annotate the color bar.

;  IF self.postscript AND (self.pscolor NE 1) THEN BEGIN
  IF self.postscript THEN BEGIN
     self.oldcolor = self.color
     self.color = !P.COLOR
  ENDIF

  IF self.vertical THEN BEGIN

     IF self.right THEN BEGIN

        PLOT, [0,1], [self.min,self.max], /NODATA, XTICKS=1, $
         YTICKS=self.divisions, XSTYLE=1, YSTYLE=9, $
           POSITION=self.position, COLOR=self.color, $
            CHARSIZE=self.charsize, /NOERASE, $
             YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', YTICKLEN=0.1 , $
              YRANGE=[self.min, self.max], YTITLE=self.title

        AXIS, YAXIS=1, YRANGE=[self.min, self.max], $
            YTICKFORMAT=self.format, YTICKS=self.divisions, $
           YTICKLEN=0.1, YSTYLE=1, COLOR=self.color, CHARSIZE=self.charsize

     ENDIF ELSE BEGIN

        PLOT, [0,1],[self.min,self.max], /NODATA, XTICKS=1, $
         YTICKS=self.divisions, XSTYLE=1, YSTYLE=9, $
           POSITION=self.position, COLOR=self.color, $
            CHARSIZE=self.charsize, /NOERASE, $
             YTICKFORMAT=self.format, XTICKFORMAT='(A1)', YTICKLEN=0.1 , $
              YRANGE=[self.min,self.max]

        AXIS, YAXIS=1, YRANGE=[self.min, self.max], $
         YTICKFORMAT='(A1)', YTICKS=self.divisions, $
           YTICKLEN=0.1, YTITLE=self.title, YSTYLE=1, $
            COLOR=self.color, CHARSIZE=self.charsize

     ENDELSE

  ENDIF ELSE BEGIN

     IF self.top THEN BEGIN

        PLOT, [self.min,self.max],[0,1], /NODATA, $
         XTICKS=self.divisions, YTICKS=1, $
          XSTYLE=9, YSTYLE=1, $
           POSITION=self.position, COLOR=self.color, $
            CHARSIZE=self.charsize, /NOERASE, $
             YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', XTICKLEN=0.1, $
               XRANGE=[self.min, self.max], XTITLE=self.title

        AXIS, XTICKS=self.divisions, XSTYLE=1, $
          COLOR=self.color, CHARSIZE=self.charsize, $
           XTICKFORMAT=self.format, XTICKLEN=0.1, $
             XRANGE=[self.min, self.max], XAXIS=1

     ENDIF ELSE BEGIN     

        PLOT, [self.min,self.max],[0,1], /NODATA, $
         XTICKS=self.divisions, YTICKS=1, $
          XSTYLE=1, YSTYLE=1, $
           POSITION=self.position, COLOR=self.color, $
            CHARSIZE=self.charsize, /NOERASE, $
             YTICKFORMAT='(A1)', XTICKFORMAT=self.format, XTICKLEN=0.1, $
              XRANGE=[self.min, self.max], TITLE=self.title

      ENDELSE

  ENDELSE

     ; Restore color variable if changed for PostScript.

  IF self.postScript AND (self.pscolor NE 1) THEN self.color = self.oldcolor

END

;============================================
; Definition Routine
;============================================

PRO colorbar__define
  junk = { colorbar, $
          bottom    : 0L , $ ; Bottom color index.
          charsize  : 0.0, $ ; Size of annotation characters
          oldcolor  : 0L, $  ; Temp storage
          color     : 0l, $  ; Border Color
          divisions : 0l, $  ; Number of divisions (def=2)
          format    : '', $  ; format for numbering
          position  : fltarr(4), $ ; position of colorbar, in normalized coordinates
          min       : 0.0d, $ ; Min value
          max       : 0.0d, $ ; Max value
          ncolors   : 0.0d, $ ; Number of colors
          pscolor   : 0l, $   ; use with Postscript output
          title     : '', $   ; Title for bar
          vertical  : 0L,$    ; True i you want the bar vertical
          top       : 0l, $   ; Title on top? (if vertical=1)
          right     : 0l, $   ; Title on right? (if vertical=1)
          true      : 0L, $   ; True color visual
          postscript: 0L, $   ; True if !d.name eq 'PS'
          table     : ptr_new(),$  ; Take color from this table, instead of the one
                                   ; you'd get using tvlct call, ignored
                                   ; when true=0

          bar       : ptr_new(), $ ; The actual bar.
          xstart    : 0L, $        ; It's X location
          ystart    : 0l, $        ; It's Y location
          xsize     : 0l, $
          ysize     : 0l }
END
