;+
; NAME:  OBPLot__define
; $Id$
; PURPOSE:  Create and object of type OBPLOT.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  PV 
;
; CALLING SEQUENCE:  Like all objects!
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
; Revision 1.1  1999/10/06 23:11:38  vapuser
; Initial revision
;
;
;Copyright (c) 1999, William Daffer
;No Warranties
;-

FUNCTION ObPlot::INIT, Yarray, Xarray = Xarray, $
                 AnnotArray=AnnotArray, $
                 LineStyle=linestyle, PSym=psym, $
                 Symsize= symsize, $
                 thick=thick, charsize=charsize, charthick=charthick, $
                 Color=color, Position=position, plots=plots, $
                 doAnnots=doAnnots

  Catch, error
  IF error NE 0 THEN BEGIN
     ok = Widget_Message(!Err_String)
     Print, 'Error in ObPlot::INIT--' + !Err_String
     RETURN, 0
  ENDIF

  IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
  IF N_Elements(PSym) EQ 0 THEN psym = 0
  IF N_Elements(SymSize) EQ 0 THEN symsize = 1.0
  IF N_Elements(color) EQ 0 THEN color = !D.N_Colors-1
  IF N_Elements(position) EQ 0 THEN position = [0.15, 0.15, 0.95, 0.95]
  IF n_elements(thick) EQ 0 THEN thick = 1
  IF n_elements(charthick) EQ 0 THEN charthick = 1
  IF n_elements(charsize) EQ 0 THEN charsize = 1.0
  IF n_elements(AnnotArray) NE 0 THEN self.AnnotArray =  ptr_new(AnnotArray)
  IF N_Elements(Yarray) NE 0 THEN self.Yarray = Ptr_New(Yarray)
  IF n_elements(Xarray) NE 0 THEN BEGIN 
    self.Xarray = Ptr_New(Xarray)
  ENDIF ELSE BEGIN 
    IF ptr_valid( self.Yarray) THEN $
     self.Xarray =  ptr_New(findgen(n_elements(*self.Yarray)))
  ENDELSE 

  self.linestyle = linestyle
  self.psym = psym
  self.symsize = symsize
  self.position = position
  self.color = color
  self.thick = thick
  self.charthick = charthick
  self.charsize = charsize
  
  self.xrange = [!values.f_nan,!values.f_nan]
  self.yrange = self.xrange
  self.plots = keyword_set(plots)
  self.doAnnots = keyword_set(doAnnots)

  RETURN, 1
END

;-----------------------------------------------------------------------

PRO ObPlot::CleanUp
  Ptr_Free, self.Yarray
  Ptr_Free, self.Xarray
  ptr_free, self.AnnotArray
END



;==========================================================

PRO obplot::Clear
  Ptr_Free, self.Yarray
  Ptr_Free, self.Xarray
  ptr_free, self.AnnotArray
END

;==========================================================

PRO ObPlot::Set, $
            PSym      = psym, $
            symsize= Symsize, $
            Color     = color, $
            Position  = position, $
            LineStyle = linestyle, $
            thick     = thick,$
            charthick = charthick, $
            charsize  = charsize, $
            Xrange    = Xrange,$
            Yrange    = Yrange,$
            Xarray    = Xarray,$
            Yarray    = Yarray, $
            AnnotARRAY= AnnotArray, $
            Draw      = Draw, $
            plots     =plots, $
            doAnnots   =doAnnots 

  IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
  IF N_Elements(psym)      NE 0 THEN self.psym = psym  
  IF N_Elements(symsize)      NE 0 THEN self.symsize = symsize  
  IF N_Elements(color)     NE 0 THEN self.color = color
  IF N_Elements(position)  NE 0 THEN self.position = position
  IF N_Elements(xrange)    NE 0 THEN self.xrange = xrange
  IF n_elements(thick)     NE 0 THEN self.thick = thick
  IF n_elements(charthick) NE 0 THEN self.charthick = charthick
  IF n_elements(charsize)  NE 0 THEN self.charsize = charsize 
  IF N_Elements(xarray)    NE 0 THEN BEGIN 
    IF ptr_valid( self.xarray ) THEN $
       *self.xarray = xarray ELSE  $
        self.xarray = ptr_new(xarray)
  ENDIF 

  IF N_Elements(yarray)    NE 0 THEN BEGIN 
    IF ptr_valid( self.yarray ) THEN $
       *self.yarray = yarray ELSE  $
        self.yarray = ptr_new(yarray)
  ENDIF 

  IF N_Elements(AnnotArray)    NE 0 THEN BEGIN 
    IF ptr_valid( self.AnnotArray ) THEN $
       *self.AnnotArray = AnnotArray ELSE  $
        self.AnnotArray = ptr_new(AnnotArray)
  ENDIF 


  IF N_Elements(yrange)    NE 0 THEN self.yrange = yrange
  IF keyword_set( Draw )        THEN self->Draw
  IF n_elements( plots )   NE 0 THEN self.plots = keyword_set(plots)
  IF n_elements(doAnnots)   NE 0 THEN self.doAnnots = keyword_set( doAnnots )
END
;-----------------------------------------------------------------------



PRO ObPlot::Get, $
            PSym      = psym, $
            symsize= symsize, $
            Color     = color, $
            Position  = position, $
            LineStyle = linestyle, $
            thick     = thick, $
            charthick = charthick, $
            charsize  = charsize, $
            plots     = plots, $
            doAnnots  = doAnnots

  psym = self.psym
  symsize = self.symsize
  color = self.color
  position = self.position
  linestyle = self.linestyle
  plots = self.plots
  doAnnots = self.doAnnots
  thick = self.thick
  charthick = self.charthick
  charsize = self.charsize
END
;-----------------------------------------------------------------------



PRO ObPlot::SetYarray, Yarray, Xarray = Xarray, Draw = Draw
  IF ptr_valid( self.Yarray ) THEN $
    *self.Yarray = Yarray ELSE $
    self.Yarray= ptr_new(yarray)
  IF N_Elements(Xarray) EQ 0 THEN Xarray = findgen(n_Elements(Yarray))
  IF ptr_valid(self.xarray) THEN $
    self.xarray = xarray ELSE $
    self.xarray = ptr_new(xarray)
  IF Keyword_set( Draw ) THEN self->draw
END


;-----------------------------------------------------------------------



FUNCTION ObPlot::GetYarray
  RETURN, self.Yarray
END


FUNCTION ObPlot::GetXarray
  RETURN, self.Xarray
END

FUNCTION ObPlot::GetAnnotArray
  RETURN, self.AnnotArray
END


;-----------------------------------------------------------------------



PRO ObPlot::Draw, NoErase=noerase, $
            Xrange=xrange, Yrange=yrange, $
              _Extra=extra, plots=plots, doAnnots= doAnnots

  IF NOT (ptr_valid(self.yarray) AND ptr_valid(self.xarray) ) THEN return

  Catch, error
  IF error NE 0 THEN BEGIN
     ok = Widget_Message(!Err_String)
     Message,!Err_String,/cont
     RETURN
  ENDIF

  IF N_Elements(Xrange) EQ 2 THEN self.xrange = xrange
  IF N_Elements(Yrange) EQ 2 THEN self.yrange = yrange

  x =  where( finite( self.xrange ), nx )
  y =  where( finite( self.yrange ), ny )


  CASE 1 OF 
    nx EQ 2 AND ny EQ 2: BEGIN 
      x = where( *self.Xarray GE self.xrange[0] AND $
                 *self.Xarray LE self.xrange[1], nx )
      y = where( *self.Yarray GE self.yrange[0] AND $
                 *self.Yarray LE self.yrange[1], ny )
    END 
    nx EQ 2: BEGIN 
      x = where( *self.Xarray GE self.xrange[0] AND $
                 *self.Xarray LE self.xrange[1], nx )
      IF nx NE 0 THEN BEGIN 
        ny = nx
        y =  x
      ENDIF 
    END
    ny EQ 2: BEGIN 
      y = where( *self.Yarray GE self.yrange[0] AND $
                 *self.Yarray LE self.yrange[1], ny )
      IF ny NE 0 THEN BEGIN 
        nx = ny
        x =  y
      ENDIF 
    end
    ELSE: BEGIN 
      ; No limits. 
      nx = N_elements(*self.Yarray)
      X = findgen(nx)
      y = x
      ny = nx
    END
  ENDCASE 


  IF nx*ny NE 0 THEN BEGIN 
    IF self.plots THEN BEGIN 
      Plots, (*self.Xarray)[x], (*self.Yarray)[y], $
       LineStyle=self.lineStyle, $
       Color=self.color, $
       PSym=self.psym, $
       symsize=self.symsize, $
       thick = self.thick, $
       _Extra=extra
    ENDIF ELSE BEGIN 
      Plot, (*self.Xarray)[x], (*self.Yarray)[y], $
       LineStyle=self.lineStyle, $
       Color=self.color, $
       PSym=self.psym, $
       symsize=self.symsize, $
       Position=self.position, $
       charsize=self.charsize, $
       thick = self.thick, $
       charthick=self.charthick, $
       NoErase=Keyword_Set(noerase), $
       _Extra=extra
    ENDELSE
    IF ptr_valid(self.AnnotArray) THEN BEGIN 
      Annots = (*self.AnnotArray)[0:n_elements(*self.Xarray)-1]
      reallydoAnnots = 0
      IF n_elements(doAnnots) NE 0 THEN BEGIN 
        IF keyword_set(doAnnots) THEN reallydoAnnots = 1
      ENDIF ELSE $
        IF self.doAnnots THEN reallydoAnnots = 1
      IF reallydoAnnots THEN BEGIN 
        FOR i=0,n_elements(annots)-1 DO BEGIN 
          IF strlen(Annots[i]) NE 0 THEN BEGIN 
            xyouts, (*self.Xarray)[i], (*self.Yarray)[i], $
              Annots[i],color=self.color, $
                charsize=self.charsize, charthick=self.charthick
          ENDIF 
        ENDFOR 
      ENDIF 
    ENDIF 
  ENDIF 
END


;-----------------------------------------------------------------------


PRO ObPlot__Define

  struct = { ObPlot, $                ; The name of the object.
             Yarray    : Ptr_New(), $ ; The data to be plotted.
             Xarray    : Ptr_New(), $ ; The Xarray for the data.
             AnnotArray: Ptr_New(),$  ; Possible Annotations.
             doAnnots   : 0, $
             lineStyle : 0, $         ; The data line style.
             thick     : 0, $          ; The 'thickness' of the line.
             psym      : 0, $         ; The data plot symbol.
             SymSize   : 0.0, $       ;
             charsize  : 0.0, $
             charthick : 0, $
             color     : 0L, $        ; The color of the data.
             position  : FltArr(4),$  ; The position in the plot window.
             Xrange    : fltarr(2) ,$ ; xrange
             YRange    : fltarr(2) ,$ ; yrange
             plots     : 0L }         ; flag, use plots if ==1
END

