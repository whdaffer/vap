;+
; NAME:  Frames2xmovie.pro
; $Id$
; PURPOSE:  Given some number of frames, make an 'xinteranimate' movie
;          from them.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Animation
;
; CALLING SEQUENCE:  frames2xmovie, array_of_filenames [,/gif, title=title]
; 
; INPUTS:  array_of_filename: An array of ... filenames
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  gif: flag, files are read with read_gif, if
;                      set. Otherwise, the routine tries to figure out
;                      what kind of files the input array is.
;
; OUTPUTS:  nothing, just pretty pictures on the screen
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  A window opens on the current display and a movie is displayed.
;
; RESTRICTIONS:  Must be in X windows
;
; PROCEDURE:  Read the files, feed them into CW_ANIMATE. Watch
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.3  2000/01/11 20:43:07  vapuser
; Added a call to tvlct, hoping to be able to get multiple colorbars in
; one animation. Alas, cw_animate isn't smart enough. I'll have to write
; my own.
;
; Revision 1.2  1999/10/05 16:23:59  vapuser
; Changed calls to 'read_jpeg'
;
; Revision 1.1  1999/04/07 22:34:43  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO frames2xmovie_events, event
   widget_control, event.top,/destroy
END

PRO frames2xmovie, files, gif=gif, title=title, png=png


  IF n_params() LT 1 THEN BEGIN 
    Usage, "frames2xmovie, files"
    return
  ENDIF 

  IF NOT isa(files,/string,/nonempty) THEN BEGIN 
    Message,'Files must be non-empty string',/cont
    return
  ENDIF 

  nframes = n_elements(files)
  IF nframes EQ 0 THEN BEGIN 
    Usage, "frames2xmovie, files"
    return
  ENDIF 
  

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return
  ENDIF

  Genv,/save

  set_plot,'x'

  gif =  keyword_set(gif)
  png = keyword_set(png)
  IF gif THEN type =  'GIF'
  IF png THEN type = 'PNG'
  
  IF NOT exist(type) THEN BEGIN 
    junk = rstrpos(files[0],'/')+1
    tmp = str_sep(strmid( files[0], junk, strlen(files[0])-junk ),'.')
    type =  strupcase(tmp[1])
  ENDIF 

  order = 0

  CASE type OF 
    'GIF': BEGIN 
      read_gif, files[0], image, r,g,b
      device,pseudo=8, retain=2
      device,get_visual_name=visual
      visual =  strupcase(visual)
      IF strpos(visual, 'PSEUDO') EQ -1 THEN BEGIN 
        Message,'Device must be in PSEUDOCOLOR visual for this operation',/info
        Print,'   Restart IDL, enter "IDL> device, pseudo=8" BEFORE '
        Print,'   any other calls which set the visual class '
        Print,'   (e.g. device,get_visual_name or any calls that open X windows)'
        return
      ENDIF 
      ncolors = n_elements(r)
      window,colors=ncolors,/free,/pixmap,xsize=10,ysize=10
      wdelete,!d.window
      tvlct,r,g,b
    END 
    'JPG': BEGIN 
      read_jpeg, files[0], image, colortable
    END 
    'JPEG': BEGIN 
      read_jpeg, files[0], image, colortable
    END 
    'PNG': BEGIN 
      image =  read_png( files[0], r,g,b)
      ncolors = n_elements(r)
      window,colors=ncolors,/free,/pixmap,xsize=10,ysize=10
      wdelete,!d.window
      tvlct,r,g,b      
      order = 1
    END 
    ELSE: BEGIN 
      Message,'Unknown file type ' + type ,/cont
      return
    END
  ENDCASE 

  dims = size(image,/dimension)
  nx = dims[0]
  ny = dims[1]
  IF n_elements(title) EQ 0 THEN title =  'Animation Widget'
  tlb = widget_base(Title=title)
  animate = cw_animate(tlb, nx,ny,nframes)
  Widget_Control, /Realize, tlb
  cw_animate_load, animate, frame=0, image=image, order=order
  FOR i=1,nframes-1 DO BEGIN 
    CASE type OF 
      'GIF': BEGIN 
        read_gif, files[i], image, r,g,b
        tvlct,r,g,b
      END 
      'JPG': BEGIN 
        read_jpeg, files[i], image, colortable
      END 
      'JPEG': BEGIN 
        read_jpeg, files[i], image, colortable
      END 
      'PNG': BEGIN 
        image = read_png( files[i], r,g,b)
        tvlct,r,g,b
      END 
    ENDCASE 
    cw_animate_load, animate, frame=i, image=image, order=order
  ENDFOR 
  cw_animate_run, animate

  xmanager,'Frames2xmovie',tlb,event_handler='frames2xmovie_events',/no_block

  Genv,/restore

END
