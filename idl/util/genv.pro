;
; $Id$
;
PRO genv, save= save, clear= clear, restore= restore

COMMON graphenv, device,pmulti,xstyle,ystyle,psym,mtitle,xtitle,ytitle,ztitle,$
                 subtitle , xmargin, ymargin, zmargin, xrange, yrange, zrange, ppos


CASE 1 OF 
  keyword_set( save )    : BEGIN
    device  = !d.name
    pmulti  = !p.multi    
    xstyle  = !x.style    
    ystyle  = !y.style    
    psym    = !psym       
    mtitle  = !mtitle 
    xtitle  = !xtitle
    ytitle  = !ytitle     
    ztitle  = !z.title    
    subtitle= !p.subtitle 
    xmargin =  !x.margin
    ymargin =  !y.margin
    zmargin =  !z.margin
    xrange =  !x.range
    yrange =  !y.range
    zrange =  !z.range
    ppos    =  !p.position
  END
  keyword_set( clear )   : BEGIN
    IF n_elements( xmargin  ) EQ 0 OR $
       n_elements( ymargin  ) EQ 0 OR $
       n_elements( zmargin  ) EQ 0 OR $
       n_elements( ppos  ) EQ 0 THEN BEGIN 
      genv,/save
    ENDIF 

    set_plot,'x'
    !p.multi   = 0
    !x.style   = 2
    !y.style   = 2
    !psym      = 0
    !mtitle    = ''
    !xtitle    = ''
    !ytitle    = ''
    !z.title  = ''
    !p.subtitle= ''
    !x.margin  = [10.,3]
    !y.margin  = [4.,2]
    !z.margin  = [0.,0.]
    !x.range =  0
    !y.range =  0
    !z.range =  0
    !p.position= 0
    genv,/save
  END
  keyword_set( restore ) : BEGIN
    IF n_elements( xmargin  ) EQ 0 OR $
       n_elements( ymargin  ) EQ 0 OR $
       n_elements( zmargin  ) EQ 0 OR $
       n_elements( ppos  ) EQ 0 THEN BEGIN 
      genv,/save
    ENDIF 

    set_plot,device
    !p.multi   = pmulti  
    !x.style   = xstyle  
    !y.style   = ystyle  
    !psym      = psym 
    !mtitle    = mtitle     
    !xtitle    =  xtitle
    !ytitle    = ytitle     
    !z.title   = ztitle    
    !p.subtitle= subtitle 
    !x.margin  =  xmargin
    !y.margin  =  ymargin
    !z.margin  =  zmargin
    !x.range   =  xrange
    !y.range   =  yrange
    !z.range   =  zrange
    !p.position=  ppos
    genv,/save
  END
  ELSE: message," one keyword from 'save', 'clear' or 'restore' is required",/cont
ENDCASE 
return
end
