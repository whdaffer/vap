PRO make_rsf_frames, animpar, loncent
  maxnland = -1l
  set_plot,'z'
  device,set_resolution=[animpar[0],animpar[1]]
  nframes = animpar[2]
  numland = lonarr(nframes)

  lonrotinc = 360./nframes
  
  
  grid = replicate(255b,360,179)
  loni = findgen(360)#replicate(1.,179)
  lati = replicate(1.,360)#(findgen(179)-89.)

  xs_str = padandjustify(animpar[0],3,pad='0',/right)
  ys_str = PadAndJustify(animpar[1],3,pad='0',/right)
  nframes_str= PadAndJustify(animpar[2],3,pad='0',/right)
  loncent_str = string(loncent,form='(f5.1)')
  loncent_str = padAndJustify(loncent_str,5,pad='0',/right)

  basename = 'RSF-' + xs_str + ',' + ys_str + ',' + $
   nframes_str + ',' + loncent_str + ','

  rsf = obj_new('rsf',basename,animpar, loncent)
  IF NOT obj_valid(rsf) THEN BEGIN 
    Message,"Can't create rsf object!",/cont
    return
  ENDIF 

  FOR frm=0,nframes-1 DO BEGIN 
     print,'working on frame', frm
     MAP_SET,0,(loncent-frm*lonrotinc) MOD 360.,$
      sat_p=[20,0,0],/noborder,/satellite
     contour_im2 = map_patch(grid,$
                            lon0=min(loni),lon1=max(loni),$
                            lat0=min(lati),lat1=max(lati),$
                            xsiz=xsz,ysiz=ysz,xstart=xs,ystart=ys)
     tv,contour_im2,xs,ys
     wim =  tvrd()
     mappedPixels =  (x=where( wim ) )
     IF frm EQ 0 THEN s = rsf-> writeMappedPixels(mappedPixels, animpar, loncent)
     unpack_where, wim, x, cc, rr 
     
     oo = convert_coord( cc,rr,/dev,/to_data) 
     yy = where( oo(0,*) lt 360. and oo(1,*) lt 360.,nyy) 
     cc=cc(yy) & rr=rr(yy) & oo=oo(0:1,yy)  
     
     tlon  = reform(oo(0,*)) 
     tlat  = reform(oo(1,*))
     tmask = long(tlon*0) 
     Land_Mask,tlon,tlat,tmask 
     land=where(tmask eq 1,nland)

      t = where( tlon lt 0., nt ) 
      IF nt GT 0 THEN tlon(t) = tlon(t)+360. 
      ix = long(tlon(land)*12.)
      iy = long((tlat(land)+90)*12. )


      landi = cc[land] + animpar[0]*rr[land]
      landeli = ix + 12L*360*iy
      tlon=(tlat=(cc=(rr=(ix=(iy=(land=(tmask=0)))))))

      IF rsf-> WriteFILE( frm, landi, landeli, animpar, loncent) EQ 0 THEN BEGIN 
        Message,"Error writing data from frame " + strtrim(frm),/cont
        GOTO, bad_end
      ENDIF 
  ENDFOR 

  bad_end:
    
END
