PRO create_guess_field, u,v,lon,lat,ug,vg

   ug =  replicate(mean(u),360,181)
   vg = replicate(mean(v),360,181)
   
   
   FOR i=0,359 DO BEGIN 
     FOR j=-90,90 DO BEGIN
       jj = j+90
       xx = where( lon GE i-3 AND $
                   lon LE i+3 AND $
                   lat GE j-3 AND $
                   lat LE j+3,nxx)
       IF nxx EQ 0 THEN CONTINUE
       ug[i,jj] =  mean(u[xx])
       vg[i,jj] =  mean(v[xx])
     ENDFOR
   ENDFOR 
end
