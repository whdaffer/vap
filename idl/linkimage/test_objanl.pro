PRO test_objanl, u,v,lon,lat,ug,vg,long,latg

   IF n_elements(u) *n_elements(v) *$
      n_elements(lon) * n_elements(lat) EQ 0 THEN restore,'ascat_data.save'

   lonpar = [0.,359,1]
   latpar = [-90,90,1.]
   rainf = [12.,4,2]
   ermax = 50*[1.,1,1]
   t0 = systime(1)
   status = objanl(u,v,lon,lat,lonpar,latpar,rainf,ermax,ug,vg)

   long = findgen(360)
   latg = findgen(181)-90

   print,'Status from objanl : ' ,status
   print, ' objanl took ',systime(1)-t0, ' seconds '
END 
