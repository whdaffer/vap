PRO test_objanl, u,v,lon,lat,ug,vg

   restore,'ascat_data.save'
   lonpar = [0.,359,1]
   latpar = [-90,90,1.]
   rainf = [12.,4,2]
   ermax = 50*[1.,1,1]
   status = objanl(u,v,lon,lat,lonpar,latpar,rainf,ermax,ug,vg)
   print,'Status from objanl : ' ,status
END 
