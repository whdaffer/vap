wf=findfile('/disk3/winds/N970628*')
print,' wf has ',n_elements(wf) , ' elements'
read_rmgdr_data, wf, u,v,lon,lat
times=fltarr(10)
print,'About to start loop'
for i=0,9 do begin 
   ui = fltarr(360,121) &  vi=ui
   lonpar =  [0,360,1.] &  latpar=[-60., 60, 1]
   rainf = [12., 10., 6,   4 ] 
   ermax = [50., 20., 10., 5.]
   t1 = systime(1) &  print,systime()
   SUCCOR, lon,lat,u,v,ui,vi,lonpar,latpar,ermax,rainf
   times(i) =  systime(1)-t1
endfor 

s = stdev(times,a)
print, 'average exe time= ',a, ' rms = ',s
END
