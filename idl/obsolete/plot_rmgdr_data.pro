PRO plot_rmgdr_data, files,lim= lim, uu,vv,llon,llat, length= length

IF NOT( keyword_set( length ) ) THEN length =  2
IF NOT( keyword_Set( lim ) ) THEN lim =  [-80,0,80,360]
nf =  n_elements( files )
first =  1
READ_RMGDR_DATA, files, uu,vv,llon,llat
window,/free,xsiz=800n,ysiz=640
loadct,12
MAP_SET,/cont,/grid,/lab,lim= lim 
PLOTVECT,uu,vv,llon,llat,length = length

RETURN
END


