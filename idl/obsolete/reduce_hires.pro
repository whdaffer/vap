PRO reduce_hires, file
;on_error,1
IF n_params() EQ 0 THEN message,'Need FILE parameter '

openr,rlun,file,/get,error=err
IF err NE 0 THEN message,!err_string
tmp =   strsplit(  file,'.',/extract) 
ofile =  tmp(0) + 'R' + tmp(1)
openw,wlun, ofile,/get,error=err
IF err NE 0 THEN message, !err_string

m =  mgdr25_str()
r = rmgdr25_str()
WHILE NOT eof(rlun) DO BEGIN
  readu,rlun,m
  r.Time =  m.time
  r.Rev = m.Rev            
  r.Row = m.Row          
  r.Lat = m.Lat          
  r.Lon = m.Lon          
  r.Col = m.Col          
  r.wvcqualflag = m.wvcqualflag  
  r.mean_wind = m.mean_wind    
  r.nambig = m.nambig       
  r.wvc_sel = m.wvc_sel      
  r.windspd = m.windspd      
  r.errspd = m.errspd       
  r.winddir = m.winddir      
  r.errdir = m.errdir       
  r.mle_like = m.mle_like     
  r.l_wind_flg = m.l_wind_flg   
  r.h_wind_flg = m.h_wind_flg   

  writeu,wlun, r

ENDWHILE 

free_lun,rlun,wlun

END 



