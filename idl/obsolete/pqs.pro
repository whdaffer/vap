PRO pqs, lon
  tt = qswathextent(lon)
  FOR i=0,2 DO plots,tt[i,*,0],tt[i,*,1]
END
