FUNCTION  goes2readdoc, datetime, region
  IF n_params() LT 1 THEN BEGIN 
    Usage,'goesdoc_struct=GoesReadDoc(datetime)'
    return,''
  ENDIF 
 
  IF n_elements(region) EQ 0 THEN region =  'west'
  IF NOT isa(region,/string,/nonempty) THEN BEGIN 
    Message,"Parameter REGION must be non-empty string (east|west)",/cont
    return,''
  ENDIF 

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    IF exist(lun) THEN free_lun, lun
    return,''
  ENDIF 

  goestopdir = getenv('GOES2TOPDIR')
  IF strlen(goestopdir) EQ 0 THEN $
    Message,"Env Var GOES2TOPDIR must be defined"

  f = goestopdir + '/' + region + '/doc/' + datetime + '.txt'
  file = findfile(f,count=nf)

  IF nf EQ 0 THEN $
    Message,"No such file <" + f + ">"

  file = file[0]
  openr, lun, file, /get, error=err
  IF err NE 0 THEN $
    Message, !error_state.msg

  goesdoc = {GOES2DOC}
  tags = tag_names(goesdoc)
  done = 0
  rec = ''
  readf, lun, rec
  readf, lun, rec
  t = strsplit(rec,'=',/extract)
  goesdoc.spacecraftid =  fix(t[1])
    
  readf, lun, rec
  t = strsplit(rec,'=',/extract)
  goesdoc.SensorProcId =  fix(t[1])
  
 
  FOR i=0,3 DO BEGIN 
    readf, lun, rec
    t = strsplit(rec,'=',/extract)
    goesdoc.iscan[i] =  byte(fix(strtrim(t[1],2)))
  ENDFOR    

  FOR i=0,5 DO BEGIN 
    readf,lun,rec
    t = strsplit(rec,'=',/extract)
    goesdoc.spsTime[i] = fix(t[1])
  ENDFOR 

  FOR i=0,5 DO BEGIN 
    readf,lun,rec
    t = strsplit(rec,'=',/extract)
    goesdoc.headerTime[i] = fix(t[1])
  ENDFOR 

  FOR i=0,5 DO BEGIN 
    readf,lun,rec
    t = strsplit(rec,'=',/extract)
    goesdoc.trailerTime[i] = fix(t[1])
  ENDFOR 

  error = 0
  REPEAT BEGIN 
    readf, lun, rec
    rec = strcompress(strtrim(rec,2))
    t = strsplit(rec,'=',/extract)
    t1 = strsplit(strtrim(t[0],2,/extract),' ')
    
    field = strupcase(t1[n_elements(t1)-1])
    x = where(tags EQ field, nx )
    IF nx NE 0 THEN BEGIN 
      CASE VARTYPE(goesdoc.(x[0])) OF 
        'DOUBLE'   : goesdoc.(x[0]) = double(t[1])
        'LONGWORD' : goesdoc.(x[0]) = long(t[1])
        'INTEGER'  : goesdoc.(x[0]) = fix(t[1])
        'STRING'   : goesdoc.(x[0]) = t[1]
        ELSE       : Message,"Unknown fieldtype!"
      ENDCASE
    ENDIF ELSE $
      Message,"Can't find field <" + field + ">"
  ENDREP UNTIL field EQ 'A11' OR eof(lun)

  IF eof(lun) THEN $
    Message,"Short file!"


  FOR i=0,5 DO BEGIN 
    readf,lun,rec
    t = strsplit(rec,'=',/extract)
    goesdoc.epochTime[i] = fix(t[1])
  ENDFOR 

  REPEAT BEGIN 
    readf, lun, rec
    rec = strcompress(strtrim(rec,2))
    t = strsplit(rec,'=',/extract)
    t1 = strsplit(strtrim(t[0],2,/extract),' ')
    
    field = t1[n_elements(t1)-1]
    index = fix(strmid(field,1,strlen(field)-1))
    goesdoc.a[index] =  double(t[1])

  ENDREP UNTIL eof(lun)


  free_lun, lun
  return, goesdoc
END

