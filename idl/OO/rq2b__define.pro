;
; NAME:   rq2b__define
; $Id$
; PURPOSE:   Defines and object of type rq2b. This object reads
;            either a regular QuikSCAT level 2B file or a 'reduced'
;            level 2B file (one which has already been processed
;            through this object) It will write out the latter.
;
;            The fundamental use for this object is to read L2B files
;            and write out only that portio of the data needed in the
;            TRMM/QSCAT colocation research.
;
; AUTHOR; William Daffer
; CATEGORY:   TRMM/Qscat research 
; CALLING SEQUENCE:   rq2b = obj_new('rq2b',file [,/always_compress])
; INPUTS:  
;
;   file: scalar string. FQFN of QuikSCAT Level 2B file you want to
;         read.
;
; KEYWORDS:  always_compress: flag. If set, always compress the file
;           you're writing. If reading an .reduced file, make sure
;           it's compressed after you've read it.
;
; OPTIONAL INPUTS:  
;
;
; Methods:
;
;    Init:
;    Cleanup: the Object destructor
;
;    Set:     file=file, /write
;             Set a new file, will cause that file to be [decompressed
;            if need be and ] read. If the /write switch is set, the
;            file will be read and then immediately written as a
;            `.reduced' file. 
;
;    Get, starttime=starttime, $  ; the earliest time in the file
;         endtime=endtime, $      ; the latest time in the file
;         rstarttime=rstarttime, $ ; the earliest time when only the
;                                   data within the TRMM zone is
;                                   considered. 
;
;         rendtime=rendtime, $     ; simile to rstarttime
;         lon = lon,$              ; the longitude of the data
;         lat=lat,$                ; the latitude fo the data
;         rowtime=rowtime,$        ; the 'rowtime'
;         center=center, $         ; center lon/lat
;         rdt=rdt,$                ; the rowtime as IDLDT
;         file=file,$              ; the filename
;         status=status
;    Read, file ; Read the file
;    Rowtime2dt ; Convert the rowtime to idldt. 
;                 <internal routine, no need for user to call>
;    Write      : Write the file: 
;                 <internal routine, no need for user to call>
;    Decompress : If reading a compress file, make system calls to
;                 decompress it.
;                 <internal routine, no need for user to call>
;    Compress   : Make system calls to compress  file, if necessary.
;                 <internal routine, no need for user to call>
;    Checkfile  : Check to see if the file is there and whether it's a
;                 compressed file or not, and do what's right
;                 <internal routine, no need for user to call>
;    IsCompressed: returns `true' if the file being read is
;                  compressed.
;                 <internal routine, no need for user to call>
;    GetNameToRead: Get the name of the file to be read, given the
;                   fact that there may be an incorrect extension on
;                   the input filename.
;                 <internal routine, no need for user to call>
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
;============================================

FUNCTION rq2b::Init, file, alwayscompress = alwayscompress
  IF n_params() LT 1 THEN BEGIN 
    Message,"Usage: rq2b=obj_new('rq2b',file)",/cont
    return,0
  ENDIF 
  IF n_elements(alwayscompress) NE 0 THEN $
    self.alwayscompress = alwayscompress
  self.status = self-> read(file)
  return,self.status
END


;============================================
; Cleanup
;============================================

PRO rq2b::Cleanup
   Ptr_Free, self.lon,self.lat,$
    self.rowtime, self.rdt
END


;============================================
; Set Routine
;============================================

PRO rq2b::Set, file = file, write=write
  self.status = self-> read(file)
  IF keyword_set(write) THEN self.status = self-> write()
END


;============================================
; Get Routine
;============================================

PRO rq2b::Get, $
        starttime=starttime, $
        endtime=endtime, $
        rstarttime=rstarttime, $
        rendtime=rendtime, $
        lon = lon,$
        lat=lat,$
        rowtime=rowtime,$
        center=center, $
        rdt=rdt,$
        file=file,$
        status=status
   IF arg_present(starttime) THEN starttime = self.starttime
   IF arg_present(endtime) THEN endtime = self.endtime

   IF arg_present(rstarttime) THEN rstarttime = self.rstarttime
   IF arg_present(rendtime) THEN rendtime = self.rendtime

   IF arg_present(lon) THEN lon = self.lon
   IF arg_present(lat) THEN lat = self.lat
   IF arg_present(center) THEN BEGIN 
     IF ptr_valid(self.lon) AND ptr_valid(self.lat) THEN BEGIN 
       dd = size(*self.lon,/dim) &  c=dd[0]/2
       center = [ [reform((*self.lon)[c,*])], $
                  [reform((*self.lat)[c,*])] ]
     ENDIF ELSE center = 0
   ENDIF 
   IF arg_present(rowtime) THEN rowtime = self.rowtime
   IF arg_present(rdt) THEN rdt = self.rdt
   IF arg_present(file) THEN file = self.file
   IF arg_present(status) THEN status = self.status
END
;============================================
; File reading Routine
;============================================

FUNCTION rq2b::read, file



  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!error_state.msg,/noname,/cont
    self.status = 0
    self.file = ''
    ptr_free, self.lon,self.lat,self.rowtime,self.rdt
    return, 0
  ENDIF 

  IF n_params() LT 1 THEN $
    Message,"Usage: status=rq2b->read(file)"

  tfile = self-> GetNameToRead(file)

  IF NOT isa(tfile,/string,/nonempty) THEN $
    Message,'<' +tfile + '> must be a STRING'

  attr = hdfgetattr(tfile,attr='SHORTNAME')
  IF ptr_valid( attr.value ) THEN BEGIN 
    IF (*attr.value)[0] EQ "QSCATL2B" THEN BEGIN 
      q2b = obj_new('q2b',file=tfile)
      IF obj_valid(q2b) THEN BEGIN 
        s = q2b->set(crd=[1,1],exclude='',decimate=0,rainflag=0)
        s = q2b-> getplotdata(u,v,lon,lat)
        s = q2b-> get(nambig=nambig, sel=sel, $
                      rowtime=rowtime, starttime=starttime, endtime=endtime)
        u = (v=0)
        obj_destroy,q2b

        self.starttime = starttime
        self.endtime = endtime

          ; Find the start/stop time of the data in the TRMM region
          ; (+/- 40 lat)
        dd = size(lat,/dim)
        c = dd[0]/2
        x=where( lon[c,*]      NE 0  AND $
                 lat[c,*]      NE 0  AND $
                 abs(lat[c,*]) LE 42 AND $
                 nambig[c,*]   GE 1  AND $
                 sel[c,*]      GE 1,nx )  


        IF nx EQ 0 THEN BEGIN 
          Message,"Bad data all-around or -- ",/info
          Message," No good data within the +/- 42 lat band! Exiting!",/cont
          return,0
        ENDIF 

        lon = lon[*,x]
        lat = lat[*,x]
        rowtime = rowtime[x]
        self->rowtime2dt,rowtime

        self.rstarttime = idldt2vaptime((*self.rdt)[0])
        self.rendtime = idldt2vaptime((*self.rdt)[nx-1])

        
        self.lon = ptr_new(lon,/no_copy)
        self.lat = ptr_new(lat,/no_copy)
        self.rowtime = ptr_new(rowtime,/no_copy)
        
        self.file = file
        self.status = 1
      ENDIF 
    ENDIF ELSE BEGIN 

      sd_id = hdf_sd_start(tfile,/read)

      r = hdf_sd_attrfind(sd_id,'StartTime')
      hdf_sd_attrinfo,sd_id,r,data=data
      self.starttime = data


      r = hdf_sd_attrfind(sd_id,'EndTime')
      hdf_sd_attrinfo,sd_id,r,data=data
      self.endtime = data


      r = hdf_sd_attrfind(sd_id,'Restricted_StartTime')
      hdf_sd_attrinfo,sd_id,r,data=data
      self.rstarttime = data


      r = hdf_sd_attrfind(sd_id,'Restricted_EndTime')
      hdf_sd_attrinfo,sd_id,r,data=data
      self.rendtime = data

      r = hdf_sd_nametoindex(sd_id,'lon')
      sdsid = hdf_sd_select(sd_id,r)
      hdf_sd_getdata,sdsid, lon
      hdf_sd_endaccess, sdsid
      self.lon =  ptr_new(lon,/no_copy)

      r = hdf_sd_nametoindex(sd_id,'lat')
      sdsid = hdf_sd_select(sd_id,r)
      hdf_sd_getdata,sdsid, lat
      hdf_sd_endaccess, sdsid
      self.lat =  ptr_new(lat,/no_copy)

      r = hdf_sd_nametoindex(sd_id,'rowtime')
      sdsid = hdf_sd_select(sd_id,r)
      hdf_sd_getdata,sdsid, rowtime
      hdf_sd_endaccess, sdsid
      self->rowtime2dt,rowtime
      self.rowtime =  ptr_new(rowtime,/no_copy)
      
      hdf_sd_end, sd_id
      self.file = tfile
      self.status = 1
    ENDELSE 
    ptr_free,attr.value
  ENDIF ELSE BEGIN 
    Message,"Can't get SHORTNAME attribute!\n",/cont
    self.status = 0
  ENDELSE 

  self-> compress, tfile
  return,self.status

END

;============================================
; Pro rowtime2dt
;============================================
PRO rq2b::rowtime2dt, rowtime
;           1         2
; 012345678901234567890
; 1999-244T00:08:52.678
   year = fix(strmid(rowtime,0,4))
   doy  = fix(strmid(rowtime,5,3))
   nn = n_elements(doy)
   month = (day=intarr(nn))
   FOR i=0,nn-1 DO BEGIN 
     dd = fix(doy2date(year[i],doy[i]))
     month[i] = dd[0]
     day[i] = dd[1]
   ENDFOR 
   hh   = fix(strmid(rowtime,9,2))
   mm   = fix(strmid(rowtime,12,2))
   ss   = float(strmid(rowtime,15,6))
   dt = var_to_dt( year, month, day, hh, mm, ss)
   self.rdt = ptr_new(dt,/no_copy)
END

;===========================================
; write
;===========================================
FUNCTION rq2b::write, file, compress = compress

  catch,error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!error_state.msg,/cont
    return,0
    IF exist(sd_id) THEN hdf_sd_end, sd_id
  ENDIF 

  IF n_params() LT 1 THEN BEGIN 
    IF stregex(self.file,'(.*)\.(Z|gz)',/bool) THEN BEGIN 
      ext = (stregex(self.file,'.*(Z|gz)$',/subexp))[1]
      ext = strmid(self.file,ext-1,strlen(self.file)-(ext-1))
      basename = basename(self.file,ext=ext)
      file = basename + '.reduced.hdf'
    ENDIF ELSE file = self.file + '.reduced.hdf'
  ENDIF 

  Message,"Attempting to write " + file,/info

  sd_id = hdf_sd_start( file,/create )

  hdf_sd_attrset,sd_id,'ShortName','QSCATL2B_REDUCED_1.0',/string
  hdf_sd_attrset, sd_id, 'StartTime', self.starttime,/string
  hdf_sd_attrset, sd_id, 'EndTime', self.endtime,/string
  hdf_sd_attrset, sd_id, 'Restricted_StartTime', self.rstarttime,/string
  hdf_sd_attrset, sd_id, 'Restricted_EndTime', self.rendtime,/string

  dims = size( *self.lon,/dim)
  sdsid = hdf_sd_create(sd_id, 'lon', dims, /float)
  hdf_sd_adddata,sdsid,*self.lon
  hdf_sd_endaccess,sdsid

  sdsid = hdf_sd_create(sd_id, 'lat', dims, /float)
  hdf_sd_adddata,sdsid,*self.lat
  hdf_sd_endaccess,sdsid

  sdsid = hdf_sd_create(sd_id, 'rowtime', dims, /string)
  hdf_sd_adddata,sdsid,*self.rowtime
  hdf_sd_endaccess,sdsid

  hdf_sd_end,sd_id  

  IF keyword_set(compress) THEN BEGIN 
    Message,"Compressing " + file,/info
    spawn,"gzip " + file
    IF NOT fexist(file + '.gz') THEN BEGIN 
      Message,"Error compressing " + file,/cont
      return,2
    ENDIF 
  ENDIF 
  return,1
END

;============================================
;  Decompress file
;============================================


FUNCTION rq2b::decompress, file
   lf =  string(10b)
   tfile = self-> CheckFile(file)
   IF NOT self-> IsCompressed(tfile) THEN BEGIN 
     Message,"File " + lf + file + lf + "  is already decompressed!",/info
     return, tfile
   ENDIF ELSE BEGIN 
     IF fexist(file) THEN BEGIN 
       ext = (stregex(file,'.*(Z|gz)$',/subexp))[1]
       ext = strmid(file,ext-1,strlen(file)-(ext-1))
       path = path(file)
       basename = basename(file,ext=ext)
       spawn,'gunzip ' + file,ret
       IF NOT fexist(path + basename) THEN BEGIN 
         Message,"Couldn't gunzip " + lf + file + "!",/cont
         self.status = 0
         return,0
       ENDIF 
     ENDIF ELSE return, path + basename
   ENDELSE 
END

;============================================
;  Compress file
;============================================

PRO rq2b::compress, file
  IF self.wascompressed OR self.alwayscompress THEN BEGIN 
    spawn,' gzip ' + file,ret
    IF NOT fexist( file + '.gz' ) THEN BEGIN 
      Message,"Error compressing " + file,/cont
      self.status = 0
    ENDIF 
  ENDIF 
END

;============================================
; CheckFile
;  Returns the name of the file that should be read after checking for
;  [de]compressed versions of the same filename. 
;  
;  If the input name is of a compressed file and it exists, return
;  that name. Otherwise, look for the the decompressed version and, if
;  it exists, return that. If it's a decompressed file but doesn't
;  exist, check for the compressed file and return that if it exists.
;  In short, find the file to read disregarding 'compressedness.'
;
;============================================
FUNCTION rq2b::CheckFile, file
   IF fexist(file) THEN return, file
   IF self->IsCompressed(file) THEN BEGIN 
       ; They gave us the name for a compressed file and it isn't
       ; there so check for the uncompressed version.
     ext = (stregex(file,'.*(Z|gz)$',/subexp))[1]
     ext = strmid(file,ext-1,strlen(file)-(ext-1))
     path = path(file)
     basename = basename(file,ext=ext)
     uncompressedfile = path+basename
     IF fexist(uncompressedfile) THEN $
       return,uncompressedfile ELSE  $
       return,''
   ENDIF ELSE BEGIN 
     test = findfile(file + '*',count=nf)
     IF nf NE 0 THEN BEGIN 
       IF self-> IsCompressed(test[0]) THEN return,test[0] ELSE return,''
     ENDIF 
   ENDELSE 
   return,'' ; Job security!
end

;============================================
; IsCompressed
;   Return 1 if filename ends in '.Z' or '.gz' 
;   This says nothing about whether this file exists 
;   on the disk.
;============================================

FUNCTION rq2b::IsCompressed, file
  compressed =  stregex(file,'.*(Z|gz)$',/boolean) 
  return, compressed
END


;============================================
; GetNameToRead
;   Check for compressedness, decompress if necessary and return the
;   name of the decompressed file to be read. 
;============================================
FUNCTION rq2b::GetNameToRead, file
  name = file
  IF self-> IsCompressed(file) THEN BEGIN 
    name = self->CheckFile(name)
    self.wascompressed = self-> IsCompressed(name)
    IF self.wascompressed THEN name = self-> deCompress(name)
  ENDIF 
  return, name
END


;============================================
; Definition Routine
;============================================

PRO rq2b__define
  junk = {RQ2B, $
          wascompressed: 0l, $
          alwayscompress: 0, $
          file: '', $
          status: 0, $
          starttime: '', $
          endtime: '' ,$
          rstarttime: '', $
          rendtime: '', $
          lon: ptr_new(), $
          lat: ptr_new(), $
          rowtime: ptr_new(), $
          rdt: ptr_new() $
         }
END
