;+
; NAME:   geoanim__define
; PURPOSE:   Defines and object of type animate. This object is used
;          to animate geographically located data, i.e. data in rectangular
;          arrays which have associated with each element a
;          longitude/latitude duple. 
;
; AUTHOR; William Daffer
;
; CATEGORY:   OO
;
; CALLING SEQUENCE:   
;
;     geoanim = obj_new('geoanim',nframe,files,object,...)
;
;                   See the INIT method for a thorough discussion of
;                   the parameters/keywords to be passed in during
;                   object initialization.
; 
; METHODS:
;       Public Interface: All that is needed to use this object is
;                         contained in the INIT, GETFRAME,
;                         GETNEXTFRAME routines. The programmer may
;                         want to look as 'get' and 'set' methods, but
;                         shouldn't really have to use them. IDL
;                         adopts a Perl-like convention, we think you
;                         should stay out of the code because you
;                         haven't been invited, not because we've
;                         locked the doors and windows.
;
;           Function: INIT, nframes, files, object $
;                        [,starttime=starttime,
;                         endtime=endtime, filetimes=filetimes,
;                         lonpar=lonpar, latpar=latpar, badval=badval]
;
;                nframes: (I), scalar integer, the number of frames in
;                         the animation. (REQUIRED)
;                files:   (I), vector of strings. Fully qualified
;                       names of the files to be read (REQUIRED)
;                object:  (I), scalar string. The name of the object
;                        to be used in reading the files in paramter
;                        FILES. (REQUIRED)
;                starttime: (I) The starttime of the animation. Must lie
;                           between the time of the first and last
;                           files. May be input as vaptime (a string
;                           having the form 'yyyy/mm/dd/hh/mm' or as
;                           an IDLDT variable (See 'Date and Time
;                           routines' in the IDL online help for the
;                           definitions of this type of variable) 
;
;                           Default: filetimes[0], if that variable is
;                                    input, or the time read from the
;                                    first file. The routine fails if
;                                    none of these conditions are met.
;
;                endtime:  (I) the end time of the animation. See
;                         STARTTIME.
;                lonpar:   (I) A float 2-vector. [minlon, maxlon]
;                          Default: Assume the object in OBJECT knows
;                                   the shape of the data and will
;                                   tell us what it is. Assume that
;                                   we'll be returning the whole of
;                                   whatever is read by OBJECT.
;                latpar:   (I) a float 2-vector. [minlat, maxlat]
;                           Default: Same as LONPAR
;
;           Function: GETFRAME, frame 
;              frame: (I) scalar integer. The frame to be
;                     retrieved. Must be between 0 and nframes-1.
;                     Returns: 
;                        Failure: 0
;                        Success: the data. If the data is scalar, this is
;                              just a 2-d array. If the data is
;                              vector, the data is a 3-d array, with
;                              the last dimension (i.e. the right-most
;                              index) being over the vector. So, if
;                              the array returns as x=fltarr(100,2002)
;                              the U field is x[*,*,0] and the V field
;                              is x[*,*,1].
;
;                              NB. This routine puts no
;                              interpretations  on the data. It's up
;                              the the caller to do that. 
;
;                              'U' and 'V' used above for illustrative
;                              purposes only. 
;
;
;           Function: GETNEXTFRAME 
;              No arguments. Just retreive the next frame. Same
;              returns as GetFrame.
;
;  ======== Expert Users and Programmers Only. Go read the code. ==========
;
;
;           Function: CALCFRAMETIME, frame
;              Frame: (I), scalar integer between 0 and nframes-1. 
;              Returns: Failure: 0, Success, The time of 'frame' as an
;                       IDLDT structure. (See 'Date and Time routines'
;                       in the online IDL help.)
;
;
;
;           Pro:      CLEANUP 
;             No arguments. Cleans up all the heap memory.
;
;           Pro:      SET,data0 = data0, data1=data1, 
;                         delta_d=delta_d,curdata=curdata, $
;                         file_ptr=file_ptr, starttime=starttime, $
;                         endtime=endtime, frametime=frametime, $
;                         nframes=nframes, delta_t=delta_t, dt=dt, $
;                         units=units, files=files, filetimes=filetimes
;
;           Pro:      GET, data0 = data0, data1=data1, $
;                       delta_d=delta_d, curdata=curdata, $
;                       file_ptr=file_ptr, starttime=starttime, 
;                       endtime=endtime, frametime=frametime, $
;                       nframes=nframes,  delta_t=delta_t, dt=dt,$
;                       files=files, filetimes=filetimes, $
;                       framenum=framenum
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
;    First and foremost, you need at least IDL version 5.1! No check 
;    is made of this in this object, since without IDL version 5.1 you
;    won't even get into this routine.
;
;    This object is really intended for use with Geo-located
;    data. I've tried to make the dependence on longitude and latitude
;    as minimal as possible. It only intrudes via the lonpar/latpar
;    keywords. These are passed to OBJECT (which you pass in the
;    'object' parameter) and used there, so if you're sharp enough to
;    use non-geolocated data by creating a smart object, go for it. In
;    fact, this routine doesn't use the lonpar/latpar keywords, all it
;    does is pass them to whatever object is used to read and
;    understand the data. There is no provision made for handling
;    regions that pass over 0 or 180 degress longitude. This has to be
;    handled in the object OBJECT!
;
;    A word about the OBJECT parameter. This parameter refers to an
;    object, e.g. it wil appear as x=obj_new(OBJECT,...), so it must
;    completely understand the data it is being asked to read and pass
;    back to this (geoanim) object! 
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
; $Log$
;
;William Daffer
;Copyright (c) 2000
;
;-
; No Warranties!
;


;============================================
; Init
; 
;     INPUTS:  
;
;        These three parameters (nframes,files,object) are REQUIRED!
;
;        nframes: The number of frames in the movie.
;      
;        files: And array containing the fully qualified file to 
;               be read in making this animation. The files must be in
;               order.
; 
;
;        object: The name of the object to be used in reading this
;                data. This object must have the following
;                properties and methods (with the indicated argumentation)
;
;                Properties: Should understand the data it is being
;                            asked to encapsulate. Should know how to
;                            read the data. Should know how to select
;                            from the data the region of interest, so
;                            that it isn't required to carry around
;                            alot of useless memory.
;
;                REQUIRED METHODS:
;
;                  Procedures: 
; 
;                     Get: data=data, time=time 
;
;                       DATA returns the data. If Vector, the format is
;                       [nx,ny,2] where data[*,*,0] is the 'U' and
;                       data[*,*,1] is the 'V'.
;                       TIME returns and IDLDT variable representing the
;                       synoptic time of the data in this object.
;
;                     Set: data=data, time=time, lonpar=lonpar, latpar=latpar 
;
;                       The analog to 'Get'
;                       Data/Time: As in 'Get'
;
;                       Lonpar: [lon0,lon1] sets the longitude extent of
;                               the area of interest in this animation.
;                               Selects and saves only the region of
;                               the data within these longitude
;                               limits. 
;            
;                       Latpar: [lat0,lat1] sets the latitude extent of
;                               the area of interest in this animation.
;                               Selects and saves only the region of
;                               the data within these latitude
;                               limits. 
;            
;
;                Functions:
; 
;                   init([file][,lonpar=lonpar,latpar=latpar])
;                       : this is the standard IDL object init
;                         (constructor) method. All of the
;                         parameters/keywords should be optional so
;                         that some methods may be used without the
;                         object variables being initialized. As
;                         always, it returns 1 for success and 0 for
;                         failure.
;
;
;                   time(): returns the synoptic time of the data in
;                           the object or 0, if there is none or it
;                           doesn't know the time.
;
;                   type(): returns one of the strings 'VECTOR' or
;                           'SCALAR' or 'UNKNOWN' if the object hasn't
;                           yet been populated.
;
;                   read(file): returns 1 for success, 0 for
;                               failure. Sets internal data structures
;                               'data', 'time' and 'type'. Applies
;                               whatever restriction is imposed by
;                               lonpar/latpar, if they've been
;                               populated.
;
;                                  
;                   gettimefromfile(file): returns the time as an
;                                          IDLDT if it is present in
;                                          the data, otherwise returns
;                                          0
;                      
;
;     OPTIONAL INPUTS:  
;
;       KEYWORD PARAMETERS:  
;
;        StartTime: The time of the first frame of the movie.
;                   Input as VapTime (yyyy/mm/dd/hh/mm) or IDLDT
;                   If parameter is not present then:
;                     If filetimes is then 
;                        starttime=filetimes[0]
;                     else 
;                       read files for time. Construct 'filetimes' 
;                       if it has time or a time can be construed from
;                       it then starttime=filetimes[0], else fail
;
;      
;        EndTime: The time of the lastframe of the movie.
;                   Input as VapTime (yyyy/mm/dd/hh/mm) or IDLDT
;      
;                   If parameter is not present then:
;                     If filetimes is then 
;                        endtime=filetimes[n-1] ;n=n_elements(files)
;                     else 
;                       read files for time.
;                       if it has time or a time can be construed from
;                       it then endtime=filetimes[n-1], else fail
;
;         Filetimes: The times of the files given in the 'files'
;                    parameter. 
;
;                   If this parameter is not present OBJECT must be
;                   able to calculate or otherwise discern  -- e.g. it
;                   could be in the filename itself -- the time of the
;                   file. In short, either you must pass the time for
;                   each file in, via the filetimes keyword, or OBJECT
;                   must be able to determine it.
;
;        lonpar: The start/stop of the longitude range to be
;                animated. (Default, return the whole array)
;
;        latpar: The start/stop of the latitude range to be
;                animated. (Default, return the whole array)
;
;        badval: The value that signifies bad or missing data in the
;                data to be read by 'object.'  Data returned by
;                various routines of this object will have each
;                location set to this value if either of the bounding
;                fields have this entry set. It's up to the code which
;                uses this object to make use of this information. 
;   
;     OUTPUTS:  
;
;       An object of type 'geoanim.'

;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  

;============================================

FUNCTION geoanim::Init, nframes, files, object, $
                        starttime = starttime, $
                        endtime=endtime, $
                        filetimes=filetimes, $
                        lonpar=lonpar, $
                        latpar=latpar, $
                        badval=badval
  status = 0
  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!error_state.msg,/cont
    return,0
  ENDIF 
  IF n_params() LT 3 THEN BEGIN 
    str = "Usage: animobj=obj_new('animate',nframes,files, object " + $
      "[,starttime=starttime,endtime=endtime," + $
      "filetimes=filetimes,lonpar=lonpar,latpar=latpar)"
    Message,str
  ENDIF 
  self.nframes = nframes
  self.object = object

  IF NOT isa(files,/string,/nonempty) THEN $
    Message,"Files must be string array!"
  
  nf = n_elements(files)
  self.files = ptr_new(files)
  nft = n_elements(filetimes) 
  IF nft NE 0 THEN BEGIN 
    IF nf NE nft THEN Message,$
       "Files and FileTimes have different number of elements!"
    IF isa(filetimes[0],/idldt) THEN BEGIN 
      self.filetimes =  ptr_new(filetimes,/no_copy) 
    ENDIF ELSE BEGIN 
      ft = replicate(idldt,nf)
      FOR f=0,nf-1 DO BEGIN 
        tt = vaptime2idldt(filetimes[f])
        IF NOT isa(tt,/idldt) THEN Message,"<filetimes>: Error Converting to VAPTIME"
        ft[f] = tt
      ENDFOR 
      self.filetimes =  ptr_new(ft,/no_copy)
    ENDELSE 
  ENDIF 

  self.badval = ptr_new()
  IF n_elements(badval) NE 0 THEN self.badval = ptr_new(badval)


    ; ====== Start time =======

  IF n_elements(starttime) EQ 0 THEN BEGIN 
    IF nft NE 0 THEN BEGIN 
      starttime = (*(self.filetimes))[0]
    ENDIF ELSE BEGIN 
      ;Message,"Reading files for starttime: Not implemented yet!"
      filetimes = replicate({idldt},nf)
      obj = obj_new(object)
      IF NOT obj_valid(obj) THEN Message,"Can't create obj " + object
      found_one = 0
      FOR f=0,nf-1 DO BEGIN 
        file = (*(self.files))[f]
        time = obj-> GetTimeFromFile(file)
        IF isa(time,/idldt) THEN BEGIN 
          found_one = 1
          filetimes[f] = time
        ENDIF ELSE BEGIN 
          IF found_one EQ 1 THEN $
            Message,"Either *all* files must have times in them, or *none*"
        ENDELSE 
      ENDFOR 
      obj_destroy,obj
      starttime = filetimes[0]
      self.filetimes = ptr_new(filetimes,/no_copy)
      nft = nf
    ENDELSE 
  ENDIF 
  CASE 1 OF 
    isa(starttime,/idldt): self.starttime= starttime
    isa(starttime, /string, /nonempty): self.starttime=vaptime2idldt(starttime) 
    ELSE: Message,"<starttime> Need VapTime or IDLDT",/noname
  ENDCASE 

    ; ====== End  time =======

  IF n_elements(endtime) EQ 0 THEN BEGIN 
    IF nft NE 0 THEN BEGIN 
      endtime = (*(self.filetimes))[nft-1]
    ENDIF ELSE BEGIN 
      filetimes = replicate({idldt},nf)
      obj = obj_new(object)
      IF NOT obj_valid(obj) THEN Message,"Can't create obj " + object
      found_one = 0

      ;Message,"Reading files for endtime: Not implemented yet!"
      FOR f=0,nf-1 DO BEGIN 
        file = (*(self.files))[f]
        time = obj-> GetTimeFromFile(file)
        IF isa(time,/idldt) THEN BEGIN 
          found_one = 1
          filetimes[f] = time
        ENDIF ELSE BEGIN 
          IF found_one EQ 1 THEN $
            Message,"Either *all* files must have times in them, or *none*"
        ENDELSE 
      ENDFOR 
      obj_destroy,obj
      nft = nf
      endtime = filestimes[nf-1]
      self.filetimes = ptr_new(filetimes,/no_copy)
    ENDELSE 
  ENDIF 
  CASE 1 OF 
    isa(endtime,/idldt): self.endtime= endtime
    isa(endtime, /string, /nonempty): self.endtime=vaptime2idldt(endtime) 
    ELSE: Message,"<endtime> Need VapTime or IDLDT",/noname
  ENDCASE 

  tt = idldt2vaptime([self.starttime,self.endtime])
  Message,"Start Time of Animation: " + tt[0],/info
  Message,"End   Time of Animation: " + tt[1],/info
  Message,"Number of Frames: " + strtrim( self.nframes,2),/info

  IF self.starttime.julian LT ((*(self.filetimes))[0]).julian OR $
     self.endtime.julian GT ((*(self.filetimes))[nf-1]).julian THEN BEGIN 
    ft = idldt2vaptime( (*(self.filetimes))[ [0,nf-1] ] )
    Message,"Start/Endtime exceed filetimes",/info
    Message,"Time for First File: " + ft[0],/info
    Message,"Time for Last  File: " + ft[1]
  ENDIF 


  p0 = (where( (*(self.filetimes)).julian GT self.starttime.julian, nx ))[0]-1
  p1 = p0+1

  self.file_ptr =  [p0,p1]

  IF n_elements(lonpar) eq 2 THEN self.lonpar = lonpar ELSE $
    IF n_elements(lonpar) NE 0 THEN Message,"lonpar must be 2-vector!"

  IF n_elements(latpar) EQ 2 THEN self.latpar = latpar ELSE $
    IF n_elements(latpar) NE 0 THEN Message,"latpar must be 2-vector!"

  dt = (self.endtime.julian - self.starttime.julian)/(nframes-1)

  unit = 'DAY'
  eps = dt*86400./100.
  IF dt LT 1 THEN BEGIN 
    unit = 'HOUR'
    dt = dt*24.
    eps = eps/24.
  ENDIF 
  IF unit EQ 'HOUR' AND dt LT 1 THEN BEGIN 
    unit = 'MIN'
    dt = dt*60.
    eps = eps/60.
  ENDIF 
  IF unit EQ 'MIN' AND dt LT 1 THEN BEGIN 
    unit = 'SEC'
    dt = dt*60.
    eps = eps/60.
  ENDIF 

  self.delta_t.units = unit
  self.delta_t.dt = dt
  self.delta_t.eps =  eps

  self.data0 =  obj_new(object,(*(self.files))[self.file_ptr[0]], $
                                lonpar=lonpar,latpar=latpar)
  IF NOT obj_valid(self.data0) THEN $
    Message,"Can't create data0"
  self.data1 =  obj_new(object,(*(self.files))[self.file_ptr[1]], $
                                lonpar=lonpar,latpar=latpar)
  IF NOT obj_valid(self.data1) THEN $
    Message,"Can't create data1"

  self.data0-> get,data = d0, time=t0
  self.data1-> get,data = d1, time=t1
  dd = d1-d0
  tt = self.data0-> Time()
  self.delta_d = obj_new(object,data=dd,lonpar=lonpar,latpar=latpar,time=tt)
  IF NOT obj_valid(self.delta_d) THEN $
    Message,"Can't create data_d"

  self.cur_data = obj_new(object)
  self.frame = -1
  tt = self-> CalcFrameTime(-1)
  self.frametime = tt
  cur_data = self-> GetNextFrame()
  dd = (d0=(d1=(cur_data=(cur_data=0))))

  return,1
END


;============================================
; Cleanup
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO geoanim::Cleanup
   ptr_free, self.filetimes, self.files, $
       self.badval
   obj_destroy,self.data0
   obj_destroy,self.data1
   obj_destroy,self.delta_d
   obj_destroy,self.cur_data
   
END


;============================================
; Cleanup
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

FUNCTION geoanim::GetFrame, frame
   CASE 1 OF 
     frame EQ self.frame: BEGIN 
       self.cur_data-> get,data = data
       return, data
     END 
     frame-self.frame EQ 1: BEGIN 
       tt = self-> CalcFrameTime(frame)
       self.frametime = tt
       self.frame = frame
       t1 = self.data1->time()

         ; Check to make sure that we haven't stepped into the next
         ; interval!

       IF abs(self.frametime.julian-t1.julian)*86400. LE self.delta_t.eps THEN BEGIN 
         self.data1-> get,data = cur_data
         self.cur_data-> Set,data = cur_data,time=t1,$
             lonpar=self.lonpar,latpar=self.latpar
         IF self.frametime.julian GT t1.julian THEN BEGIN 
           IF self.frame NE self.nframes-1 THEN BEGIN 
             self.data1-> get,data = d1,time=t1
             files = (*self.files)
             fp = (self.file_ptr= self.file_ptr+1)
             obj_destroy,self.data0
             self-> get,data1 = data1
             self.data0 =  data1
             self.data1 =  obj_new(self.object, files[fp[1]], $
                                   lonpar=self.lonpar, latpar=self.latpar)

             self.data0->Get,data = d0
             self.data1->Get,data = d1
             delta_d = d1-d0
             tt = self.data0->Time()
             self.delta_d-> Set,data = delta_d,$
                             lonpar=self.lonpar,$
                                latpar=self.latpar,time=tt

           ENDIF 
         ENDIF 
         return,cur_data
       ENDIF 

       IF self.frametime.julian GT t1.julian THEN BEGIN 
           ; We've gone into the next interval.
         self.data1-> get,data = d1,time=t1
         files = (*self.files)
         fp = (self.file_ptr= self.file_ptr+1)
         obj_destroy,self.data0
         self-> get,data1 = data1
         self.data0 =  data1
         self.data1 =  obj_new(self.object, files[fp[1]], $
                               lonpar=self.lonpar, latpar=self.latpar)

         ;self.data0-> set,data = d1,time=t1,lonpar=self.lonpar,latpar=self.latpar
         ;s =  self.data1-> read(files[fp[1]])

         self.data0->Get,data = d0
         self.data1->Get,data = d1
         delta_d = d1-d0
         tt = self.data0->Time()
         self.delta_d-> Set,data = delta_d,$
                         lonpar=self.lonpar,$
                            latpar=self.latpar,time=tt

       ENDIF 

     END
     ELSE: BEGIN 
       self.frametime = self-> CalcFrameTime(frame)
       self.frame = frame
       fjd = self.frametime.julian
       filejd = (*self.filetimes).julian
       x = where(filejd GE fjd,nx)
       fp = [-1,0]+x[0]
       IF fp[0] LT 0 THEN fp = fp+1
       self.file_ptr = fp
       files = (*self.files)
       s =  self.data0-> read(files[fp[0]])
       s =  self.data1-> read(files[fp[1]])
       self.data0->Get,data = d0
       self.data1->Get,data = d1
       delta_d = d1-d0
       IF ptr_valid(self.badval) THEN BEGIN 
         x = where( d0 EQ *self.badval OR d1 EQ *self.badval, nx )
         IF nx NE 0 THEN delta_d[x] =  *self.badval
       ENDIF 
       tt = self.data0->Time()
       self.delta_d-> Set,data = delta_d,$
                       lonpar=self.lonpar,$
                          latpar=self.latpar,time=tt

     END 
   ENDCASE 

   t0 = self.data0->time()
   t1 = self.data1->time()
   self.data0-> get,data = d0
   self.data1-> get,data = d1
   self.delta_d-> Get,data = delta_d

   top = (self.frametime.julian-t0.julian)
   bot = (t1.julian-t0.julian)
   CASE self.delta_t.units OF 
     'SEC'  : BEGIN 
       top = top*24*60*60
       bot = bot*24*60*60
      END 
     'MIN'  : BEGIN 
       top = top*24*60
       bot = bot*24*60
     END 
     'HOUR' : BEGIN 
       top = top*24
       bot = bot*24
     END 
     'DAY'  : 
   ENDCASE 
   factor = top/bot

   nbad = 0
   IF ptr_valid(self.badval) THEN $
     bad = where( delta_d EQ *self.badval,nbad)


   IF self.data0-> type() EQ 'VECTOR' THEN BEGIN 
     x = d0[*,*,0]
     y = d0[*,*,1]
     dx = delta_d[*,*,0]
     dy = delta_d[*,*,1]

     cur_x = x+factor*dx
     cur_y = y+factor*dy
     cur_data = [ [[temporary(cur_x)]], [[temporary(cur_y)]] ]
     x=(y=(d0=(dx=(dy=0))))
   ENDIF ELSE $
     cur_data = temporary(d0)+factor*temporary(delta_d)

   IF nbad NE 0 THEN cur_data[bad] =  *self.badval
   self.cur_data-> Set,data = cur_data, lonpar=self.lonpar,$
                       latpar=self.latpar,time=self.frametime
   return,cur_data

END

;============================================
; GetNextFrame
;
;============================================
FUNCTION geoanim::GetNextFrame
  return,self-> GetFrame(self.frame+1)
END

;============================================
; CalcFrameTime
;   Given the frame, calculate it's time and 
;   return it.
;============================================
FUNCTION geoanim::CalcFrameTime, frame
  IF abs(frame-self.frame) EQ 1 THEN BEGIN 
    IF sign(frame-self.frame) EQ 1 THEN BEGIN 
      CASE self.delta_t.units OF 
        'SEC'  : frametime = dt_add(self.frametime, $
                                         sec = self.delta_t.dt)
        'MIN'  : frametime = dt_add(self.frametime, $
                                         min = self.delta_t.dt)
        'HOUR' : frametime = dt_add(self.frametime, $
                                         hour= self.delta_t.dt)
        'DAY'  : frametime = dt_add(self.frametime, $
                                         day = self.delta_t.dt)
      ENDCASE 
    ENDIF ELSE BEGIN 
      CASE self.delta_t.units OF 
        'SEC'  : frametime = dt_subtract(self.frametime, $
                                         sec = self.delta_t.dt)
        'MIN'  : frametime = dt_subtract(self.frametime, $
                                         min = self.delta_t.dt)
        'HOUR' : frametime = dt_subtract(self.frametime, $
                                         hour= self.delta_t.dt)
        'DAY'  : frametime = dt_subtract(self.frametime, $
                                         day = self.delta_t.dt)
      ENDCASE 
    ENDELSE 
  ENDIF ELSE BEGIN 
    IF frame GT 0 THEN BEGIN 
      CASE self.delta_t.units OF 
        'SEC'  : frametime = dt_add(self.starttime, $
                                         sec = frame*frame*self.delta_t.dt)
        'MIN'  : frametime = dt_add(self.starttime, $
                                         min = frame*self.delta_t.dt)
        'HOUR' : frametime = dt_add(self.starttime, $
                                         hour= frame*self.delta_t.dt)
        'DAY'  : frametime = dt_add(self.starttime, $
                                         day = frame*self.delta_t.dt)
      ENDCASE 
    ENDIF ELSE BEGIN 
      CASE self.delta_t.units OF 
        'SEC'  : frametime = dt_subtract(self.starttime, $
                                         sec = abs(frame)*self.delta_t.dt)
        'MIN'  : frametime = dt_subtract(self.starttime, $
                                         min = abs(frame)*self.delta_t.dt)
        'HOUR' : frametime = dt_subtract(self.starttime, $
                                         hour= abs(frame)*self.delta_t.dt)
        'DAY'  : frametime = dt_subtract(self.starttime, $
                                         day = abs(frame)*self.delta_t.dt)
      ENDCASE 
    ENDELSE 
  ENDELSE 
  return, frametime
END

;============================================
; Set Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO geoanim::Set, data0 = data0, data1=data1, delta_d=delta_d, $
           curdata=curdata, file_ptr=file_ptr, starttime=starttime, $
           endtime=endtime, frametime=frametime, nframes=nframes, $
           delta_t=delta_t, dt=dt, units=units, files=files, filetimes=filetimes


  IF n_elements(data0)     NE 0 THEN BEGIN 
    obj_destroy, self.data0
    self.data0 =  data0
  ENDIF 
  IF n_elements(data1)     NE 0 THEN BEGIN 
    obj_destroy, self.data1
    self.data1 =  data1
  ENDIF 
  IF n_elements(delta_d)   NE 0 THEN BEGIN 
    obj_destroy,self.delta_d
    self.delta_d = delta_d
  ENDIF 
  IF n_elements(curdata)   NE 0 THEN BEGIN 
    obj_destroy, self.curdata
    self.curdata =  curdata 
  ENDIF 
  IF n_elements(file_ptr)  NE 0 THEN self.file_ptr =  file_ptr 
  IF n_elements(starttime) NE 0 THEN starttime = self.starttime
  IF n_elements(endtime)   NE 0 THEN endtime = self.endtime
  IF n_elements(frametime) NE 0 THEN frametime = self.frametime
  IF n_elements(nframes)   NE 0 THEN nframes = self.nframes
  IF n_elements(delta_t)   NE 0 THEN delta_t = self.delta_t
  IF n_elements(dt)        NE 0 THEN dt = self.delta_t.dt
  IF n_elements(files)     NE 0 THEN *(self.files) = files
  IF n_elements(filetimes) NE 0 THEN *(self.filetimes) = filetimes

END


;============================================
; Get Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO geoanim::Get, data0 = data0, data1=data1, delta_d=delta_d, $
           curdata=curdata, file_ptr=file_ptr, starttime=starttime, $
           endtime=endtime, frametime=frametime, nframes=nframes, $
           delta_t=delta_t, dt=dt,files=files, filetimes=filetimes, $
           framenum=framenum

  
  IF arg_present(data0) THEN data0=self.data0
  IF arg_present(data1) THEN data1=self.data1
  IF arg_present(delta_d) THEN delta_d=self.delta_d
  IF arg_present(curdata) THEN curdata = self.curdata
  IF arg_present(file_ptr) THEN file_ptr = self.file_ptr
  IF arg_present(starttime) THEN starttime = self.starttime
  IF arg_present(endtime) THEN endtime = self.endtime
  IF arg_present(frametime) THEN frametime = self.frametime
  IF arg_present(nframes) THEN nframes = self.nframes
  IF arg_present(delta_t) THEN delta_t = self.delta_t
  IF arg_present(dt) THEN dt = self.delta_t.dt
  IF arg_present(files) THEN files = self.files
  IF arg_present(filetimes) THEN filetimes = self.filetimes
  IF arg_present(framenum) THEN framenum=self.frame


END
;============================================
; Type
;============================================

FUNCTION geoanim::type
  return, self.data0-> type()
END


;============================================
; Times
;============================================

PRO geoanim::print_times
  print,['Frame time','File time'],form='(2(a20,2x))'
  FOR frame=0,self.nframes-1 DO BEGIN 
    dd = self-> getframe(frame)
    self.data0-> get,time = ftime1
    self.data1-> get,time = ftime2
    CASE frame OF 
      0: BEGIN 
       self.data0-> get,time = ftime1
       IF abs(self.frametime.julian-ftime1.julian) GT 1.d/(24.*60* 60)THEN  BEGIN 
         print,frame,idldt2vaptime(ftime1),form='(i3, 24x, a20)'
         print,idldt2vaptime(self.frametime),form='(2x, a20)'
       ENDIF ELSE BEGIN 
          print,frame, idldt2vaptime([self.frametime,ftime1]), form='(i3, 2x, 2(a20,2x))'
       ENDELSE 
      END 
      (self.nframes-1): BEGIN 
        self.data1-> get,time = ftime2
        IF abs(self.frametime.julian-ftime2.julian) GT 1.d/(24.*60*60) THEN BEGIN 
          print,frame, idldt2vaptime(ftime2),form='(i3, 24x, a20)'
          print,idldt2vaptime(self.frametime),form='(2x,a20)'
        ENDIF ELSE BEGIN 
          print,frame,idldt2vaptime([self.frametime,ftime2]),form='(i3,2x,2(a20,2x))'
        ENDELSE 
      END 
      ELSE: BEGIN 
        ft1 =  self-> calcframetime(frame+1)
        IF self.frametime.julian LT ftime2.julian AND $
           ft1.julian GE ftime2.julian THEN BEGIN 
          IF abs(self.frametime.julian-ftime2.julian) GE 1.d/86400. THEN BEGIN 
            t = idldt2vaptime([self.frametime,ftime2])
            print,frame,t[0],form='(i3,2x,a20)'
            print,t[1],form='(27x,a20)'
          ENDIF ELSE BEGIN 
            print,frame,idldt2vaptime([self.frametime,ftime2]),form='(i3,2x,2(a20,2x))'
          ENDELSE 
        ENDIF ELSE print,frame,idldt2vaptime(self.frametime),form='(i3,2x,a20)'
      END 
    ENDCASE 
  ENDFOR 
END


;============================================
; Definition Routine
;============================================

PRO geoanim__define
   junk =  {delta_t, $
            dt   : 0.0d, $
            eps  : 0.0d, $
            units: 'SEC' }
  junk = {geoanim,              $
          type      : ''        ,$ ; SCALAR | VECTOR
          object    : ''        ,$ ; Object for reading data.
          nframes   : 0l        ,$
          frame     : 0l        ,$ ; 0 <=frame<=nframes-1
          lonpar    : [0.,0]    ,$
          latpar    : [0.,0]    ,$
          frametime : {idldt}   ,$
          delta_t   : {delta_t} ,$
          starttime : {idldt}   ,$
          endtime   : {idldt}   ,$
          files     : ptr_new() ,$
          file_ptr  : intarr(2) ,$
          filetimes : ptr_new() ,$
          badval    : ptr_new(), $
          missing   : ptr_new(), $
          data0     : Obj_new() ,$
          data1     : obj_new() ,$
          cur_data  : obj_new() ,$
          delta_d   : obj_new() }
END

