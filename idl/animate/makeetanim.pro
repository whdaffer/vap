;+
; NAME:  makeetanim.pro
; $Id$
; PURPOSE:  Make an Earth Today animation
;
; AUTHOR:  whd
;
; CATEGORY:  Animation
;
; CALLING SEQUENCE:  makeetanim [, date_time, time_inc, 
;                                interpfile=interpfile,
;                                wpath=wpath, filter=filter,
;                                wfiles=wfiles, lonpar=lonpar,
;                                latpar=latpar, rainf=rainf,
;                                ermax=ermax, startime=starttime,
;                                endtime=endtime,
;                                interptime=interptime,
;                                decimate=decimate,
;                                crdecimate=crdecimate, rflag=rflag,
;                                tolerance=tolerance, gif=gif,
;                                lockfile=lockfile 
;
; 
; INPUTS:  
;
;    date_time: The end of the time range searched for wind
;               data to be used in making the
;               animation. (default=current time)
;    time_inc: subtracted from 'date_time' to caluclate the start time
;              of the interval (default=26 hours)
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
;  interpfile: (I) scalar string, FQFN: Name of interpolated file to use,
;              overrides all other pertinent parameters/keywords.
;  Wpath: (I) Place to look for wind files (default=VAP_WINDS)
;  
;  Filter: (I) Filter to use in finding wind files.
;
;  Wfiles: (I) Array of FQFNs. Wind files to use in making animation. This
;          keyword overrides date_time/time_inc combo.
;
;  Lonpar: (I/O) float 3-vector. [Min, Max, Inc]
;          If set on input, these numbers will be used in the
;          manufacture of the field. Otherwise the values which are
;          used are returned in this keyword.
;
;  Latpar: (I/O) Same as Lonpar, but for latitude.
;
;  Rainf: (I) float vector: The 'RAdius of INfluence' used in making the
;         interpolated field. (cf. makeinterpfile or
;         succor.pro). Breifly, a vector of decreasing values
;         indicating the radiuis of influence (in grid increments) to
;         be used in each successive approximation when determining
;         which data false within the 'influence' of a particular grid
;         location. (def=[12.,6,1])
;
;
;  ermax: (I) float vector: The maximum difference between the value at a
;         grid location and the value of a datum before that datum is
;         discarded. Default=replicate(50., n_elements(rainf)) which
;         says, essentially, that no data is thrown out)
;
;  Starttime: (O) scalar string. Earliest time found in whatever wind files are used.
;
;  Endtime:   (O) scalar string. Latest time found in whatever wind files are used.
;
;
;  Interptime: (I/O) scalar string. The 'Time' of the interpolation. If not input,
;              it's the mean of the start/end times. If input is must
;              lie between the start/end times.
;
;  Outfile: (I/O) scalar string. If not present on input, the output file is returned
;           in this keyword. If present on input it is taken as the
;           FQFN to give to the  output file.
;
;  Decimate:  (I) Scalar int. Take every 'n'th vector. This keyword is
;            passed directly to makeetfield where it is dealt with.  
;
;  CRDecimate: (I) 2 vector. If crdecimate=[m,n], take every m-th
;              column and 'n'th row.  This keyword is passed directly
;              to `MakeETField' where it is dealt with. 
;
;  ExcludeCols:(I)  scalar string. Must be a string of comma separated
;               fields. Each field may be either a single number or
;               two numbers separated by a singel colon. The colon
;               separated fields represent a range of columns and the
;               single number is a single column. 
;
;                 E.g. '0:3, 5, 23:30, 70, 74' would exclude columns 0
;                 through 3, 5, 23 through 30, 70 and 74.
;
;              Passed directly to MakeETField.
;
;  rflag:  (I). Flag. If set, remove rain flagged data.
;
;  tolerance: (I). scalar float. The radius, in grid elements, off
;             allowable voids in the *INPUT* data. (The Succor will
;             fill voids of certain sized, so voids in the input data
;             are allowed)
;
;  gif:  (I). Flag. Make gif files instead.
;
;  lockfile: (I). For use with the automated processing.
;
; OUTPUTS:  
;
;   In the standard operating procedure, this routine will make an
;   interpolated field and then however many frames are called for in
;   the animation in the current directory. The control of the
;   animation is hard coded at the moment. 
;
;   I put some keywords in to make it more flexible (outfile) but I
;   haven't really tested them yet.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 2000, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO makeetanim, date_time, $;(I/O) yy/mm/dd/hh End time 
                           ; of interpolated field 
                           ; (default = current_time)
   time_inc  , $                ; (I/O) start_time=date_time-time_inc
                                ; default=26 hours.
   interpfile = interpfile, $   ; Name of interpolated file to use. 
                                ; overrides all the other pertanent 
                                ; keyword/parameters
    Wpath     = Wpath    , $    ; path to wind files.
    Filter    = Filter   , $    ; Filter to use in finding wind 
                                ; files.
   Wfiles    = Wfiles   , $     ; (I/O) Wind files included in 
                                ; Interpolated
                                ; field. if this
                                ; keyword is set,
                                ; date_time and
                                ; time_inc are ignored.
   Lonpar    = Lonpar   , $     ; (I/O) 3-vector, [min,max,inc]. 
                                ; If set on input, these 
                                ; numbers determine the field,
                                ; otherwise, they
                                ; are defaulted and
                                ; returned in this
                                ; keyword.
   Latpar    = Latpar   , $     ; (I/O) Same as Lonpar, but 
                                ; for latitude.
   RaInf     =  RaInf   , $     ; (I) Radius of Influence, 
                                ; a vector, a
                                ; decreasing
                                ; sequence of floats
                                ; indicating the
                                ; radius of grid
                                ; cells to use in
                                ; the calculation of
                                ; current cell.
                                ; def=[12.,6,1]
   ErMax =  ErMax       , $     ; (I), Error Max. vector (float)
                                ; Maximum Error
                                ; allowed before the
                                ; data field is discarded
                                ; in favor of the
                                ; computed model.
                                ; default = replicate(50., n_elements(rainf))
   StartTime = StartTime, $     ; (O) Returned earliest time 
                                ; in Wind Files
   EndTime   = EndTime  , $     ; (O) Returned latest time 
                                ;  in Wind Files
   Interptime = InterpTime,$    ; (I/O) 'Time' of interpolation. 
                                ; If not input, it's the mean of 
                                ; the start/end
                                ; times.  If input,
                                ; it must lie
                                ; between start/end
                                ; times. 
   OutFile = OutFile ,$         ; (I/O). If present on input, 
                                ; this will be the name of the
                                ; output file. If present as a
                                ; return argument, the name of the
                                ; output file will be returned in it.
   Decimate = Decimate, $       ; See Explanation above
    CRDecimate = CRDecimate,$   ; See Explanation above
    ExcludeCols = ExcludeCols,$ ; See Explanation above
    rflag=rflag , $             ; If set, remove rain flagged data
    tolerance=tolerance,$       ; Permissable radius of void areas in grid, 
                                ; expressed in units of 'grid
                                ; locations.' default=rainf[0]*1.5
   gif = gif, $                 ; Make 'gif's instead
   lockfile = lockfile          ; Useful in automatic processing


  gif = keyword_set(gif);
  auto = 0
  catch, error
  IF error NE 0 THEN BEGIN 
    Message,"ERROR: " + !error_state.msg,/noname,/cont
    IF auto  THEN BEGIN 
      IF n_elements(llun) NE 0 THEN BEGIN 
        catch,/cancel
        printf, llun, "ERROR: " + !error_state.msg
        free_lun, llun
      ENDIF 
    ENDIF 
    return
  ENDIF 
   

  auto = n_elements(lockfile) NE 0 
  elapsedtime = -1.0
  IF auto THEN BEGIN 
    t0 = systime(1)
    basename = basename(lockfile)
    tmp =  strsplit(basename,".",/extract)
    time = tmp[1]
    pid = long(tmp[2])
    openr, llun, lockfile, /get, error=err
    IF err NE 0 THEN $
       Message,!error_state.msg
    ppid = 0l
    readf, llun, ppid
    free_lun,llun
    IF ppid NE pid THEN BEGIN 
      print,"ppid= ",ppid, " pid= ", pid
      Message,"pid and ppid mismatch! How is that possible!"
    ENDIF 
    openw, llun, lockfile, error=err,/get
    IF err NE 0 THEN $
       Message,!error_state.msg
  ENDIF 

  IF n_elements(interpfile) EQ 0 THEN BEGIN 
    struct = makeEtfield(date_time, time_inc, wfiles=wfiles, $
                         rainf=rainf, ermax=ermax, tolerance=tolerance)

    str = 'Date_Time: '+ date_time +' and time_inc ' + $
       string(time_inc,form='(f7.2)')
    message,str,/info
    IF auto THEN $
      printf, llun, "INFO: " + str
    wfiles_str =  "WFILES=" + strjoin( wfiles, ",")
    message, wfiles_str,/info
    IF auto THEN  printf, llun, "INFO:" + wfiles_str

    IF struct.status EQ 0 THEN Message,struct.error_state.msg
    str = "INTERPFILE=" + struct.filename
    message,str,/info
    IF auto THEN  printf, llun, "INFO:" + str
    interpfile = struct.filename

  ENDIF ELSE  Message,'Using ' + interpfile ,/info


  lonpar = [0,359.,1]
  latpar = [-60,60,1]
  vlonpar = [lonpar[0:1],3]
  vlatpar = [latpar[0:1],3]
  animpar = [1024,512,60]

  !x.margin = (!y.margin = [0,0])

  q = obj_new('qmodel',file=interpfile)
  s = q-> getplotdata(u,v,lon,lat)
  
  obj_destroy,q
  
  nvlon = (vlonpar[1]-vlonpar[0])/vlonpar[2]+1
  nvlat = (vlatpar[1]-vlatpar[0])/vlatpar[2]+1
  vloni = (vloni0 = (findgen(nvlon)*vlonpar[2]+vlonpar[0])#replicate(1.,nvlat))
  vlati = (vlati0 = replicate(1.,nvlon)#(findgen(nvlat)*vlatpar[2]+vlatpar[0]))
  
  nframes = animpar[2]

  pathinc = 0.02

  dim = size(vloni,/dim)
  timei = fix( randomu(seed,dim[0],dim[1])*(nframes-1))

  ;; Spinup

  save_device =  !d.name
  set_plot,'z'
  device,set_resolution=[1024,512]
  !x.margin = [0,0]
  !y.margin = [0,0]
  PTC = ReadColorTable( $
     "$VAP_RESOURCES/Color_Tables/vap-animation.ct")  
  ct = *ptc &  ptr_free, ptc
  
  Red   = reform(CT[0,*])
  Green = reform(CT[1,*])
  Blue  = reform(CT[2,*])

  loadct,0
  tvlct,transpose(ct),0
  tvlct,r,g,b,/get
  windStart = 1
  windEnd = 28
  veccolor = 29
  nwindcolors = windEnd-windStart+1

  map_set,0,180,/noborder

  tv,replicate(255,1024,512)
  map_continents,/fill,color=0
  landmask = tvrd()
  
  ss = 2> sqrt(u^2+v^2) < 30
  c_colors=findgen(29)/29*nwindcolors+windstart
  contour,ss,lon,lat,min=2,max=30,c_colors=c_colors,$
    lev=findgen(29)+1,/overplot,/cell_fill
  map_continents,/fill,color=0
  contourim = tvrd()

  FOR frm=0,nframes-1 DO BEGIN
    ii = (vloni-lonpar[0])/lonpar[2]
    jj = (vlati-latpar[0])/latpar[2]
    ui = bilinear( u,ii,jj)
    vi = bilinear( v,ii,jj)
    vloni = (vloni+pathinc*ui) MOD 360.
    vlati = vlati+pathinc*vi
    timei = timei+1
    expired = where(timei GE nframes,nexpired)
    IF nexpired NE 0 THEN BEGIN 
      vloni[expired] = vloni0[expired]
      vlati[expired] = vlati0[expired]
      timei[expired] = 0
    ENDIF 
    
    x = where( vloni LT 0, nx )
    IF nx NE 0 THEN vloni[x] =  vloni[x] + 360.

    x = where( abs(vlati) GE 90, nx )
    IF nx NE 0 THEN BEGIN 
      vloni[x] =  (vloni[x] + 180) MOD 360.
      vlati[x] =  sign(vlati[x])*90 + $
       (sign(vlati[x])*90 - vlati[x]) 
    ENDIF 

  ENDFOR 
  FOR frm=0,nframes-1 DO BEGIN 

    map_set,0,180,/noborder
    tv,contourim
    plotvect,ui,vi,vloni,vlati,len=4,color=veccolor

    finalim = tvrd() AND landmask
    tv,finalim
    IF gif THEN BEGIN 
      finalim = tvrd() 
      filename ='gwind.' + string(frm,form='(i3.3)') 
      write_gif, filename, finalim, r,g,b
    ENDIF ELSE BEGIN 
      finalim = tvrd(/order)
      filename ='nwind.' + string(frm,form='(i3.3)') 
      write_png,filename, finalim, r,g,b
    ENDELSE 


    ii = (vloni-lonpar[0])/lonpar[2]
    jj = (vlati-latpar[0])/latpar[2]
    ui = bilinear( u,ii,jj)
    vi = bilinear( v,ii,jj)

    vloni = vloni+pathinc*ui
    vlati = vlati+pathinc*vi
    timei = timei+1
    expired = where(timei GE nframes,nexpired)
    IF nexpired NE 0 THEN BEGIN 
      vloni[expired] = vloni0[expired]
      vlati[expired] = vlati0[expired]
      timei[expired] = 0
    ENDIF 

    x = where( vloni LT 0, nx )
    IF nx NE 0 THEN vloni[x] =  vloni[x] + 360.

    x = where( abs(vlati) GE 90, nx )
    IF nx NE 0 THEN BEGIN 
      vloni[x] =  (vloni[x] + 180) MOD 360.
      vlati[x] =  sign(vlati[x])*90 + $
       (sign(vlati[x])*90 - vlati[x]) 
    ENDIF 
  ENDFOR   

  IF auto THEN BEGIN 
    elapsed = string((systime(1)-t0)/60.d,form='(g15.4)')
    printf, llun, 'INFO: ELAPSED_TIME='+elapsed
  ENDIF 
  set_plot,save_device
END

  

  
    

  
