PRO FILEMENU_Event, Event
stateid =  widget_info( event.top,/child )
widget_control, stateid, get_uval=s
lf =  string(10b)
recsep =  lf + '                 -- % --                   ' + lf


CASE Event.value OF 
  1: BEGIN
     PRINT, 'Event for Pick Files.Wind Files(s)'
     str =  recsep + ' Picking Goes file '
     WIDGET_CONTROL, s.ids.statusid, get_uvalue=v
     v =  [str, v ]
     WIDGET_CONTROL, s.ids.statusid, set_value= v
     WIDGET_CONTROL, s.ids.statusid, set_uvalue= v     
     goes_file =  whdpickfile( filter= s.goesfilter,$
                               path=s.goespath, $
                               get_path= npath ,$
                               get_filter= nfilt ,/one )

     s.goesfile = goes_file(0)
     goes_file =  goes_file(0)
     s.goespath =  npath
     s.goesfilter =  nfilt
     nf =  n_elements( goes_file )
     IF nf EQ 1 AND strlen( goes_file ) GT 0 THEN BEGIN
       str =  recsep + 'Reading Goes file: ' + GOES_FILE + lf
       WIDGET_CONTROL, s.ids.statusid, get_uvalue= v
       v =  [str, v ]
       WIDGET_CONTROL, s.ids.statusid, set_value= v
       WIDGET_CONTROL, s.ids.statusid, set_uvalue= v

       ; open goes_file and read the time info from it.
       READ_PCGOES, s.goesfile, limits, data, image, year, jday, time, nlon, $
        nlat, lons, lats, info, /just_header, status= status, $
        version= version, area_file= area_file
        DATA= 0; just in case it didn't stop
        image =  0;
        IF version EQ 0 THEN BEGIN 
          str =  "Antiquated Grid File !!! " + lf + $
           " This Grid file doesn't have then name " + $
           " of the input AREA file in it." + lf + $
           " We can't continue without this information " + lf + $
           " You'll have to do this one by hand or pick " + $
           " another grid file! " + lf + $
           " Sorry " + lf
          message,' str, /info
          CW_WARN, str, "Wrong grid file Version !!!"
          
        ENDIF ELSE BEGIN 
         IF status NE 1 THEN BEGIN
           str =  ' Error reading goes file ' + s.goesfile
           message, str, /info
           CW_WARN, str, "Error reading Goes file File!!!"
           str =  " Couldn't read goes file " + s.goesfile + lf
           str =  str + ' Pick Another !'
           WIDGET_CONTROL, s.ids.statusid, get_uval = v
           v =  [str, v ]
           WIDGET_CONTROL, s.ids.statusid, set_value= v
           WIDGET_CONTROL, s.ids.statusid, set_uvalue= v
         ENDIF ELSE BEGIN 

           IF year EQ 0 THEN BEGIN
             ; older version of the grid file don't
             ; have the year info in them.
             spawn,'date',ret_str
             tmp =  str_sep( ret_str(0), ' ' )
             year =  tmp( n_elements(tmp)-1 )
           ENDIF 
           hhmm =  strtrim( time, 2 )
           l =  strlen( hhmm )
           CASE l OF 
             4: hhmm =  hhmm
             3: hhmm =  '0' + hhmm
             2: hhmm =  '00' + hhmm
             ELSE : stop
           ENDCASE
           hh =  strmid( hhmm,0,2)

           tt =  doy2date( year, jday  )
           month =  strtrim(tt(0) ,2 )
           IF strlen( month ) EQ 1 THEN  month= '0' + month
           dom =  strtrim( tt(1), 2 )
           IF strlen( dom ) EQ 1 THEN dom =  '0' + dom
           s.datetime = month+dom+hh
           s.area_file =  area_file
           WIDGET_CONTROL, stateid, set_uval=s
           junk =  where( strlen( s.windfiles ) NE 0, nn )
           IF nn EQ 0 THEN $
            str1 =  ' Waiting to pick wind file ' ELSE $
            str1 =  ' Waiting to do overlay '
           str =  ' Goes file read success '+ lf + str1 
           widget_control,s.ids.statusid,get_uval= v
           v =  [str, v ]
           WIDGET_CONTROL,s.ids.statusid,set_val=v
           WIDGET_CONTROL,s.ids.statusid,set_uval=v
         ENDELSE 
       ENDELSE 
     ENDIF 

  END
  2 : BEGIN
      PRINT, 'Event for Pick Files.Wind Files(s)'
      str =  recsep + ' Picking Wind Files '
      WIDGET_CONTROL, s.ids.statusid, get_uval=v
      v =  [str, v ]
      WIDGET_CONTROL, s.ids.statusid, set_val=v
      WIDGET_CONTROL, s.ids.statusid, set_uval=v

      files =  whdpickfile( path= s.windpath ,$
                            filter= s.windfilter, $
                            get_path= npath, $
                            get_filter= nfilt )
      nf =  n_elements( files )
      IF npath NE s.windpath THEN s.windpath =  npath
      IF nfilt NE s.windfilter THEN s.windfilter =  nfilt

      IF nf GT 1 OR $
         nf EQ 1 AND strlen( files(0) ) GT 0 THEN $
       s.windfiles =  files 
      junk =  strlen( s.goesfile ) NE 0
      IF junk THEN $
       str1 =  ' Ready to do Goes Overlay ' ELSE $
       str1 =  ' Must Pick Goes File ' 
      str =  ' Finished Picking Wind Files: ' + str1 + lf
      WIDGET_CONTROL, s.ids.statusid, get_uval=v
      v =  [str, v ]
      WIDGET_CONTROL, s.ids.statusid, set_val=v
      WIDGET_CONTROL, s.ids.statusid, set_val=v
      WIDGET_CONTROL, stateid, set_uval=s
  END
ENDCASE
END




PRO CWGO_Event, Event

lf =  string(10b)
recsep =  lf + '                 -- % --                   ' + lf
stateid =  widget_info( event.top ,/child )
widget_control, stateid, get_uval=s


CASE event.id OF 

; Event for PICK_FILES
  s.ids.configid : BEGIN 
    cwgo_config, s
    WIDGET_CONTROL, stateid, set_uval=s
  END 
    
  s.ids.filemenuid: FILEMENU_Event, Event
  s.ids.RUN_OVERLAYid: BEGIN
    Print, 'Event for Run Overlay'
    str =  recsep + $
     ' Mapping Goes data to Cylindrical projection and overlaying winds' + lf
    str =  str + ' This may take about 5 minutes, please be patient! '
    WIDGET_CONTROL, s.ids.statusid, get_uval=v
    v =  [str, v ]
    WIDGET_CONTROL, s.ids.statusid, set_uval=v
    WIDGET_CONTROL, s.ids.statusid, set_val=v
    len = strlen( s.windfiles )
    g =  (where( len EQ 0, ng ))(0)
    IF ng EQ 0 THEN g =  19 
    set_plot,'x'
    GOES_OVERLAY, s.goesfile, image, wdata,  $
     wf=s.windfiles(0:g-1), xsiz=1280, ysiz=1024, $
     windowid= s.ids.windowid, getminpix= getminpix, l2=s.l2
    
    s.gimage =  image &  image=0;
    s.minpix =  getminpix
    WIDGET_CONTROL, stateid, set_uval=s
    WIDGET_CONTROL, s.ids.dataid, get_uval=d
    d =  replicate( d, n_elements( wdata(*,0) ) )
    d.u =  wdata(*,0) 
    d.v =  wdata(*,1)
    d.lon =  wdata(*,2)
    d.lat =  wdata(*,3)
    wdata =  0
    WIDGET_CONTROL, s.ids.dataid, set_uval=d    

    print,' Done with GOES_OVERLAY'
    CW_WARN," DONE With GOES_OVERLY, Ready to pick zooms ", 'DONE!!!!'
    str =  recsep + ' Ready to pick zooms!' + lf
    WIDGET_CONTROL,s.ids.statusid, get_uvalu= v
    v =  [str, v ]
    WIDGET_CONTROL,s.ids.statusid, set_valu= v
    WIDGET_CONTROL,s.ids.statusid, set_uvalu= v

  END
  s.ids.ZOOMid: BEGIN
    Print, 'Event for Zooms'
    
    str =  recsep + ' Picking zooms! ' +LF 
    WIDGET_CONTROL,s.ids.statusid, get_uvalu= v
    v =  [str, v ]
    WIDGET_CONTROL,s.ids.statusid, set_uvalu= v
    WIDGET_CONTROL,s.ids.statusid, set_valu= v
    IF s.zoomix EQ 0 THEN BEGIN 
      zooms =  [-1,-1,-1,-1] 
      first_call = 1
    ENDIF ELSE BEGIN 
      zooms =  s.zooms(0:s.zoomix-1).zoom
      first_call = 0
    ENDELSE 
    CW_GO_ZOOM, zooms, first_call=first_call
    nz =  n_elements( zooms(0,*) )
    IF nz EQ 1 THEN BEGIN
      xx =  where( zooms - [-1,-1,-1,-1], nxx )
      nz =  (nxx NE 0)
    ENDIF 
    IF nz NE 0 THEN BEGIN
      s.zoomix =  nz
      s.zooms(0:nz-1).zoom =  zooms
      print,' Zooms = ', zooms
      WIDGET_CONTROL, stateid, set_uvalue=s
    ENDIF 
    str =  recsep + ' Done with picking Zooms!' + lf + $
     ' Ready to start running GOES gridder for each of the zooms '
    WIDGET_CONTROL,s.ids.statusid, get_uvalu= v
    v =  [str, v ]
    WIDGET_CONTROL,s.ids.statusid, set_uvalu= v    
    WIDGET_CONTROL,s.ids.statusid, set_valu= v
  END
  s.ids.RUNGRIDid: BEGIN
    Print, 'Event for Run Gridder'
    str1 = ' Running GOES gridder for Zoom: '
    WIDGET_CONTROL, s.ids.statusid, get_uvalu= v
    v =  [v, recsep ]
    WIDGET_CONTROL, s.ids.statusid, set_valu = v
    WIDGET_CONTROL, s.ids.statusid, set_uvalu= v

    IF s.zoomix GT 0 THEN BEGIN 
      zooms =  s.zooms(0:s.zoomix-1).zoom
      FOR z=0,s.zoomix-1 DO BEGIN
        str =  str1 + string( zooms(*,z), form='(4(i5))' ) 
        WIDGET_CONTROL, s.ids.statusid, get_uvalu= v
        v =  [str, v ]
        WIDGET_CONTROL, s.ids.statusid, set_valu = v
        WIDGET_CONTROL, s.ids.statusid, set_uvalu= v
        goesfile =  s.goesfile
        tmp =  str_sep( goesfile, '/')
        goesfile =  tmp( n_elements(tmp)-1 )
        sat  =  strmid( goesfile, 4,1 )
        sens =  strmid( goesfile, 5,1 )
        datetime =  s.datetime
        zoomstr =  '%' + $
         strtrim( zooms(0,z), 2 ) + ',' + $
         strtrim( zooms(1,z), 2 ) + ',' + $
         strtrim( zooms(2,z), 2 ) + ',' + $
         strtrim( zooms(3,z), 2 ) + '%'
        IF sens GT 1 THEN sensname =  'ir'+sens ELSE sensname = 'vis'
        exe_str =  '/usr/people/vapuser/perl/rgoes  sat=' + sat + $
         ' sensor=' + sensname + $
         ' time=' + datetime + $
         ' limits=' +  zoomstr
         ; spawn to rgoes. rgoes will create a
         ; script, run the 'goes' program using the script as input,
         ; gather the output from 'goes' (which is written to a log
         ; file with the same name as the script but the
         ; extension '.log'), rename the output file and return the
         ; name to this routine to be used in the next step. If the
         ; 'goes' program is unable to find the appropriate AREA file
         ; name, it'll signify this by returning the file name
         ; 'NO_SUCH_FILE' 
        SPAWN, exe_str, retval
        retval =  strtrim( retval(0), 2 )
        IF strpos( retval, 'GOES' ) NE 0 THEN BEGIN
          str =  ' **** Bad return from rgoes: ' + lf + $
           'for  exe_str = ' + exe_str + ' retval = ' + retval
          message,str,/cont
          WIDGET_CONTROL, s.ids.statusid, get_uvalu= v
          v =  [str, v ]
          WIDGET_CONTROL, s.ids.statusid, set_valu = v
          WIDGET_CONTROL, s.ids.statusid, set_uvalu= v
          IF retval EQ 'NO_SUCH_FILE' THEN BEGIN
            str =  " 'goes' gridding program unable to find AREA file for specified time " + lf
            str =  str + ' examine string sent to rgoes perl script for bad time field ' + lf 
            str =  str + " (scroll back a few  lines in 'status' window, " + lf
            str =  str + " look for '*** Bad return' ) "  + lf
            str =  str + ' if all else fails, use this program to determine the limits ' + lf 
            str =  str + " of the zooms, then exit and run 'goes' then  'goes_overlay'  " + lf
            str =  str + " by hand" + lf
            WIDGET_CONTROL, s.ids.statusid, get_uvalu= v
            v =  [str, v ]
            WIDGET_CONTROL, s.ids.statusid, set_valu = v
            WIDGET_CONTROL, s.ids.statusid, set_uvalu= v
            CW_WARN, str, " Can't find AREA file !!!"
          ENDIF 
            
        ENDIF ELSE BEGIN 
          str =  ' File created for zoom ' + $
           string( zooms(*,z), form = '(4(i5))')  + ' is:' + retval + recsep
          WIDGET_CONTROL, s.ids.statusid, get_uvalu= v
          v =  [str, v ]
          WIDGET_CONTROL, s.ids.statusid, set_valu = v
          WIDGET_CONTROL, s.ids.statusid, set_uvalu= v
        ENDELSE 
        s.zooms(z).file =  retval
        
      ENDFOR 
    ENDIF ELSE BEGIN
      str =  "No Zooms Chosen, There's nothing to do!"
      CW_WARN, "No Zooms!!!", str
      message, str,/cont
    ENDELSE 
    WIDGET_CONTROL, stateid, set_uval=s 
    WIDGET_CONTROL, s.ids.statusid, get_uvalu= v
    str =  recsep + " Done Gridding all Zooms, Ready to run GOES_Overlay on zooms" + lf
    v =  [str, v ]
    WIDGET_CONTROL, s.ids.statusid, set_valu = v
    WIDGET_CONTROL, s.ids.statusid, set_uvalu= v
    CW_WARN,' Done with gridding zooms, Ready to overlay winds ', 'DONE!!!'

  END
  s.ids.ovlyzid : BEGIN 
    WIDGET_CONTROL, s.ids.dataid, get_uval=d
    WIDGET_CONTROL, s.ids.statusid, get_uval=v
    str =  'Doing Overlays for each Zoom ' + recsep
    v =  [str, v ]
    WIDGET_CONTROL, s.ids.statusid, set_val=v
    WIDGET_CONTROL, s.ids.statusid, set_uval=v    

    FOR z=0,s.zoomix-1 DO BEGIN 
      infile = s.outpath + s.zooms(z).file
      WIDGET_CONTROL, s.ids.statusid, get_uval=v
      zoomstr =  string( s.zooms(z).zoom, form='(4(i4))' )
      str =  'Doing Zoom ' + zoomstr + lf
      str =  str + ' Input Goes grid file: ' + infile + lf
      str =  str + ' Output goes to : ' + s.outpath + lf + lf
      str =  str + ' Please be patient, each overlay may take 5 minutes'
      v =  [str, v ]
      WIDGET_CONTROL, s.ids.statusid, set_val=v
      WIDGET_CONTROL, s.ids.statusid, set_uval=v    

      GOES_OVERLAY, infile, uu=d.u, vv=d.v, $
       llon=d.lon, llat=d.lat, minpix= s.minpix,$
       outpath=s.outpath,getoutfile= outfile,/z,/gif
      s.zooms(z).ofile =  outfile
      WIDGET_CONTROL, stateid, set_uval=s

      ; read file back in
      READ_GIF, outfile, im, r,g,b
      ss =  size(im)
      ; open window and display image
      window, xsiz=ss(1), ysiz=ss(2), title=outfile,/free
      TV,im
      WIDGET_CONTROL, s.ids.statusid, get_uval=v
      str =  ' Output filename :' + outfile + lf
      v =  [str, v ]
      WIDGET_CONTROL, s.ids.statusid, set_val=v
      WIDGET_CONTROL, s.ids.statusid, set_uval=v    
      
      
    ENDFOR 

  END 
  s.ids.QUITid: BEGIN
    Print, 'Event for Quit'
    WIDGET_CONTROL, EVENT.TOP,/ DESTROY
  END
  s.ids.DRAWid: BEGIN
    ;Print, 'Event for DRAW'
  END
ENDCASE

END


; DO NOT REMOVE THIS COMMENT: END CWGO
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO cwgo, areafile, $
           GROUP       = Group, $
           goes_filter = goes_filter ,$
           goes_path   = goes_path   ,$
           goes_file   = goes_file   ,$
           area_file   = area_files  ,$
           wind_files  = wind_files  ,$
           wind_filter = wind_filter ,$
           wind_path   = wind_path ,$
          output_path= output_path ,$
          min_pix= min_pix ,$
          l2= l2

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


IF NOT keyword_set( goes_path )   THEN $
 goes_path =  '/disk2/vap/goes/today/'
IF NOT keyword_set( goes_filter ) THEN $
 goes_filter =  'GOES*.dat'

IF NOT keyword_set( wind_path ) THEN $
 wind_path =  '/disk2/vap/fromshiokaze2/'
IF NOT keyword_set( wind_filter ) THEN $
 wind_filter =  'S*'
IF NOT keyword_set( wind_files )  THEN $
 wind_files =  ''

IF NOT keyword_set( output_path ) THEN $
 output_path =  '/disk2/vap/goes/gridding/'
IF NOT keyword_set( min_pix ) THEN min_pix = -1;

area_file =  ''
IF keyword_set( goes_file ) THEN BEGIN 

  ;  nlon =  0L
  ;  nlat =  nlon
  ;  jday =  nlon
  ;  time =  nlon
  ;  limits =  lonarr(4)
  ;  lonsize =  0.
  ;  latsize = 0.


  ;  openr,rlun, goes_file, /f77, /get, error= err
  ;  IF err NE 0 THEN BEGIN
  ;    message, !err_string, /cont
  ;    return
  ;  ENDIF 

  ;  readu,rlun, nlat, nlon, jday, time, limits, lonsize, latsize
  ;  free_lun, rlun

  ;  year =  strtrim( fix( jday/1.e3),2 )

  ; open goes_file and read the time info from it.
  READ_PCGOES, goes_file, limits, data, image, year, jday, time, nlon, $
   nlat, lons, lats, info, /just_header, status= status, $
   version= version, area_file= area_file

  IF version EQ 0 THEN BEGIN 
    str =  "Antiquated Grid File !!! " + lf + $
     " This Grid file doesn't have then name of the input AREA file in it." + lf + $
     " We can't continue without this information " + lf + $
     " You'll have to do this one by hand or pick another grid file! " + lf + $
     " Sorry " + lf
    message,' str, /info
    CW_WARN, str, "Wrong grid file Version !!!"

  ENDIF ELSE BEGIN 
    DATA= 0                     ; just in case it didn't stop
    image =  0                  ;

    IF status NE 1 THEN BEGIN
      str =  ' Error reading goes file ' + s.goesfile
      message, str, /info
      CW_WARN, str, "Error reading Goes file File!!!"
      str =  " Couldn't read goes file " + s.goesfile + lf
      str =  str + ' Pick Another !'
      WIDGET_CONTROL, s.ids.statusid, get_uval = v
      v =  [str, v ]
      WIDGET_CONTROL, s.ids.statusid, set_value= v
      WIDGET_CONTROL, s.ids.statusid, set_uvalue= v
    ENDIF ELSE BEGIN 

      IF year EQ 0 THEN BEGIN
        ; older version of the grid file don't
        ; have the year info in them.
        spawn,'date',ret_str
        tmp =  str_sep( ret_str(0), ' ' )
        year =  tmp( n_elements(tmp)-1 )
      ENDIF 
      hhmm =  strtrim( time, 2 )
      l =  strlen( hhmm )
      CASE l OF 
        4: hhmm =  hhmm
        3: hhmm =  '0' + hhmm
        2: hhmm =  '00' + hhmm
        ELSE : stop
      ENDCASE
       hh =  strmid( hhmm,0,2)

       tt =  doy2date( year, jday  )
       month =  strtrim(tt(0) ,2 )
       IF strlen( month ) EQ 1 THEN  month= '0' + month
       dom =  strtrim( tt(1), 2 )
       IF strlen( dom ) EQ 1 THEN dom =  '0' + dom
       s.datetime = month+dom+hh
       s.areafile =  area_file
       jday =  strtrim( jday,2 )
       doy =  fix( strmid( jday, 2, 3 ) )
       hh =  strtrin( fix( strmid( jday, 5,2 ) ), 2 )
       tt =  doy2date( year, doy )
       month =  strtrim(tt(0) ,2 )
       IF strlen( month ) EQ 1 THEN  month= '0' + month
       dom =  strtrim( tt(1), 2 )
       IF strlen( dom ) EQ 1 THEN dom =  '0' + dom
       datetime = month+dom+year+ hhmm
    ENDELSE 
  ENDELSE 
ENDIF ELSE goes_file =  ''

; define state array
cwgoflags =  { cwgoflags ,$
               flag1 :0 }


cwgoids =  { cwgoids, $
             drawid : 0l ,$ ; widget id
             windowid: 0l,$ ; wset arg
             dataid : 0l, $
             configid : 0l ,$
             filemenuid : 0l ,$
             run_overlayid : 0l ,$
             zoomid : 0l ,$
             rungridid : 0l ,$
             ovlyzid : 0l ,$
             quitid :0l,$
             statusid : 0L }

cwgoz =  { cwgoz, $
           zoom : intarr(4) ,$
           file : '' ,$
           ofile : '' }

cwgostate =  { cwgostate                   ,$
       areafile  : ''              ,$ ; area file for current goes file
       goesfile  : ''              ,$ ; current goes file
       goespath  : ''              ,$ ; 
       goesfilter: ''              ,$ ; 
       datetime  : ''              ,$ ; datetime of goes file.
       windpath  : ''              ,$ ;
       windfilter: ''              ,$ ;
       outpath   : ''              ,$
       windfiles : strarr(20)      ,$ ; list of wind files 
       minpix    : 0L              ,$ ; returned from first call to goes_overlay
       zoomix    : 0L              ,$ ; index into zooms
       l2        : 0l              ,$ ; if level2 file, l2=1
       gimage    : bytarr(1280,1028) ,$ ; goes image
       zooms     : replicate( cwgoz, 10 ) ,$ ; zooms
       ids       : { cwgoids }       }  ; ids for sub widgets.
;       flags     : { flags }         }


cwgostate.goesfilter = goes_filter
cwgostate.goespath = goes_path
cwgostate.goesfile =  goes_file

cwgostate.windfilter = wind_filter
cwgostate.windpath = wind_path

cwgostate.outpath =  output_path
cwgostate.minpix =  min_pix
cwgostate.areafile =  area_file

cwgodata =  { cwgodata, u:0., v:0., lon:0., lat:0. }

cwgostate.l2 =  keyword_set(l2)
CWGO = WIDGET_BASE(GROUP_LEADER=Group, $
    TITLE='Goes Cloud Data/Wind Data Browser', $
    UVALUE='CWGO')

stateid =  widget_base( cwgo )
wdataid =  widget_base( stateid, $
                      ROW=3, $
                      MAP=1 )

MenuDesc122 = [ $
    { CW_PDMENU_S,    3, 'Pick Files' }, $   ; 
      { CW_PDMENU_S,  0, 'Goes File' }, $    ; 1
      { CW_PDMENU_S,  2, 'Wind Files(s)' } $ ; 2

]

configid = WIDGET_BUTTON( WDATAID, $
    UVALUE='Config', $
    VALUE='Configure')


FILEMENUid = CW_PDMENU( WDATAID, MenuDesc122,$
    UVALUE='PICK_FILES', /RETURN_INDEX)

run_overlayid = WIDGET_BUTTON( WDATAID, $
    UVALUE='RUN_OVERLAY', $
    VALUE='Run Overlay')

zoomid = WIDGET_BUTTON( WDATAID, $
    UVALUE='ZOOM', $
    VALUE='Zooms')

RUNGRIDID = WIDGET_BUTTON( WDATAID, $
    UVALUE='RUN_GRIDDER', $
    VALUE='Run Gridder')

ovlyzid = WIDGET_BUTTON( WDATAID, $
    UVALUE='OVERLAY_ZOOMS', $
    VALUE='Overlay Zooms')

QUITID = WIDGET_BUTTON( WDATAID, $
    UVALUE='QUIT', $
    VALUE='Quit')

DRAWID = WIDGET_DRAW( WDATAID, $
    BUTTON_EVENTS=1, $
    MOTION_EVENTS=1, $
    RETAIN=2, $
    UVALUE='DRAW', $
    XSIZ=1280, $
    YSIZ=1024 ,$
    X_SCROLL_SIZE=640, $
    Y_SCROLL_SIZE=480 )

LABELID = WIDGET_LABEL( WDATAID, $
    UVALUE='STATUS_LAB', $
    VALUE='Status')

Status_text = [ $
  'Waiting to Pick Files' ]
STATUSID = WIDGET_TEXT( WDATAID,VALUE=Status_text, $
    FRAME=2, $
    UVALUE='Status_text', $
    XSIZE=100, $
    YSIZE=4, /scroll, /no_newline )

WIDGET_CONTROL, cwgo, /REALIZE

  ; Get drawable window index

COMMON DRAW_Comm, DRAW_Id
WIDGET_CONTROL, DRAWid, GET_VALUE=DRAW_Id

cwgostate.ids.dataid        = wdataid
cwgostate.ids.configid = configid
cwgostate.ids.filemenuid    = filemenuid
cwgostate.ids.run_overlayid = run_overlayid
cwgostate.ids.ovlyzid       = ovlyzid                        
cwgostate.ids.zoomid        = zoomid                         
cwgostate.ids.rungridid     = rungridid                      
cwgostate.ids.quitid        = quitid                         
cwgostate.ids.windowid      = draw_id ; window id arg to wset.
cwgostate.ids.drawid        = drawid ; widget id             
cwgostate.ids.statusid      = statusid                       

WIDGET_CONTROL, stateid, set_uvalu= cwgostate
WIDGET_CONTROL, wdataid, set_uvalue= cwgodata

XMANAGER, 'CWGO', CWGO
END

