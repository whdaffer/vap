;+
;
; NAME: CLOUD_OVERLAY
; $Id$
;
;
;
;
; PURPOSE:  This procedure is meant to be called in batch mode to make
;          an overlay of Qscat/SeaWinds data on a Goes (or perhaps in
;          the future, GMS) file.
; 
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY: Plotting, image
;
;
;
; CALLING SEQUENCE: 
;
;       CLOUD_OVERLAY, cloud_file,
;                      date_time,     $ ; time of GOES data used in overlay
;                                       ; (def=current time - 3 hours)
;                                       ; ((yy)yy/mm/dd/hh) If the year field 
;                                       ; is 2 characters, then 1900 will be
;                                       ; added to it.
;                      time_inc,      $ ; select wind files this number 
;                                       ; of hours +/- time given 
;                                       ; in date_time. def=3
;                      wpath = wpath, $ ; path to wind files (def=$VAP_WINDS)
;                      overlay_path = overlay_path,$ ; path to output overlay file
;                                       ; def = $VAP_ROOT/overlay
;                      decimate=decimate,$ ; (I), scalar, decimate=n
;                                          ; means take every n-th vector
;                      CRDecimate=CRDecimate,$ ; (I), 2-vector,
;                                              ; CRDecimate=[p,q
;                                              ; means take every p-th
;                                              ; column of every q-th row
;                      ExcludeCols=ExcludeCols,$ ; (I) string,
;                                             ; excludeCols='0,38:40,75'
;                                             ; means exclude columns 
;                                             ; 0, 38,39,40 and 75.
;                      ps =  ps         ; make a postscript instead of a gif
;
;
;
; 
; INPUTS: 
;
;        Cloud_file - A gridded file (i.e. one created by the compiled
;                     program 'grid_goes')
;        date_time  - The date/ time around which to retrieve
;                     wind files overplotted on the cloud data.
;                     default=current_time-3hours
;
;
; OPTIONAL INPUTS: 
;
;        time_inc   - Select wind files within this time of
;                     'date_time' default=+/- 3 hours
;
;
;	
; KEYWORD PARAMETERS: 
;
;        wpath       - Path to wind files
;                      Default=$VAP_WINDS
;        overlay_path- path to output overlay files,
;                     Default=$VAP_ROOT/overlay/
;        Decimate    - (I) scalar, decimate=n means take
;                      every n-th vector. Default=1, take every
;                      vector. Decimate is ignored if CRDecimate is
;                      present.
;        CRDecimate  - (I), 2-vector, CRDecimate=[p,q] means take
;                      every p-th column from every q-th row.
;                      Default=[1,1] meaning, take every vector.
;        ExcludeCols - (I) string, ExcludeCols='0,38:40,75' means
;                      exclude columns 0, 38,39,40 and 75
;        ps          - Make Postscript file instead of gif.
;        pid         - used with cronjobs
;
;
; OUTPUTS: 
;
;
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS: 
;
;
;
; RESTRICTIONS: 
;
;
;
; PROCEDURE: 
;
;
;
; EXAMPLE: 
;
;
; $Id$
;
; Modification History:
;
; $Log$
; Revision 1.5  1998/11/20 19:59:47  vapuser
; Incorporated goes_overlay24, making whatever other changes
; were required
;
; Revision 1.4  1998/10/17 00:15:10  vapuser
; Added CRDecimate, ExcludeCols, decimate keywords.
; Killed a few bugs
;
; Revision 1.3  1998/10/06 00:21:57  vapuser
; Added DeEnvVar
;
; Revision 1.2  1998/09/09 17:49:41  vapuser
; Just added some RCS Header Macros
;
;
;-

PRO cloud_overlay, cloud_file,     $ ; full name of grid file
                      date_time,     $ ; time of GOES data used in overlay
                                       ; (def=current time - 3 hours)
                                       ; ((yy)yy/mm/dd/hh) If the year field 
                                       ; is 2 characters, then 1900 will be
                                       ; added to it.
                      time_inc,      $ ; select wind files this number 
                                       ; of hours +/- time given 
                                       ; in date_time. def=3
                      wpath = wpath, $ ; path to wind files (def=$VAP_WINDS)
                      overlay_path = overlay_path,$ ; path to output overlay file
                                       ; def = $VAP_OVERLAY
                      decimate=decimate,$ ; (I), scalar, decimate=n
                                          ; means take every n-th vector
                      CRDecimate=CRDecimate,$ ; (I), 2-vector,
                                              ; CRDecimate=[p,q
                                              ; means take every p-th
                                              ; column of every q-th row
                      ExcludeCols=ExcludeCols,$ ; (I) string,
                                             ; excludeCols='0,38:40,75'
                                             ; means exclude columns 
                                             ; 0, 38,39,40 and 75.

                      ps =  ps,$       ; make a postscript file.
                      gif=gif,$        ; Make gif file
                      pid=pid          ; Used with cron jobs
                      


 Rcs_id = "$Id$";

  auto_cloud_overlay = keyword_set(pid) ; flag for cronjob runs.
  user = getenv('USER')
  IF (user NE "" ) AND auto_cloud_overlay THEN BEGIN 
    lockfile = (findfile('/tmp/' + user + '.cloud_overlay.lock', count=n))(0)
    IF n NE 0 THEN BEGIN 
      openr, lun, lockfile, /get, error=err
      IF err ne 0 THEN BEGIN 
        Message,!error_state.msg,/cont
        return
      ENDIF 
      ppid = 0L
      readf, lun, ppid
      free_lun, lun
      IF ppid ne pid THEN auto_cloud_overlay = 0 ; not our lock file.
    ENDIF ELSE BEGIN 
      Message,'ERROR: No lock file!',/cont
      return
    ENDELSE 
  ENDIF 

  catch, error_status
  IF error_status NE 0 THEN BEGIN
    IF auto_cloud_overlay THEN $
     IF exist(llun) THEN printf, llun, 'ERROR: ' + !err_string
    message, !err_string,/cont
    return
  ENDIF 

  IF !version.release LT 5.1 THEN BEGIN 
    Message,'This software requires idl 5.1',/cont
    return
  ENDIF 

  IF n_params() NE  2 THEN BEGIN 
    message,' Both paramters (GOES_FILE & DATE_TIME) are required ',/cont
    return
  ENDIF 


  ps =  keyword_set( ps );
  gif = keyword_set(gif)
  jpeg = (gif OR ps ) EQ 0;

  CASE 1 OF 
    ps: OutputType = 'Postscript'
    gif: OutputType =  'Gif'
    Jpeg: OutputType =  'Jpeg'
    ELSE:
  ENDCASE
  Message,'File will be output as ' + OutputType,/info

  IF N_elements( time_inc ) EQ 0 THEN time_inc = 3
  IF n_elements( wpath ) EQ 0 THEN wpath =  '$VAP_WINDS'
  IF n_elements( overlay_path ) EQ 0 THEN overlay_path =  '$VAP_OVERLAY'

  IF n_elements(CRDecimate) NE 2 THEN BEGIN 
    IF n_elements(decimate) EQ 0 THEN CRDecimate = [1,1]
  ENDIF 

  cd,current=cur_dir



  IF auto_cloud_overlay THEN BEGIN 
    openw, llun, lockfile, /get, error= err
    IF err NE 0 THEN BEGIN 
      message,!err_string,/cont
      return
   ENDIF 
  ENDIF 

  CASE 1 OF 
    strpos( cloud_file, 'GOES' ) NE -1: grid_type = 'GOES'
    strpos( cloud_file, 'GMS' ) NE -1: BEGIN 
      grid_type = 'GMS'
      str =  'ERROR: GMS not implemented'
      Message,str,/cont
      IF auto_cloud_overlay THEN BEGIN 
        printf, llun, str
        free_lun, llun
      ENDIF 
      return
    end
    ELSE: BEGIN 
      str =  'ERROR: Unknown Grid type in file ' + cloud_file
      Message,str,/cont
      IF auto_cloud_overlay THEN BEGIN 
        printf, llun, str
        free_lun, llun
      ENDIF 
      return
    END 
  ENDCASE 

  print,'$VAP_ROOT=',getenv('VAP_ROOT')
  overlay_path = DeEnvVar(overlay_path)
  print,' overlay_path = ',overlay_path



    ; CD to the directory where the overlay will reside.
  CD,overlay_path

  openr,rlun, cloud_file, /get_lun, error= err
  IF err NE 0 THEN BEGIN
    message,!err_string,/cont
    IF auto_cloud_overlay THEN $
     printf, llun,' ERROR: ' + !err_string
    return
  ENDIF 
  free_lun,rlun

  str =  ' Taking wind data from   ' + wpath
  message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun, "INFO: " + str
  str =  ' Putting output in       ' + overlay_path
  Message,str ,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str
  str = ' Using time increment of ' + strtrim( time_inc, 2 ) + ' hours'
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str
  str = ' Using date_time of      ' + date_time
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str


  wf = GetWindFiles( date_time, delta=time_inc, path= wpath, filter='Q*')
  nf = n_elements(wf)

  
  IF strlen(wf[0]) EQ 0 AND nf EQ 1 THEN BEGIN 
    str = 'ERROR: No wind files in dir ' + WPATH
    Message,str,/cont
    IF auto_cloud_overlay THEN BEGIN 
      printf,llun,str
      free_lun,llun
    ENDIF 
    return 
  ENDIF ELSE BEGIN

      ; Get the visual name, it determines which 
      ; version of goes_overlay to call.
    Device,Get_Visual_Name= visual
    visual = strupcase(visual)
    

    str = 'INFO: Found ' + strtrim(nf,2) + ' wind files'
    Message,str,/info
    IF auto_cloud_overlay THEN $
      printf,llun,"INFO: " + str
    FOR ff=0,nf-1 DO BEGIN 
      str = 'INFO: ' + wf[ff]
      Message,str,/info
      IF auto_cloud_overlay THEN $
        printf,llun,"INFO: " + str
    ENDFOR 
          

    CASE grid_type OF 
      'GOES': BEGIN 
        IF visual EQ 'PSEUDOCOLOR' THEN BEGIN 
          GOES_OVERLAY, cloud_file, wfiles=wf, $
           minspeed=2, maxspeed=20, $
            len=2,getoutfile=ofile, $
             Decimate=decimate, CRDecimate=CRDecimate, $
              ExcludeCols=ExcludeCols, ps=ps, gif=gif,/z
        ENDIF ELSE BEGIN 
          GOES_OVERLAY24,cloud_file,windFiles=wf,$
           minspeed=2, maxspeed=20, $
            len=2,outfile=ofile, $
             Decimate=decimate, CRDecimate=CRDecimate, $
              ExcludeCols=ExcludeCols, ps=ps, gif=gif, jpeg=jpeg
        ENDELSE 
      END
       'GMS' : BEGIN 
         str = 'ERROR: GMS not implemented yet'
        Message,str,/cont
        IF auto_cloud_overlay THEN BEGIN 
          printf,llun,str
          free_lun,llun
       ENDIF 
       return
      END
    ENDCASE 
    IF auto_cloud_overlay THEN BEGIN 
      openw, wlun, '/tmp/auto_cloud_overlay_output_file',/get,error=err
      IF err NE 0 THEN BEGIN
        str =  'ERROR: ' + !err_string
        printf,llun,str
        Message,str,/cont
        RETURN
      ENDIF 
      Printf, wlun, ofile
      free_lun, wlun
    ENDIF 
  ENDELSE  
  IF exist( llun ) THEN   free_lun,llun
  cd,cur_dir

END 

