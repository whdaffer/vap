;+
; NAME:  GetInterpFiles
; $Id$
; PURPOSE:  
;
;     Finds the interpolated wind files between date_time
;     -interp_time_inc and date_time. If there aren't any, make one
;     where the input data used in the interpolated wind field starts
;     at date_time-time_inc and goes to date_time.
;
;     It returns the file(s) sorted from minimum difference to maximum
;     difference.
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  Qscat Vap Processing
;
;
;
; CALLING SEQUENCE:  
;
;      interpfiles = GetInterpFiles(date_time, $
;                                   time_inc        = time_inc,$
;                                   interp_time_inc = interp_time_inc,$
;                                   interp_path     = interp_path, $
;                                   wpath           = wpath,$       
;                                   nscat           = nscat, $      
;                                   Min_Nvect       = min_Nvect, $
;                                   filetimes       = filetimes, $  
;                                   decimate        = decimate, $         
;                                   CRDecimate      = CRDecimate,$      
;                                   ExcludeCols     = ExcludeCols,$  
;                                   count           = count)        
;
;
; 
; INPUTS:  
;
;   Date_time: String, Vaptime (yyyy/mm/dd/hh/mi)
;
;
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:  
;
;     time_inc        : (I) Number of hours to go back from 'date_time' in
;                       search of wind files (def=26)
;     interp_time_inc : (I) Number of hours to go backward from date_time
;                       to search for interpolated wind field files (def=2)
;     interp_path     : (I) string, path in which to search for interpolated wind
;                       field files (Def=$VAP_ANIM)
;     wpath           : (I), string, Path in which to search for wind files
;                       (Def=$VAP_WINDS)
;     decimate        : (I) scalar, decimate=n means take  every n-th vector
;     CRDecimate      : (I) 2-vector: CRDecimate=[p,q] means take
;                       every p-th column of every q-th row 
;     ExcludeCols     : (I) string, ExcludeCols='0,38:42,75' means
;                       exclude cols 0, 38,39,40,41,42 and  75 from use when calculating
;                       interpolated field
;     nscat           : (I), Flag, if set, expect Nscat data/wind
;                       field files
;     Min_Nvect       : (I) scalar, Don't make interp file if there are less 
;                       than this number of vectors.
;
;     filetimes       : (O) returned array of IDLDT time structures
;                       for returned wind field files
;     count           : (O) number of interpolated wind field files found.
;
;
;    
;
;
;
; OUTPUTS:  
;
;  InterpFiles: List of filename(s) sorted from minimum difference to
;               maximum, if there are more than one.
;
;
;
; OPTIONAL OUTPUTS:  
;
;     count: see above
;     filetimes: see above
;
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  None
;
;
;
; RESTRICTIONS:  None
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
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.7  2000/02/14 21:17:27  vapuser
; Changed 'findfile' to 'spawn' with a 'find'
;
; Revision 1.6  1999/10/05 17:17:55  vapuser
; Changed default Wind file time range from 26 to 14. Take abs of
; differences. This way, the routine can look forward in time, too.
;
; Revision 1.5  1999/04/09 15:37:11  vapuser
; *** empty log message ***
;
; Revision 1.4  1998/11/25 22:39:49  vapuser
; Squashed some bugs.
;
; Revision 1.3  1998/11/23 21:40:17  vapuser
; Corrected a mispelling
;
; Revision 1.2  1998/10/22 21:19:24  vapuser
; Added/Changed some comments, squashed bugs.
;
; Revision 1.1  1998/10/17 00:21:39  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION GetInterpFiles,date_time, $ ; VapTime yyyy/mm/dd/hh/mi, the 
                                     ; End of the timerange in question
   time_inc = time_inc,$        ; Hours to subtract from date_time to 
                                ; establish time range, if it is
                                ; required to create an interp file. 
                                ; default=14.
   interp_time_inc = interp_time_inc, $ ; Amount of time back from date_time 
                                ; to look for interp files. Default=2.
   Wpath = Wpath,$              ; Path to wind files
   Interp_path = Interp_path, $ ; Path to the interpolated files
   decimate=decimate, $         ; (I) scalar, decimate=n means take 
                                ; every n-th vector
   CRDecimate=CRDecimate,$      ; (I) 2-vector: CRDecimate=[p,q] means
                                ; take every p-th column of every q-th
                                ; row
   ExcludeCols = ExcludeCols,$  ; (I) string, ExcludeCols='0,38:42,75' means
                                ; exclude cols 0, 38,39,40,41,42 and
                                ; 75 from use when calculating
                                ; interpolated field
   Nscat = nscat, $             ; Flag, if set, expect NSCAT interp files and 
                                ; NSCAT data files
   Min_Nvect = Min_Nvect, $     ; Don't make interp file if there are less 
                                ; than this number of vectors.
   filetimes=filetimes, $       ; Array of IDLDT containing the times 
                                ; of the interp files found
   count=count                  ; Number of interp files found.


  rcsid = "$Id$"
  cd, current=savedir
  Interp_Files = ''
  lf =string(10b)
  count = 0
  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_State.msg,/cont
    cd,savedir
    count = 0
    return,''
  END

  IF n_Elements(date_time) EQ 0 THEN date_time = TodayAsString(sep='/');
  IF n_elements(time_inc) EQ 0 THEN time_inc = 14.
  IF N_Elements(interp_time_inc) EQ 0 THEN interp_time_inc = 2.
  IF n_elements(Interp_Path) EQ 0 THEN Interp_Path = '$VAP_ANIM'

  tdate_time = regularizeVapTime(date_time, /max)

  IF rstrpos(Interp_Path,'/') NE strlen(Interp_Path)-1 THEN $
     Interp_Path = Interp_Path + '/'
  IF n_Elements(Wpath) EQ 0 THEN Wpath = GetEnv('VAP_WINDS')
  IF rstrpos(WPath,'/') NE strlen(WPath)-1 THEN $
     WPath = WPath + '/'

  nscat = keyword_set(nscat)
 
  interp_filter = 'QIF-*.hdf'
  windfile_filter = 'Q*'
  IF nscat THEN BEGIN 
    interp_filter = 'NIF-*.hdf'
    windfile_filter = 'N*'    
  ENDIF 
  ;interp_files = findfile(Interp_Path+interp_filter,count=cnt)
  spawn,'find ' + Interp_Path + ' -name "' + interp_filter + '" -print ', interp_files
  good = where(strlen(interp_files) GT 0,cnt)
  interp_files = interp_files[good]
  make_interp_file =  0 

  IF cnt NE 0 THEN BEGIN 

    junk = (rstrpos( interp_files[0], '/'))[0] + 1
    len = strlen(interp_files[0])
    basenames = strmid(interp_files, junk, len-junk)
    filetimes = ifnames2dt(basenames)
    tmp = str_sep( tdate_time, '/')
    testtime = var_to_dt( fix(tmp[0]), fix(tmp[1]), fix(tmp[2]), $
                          fix(tmp[3]), fix(tmp[4]) )

    test = abs(testtime.julian-filetimes.julian)*24.d
    x = where( test LE interp_time_inc, nx )
    IF nx NE 0 THEN BEGIN 
      diffs =  testtime.julian-filetimes[x].julian
      s = sort(abs(diffs))
      filetimes = filetimes[x[s]]
      interp_files = interp_files[x[s]]
      count = nx
      return, interp_files
    ENDIF ELSE make_interp_file = 1
  ENDIF ELSE make_interp_file = 1

  IF make_interp_file THEN BEGIN 
    Message,'Either no interpolated wind field files present in ' + interp_path + lf + $
     ' or none in time range' ,/cont
    print,'   Making new interp file '
    cd,interp_path
    field = MakeInterpFile( tdate_time,time_inc, $
                            wpath = wpath, $
                            filter = windfile_filter,$
                            min_nvect=Min_Nvect, $
                            decimate=decimate, $
                            CRDecimate=CRDecimate, $
                            ExcludeCols=ExcludeCols, $
                            nscat = nscat, $
                            Outfile = OutFile )
    ndim = size(field,/n_dim)
    IF ndim EQ 2 OR $
       isa(outfile,/string,/nonempty) THEN BEGIN 
        ; 01234567890123456789
        ; XIF-yyyymmddhhmi.hdf
      Interp_files = Outfile
      filetimes = ifnames2dt(Interp_files)
      count = 1
    ENDIF ELSE BEGIN 
      Message,"ERROR: Can't make interp field!",/cont
      interp_files = ''
    ENDELSE 
    cd,savedir
  ENDIF 

  return,interp_Files
END
