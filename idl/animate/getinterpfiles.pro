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
; CATEGORY:  SeaWinds Vap Processing
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
;                                   interpfilter    = interpfilter,$    
;                                   windfilefilter  = windfilefilter,$    
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
;                       to search for interpolated wind field files (def=3)

;     interp_path     : (I) string(s), path in which to search for interpolated wind
;                       field files (Def=$VAP_OPS_ANIM)
;                        It is possible that this could be an array,
;                        in which case it will be interated over using
;                        the filters given in `interpfilter' and the
;                        results concatenated. The caller is
;                        responsible for arranging the output to
;                        his/her desires.
;              
;     wpath           : (I), string(s), Path in which to search for wind files
;                       (Def=$VAP_DATA_TOP)
;                        It is possible that this could be an array,
;                        in which case case it will be interated over
;                        using the filters given in windfilefilter and
;                        the results concatenated. The caller is
;                        responsible for arranging the output to
;                        his/her desires.
;     decimate        : (I) scalar, decimate=n means take  every n-th vector
;     CRDecimate      : (I) 2-vector: CRDecimate=[p,q] means take
;                       every p-th column of every q-th row 
;     ExcludeCols     : (I) string, ExcludeCols='0,38:42,75' means
;                       exclude cols 0, 38,39,40,41,42 and  75 from use when calculating
;                       interpolated field
;     interpfilter    : (I), scalar string or array of strings.
;                       The filter to use in finding the interp
;                       files. vfindfile is used to 
;                       to find the files using this filter. If the
;                       variable is an array, it will be interated
;                       over and the results concatenated. The caller
;                       should check the arrangement of files, no
;                       promises are made on this end.
;     windfilter      : Same as interpfile filter, but this one is
;                       used to find the wind files. Needs to be
;                       the sort of filter getwindfiles() can use.
;
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
; Revision 1.13  2002/08/12 22:55:51  vapdev
; Fixed a few stupid bugs
;
; Revision 1.12  2002/05/08 16:01:09  vapdev
; Removed variables that will now be defined in environment.
; Changed email addresses
; Changed ENV variables to reflect new scenario
;
; Revision 1.11  2002/05/03 01:06:25  vapdev
; Changes environmental variables to reflect new vapdev/vaprun env variables.
; Also made sure that all the various env variable routines were being
; called correctly.
;
; Revision 1.10  2001/12/10 23:31:25  vapdev
; replace obsolete RSI routines
;
; Revision 1.9  2001/12/08 00:02:35  vapdev
; Getting rid of obsolete RSI routines and fixing ENV vars
;
; Revision 1.8  2000/03/08 21:50:52  vapuser
; Added some error checking code
;
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
                                ; to look for interp files. Default=3.
   Wpath = Wpath,$              ; Path(s) to wind files 
   Interp_path = Interp_path, $ ; Path(s) to the interpolated files
   decimate=decimate, $         ; (I) scalar, decimate=n means take 
                                ; every n-th vector
   CRDecimate=CRDecimate,$      ; (I) 2-vector: CRDecimate=[p,q] means
                                ; take every p-th column of every q-th
                                ; row
   ExcludeCols = ExcludeCols,$  ; (I) string, ExcludeCols='0,38:42,75' means
                                ; exclude cols 0, 38,39,40,41,42 and
                                ; 75 from use when calculating
                                ; interpolated field

   interpfilter =  interpfilter, $ ; (I) scalar string or array of strings. 
                                   ; Filter(s) to be used when
                                   ; searching for interpfiles.


   windfilter=windfilter, $ ; (I), same as interpfilter, but for 
                                    ; wind files.

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
  IF N_Elements(interp_time_inc) EQ 0 THEN interp_time_inc = 3.
  IF n_elements(Interp_Path) EQ 0 THEN Interp_Path = '$VAP_OPS_ANIM'

  tdate_time = regularizeVapTime(date_time, /max)

  FOR i=0,n_elements(interp_path)-1 DO BEGIN 
    IF  strpos(Interp_Path[i],'/',/reverse_search)  NE strlen(Interp_Path[i])-1 THEN $
       Interp_Path[I] = Interp_Path[I] + '/'
  ENDFOR 
  IF n_Elements(Wpath) EQ 0 THEN Wpath = GetEnv('VAP_DATA_TOP')

  FOR i=0,n_elements(wpath)-1 DO BEGIN 
    IF  strpos(Wpath[I],'/',/reverse_search)  NE strlen(Wpath[I])-1 THEN $
     Wpath[I] = Wpath[I] + '/'
  ENDFOR 

  IF n_elements(interp_filter) EQ 0 THEN interp_filter = '*IF-*.hdf'
  IF n_elements(windfilter) EQ 0 THEN windfilter = '{QS,SW}*'

  FOR p=0,n_elements(interp_path)-1 DO BEGIN 
    FOR i=0,n_elements(interp_filter)-1 DO BEGIN 
      tmpfiles = vfindfile(interp_filter[i],interp_path[p],count=count)
      IF count NE 0 THEN BEGIN 
        tmpfiles = interp_path + tmpfiles;
        IF n_elements(interp_files) EQ 0 THEN $
           interp_files = tmpfiles ELSE $
           interp_files = [interp_files, tmpfiles]
        tmpfiles = 0
      ENDIF 
    ENDFOR 
  ENDFOR 

  good = where(strlen(interp_files) GT 0,cnt)

  IF cnt NE 0 THEN BEGIN 

    interp_files = interp_files[good]
    make_interp_file =  0 

    junk = ( strpos( interp_files[0], '/',/reverse_search) )[0] + 1
    len = strlen(interp_files[0])
    basenames = strmid(interp_files, junk, len-junk)
    filetimes = ifnames2dt(basenames)
    tmp =  strsplit(  tdate_time, '/',/extract) 
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
    IF n_elements(interp_path) GT 0 THEN $
       ipath = strjoin(interp_path,lf) ELSE $
       ipath = interp_path
    Message,'Either no interpolated wind field files present in ' + ipath + lf + $
     ' or none in time range' ,/cont
    print,'   Making new interp file '
    cd,interp_path
    field = MakeInterpFile( tdate_time,time_inc, $
                            wpath = wpath, $
                            filter = windfilter,$
                            min_nvect=Min_Nvect, $
                            decimate=decimate, $
                            CRDecimate=CRDecimate, $
                            ExcludeCols=ExcludeCols, $
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
