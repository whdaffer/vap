;+
; NAME:  MakeInterpFile
; $Id$
; PURPOSE:  Creates an interpolated wind field and writes it out to a
;          file. Also returns it as the result of the function
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  NSCAT/Qscat/Seawinds processing
;
;
;
; CALLING SEQUENCE:  
;
;          field=MakeInterpFile( [ [date_time , time_inc, |
;                                   Wfiles=Wfiles],OutFile=OutFile,$
;                                   Wpath=Wpath, Filter=Filter,$
;                                   Lonpar=lonpar, Latpar=Latpar,
;                                   Rainf=Rainf, Ermax=Ermax, $
;                                   min_Nvect=min_Nvect,$
;                                   Decimate=Decimate,$
;                                   CRDecimate=CRDecimate,$
;                                   ExcludeCols=ExcludeCols,$
;                                   Nscat=Nscat, noFile=nofile )
;
;
; 
; INPUTS:  All inputs, parameter and keyword, are optional.
;
;
;
; OPTIONAL INPUTS:  
;
;     date_time - Wind files with an start time after this time won't
;                 be used in the interpolated field.
;     time_inc - start_time = date_time-time_inc. Default=26 hours
;
;
;
;
;	
; KEYWORD PARAMETERS:  
;
;   Wfiles: (I) Vector of fully qualified file names. If this keyword is
;           set, parameters 'date_time' and 'time_inc' are ignored.
;
;   OutFile: (I) if this keyword is a string, it is taken to be the fully qualified
;              output file name. If it is not set, and the 'NoFile'
;              flag is also clear,  then the field will be
;              output to a file with the standard name formula
;              (i.e. QIF-yyyymmddhh.hdf) Q=Qscat,IF=Interpolated
;              field. yyyymmddhh is the end time of the field. 
;
;
;              See 'file format' below for the format of the file.
;
;   Nofile:(I) flag, if set, no output file will be written.
;
;   WPath: (I) string, path to wind files (def=$VAP_WINDS)
;   Filter: (I) string, filter you use when finding wind files.
;   LonPar: (I/O) if set on input to a 3-vector (n,min,max), these parameters are
;                 used in creating the interpolated field. If only
;                 present as an output variable, these parameters are
;                 defaulted and lonpar is set to them on output.
;
;   LatPar: (I/O) Same as for LonPar, but for Latitude.
;   RaInf: (I) Radius of Influence. 4-vector, Radius of
;              influence. This is the number of grid cells to consider
;              when computing a value for the current grid
;              cell. Default = [12., 10., 6,   4 ] 
;
;   ErMax: (I) 4-vector, Maximum Error allowed before the data in the data
;              field is discarded in favor of the computed model
;              default = [50., 20., 10., 5.];
;
;   Min_Nvect - (I) scalar: Don't make interp file if there are less 
;               this number of vectors. (default=0, i.e. make it regardless)
;   Decimate - (I) scalar: take every nth vector, i.e. 2=> take
;                  every other, 3=> take every 3rd. (Defaults to 1,
;                  meaning take evey vector)
;
;   CRDecimate - (I) 2-vector: 1st entry specifies column, 2nd the
;                   row. e.g. [2,3] means take every other column, every
;                   3rd row. (defaults to [1,1] meaning take every
;                   vector)
;
;   ExcludeCols - (I) string: comma seperated list of individual columns
;                   or ranges of colums to exclude. Ranges are indicated
;                   by start:stop (e.g. 33:42 will exclude columns 33
;                   through 42, inclusive). Ex: 0,40:42,72 will exclude
;                   columns 0, 40,41,42 and 72.  (Defaults to '' meaning
;                   exclude NO columns)
;

;   StartTime: (O) - the Earliest time which appears in the Wind
;                    Files.
;   EndTime: (O) - the Latesest time which appears in the Wind
;                  Files.
;   OutFile: (I/O) - If set on input, this will be the name of the
;                  output file. If present as a return argument, it
;                  will return the name of the output file.
;   Nscat: (I) flag. If set, expect Nscat data.
;
;
; FILE FORMAT:
;
;     The output is an HDF file consisting of the following
;     quantities.
;
;              Global Attributes:
;
;                 LongName     : QSCAT_VAP_SUCCOR_INTERP_FIELD
;                 VersionID    : the rcsid of this routine.
;                 Creation_Time: The time this field was created.
;                 StartTime: Earliest Wind Data time included in this
;                            field. String 'yyyy/mm/dd/hh/mm'
;                 EndTime  : Latesest Wind Data time included in this
;                            field. String 'yyyy/mm/dd/hh/mm'
;                 LonPar:   a 3 vector. [minlon, maxlon,inc]
;                 LatPar:   a 3 vector. [minLat, maxLat,inc]
; 
;
;             
;              Science Data Sets:
;
;                U : A Nlon by Nlat float array, the U component of
;                     the field
;                V : A Nlon by Nlat float array, the V component of
;                     the field
;
;
;
; OUTPUTS:  
;
;   interp_field - returned as function result. [ [UI],[VI] ]
;                  If there has been a failure, a scalare 0 is
;                  returned.

; 

;
;
; OPTIONAL OUTPUTS:  
;
;     LonPar - a 3 vector, the same as is written to output file
;     LatPar - a 3 vector, the same as is written to output file
;     StartTime - a string, the same as is written to output ifle
;     EndTime - a string, the same as is written to output ifle
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  None
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
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.5  1998/11/23 21:39:48  vapuser
; Changed meaning of min_Nvect (-1 .vs. 0 for 'make regardless')
; Corrected an endif/endelse bug
;
; Revision 1.4  1998/10/17 00:24:05  vapuser
; Added min_Nvect to call and requisite code to
; use this quantity.
;
; Revision 1.3  1998/10/15 02:15:48  daffer
; Added Ofile keyword
;
; Revision 1.2  1998/10/07 18:30:54  vapuser
; Some Comment work
;
; Revision 1.1  1998/10/07 00:08:33  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION MakeInterpFile, date_time, $            ;((yy)yy/mm/dd/hh End time 
                                                 ; of interpolated field
                         time_inc  , $;          ; start_time=date_time-time_inc
                         Wpath     = Wpath    , $ ; path to wind files.
                         Filter    = Filter   , $ ; Filter to use in finding wind 
                                                  ; files.
                         Wfiles    = Wfiles   , $ ; (I/O) Wind files included in 
                                                  ; Interpolated
                                                  ; field. if this
                                                  ; keyword is set,
                                                  ; date_time and
                                                  ; time_inc are ignored.
                         Lonpar    = Lonpar   , $ ; (I/O) 3-vector, [min,max,inc]. 
                                                  ; If set on input, these 
                                                  ; numbers determine the field,
                                                  ; otherwise, they
                                                  ; are defaulted and
                                                  ; returned in this
                                                  ; keyword.


                         Latpar    = Latpar   , $ ; (I/O) Same as Lonpar, but 
                                                  ; for latitude.
                         RaInf     =  RaInf   , $ ; (I) Radius of Influence, 
                                                  ; a 4-vector, a
                                                  ; decreasing
                                                  ; sequence of floats
                                                  ; indicating the
                                                  ; radius of grid
                                                  ; cells to use in
                                                  ; the calculation of
                                                  ; current cell.
                         ErMax =  ErMax       , $ ; (I), Error Max. 4-vector (float)
                                                  ; Maximum Error
                                                  ; allowed before the
                                                  ; data field is discarded
                                                  ; in favor of the
                                                  ; computed model.
                                                  ; default = [50., 20, 10, 5];
                         StartTime = StartTime, $ ; (O) Returned earliest time 
                                                  ; in Wind Files
                         EndTime   = EndTime  , $ ; (O) Returned latest time 
                                                  ;  in Wind Files
                         Nscat     = Nscat, $     ; (I) flag, if set, expect 
                                                  ;  Nscat data
                         NoFile = NoFile, $       ; (I), flag, if set, don't write 
                                                  ; file.
                         OutFile = OutFile ,$         ; (I/O). If present on input, 
                                                  ; this will
                                                  ; be the name of the
                                                  ; output file. If
                                                  ; present as a
                                                  ; return argument,
                                                  ; the name of the
                                                  ; output file will
                                                  ; be returned in it.
                         min_Nvect = Min_Nvect,$  ; Don't make interp file if there
                                                  ; are less than this number 
                                                  ; of vectors.
                         Decimate = Decimate, $   ; See Explanation above
                         CRDecimate=CRDecimate,$  ; See Explanation above
                         ExcludeCols=ExcludeCols  ; See Explanation above


  rcsid = "$Id$"

  LongName = "QSCAT_VAP_SUCCOR_INTERP_FIELD"
  VersionID=rcsid
  CreationTime =  (idldt2Vaptime(today()))[0]

  Nscat = Keyword_set(Nscat)
  IF n_elements(Wfiles) eq 0 THEN BEGIN 
    IF n_Elements(date_time) EQ 0 THEN date_time = TodayAsString(sep='/')
    IF n_elements(time_inc) NE 0 THEN time_inc = 26
    IF n_elements(Wpath) EQ 0 THEN Wpath = getenv('VAP_WINDS')
    IF N_Elements(filter) EQ 0 THEN BEGIN 
      IF Nscat THEN filter = 'N*' ELSE filter = 'Q*'
    ENDIF  
     Wfiles = GetWindFiles(date_time,delta=time_inc,path=wpath, $
                           filter=filter, count=cnt, nscat=nscat)
  ENDIF 

  IF strlen( Wfiles[0] ) EQ 0 AND N_Elements(Wfiles) EQ 0 THEN BEGIN 
    Message,'Error in GetWindFiles',/cont
    return,0
  ENDIF 

  minnvect = 0
  IF n_elements(min_nvect) ne 0 THEN BEGIN 
    IF min_nvect NE 0 THEN minnvect = min_nvect
  ENDIF 


  data = Read_Wind_Files( Wfiles, nscat=nscat, $
                        Decimate=Decimate, $
                        CRDecimate=CRDecimate, $
                        ExcludeCols=ExcludeCols, $
                        StartTime=StartTime,$
                        EndTime=EndTime )

  IF n_Elements(data) GT 1 THEN BEGIN 

    U   = data[*,0]
    V   = data[*,1]
    Lon = data[*,2]
    Lat = data[*,3]

      ; check to see if there are enough vectors.
    IF n_elements(U) LT minNvect THEN BEGIN 
      nu =strtrim( n_elements(u),2)
      mnu = strtrim( minNVect,2 )
      Message,'Too Few vectors!',/cont
      print,'U has only ', nu, ' min NVect = ', mnu
      return,0
    ENDIF 

    IF N_elements(LonPar) NE 3  THEN LonPar =  [0.,359,1.]
    IF N_elements(LatPar) NE 3  THEN LatPar =  [-60.,60.,1.]
    IF N_elements(RaInf) NE 4 THEN Rainf = [12.,10,6,4]
    IF N_elements(ErMax) NE 4 THEN Ermax = [50.,20.,10,5]


  ENDIF ELSE BEGIN 
    Message,'Error in ReadWindFiles - Aborting',/cont
    return,0
  ENDELSE 

  status = RunSuccor( U,V,Lon,Lat,ui,vi,lonpar,latpar,$
                      rainf=rainf,ermax=ermax)
  IF status eq 0 THEN BEGIN 
    Message,'Bad return from 1st succor run',/cont
    return,0
  ENDIF 
   rainf = [10.,6.,3.,2]
   ermax = [10.,6.,3.,2]
   status = RunSuccor( U,V,Lon,Lat,ui,vi,lonpar,latpar,$
                       rainf=rainf,ermax=ermax,/reuse )
  IF status eq 0 THEN BEGIN 
    Message,'Bad return from 2nd succor run',/cont
    return,0
  ENDIF 

    ; Well, we've made it here, so the field must be okay. 



  IF NOT keyword_set( NoFile) THEN BEGIN 
    IF N_Elements(OutFile) eq 0 THEN BEGIN 
      cd,current=cur
      OutFile = cur + '/QIF-'
      tmp = str_sep( EndTime,'/' )
      nn = n_elements(tmp)
      FOR i=0,nn-1 DO OutFile = OutFile + tmp[i]
      OutFile = OutFile + '.hdf'
    ENDIF 

    OutFile = OutFile[0]
      ; Let's write it out.
    Message,' Writing file to ' + OutFile,/info
    s = qmodelhdfwrite( OutFile,Ui,Vi, lonpar=lonpar, latpar=Latpar, $
                        Version=Versionid, Longname=Longname, $
                        CreationTime=CreationTime, StartTime=StartTime, $
                        EndTime=EndTime )
    IF s NE 1 THEN $
      Message,'Failure writing model to file ' + OutFile,/cont
  ENDIF 

    ; return the field.
  field = [ [ui],[vi]] 
  return, field

END
