;+
; NAME:  makeETfield.pro
; $Id$
; PURPOSE:  make the Interpolated field for the Earth Today program
;
; AUTHOR:  whd
;
; CATEGORY:  Seawinds Animation
;
; CALLING SEQUENCE:  status=makeETfield([date_time , time_inc, |
;                                   Wfiles=Wfiles],OutFile=OutFile,$
;                                   Wpath=Wpath, Filter=Filter,$
;                                   Lonpar=lonpar, Latpar=Latpar,
;                                   Rainf=Rainf, Ermax=Ermax, $
;                                   min_Nvect=min_Nvect,$
;                                   Decimate=Decimate,$
;                                   CRDecimate=CRDecimate,$
;                                   ExcludeCols=ExcludeCols,$
;                                   starttime=starttime, $
;                                   endtime=endtime, $
;                                   interptime=interptime, $
;                                   rflag=rflag, tolerance=tolerance)
;
;
; 
; INPUTS:  All inputs, parameter and keyword, are optional.
;
;
;
; OPTIONAL INPUTS:  
;
;     date_time: (I/O) scalar string.  Wind files with an start time
;                after this time won't be used in the interpolated
;                field. Default is  the current time. If the field is
;                present only as a return argument
;                (i.e. n_elements(date_time) eq 0 and
;                arg_present(date_time) eq 1) then the value used is
;                returned.

;     time_inc: (I/O) start_time = date_time-time_inc. Default=26 hours
;                If the field is present only as a return argument
;                (i.e. n_elements(date_time) eq 0 and
;                arg_present(date_time) eq 1) then the value used is
;                returned.
;
;
;
;
;	
; KEYWORD PARAMETERS:  
;
;   Wfiles: (I/O) Vector of fully qualified file names. If this keyword is
;           non-empty, parameters 'date_time' and 'time_inc' are
;           ignored. Otherwise, the files used in the processing are
;           returned in this keyword.
;
;   OutFile: (I/O) if this keyword is a string, it is taken to be the
;            fully qualified output file name. If it is not set then
;            the field will be output to a file with the standard name
;            formula (i.e. QIF-yyyymmddhh.hdf) Q=Qscat,IF=Interpolated
;            field. yyyymmddhh is the end time of the field.
;
;
;              See 'file format' below for the format of the file.
;
;
;   WPath: (I) string, path to wind files (def=$VAP_DATA_TOP)
;
;   Filter: (I) string, filter you use when finding wind files.
;
;   LonPar: (I/O) if set on input to a 3-vector (min,max,inc), these
;           parameters are used in creating the interpolated field. If
;           only present as an output variable, these parameters are
;           defaulted and lonpar is set to them on
;           output. Default=[0,359,1.]
;
;   LatPar: (I/O) Same as for LonPar, but for Latitude. Default=[-60,60,1.]
;
;   RaInf: (I) Radius of Influence. vector, Radius of
;              influence. This is the number of grid cells to consider
;              when computing a value for the current grid
;              cell, so it's effect depends on the values of
;              lonpar/latpar.
;
;              This vector may contain any number of elements >0,
;              Default = [12., 6, 2 ]
;
;   ErMax: (I) vector, Maximum Error allowed before the data in the data
;              field is discarded in favor of the computed model.
;              Must have the same number of elements as 'rainf'
;              default = replicate( 50., n_elements(rainf))
;
;   Decimate - (I) scalar: take every nth vector, i.e. 2=> take
;                  every other, 3=> take every 3rd. (Defaults to 1,
;                  meaning take evey vector)
;
;   CRDecimate - (I) 2-vector: 1st entry specifies column, 2nd the
;                   row. e.g. [2,3] means take every other column, every
;                   3rd row. (defaults to [2,2] meaning take every
;                   *other* column and row.)
;                   This keyword overrides the `Decimate'
;
;   ExcludeCols - (I) string: comma seperated list of individual columns
;                   or ranges of colums to exclude. Ranges are indicated
;                   by start:stop (e.g. 33:42 will exclude columns 33
;                   through 42, inclusive). Ex: 0,40:42,72 will exclude
;                   columns 0, 40,41,42 and 72.  (Defaults to '' meaning
;                   exclude NO columns)
;
;
;   StartTime: (O) - the Earliest time which appears in the Wind
;                    Files.
;
;   EndTime: (O) - the Latest time which appears in the Wind
;                  Files.
;
;   InterpTime: (I/O) - The 'Time' of the interpolation. If not input,
;               it will be the mean of the actual timerange defined by
;               the data. IF input, it must lie between the start/end
;               time of the actual data, or an error will result. 
;
;   OutFile: (I/O) - If set on input, this will be the name of the
;                  output file. If present as a return argument, it
;                  will return the name of the output file.
;
;   rflag: (I), boolean. If set, remove rainf flagged data.
;
;   Tolerance: (I), scalar float. The permissable size of any void, in
;              units of grid cells, once a grid of the specified size
;              and dimension has been filled with the input data. The
;              grid is specified by the joint operation of
;              lonpar/latpar. See also the routine 'hasgaps.pro' for
;              the method of checking for gaps. 
;
;              Default = rainf[0]*1.5
;
;              NB. This checking is done on the input data. It's best
;              to set this value rather loosely and let the final
;              determination be made on the output data.
;
;
; OUTPUTS:  
;
;   A structure of the form 
;
;   retstruct = {status: int, filename: '', error_state:{!error_state} }
;
;   retstruct.status = 1 on success and 0 on failure with the system
;   error structure occupying the obvious place. If success, the
;   output filename will be in retstruct.filename (as well as the
;   `outfile' keyword)
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
;  In general neither this routine nor `MakeETAnim' are meant to be
;  called by humans. Rather they are supposed to be called from the
;  automated processing initiated by the Perl module ET.pm. I haven't
;  tested their interactive use, so there may be some problems that
;  have to be worked out if the processing scenario changes.
; 
;  But you're welcome to try, if you wish. I did try to write them
;  with some level of interactivity in mind.
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  2001/02/07 19:15:29  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 2000, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
FUNCTION makeETfield, date_time, $;(I/O) yyyy/mm/dd/hh End time 
                                ; of interpolated field. 
   time_inc  , $                ; start_time=date_time-time_inc
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
                                ; this will
                                ; be the name of the
                                ; output file. If
                                ; present as a
                                ; return argument,
                                ; the name of the
                                ; output file will
                                ; be returned in it.
   Decimate = Decimate, $       ; See Explanation above
    CRDecimate=CRDecimate,$     ; See Explanation above
    ExcludeCols=ExcludeCols,$   ; See Explanation above
    rflag=rflag , $             ; If set, remove rain flagged data
    tolerance=tolerance
   
   
   rcsid = "$Id$"

  LongName = "VAP_SUCCOR_INTERP_FIELD"
  VersionID=rcsid
  CreationTime =  (idldt2Vaptime(today()))[0]

  retstruct = {status:0, filename:'', error_state: {!error_state} }
  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    retstruct.status = 0
    Message,!error_state.msg,/noname,/cont
    retstruct.error_state = !error_state
    retstruct.filename = ''
    return,retstruct
  ENDIF 


  IF N_elements(LonPar) NE 3  THEN LonPar =  [0.,359,1.]
  IF N_elements(LatPar) NE 3  THEN LatPar =  [-60.,60.,1.]

  IF n_elements(rainf) eq 0 THEN rainf =     [12.,6,1]
  IF N_Elements(ermax) eq 0 THEN ermax = replicate(50.,n_elements(rainf))

  IF N_elements(rainf) NE n_Elements(ermax) THEN BEGIN 
    Message,"Rainf and Ermax must have same num elements!",/cont
    return,0
  ENDIF 
  IF n_elements(tolerance) EQ 0 THEN Tolerance = floor(rainf[0]*1.5)
  IF n_elements(crdecimate) NE 2 THEN crdecimate = [2,2]

  Message,'LonPar = ' + string(lonpar,form='(3(f7.2,:,","))'),/info
  Message,'LatPar = '+ string(latpar,form='(3(f7.2,:,","))'),/info
  nn = n_elements(rainf)
  form = '(' + strtrim(nn,2) + '(f7.2,:,","))'
  Message,'Rainf  = '+ string(rainf,form=form),/info
  Message,'ErMax  = '+ string(ermax,form=form),/info
  Message,'Tolerance = ' + strtrim(tolerance,2),/info

  IF n_Elements(crdecimate) NE 0 THEN  $
    Message,'CRDecimate = '+ string(crdecimate,form='(2(i2,:,","))'), /info
  IF n_elements(decimate) NE 0 THEN $
    Message,'Decimate = ' + strtrim(decimate,2), /info
  IF n_elements(excludeCols) NE 0 THEN $
    Message,'ExcludeCols = '+ string(excludeCols), /info

  
  rflag = keyword_set(rflag)

  IF n_elements(Wfiles) eq 0 THEN BEGIN 
    IF n_Elements(date_time) EQ 0 THEN date_time = TodayAsString(sep='/')
    IF n_elements(time_inc) eq 0 THEN time_inc = 26
    IF n_elements(Wpath) EQ 0 THEN Wpath = getenv('VAP_DATA_TOP')
    Wfiles = GetWindFiles(date_time,delta=time_inc,path=wpath, $
                             filter="Q*", count=cnt)    
    
  ENDIF ELSE cnt = n_elements(Wfiles)

  IF cnt EQ 0 THEN $
    Message,'No files for time '+ date_time +' and time_inc ' + $
     string(time_inc,form='(f7.2)')

  IF strlen( Wfiles[0] ) EQ 0 OR N_Elements(Wfiles) EQ 0 THEN $
    Message,'Error in GetWindFiles'

  IF hasgaps(wfiles, tolerance, lonpar, latpar) EQ 1 THEN $
    Message,"Wind Files have gaps for input lonpar/latpar/tolerance"

  
  ff = makeInterpFile(wfiles=wfiles, $
                      lonpar=lonpar,latpar=latpar,rainf=rainf,$
                      ermax=ermax,decimate=decimate,crdecimate=crdecimate,$
                      excludecols=excludecols,outfile=outfile,$
                      rflag=rflag)
  
  status = checkInterpFile(file=outfile)
  IF status EQ 1 THEN BEGIN 
     retstruct.filename = outfile
     retstruct.status = status
  ENDIF ELSE Message,"checkInterpFile found gaps!"

  return,retstruct
  
END

