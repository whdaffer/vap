FUNCTION runsuccor, u,v,lon,lat,ui,vi,lonpar,latpar,$
                    rainf=rainf, $
                    ermax=ermax, $
                    ofile=ofile, $
                    help=help
;+
; NAME:   RunSuccor
; PURPOSE:  A Wrapper for the Succor.so interpolation routine
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:   I/O - wrapper
;
;
;
; CALLING SEQUENCE:  status=runsuccor(u,v,lon,lat,[ui,vi | ofile=ofile],
;                                     [lonpar,latpar, rainf=rainf,
;                                       ermax=ermax, help=help] )
;
;
; 
; INPUTS:  
;
;       U: U Component of  Wind field
;       V: V Component of  Wind field
;       Lon: Longitude for U/V
;       Lat: Latitude for U/V
;      
;
;
;
; OPTIONAL INPUTS:  
;
;      LonPar: float 3-vector, output fields longitude parameters 
;               [ min, max, increment ]. Default=[0.,359.,1]
;      LatPar: float 3-vector, output fields latitude parameters 
;               [ min, max, increment ]. Default=[-60.,60.,1]
;
;
;
;
;	
; KEYWORD PARAMETERS:  
;
;       rainf: Radius of influence. This is the number of grid cells
;            to consider when computing a value for the current grid
;            cell. Default = [12., 10., 6,   4 ] 
;       ermax: Maximum Error allowed before the data in the data
;              field is discarded in favor of the computed model
;              default = [50., 20., 10., 5.];
;
;       ofile: fully qualified output filename. Data will be written
;            to this file in a manner consistent with the routine
;            _ANIMATE_WIND_FIELD_ in addition to being output to the
;            variables UI/VI, if they are present.
;              
;
;
; OUTPUTS:  
;
;        UI: U component of interpolated field (array with
;            dimensionality determined by lonpar/latpar.
;        VI: V component of interpolated field (array with
;            dimensionality determined by lonpar/latpar.
;
;        RETURN_VALUE: 2=couldn't write to file but did output data to
;            UI/VI variables. 1=success, 0=failure. 
;
;            The basic idea is, if there is somewhere for the data to
;            go on output and it gets there, the routine succeeds, so
;            if UI/VI are present in the output list and UI/VI are
;            successfully created, then the routine is successful,
;            even if the write to the file that was additionally
;            specified in the ofile keyword was unsuccessful.
;
;
; OPTIONAL OUTPUTS:  
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
; PROCEDURE:  Check arguments, call succor, output results.
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

   rcsid = "$Id$"

  status = 0

  lf = string(10b)

  hstr = lf +    "Usage: result=runsuccor(u,v,lon,lat$" + lf
  hstr = hstr + "  [[,ui,vi,]|[ofile=ofile]]$ " + lf 
  hstr = hstr + "  [ lonpar, latpar, rainf=rainf, ermax=ermax,$ " +  lf 
  hstr = hstr + "  help=0|1 ])" + lf + lf 
  hstr = hstr + "  Where... " + lf + lf 
  hstr = hstr + "  U: The U component (I)" + lf
  hstr = hstr + "  V: The V component (I)" + lf
  hstr = hstr + "  LON: The longitude (I)" + lf 
  hstr = hstr + "  LAT: the Latitude (I)" + lf
  hstr = hstr + "  UI: The U component of the interpolated field (O)" + lf
  hstr = hstr + "  VI: The V component of the interpolated field (O)" + lf
  hstr = hstr + "  OFILE: Output Filename, data will be written to the file" + lf
  hstr = hstr + "    NB. you must have either UI/VI or OFILE specified" + lf
  hstr = hstr + "    Additionally, this routine will return a failure if " + lf
  hstr = hstr + "    you specify a file but not UI/VI and the write to the"+lf
  hstr = hstr + "    file fails. It's best to always specify UI/VI and test"+lf
  hstr = hstr + "    the status after this routine returns." + lf
  hstr = hstr + "  LONPAR: the longitude parameters of the output " + lf
  hstr = hstr + "    interpolated field [min,max,inc] " +lf
  hstr = hstr + "    (def=[0.,359.,1.] ) " + lf
  hstr = hstr + "  LATPAR: the latitude parameters of the output " + lf
  hstr = hstr + "    interpolated field [min,max,inc] " + lf
  hstr = hstr + "    (def=[-60.,60.,1.] )" + lf
  hstr = hstr + "  RAINF: The 'radius of influence' for each of the four " + lf
  hstr = hstr + "    runs, should be a decreasing sequence. Each entry is the " +lf
  hstr = hstr + "    number of grid cells away from the current cell to " + lf
  hstr = hstr + "    consider when calculating the current cell " + lf
  hstr = hstr + "    (def=[12.,10,6,4]) "+lf
  hstr = hstr + "  ERMAX: The 'Maximum difference' between the data and " + LF
  hstr = hstr + "    model field allowed " + lf
  hstr = hstr + "    (Def=[50.,20.,10,.5] " + lf
  hstr = hstr + "  HELP: this message" + lf + lf
  hstr = hstr + "  The interpolated field is returned in the variables " + lf
  hstr = hstr + "  UI and VI, and optionally written to the file given " + lf
  hstr = hstr + "  in 'OFILE'" + lf
  hstr = hstr + "  Successful completion returns of 1, unsuccessful "+ lf
  hstr = hstr+  "  write in the presence of UI/VI returns 2." + lf
  hstr = hstr + "  Otherwise 0 is returned." + lf

  IF keyword_set(help) THEN BEGIN 
    print,hstr
    return,0
  ENDIF 


  IF N_Params() LT  4 THEN BEGIN 
    Message,"Insufficient Parameters",/cont
    print,hstr
    return,0
  ENDIF 

  IF NOT (Arg_Present(ui) AND Arg_Present(vi)) AND $
     n_elements(ofile) EQ 0 THEN BEGIN 
    str =  'You must either give UI and VI as return arrays ' + lf
    str =  str + ' or provide an output file via "ofile" keyword' + lf + lf
    Message,str,/cont
    print, hstr
    return,0
  ENDIF 
  IF N_elements(lonpar) ne 3 THEN lonpar = [0.,359,1]
  IF N_elements(latpar) NE 3 THEN latpar = [-60.,60,1]
  IF n_elements(rainf) NE 4 THEN rainf = [12., 10., 6,   4 ] 
  IF N_Elements(ermax) NE 4 THEN ermax = [50., 20., 10., 5.]

  good = where( finite(u) AND finite(v), ngood)
  IF ngood NE 0 THEN BEGIN 
    uu = u[good]
    vv = v[good]
    llon = lon[good]
    llat = lat[good]

    IF VarType(uu)   NE 'FLOAT' THEN uu = float(uu)
    IF VarType(vv)   NE 'FLOAT' THEN vv = float(vv)
    IF VarType(llon) NE 'FLOAT' THEN llon = float(llon)
    IF VarType(llat) NE 'FLOAT' THEN llat = float(llat)
    IF VarType(rainf) NE 'FLOAT' THEN rainf = float(rainf)
    IF VarType(ermax) NE 'FLOAT' THEN ermax = float(ermax)

    nu = (lonpar[1]-lonpar[0])/lonpar[2] + 1
    nv = (latpar[1]-latpar[0])/latpar[2] + 1 
    ui = fltarr(nu,nv)
    vi = fltarr(nu,nv)
    Succor, llon,llat,uu,vv,ui,vi,lonpar,latpar,ermax,rainf
    status = 1
    IF n_elements(ofile) THEN BEGIN 
      openw, lun, ofile, /get, error=err
      IF err EQ 0 THEN BEGIN 
        WriteU, lun, ui, vi
        free_lun, lun
        Message,' Data written successfully to file ' + ofile,/cont
        status = 1
      ENDIF ELSE BEGIN 
        Message,!error_State.msg,/cont
        IF Arg_Present(ui) AND Arg_Present(vi) THEN BEGIN 
          Message,'Data return via UI/VI arguments ',/cont
          status = 2
        ENDIF ELSE BEGIN 
           Message,'UI/VI not present on command line, aborting!',/cont
           status = 0
         ENDELSE 
      ENDELSE   
    ENDIF ELSE status = 1
  ENDIF ELSE status = 0
  return,status
END




