;+
; NAME:  Q2B__DEFINE
; PURPOSE:   Defines an object of type 'Q2B'
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:   OO
;
;
;
; CALLING SEQUENCE:   q=obj_new('Q@B',file=file...)
;
; Provision has been made to input each of the quantities needed
; individually, but I wouldn't try it, if I were you. Better to have
; it read the file. At the moment it will recognize a Q2B HDF file
; and one made by SVH's software.
;
; OUTPUTS:   if successful, an object of type Q2b, otherwise, nothing.
;
; METHODS:
; ===============================================================
;
; FUNCTION INIT: 
;
; Initializes Q2B object
; 
; You can (try to) build the object up from individual quantities or
; pass a data structure in using the data_struct keyword, but none of
; these methods are tested. Caveat User. By far the normal way to
; instantiate this object is to give it a filename and let it read the
; data, possibly configuring the interface with the object with some
; other, additional keywords. So, I'm just going to ignore all the
; u,v,lon,lat... datastruct keywords and move directly to the more
; natural interface, which is:
;
;  filename   : the fully-qualified filename to read.
;  decimate   : configure the 1-d decimation. (see below)
;  crdecimate : configure the 2-d decimation (see below)
;  excludeCols: configure which excluded columns.
;  ambig      : configure the default ambiguity to select (default=0, the
;               `selected' ambiguity) See below.
;
;
; 
; INPUTS:   (building object from the ground up, *NOT RECOMMENDED*)
;
;          u      : fltarr [4, n, m], the U component
;          v      : "         "     , the V
;          lon    : fltarr [n,m]    , The longitude
;          lat    : fltarr [n,m]      the Latitude
;          sel    : intarr [n,m]      The 'Selected vector'
;          sel2   : intarr [n,m]      Another
;          idx    : intarr [n,m]      ??
;          row    : intarr [n,m]     The Row
;          qual   : intarr [n,m]     The Quality flag
;          nambig : intarr [n,m]     Number of ambiguities
;          mu     : fltarr [n,m]     A model field U
;          mv     : fltarr [n,m]     A model field U
;          su     : fltarr [n,m]     The Selected U field
;          sv     : fltarr [n,m]     The Selected V field
;          eqx_date : scalar string
;          eqx_time  : scalar string
;          eqx_lon   : scalar string
;
;
;
;
;	
; KEYWORD PARAMETERS:  
;
;          type       :
;          ambig      : The default ambiguity to select (also see
;                       GetPlotData for discussion of the effect of
;                       this state variable)
;                       Once set, the object will return this
;                       ambiguity until it's changed, either by a
;                       call to obj->set,ambig=n or by passing ambig
;                       in as the fifth parameter in the call to
;                       GetPlotData.

;          data_struct: A structure of type Q2b of Qmodel
;          eqx        : A structure of type Eqx
;          filename   : Read this file
;          decimate   : n: take every n-th vector
;          crdecimate : [p,q] take every p-th column of every n-th row
;          ExcludeCols: string: '0,23,25,72' means exclude columns
;                       0,23,24,25 and 72
;
;
;          Verbose : Be verbose
;          rainflag : flag, 0=ignore raing flagged data, 1 = do
;                     whatever rf_action specifies to do
;
;           rf_action: 0=remove rain flagged data, 1=plot it with
;                      rf_color.
;
;           rf_color; Color index (8 bit) or the color itself( 24 bit)
;                     to plot rain flagged data with.
;
;
; Returns 1 for success, 0 for failure. Since this method is called by
; the obj_new() builtin, the effect for the user is, either the object
; is defined or it isn't.
;
; The routine first checks for the 'filename' keyword. If present,
; it tries to read that file. The status returned by the read
; routine is passed along by this routine.
; 
; The routine then checks for the 'data_struct' keyword. If the
; data_struct keyword is present, it tries to take the data from it,
; subject to the restriction that the datum pointed to by the
; keyword is a structure of type 'Q2BDATA,' 'QMODEL' or 'RQ2BDATA.'
; (see files q2b_str.pro, qmodel_str.pro and 4q2b_str for
; definitions) If this keyword is not present, it will check for the
; first positional parameter. If it is present, then ALL 14 must be
; present.
; 
; 
; Each of the postional  parameters must be the correct dimension,
; but will be cast to the  correct type without concern for the
; effect of that cast. Caveat Emptor.  
;
; The EQX structure will be constructed from the eqx_* positional
; parameters, if they are present. Each will be cast as needed,
; without regard for the effect of that cast. If these aren't
; present, self.eqx will not be set unless the eqx keyword is present.
; The Eqx quantity pointed to by the EQX keyword must be a structure
; of type EQX. (See the file eqx_str.pro for the definition)
;
; ===============================================================
; FUNCTION SET:  Set quantities in the object
;
;   (All quantities are keywords)
;
;     u      : ---|
;     v      :    | 
;     lon    :    | 
;     lat    :    | 
;     sel    :    | 
;     sel2   :    | 
;     nambig :    | 
;     row    :    | ----- Completely untested, DON'T USE!
;     idx    :    | 
;     qual   :    | 
;     mu     :    | 
;     mv     :    | 
;     su     :    | 
;     sv     :    | 
;     eqx    : ---|
; 
;     ambig       : --|
;     decimate    :   |
;     crdecimate  :   |
;     ExcludeCols :   |
;     StartTime   :   | ----- See descrition in ::INIT
;     EndTime     :   |
;     rainflag    :   |
;     rf_action   :   |
;     rf_color    : ---
;
;
; ===============================================================
; FUNCTION GET:  Get quantities (data or settings) from the object
;
;  (all quantities are keywords)
;
;  The quantities you will most frequently extract from the object
;  are:
;
;               ambig       : The current ambiguity that will be
;                             selected if not specified otherwise.
;               decimate    : the current state of the decimate 
;               crdecimate  :  crdecimate and 
;               ExcludeCols :  excludecols parameters.
;               data        : The *FULL* data structure
;               StartTime   : Start ---
;               EndTime     : and End time of the data
;               rainflag    : The current state of the rainflag 
;               rf_action   : rf_action and 
;               rf_color    : rf_color flags
;
;               u        : -----
;               v        :      |
;               lon      :      |
;               lat      :      |
;               sel      :      |
;               nambig   :      |
;               qual     :      |
;               row      :      |
;               idx      :      |
;               mu       :      |
;               mv       :      |
;               su       :      |
;               sv       :      |
;               mle      :      | -- Very infrequently used.
;               smle     :      |    But you may extract individual quantities
;               rflag    :      |    Out of the data struct if you wish.
;               tb_h     :      |
;               tb_v     :      |
;               num_tb   :      |
;               tb_rainrate :   |
;               tb_atten :      |
;               rowtime  :      |
;               eqx      :      |
;               filename :      |
;               type     :      |
;               ncells   :      |
;               nrecs    : -----
;
;              infostruct : um, don't use this!
;
;
;  
; ===============================================================
; FUNCTION GETPLOTDATA: 
;
;  Retrieve the data from the object subject to the internal settings
;  of the object as well as possible input parameters/keywords.
;
;
; Parameters: u,v,lon,lat, ambig
;
;   Output: u,v,lon,lat. The data. These come out in arrays that have
;           the same row/column format as the swath data. This is so I
;           can do row/column operations on the data.
;
;           HOWEVER, all the data that isn't meant to be there
;           (i.e. rows and columns that have been removed by the
;           operation of those keywords
;           (decimate, crdecimate, excludecols) that affect these
;           things has been replaced with NANs, so the user has to
;           eliminate these from the data if he/she doesn't need the
;           row/column ness of the data.
;
;   Input: Ambig. The ambiguity to return. If unspecified, the
;          ambiguity defined by the internal state variable `ambig' is
;          returned. If present on input, the internal
;          quantity is set to this number. All subsequent calls to
;          this routine with `ambig' unspecified will return this
;          ambiguity. 
;
;          For instance, if you do not set ambig upon object
;          initialization it is set to '0' (the `selected' ambiguity)
;          and the first time you call this routine you leave this
;          parameter unset then the `selected' ambiguity will be
;          returned. If the second time, you set `ambig' = 1 in the
;          call to this routine, then that call and all subsequent
;          calls for which you neither pass in `ambig' nor set the
;          ambiguity with a call to obj->set, will also return
;          ambituity 1.
;
; Keywords:
;    limit : (I), 4 vector, [lonmin, latmin, lonmax, latmax]
;    decimate: (I), scalar number, See INIT for description
;    crdecimate : (I), 2 vector, see INIT for description
;    excludecols : (I), scalar string, see INIT for description
;    Silent : (I), flag. Be quiet about it
;    rf_index: (O). Return the indices which are rainflagged, so that
;              the calling program can decide what to do with them.

; ===============================================================
; ===============================================================
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; 
; MODIFICATION HISTORY:
; $Log$
; Revision 1.22  2000/12/14 23:04:34  vapuser
; changed the semantics of the rain flag stuff.
; Incorporated the new MGDR quantities.
;
; Revision 1.21  2000/03/01 19:33:26  vapuser
; fixed a small problem with rain flagging
;
; Revision 1.20  2000/02/23 21:59:15  vapuser
; Added code to handle the rain flags.
;
; Revision 1.19  2000/01/11 20:39:47  vapuser
; Added a 'self-destruct' method
;
; Revision 1.18  1999/11/12 19:55:46  vapuser
; Added code to support new DIRTH selected vectors in the
; new L2B data product. (per Bryan's request)
;
; Revision 1.17  1999/10/21 23:05:01  vapuser
; Added 'infostruct' to GET, to support cw_pvfinfo code
; in pv and pv_config.
;
; Revision 1.16  1999/10/05 16:42:52  vapuser
; Added code to capture the Equator times/Longitude. Commented out
; some of the code in qswathextent.
;
; Revision 1.15  1999/08/23 17:43:52  vapuser
; corrected 'unknown' type, added and changed some arguments
; to ::GetAll
;
; Revision 1.14  1999/06/29 20:46:13  vapuser
; Fix name disagreement between Rnoaa file and object.
;
; Revision 1.13  1998/11/25 22:42:22  vapuser
; Set start/End time when reading Rnoaa data.
;
; Revision 1.12  1998/11/10 00:46:57  vapuser
; Change to assuming rnoaa data.
;
; Revision 1.11  1998/10/29 22:36:00  vapuser
; added Verbose keyword/member.
;
; Revision 1.10  1998/10/28 23:29:01  vapuser
; worked on getextent
;
; Revision 1.9  1998/10/26 22:08:38  vapuser
; Added Rq2bdata structure stuff, updated comments.
;
; Revision 1.8  1998/10/23 22:21:53  vapuser
; Added 'arg_present' to ::GetAll,
; Incorporate DeEnvVar to protect RSIs HDF_... code from itself.
;
; Revision 1.7  1998/10/22 21:35:33  vapuser
; Added GETALL method
;
; Revision 1.6  1998/10/12 22:35:30  vapuser
; Added some calls to IsQ2B and other things I can't remember
;
; Revision 1.5  1998/10/07 00:10:35  vapuser
; Squashed a bug having to do with (Start|End)Time
;
; Revision 1.4  1998/10/05 22:54:41  vapuser
; Added Start/EndTime to Read,Set,Get
; and Object Definition
;
; Revision 1.3  1998/10/01 17:52:44  vapuser
; Modified 'version' method so that it will report
; the versions of member classes. Put in some error handling
; so that it'll ignore calls to undefined 'version' methods.
;
; Revision 1.2  1998/10/01 15:38:51  vapuser
; Added 'Version' Method. Returns rcsid string. Eliminated rcsid member.
; It's now a string local to the version method.
;
; Revision 1.1  1998/09/30 23:39:58  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-


FUNCTION Q2B::Init, $
            u,     $ ; fltarr [4,n,m]
            v,     $ ; "         "
            lon,   $ ; fltarr [n,m]
            lat,   $ ; fltarr [n,m]
            sel,   $ ; intarr [n,m]
            sel2,  $ ; intarr [n,m]
            idx,   $ ; intarr [n,m]
            row,   $ ; intarr [n,m]
            qual,  $ ; intarr [n,m]
            nambig,$ ; intarr [n,m]
            mu,    $ ; fltarr [n,m]
            mv,    $ ; fltarr [n,m]
            su,    $ ; fltarr [n,m]
            sv,    $ ; fltarr [n,m]
            eqx_date, $ ; scalar string
            eqx_time, $ ; scalar string
            eqx_lon,  $ ; scalar string
            type= type ,$
            data_struct = data_struct, $ ; structure of type Q2BDATA,RQ2BDATA or 
                                         ; Q2B_RNOAA
            eqx=eqx,$                    ; structure of type EQX
            filename=filename ,$
            decimate=decimate ,$
            crdecimate=crdecimate ,$
            ExcludeCols=ExcludeCols, $
            Verbose=Verbose, $
             ambig=ambig, $

                ; All 'rf_' or '_rf' are Rain Flag quantities.
                ;
                ; rainflag: boolean. 0=don't use flag, 1=use flat.
                ;
                ; rf_action determines whether to skip plotting the
                ; data (rf_action=0) or plot it with whatever quantity
                ; is given in rf_color (rf_action=1)
                ;
                ; rf_color is either the color index, if 8-bit env, or
                ; the 24 bit color itself with which rain flagged data
                ; is plotted, provided rf_action=1.
                ;

               rainflag    = rainflag, $ ; 
               rf_action = rf_action, $ ; 0|1
               rf_color  = rf_color     ; color_index(8bit)|color(24bit)
               
             


  Forward_Function  q2b_str  

  
  Catch, Error
  IF Error NE 0 THEN BEGIN 
    Catch,/Cancel
;    ok = Dialog_Message(!Error_State.msg)
;    Message,!error_state.msg,/cont
    ok = Dialog_Message(!err_string)
    Message,!err_string,/cont
    ; Obj_Destroy,self
    return,0
  ENDIF 



  IF n_elements(rainflag) EQ 0 THEN rainflag =  0
  IF n_elements(rf_action) EQ 0 THEN  rf_action=0
  IF n_elements(rf_color) EQ 0 THEN rf_color = 'ffffff'xl
  IF n_elements(ambig) NE  0 THEN self.ambig = 0 >  ambig[0] <  6

  self.rain_flag = rainflag
  self.rain_flag_action = rf_action
  self.rain_flag_color = rf_color
  
  status = 0
  self.filename = '<No Name>'
  self.type = 'UNKNOWN'
  self.Verbose = keyword_set(Verbose)

  IF N_Elements(filename) NE 0 THEN BEGIN 
    IF N_Elements(type) NE 0 THEN self.type = StrupCase(type)
    self.filename = filename
    status = self->Read(self.filename)
  ENDIF ELSE BEGIN 
    IF N_Elements( data_struct ) NE 0 THEN BEGIN 
      name = Tag_Names( data_struct, /structure_name ) 
      IF name EQ 'Q2BDATA' OR $
         name EQ 'QMODEL' OR $
         name EQ 'RQ2BDATA' THEN BEGIN
        IF name EQ 'QMODEL' THEN BEGIN 
          Self.nrecs = -1
          self.ncells = -1
          self.model = 1
        ENDIF ELSE BEGIN 
          self.nrecs =  N_Elements(data_struct)
          self.ncells = N_Elements(data_struct(0).su)
          self.model = 0
        ENDELSE 
        self.data =  ptr_new( data_struct )
        status = 1
      ENDIF ELSE BEGIN
        STR = 'Input Data Structure must be of type Q2BDATA, QMODEL, or RQ2BDATA'
        Message,str,/cont
      ENDELSE 
      IF N_Elements( eqx ) NE 0 THEN self.eqx = eqx
        ; Get the extent of the data
      ; self-> GetExtent
    ENDIF ELSE BEGIN 
      IF n_params() ge 14 THEN  BEGIN 
        IF N_Elements(u) NE 0 THEN BEGIN 
          self.type = ''
          s = size(u)
          type_flag = s(s(0)+1)
          IF s(0) EQ 3 THEN BEGIN 
            nrecs = s(3)
            ncells = s(2)
            self.nrecs = nrecs
            self.ncells = ncells
            ndims_string2 =  '[' + $
                              strtrim(nrecs,2) + ',' + $
                              strtrim(ncells,2) + $
                             ']'
            ndims_string3 =  '[4,' + $
                              strtrim(nrecs,2) + ',' + $
                              strtrim(ncells,2) + $
                             ']'

            data =  Q2B_Str( nrecs, ncells=ncells)

              ; U
            IF s(1) NE 4 OR $
               s(2) NE ncells OR s(3) NE nrecs THEN BEGIN 
              Message,'U must have dims: ' + $
               ndims_string3,/cont 
              return,0
            ENDIF ELSE $
              IF vartype(u)  NE 'FLOAT' THEN u = FLOAT(u)
            data.u =  u


              ; V
            s =  size(v) 
            IF s(1) NE 4 OR $
               s(2) NE ncells OR s(3) NE nrecs THEN BEGIN 
              Message,'V must have dims: ' + $
               ndims_string3,/cont 
              return,0
            ENDIF ELSE $
              IF VarType(v)  NE 'FLOAT' THEN v = FLOAT(v)
            data.v = v

              ; Lon
            s =  size(lon) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'LON must have dims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE  $
              IF VarType(lon)  NE 'FLOAT' THEN lon = FLOAT(lon)
            data.lon = lon

              ; Lat
            s =  size(lat) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'LAT must have dims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE  $
              IF VarType(lat)  NE 'FLOAT' THEN lat = FLOAT(lat)
            data.lat = lat

              ; Sel
            s =  size(sel) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'SEL must have dims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE  $
              IF VarType(sel)  NE 'INT' THEN sel = FIX(sel)
            data.sel = sel

              ; Sel2
            s =  size(sel2) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'SEL2 must have dims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE $
              IF VarType(sel2)  NE 'INT' THEN sel2 = FIX(sel2)
            data.sel2 = sel2

              ; Nambig
            s =  size(nambig) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'NAMBIG must have dims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE  $
              IF VarType(nambig)  NE 'INT' THEN nambig = FIX(nambig)
            data.nambig = nambig

              ; Qual
            s =  size(qual) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'QUAL must have Ndims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE  $
              IF VarType(qual)  NE 'INT' THEN qual = FIX(qual)
            data.qual = qual


              ; Row
            s =  size(row) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'ROW must have Ndims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE  $
              IF VarType(row)  NE 'INT' THEN row = FIX(row)
            data.row = row

              ; Idx
            s =  size(idx) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'IDX must have Ndims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE  $
              IF VarType(idx)  NE 'INT' THEN idx = FIX(idx)
            data.idx = idx


              ; Mu
            s =  size(mu) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'MU must have dims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE  $
              IF VarType(mu)  NE 'FLOAT' THEN mu = FLOAT(mu)
            data.mu = mu

             ; Mv
            s =  size(mv) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'MV must be FLOAT Array: Ndims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE   $
              IF VarType(mv)  NE 'FLOAT' THEN mv = FLOAT(mv)
            data.mv = mv

             ; Su
            s =  size(su) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'SU must have dims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE  $
              IF VarType(su)  NE 'FLOAT' THEN su = FLOAT(su)
             data.su = su

             ;Sv
            s =  size(sv) 
            IF s(1) NE ncells OR $
               s(2) NE nrecs THEN BEGIN 
              Message,'SV must have dims: ' + $
               ndims_string2,/cont 
              return,0
            ENDIF ELSE   $
              IF VarType(sv)  NE 'FLOAT' THEN sv = FLOAT(sv)
            data.sv = sv
            status = 1
          ENDIF ELSE BEGIN 
            message,"U must have 3 dims",/cont
          ENDELSE 
        ENDIF ELSE BEGIN 
           str =  "If neither 'data_struct' nor 'filename' " + $
            "are defined, U/V/Lon/Lat... must be."
           Message,str,/cont
        ENDELSE 
        IF N_Elements(eqx_date) NE 0 THEN $
          self.eqx.date = string(eqx_date)
        IF N_Elements(eqx_time) NE 0 THEN $
          self.eqx.time = string(eqx_time)
        IF N_Elements(eqx_lon) NE 0 THEN $
          self.eqx.lon = float(eqx_lon)

        self.data =  ptr_new(data)
          ; Get the extent of the data, if possible.
        ; self-> GetExtent

      ENDIF ELSE $
        Message, $
         "If not using 'data_struct= ' call: MUST use AT LEAST 14 params" ,/cont
    ENDELSE 
  ENDELSE 

  IF N_Elements(crdecimate) eq 2 THEN BEGIN 
    self.crdecimate = $
     [ 0> crdecimate[0] < self.ncells, $
       0 > crdecimate[1] < self.nrecs]
    self.decimate = 0;
  ENDIF ELSE BEGIN 
    IF N_Elements(decimate) eq 0 THEN decimate =  5
    decimate = 1> decimate < 99 ; Check decimate's bounds
    self.decimate = decimate
    self.crdecimate = [0,0]
  ENDELSE  

  IF N_Elements(ExcludeCols) NE 0 AND $
   VarType(ExcludeCols) EQ 'STRING' THEN BEGIN 
    IF strlen( ExcludeCols ) GT 0 THEN BEGIN 
      self-> SetExcludeCols,ExcludeCols
    ENDIF 
  ENDIF ELSE self-> SetExcludeCols
  return,status
END ; End Init


  ; ===============================================================
  ;
  ; Q2B::SET: 
  ;
  ; Allows the setting of any of the quantities in the
  ; q2b structure.
  ;
  ; Pertenant quantities.
  ; 4: number of ambiquities.
  ; n: number of cross-track columns, nominally 76
  ; m: number of records
  ;
  ; All keywords are input quantities. 
  ; 0 returned if failure, 1 if successful.  Multiple keywords may be
  ; present. The routine will try to set each one, but will return 0
  ; if there is any failure. It will display an error message for any
  ; failures.
  ;
  ; ===============================================================

FUNCTION q2b::Set, $
               u      = u      , $ ; Raw U [4,n,m]
               v      = v      , $
               lon    = lon    , $
               lat    = lat    , $
               sel    = sel    , $
               sel2   = sel2   , $
               nambig = nambig , $
               row    = row    , $
               idx    = idx    , $
               qual   = qual   , $
               mu     = mu     , $
               mv     = mv     , $
               su     = su     , $
               sv     = sv     ,$
               eqx    = eqx ,$
               ambig  = ambig, $
               decimate    =  decimate,$
               crdecimate  = crdecimate, $
               ExcludeCols = ExcludeCols, $
               StartTime= StartTime, $
               EndTime= EndTime, $

                ; All 'rf_' or '_rf' are Rain Flag quantities.
                ;
                ; rainflag: boolean, 0=don't use flag, 1=use flag
                ; rf_action determines whether to skip plotting the
                ; data (rf_action=0) or plot it with whatever quantity
                ; is given in rf_color (rf_action=1)
                ;
                ; rf_color is either the color index, if 8-bit env, or
                ; the 24 bit color itself with which rain flagged data
                ; is plotted, provided rf_action=1.
                ;

               rainflag    = rainflag, $ ; ''|'MP'|'NOF' or 0|1|2
               rf_action = rf_action, $ ; 0|1
               rf_color  = rf_color     ; color_index(8bit)|color(24bit)
               
               
   

  recalc_extent = 0
  status = 1

  IF n_elements(rainflag) NE 0 THEN self.rain_flag = rainflag
  IF n_elements(rf_action) NE 0 THEN self.rain_flag_action = 0> rf_action < 1
  IF n_elements(rf_color) NE 0 THEN $
    self.rain_flag_color = 0> rf_color < !d.n_colors-1

  IF n_elements(ambig) NE 0 THEN self.ambig =  0> ambig < 6

  IF NOT self.model THEN BEGIN 
    IF N_Elements(crdecimate) EQ 2 THEN BEGIN 
      self.crdecimate = [ 0> crdecimate[0] < self.ncells, $
                          0> crdecimate[1] < self.nrecs ]
    ENDIF 
    IF N_Elements(decimate) NE 0 THEN self.decimate = (1> decimate < 99)
    IF N_Elements(ExcludeCols) NE 0 AND $
      VarType(ExcludeCols) EQ 'STRING' THEN BEGIN  
      self-> SetExcludeCols,ExcludeCols
    ENDIF     
  ENDIF 

  IF n_Elements(StartTime) NE 0 THEN self.StartTime =  StartTime
  IF n_Elements(EndTime) NE 0 THEN self.EndTime =  EndTime
    
  IF ptr_valid( self.data ) THEN BEGIN 
      ; ==========
      ; Set U 
      ; ========== 
    IF N_Elements( u ) NE 0 THEN BEGIN 
      s1 = size(u)
      s2 = size(*self.data.u)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! U must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.u =  u
    ENDIF 

      ; ==========
      ; Set V
      ; ========== 

    IF N_Elements( v ) NE 0 THEN BEGIN 
      s1 = size(v)
      s2 = size(*self.data.v)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! V must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.v =  v
    ENDIF

      ; ==========
      ; Set Lon 
      ; ========== 

    IF N_Elements( lon ) NE 0 THEN BEGIN 
      s1 = size(lon)
      s2 = size(*self.data.lon)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! LON must have dims '+size_string,/cont
        status = 0
      ENDIF ELSE recalc_extent = 1
      *self.data.lon =  lon
    ENDIF

      ; ==========
      ; Set Lat
      ; ========== 

    IF N_Elements( lat ) NE 0 THEN BEGIN 
      s1 = size(lat)
      s2 = size(*self.data.lat)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! LAT must have dims '+size_string,/cont
        status = 0
      ENDIF ELSE recalc_extent = 1
      *self.data.lat =  lat
    ENDIF

      ; ==========
      ; Set Sel
      ; ========== 

    IF  N_Elements( sel ) NE 0 THEN BEGIN 
      s1 = size(sel)
      s2 = size(*self.data.sel)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! SEL must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.sel =  sel
    ENDIF

      ; ==========
      ; Set Sel2
      ; ========== 

    IF  N_Elements( sel2 ) NE 0 THEN BEGIN 
      s1 = size(sel2)
      s2 = size(*self.data.sel2)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! SEL2 must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.sel2 =  sel2
    ENDIF

      ; ==========
      ; Set Nambig
      ; ========== 

    IF  N_Elements( nambig ) NE 0 THEN BEGIN 
      s1 = size(nambig)
      s2 = size(*self.data.nambig)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! NAMBIG must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.nambig =  nambig
    ENDIF

      ; ==========
      ; Set Qual
      ; ========== 

    IF  N_Elements( qual ) NE 0 THEN BEGIN 
      s1 = size(qual)
      s2 = size(*self.data.qual)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! QUAL must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.qual =  qual
    ENDIF


      ; ==========
      ; Set Row
      ; ========== 

    IF  N_Elements( row ) NE 0 THEN BEGIN 
      s1 = size(row)
      s2 = size(*self.data.row)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! ROW must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.row =  row
    ENDIF

      ; ==========
      ; Set Idx
      ; ========== 

    IF  N_Elements( idx ) NE 0 THEN BEGIN 
      s1 = size(idx)
      s2 = size(*self.data.idx)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! IDX must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.idx =  idx
    ENDIF


      ; ==========
      ; Set MU 
      ; ========== 

    IF  N_Elements( mu ) NE 0 THEN BEGIN 
      s1 = size(mu)
      s2 = size(*self.data.mu)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! MU must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.mu =  mu
    ENDIF

      ; ==========
      ; Set MV
      ; ========== 

    IF N_Elements( mv ) NE 0 THEN BEGIN 
      s1 = size(mv)
      s2 = size(*self.data.mv)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! MV must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.mv =  mv
    ENDIF 

      ; ==========
      ; Set SU 
      ; ========== 

    IF N_Elements( su ) NE 0 THEN BEGIN 
      s1 = size(su)
      s2 = size(*self.data.su)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! SU must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.su =  su
    ENDIF 


      ; ==========
      ; Set SV
      ; ========== 

    IF N_Elements( sv ) NE 0 THEN BEGIN 
      s1 = size(sv)
      s2 = size(*self.data.sv)
      x = where( s1-s2,nx )
      IF nx NE 0 THEN BEGIN 
        size_string =  string( s2[1:3], $
                            form="('[',i4,',',i4,',',i4,']')" )
        Message,'Size disagreement! SV must have dims '+size_string,/cont
        status = 0
      ENDIF 
      *self.data.sv =  sv
    ENDIF 

  ENDIF ELSE BEGIN 
    Message,'Data Ptr NOT valid, use Q2B::ConfigDataPtr to configure',/cont
    status = 0
  ENDELSE 

    ; ==========
    ; Set EQX
    ; ========== 

  IF N_Elements( eqx ) NE 0 THEN BEGIN 
    s1 = size(eqx)
    s2 = size(*self.data.eqx)
    x = where( s1-s2,nx )
    IF nx NE 0 THEN BEGIN 
      str =  'Size disagreement! '
      IF VarType(eqx) NE 'STRUCTURE' THEN $
       str =  str + ' EQX must be structure ' ELSE BEGIN 
        struct = Tag_Names(eqx,/structure_name)
        IF struct_name NE 'EQX' THEN str =  str + $
         " EQX must be structure of type 'EQX' "
      ENDELSE 
      status = 0
    ENDIF 
    self.eqx =  eqx
    IF recalc_extent THEN BEGIN 
      Message,'--- Extent may have changed! ---- ',/cont
      str =  '    Unexpected results possible if ' + $
       ' EQX structure disagrees with lon/lat extent'
      Message,str,/cont
    ENDIF ELSE recalc_extent = 1
  ENDIF 

  IF recalc_extent THEN BEGIN 
      ; Something has changed that might change the extent of the
      ; data. 
    ; self-> GetExtent
  ENDIF 

  RETURN, status          

END



  ; ===============================================================
  ;
  ; Q2B::ClearData: 
  ;
  ; Allows the user clear the DataPtr.
  ;
  ; ===============================================================

PRO q2b::ClearData
   IF ptr_valid( self.data ) THEN ptr_free, self.data
   IF ptr_valid( self.extent ) THEN ptr_free, self.extent
   self.extent = Ptr_New()
END



  ; ===============================================================
  ;
  ; Q2B::ConfigDataPtr
  ;
  ; Allows the user setup a new data ptr.
  ;
  ; If the data_struct keyword is missing, this routine takes two parameters:
  ; nrecs:  Number of records in the dataptr (required)
  ; ncells: Number of Crosstrack columns (default=76)
  ; If the data_struct keyword is present, it must be of type
  ; 'Q2BDATA', 'QMODEL' or 'RQ2BDATA'.
  ;
  ; Returns 0 for failure, 1 on success.
  ;
  ; ===============================================================

FUNCTION q2b::ConfigDataPtr, $
            nrecs, $ ; This parameter is required
            ncells,$   ; Defaults to 76
            data_struct= data_struct 
   Forward_Function  q2b_str
   status = 0
   IF ptr_valid( self.data ) THEN ptr_free, self.data
   IF N_Elements(data_struct) NE 0 THEN BEGIN 
     name =  Tag_Names( date_struct, /Structure_name)
     IF name EQ 'Q2BDATA' OR $
        name EQ 'QMODEL' OR $
        name EQ 'RQ2BDATA' THEN BEGIN 
       IF name EQ 'QMODEL' THEN BEGIN 
         Self.nrecs = -1
         self.ncells = -1
         self.model = 1
       ENDIF ELSE BEGIN 
         self.nrecs =  N_Elements( data_struct )
         self.ncells =  N_Elements( data_struct[0].su )
         self.model = 0
       ENDELSE 
       self.data =  Ptr_New( data_struct )
       
     ENDIF ELSE BEGIN 
        STR = 'Input Data Structure must be of type Q2BDATA, QMODEL or RQ2BDATA'
        Message,str,/cont
     ENDELSE 
     ; self->GetExtent
     status = 1
   ENDIF ELSE BEGIN 
     IF N_Elements(ncells) EQ 0 THEN ncells = 76
     IF N_Elements(nrecs) NE  0 THEN BEGIN 
       data = Q2B_Str( nrecs, ncells=ncells)
       self.data =  ptr_new( data )
       self.nrecs =  nrecs
       self.ncells = ncells
       self.extent = Ptr_New();
       status = 1
     ENDIF ELSE message,'Parameter NRECS is REQUIRED',/cont
   ENDELSE 
  return,status
END




  ; ===============================================================
  ;
  ; Q2B::GET: 
  ;
  ; Allows the user to get any of the quantities in the
  ; q2b structure.
  ;
  ;
  ; All keywords are Ouput quantities. 
  ; Returns 1. Multiple keywords may be present. 
  ; ===============================================================


FUNCTION q2b::Get, $
               u        = u       , $ 
               v        = v       , $
               lon      = lon     , $
               lat      = lat     , $
               sel      = sel     , $
;               sel2     = sel2    , $
               nambig   = nambig  , $
               qual     = qual    , $
               row      = row     , $
               idx      = idx     , $
               mu       = mu      , $
               mv       = mv      , $
               su       = su      , $
               sv       = sv      , $
               mle      = mle     , $
               smle     = smle    , $
               rflag    = rflag   , $
               tb_h     = tb_h, $
               tb_v     = tb_v, $
               num_tb   = num_tb, $
               tb_rainrate = tb_rainrate, $
               tb_atten = tb_atten, $
               rowtime  = rowtime, $
               eqx      = eqx     , $
               filename = filename, $
               type     = type    , $
               ncells   = ncells  , $
               nrecs    = nrecs   , $
               ambig    = ambig, $
               decimate = decimate, $
               crdecimate  = crdecimate,$
               ExcludeCols = ExcludeCols,$
               data     = data,$
               StartTime = StartTime, $
               EndTime   = EndTime , $
               rainflag    = rainflag, $
               rf_action = rf_action, $
               rf_color  = rf_color, $
               infostruct = infostruct

  Forward_Function  q2b_str

  IF Arg_Present(decimate)     THEN decimate  = self.decimate
  IF Arg_Present(crdecimate)   THEN crdecimate  = self.crdecimate
  IF Arg_Present(ExcludeCols)  THEN ExcludeCols = self.ExcludeCols
  IF Arg_Present(filename)     THEN filename    = self.filename     
  IF Arg_Present(eqx)          THEN eqx         = self.eqx         
  IF Arg_Present(type)         THEN type        = self.type        
  IF Arg_Present(ncells)       THEN ncells      = self.ncells      
  IF Arg_Present(nrecs)        THEN nrecs       = self.nrecs       
  IF Arg_Present(data)         THEN data        = self.data        
  IF Arg_Present(StartTime)    THEN StartTime   = self.StartTime   
  IF Arg_Present(EndTime)      THEN EndTime     = self.EndTime  
  IF Arg_Present(ambig)        THEN Ambig       = self.ambig
  IF Arg_Present(rf_action)    THEN rf_action   = self.rain_flag_action
  IF Arg_Present(rf_color)     THEN rf_color    = self.rain_flag_color
  IF Arg_Present(rainflag)     THEN rainflag    = self.rain_flag


  IF Arg_Present(infostruct) THEN BEGIN 
    infostruct = { Start_Time: self.starttime, $
                   END_Time : self.endtime, $
                   Equator_Xing_Time : self.eqx.date + 'T' +self.eqx.time, $
                   Equator_Xing_Lon  : self.eqx.lon}
  ENDIF 

  IF ptr_valid( self.data) THEN BEGIN 
    IF Arg_Present(u)        THEN u         = (*self.data).u       
    IF Arg_Present(v)        THEN v         = (*self.data).v       
    IF Arg_Present(lon)      THEN lon       = (*self.data).lon     
    IF Arg_Present(lat)      THEN lat       = (*self.data).lat     
    IF Arg_Present(sel)      THEN sel       = (*self.data).sel     
;    IF Arg_Present(sel2)     THEN sel2      = (*self.data).sel2    
    IF Arg_Present(nambig)   THEN nambig    = (*self.data).nambig  
    IF Arg_Present(qual)     THEN qual      = (*self.data).qual    
    IF Arg_Present(row)      THEN row       = (*self.data).row     
    IF Arg_Present(idx)      THEN idx       = (*self.data).idx     
    IF Arg_Present(mu)       THEN mu        = (*self.data).mu      
    IF Arg_Present(mv)       THEN mv        = (*self.data).mv      
    IF Arg_Present(su)       THEN su        = (*self.data).su      
    IF Arg_Present(sv)       THEN sv        = (*self.data).sv      
    IF Arg_Present(mle)      THEN mle       = (*self.data).mle      
    IF Arg_Present(smle)     THEN smle      = (*self.data).smle      
    IF Arg_Present(rflag)    THEN rflag     = (*self.data).rain_flag      
    IF Arg_Present(tb_h)     THEN tb_h      = (*self.data).tb_h
    IF Arg_Present(tb_v)     THEN tb_v      = (*self.data).tb_v
    IF Arg_Present(num_tb)   THEN num_tb    = (*self.data).num_tb
    IF Arg_Present(rowtime) THEN rowtime    = (*self.data).rowtime
    IF Arg_Present(tb_rainrate) THEN tb_rainrate =  (*self.data).tb_rainrate
    IF Arg_Present(tb_atten)     THEN tb_atten   =  (*self.data).tb_atten
    status = 1
  ENDIF ELSE BEGIN 

    Message,'Data Ptr NOT valid, use Q2B::ConfigDataPtr to configure',/cont

    IF Arg_Present(u)         OR $
     Arg_Present(v)           OR $
     Arg_Present(lon)         OR $
     Arg_Present(lat)         OR $
     Arg_Present(sel)         OR $
;     Arg_Present(sel2)        OR $
     Arg_Present(nambig)      OR $
     Arg_Present(qual)        OR $
     Arg_Present(row)         OR $
     Arg_Present(idx)         OR $
     Arg_Present(mu)          OR $
     Arg_Present(mv)          OR $
     Arg_Present(su)          OR $
     Arg_Present(sv)          OR $
     Arg_Present(mle)         OR $
     Arg_Present(smle)        OR $
     Arg_Present(rflag)       OR $
     Arg_Present(tb_h)        OR $
     Arg_Present(tb_v)        OR $
     Arg_Present(num_tb)      OR $
     Arg_Present(rowtime)     OR $
     Arg_Present(tb_rainrate) OR $
     Arg_Present(tb_atten)   THEN status = 0

  ENDELSE 


  RETURN,status
   
END

  ; ==========================================
  ;
  ; Read the input filename
  ;
  ; ==========================================

FUNCTION Q2b::Read, filename

  Forward_Function q2bhdfread, q2bsvhread
  status = 0
  Ptr_Free, self.data 
  data = 0

  CASE self.type OF 
    'SVH' : BEGIN 
      data =  q2bsvhread(filename, $
                        verbose=self.verbose)
      IF Vartype(data) EQ 'STRUCTURE' THEN BEGIN 
        status = 1
        self.type = 'SVH'
      END 
    END

    'HDF': BEGIN 
      data = q2bhdfread(filename,eqx=eqx,$
                        StartTime=StartTime,$
                        EndTime=EndTime, $
                       Verbose=self.Verbose )
      IF Vartype(eqx) EQ 'STRUCTURE' THEN self.eqx = eqx
      IF Vartype(data) EQ 'STRUCTURE' THEN BEGIN 
        status = 1
        self.type = 'HDF'
        self.StartTime = StartTime
        self.EndTime = EndTime
      ENDIF 

    END 
    'RNOAA': BEGIN 
      data = Q2bRNoaaRead(filename,verbose=self.verbose, header=header)
      retstruct = ParseRnoaaHeader(header)
      self.StartTime =  '0000/00/00/00/00'
      self.EndTime =  '0000/00/00/00/00'
      IF VarType(retstruct) EQ 'STRUCTURE' THEN BEGIN 
        self.StartTime =  retstruct.DataStartTime
        self.EndTime =  retstruct.DataEndTime
        tmp = strtrim(str_sep(retstruct.EquatorCrossingtime,'T'),2)
        self.eqx.date =  tmp[0]
        self.eqx.time =  tmp[1]
        self.eqx.lon = float(retstruct.equatorcrossinglongitude)
      ENDIF 
      status =  (VarType(data) EQ 'STRUCTURE')
    END 
    ELSE: BEGIN 

      IF Hdf_IsHdf(DeEnvVar(filename)) THEN BEGIN 
        IF IsQ2b( DeEnvVar(filename) ) THEN BEGIN 
          data = q2bhdfread(filename,eqx=eqx,$
                            StartTime=StartTime,$
                            EndTime=EndTime, verbose=self.verbose)
        ENDIF ELSE BEGIN 
          Message,"Not a Q2B HDF file " + filename,/cont
          return,0
        ENDELSE 
        IF Vartype(eqx) EQ 'STRUCTURE' THEN self.eqx = eqx
        IF Vartype(data) EQ 'STRUCTURE' THEN BEGIN 
          status = 1
          self.type = 'HDF'
          self.StartTime = StartTime
          self.EndTime = EndTime
        ENDIF 
      ENDIF ELSE BEGIN 
          ; Assume it's RSDs RNOAA data.
        data = q2bRNoaaRead(filename, verbose=self.verbose, header=header)
        IF Vartype(data) EQ 'STRUCTURE' THEN BEGIN 
          status = 1
          self.type = 'RNOAA'
          retstruct = ParseRnoaaHeader(header)
          self.StartTime =  '0000/00/00/00/00'
          self.EndTime =  '0000/00/00/00/00'
          IF VarType(retstruct) EQ 'STRUCTURE' THEN BEGIN 
            self.StartTime =  retstruct.DataStartTime
            self.EndTime =  retstruct.DataEndTime
            tmp = strtrim(str_sep(retstruct.EquatorCrossingtime,'T'),2)
            self.eqx.date =  tmp[0]
            self.eqx.time =  tmp[1]
            self.eqx.lon = float(retstruct.equatorcrossinglongitude)
          ENDIF 
        ENDIF 
      ENDELSE 
    END
  ENDCASE 

  IF status THEN BEGIN 
    self.nrecs  =  N_Elements( data )
    self.ncells =  N_Elements( data(0).su )
    self.data   =  ptr_new(data)
    ; self-> GetExtent
  ENDIF ELSE status = 0

  
  return, status
END


  ; ==========================================
  ;
  ; Q2B::GetPlotData
  ; 
  ; Returns one ambiguity of u and v, along with the associated
  ; locations lon,lat, subject to the decimation given by keywords
  ; self.decimate and self.crdecimate. 'crdecimate' dominates if
  ; either of its enties are non-zero and tells how to decimate
  ; columns (crdecimate[0]) and rows (crdecimate[1]) otherwise,
  ; decimate is used.  'decimate' tells how to decimate in simple
  ; ordinal fashion. Both work the same way, telling which vector to
  ; take, if decimate =1, take every vector, decimate=2, take every
  ; other. If crdecimate=[2,3], take every other column and every
  ; third row. A zero entry means do not decimate that dimension, and
  ; has the same effect as an entry of '1.' NB crdecimate = [0,1] =
  ; [1,0] = [1,1] = decimate=1.The positional parameter 'ambig' allows
  ; the user to chose a specific ambiguity, the default is the
  ; 'selected' ambiguity. The ambiguities are numbered 1 through 4 (as
  ; in first, second, ... ) Excludecols tells which columns to
  ; exclude. This keyword is a vector of column subscripts.
  ; 
  ; Limits allows the subsetting of the data by lon/lat range, with
  ; limits[0:1] equaling the lower left lon/lat and limit[2:3] the
  ; upper right, i.e. limit=[lon0,lat0,lon1,lat1] 
  ;
  ; 
  ; All the elements that are excluded using
  ; decimate/CRDecimate/ExcludeCols are returned as NaNs. PlotVect
  ; knows about NaNs.


  ; ==========================================


FUNCTION q2b::GetPlotData,u,v,lon,lat, ambig, $
            limit = limit, $
            decimate=decimate, $
            crdecimate=crdecimate,$
            excludecols = excludecols, $
            Silent=Silent, $
            rf_index=rf_index

   status = 1   
   nodata = -2
   GenericFailure = 0

   Silent = Keyword_Set( Silent )

   Catch,error
   IF error NE 0 THEN BEGIN 
     Catch,/Cancel
     ok = Dialog_Message(!Error_State.msg)
     Message,!Error_State.msg,/cont
;     ok = Dialog_Message(!Err_string)
;     Message,!Err_string,/cont
     return,GenericFailure
   ENDIF 


   ncells = self.ncells
   nrecs = self.nrecs
   west = 0
   IF n_elements(limit) EQ 4 THEN $
     lonrange = FixLonRange( [limit[0],limit[2]],west=west )

   IF N_elements(decimate) NE 0 THEN self.decimate=1> decimate < 99
   IF N_Elements(crdecimate) EQ 2 THEN $
    self.CRDecimate=fix( $
                        [ 0> CRDecimate[0] < self.ncells, $
                          0> CRDecimate[1] < self.nrecs ] )

   crdecimate = self.crdecimate
   decimate = self.decimate

   rf_action = self.rain_flag_action

   junk = where( self.crdecimate, njunk )
   IF njunk NE 0 THEN BEGIN 
     decimate = self.crdecimate
     IF decimate[0] LE 1 AND decimate[1] LE 1 THEN decimate = 1
   ENDIF 

   IF n_elements(excludeCols) NE 0 AND $
      VarType(excludeCols) EQ 'STRING' THEN BEGIN 
     IF strlen( excludeCols) NE 0 THEN self-> SetExcludeCols,excludeCols
   ENDIF 
   PtrToExcludeCols = self.ExcludeCols

   IF ptr_valid( self.data ) THEN BEGIN 

     name = Tag_Names( (*self.data), /Structure_Name )

     IF self.model THEN BEGIN 
       IF name eq 'QMODEL' THEN BEGIN 

         U = *((*self.data).u)
         V = *((*self.data).v)
         LonPar = (*self.data).hdr.lonpar
         LatPar = (*self.data).hdr.latpar
         NLon = (*self.data).hdr.nlon
         NLat = (*self.data).hdr.nlat
         lon = ( findgen( nlon )*lonpar[2] + lonpar[0] )#replicate(1, nlat )
         lat =  replicate(1,nlon)#( findgen(nlat)*LatPar[2]-LatPar[0] )


         IF N_Elements( Limit ) EQ 4 THEN BEGIN 
           xx = where( lon GE limit[0] AND lon LE limit[2] AND $
                       lat GE limit[1] AND lat LE limit[3], nxx )
           IF nxx NE 0 THEN BEGIN 
             u = u[xx]
             v = v[xx]
             lon = lon[xx]
             lat = lat[xx]
             status = 1
           ENDIF ELSE status = NoData
         ENDIF ELSE $
           Message,'Limit must be 4-vector: Ignored',/cont
       ENDIF ELSE BEGIN 
         Message,'Unknown Structure Type ' + name,/cont
         status = GenericFailure
       ENDELSE 
     ENDIF ELSE BEGIN 

         ; Not model data. Could still be RQ2B data. 
       IF name EQ 'RQ2BDATA' THEN BEGIN 
           u =  (*self.data).su
           v =  (*self.data).sv
           lon = (*self.data).lon
           lat = (*self.data).lat
       ENDIF ELSE BEGIN 
         ; Regular old Q2BDATA or Q2B_RNOAA


  ;      IF Ptr_Valid( self.extent ) THEN BEGIN 
  ;        extent = *self.extent
  ;        lonextent = extent[*,*,0]
  ;        latextent = extent[*,*,1]
  ;        IF west THEN BEGIN 
  ;          xx = where( lonextent GT 180, nxx )
  ;          IF nxx NE 0 THEN lonextent(xx) =  lonextent(xx) - 360. 
  ;        ENDIF 
  ;        good = where( lonextent GE lonrange[0] AND lonextent LE lonrange[1] AND $
  ;                      latextent GT limit[1] AND latextent LE limit[3], ngood )
  ;        IF ngood eq 0 THEN BEGIN 
  ;             ; Only care if no data.
  ;          return, 0
  ;        ENDIF 
  ;       ENDIF 

         ; If we got here, there's data to be extracted!
         lon =  (*self.data).lon
         lat =  (*self.data).lat

         IF N_Elements(ambig) NE 0 THEN self.ambig =  0> ambig < 6

         ambig = self.ambig
         IF self.rain_flag NE 0 THEN rf = (*self.data).rain_flag 

         CASE ambig OF 
           0:BEGIN 
             ; The 'selected' ambiguity has been specifically
             ; requested.
             u   = (*self.data).su 
             v   = (*self.data).sv 
           END
           5:BEGIN 
               ; Ambiguity = 5 means use model field.
             u = (*self.data).mu
             v = (*self.data).mv
           END 
           6:BEGIN 
             ; Ambig=6 means use 'Dirth' vectors.
             u   = (*self.data).su2 
             v   = (*self.data).sv2 
           END 
           ELSE: BEGIN 
               ; one of the other ambiguities has been 
               ; specified.
             u   = reform((*self.data).u[ambig-1,*])
             v   = reform((*self.data).v[ambig-1,*])
           END
         ENDCASE 
       ENDELSE ; Come from if name eq 'RQ2BDATA'

       IF n_elements( limit ) NE 0 THEN BEGIN 
         IF n_elements( limit ) EQ 4 THEN BEGIN 
           IF west THEN BEGIN 
             xx = where( lon GT 180, nxx )
             IF nxx NE 0 THEN lon(xx) = lon(xx)-360.
           ENDIF 
           selection = where( $
                             lon GE lonrange[0] AND $
                             lon LE lonrange[1] AND $
                             lat GE limit[1] AND $
                             lat LE limit[3], n_selection )

           IF n_selection EQ 0 THEN BEGIN 
             IF NOT Silent THEN Message,' No data in selected area!',/cont
             return,NoData
           ENDIF 
         ENDIF ELSE BEGIN 
           Message,'Limit keyword must have 4 elements '
           return,GenericFailure
         ENDELSE 
       ENDIF 

       IF n_elements( limit ) NE 0 THEN BEGIN 
         Unpack_Where, lon, selection, c, r
           ; We're really interested only in the rows.
           ; Later, when we get more involved, we'll zero out the
           ; columns of the first and last rows that we're putting in
           ; these arrays that weren't actually selected.
         R=minmax(r)
         u   = u[*,r[0]:r[1]]
         v   = v[*,r[0]:r[1]]
         lon = lon[*,r[0]:r[1]]
         lat = lat[*,r[0]:r[1]]
         IF self.rain_flag GT 0 THEN rf = rf[*,r[0]:r[1]]
       ENDIF 

       IF Ptr_Valid(PtrToExcludeCols) THEN BEGIN 
         excludecols =  0> *PtrToExcludeCols < (self.ncells-1)
         u[excludecols,*] = !values.f_nan
         v[excludecols,*] = !values.f_nan
       ENDIF 

         ; We could just eliminate the unwanted vectors, but it will
         ; be good, I think, to maintain the underlying ncells by nrecs
         ; shape of the arrays. So, until I decide otherwise, I'll just
         ; turn the unwanted ones into NaNs. Will have to revisit later.

       IF n_elements(decimate) EQ 2 THEN BEGIN 

         cols = decimate[0]
         rows = decimate[1]
         s = size(u)
         nc = s[1]
         nr = s[2]

         IF cols GT 1 THEN BEGIN 
           junk = lonarr( nc )
           junk(lindgen( nc/cols )*cols) =  1
           junk = where(junk EQ 0, njunk )
           u[junk,*] = !values.f_nan
           v[junk,*] = !values.f_nan
         ENDIF 

         IF rows GT 1 THEN BEGIN 
           junk = lonarr( nr )
           junk(lindgen( nr/rows )*rows) =  1
           junk = where(junk EQ 0, njunk )
           u[*,junk] = !values.f_nan
           v[*,junk] = !values.f_nan
         ENDIF 

       ENDIF ELSE BEGIN 

         IF decimate GT 1 THEN BEGIN 
           junk = lonarr( n_elements( u ) ) 
           junk( lindgen( n_elements(u)/decimate )*decimate ) =  1
           junk = where( junk EQ 0, njunk)
           u[junk] = !values.f_nan
           v[junk] = !values.f_nan
           junk=0
         ENDIF
       ENDELSE 

       rf_index = -1l;
       IF self.rain_flag NE 0 THEN BEGIN 
         rf_index = where(rf, nrf)
         IF nrf NE 0 THEN BEGIN 
           IF self.rain_flag_action EQ 0 THEN BEGIN 
             u[rf_index] = !values.f_nan
             v[rf_index] = !values.f_nan
           ENDIF 
         ENDIF 
       ENDIF 
     ENDELSE   ; Come from if self.model
   ENDIF ELSE BEGIN 
     Message,$
      'Invalid Ptr to Data: Use Q2B::ConfigDataPtr to  correct',/cont
     status = GenericFailure
   ENDELSE 

   return,status
END


  ; ==========================================
  ;
  ; Q2B::Cleanup  Object destruction routine
  ;
  ; ==========================================

PRO Q2B::Cleanup
  Ptr_Free, self.data
  Ptr_Free, self.extent
  Ptr_Free, self.ExcludeCols
END


  ; ==========================================
  ;
  ; Q2b::GetExtent. Calculates the extent of swath for easier
  ; determination of whether a particular region has data in it. It
  ; does the by first checking if the EQX (equator Crossing) structure
  ; has non-empty date/time strings. If this is true, it calculates
  ; the theoretical swath using QSwathExtent. Otherwise, it computes
  ; something using the actual data. In any case, it puts either the
  ; result or a NULL pointer into 'self.extent'
  ;
  ; ==========================================

PRO Q2B::GetExtent
   IF self.model THEN BEGIN 
     lonpar = (*self.data).hdr.lonpar
     latpar = (*self.data).hdr.latpar
     self.extent = ptr_new([ lonpar[0], latpar[0], lonpar[1], latpar[1] ] )
   ENDIF ELSE BEGIN 
     IF (self.eqx.date NE '' AND self.eqx.time NE '' ) THEN BEGIN 
        extent = QSwathExtent(self.eqx.lon)
       IF Ptr_Valid(self.extent) THEN Ptr_Free,self.extent
       IF NOT Finite(extent[0]) THEN $ 
         self.extent = Ptr_New( extent,/no_copy ) ELSE $
         self.extent = Ptr_New()
     ENDIF ELSE BEGIN 
         ; No equator crossing info. Have to figure it out ourselves.
         ; First try to find a equator crossing longitude. If that can't
         ; be determined then calculate the extent directly from the
         ; data.
;        data = *(self.data)
;        ncells = self.ncells
;        nrecs =  n_elements( data.lat[0,*] )
        
;        lat = data.lat[ncells/2:ncells/2+1,findgen(nrecs/2)*2]
;        lon = data.lon[ncells/2:ncells/2+1,findgen(nrecs/2)*2]
;        findSmallLat = where( abs( lat ) LT 1, nfindSmallLat )
;        IF nfindSmallLat NE 0 THEN BEGIN 

;          unpack_where, lat, findSmallLat, col, row
;          drow =  diffarr( row )
          
;            ; Optimally, the 'small Lat' should fall into two
;            ; areas, one on the ascending side and one on the
;            ; descending side. If there are two, the first will be
;            ; the ascending one. If only one, it may be either.
          
;          extent = QSwathExtent(lon[xx[ilatmin]])
;          IF Finite(extent[0]) THEN $ 
;            self.extent = Ptr_New( extent,/no_copy ) ELSE $
;            self.extent = Ptr_New()
            self.extent = Ptr_New()
;        ENDIF ELSE BEGIN 
         ; We don't have a equator crossing, determine the extent
         ; directly from the data
;          good = where( data.lon[2,*] NE 0. AND $
;                        data.lon[self.ncells/2,*] NE 0. AND $
;                        data.lon[self.ncells-3,*] AND 0. AND $
;                        data.lat[2,*] NE 0. AND $
;                        data.lat[self.ncells/2,*] NE 0. AND $
;                        data.lat[self.ncells-3,*] AND 0., ngood )
;          IF ngood GE self.nrecs/10 THEN BEGIN 
;            extent =  [ $
;                       [[ data.lon[ [2, ncells/2, self.ncells-3],good]]],$
;                       [[ data.lat[ [2, ncells/2, self.ncells-3],good]]] $
;                      ]
;            IF ptr_valid( self.extent ) THEN ptr_free, self.extent
;            self.extent = ptr_new( extent,/no_copy )
;          ENDIF ELSE BEGIN 
;            IF ptr_valid( self.extent ) THEN ptr_free, self.extent
;            self.extent = ptr_new()
;          ENDELSE 

;        ENDELSE 
     ENDELSE 
   ENDELSE 
END


  ; ==========================================
  ;
  ; Q2B::DataInRegion
  ;
  ; ==========================================
FUNCTION Q2B::DataInRegion
  DataInRegion = 0
  
  return,DataInRegion
END




  ; ==========================================
  ;
  ; Q2B::SetExcludeCols
  ;
  ; ==========================================
PRO Q2B::SetExcludeCols, Cols
   catch, error
   IF error NE 0 THEN BEGIN 
     Message,!err_string,/cont
     return
   ENDIF 
   IF N_Params() EQ 1 THEN BEGIN 
     Cols = strcompress(string(cols),/remove_all)
     IF strlen(cols) NE 0 THEN BEGIN 
       tmp = str_sep(cols,',');
       nn = n_elements(tmp)
       FOR i=0,nn-1 DO BEGIN 

         junk = strpos( tmp[i], ':')
         IF junk EQ -1 THEN BEGIN 
           NewCols = fix(tmp[i])
         ENDIF ELSE BEGIN 
           tmp2 = str_sep( tmp[i], ':' )
           x0 = fix(tmp2[0])
           x1 = fix(tmp2[1])

           newCols = indgen( x1-x0 +1 )+x0
         ENDELSE 

         IF i EQ 0 THEN $
           ColList = newcols $
         ELSE $
           ColList =  [ColList, newCols ]
       ENDFOR 
       Ptr_Free, self.ExcludeCols
       self.ExcludeCols = Ptr_New(ColList,/No_Copy)

     ENDIF ELSE BEGIN 
       Ptr_Free,self.ExcludeCols
     ENDELSE 
   ENDIF ELSE BEGIN 
     Ptr_Free,self.ExcludeCols
   ENDELSE 
END


;============================================
; Version
;============================================

FUNCTION q2b::Version

     ; Version number for this class
   rcsid = "$Id$"

     ; Find version number for member objects.
   s=execute( 'tags=tag_names({' + 'q2b' + '})' ) 
   n_tags = n_elements(Tags)
   i = 0

   WHILE i LE n_tags-1 DO BEGIN 

     catch, error
     IF error NE 0 THEN BEGIN 
         ; Ignore 'undefined method' errors
       IF strpos( strupcase(!Error_state.Msg), $
                  "UNDEFINED METHOD" ) NE  -1 THEN BEGIN 
         error = 0
         i = i+1
       ENDIF ELSE return,''
     ENDIF 
     
     IF VarType( self.(i) ) EQ 'OBJECT' THEN BEGIN 
       IF Obj_Valid( self.(i) ) THEN BEGIN 
         V =  Call_Method( "VERSION", self.(i) )
         nv = N_Elements(V)
         IF exist(member_versions) THEN $
            member_versions =  [ member_versions, v ] ELSE $
            member_versions =  v
       ENDIF 
     ENDIF 
     i =  i+1
   ENDWHILE 

     ; find version number for superclasses.
   super = Obj_Class(self,/Super,count=cnt)
        
   IF cnt NE 0 THEN BEGIN 
     WHILE i LE cnt-1 DO BEGIN 
       catch, error
       IF error NE 0 THEN BEGIN 
           ; Ignore 'undefined method' errors
         IF strpos( strupcase(!Error_state.Msg), $
                    "UNDEFINED METHOD" ) NE -1 THEN BEGIN 
           error = 0
           i = i+1
         ENDIF ELSE BEGIN 
           Message,!error_state.msg,/cont
           return,''
         ENDELSE 
       ENDIF 

       V  = call_method("VERSION",super[i])

       IF exist( super_versions ) THEN $
         super_versions =  [super_versions, v ] ELSE $
         super_versions =  v 
       i = i+1

     ENDWHILE 
   ENDIF

   versions =  rcsid

   IF exist(super_versions) THEN $
      versions =  [versions, super_versions]

   IF exist( member_versions ) THEN $
      versions =  [versions, member_versions ] 

   Catch,/cancel
  return,versions(uniq(versions,sort(versions)))
END


  ; ==========================================
  ;
  ; Q2B::GetAll
  ;
  ; ==========================================
PRO Q2B::GetAll, U = U, V=V, Lon=Lon, Lat=Lat, Sel=Sel, $
       Qual=Qual, idx=idx, nambig=nambig, rowtime=rowtime, $
       rownumber=rownumber, SU=SU, SV=SV 

   IF arg_present(lon)    THEN lon = (*self.data).lon
   IF arg_Present(lat)    THEN lat = (*self.data).lat
   IF arg_Present(u)      THEN u   = (*self.data).u
   IF arg_Present(v)      THEN v   = (*self.data).v
   IF arg_Present(su)     THEN su  = (*self.data).su
   IF arg_Present(sv)     THEN sv  = (*self.data).sv
   IF arg_Present(sel)    THEN sel = (*self.data).sel
   IF arg_Present(idx)    THEN idx  = (*self.data).idx
   IF arg_Present(qual)   THEN qual = (*self.data).qual
   IF arg_Present(rownumber) THEN rownumber  = (*self.data).row
   IF arg_Present(rowtime)   THEN rowtime  = (*self.data).rowtime
   IF arg_Present(nambig)    THEN nambig = (*self.data).nambig
   
END

  ; ==========================================
  ; Nrecs
  ;   Return the number of records (rows)
  ; ==========================================

FUNCTION Q2B::Nrecs
   tt = size( (*self.data).lon,/dim)
   return,tt[1]
END



  ; ==========================================
  ; SelfDestruct
  ;   Like it says
  ; ==========================================

Pro Q2B::SelfDestruct
   obj_destroy, self
END

  ; ==========================================
  ;
  ; Object Definition Routine
  ;
  ; ==========================================

PRO q2b__define
   eqx = eqx_str(1)
  junk = { Q2B,$
           filename : '',$        ; Name of file containing data (if there)
           StartTime: '',$        ; Start time of data (yyyy/mm/dd/hh/mm)
           EndTime  : '',$        ; End time of data (yyyy/mm/dd/hh/mm)
           type     : '',$        ; 'SVH', 'HDF' or '???'
           Verbose  : 0l,$
           ambig    : 0l,$
           model    : 0l,$               ; Indicates model data (which means 
                                  ; ncells and nrecs are meaningless)
           ncells   : 0l,$        ; number of crosstrack cells 
                                  ; (see note on 'model')
           nrecs    : 0l,$        ; number of records 
                                  ; (see note on 'model')

           decimate: 0l,$         ;
                                  ; n=>Take every n-th point. Note interaction
                                  ; with keyword 'crdecimate.'

           crdecimate: intarr(2),$; Row Column Decimate. Allows user to 
                                  ; decimate by row and column, rather
                                  ; than simply by linear ordinal. If
                                  ; either of the entries is non-zero,
                                  ; this member variable overrides
                                  ; 'decimate.' This keyword operates
                                  ; exectly like 'decimate,' except it
                                  ; referes to the individual columns
                                  ; and rows. So, crdecimate=[2,3]
                                  ; means take every 2nd column and
                                  ; every 3rd row.  NB, A zero entry
                                  ; means the same as an entry of '1',
                                  ; namely take every element in that
                                  ; dimension, i.e. [0,2] means take
                                  ; every other row but keep all
                                  ; columns. NB crdecimate=[0,1] =
                                  ; [1,0] = [1,1]= decimate=1 = take
                                  ; every vector.


           ExcludeCols: ptr_new(),$ ; pointer to vector of array indices 
                                     ;of columns to exclude from plotting.

           Rain_Flag: 0 , $     ; 0= don't use any rain flag, 
                                ; i.e. don't remove or otherwise
                                ; process data that has been rain
                                ; flagged.


                                  ; 1 = use MP rain flag
                                  ; 2 = use NOF rain flag

           Rain_Flag_Action: 0,$  ; 0 = don't plot rain flagged data
                                  ; 1 = plot with color =
                                  ; rain_flag_color

           rain_flag_color: 0l, $ ; Either the color index or the color 
                                ;itself to be used in plotting rain 
                                ; flagged data (depending on whether
                                ; the display is in 8 bit or 24 bit color.
                    

           eqx      : eqx,$       ; 'EQX' structure 
                                  ; (e.g. {EQX,date:'',time:'',lon:0.})

           data     : ptr_new(),$ ; Pointer to structure of type
                                  ; 'Q2BDATA' or 'QMODEL' or 'RQ2BDATA'
           extent   : ptr_new() } ; pointer to array of size [n,3,2] 
                                  ; which gives a proxy to the
                                  ; location of the actual data. This
                                  ; provides a faster way of
                                  ; determining whether there is any
                                  ; data in any geographical region
                                  ; the user may want


END
