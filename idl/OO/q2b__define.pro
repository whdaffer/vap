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
  ; 
  ; INPUTS:   
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
  ; OPTIONAL INPUTS:  
  ;
  ;
  ;	
  ; KEYWORD PARAMETERS:  
  ;
  ;          type       :
  ;          data_struct: A structure of type Q2b of Qmodel
  ;          eqx        : A structure of type Eqx
  ;          filename   : Read this file
  ;          decimate   : n: take every n-th vector
  ;          crdecimate : [p,q] take every p-th column of every n-th row
  ;          ExcludeCols: string: '0,23,25,72' means exclude columns
  ;                       0,23,24,25 and 72
  ;
  ;
  ; OUTPUTS:   if successful, an object of type Q2b, otherwise, nothing.
  ;
  ;
  ;
  ; OPTIONAL OUTPUTS:  
  ;
  ;
  ;
  ; COMMON BLOCKS:  
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
  ;
  ; MODIFICATION HISTORY:
  ; $Log$
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

  ; ===============================================================
  ;
  ; Q2B::Init: 
  ;
  ; Initializes Q2B object
  ; 
  ; Relevant Quantities
  ; 4: number of ambiguities
  ; n: number of crosstrack colums (nominally 76)
  ; m: Number of records
  ; 
  ; All keywords are Input quantities. 
  ; Returns 1 for success, 0 for failure.
  ;
  ; The routine first checks for the 'filename' keyword. If present,
  ; it tries to read that file. The status returned by the read
  ; routine is passed along by this routine.
  ; 
  ; The routine then checks for the 'data_struct' keyword. If the
  ; data_struct keyword is present, it tries to take the data from it,
  ; subject to the restriction that the datum pointed to by the
  ; keyword is a structure of type 'Q2BDATA.' (see the file
  ; q2b_str.pro for a definition) If this keyword is not present, it
  ; will check for the first positional parameter. If it is present,
  ; then ALL 14 must be present.
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

FUNCTION q2b::init, $
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
            data_struct = data_struct, $ ; structure of type Q2BDATA
            eqx=eqx,$                    ; structure of type EQX
            filename=filename ,$
            decimate=decimate ,$
            crdecimate=crdecimate ,$
            ExcludeCols=ExcludeCols


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

  
  status = 0
  self.filename = '<No Name>'
  self.type = 'UNKOWN'
  IF N_Elements(filename) NE 0 THEN BEGIN 
    IF N_Elements(type) NE 0 THEN self.type = StrupCase(type)
    self.filename = filename
    status = self->Read(filename)
  ENDIF ELSE BEGIN 
    IF N_Elements( data_struct ) NE 0 THEN BEGIN 
      name = Tag_Names( data_struct, /structure_name ) 
      IF name EQ 'Q2BDATA' OR name EQ 'QMODEL' THEN BEGIN 
        self.nrecs =  N_Elements(data_struct)
        self.ncells = N_Elements(data_struct(0).su)
        self.data =  ptr_new( data_struct )
        IF N_Elements( eqx ) NE 0 THEN self.eqx = eqx
        status = 1
      ENDIF ELSE BEGIN
        STR = 'Input Data Structure must be of type Q2BDATA or QMODEL'
        Message,str,/cont
      ENDELSE 
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
END


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
               decimate    =  decimate,$
               crdecimate  = crdecimate, $
               ExcludeCols = ExcludeCols
   

  recalc_extent = 0

  IF N_Elements(crdecimate) EQ 2 THEN BEGIN 
    self.crdecimate = [ 0> crdecimate[0] < self.ncells, $
                        0> crdecimate[1] < self.nrecs ]
  ENDIF 
  IF N_Elements(decimate) NE 0 THEN self.decimate = (1> decimate < 99)
  IF N_Elements(ExcludeCols) NE 0 AND $
    VarType(ExcludeCols) EQ 'STRING' THEN BEGIN  
    self-> SetExcludeCols,ExcludeCols
  ENDIF     
    
  IF ptr_valid( self.data ) THEN BEGIN 
    status = 1
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
  ; 'Q2BDATA'.
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
     self.nrecs =  N_Elements( data_struct )
     self.ncells =  N_Elements( data_struct[0].su )
     self.data =  Ptr_New( data_struct )
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
               sel2     = sel2    , $
               nambig   = nambig  , $
               qual     = qual    , $
               row      = row     , $
               idx      = idx     , $
               mu       = mu      , $
               mv       = mv      , $
               su       = su      , $
               sv       = sv      , $
               eqx      = eqx     , $
               filename = filename, $
               type     = type    , $
               ncells   = ncells  , $
               nrecs    = nrecs   , $
               decimate = decimate, $
               crdecimate  = crdecimate,$
               ExcludeCols = ExcludeCols
               data     = data

  Forward_Function  q2b_str
  status = 0
  IF ptr_valid( self.data) THEN BEGIN 
    IF Arg_Present(u)        THEN u         = (*self.data).u       
    IF Arg_Present(v)        THEN v         = (*self.data).v       
    IF Arg_Present(lon)      THEN lon       = (*self.data).lon     
    IF Arg_Present(lat)      THEN lat       = (*self.data).lat     
    IF Arg_Present(sel)      THEN sel       = (*self.data).sel     
    IF Arg_Present(sel2)     THEN sel2      = (*self.data).sel2    
    IF Arg_Present(nambig)   THEN nambig    = (*self.data).nambig  
    IF Arg_Present(qual)     THEN qual      = (*self.data).qual    
    IF Arg_Present(row)      THEN row       = (*self.data).row     
    IF Arg_Present(idx)      THEN idx       = (*self.data).idx     
    IF Arg_Present(mu)       THEN mu        = (*self.data).mu      
    IF Arg_Present(mv)       THEN mv        = (*self.data).mv      
    IF Arg_Present(su)       THEN su        = (*self.data).su      
    IF Arg_Present(sv)       THEN sv        = (*self.data).sv      
    IF Arg_Present(filename) THEN filename  = self.filename
    IF Arg_Present(eqx)      THEN eqx       = self.eqx      
    IF Arg_Present(type)     THEN type      = self.type     
    IF Arg_Present(ncells)   THEN ncells    = self.ncells   
    IF Arg_Present(nrecs)    THEN nrecs     = self.nrecs    
    IF Arg_Present(data)     THEN data      = self.data
    status = 1
  ENDIF ELSE $
    Message,'Data Ptr NOT valid, use Q2B::ConfigDataPtr to configure',/cont

  IF Arg_Present(decimate) THEN decimate  = self.decimate
  IF Arg_Present(crdecimate) THEN crdecimate  = self.crdecimate
  IF Arg_Present(ExcludeCols)  THEN ExcludeCols = self.ExcludeCols

  return,status
   
END

  ; ==========================================
  ;
  ; Read the input filename
  ;
  ; ==========================================

FUNCTION q2b::read, filename

  Forward_Function q2bhdfread, q2bsvhread
  status = 0
  Ptr_Free, self.data 
  data = 0

  CASE self.type OF 
    'SVH' : BEGIN 
      data =  q2bsvhread( filename )
      IF Vartype(data) EQ 'STRUCTURE' THEN BEGIN 
        status = 1
        self.type = 'SVH'
      END 
    END

    'HDF': BEGIN 
      data = q2bhdfread(filename,eqx=eqx)
      IF Vartype(eqx) EQ 'STRUCTURE' THEN self.eqx = eqx
      IF Vartype(data) EQ 'STRUCTURE' THEN BEGIN 
        status = 1
        self.type = 'HDF'
      ENDIF 

    END 

    ELSE: BEGIN 

      IF Hdf_IsHdf(filename) THEN BEGIN 
        data = q2bhdfread(filename,eqx=eqx)
        IF Vartype(eqx) EQ 'STRUCTURE' THEN self.eqx = eqx
        IF Vartype(data) EQ 'STRUCTURE' THEN BEGIN 
          status = 1
          self.type = 'HDF'
        ENDIF 
      ENDIF ELSE BEGIN 
          ; Assume it's SVH data.
          ; Message,'Can only read SVH and HDF data currently!',/cont
        data = q2bsvhread(filename)
        IF Vartype(data) EQ 'STRUCTURE' THEN BEGIN 
          status = 1
          self.type = 'SVH'
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

  ; All the elements that are excluded using
  ; decimate/CRDecimate/ExcludeCols are returned as NANs. PlotVect
  ; knows about NaNs.


  ; ==========================================


FUNCTION q2b::GetPlotData,u,v,lon,lat, ambig, $
            limit = limit, $
            decimate=decimate, $
            crdecimate=crdecimate,$
            excludecols = excludecols, $
            Silent=Silent
   status = 1
   
   Silent = Keyword_Set( Silent )

   Catch,error
   IF error NE 0 THEN BEGIN 
     Catch,/Cancel
;     ok = Dialog_Message(!Error_State.msg)
;     Message,!Error_State.msg,/cont
     ok = Dialog_Message(!Err_string)
     Message,!Err_string,/cont
     return,0
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
     
;     IF Ptr_Valid( self.extent ) THEN BEGIN 
;       extent = *self.extent
;       lonextent = extent[*,*,0]
;       latextent = extent[*,*,1]
;       IF west THEN BEGIN 
;         xx = where( lonextent GT 180, nxx )
;         IF nxx NE 0 THEN lonextent(xx) =  lonextent(xx) - 360. 
;       ENDIF 
;       good = where( lonextent GE lonrange[0] AND lonextent LE lonrange[1] AND $
;                     latextent GT limit[1] AND latextent LE limit[3], ngood )
;       IF ngood eq 0 THEN BEGIN 
;            ; Only care if no data.
;         return, 0
;       ENDIF 
;      ENDIF 

       ; If we got here, there's data to be extracted!
     lon =  (*self.data).lon
     lat =  (*self.data).lat

     IF N_Elements(ambig) eq 0 THEN ambig = 0
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
           return,0
         ENDIF 
       ENDIF ELSE BEGIN 
         Message,'Limit keyword must have 4 elements '
         return,0
       ENDELSE 
     ENDIF 
     IF ambig EQ 0 THEN BEGIN 
         ; The 'selected' ambiguity has been specifically requested.
       u   = (*self.data).su 
       v   = (*self.data).sv 
     ENDIF ELSE BEGIN 
       IF ambig LT 5 THEN BEGIN 
         u   = reform((*self.data).u[ambig-1,*])
         v   = reform((*self.data).v[ambig-1,*])
       ENDIF ELSE BEGIN 
         ; Ambiguity = 5 means use model field.
         u = (*self.data).mu
         v = (*self.data).mv
       ENDELSE 
     ENDELSE 

     IF n_elements( limit ) NE 0 THEN BEGIN 
       Unpack_Where, lon, selection, c, r
         ; We're really interested only in the rows.
         ; Later, when we get more involved, we'll zero out the
         ; columns of the first and last rows that we're putting in
         ; these arrays that weren't actually selected.
       R=minmax(r)
       u   = u(*,r[0]:r[1])
       v   = v(*,r[0]:r[1])
       lon = lon(*,r[0]:r[1])
       lat = lat(*,r[0]:r[1])
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
   ENDIF ELSE BEGIN 
     Message,$
      'Invalid Ptr to Data: Use Q2B::ConfigDataPtr to  correct',/cont
     status = 0
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
  ; Q2b::GetExtent. Calculates the extent of swath for easier determination
  ; of whether a particular region has data in it. It does the by
  ; first checking if the EQX (equator Crossing) structure has
  ; non-empty date/time strings. If this is true, it calculates the
  ; theoretical swath using QSwathExtent. Otherwise, it computes
  ; something using the actual data. In any case, it puts either the
  ; result or a NULL pointer into 'self.extent'
  ;
  ; ==========================================

PRO Q2B::GetExtent
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
      data = *(self.data)
      ncells = self.ncells
      lat = data.lat[ncells/2:ncells/2+1,*]
      lon = data.lon[ncells/2:ncells/2+1,*]
      xx = where( lat AND lon, nxx )
      IF nxx NE 0 THEN BEGIN 
        latmin = min(abs(lat(xx)),ilatmin)
        IF latmin LE 0.25 THEN BEGIN 
          extent = QSwathExtent(lon[xx[ilatmin]])
          IF Finite(extent[0]) THEN $ 
            self.extent = Ptr_New( extent,/no_copy ) ELSE $
            self.extent = Ptr_New()
        ENDIF ELSE BEGIN 
         ; We don't have a equator crossing, determine the extent
         ; directly from the data
          good = where( data.lon[2,*] NE 0. AND $
                        data.lon[self.ncells/2,*] NE 0. AND $
                        data.lon[self.ncells-3,*] AND 0. AND $
                        data.lat[2,*] NE 0. AND $
                        data.lat[self.ncells/2,*] NE 0. AND $
                        data.lat[self.ncells-3,*] AND 0., ngood )
          IF ngood GE self.nrecs/10 THEN BEGIN 
            extent =  [ $
                       [[ data.lon[ [2, ncells/2, self.ncells-3],good]]],$
                       [[ data.lat[ [2, ncells/2, self.ncells-3],good]]] $
                      ]
            IF ptr_valid( self.extent ) THEN ptr_free, self.extent
            self.extent = ptr_new( extent,/no_copy )
          ENDIF ELSE BEGIN 
            IF ptr_valid( self.extent ) THEN ptr_free, self.extent
            self.extent = ptr_new()
          ENDELSE 
        ENDELSE 
      ENDIF ELSE self.extent = ptr_new()
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
   Tags = Tag_Names(self)
   n_tags = n_elements(Tags)
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
       V =  Call_Method( "VERSION", self.(i) )
       nv = N_Elements(V)
       IF exist(member_versions) THEN $
          member_versions =  [ member_versions, v ] ELSE $
          member_versions =  v
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
         ENDIF ELSE return,''
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
  return,versions
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
           type     : '',$        ; 'SVH', 'HDF' or '???'
           ncells   : 0l,$        ; number of crosstrack cells
           nrecs    : 0l,$        ; number of records 

           decimate: 0l,$
                                  ; takes all points. Note interaction
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
                                  ; means take every other column and
                                  ; every third row.  NB, A zero entry
                                  ; means the same as an entry of '1',
                                  ; namely take every element in that
                                  ; dimension, i.e. [0,2] means take
                                  ; every other row but keep all
                                  ; columns. NB crdecimate=[0,1] =
                                  ; [1,0] = [1,1]= decimate=1 = take
                                  ; every vector.

           ExcludeCols: ptr_new(),$ ; pointer to vector of array indices 
                                    ; of columns to exclude from plotting.

           eqx      : eqx,$       ; 'EQX' structure 
                                  ; (e.g. {EQX,date:'',time:'',lon:0.})
           data     : ptr_new(),$ ; Pointer to structure of type
                                  ; 'Q2BDATA'.
           extent   : ptr_new() } ; pointer to array of size [n,3,2] 
                                  ; which gives a proxy to the
                                  ; location of the actual data. This
                                  ; provides a faster way of
                                  ; determining whether there is any
                                  ; data in any geographical region
                                  ; the user may want


END

