;+
; NAME:  obhisto__define.pro
; $Id$
; PURPOSE:  Defines an object useful in manipulating and storing
;          histogram data.
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  OO
;
;
;
; CALLING SEQUENCE:  
;
;        obhisto=Obj_New('obhisto', Data, $
;                [BinSize = BinSize, $
;                [Min     = Min, $
;                [Max     = Max, $
;                [NBins   = Nbins ,$
;                [Help    = Help]]]]]
;
;
; 
; INPUTS:   
;
;       Data: The data to be histogramed.
;
;
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:  
;
;   BinSize : The binsize, default=1
;   Min     : The Minimum for the histogram to consider, def=min(data)
;   Max     : The Maximum for the histogram to consider, def=max(data)
;   NBins   : The Number of bins in the histogram, an
;             alternative to binsize
;   Help    : a Help message.
;
;
;
;
; OUTPUTS:  If successful, an object of type obhisto.
;           If not, the NULL object
;
;
;
; OPTIONAL OUTPUTS:  NONE
;
;
;
; COMMON BLOCKS:  NONE
;
;
;
; SIDE EFFECTS:  Use of Heap memory. Any object using this one must
;                destroy it in its cleanup method.
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
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.2  1998/10/01 17:53:19  vapuser
; Modified 'version' method so that it will report
; the versions of member classes. Put in some error handling
; so that it'll ignore calls to undefined 'version' methods.
;
; Revision 1.1  1998/10/01 16:46:09  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
;============================================

FUNCTION ObHisto::Init, Data, $
                BinSize = BinSize, $
                Min     = Min, $
                Max     = Max, $
                NBins   = Nbins ,$
                Help    = Help

  self.data = Ptr_New()
  self.Histo = Ptr_New()
  self.XHisto = Ptr_New()

  Catch, error
  IF error NE 0 THEN BEGIN 
    Catch,/cancel
    ok =  Dialog_Message( !error_state.msg )
    Message,!error_state.msg,/cont
;    ok =  Dialog_Message( !err_string )
;    Message,!err_string,/cont

    return,0
  ENDIF 

  IF Keyword_Set(Help) THEN self-> SelfHelp
     
  IF n_Elements(Data) NE 0 THEN BEGIN 
    IF Ptr_Valid( data[0] ) THEN $
      self.data = Data ELSE $
      self.Data = Ptr_New( data )

    IF N_elements(Min) EQ 0 THEN Min = Min(Data)
    IF N_elements(Max) EQ 0 THEN Max = Max(Data)

    self.Min = Min
    self.Max = Max
    self.nBins = 0l

    range = abs(self.Max-self.Min)

    IF N_Elements(BinSize) EQ 0 THEN BEGIN 
      IF N_Elements(Nbins) EQ 0 THEN $
       self.NBins = 200 ELSE $
       self.nBins = nBins
      self.BinSize = range/(float(self.nBins)>  1)
    ENDIF ELSE self.binsize = binsize
    self-> Histo

  ENDIF ELSE BEGIN

    IF N_elements(Min)     eq 0 THEN Min     = 1.e10
    IF N_elements(Max)     eq 0 THEN Max     = -1.e10
    IF N_elements(BinSize) eq 0 THEN BinSize = -1
    IF N_elements(nBins)   eq 0 THEN nBins   =  0

    self.min     = Min
    self.max     = Max
    self.Binsize = BinSize
    self.NBins   = nBins

  ENDELSE 
  RETURN,1
END


;============================================
; Cleanup
;============================================

PRO ObHisto::Cleanup
   Ptr_Free, self.Data
   Ptr_Free, self.Histo
   Ptr_Free, self.XHisto
   Ptr_Free, self.Help
END

;============================================
; Histo
;============================================
PRO ObHisto::Histo, Input = Input

  IF Ptr_Valid(self.data) THEN BEGIN 
    IF n_Elements(Input) EQ 0 THEN BEGIN 
      hist = histogram( *self.Data, min=self.min, max=self.max, $
                           binsize=self.binsize )
    ENDIF ELSE BEGIN 
      hist = histogram( *self.Data, min=self.min, max=self.max, $
                           binsize=self.binsize, Input=Input )
    ENDELSE 
    self.nBins = N_Elements(hist)
    IF self.nBins NE 0 THEN BEGIN 

      IF Ptr_Valid( self.Histo ) THEN $
       *self.Histo = hist ELSE $
        self.Histo = Ptr_New(hist,/no_copy) 

      x = findgen(self.nBins)*self.BinSize + self.Min

      IF Ptr_Valid( self.XHisto ) THEN $
       *self.XHisto = x ELSE $
        self.XHisto = Ptr_New(x,/no_copy)
    ENDIF 
  ENDIF 
    
END



;============================================
; Set Routine
;============================================

PRO ObHisto::Set, $
           Data       = Data, $  
           Min        = Min, $   
           Max        = Max, $   
           BinSize    = BinSize,$
           nBins      = nBins, $ 
           Append     = Append,$ 
           DeleteData = DeleteData,$
           Quiet      = Quiet

   recalc = 0
   IF N_elements(Quiet) NE 0 THEN self.Quiet = Quiet

   IF N_Elements(Min) NE 0 THEN BEGIN 
     self.Min = Min
     recalc = 1
   END

   IF N_Elements(Max) NE 0 THEN BEGIN 
     self.Max = Max
     recalc = 1
   ENDIF 

   CASE 1 OF 
     N_Elements(Binsize) NE 0 : BEGIN 
       self.Binsize = Binsize
       recalc = 1
     END

     N_Elements(nBins) NE 0 : BEGIN 
       self.nBins = nBins
       range = ( abs(self.Max-self.Min) > 1.e-6 )
       self.BinSize = range/(nBins> 1)
       recalc = 1
     END
     ELSE :
   ENDCASE 


   IF N_Elements(Data) NE 0 THEN BEGIN 
       ; Check to see if binsize/min/max/nbins make sense
     IF self.max LT self.min THEN BEGIN 
        self.min=Min(Data)
        self.Max=Max(Data)
        recalc = 1
     ENDIF 

     range = abs(self.Max-self.Min)> 1.e-6

     IF self.BinSize eq -1 THEN BEGIN 
       IF self.Nbins eq 0 THEN self.Nbins = 100
       self.BinSize = range/(self.nBins> 1)
       recalc = 1
     ENDIF


     IF Keyword_Set(Append) THEN BEGIN 
       IF NOT Ptr_Valid( self.Data ) THEN BEGIN 
           ; Nothing to accumlate. Just set the pointer
           ; to the new data.
         self.Data = Ptr_New(Data)
         self->Histo
       ENDIF ELSE BEGIN 
           ; Object already has a valid data pointer. How about a
           ; valid Histogram pointer?
         IF Ptr_Valid( self.Histo) THEN BEGIN 

             ; Valid Histo array. 
             ; Faster to histogram the new data only and accumulate, if
             ; possible. If anything else has changed, have to do it
             ; from scratch
           IF NOT recalc THEN BEGIN 
               ; Save the old Data Array
             olddata =  *self.Data
             *self.Data = Data
             h = *self.histo
             self-> Histo, input = h
               ; Now, concatenate the two data arrays
             *self.Data =  [olddata, *self.Data]
           ENDIF ELSE BEGIN 
               ; Something's Changed! Append the two data arrays and
               ; calculate from scratch
             olddata =  *self.Data
             *self.Data = Data
               ; Now, concatenate the two data arrays
             *self.Data =  [olddata, *self.Data]
             self-> Histo
           ENDELSE 

         ENDIF ELSE BEGIN 
             ; No Valid Histogram Pointer. Append the two arrays and
             ; do it from scratch
           olddata =  *self.Data
           *self.Data = Data
             ; Now, concatenate the two data arrays
           *self.Data =  [olddata, *self.Data]
           self-> Histo
         ENDELSE 
       ENDELSE 
     ENDIF ELSE BEGIN 
         ; Not Appending
       IF Ptr_Valid(self.Data) THEN BEGIN 
         Ptr_Free,self.Data
         self.Data = Ptr_New(Data)
       ENDIF ELSE BEGIN 
         self.Data = Ptr_New(Data)
       ENDELSE 
       self->Histo
     ENDELSE 
   ENDIF ELSE BEGIN 
       ; No new data. Check to see if anything else changed that
       ; requires a recalculation
     IF recalc THEN self-> Histo
   ENDELSE 

   IF Keyword_Set( DeleteData ) THEN BEGIN 
     Ptr_Free, self.Data
     self.Data = Ptr_New()
   ENDIF

   IF Ptr_Valid( self.Data ) THEN BEGIN 
     IF n_elements( *self.data ) GT 1.e6 AND $
       NOT self.Quiet THEN Message,'Data Array has more than 1 million elements!',/cont
   ENDIF 

END


;============================================
; Get Routine
;============================================

PRO ObHisto::Get, $
           Data    = Data, $
           Histo   = Histo,$
           XHisto  = XHisto,$
           Min     = Min,$
           Max     = Max,$
           BinSize = Binsize,$
           nBins   = nBins

   Data    = self.Data
   Histo   = self.Histo
   XHisto  = self.XHisto
   Min     = self.Min
   Max     = self.Max
   BinSize = self.Binsize
   nBins   = self.nBins

END



;============================================
; SelfHelp routine
;============================================

PRO ObHisto::SelfHelp
  IF NOT Ptr_Valid( self.help ) THEN BEGIN   
    help_array =  strarr(500) &  i=-1
    i = i+1 &  help_array[i] =  ' Object Histogram ========================== '
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  '   Creation: ==============================='
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  "     YourVariableNameHere = Obj_New('ObHisto',data) "
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  '   Data Members: ============================'
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  '     self.Data    - Pointer to input data'
    i = i+1 &  help_array[i] =  '     self.Min     - Minimum to consider when Histograming data   '
    i = i+1 &  help_array[i] =  '                      (def = Min(data))  '
    i = i+1 &  help_array[i] =  '     self.Max     - Maximum to consider when Histograming data'
    i = i+1 &  help_array[i] =  '                      (def = Max(data)'
    i = i+1 &  help_array[i] =  '     self.Binsize - BinSize to use when Histograming data  '
    i = i+1 &  help_array[i] =  '                       (def=(abs(Max-Min)/nBins, See note for nBins)'
    i = i+1 &  help_array[i] =  '     self.nBins   - Alternate way of specifying BinSize. In ALL cases,'
    i = i+1 &  help_array[i] =  '                    this object checks for binsize FIRST. Finding none,'
    i = i+1 &  help_array[i] =  '                    it will check for nBins and calculate BinSize from'
    i = i+1 &  help_array[i] =  '                    it and the range. If neither nBins nor BinSize are'
    i = i+1 &  help_array[i] =  '                    specified, the object will default Nbins to 200 and'
    i = i+1 &  help_array[i] =  '                    revert to the previous case.'
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  '     self.Histo - Pointer to histogram of data, subject to restrictions'
    i = i+1 &  help_array[i] =  '                    imposed by Min/Max/(BinSize/nBins)'
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  '     self.XHisto - Pointer to X array for self.Histo '
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  '   Methods: ================================='
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  '     Get - Get any/all of Data, Histo, XHisto, Min, Max, BinSize, nBins'
    i = i+1 &  help_array[i] =  ' 	    (Calling Method: obj->Get, item=return_variable'
    i = i+1 &  help_array[i] =  '                      where Item equals one or more f Data, Histo,'
    i = i+1 &  help_array[i] =  '                      XHisto, Min, Max, Binsize or nBins )'
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  ' 	     NB: Data, Histo and XHisto are returned as POINTERS and'
    i = i+1 &  help_array[i] =  ' 	     should be checked before dereferencing! Probably not a'
    i = i+1 &  help_array[i] =  " 	     worry with 'data' but Histo is set to a NULL POINTER if"
    i = i+1 &  help_array[i] =  ' 	     the call to the native IDL Histogram fails for some '
    i = i+1 &  help_array[i] =  ' 	     reason.'
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  '     Set - Set any/all of Data, Min, Max, BinSize, nBins'
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  ' 	    (Calling Method: obj->Set, item=return_variable'
    i = i+1 &  help_array[i] =  ' 	             where Item equals one of Data, Min, Max, Binsize'
    i = i+1 &  help_array[i] =  ' 	             or nBins )'
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  ' 	     After any call to Set, Histogram is recalculated subject to'
    i = i+1 &  help_array[i] =  ' 	     the restrictions imposed by the newly input value. (NB. if'
    i = i+1 &  help_array[i] =  ' 	     your input value is the same as the one already there, it'
    i = i+1 &  help_array[i] =  ' 	     will still recaculate!)'
    i = i+1 &  help_array[i] =  " 	     "
    i = i+1 &  help_array[i] =  " 	     Additional 'flag' Keywords to Set:"
    i = i+1 &  help_array[i] =  " 	     "
    i = i+1 &  help_array[i] =  " 	     Append - MUST APPEAR in conjunction with the Data keyword"
    i = i+1 &  help_array[i] =  " 	       This keyword implements the same functionality as the "
    i = i+1 &  help_array[i] =  " 	       'Input' keyword to RSIs native Histogram function, that is, it "
    i = i+1 &  help_array[i] =  " 	       allows for the accumlation of the Histograming results. If "
    i = i+1 &  help_array[i] =  " 	       the additional keyword 'DeleteData' is NOT present, this "
    i = i+1 &  help_array[i] =  " 	       keyword will cause the input Data array to be appended to the "
    i = i+1 &  help_array[i] =  " 	       array pointed to in member self.data. Needless to say, this "
    i = i+1 &  help_array[i] =  " 	       allows that array to grow without bound. One is encoraged to "
    i = i+1 &  help_array[i] =  " 	       only use this keyword in conjunction with the 'DeleteData' "
    i = i+1 &  help_array[i] =  " 	       keyword"
    i = i+1 &  help_array[i] =  " 	       When the array pointed to by self.data has grown to over "
    i = i+1 &  help_array[i] =  " 	       1 million elements, warnings will be issued at each call to "
    i = i+1 &  help_array[i] =  " 	       histogram self->Histo unless the 'Quiet' flag is set. You're"
    i = i+1 &  help_array[i] =  " 	       on you own if you set the Quiet flag."
    i = i+1 &  help_array[i] =  " 	     "
    i = i+1 &  help_array[i] =  " 	      DeleteData - Deletes the data after calculation of the histogram"
    i = i+1 &  help_array[i] =  " 	      Quiet - Suppresses most messages"
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  '     Histo - Recalulate the histogram'
    i = i+1 &  help_array[i] =  ' 	    ( obj->Histo )'
    i = i+1 &  help_array[i] =  ' '
    i = i+1 &  help_array[i] =  '     SelfHelp - Display this message'
    i = i+1 &  help_array[i] =  ' 	    ( obj->SelfHelp )'
    help_array = help_array[0:i]
    self.help =  ptr_new(help_array)
  ENDIF 
  

  SaveDevice = !d.Name
  Set_plot,'X'
  XDisplayFile,'',text= *self.help
  set_plot,SaveDevice
END


;============================================
; Version
;============================================

FUNCTION ObHisto::Version

     ; Version number for this class

   rcsid = "$Id$"

     ; Find version number for member objects.
   s=execute( 'tags=tag_names({' + 'OBHISTO' + '})' ) 
   n_tags = n_elements(Tags)
   i = 0

   WHILE i LE n_tags-1 DO BEGIN 

     catch, error
     IF error NE 0 THEN BEGIN 
         ; Ignore 'undefined method' errors
       IF strpos( strupcase(!Error_state.Msg), $
                  "UNDEFINED METHOD" ) NE -1 THEN BEGIN 
         error = 0
         i = i+1
       ENDIF ELSE return,''
     ENDIF 
     
     IF VarType( self.(i) ) EQ 'OBJECT' THEN BEGIN 
       IF obj_valid( self.(i) ) THEN BEGIN 
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


;============================================
; ClearAll - delete all pointers
;============================================

PRO ObHisto::ClearAll
   IF Ptr_Valid(self.Data) THEN BEGIN 
     Ptr_Free, self.Data 
     self.Data =  Ptr_New()
   ENDIF 
   IF Ptr_Valid( self.Histo) THEN BEGIN 
     Ptr_Free, self.Histo
     self.Histo = Ptr_New()
   ENDIF 
   self.min = 1e6
   self.max = -1e6
END

;============================================
; Definition Routine
;============================================

PRO ObHisto__define

  junk = {OBHISTO, $
         Data    : Ptr_New(),$ ; Ptr To data to be histogramed

         Histo   : Ptr_New(),$ ; And the histogram

         XHisto  : Ptr_New(),$ ; Ptr to 'X' Array that will go 
                               ; with 'histo.' Always calculated
                               ; within, useful in plotting.

         Help    : Ptr_New(),$ ; Contains Help message

         Quiet   : 0L ,$       ; Noisesome message suppressor

         BinSize : 0.0, $      ; BinSize to be used in 
                               ; Histogram (def=range(data)/nbins)
                               ; Nbins defaults to 200 

         Min     : 0.,$        ; Min Data to consider (def=MIn(Data))

         Max     : 0., $       ; Max Data to consider (def=max(data))

         Nbins   : 0l       }  ; Nbins in histogram. Default=200 unless
                               ; BinSize is define, in which case it
                               ; equals however many bins Historgram
                               ; returns.

END
