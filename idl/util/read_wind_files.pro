;+
; NAME:  read_wind_files
; $Id$
; PURPOSE:  Read Qscat/Nscat files, return data
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:   Data I/O
;
;
;
; CALLING SEQUENCE:  
;
; data = read_wind_files( files, 
;                         Decimate    = Decimate, $
;                         CRDecimate  = CRDecimate, $
;                         ExcludeCols = ExcludeCols, $
;                         StartTime   = StartTime, $
;                         EndTime     = EndTime, $                                       
;                         Help        = Help, $
;                         Nscat       = Nscat, $
;                         use_rf      = use_rf, $
;                         rf_action   = rf_action, $
;                         rf_index    = rf_index
;                         
;
;
; 
; INPUTS:   
;
;        files - vector of fully qualified filenames
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;  decimate - (I) scalar: take every nth vector, i.e. 2=> take
;                 every other, 3=> take every 3rd. (Defaults to 1,
;                 meaning take evey vector)
;
; CRDecimate - (I) 2-vector: 1st entry specifies column, 2nd the
;                 row. e.g. [2,3] means take every other column, every
;                 3rd row. (defaults to [1,1] meaning take every
;                 vector)
;
; ExcludeCols - (I) string: comma seperated list of individual columns
;                 or ranges of colums to exclude. Ranges are indicated
;                 by start:stop (e.g. 33:42 will exclude columns 33
;                 through 42, inclusive). Ex: 0,40:42,72 will exclude
;                 columns 0, 40,41,42 and 72.  (Defaults to '' meaning
;                 exclude NO columns)
;
; Help        - (F) Emit message and exit.
; Nscat       - (F) Expect Nscat data, ignore decimate, CRDecimate and
;                 ExcludeCols. (default=0, meaning, expect Qscat data)
; StartTime   - (O) Earliest Time in Data files
; EndTime     - (O) Latest Time in Data files
;
; use_rf      - (I) flag: 0|1|2 depending on whether you want to use
;                         No rain flagging, the MP flag or the NOF
;                         flag. Default = 1, the MP flag.
;
; rf_action   - (I) flag: 0|1 depending on whether you want skip
;                         plotting rain flagged data or whether you
;                         want to plot it in a different
;                         color. Default=1, plot in a different color.
; rf_index    - (O) array of Longs: Indices of output data which are
;                                   rain flagged. equals -1 if rf_action=0.
;
;
; OUTPUTS:  Success: a [nrecs,4] array with [*,0] = U, [*,1] = v, [*,2] = lon
;                    and [*,3] = lat.
;           Failure: a scalar 0.
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
;    Nscat Flag set: repeatedly call READ_RMGDR_DATA for each file
;    appending the result of that call to the output array.
;
;    Nscat flag NOT set: (the default) iterate over the file array
;    creating a Q2B object for each file. Pass the decimate/CRDecimate
;    and ExcludeCols data to the object, call q->getplotdata() for it
;    to return the data. Get rid of any data flagged as Nans (i.e. the
;    excluded data). Add this data into the output array.
;
;
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.5  1999/10/05 17:26:38  vapuser
; Added ability to read Qmodel files.
;
; Revision 1.4  1998/11/25 22:41:42  vapuser
; fixed some problems calculating start/end time.
;
; Revision 1.3  1998/10/07 00:12:21  vapuser
; Added (Start|End)Time keywords
;
; Revision 1.2  1998/10/06 00:17:09  vapuser
; Added Start/End Time.
;
; Revision 1.1  1998/09/30 17:39:49  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION read_wind_files, files, $
                          Decimate    = Decimate, $
                          CRDecimate  = CRDecimate, $
                          ExcludeCols = ExcludeCols, $
                          StartTime   = StartTime,$
                          EndTime     = EndTime,$
                          Help        = Help, $
                          Nscat       = Nscat, $
                          use_rf      = use_rf, $
                          rf_action   = rf_action, $
                          rf_index    = rf_index

   rcsid="$Id$"
   lf = string(10b)

   hstr1 =             'Usage: data=read_wind_files( files, $ ' 
   hstr1 = hstr1 + lf + "  [, decimate=n , CRDecimate=[m,n] ,$ "
   hstr1 = hstr1 + lf + "    ExcludeCols='exclude_string' , help=0|1 , $" 
   hstr1 = hstr1 + lf + "     Nscat=0|1, use_rf=0|1|2, rf_action=0|1, $ "
   hstr1 = hstr1 + lf + "  rf_index=rf_index ]"
   hstr1 = hstr1 + lf + lf + 'Where...' + lf + lf
   hstr1 = hstr1 + " Files  - vector of fully qualified file names " + lf
   hstr1 = hstr1 + " Decimate - scalar, take every n-th vector, e.g. 2 " + lf
   hstr1 = hstr1 + "   means take every 2nd, 3 means every 3rd " + lf 
   hstr1 = hstr1 + "   (def=1)" + lf
   hstr1 = hstr1 + " CRDecimate (Col/Row Decimate) a 2-vector. " + lf
   hstr1 = hstr1 + "   1st element = Columns, 2nd = rows, e.g. " + lf
   hstr1 = hstr1 + "   CRDecimate=[2,3] means take every other column " + lf
   hstr1 = hstr1 + "   and every third row. (def=[1,1])" + lf
   hstr2 =         " ExcludeCols - a string designating the columns to exclude " + lf
   hstr2 = hstr2 + "   in addition to those excluded by Decimate/CRDecimate. " + lf
   hstr2 = hstr2 + "   The string is a comma separated list of two types " + lf
   hstr2 = hstr2 + "   of entries. "+lf
   hstr2 = hstr2 + "   The first type is a single number, in which case the " + lf
   hstr2 = hstr2 + "   indicated column is excluded. The second is " + lf
   hstr2 = hstr2 + "   two numbers separated by a colon. This second type " + lf
   hstr2 = hstr2 + "   expands into a range with the indicated starting and " + lf
   hstr2 = hstr2 + "   ending columns. Example: ExcludeCols='0,34:42,75 will " + lf 
   hstr2 = hstr2 + "   exclude Columns 0, 34,35,36,37,38,39,40,41,42 and 75" + lf 
   hstr2 = hstr2 + "   (def='')" + lf
   hstr3 =         " Nscat - Flag, if set, expect nscat data " + lf
   hstr3 = hstr3 + "   If set, decimate/CRDecimate/Fill and ExcludeCols are " + lf
   hstr3 = hstr3 + "   ignored. " + lf
   hstr3 = hstr3 + " Help  - flag, if set, emit this message and exit. " + lf 
   hstr4 =         " Returns an array of data if successful, a scaler 0 if not." + lf
   hstr4 = hstr4 + "   The returned array has the form [nrecs,4] " + lf
   hstr4 = hstr4 + "   where u=[*,0], v=[*,1], lon=[*,2] and " + lf
   hstr4 = hstr4 + "   lat=[*,3] " + lf
   hstr5 =         " Use_RF = 0|1|2 depending on whether you want to use: " + lf 
   hstr5 = hstr5 + "   NO flagging (0), or MP flagging (1) or NOF flagging (2) " + lf
   hstr5 = hstr5 + " RF_Action = 0|1 depending on whether you want to:" + lf
   hstr5 = hstr5 + "   SKIP plotting the rain flagged data (0) or " + lf 
   hstr5 = hstr5 + "   PLOT it with a different color (1)" + lf
   hstr5 = hstr5 + " if RF_Action = 1: " + lf 
   hstr5 = hstr5 + "     RF_Index is a Long Array with the indices of the rain " + lf 
   hstr5 = hstr5 + "     flagged data, otherwise, RF_Index = -1"


   hstr = hstr1 + hstr2 + hstr3 + hstr4 + hstr5
   IF keyword_set(Help) THEN BEGIN 
     Print,hstr
     return,0
   ENDIF 

   IF n_params() EQ 0 THEN BEGIN 
     Message, "No Parameters! ",/cont
     Print,hstr
     return,0
   ENDIF 

   nf = n_elements(files)
   nofill = keyword_set(fill) NE 1

   IF keyword_set( Nscat ) THEN BEGIN 

     print,'reading ' + files(0)
     READ_RMGDR_DATA,files(0),uu,vv,llon,llat,mint=mmint,maxt=mmaxt
     FOR f=1,nf-1 DO BEGIN
       print,'reading ' + files(f)
       READ_RMGDR_DATA,files(f),u,v,lon,lat,mint=mint,maxt=maxt
       uu =  [uu,u]
       vv =  [vv,v]
       llon = [llon,lon]
       llat = [llat,lat]
       mmint = [mmint, mint]
       mmaxt =  [mmaxt,maxt]
     ENDFOR 
     StartTime =  min(mint)
     EndTime =  min(mint)
   ENDIF ELSE BEGIN 
     IF n_Elements(decimate) EQ 0 THEN decimate = 1
     IF N_Elements(CRDecimate) NE 2 THEN CRDecimate =  [1,1]
     IF VarType(ExcludeCols) EQ 'UNDEFINED' OR $
        VarType(ExcludeCols) NE 'STRING' THEN ExcludeCols = ''
     StartTime = '0000/00/00/00'
     Endtime = StartTime
     FOR f=0,nf-1 DO BEGIN 
       print,'reading ' + files(f)
       tt = deenvvar(files[f])
       IF Hdf_IsHDF(tt) THEN BEGIN 
         q = obj_new()
         attr = hdfgetattr( tt, attr='SHORTNAME')
         IF VarType(attr) EQ 'STRUCTURE' THEN BEGIN 
           CASE (*attr.value)[0] OF  
             'QSCATVAPMODEL': q = obj_new('qmodel',filename=tt, $
                                          use_rf=use_rf, $
                                          rf_action=rf_action)
             'QSCATL2B'     : q = obj_new('q2b',file=tt, $
                                          decimate=decimate, $
                                          crdecimate=crdecimate, $
                                          excludecols=excludecols, $
                                          use_rf=use_rf, $
                                          rf_action=rf_action)
             ELSE: BEGIN 
               Message,"Can't identify type of HDF file ",/cont
               print,'  Attribute "ShortName" must be either QSCATL2B or QSCATVAPMODEL'
             END 
           ENDCASE
         ENDIF ELSE $
           Message,"Can't get SHORTNAME attribute from HDF file",/cont
           
       ENDIF ELSE BEGIN 
         q = obj_new('q2b',file=tt, $
                     decimate=decimate, $
                     crdecimate=crdecimate, $
                     excludecols=excludecols, $
                     use_rf=use_rf, $
                     rf_action=rf_action)
       ENDELSE 
       IF obj_valid(q) THEN BEGIN 
         s = q-> GetPlotData(u,v,lon,lat, rf_index=rf_index)
         s = q-> Get( StartTime = ST, Endtime=ET)
         IF strlen(st) NE 0 THEN BEGIN 
           tmp = str_sep(st,'/')
           IF fix(tmp[0]) NE 0 THEN BEGIN 
             tmp2 = str_sep(startTime,'/')
             IF fix(tmp2[0]) NE 0 THEN $
               StartTime = Min( [StartTime, st]) ELSE $
               StartTime = st
           ENDIF 
         ENDIF 
         IF strlen(et) NE 0 THEN BEGIN 
           tmp = str_sep(et,'/')
           IF fix(tmp[0]) NE 0 THEN BEGIN 
             tmp2 = str_sep(EndTime,'/')
             IF fix(tmp2[0]) NE 0 THEN $
               EndTime = Max( [EndTime, et]) ELSE $
               EndTime = et
           ENDIF 
         ENDIF 
         IF nofill THEN BEGIN 

           good = where( finite(u) AND finite(v), ngood )
           
           IF rf_index[0] NE -1 THEN BEGIN 
             ii = lonarr(n_elements(u))
             ii[rf_index] =  1
             rf_index =  where( ii[good], nn) &  ii=0
           ENDIF 

           IF ngood NE 0 THEN BEGIN 
             u = u[good]
             v = v[good]
             lon = lon[good]
             lat = lat[good]
             IF exist(uu) THEN BEGIN 
               rrf_index =  [rrf_index, $
                             rf_index + n_elements(uu)]
               uu = [uu,u]
               vv = [vv,v]
               llon = [llon,lon]
               llat = [llat,lat]

             ENDIF ELSE BEGIN 
               uu = u
               vv = v
               llon = lon
               llat = lat
               rrf_index = rf_index;
             ENDELSE 
           ENDIF 
         ENDIF ELSE BEGIN 
           ; fill.
;           IF exist(uu) THEN BEGIN 
;             uu =  [ [[uu]],[[u]] ]
;             vv =  [ [[vv]],[[v]] ]
;             llon =  [ [[llon]],[[lon]] ]
;             llat =  [ [[llat]],[[lat]] ]
;           ENDIF ELSE BEGIN 
;             uu = u
;             vv = v
;             llon = lon
;             llat = lat
;           ENDELSE 
         ENDELSE 
         Obj_destroy,q
       ENDIF ELSE $
         Message,"Can't Read file " + files(f),/cont

     ENDFOR 
   ENDELSE 
   data =  [ [temporary(uu)],   [temporary(vv)], $
             [temporary(llon)], [temporary(llat)] ]
   rf_index =  temporary(rrf_index)
   return, data
END





