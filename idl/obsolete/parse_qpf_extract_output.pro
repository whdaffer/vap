pro parse_qpf_extract_output,file

catch, error
if error ne 0 then begin 
  Message,!error_state.msg,/cont
  return
endif 

tbl = { id: -1, nwords: 0, end_date: '', data: Ptr_new() }

tblarr = replicate(tbl,1);

openr,lun,file,/get,error=err
if err eq 0 then begin 
  openw, wlun, 'tables.out', /get, error=err
  if err ne 0 then begin 
    Message,!error_state.msg
    free_lun, lun
    return
  endif 

  rec = ''
  id = -1 &  lastid=id
  off = -1 &  lastoff=off
  val = 1l
  val1 = 1l &  val2=1l
  nvals = 1000
  valarray = lonarr(nvals)
  while not eof( lun ) do begin 
    readf, lun, rec
    rec =  strtrim( strcompress(rec), 2 )
    if strpos( rec, '@' ) eq -1 and $
       strpos( rec, '#' ) eq -1  AND $
       strlen(rec) NE 0 then begin 
      tmp =  strsplit(  rec, ' ',/extract) 
      date = tmp[0]
      ids  = strcompress(tmp[1],/remove_all)
      reads, strmid( ids, 2,2 ), id, format = '(Z2)'
      offs = strcompress(tmp[2],/remove_all)
      reads, strmid( offs, 2, 4 ), off, format='(Z4)'
      vals = strcompress(tmp[3],/remove_all)
      reads, strmid( vals, 2, 8 ), val, format='(Z8)'

      off = off/2
      if lastid eq id OR $
         lastid EQ -1 THEN BEGIN 
        if lastoff le off then begin 
          if off le nvals-3 then begin 
            v2 = val AND 'ffff'x
            v1 = ishft(val,-16)
            tmp3 = [v1,v2]
            x = where(tmp3 LT 0, nx )
            IF nx NE 0 THEN tmp3(x) =  tmp3(x) + 65536
            valarray[off:off+1] =  tmp3
          endif else begin 
            tmp1 = valarray
            nn = n_elements(tmp1)
            nvals = nvals + 1000
            valarray = lonarr(nvals)
            valarray[0:nn-1] =  tmp1
            tmp1 = 0
          endelse 
        endif 
      endif else begin 
        print,'finnished with table ', lastid, ' starting table ', id

        valarray = valarray[0:lastoff+2-1]
        ids = tblarr.id
        x = where( ids eq lastid, nx )
        if nx eq 0 then begin 
          IF n_elements(tblarr) EQ 1 AND $
           tblarr[0].id eq -1 THEN BEGIN 
            tblarr[0] = replicate( tbl, 1 )
            tblarr[0].id =  lastid
            tblarr[0].nwords = n_elements(valarray)
            tblarr[0].end_date = date
            tblarr[0].data =  Ptr_New( valarray, /no_copy )
          ENDIF ELSE BEGIN 
            newtbl = replicate( tbl, 1 )
            newtbl.id = lastid
            newtbl.nwords = n_elements(valarray)
            newtbl.end_date = date
            newtbl.data =  Ptr_New( valarray, /no_copy )
            tblarr =  [tblarr,newtbl]
          ENDELSE 
        endif else begin 
          nn = n_elements(x)
            ; Only look at the last example of this table
          if tblarr[x[nn-1]].nwords ne n_elements(valarray) then begin 
              ;Same id, different number of words.
            Message,"  Same Id, Different number of words, making new entry",/cont
            newtbl = replicate( tbl, 1 )
            newtbl.id = lastid
            newtbl.nwords = n_elements(valarray)
            newtbl.end_date = date
            newtbl.data =  Ptr_New( valarray, /no_copy )
            tblarr =  [tblarr,newtbl]

          endif else begin 
            ; Compare the actual data
            testdata = *tblarr[x[nn-1]].data
            y = where(testdata-valarray,ny)
            if ny ne 0 then begin 
              Message,'  Same Id, Different table!, Adding',/cont
              newtbl = replicate( tbl, 1 )
              newtbl.id = lastid
              newtbl.nwords = n_elements(valarray)
              newtbl.end_date = date
              newtbl.data =  Ptr_New( valarray, /no_copy )
              tblarr =  [tblarr,newtbl]
            endif 
          endelse 
        endelse 

        nvals = 1000
        valarray = lonarr(nvals)
        v2 = val AND 'ffff'x
        v1 = ishft(val,-16)
        tmp3 = [v1,v2]
        x = where(tmp3 LT 0, nx )
        IF nx NE 0 THEN tmp3(x) =  tmp3(x) + 65536
        valarray[off:off+1] =  tmp3

      endelse 
      lastid = id
      lastoff = off

    endif 
  endwhile 


  for i=0,n_elements(tblarr)-1 do begin 
    printf, wlun,'                                       '
    printf, wlun, tblarr[i].id, tblarr[i].nwords, $
      tblarr[i].end_date, format='(Z2,2x,i4,2x,A26)'
    printf, wlun, *tblarr[i].data, format='(16(Z4,:,2x))'
    ptr_free, tblarr[i].data
  endfor 
  free_lun, lun, wlun

endif else message,!error_state.msg,/cont

end


