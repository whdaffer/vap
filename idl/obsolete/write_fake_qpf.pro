PRO write_fake_qpf, tables_file

QPF_PROJECT_NAME                = 'QSCAT '
QPF_AO_PROVIDER_CODE            = 'JPL '
QPF_NASDA_TACC_CODE             = 'LASP'
QPF_FILE_FORMAT_VERSION_DATE    = '19980601'
QPF_LENGTH_OF_DATA_RECORD       =  80
QPF_FILE_FORMAT_VERSION_NUMBER =  1
QPF_RESERVED                    = ''


junk = { id: 0, mnemonic:'' }

id2mnemonic =  replicate( junk, 15 )

mnemonics =  ['SCPSTTBL'  ,$ 
              'SCPWOTBL'  ,$ 
              'SCPCLTBL'  ,$ 
              'SCPRCTBL'  ,$ 
              'SCSALLTBL' ,$ 
              'SCSSTTBL'  ,$ 
              'SCSWOTBL'  ,$ 
              'SCSCLTBL'  ,$ 
              'SCSRCTBL'  ,$ 
              'SCENGTBL'  ,$ 
              'SCSTATBL'  ,$ 
              'SCBDATBL'  ,$ 
              'SCBDBTBL'  ,$ 
              'SCBRATBL'  ,$ 
              'SCBRBTBL'  ] 

id2mnemonic.id = indgen(15)
id2mnemonic.mnemonic = mnemonics



openr, lun, tables_file, error=err, /get
IF err NE 0 THEN BEGIN 
  message,!error_state.msg,/cont
  return
END

qpf_number = 9000000
openw, wlun1, 'qpf.db', /get, error=err
IF err NE 0 THEN BEGIN 
  Message,!error_state.msg,/cont
  return
END

qpf_file = 'QPF' + strtrim( qpf_number,2 )
openw, wlun2, qpf_file, error=err, /get
IF err NE 0 THEN BEGIN 
  Message,!error_state.msg,/cont
  return
ENDIF 

first = 1
id = 0l
nwords = 0l
date = ''
fmt_string =  '(A10,1x, a6, 1x, a4, 1x, 2(a8,1x), i4, 1x, i5, 1x, 3(a8,1x), ' + $
 'a3, a8, 1x, a3, 1x, a9, 1x, i6, 1x, i2, 1x, a6)'

WHILE NOT eof( lun ) DO BEGIN 
  rec = ''
  readf, lun, rec
  rec = strtrim( strcompress(rec),2)
  tmp = str_sep(rec,' ')
  reads, tmp[0], id, form='(z2)'
  nwords = long(tmp[1])
  nlines = nwords/16+1
  date = tmp[2]
  x = where( id2mnemonic.id EQ id, nx )
  IF nx ne 1 THEN BEGIN 
    message,"Error: Can't find ID",/cont
    return
  ENDIF 
  mnemonic = id2mnemonic[x[0]].mnemonic
  header_filename = "QPF" + strtrim(qpf_number,2)
  file_format_version_string =  'V00'

  printf, wlun2, header_filename, QPF_PROJECT_NAME, QPF_AO_PROVIDER_CODE,$
   QPF_NASDA_TACC_CODE, file_creation_date, file_creation_time,$
   QPF_LENGTH_OF_DATA_RECORD, nlines,$
   begin_date_of_request, end_date_of_request,$
   QPF_FILE_FORMAT_VERSION_DATE, FILE_FORMAT_VERSION_string,$
   date_of_commanding, pathString, mnemonic, $
   nwords, tableRepetition, QPF_RESERVED, $
   format=fmt_string 
  
;    fprintf(ofp,
;        "QPF%07d %6s %4s %4s %8s %8s %4d %5d %8s %8s %8s V%02d"
;        " %8s %3s %-9s %6d %2d %6s\n",
;        _fileNumber, QPF_PROJECT_NAME, QPF_AO_PROVIDER_CODE,
;        QPF_NASDA_TACC_CODE, file_creation_date, file_creation_time,
;        QPF_LENGTH_OF_DATA_RECORD, numLines,
;        begin_date_of_request, end_date_of_request,
;        QPF_FILE_FORMAT_VERSION_DATE, QPF_FILE_FORMAT_VERSION_NUMBER,
;        date_of_commanding, pathString, cmd->mnemonic,
;        cmd->GetNumWordsInParamFile(), cmd->tableRepetition,
;        QPF_RESERVED)

  readf, lun, rec
  REPEAT BEGIN 
    printf, wlun2, rec
  ENDREP UNTIL strlen(strtrim( strcompress(rec),2) ) EQ 0
    ; finished one, about to make another.
    ; close out processing
  printf, wlun1, id, nwords, date, qpf_file
  qpf_number =  qpf_number+1
  free_lun, wlun2
  qpf_file = 'QPF' + strtrim( qpf_number,2 )
  openw, wlun2, qpf_file, error=err, /get
  IF err NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return
  ENDIF 

ENDWHILE 
free_lun, lun, wlun1, wlun2

END
