
pro findrecs,node,minlat,maxlat,minlon,maxlon,n,wvci,minj,maxj
;
;  determine the list of WVC's in a given lat/lon box
;
common GRID, ni, nj, latw, lona, lonw

if node lt 0. then node=node + 360.
if node gt 360. then node=node - 360.

lonw = lona + node
jj=where(lonw gt 360.,knt)
if knt gt 0 then lonw(jj) = lonw(jj) - 360.

; Use the 'where' to find the WVC's that fall in the box

idx = where(((latw ge minlat) and (latw le maxlat) and $
  (lonw ge minlon) and (lonw le maxlon)),knt)

if knt ne 0 then begin
    wvcrow = intarr(knt)
    wvccol = intarr(knt)
    wvcrow = fix(idx/nj) + 1
    wvccol = (idx MOD nj) + 1
;
; Find ranges of wvccol for which wvcrow = constant
;
    mini = min(wvcrow)
    maxi = max(wvcrow)
    n = maxi - mini + 1      ; this is the MAX number of rows, since
                             ; there could be 2 segments in subset
    ii = -1
    wvcitmp=intarr(n) & minjtmp = wvcitmp & maxjtmp=wvcitmp
    for i = mini,maxi do begin
        jj  = where(wvcrow eq i,cnt)
        if cnt ne 0 then begin
            ii = ii + 1
            wvcitmp(ii) = i
            minjtmp(ii) = min(wvccol(jj))
            maxjtmp(ii) = max(wvccol(jj))
        endif
    endfor
    n = ii + 1
    wvci=intarr(n) & minj = wvci & maxj=wvci
    wvci = wvcitmp(0:ii)
    minj = minjtmp(0:ii)
    maxj = maxjtmp(0:ii)
endif else begin
;    print,"No WVC''s found in the subset box"
    n=0
    wvci=0
    minj=0
    maxj=0
endelse
return
end
