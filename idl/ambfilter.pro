pro ambfilter,ilo,ihi,jlo,jhi,isel,npass,speedamb,diramb,nambig,wt
;
; Select the ambiguities which minimize the difference between the selected
; wind vector and the vector median of the box specified by wt centered
; on each cell.  This is equivalent to minimizing the sum of norms
; |v - a(i)| where v is the selected vector, and a(i) are the selected
; vectors in the filter box.
;
; INPUTS:
;  ilo,ihi = column range (ie., cross track range) to apply median filter to
;  jlo,jhi = row range (ie., along track range) to apply median filter to
;  isel = current selection array.
;  npass = the number of passes to perform (unless 0 changes occur first)
;          a negative value means use the magnitude and print a message if
;          there are still changes in the last iteration.
;  speedamb,diramb = all the ambiguities (4,ncols,nrows)
;  nambig = array of numbers of ambiguities for each cell
;  wt = array of weights to use (also determines the size of the filter)
;
; OUTPUTS
;  isel is updated
nisel = size(isel)
nwt = size(wt)
ncols = nisel(1)
nrows = nisel(2)
nwtcols = nwt(1)
nwtrows = nwt(2)
isel1 = isel

xamb = speedamb*cos(diramb*!dtor)
yamb = speedamb*sin(diramb*!dtor)

msgflag = 0
if npass lt 0 then begin
  npass = -npass
  msgflag = 1
endif

mark = nambig gt 0
newmark = mark
p = 1
done = 0
while done eq 0 do begin

xsel = getselected(xamb,isel)
ysel = getselected(yamb,isel)
;print,xsel,ysel

for i=ilo,ihi do begin
  for j=jlo,jhi do begin

    ; Figure out the index ranges to use (accounting for boundaries)

    if i lt nwtrows/2 + ilo then begin
      i1 = ilo
      i2 = i + nwtrows/2
      iwt2 = nwtrows - 1
      iwt1 = iwt2 - (i2 - i1)
    endif
    if i gt ihi - nwtrows/2 then begin
      i1 = i - nwtrows/2
      i2 = ihi
      iwt1 = 0
      iwt2 = i2 - i1
    endif
    if i ge nwtrows/2 + ilo and i le ihi-nwtrows/2 then begin
      i1 = i - nwtrows/2
      i2 = i + nwtrows/2
      iwt1 = 0
      iwt2 = nwtrows - 1
    endif

    if j lt nwtcols/2 + jlo then begin
      j1 = jlo
      j2 = j + nwtcols/2
      jwt2 = nwtcols - 1
      jwt1 = jwt2 - (j2 - j1)
    endif
    if j gt jhi - nwtcols/2 then begin
      j1 = j - nwtcols/2
      j2 = jhi
      jwt1 = 0
      jwt2 = j2 - j1
    endif
    if j ge nwtcols/2 + jlo and j le jhi-nwtcols/2 then begin
      j1 = j - nwtcols/2
      j2 = j + nwtcols/2
      jwt1 = 0
      jwt2 = nwtcols - 1
    endif

    submark = mark(j1:j2,i1:i2)
    jm = where(submark ne 0, jmcount)
    if jmcount ne 0 then begin
      subx = xsel(j1:j2,i1:i2)
      suby = ysel(j1:j2,i1:i2)
      subwt = wt(jwt1:jwt2,iwt1:iwt2)
      k = where(nambig(j1:j2,i1:i2) ne 0, num)
      if num eq 0 then begin
        isel1(j,i) = 0
      endif else begin
        ; Find the ambiguity which yields the smallest sum of norms
        mindif = 1e6
        mmin = -1
        for m=0,nambig(j,i)-1 do begin
	  normsum = total(subwt*sqrt((xamb(m,j,i)-subx(k))^2 + $
			             (yamb(m,j,i)-suby(k))^2))
	  if normsum lt mindif then begin
	    mindif = normsum
	    mmin = m
          endif
        endfor
        if isel1(j,i) ne mmin+1 then begin
          isel1(j,i) = mmin+1
	  newmark(j,i) = 1
        endif else begin
          newmark(j,i) = 0
        endelse
;      print,i,j,mmin+1,mindif,(isel(j,i) eq isel1(j,i))
      endelse
    endif
  endfor
endfor

p = p + 1
j = where(isel ne isel1, num)
print,num,' flipped'

;for i=0,nrows-1 do begin
;  print,isel(*,i),isel1(*,i),mark(*,i),format='(3(i2,11i1))'
;endfor

if num eq 0 then begin
  done = 1
endif else if p gt npass then begin
  done = 1
  if msgflag eq 1 then print,'maximum iteration count exceeded in ambfilter'
endif

isel = isel1
mark = newmark

endwhile

end

