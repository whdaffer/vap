PRO make_bill_nye_libserv
;
; use this to create the 05/31 animation of indian ocean.
;
;
CD,'/disk4/vap/indian'
print,' doing indian movie'
f='/disk2/vap/animate/indian/97053112_interp.bin'
ANIMATE_WIND_FIELD,f,[[0,359.,1],[-60.,60.,1]],[15.,135],[-60.,30],$
 vlon=[5.,145.,1.5],$
 vlat=[-70.,40.,1.5],$
 animpar=[640,480,60],/write_pict,/tvsafe,/nologo
;
; --- summer
;
cd,'/disk4/vap/npac/summer/bill-nye'
print,' doing summer movie'
F = '/disk4/vap/npac/summer/97063004_interp.bin'
ANIMATE_WIND_FIELD,F,[[0,359.,1],[-60,60,1.]],[110.,180.],[0.,60],$
 vlon=[100.,190,1.5],$
 vlat=[-10.,60.,1.5],$
 anim=[640,480,60],/write_pict,/tvsafe,/nologo

;
; -- Opal
;

cd,'/disk4/vap/nwpac/evolution/bill-nye'
print,'Doing Opal evolution'
EVOLUTION_movie,'97/06/23/04',10,roi='nwpac',/dateit,$
 animpar=[640,480,120], $
 interp_path='/disk4/vap/nwpac/',/interpolate,$
 anim_path='/disk4/vap/nwpac/evolution/bill-nye',/tvsafe,/nologo,/nomovie

END
