PRO read_vap_anim_ct, red, green, blue
COMMON colors, r_curr, g_curr, b_curr, r_orig, g_orig, b_orig

red =  bytarr(51) &  green=red &  blue=red
openr,1,'$VAP_COLORTABLES/nscat-vap-animation.ct2', error= err
readu,1,red,green,blue
close,1
r_curr =  red &  g_curr= green &  b_curr= blue
r_orig =  red &  g_orig= green &  b_orid= blue

return
END
