PRO goes2doc__define

  ; Used in reading the Goes .doc files  maintained by Scott Gennari
  ; on the Goes 8/10 archive on explorer.arc.nasa.gov and
  ; rsd.gsfc.nasa.gov


  junk = {GOES2DOC, $
          SpaceCraftId : 0,$
          SensorProcID : 0,$
          iscan : bytarr(4),$
          spsTime      : intarr(6),$ ;year,doy,hour,min,sec,milli
          headerTime   : intarr(6),$ ; ditto
          trailerTime  : intarr(6),$ ; ditto
          risct : 0l,$ ; relative output scan seq             
          aisct : 0l,$ ; absolute number of the current scan  
          insln : 0l,$ ; northern-most vis dectect scan line  
          iwfpx : 0l,$ ; western-most visible pixel number    
          iefpx : 0l,$ ; eastern-most visible pixel number    
          infln : 0l,$ ; norther-most visible pixel number    
          isfln : 0l,$ ; southern-most visible pixel number   
          imdpx : 0l,$ ; visbile pixel number at 0 degrees    
          imdln : 0l,$ ; scan line number at 0 degrees        
          imdct : 0l,$ ; output scan number at 0 degrees      
          igvln : 0l,$ ; visible detector scan line number    
          igvpx : 0l,$ ; pixel number which intersect sub sat 
          subla : 0.0d,$ ; sub-sat point latitude              
          sublo : 0.0d,$ ; sub-sat point longitude             
          czone : 0,$ ; current compensation zone           
          v1phy : 0,$ ; physical detector number            
          g1cnt : 0,$ ; grid 1 active entry count           
          g2cnt : 0,$ ; grid 2 active entry count           
          pbias : 0,$ ; east-west grid bias                 
          lbias : 0,$ ; north-south grid bias               
          idber : 0.0d,$ ; current raw data bit error rate     
          range : 0.0d,$ ; most recently computed range        
          gpath : 0.0d,$ ; range calibration ground path delay 
          xmsne : 0.0d,$ ; call tower range calib value        
          istim : 0,$ ; current line scan time (ms)         
          ifram : 0,$ ; current frame counter               
          imode : 0,$ ; current imaging mode                
          a1 : "" ,$ ;  IMC set identifier (4 ASCII char)                 
          a5 : 0.0d,$ ; reference longitude + East (rad)                  
          a6 : 0.0d,$ ; reference radial distance from nominal (km)       
          a7 : 0.0d,$ ; reference latitude + North (rad)                  
          a8 : 0.0d,$ ; reference orbit yaw (rad)                         
          a9 : 0.0d,$ ; reference attitude: roll (rad)                    
          a10 : 0.0d,$ ; reference attitude: pitch (rad)                   
          a11 : 0.0d,$ ; reference attitude: yaw (rad)                     
          epochTime : intarr(6),$ ;year,doy,hour,min,sec,mill
          a  : dblarr(337) } ; A[0:13] == 0 doing it this way just makes it easier.

          ; This is what each is in this
          ; array. The number after 'a' is the
   ;       ; index into the 'a' array.

;          a14 : 0.0d,$ ; IMC set enable time from epoch (min)
;          a15 : 0.0d,$ ; spacecraft compensation: 0.0d,$ roll (rad)
;          a16 : 0.0d,$ ; spacecraft compensation: 0.0d,$ pitch (rad)
;          a17 : 0.0d,$ ; spacecraft compensation: 0.0d,$ yaw (rad)      
;          a18 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a19 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a20 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a21 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a22 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a23 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a24 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a25 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a26 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a27 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a28 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a29 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a30 : 0.0d,$ ; change in longitude from ref + E (rad)  
;          a31 : 0.0d,$ ; change in radial distance from ref (km) 
;          a32 : 0.0d,$ ; change in radial distance from ref (km) 
;          a33 : 0.0d,$ ; change in radial distance from ref (km) 
;          a34 : 0.0d,$ ; change in radial distance from ref (km) 
;          a35 : 0.0d,$ ; change in radial distance from ref (km) 
;          a36 : 0.0d,$ ; change in radial distance from ref (km) 
;          a37 : 0.0d,$ ; change in radial distance from ref (km) 
;          a38 : 0.0d,$ ; change in radial distance from ref (km) 
;          a39 : 0.0d,$ ; change in radial distance from ref (km) 
;          a40 : 0.0d,$ ; change in radial distance from ref (km) 
;          a41 : 0.0d,$ ; change in radial distance from ref (km) 
;          a42 : 0.0d,$ ; sine geocentric latitude, total (no unit)
;          a43 : 0.0d,$ ; sine geocentric latitude, total (no unit)
;          a44 : 0.0d,$ ; sine geocentric latitude, total (no unit)
;          a45 : 0.0d,$ ; sine geocentric latitude, total (no unit)
;          a46 : 0.0d,$ ; sine geocentric latitude, total (no unit)
;          a47 : 0.0d,$ ; sine geocentric latitude, total (no unit)
;          a48 : 0.0d,$ ; sine geocentric latitude, total (no unit)
;          a49 : 0.0d,$ ; sine geocentric latitude, total (no unit)
;          a50 : 0.0d,$ ; sine geocentric latitude, total (no unit)
;          a51 : 0.0d,$ ; sine orbit yaw, total (no unit)         
;          a52 : 0.0d,$ ; sine orbit yaw, total (no unit)         
;          a53 : 0.0d,$ ; sine orbit yaw, total (no unit)         
;          a54 : 0.0d,$ ; sine orbit yaw, total (no unit)         
;          a55 : 0.0d,$ ; sine orbit yaw, total (no unit)         
;          a56 : 0.0d,$ ; sine orbit yaw, total (no unit)         
;          a57 : 0.0d,$ ; sine orbit yaw, total (no unit)         
;          a58 : 0.0d,$ ; sine orbit yaw, total (no unit)         
;          a59 : 0.0d,$ ; sine orbit yaw, total (no unit)         
;          a60 : 0.0d,$ ; daily solar rate                        
;          a61 : 0.0d,$ ; exponential start time from epoch (min) 
;          a62 : 0.0d,$ ; exponential magnitude (rad)             
;          a63 : 0.0d,$ ; exponential time constant (min)         
;          a64 : 0.0d,$ ; constant, mean attitude angle           
;          a65 : 0.0d,$ ; number of sinusiods and phase angles (n)
;          a66 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a67 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a68 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a69 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a70 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a71 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a72 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a73 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a74 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a75 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a76 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a77 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a78 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a79 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a80 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a81 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a82 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a83 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a84 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a85 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a86 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a87 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a88 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a89 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a90 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a91 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a92 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a93 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a94 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a95 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a96 : 0.0d,$ ; number of monomial sinusoids            
;          a97 : 0.0d,$ ; order of applicable sinusoids for 1st   
;          a98 : 0.0d,$ ; order of 1st monomial sinusoids         
;          a99 : 0.0d,$ ; magnitude of monomial sinusoids         
;          a100 : 0.0d,$ ; phase angle of monomial sinusoids       
;          a101 : 0.0d,$ ; angle from epoch where monomial is zero 
;          a102 : 0.0d,$ ; order of applicable sinusoids for 2nd   
;          a103 : 0.0d,$ ; order of 2nd monomial sinusoids         
;          a104 : 0.0d,$ ; magnitude of 2nd monomial sinusoids     
;          a105 : 0.0d,$ ; phase angle of 2nd monomial sinusoids   
;          a106 : 0.0d,$ ; angle from epoch where 2nd monomial is z
;          a107 : 0.0d,$ ; order of applicable sinusoids for 3rd   
;          a108 : 0.0d,$ ; order of 3rd monomial sinusoids         
;          a109 : 0.0d,$ ; magnitude of 3rd monomial sinusoids     
;          a110 : 0.0d,$ ; phase angle of 3rd monomial sinusoids   
;          a111 : 0.0d,$ ; angle from epoch where 3rd monomial is z
;          a112 : 0.0d,$ ; order of applicable sinusoids for 4th   
;          a113 : 0.0d,$ ; order of 4th monomial sinusoids         
;          a114 : 0.0d,$ ; magnitude of 4th monomial sinusoids     
;          a115 : 0.0d,$ ; phase angle of 4th monomial sinusoids   
;          a116 : 0.0d,$ ; angle from epoch where 4th monomial is z
;          a117 : 0.0d,$ ; exponential magnitude (rad)             
;          a118 : 0.0d,$ ; exponential time constant (min)         
;          a119 : 0.0d,$ ; constant, mean attitude angle           
;          a120 : 0.0d,$ ; number of sinusiods and phase angles (n)
;          a121 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a122 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a123 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a124 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a125 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a126 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a127 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a128 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a129 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a130 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a131 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a132 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a133 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a134 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a135 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a136 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a137 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a138 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a139 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a140 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a141 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a142 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a143 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a144 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a145 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a146 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a147 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a148 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a149 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a150 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a151 : 0.0d,$ ; number of monomial sinusoids            
;          a152 : 0.0d,$ ; order of applicable sinusoids for 1st   
;          a153 : 0.0d,$ ; order of 1st monomial sinusoids         
;          a154 : 0.0d,$ ; magnitude of monomial sinusoids         
;          a155 : 0.0d,$ ; phase angle of monomial sinusoids       
;          a156 : 0.0d,$ ; angle from epoch where monomial is zero 
;          a157 : 0.0d,$ ; order of applicable sinusoids for 2nd   
;          a158 : 0.0d,$ ; order of 2nd monomial sinusoids         
;          a159 : 0.0d,$ ; magnitude of 2nd monomial sinusoids     
;          a160 : 0.0d,$ ; phase angle of 2nd monomial sinusoids   
;          a161 : 0.0d,$ ; angle from epoch where 2nd monomial is z
;          a162 : 0.0d,$ ; order of applicable sinusoids for 3rd   
;          a163 : 0.0d,$ ; order of 3rd monomial sinusoids         
;          a164 : 0.0d,$ ; magnitude of 3rd monomial sinusoids     
;          a165 : 0.0d,$ ; phase angle of 3rd monomial sinusoids   
;          a166 : 0.0d,$ ; angle from epoch where 3rd monomial is z
;          a167 : 0.0d,$ ; order of applicable sinusoids for 4th   
;          a168 : 0.0d,$ ; order of 4th monomial sinusoids         
;          a169 : 0.0d,$ ; magnitude of 4th monomial sinusoids     
;          a170 : 0.0d,$ ; phase angle of 4th monomial sinusoids   
;          a171 : 0.0d,$ ; angle from epoch where 4th monomial is z
;          a172 : 0.0d,$ ; exponential magnitude (rad)             
;          a173 : 0.0d,$ ; exponential time constant (min)         
;          a174 : 0.0d,$ ; constant, mean attitude angle           
;          a175 : 0.0d,$ ; number of sinusiods and phase angles (n)
;          a176 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a177 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a178 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a179 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a180 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a181 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a182 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a183 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a184 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a185 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a186 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a187 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a188 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a189 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a190 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a191 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a192 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a193 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a194 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a195 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a196 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a197 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a198 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a199 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a200 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a201 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a202 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a203 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a204 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a205 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a206 : 0.0d,$ ; number of monomial sinusoids            
;          a207 : 0.0d,$ ; order of applicable sinusoids for 1st   
;          a208 : 0.0d,$ ; order of 1st monomial sinusoids         
;          a209 : 0.0d,$ ; magnitude of monomial sinusoids         
;          a210 : 0.0d,$ ; phase angle of monomial sinusoids       
;          a211 : 0.0d,$ ; angle from epoch where monomial is zero 
;          a212 : 0.0d,$ ; order of applicable sinusoids for 2nd   
;          a213 : 0.0d,$ ; order of 2nd monomial sinusoids         
;          a214 : 0.0d,$ ; magnitude of 2nd monomial sinusoids     
;          a215 : 0.0d,$ ; phase angle of 2nd monomial sinusoids   
;          a216 : 0.0d,$ ; angle from epoch where 2nd monomial is z
;          a217 : 0.0d,$ ; order of applicable sinusoids for 3rd   
;          a218 : 0.0d,$ ; order of 3rd monomial sinusoids         
;          a219 : 0.0d,$ ; magnitude of 3rd monomial sinusoids     
;          a220 : 0.0d,$ ; phase angle of 3rd monomial sinusoids   
;          a221 : 0.0d,$ ; angle from epoch where 3rd monomial is z
;          a222 : 0.0d,$ ; order of applicable sinusoids for 4th   
;          a223 : 0.0d,$ ; order of 4th monomial sinusoids         
;          a224 : 0.0d,$ ; magnitude of 4th monomial sinusoids     
;          a225 : 0.0d,$ ; phase angle of 4th monomial sinusoids   
;          a226 : 0.0d,$ ; angle from epoch where 4th monomial is z
;          a227 : 0.0d,$ ; exponential magnitude (rad)             
;          a228 : 0.0d,$ ; exponential time constant (min)         
;          a229 : 0.0d,$ ; constant, mean attitude angle           
;          a230 : 0.0d,$ ; number of sinusiods and phase angles (n)
;          a231 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a232 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a233 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a234 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a235 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a236 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a237 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a238 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a239 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a240 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a241 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a242 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a243 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a244 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a245 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a246 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a247 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a248 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a249 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a250 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a251 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a252 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a253 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a254 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a255 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a256 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a257 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a258 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a259 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a260 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a261 : 0.0d,$ ; number of monomial sinusoids            
;          a262 : 0.0d,$ ; order of applicable sinusoids for 1st   
;          a263 : 0.0d,$ ; order of 1st monomial sinusoids         
;          a264 : 0.0d,$ ; magnitude of monomial sinusoids         
;          a265 : 0.0d,$ ; phase angle of monomial sinusoids       
;          a266 : 0.0d,$ ; angle from epoch where monomial is zero 
;          a267 : 0.0d,$ ; order of applicable sinusoids for 2nd   
;          a268 : 0.0d,$ ; order of 2nd monomial sinusoids         
;          a269 : 0.0d,$ ; magnitude of 2nd monomial sinusoids     
;          a270 : 0.0d,$ ; phase angle of 2nd monomial sinusoids   
;          a271 : 0.0d,$ ; angle from epoch where 2nd monomial is z
;          a272 : 0.0d,$ ; order of applicable sinusoids for 3rd   
;          a273 : 0.0d,$ ; order of 3rd monomial sinusoids         
;          a274 : 0.0d,$ ; magnitude of 3rd monomial sinusoids     
;          a275 : 0.0d,$ ; phase angle of 3rd monomial sinusoids   
;          a276 : 0.0d,$ ; angle from epoch where 3rd monomial is z
;          a277 : 0.0d,$ ; order of applicable sinusoids for 4th   
;          a278 : 0.0d,$ ; order of 4th monomial sinusoids         
;          a279 : 0.0d,$ ; magnitude of 4th monomial sinusoids     
;          a280 : 0.0d,$ ; phase angle of 4th monomial sinusoids   
;          a281 : 0.0d,$ ; angle from epoch where 4th monomial is z
;          a282 : 0.0d,$ ; exponential magnitude (rad)             
;          a283 : 0.0d,$ ; exponential time constant (min)         
;          a284 : 0.0d,$ ; constant, mean attitude angle           
;          a285 : 0.0d,$ ; number of sinusiods and angles (n)      
;          a286 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a287 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a288 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a289 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a290 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a291 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a292 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a293 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a294 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a295 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a296 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a297 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a298 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a299 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a300 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a301 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a302 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a303 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a304 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a305 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a306 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a307 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a308 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a309 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a310 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a311 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a312 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a313 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a314 : 0.0d,$ ; magnitude of n (1-15) order sinusoid    
;          a315 : 0.0d,$ ; phase angle of n (1-15) order sinusoid  
;          a316 : 0.0d,$ ; number of monomial sinusoids            
;          a317 : 0.0d,$ ; order of applicable sinusoids for 1st   
;          a318 : 0.0d,$ ; order of 1st monomial sinusoids         
;          a319 : 0.0d,$ ; magnitude of monomial sinusoids         
;          a320 : 0.0d,$ ; phase angle of monomial sinusoids       
;          a321 : 0.0d,$ ; angle from epoch where monomial is zero 
;          a322 : 0.0d,$ ; order of applicable sinusoids for 2nd   
;          a323 : 0.0d,$ ; order of 2nd monomial sinusoids         
;          a324 : 0.0d,$ ; magnitude of 2nd monomial sinusoids     
;          a325 : 0.0d,$ ; phase angle of 2nd monomial sinusoids   
;          a326 : 0.0d,$ ; angle from epoch where 2nd monomial is z
;          a327 : 0.0d,$ ; order of applicable sinusoids for 3rd   
;          a328 : 0.0d,$ ; order of 3rd monomial sinusoids         
;          a329 : 0.0d,$ ; magnitude of 3rd monomial sinusoids     
;          a330 : 0.0d,$ ; phase angle of 3rd monomial sinusoids   
;          a331 : 0.0d,$ ; angle from epoch where 3rd monomial is z
;          a332 : 0.0d,$ ; order of applicable sinusoids for 4th   
;          a333 : 0.0d,$ ; order of 4th monomial sinusoids         
;          a334 : 0.0d,$ ; magnitude of 4th monomial sinusoids     
;          a335 : 0.0d,$ ; phase angle of 4th monomial sinusoids   
;          a336 : 0.0d } ; angle from epoch where 4th monomial is zero  

END
