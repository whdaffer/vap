FUNCTION ir2erf,im

      newim = byte( 255*( 0.5*(errorf(4.d*scale(im,max=1024,/double)-2) +1) ) ) 
return,newim
END
