FUNCTION vaptime2jd, vaptime
  idldt = vaptime2idldt(vaptime)
  return,idldt.julian
END
