#!/usr/bin/perl
#
#
# $Id$
#
# Some routines out of vap_perl.pm which are useful enough to live in
# their own place.  
#
#
# Note on unfortunate historical accident.
#
#
#
# When I first started writing the idl/perl code to run VAP, I was
# using the time format YYYYMMDDThh:mm:ss.ccc, which I took to calling
# 'vaptime' in the Perl code. Unfortunately, the IDL code is much
# easier to write if I separate all the fields in whatever time format
# I use by the same separator, instead of the 3 (one a null) that this
# format requires. So I made 'yyyy/mm/dd/hh/mm/ss' the default in the
# IDL code and took to calling that 'vaptime' as well. Now, I have two
# 'vaptimes', one for the perl code and one for the IDL code. To make
# matters infinitely worse, in the perl code that has to construct the
# IDL 'vaptimes' I call that 'vaptime' 'idltime,' easily confused with
# idldt time, the native IDL structure used to manipulate times.
#
# I'm going to continue this usage here, until I have the desire (not
# likely) to go around changing nomenclature. So, to recap:
#
#  fmt('vaptime' in perl) = yyyymmddThh:mm:ss.ccc
#  fmt('vaptime' in idl) = fmt('idltime' in perl) = yyyy/mm/dd/hh/mm/ss
#
#
# Modification Log:
#
# $Log$
# Revision 1.2  2001/02/14 22:58:36  vapuser
# Fixed small bug in doy2mday_mon
#
# Revision 1.1  2001/02/09 19:07:56  vapuser
# Initial revision
#
#
# 
package VapUtil;

require Exporter;
@ISA = qw(Exporter);
@EXPORT=qw( doy2mday_mon  date2doy vaptime2systime systime2vaptime 
	   systime2idltime idltime2systime leapyear 
	   GetNow DeltaTime ParseVapTime vaptime2idltime
	   vaptime2decyear systime2decyear parts2decyear 
	   fixlonrange prepend_yyyymmdd SysNow makeIDLOplotString
	   makeRandomTag );

use Time::Local;
use Carp;

sub leapyear{

  $year=shift;
  $leap_year = ($year % 100 != 0) && ($year % 4 == 0);
  if ($year % 400 == 0) {
    $leap_year = 1;
  }
  $leap_year;
}

sub doy2mday_mon{
  $doy=shift;
  die "DOY out of range: $doy\n" if $doy > 366;
  $year=shift;
  # cummulative number of days in the year for the end of each month
  @doys = (31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365);
  my $i;
  if (leapyear($year)){
    for ($i=1;$i<=$#doys;$i++){
      $doys[$i]++;
    }
  }
  $mday=$doy;
  for ($i=0;$i<=$#doys;$i++){
    last if $doys[$i] >= $doy;
  }
  $mday = $doy - $doys[$i-1] if $i>0;
  $mon = $i+1;

  ($mday,$mon);
}

sub date2doy{
  $year=shift;
  $month=shift;
  $dom=shift;
  @days_in_month=( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );
  $mon=$month-1;
  $i=0;
  $doy=0;
  while ($i<$mon) {
    $doy += $days_in_month[$i];
    $i++;
  }
  $doy += $dom;
  $leap=leapyear($year);
  if ($month>2) {
    $doy += $leap;
  }
 $doy;
}

sub date_string {
  @months=("Jan","Feb","Mar","Apr","May","Jun","Jul",
	 "Aug","Sep","Oct","Nov","Dec");

  $file=shift;
  ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,
	$size,$atime,$mtime,$ctime,$junk)=stat($file);
  ($sec,$min,$hour,$mday,$mon, $year,$wday,$yday,$isdst) = localtime($ctime); 
  $yday += 1;
  $mon+=1;
  if ($hour < 10) {
    $hour = "0".$hour;
  }
  
  @tmp=split("/",$file);
  $file=$tmp[$#tmp];
  @tmp=split(/\./,$file);
  $basename=shift;
  if ($basename=="") {
    $basename=$tmp[0];
  }
  $more_basename=shift;
  
  $ext=$tmp[1];
  $file = $basename."_".$mon.$mday.$year.$hour.$more_basename;
  if ($ext){
    $file .= ".".$ext;
  }
  $file;
}

sub vaptime2decyear{
  $vaptime=shift || croak "Usage: decimal_year=vaptime2decyear(vaptime)\n";
  ($year,$month,$day,$hour,$min,$sec)=split "/", $vaptime;
  $doy=date2doy($year,$month,$day);
  $decyear=$year + 
      $doy/365. + 
	  $hour/(24*365.) + 
	      $min/(24*60*365.) + 
		  $sec/(24*3600*365.);
      
}

sub systime2decyear{
  local($systime)=shift || 
      croak "Usage: decimal_year=systime2decyear(systime)\n";
  local($sec,$min,$hour,$day,$mon,$year)=gmtime( $systime );
  $year += 1900;
  $mon += 1;
  $doy=date2doy($year,$mon,$day);
  local($decyear)=$year + 
      $doy/365. + 
	  $hour/(24*365.) + 
	      $min/(24*60*365.) + 
		  $sec/(24*3600*365.);
  $decyear;
}

sub parts2decyear{
  ($year, $month, $day, $hour, $min, $sec ) = @_;
  $doy=date2doy($year,$month,$day);  
  local($decyear)=$year + 
      $doy/365. + 
	  $hour/(24*365.) + 
	      $min/(24*60*365.) + 
		  $sec/(24*3600*365.);
  $decyear;
}

sub vaptime2systime{
  # converts a string of the format yyyymmddThhmmss to 
  # gmt seconds since 1-jan-1970 00:00:00
  ($sec,$min,$hour,$mday,$mon,$year)=gmtime($^T);
  $time=shift @_;
  @time_parts=split(/T/,$time);
  if ($#time_parts == 0) {
    @hhmm=split( /:/, $time );
    $time=sprintf( "%02d:%02d", int( $hhmm[0] ), $min ) if ($#hhmm == 0);
    # only the hour:mins segment is there.
    $time=prepend_yyyymmdd( $time );
  }
    # Now we're sure to have something of the form yyyymmddThhmm.
    # So, split it up again.
  @time_parts=split(/T/,$time);
  $year=substr( $time_parts[0], 0, 4 );
  $mon=substr( $time_parts[0], 4,2 );
  $mday=substr( $time_parts[0], 6, 2 );
  ($hour,$min)=split(/:/,$time_parts[1]);
 
  $secs=timegm( 0, $min, $hour, $mday, $mon-1, $year-1900 );
  $secs;
}


sub systime2vaptime{
      # Converts seconds to yyyymmddThhmm
      # 
    ($sec,$min,$hour,$mday,$mon,$year)=gmtime($_[0]);
    $time=sprintf("%04d%02d%02dT%02d:%02d",
		$year+1900,$mon+1,$mday,$hour,$min);
    $time;
}

sub vaptime2idltime{
  @parsed_time=ParseVapTime( $_[0] );
  $idltime=join('/',reverse @parsed_time);
  $idltime
}


sub GetNow {
  @now=systime2vaptime( gmtime(time) );
  @now;
}

sub SysNow {
  $time=timegm(gmtime(time));
  $time;
}

sub ParseVapTime{
  $vaptime=shift || croak "Param #1 <YYYY-MM-DDTHH:MM> is REQUIRED!\n";
  
  @time_parts=split('T',$vaptime);
  $year=substr($time_parts[0],0,4);
  $month=substr($time_parts[0],4,2);
  $day=substr($time_parts[0],6,2);
  @tmp=split(":",$time_parts[1]);
  $hh=$tmp[0];
  $mm=$tmp[1];
  ($mm,$hh,$day,$month,$year);

}

sub idltime2systime{
  $idltime=shift || croak "Param #1 <yyyy/mm/dd/hh/mm[/ss]> is REQUIRED!\n";
  ($year, $month, $day, $hour, $min, $sec) = split "/", $idltime;
  $secs=timegm( $sec, $min, $hour, $day, $month-1, $year-1900 );  
}

sub systime2idltime{
  $secs = shift || croak "Param #1 <unix time> is REQUIRED!\n";
    ($sec,$min,$hour,$mday,$mon,$year)=gmtime($secs);
    $time=sprintf("%04d/%02d/%02d/%02d/%02d/%02d",
		$year+1900,$mon+1,$mday,$hour,$min,$sec);
    $time;
}


sub DeltaTime{

    # take a base time and add a delta to it. 
    # base time (arg 1) is in 'vaptime' format (yyyymmddThhmm)
    # delta time is in hours
    # if you want hours and minutes, make hours a 'float.'

  $basetime = shift @_ || die "Basetime (arg 1) undefined\n";
  $delta = shift @_ || die "delta (arg2) undefined\n";

  $base_time = vaptime2systime($basetime) + $delta*3600.;
  $new_time = systime2vaptime(int($base_time));
  $new_time;

}

sub fixlonrange{
  $minlon=shift @_;
  $maxlon=shift @_;
  while ($minlon>$maxlon) {
    if ($minlon>180.){
      $minlon-= 360.;
    } elsif ($maxlon<0.) {
      $maxlon += 360.;
    }
  } 
  ($minlon, $maxlon);
}


sub prepend_yyyymmdd{
  ($sec,$min,$hour,$mday,$mon,$year)=gmtime($^T);
  $tmp_hhmm=sprintf("%02d:$02d",$hour,$min);
  $hhmm=shift @_ || $tmp_hhmm;
  
  $time=sprintf("%04d%02d%02dT%s",
		$year+1900,$mon+1,$mday,$hhmm);
  $time;
  
}

sub makeIDLOplotString{
  $lon=shift || croak "Param 1 (LON) is REQUIRED\n";
  $lat=shift || croak "Param 1 (LAT) is REQUIRED\n";
  $string="{lon: $lon, lat: $lat}";
#   foreach $k (keys %hash){
#     if (ref($hash{$k}) =~ /ARRAY.*/) {
#       $nextstring = "$k:[".join(",",@{$hash{$k}})."]";
#     } elsif (ref($hash{$k}) =~ /SCALAR.*/) {
#       $nextstring = "$k:$hash{$k}";
#     } else {
#       carp "Hash may only contain SCALARS or ARRAYS! skipping $k\n";
#       next;
#     }
#     $string .= ",$nextstring";

#   }
#   $string.="}";
  $string;
}

sub makeRandomTag{
  $tag=time();
  $tag .= ".$$";
}
1;
