#!/bin/perl -w
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
# Revision 1.3  2001/08/06 18:35:06  vapuser
# Rewrote doy2mday_mon
#
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

use strict;
use Time::Local;
use Carp;

sub leapyear{

  my $year=shift;
  my $leap_year = ($year % 100 != 0) && ($year % 4 == 0);
  if ($year % 400 == 0) {
    $leap_year = 1;
  }
  $leap_year;
}

sub doy2mday_mon{
  my $doy=shift;
  die "DOY out of range: $doy\n" if $doy > 366;
  my $year=shift;
  # cummulative number of days in the year for the end of each month
  my @doys = (31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365);
  my $i;
  if (leapyear($year)){
    for ($i=1;$i<=$#doys;$i++){
      $doys[$i]++;
    }
  }
  my $mday=$doy;
  for ($i=0;$i<=$#doys;$i++){
    last if $doys[$i] >= $doy;
  }
  $mday = $doy - $doys[$i-1] if $i>0;
  my $mon = $i+1;

  ($mday,$mon);
}

sub date2doy{
  my ($year, $month, $dom)= @_;
  my @days_in_month=( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );
  my $mon=$month-1;
  my $i=0;
  my $doy=0;
  while ($i<$mon) {
    $doy += $days_in_month[$i];
    $i++;
  }
  $doy += $dom;
  my $leap=leapyear($year);
  if ($month>2) {
    $doy += $leap;
  }
 $doy;
}

# sub date_string {
#   # obsolete
#   use File::Basename

#   my $file=shift;
#   $basename=shift;
#   $more_basename=shift;

#   my @months=("Jan","Feb","Mar","Apr","May","Jun","Jul",
# 	 "Aug","Sep","Oct","Nov","Dec");


#   my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,
# 	$size,$atime,$mtime,$ctime,$junk)=stat($file);
#   my ($sec,$min,$hour,$mday,$mon, $year,$wday,$yday,$isdst) = localtime($ctime); 
#   my $yday += 1;
#   my $mon+=1;
#   if ($hour < 10) {
#     $hour = "0".$hour;
#   }

#   my ($name,$path,$suffix) = fileparse($file);

#   @tmp=split(/\./,$name);
#   if ($basename=="") {
#     $basename=$tmp[0];
#   }
  
#   $ext=$tmp[1];
#   $file = $basename."_".$mon.$mday.$year.$hour.$more_basename;
#   if ($ext){
#     $file .= ".".$ext;
#   }
#   $file;
# }

sub vaptime2decyear{
  my $vaptime=shift || croak "Usage: decimal_year=vaptime2decyear(vaptime)\n";
  my ($year,$month,$day,$hour,$min,$sec)=split "/", $vaptime;
  my $doy=date2doy($year,$month,$day);
  my $decyear=$year + 
      $doy/365. + 
	  $hour/(24*365.) + 
	      $min/(24*60*365.) + 
		  $sec/(24*3600*365.);
      
}

sub systime2decyear{
  my $systime = shift || 
      croak "Usage: decimal_year=systime2decyear(systime)\n";
  my ($sec,$min,$hour,$day,$mon,$year)=gmtime( $systime );
  $year += 1900;
  $mon += 1;
  my $doy=date2doy($year,$mon,$day);
  my $decyear=
      $year + 
      $doy/365. + 
      $hour/(24*365.) + 
      $min/(24*60*365.) + 
      $sec/(24*3600*365.);
  $decyear;
}

sub parts2decyear{
  my ($year, $month, $day, $hour, $min, $sec ) = @_;
  my $doy=date2doy($year,$month,$day);  
  my $decyear=
      $year + 
      $doy/365. + 
      $hour/(24*365.) + 
      $min/(24*60*365.) + 
      $sec/(24*3600*365.);
  $decyear;
}

sub vaptime2systime{
  # converts a string of the format yyyymmddThhmmss to 
  # gmt seconds since 1-jan-1970 00:00:00
  my $time=shift @_;
  my ($sec,$min,$hour,$mday,$mon,$year)=gmtime($^T);
  my @time_parts=split(/T/,$time);
  if ($#time_parts == 0) {
    my @hhmm=split( /:/, $time );
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
 
  my $secs=timegm( 0, $min, $hour, $mday, $mon-1, $year-1900 );
  $secs;
}


sub systime2vaptime{
      # Converts seconds to yyyymmddThhmm
      # 
    my ($sec,$min,$hour,$mday,$mon,$year)=gmtime($_[0]);
    my $time=sprintf("%04d%02d%02dT%02d:%02d",
		$year+1900,$mon+1,$mday,$hour,$min);
}

sub vaptime2idltime{
  my @parsed_time=ParseVapTime( $_[0] );
  my $idltime=join('/',reverse @parsed_time);
}


sub GetNow {
  my @now=systime2vaptime( gmtime(time) );
}

sub SysNow {
  my $time=timegm(gmtime(time));
}

sub ParseVapTime{
  my (@time_parts, $year, $month, $day, @tmp, $hh, $mm);

  my $vaptime=shift || croak "Param #1 <YYYYMMDDTHH:MM> is REQUIRED!\n";

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
  my $idltime=shift || croak "Param #1 <yyyy/mm/dd/hh/mm[/ss]> is REQUIRED!\n";
  my ($year, $month, $day, $hour, $min, $sec) = split "/", $idltime;
  my $secs=timegm( $sec, $min, $hour, $day, $month-1, $year-1900 );  
}

sub systime2idltime{
  my $secs = shift || croak "Param #1 <unix time> is REQUIRED!\n";
  my ($sec,$min,$hour,$mday,$mon,$year)=gmtime($secs);
  my $time=sprintf("%04d/%02d/%02d/%02d/%02d/%02d",
		$year+1900,$mon+1,$mday,$hour,$min,$sec);
  $time;
}


sub DeltaTime{

    # take a base time and add a delta to it. 
    # base time (arg 1) is in 'vaptime' format (yyyymmddThhmm)
    # delta time is in hours
    # if you want hours and minutes, make hours a 'float.'

  my $basetime = shift @_ || die "Basetime (arg 1) undefined\n";
  my $delta = shift @_ || die "delta (arg2) undefined\n";

  my $base_time = vaptime2systime($basetime) + $delta*3600.;
  my $new_time = systime2vaptime(int($base_time));

}

sub fixlonrange{
  my ($minlon, $maxlon) = @_;
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
  my ($sec,$min,$hour,$mday,$mon,$year)=gmtime($^T);
  my $tmp_hhmm=sprintf("%02d:$02d",$hour,$min);
  my $hhmm=shift @_ || $tmp_hhmm;
  
  my $time=sprintf("%04d%02d%02dT%s",
		$year+1900,$mon+1,$mday,$hhmm);
}

sub makeIDLOplotString{
  my $lon=shift || croak "Param 1 (LON) is REQUIRED\n";
  my $lat=shift || croak "Param 1 (LAT) is REQUIRED\n";
  my $string="{lon: $lon, lat: $lat}";
}

sub makeRandomTag{
  my $tag=time(). ".$$";
}
1;
