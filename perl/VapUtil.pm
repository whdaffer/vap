#
#
# $Id$
#
# The replacement of vap_perl.pm
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
# Revision 1.8  2002/08/08 23:28:05  vapdev
# General cleanup, work on BEGIN{} block, work on interface issues.
#
# Revision 1.7  2002/08/07 23:57:17  vapdev
# move 'use strict' to top and fixed resulting compilation errors
#
# Revision 1.6  2002/07/03 22:36:54  vapdev
# Continuing upgrade (-w/use strict) work
#
# Revision 1.5  2002/07/01 20:24:20  vapdev
# VapUtil: Moved stuff from vap_perl/VapWWW to VapUtil
# ts_overlay: check $gms4mindiff for nullness
#
# Revision 1.4  2002/05/07 20:40:36  vapdev
# Set -w and `use strict' and then fixing bugs. Start trying to standardize
# the methods used.
#
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
use strict;

use vars qw/@ISA @EXPORT_OK $VAP_LIBRARY $ARCHIVETOP $GRIDDINGTOP
	    $WINDS_DIR $IDLEXE $auto_movie_defs_file
	    $overlay_defs_file $vap_defs_file $vap_is_batch
	    %satnum2areanum %areanum2satnum %satloc2satnum
	    %sensornum2name %sensornum2dir %sensorname2num
	    %vap_defs/;

use subs qw/&doy2mday_mon  &date2doy &vaptime2systime &systime2vaptime 
	   &systime2idltime &idltime2systime &leapyear 
	   &GetNow &DeltaTime &ParseVapTime &vaptime2idltime
	   &vaptime2decyear &systime2decyear &parts2decyear 
	   &fixlonrange &prepend_yyyymmdd &SysNow &makeIDLOplotString
	   &makeRandomTag/;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK=qw( &doy2mday_mon &date2doy &vaptime2systime &systime2vaptime 
	       &systime2idltime &idltime2systime &leapyear &auto_movie_defs
	       &GetNow &DeltaTime &ParseVapTime &vaptime2idltime
	       &vaptime2decyear &systime2decyear &parts2decyear 
	       &fixlonrange &prepend_yyyymmdd &SysNow &makeIDLOplotString
	       &makeRandomTag $VAP_LIBRARY $ARCHIVETOP $GRIDDINGTOP
	       $WINDS_DIR $IDLEXE $auto_movie_defs_file
	       $overlay_defs_file $vap_defs_file $vap_is_batch 
	       %satnum2areanum   %areanum2satnum %satloc2satnum 
	       %sensornum2name %sensorname2dir);

use Time::Local;
use Carp;


BEGIN {
    # Get ENV variables

  $VAP_LIBRARY=$ENV{'VAP_LIBRARY'} || 
    croak "ENV VAP_LIBRARY is not defined!\n";

   # Get ENV variables
   $ARCHIVETOP=$ENV{'VAP_GOES_TOP'}  || 
     croak "ENV var VAP_GOES_TOP undefined!\n";
   $GRIDDINGTOP=$ENV{'VAP_GOES_GRIDDED_TOP'} || 
     croak "ENV var VAP_GOES_GRIDDED_TOP undefined\n";
   $WINDS_DIR = $ENV{'VAP_DATA_TOP'} || 
        croak "ENV var  VAP_DATA_TOP undefined\n";

  $IDLEXE = $ENV{IDLEXE} || croak "ENV var IDLEXE is undefined!\n";

    # MOVIE DEFS
  $auto_movie_defs_file=$VAP_LIBRARY."/auto_movie_defs.dat";

    # Get the Overlay Defaults
  $overlay_defs_file=$VAP_LIBRARY."/overlay_defs";
  require $overlay_defs_file;

    # Get generic VAP processing Defaults
  $vap_defs_file=$VAP_LIBRARY."/vap_defs";
  require $vap_defs_file;

    # Check for interactivity.
  $vap_is_batch = !defined($ENV{'TERM'});

  %satnum2areanum = (10 => 8,
		     8 => 9);

  %areanum2satnum = (8 => 10, 
		     9 => 8 );

  %satloc2satnum = (WEST=>10,
		    EAST=>8);

  %sensornum2name = ('1' => 'vis',
		     '2' => 'ir2',
		     '3' => 'ir3',
		     '4' => 'ir4',
		     '5' => 'ir5'
		    );
  %sensorname2num = reverse %sensornum2dir;

}

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
  croak "DOY out of range: $doy\n" if $doy > 366;
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
  my ($year, $month, $mday, $hour, $min, $sec)= split "/", $idltime;
  my $secs=timegm( $sec || 0, $min, $hour, $mday, $month-1, $year-1900 );  
}

sub systime2idltime{
  my $secs = shift || croak "Param #1 <unix time> is REQUIRED!\n";
  my ($sec,$min, $hour,$mday,$month,$year)=gmtime($secs);
  my $time=sprintf("%04d/%02d/%02d/%02d/%02d/%02d",
		$year+1900,$month+1,$mday,$hour,$min,$sec || 0);
  $time;
}


sub DeltaTime{

    # take a base time and add a delta to it. 
    # base time (arg 1) is in 'vaptime' format (yyyymmddThhmm)
    # delta time is in hours
    # if you want hours and minutes, make hours a 'float.'

  my $basetime = shift @_ || croak "Basetime (arg 1) undefined\n";
  my $delta = shift @_ || croak "delta (arg2) undefined\n";

  my $base_time = vaptime2systime($basetime) + $delta*3600.;
  my $new_time = systime2vaptime(int($base_time));

}

sub fixlonrange{
  croak "Usage fixed_array=fixlonrange(minlon,maxlon)\n"
    unless @_ == 2;
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
  my $tmp_hhmm=sprintf("%02d:%02d",$hour,$min);
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

sub auto_movie_defs {
  # parses the file $VAP_ROOT/auto_movie_defs.dat returns array
  # containing the ROI designations
  # (i.e. 'nepac','nwpac','npac','nwatl' and whatever other regions of
  # interest we may decide to do.
  

  # open the file 
  my $file=shift || $auto_movie_defs_file;
  open (DEFS,"<$file") || 
    croak "Can't open $file\n";
  my @defs =<DEFS>;
  close DEFS;

  # loop over records in the file taking the 'value' of the 'desig'
  # field in each record. Each record is a string suitable for
  # defining an IDL structure in an IDL 'execute' call, i.e. it looks
  # like { F1:X1, F2:X2, F3:X3 ... }.  So we split on the ',' then
  # find 'desig' and split on the ":"

  my @desigs = ();
  foreach my $r ( @defs) {
    next if $r =~ /^;.*$/; # next if IDL comment line
    next if $r =~ /^\s*$/; # next if empty line
    my @tmp = split(/,/, $r);
    my $tmp = $tmp[0];
    @tmp = split(/:/, $tmp);
    push @desigs, $tmp[1];
  }
  @desigs;
}


# sub date_string {
#   my @months=("Jan","Feb","Mar","Apr","May","Jun","Jul",
# 	 "Aug","Sep","Oct","Nov","Dec");

#   my $file=shift;
#   my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,
# 	$size,$atime,$mtime,$ctime,$junk)=stat($file);
#   my ($sec,$min,$hour,$mday,$mon, $year,$wday,$yday,$isdst) = localtime($ctime); 
#   $yday += 1;
#   $mon+=1;
#   if ($hour < 10) {
#     $hour = "0".$hour;
#   }
#   my @tmp=split("/",$file);
#   $file=$tmp[$#tmp];
#   @tmp=split(/\./,$file);
#   $basename=shift;
#   if ($basename=="") {
#     $basename=$tmp[0];
#   }
#   $more_basename=shift;
#   $ext=$tmp[1];
#   $file = $basename."_".$mon.$mday.$year.$hour.$more_basename;
#   if ($ext){
#     $file .= ".".$ext;
#   }
#   $file;
# }


sub date_index{ 
  # Looks for strings demarcating the 'created at' time
  # for the four files listed below in the file index.html (in the
  # current directory) and and puts in the ctime of each of the four
  # files listed below and the current time in the place of 'Last
  # modified:'.  time. It does this for the files daily_pac.mov,
  # pac_daily_a.gif, pac_daily_nn_a.gif, pac_daily_d.gif and
  # pac_daily_nn_d.gif. Also looks for the string "Last modified: "
  # When it finds this string it chops the last part off the string
  # and appends the current date/time.  
  # 
  # It also looks for strings of the form 'xxx_size' where xxx =
  # anim,pgoa or pgod and substitutes the file size (in Kbs) into the
  # file.
  # 
  # No arguments are required. It know's what it needs to do.

  my $start_dir=Cwd::cwd();
  my %file_size  = ();
  my %file_date  = ();

  my $umask = umask();
  print "Current umask = $umask\n";
  print "Setting umask to 023 \n";

  umask( 023 ) || croak "Couldn't reset umask to 023\n";
  $umask = umask();
  print "New umask = $umask\n";
  my $tmp = oct("023");
  print "Should equal $tmp\n";

  my @months=("Jan","Feb","Mar","Apr","May","Jun","Jul",
	   "Aug","Sep","Oct","Nov","Dec");
  my @days=("Sun","Mon","Tue","Wed","Thu","Fri","Sat");

  my $DOCROOT=$ENV{VAP_WWW_TOP};
  my $VAPIM = "$DOCROOT/images";

  
  my @wwwfiles = ('daily_nepac.mov',
	       'daily_nwpac.mov',
	       'daily_npac.mov',
	       'daily_nwatl.mov',
	       'daily_indian.mov',
	       'daily_atlhurr.mov',
	       'daily_pachurr.mov',
	       'GOES104NEPAC1.jpeg',
	       'GOES104PACHURR1.jpeg',
	       'GOES84NWATL1.jpeg',
	       'GOES84ATLHURR1.jpeg',
	       'GMS51WPAC1.jpeg',
	       'GMS51JAPAN1.jpeg',
	       'GMS51FAREAST1.jpeg'
	       );

  my @wwwnames = ('nepac_anim',
	       'nwpac_anim',
	       'npac_anim',
	       'nwatl_anim',
	       'indian_anim',
	       'atlhurr_anim',
	       'pachurr_anim',
#	       'npac_evol_anim',
	       'goes104nepac1',
	       'goes104pachurr1',
	       'goes84nwatl1',
	       'goes84atlhurr1',
	       'gms51wpac1',
	       'gms51japan1',
	       'gms51fareast1');

  my %wwwhash=('nepac_anim'     => 'daily_nepac.mov',
	    'nwpac_anim'     => 'daily_nwpac.mov',
	    'npac_anim'      => 'daily_npac.mov', 
	    'nwatl_anim'     => 'daily_nwatl.mov',
	    'indian_anim'    => 'daily_indian.mov',
	    'atlhurr_anim'   => 'daily_atlhurr.mov',
	    'pachurr_anim'   => 'daily_pachurr.mov',
#	    'npac_evol_anim' => 'daily_npac_evol.mov',
	    'goes104nepac1'  => 'GOES104NEPAC1.jpeg',
	    'goes104pachurr1'  => 'GOES104PACHURR1.jpeg',
	    'goes84nwatl1'   => 'GOES84NWATL1.jpeg',
	    'goes84atlhurr1'   => 'GOES84ATLHURR1.jpeg',
	    'gms51wpac1'     => 'GMS51WPAC1.jpeg',
	    'gms51japan1'    => 'GMS51JAPAN1.jpeg', 
	    'gms51fareast1'  => 'GMS51FAREAST1.jpeg');

  do {
    print "Couldn't chdir to $DOCROOT";
    return undef;
  } unless chdir $DOCROOT;
  
  
  my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
      $atime,$mtime,$ctime,$junk, $day, $month);

  while( my ($key,$value) = each %wwwhash ) {

    my $file = "$VAPIM/$value";

    if (-e $file) { 

     if (! ( ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
       $atime,$mtime,$ctime,$junk)=stat($file)))
     {
       print "Couldn't stat $file\n";
       return undef;
     }

      ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
       $atime,$mtime,$ctime,$junk)=stat( _ ) if !$nlink;
      
      $file_size{$key}= $tmp = sprintf( "%d Kb", int( $size*1.e-3+0.5) );
      # convert  mod time
      my ($sec,$min,$hour,$mday,$mon, $year,$wday,$yday,$isdst) = localtime($mtime); 
      $day=$days[$wday];
      $month=$months[$mon];
      $year += 1900;

      # construct the file date 
      $file_date{$key} = sprintf( "%3s %3s %02d %02d:%02d:%02d %04d PST",
				 $day, $month, $mday, $hour,$min,$sec, $year);

    } else {
      $file_size{$key} = "???Kb";
      $file_date{$key} = "???";
    }
  }

  #
  # Get the current time for the 'Last modified' string
  #  
  my ($sec,$min,$hour,$mday,$mon,
   $year,$wday,$yday,$isdst) = localtime(time);
  
  $day=$days[$wday];
  $month=$months[$mon];
  my $mod_date_str = sprintf( "%03s %3s %02d %02d:%02d:%02d %04d PST",
			  $day, $month, $mday, $hour,$min,$sec, $year);

  open(IN ,"index.html.blank");
  my @in=<IN>;
  close(IN);
  do { print "Couldn't rename index.html\n";
       undef;} unless rename ("index.html", "index.html.old");
  do {
    print "Can't open index.html\n";
    undef; } unless open (OUT, ">index.html");
  my $search_string1 = "(hhmts|";
  $search_string1 .= join("|",@wwwnames);
  $search_string1 .= ")";

  for (my $i=0;$i<=$#in;$i++) {
    my $rec=$in[$i];
    if ($rec !~ /^\s*<!--\s*<!--/) {
#      if ($rec=~/(anim|size|goes104|goes84|gms51|hhmts){       
#      if ($rec =~ /(anim|size|goes104nepac1|goes84nwatl1|hhmts)/) {
      if ($rec =~ /$search_string1/) {
#       print "Found something, rec = $rec\n";
	if ($rec =~ /hhmts start/) {
	    # Handle the 'Last Modified' at the end of the page 
	  print OUT $rec;
	  print OUT "Last Modified: $mod_date_str\n";
	  $i += 2;
	} else {
	  foreach my $k (@wwwnames) {
	    if ( $rec =~ /$k/ ) {
#	    print "key = $k\n";
	      my $search_string2=$k."_size";
	      if ($rec =~ /.*$search_string2.*/ ) {
		$rec =~ s/$search_string2/$file_size{$k}/;
		$in[$i] = $rec;
		last ;
#	      print "After sub, rec = \n";
	      } elsif ($rec =~ /.*$k start/ ) {
#	      print "file mod time \n";
		print OUT $rec;
		print OUT "Created: $file_date{$k} </br></br>\n";
		$i += 2;
		last;
	      } 
	    }
	  }
	}
      }
    }
    print OUT $in[$i];
  }
  do {print "Couldn't go back to $start_dir\n";
	  undef;} unless chdir $start_dir;
  close OUT;
  1;
}


# sub fix_time  {
#   $_[0]_ = sprintf("%02d",$_[0]);
#   $_[1]_ = sprintf("%02d",$_[1]);
#   $_[2]_ = sprintf("%02d",$_[2]);
#   $_[3]_ = sprintf("%04d",$_[3]+1900);
#   @_;
# }

sub make_yyyymmdd {
  my ($year,$month,$mday) = @_;
  $month = sprintf("%02d",$month);
  $mday = sprintf("%02d",$mday);
  my $yyyymmdd=$year.$month.$mday;
}
sub make_hhmm{
  my $hh=shift;
  my $mm = shift|| 0;
  my $hhmm=sprintf("%02d:%02d",$hh,$mm);
}

#sub byctime {
#  $ctime{$a} <=> $ctime{$b};
#}
#
#
# sub pgl { 
#   #
#   # pgl : Parse goes log
#   #
#   #
#   my $logfile=shift(@_);;
#   open( LOGFILE, "<$logfile");
#   while (<LOGFILE>) {

#     if ( /^Sorry, cannot find it.\s$/) {
#       my $newfile = "NO_SUCH_FILE";
#       my $skip_rename=1;
#     } else {
#       if (/^::[\s]+output to /){
# 	my @tmp=split(" ");
# 	my $oldfile=$tmp[$#tmp];
# 	@tmp = split( /\./, $oldfile );
# 	my $file = $tmp[0];
#       }

#       if (/^[\s]+minlat minlon[\s]+/){
# 	@tmp=split(" ");
# 	$minlat = $tmp[3];
# 	$minlon = $tmp[4];
# 	if ($minlon < 0) {
# 	  $minlon += 360 ;
# 	}
#       }

#       if (/^[\s]+maxlat maxlon[\s]+/){
# 	@tmp=split(" ");
# 	$maxlat = $tmp[3];
# 	$maxlon = $tmp[4];
# 	if ($maxlon < 0) {
# 	  $maxlon += 360 ;
# 	}
#       }


#       if (/precision (deg)/){
# 	@tmp=split(" ");
# 	$plat = $tmp[$3];
# 	$plon = $tmp[$4];
#       }

#       if (/^:: Now read in/) {
# 	@tmp = split( " " );
# 	$areafile = $tmp[$#tmp-1];
# 	@tmp = split( "/", $areafile );
# 	$areafile = $tmp[$#tmp];
# 	$last4 = substr( $areafile, 4, 4 );
#       }
#     }
#   } # end while
#   if (!$skip_rename) {
#     $limstr = "%".$minlon.",".$minlat.",".$maxlon.",".$maxlat."%";
#     #$time=substring( $time, 0, 4 );
#     $newfile=$file."-".$last4."-".$limstr.".dat";
#     rename( $oldfile, $newfile) || croak " couldn't rename $oldfile to $newfile \n";
#     chmod 0755, $newfile || print "Couldn't chmod 0755 on $newfile \n";

#   }
#   $newfile;

# }

sub subtract_one_day{
    


    # input the month as the real (1 indexed) month, not the 0 indexed
    # monster from the tm struct!

  my ($year, $mon, $mday) = @_;
  my $leap_year = ($year % 100 != 0) && ($year % 4 == 0);
  if ($year % 400 == 0) {
    $leap_year = 1;
  }
  my @days_in_month=(31,28,31,30,31,30,31,31,30,31,30,31);
  
  $mday -=1;
  if ($mday <= 0) {
    if ($mon == 1) {
      $mon=12;
      $mday=31;
      $year -= 1;
    } else {
      $mday = $days_in_month[ $mon-1 ];
      $mon -= 1;
      if ($leap_year && $mon == 2) {
	$mday += 1;
      }
    }
  }
  $mday = sprintf("%02d",$mday);
  $mon = sprintf("%02d",$mon);

   # return modified date
  ($year,$mon,$mday);
}
sub add_one_day{
    


    # input the month as the real (1 indexed) month, not the 0 indexed
    # monster from the tm struct!

  my ($year, $mon, $mday) = @_;
  my $leap_year = ($year % 100 != 0) && ($year % 4 == 0);
  if ($year % 400 == 0) {
    $leap_year = 1;
  }
  my @days_in_month=(31,28,31,30,31,30,31,31,30,31,30,31);
  
  $mday +=1;
  if ($mday > $days_in_month[$mon-1]) {
    if ($mon == 12) {
      $mon=1;
      $mday=1;
      $year += 1;
    } else {
      $mday = 1;
      $mon += 1;
    }
  }
  $mday = sprintf("%02d",$mday);
  $mon = sprintf("%02d",$mon);
   # return modified date
  ($year,$mon,$mday);
}


#=========================================
# GAG:
# 
# GetAndGrid - get and grid a goes AREA file, depending on the input
# paraemters
# 
#=========================================
sub gag {
  # Usage: gridded_file=gag(goesnum, sensornum, time, minlon, minlat, maxlon, maxlat);
  #

  # gag=GetAndGrid : gets the Goes file for the indicated goes
  # satellite ( '9' or '10') and indicated sensor number (1=vis,
  # 2=ir2, 3=ir3, 4=ir4) which is closest to time indicated by 'time'.
  # It Grids the file and places the output in the corresponding
  # subdirectory of $VAP_GOES_GRIDDED_TOPDIR (defaulting to
  # /disk2/vap/goes/gridded_files/ if that environmental variable is
  # undefined) according to the requested file, e.g. gag 10 1 xxx will
  # put the output in $VAP_GOES_GRIDDED_TOPDIR/goes10/vis.  

  # Time is specified as yyyymmddThh:mm with each unspecified
  # quantity defaulting to the current GMT time.
  # e.g. 0404T03 is 4 of April 1998 at 3 oclock (GMT).
  # 
  #

  my ($sec,$min,$hour,$mday,$mon,$year,$junk)=gmtime(time);
  my $yyyymmdd = make_yyyymmdd($year+1900,$mon+1,$mday);
  my $hhmm = make_hhmm($hour,$min);
  my $now = $yyyymmdd."T".$hhmm;

  my $satnum    = shift || "10";
  my $sensornum = shift || "4";
  my $time      = shift || $now;

  my ($minlon, $minlat, $maxlon, $maxlat);

  $minlon    = shift || 0;
  $minlat    = shift || 0;
  $maxlon    = shift || 0;
  $maxlat    = shift || 0;

  my $start_dir = Cwd::cwd();

  my ($host,$user,$pw) = getgoesarchive();
  do { print "Can't get goes Archive info\n";
       undef; } unless defined $pw;

  # given the satellite, returns x of AREAx
  # Goes 10 files have the name goes8, goes 8 have names AREA9
  # $sat_num{10} = 8;

  my $goes_type="goes".$satnum." ".$sensornum2name{$sensornum};



  # Get Current Greenwich Mean Time
  my ($gsec,$gmin,$ghour,$gmday,$gmon, $gyear,$gwday,$gyday,$gisdst) = gmtime($^T); 
    # Construct the input time.
  my @time_parts=split "T", $time;
  my $yearmonthday;
  my @hhmm;
  if ($#time_parts == 0) {
    $yearmonthday = make_yyyymmdd( $gyear+1900, $gmon+1, $gmday );
    @hhmm= split /:/, $time_parts[0];
  } else {
    $yearmonthday=$time_parts[0];
    @hhmm = split /:/, $time_parts[1];
  }

    # Construct the 'test_time', i.e. the time against which the area file's time 
    # will be compared. 

  my ($tyear, $tmon, $tmday, $tyday, $thour, $tmin);

  $tyear=substr($yearmonthday,0,4)-1900;
  $tmon=substr($yearmonthday,4,2)-1;
  $tmday=substr($yearmonthday,6,2);

  $tyday=date2doy( $tyear, $tmon, $tmday );
  $thour=$hhmm[0] || $ghour;
  $tmin =$hhmm[1] || $gmin;
 
  $junk=sprintf("%04d%02d%02dT%02d:%02d",
             $tyear+1900,$tmon+1,$tmday,$thour,$tmin);
  print "Looking for a $goes_type file from around GM time $junk\n";

    # Convert the test time to GMT
  my $test_time = timegm( 0, $tmin, $thour, $tmday, $tmon, $tyear );
  if ($test_time == -1)
  {
    print "Can't convert test time $tyear/$tmon/$tmday $thour\n";
    return undef; 
  } 

    # Construct file test times. The times in the 
    # area info file are already in GMT.);

    # NB, this loop is reading the 'noaa_area_info' file.
    # Construct the filename for the noaa_area_info file.
  my $noaa_area_info_filename=$ARCHIVETOP."/goes".$satnum."/";
  $noaa_area_info_filename.=$sensornum2name{$sensornum}."/noaa_area_info";
    # Open 'info' file
  if (!open(NOAA_AREA_INFO, "<$noaa_area_info_filename") )
  {
    print "Couldn't open noaa_area_info $noaa_area_info_filename file \n";
    return undef;
  }
  my @noaa_area_info = <NOAA_AREA_INFO>;
  close(NOAA_AREA_INFO);

  my (@tmp, @tmp2);
  my ($area_file, $area_file_time, $area_file_time_string);
  my $min_diff=1e20;
  foreach my $rec (@noaa_area_info) {
    next if $rec =~ /^#/;

    @tmp=split(" ", $rec);
    my $filename=substr( $tmp[0], 0, 8 );
    my $hour = substr( $tmp[6], 0, 2 ); 
    my $min = substr( $tmp[6], 2,2 );
    @tmp2=split('/',$tmp[4] );
    if ($#tmp2 < 2) {
      print "  Corrupted data/time for $filename\n";
        # Somehow, this field (mm/dd/yyyy) in the area_info file has
        # been corrupted, so we're going to read the file itself.
      my $local_path=$ARCHIVETOP."/goes".$satnum."/".$sensornum2name{$sensornum}."/";
      my $ttmp=$local_path.$filename;
      if (open( AREAFILE, "<$ttmp" )){
	my $hdr;
	read AREAFILE, $hdr, 64*4;
	close AREAFILE;
	my @hdr=unpack "L4", $hdr;
	my $year=int ($hdr[3]/1000);
	my $doy=int ($hdr[3]-$year*1000);
	($mday, $mon) = doy2mday_mon($doy, $year);
	$hour=int ($hdr[4]/100);
        $min=int ($hdr[4]-$hour*100);
      } else {
	print "  and it isn't here! Skipping this file\n";
	print "   (NB, May be the file we want, but can't tell)\n";
      }
    } else {
      $mon=$tmp2[0]-1;
      $mday=$tmp2[1];
      $year=substr($tmp2[2],0,4)
    }
    my $noaa_file_time=timegm(0,$min,$hour,$mday,$mon,$year-1900);
    my $diff=abs( $noaa_file_time-$test_time);
    if ($diff < $min_diff) {
	$area_file=$filename;
	$area_file_time=$noaa_file_time; 
        my ($sec1,$min1,$hour1,$mday1,$mon1,$year1)=gmtime( $area_file_time );
        $area_file_time_string =
           sprintf("%04d%02d%02dT%02d:%02d:%02d",
		   $year1+1900,$mon1+1,$mday1,$hour1,$min1,$sec1);
	$min_diff=$diff;
    }

  }
  my $diffhrs=$min_diff/3600.;
  print "min_diff=$min_diff, diffhrs=$diffhrs\n";
  if ($diffhrs>2.)
  {
    print "Closest File ($area_file) is over $diffhrs hours distant!\n";
    return undef;
  }

  print "  AREA file for this run = $area_file\n";

  print "    Whose Time is $area_file_time_string\n";
  print "    Checking to see whether it's already here\n";

  my $off=4;

  my $tmp=substr( $area_file,$off,2 );
  my $sat=substr( $area_file, $off, 2 );
  my $sen=substr( $area_file, $off+2, 1);
  my $filenum = substr( $area_file, $off+3, 2);

  # Construct path and file name on remote server.
  my $remote_path = "goes".$satnum."/".$sensornum2name{$sensornum}."/";

  if ( $remote_path =~ m#^g# ) {
    $tmp = "/".$remote_path ;
  } else {
    $tmp = $remote_path;
  }

    # Construct the path and filename for the AREA file 
    # on this machine.
    # While we're at it, might as well produce the path 
    # to the gridding files.
  my $local_path=$ARCHIVETOP.$tmp;
  my $gridding_path=$GRIDDINGTOP.$tmp;
  my $local_area_file=$local_path.$area_file;
  my $test2 = 1.e20;
  my $local_file;
  if (-e $local_area_file) {
 
        # the file exists on this machine. Make sure the local area_info
        # file is up to date by running mkai in the target
        # directory. See if the area file there is the same one.

    my $cwd=Cwd::getcwd();
    do { print "Couldn't cd to $local_path to run mkai\n";
	 undef;} unless  chdir $local_path; 
    my $t=Cwd::getcwd();
    print "Now in $t\n";
    my $exe_str = "/usr/people/vapuser/bin/mkai ";
    if ($user =~ /root/){
      my ($name,$passwd,$uid,$gid,$quota,$comment,$gcos,$dir,$shell) = 
	  getpwnam( "vapuser") ;
      if ($uid && $gid ) {
	chown $uid,$gid, 'area_info' || 
	    print "  Couldn't change owner,group of area_info\n";
      } else {
	print "    Failure in getpwnam, Changing permissions to rwxrwxrwx, root owned\n";
	chmod 0777, $local_file;
      }
    }

    print "Running mkai with exe string $exe_str\n";
    my $r=system( $exe_str )/256;
    if ($r != 0) {
      print "   Some kind of error in mkai\n";
    }
    do {print "Couldn't cd back to $cwd\n";
	undef;} unless chdir $cwd;
    print "Done running mkai\n";

        # read the local area_info file, see if the area file we have
        # is from the same time.

    my $local_ainf = $local_path."/area_info";
    do {print "Couldn't open $local_ainf\n";
	undef;} unless open ( INF, "<$local_ainf") ;
    my @ainf=<INF>;
    close (INF);

    foreach my $r (@ainf) {
      next if $r =~ /^##/;
      @tmp=split(" ", $r);
      next if substr($tmp[0],0,8) !~ /$area_file/;

      @tmp=split(" ", $r);

      $hour = substr( $tmp[6], 0, 2 ); 

      @tmp2=split('/',$tmp[4] );

      $mon=$tmp2[0]-1;
      $mday=$tmp2[1];
      $year=substr($tmp2[2],0,4);
      my $local_area_file_time=timegm(0,0,$hour,$mday,$mon,$year-1900);      
      $test2 = abs($local_area_file_time - $area_file_time);
      last;
    }
  }

    # test2 is in seconds
  if ( $test2 >= 3600 ) { # more than 1 hour difference.

      # either we don't have this area file, or it isn't the right one.
      # go to the archive are retrieve it.

    print "   Not here! Retrieving $area_file from the NOAA archive \n";
    my $file=getgoesfile( $area_file);

    if (!$file){
      print "Error retrieving $area_file\n";
      return undef;
    }
	  
      
        # If we're running this as root, change owner/group and
        # permissions

    my $user=$ENV{'USER'};
    if ($user =~ /root/){
      my ($name,$passwd,$uid,$gid,$quota,$comment,$gcos,$dir,$shell) = 
	  getpwnam( "vapuser") ;
        # Change uid/gid to vapuser
      if ($uid && $gid ) {
	chown $uid,$gid, $local_file || 
	    print "  Couldn't change owner,group of $file\n";
      } else {
	print "  Failure in getpwnam, Changing permissions to rwxr-xr-x, root owned\n";
	chmod 0755, $local_file;
      }
    }

  } else {
      
      print "  $area_file already exists in our archive\n";
      print "  Checking to see if a grid file exists also\n";
      my ($iminlon, $iminlat, $imaxlon, $imaxlat);

      $iminlon=int $minlon;
      $iminlat=int $minlat;
      $imaxlon=int $maxlon;
      $imaxlat=int $maxlat;
      if ($iminlon==0 && $iminlat==0 && $imaxlon==0 && $imaxlat==0) {
	  if ($satnum == 10) {
	      my $iminlon=160;
	      my $iminlax=0;
	      my $imaxlon=-70;
	      my $imaxlat=70;
	  } elsif ($satnum == 8) {
	      # Don't know what then limits are on Goes 8 files.
	  } else {
	      #Job security
	  }
      }
      my $goes_type=$satnum.$sensornum;
      my $tyear=$year;
      my $tmonth=$mon+1;
      my $grid_file_name=sprintf( 
	    "GOES%03d-%04d%02d%02d%02d-%%%04d,%03d,%04d,%03d%%.dat", 
			      $goes_type, $tyear, $tmonth, $mday, $hour, 
			      $iminlon, $iminlat, $imaxlon, $imaxlat );
      
      
      my $local_gridded_file=$gridding_path.$grid_file_name;


      # construct the name of the gridded file.
      print "   Testing for grid file matching glob\n   $local_gridded_file\n";
      if (-e $local_gridded_file) {
	  print "  grid_file ($local_gridded_file) already exists, I'll return this file instead\n";
	  # Not he prettiest way to code this, I'll think of something later.
	  return $local_gridded_file;
      } else {
	  print "  Not there, have to grid it ourselves\n";
	  
      }
  }


    #-------------------------------------------------------
    #
    # CD to the directory where we put the gridded files.
    # Then run the gridding program.
    #
    #-------------------------------------------------------
  my $local_gridded_file=grid_goes( $gridding_path, $area_file, 
				$minlon, $minlat, $maxlon, $maxlat );
  print "local gridded_file = $local_gridded_file\n";
  if (!$local_gridded_file) 
  { 
    print "Error Gridding area file $area_file\n" ;
    return undef;
  } 
  $local_gridded_file;

} # End GAG

#===================================================
# getgoesarchive
#
# 
#===================================================
sub getgoesarchive{ 

  my $start_dir = Cwd::cwd();
  my $goes_info_file=shift or croak "Need name of goes_archive file!\n";
  open(ARCHIVE_INFO, "<$goes_info_file") || croak "Can't open $goes_info_file file\n";
  my $host = <ARCHIVE_INFO>; chomp $host;
  my $user = <ARCHIVE_INFO>; chomp $user;
  my $pw   = <ARCHIVE_INFO>; chomp $pw;
  {HOST=>$host, USER=>$user, PW=>$pw};
}


#===================================================
# getgoesarchive
#
#
#===================================================

sub getgoesfile{

  use Net::FTP;
  my $start_dir = Cwd::cwd(); 
  my ($host,$user,$pw) = getgoesarchive();
  croak "Can't get goes Archive info\n" unless defined $pw;
  
  my $area_file=shift;
  $area_file="AREA".$area_file unless $area_file =~ /^A/;
  my $off=4;
  my $tmp=substr( $area_file, $off,2 );
  my $sat=substr( $area_file, $off, 1 );
  my $sen=substr( $area_file, $off+1, 1);
  my $filenum = substr( $area_file, $off+2, 2);

  my $remote_path = "goes".$satnum2areanum{$sat}."/".$sensornum2name{$sen}."/";
  my $remote_file = $remote_path.$area_file;
  
  print "remote_path = $remote_path\n";
  print "remote_file = $remote_file\n";
  my $local_path = shift;
  if (!$local_path) {
    if ( $remote_path =~ m#^g# ) {
	$tmp = "/".$remote_path ;
      } else {
	$tmp = $remote_path;
      }
    print "tmp = $tmp\n";
    $local_path=$ARCHIVETOP.$tmp;
  }
  my $local_file=$local_path.$area_file;

  
  my $ftp = Net::FTP->new( $host ) || croak "  Can't open new \n";
  $ftp->login ($user, $pw )     || croak "  Can't login\n";
  $ftp->binary                  || croak "  Can't go to binary\n";
  print "  Getting $remote_file and sending it to $local_file\n";
  $ftp->get($remote_file,$local_file) || croak "  Can't get file\n";
  $ftp->quit                          || croak "  Can't close\n";
  $local_file;
  
}

#=================================================
# grid_goes
#
#=================================================


sub grid_goes {

  my $start_dir = Cwd::cwd(); 

  my $grid_path = shift @_ || 
      croak "Usage grid_goes path area_file [ minlon [ minlat [ maxlon [ maxlat ]]]]\n";
  my $area_file = shift @_ ||
      croak "Usage grid_goes path area_file [ minlon [ minlat [ maxlon [ maxlat ]]]]\n";;
  my $minlon = shift @_ || 0;
  my $minlat = shift @_ || 0 ;
  my $maxlon = shift @_ || 0 ;
  my $maxlat = shift @_ || 0 ;

  chdir $grid_path || croak "   Can't CD to $grid_path\n";
  print "  Preparing to grid area file $area_file\n";

  my $exe_string;
  if ($minlon != 0 || $minlat != 0 || $maxlon != 0 || $maxlat != 0) {
    my $minlon2=$minlon;
    my $maxlon2=$maxlon;
    $minlon2 -= 360 if ($minlon >= 180);
    $maxlon2 -= 360 if ($maxlon >= 180);
    $exe_string=sprintf( "grid_goes -f %s -l %04d,%03d,%04d,%03d", 
	    $area_file, $minlon2, $minlat, $maxlon2, $maxlat );
  } else {
      $exe_string="grid_goes -f $area_file";
  }
  print "About to open gridding processes with exe string\n";
  print "$exe_string\n";
  open ( GRIDDING_PROCESS, "$exe_string |" );
  my @gridding_output = <GRIDDING_PROCESS>;
  print join "\n", @gridding_output;
  my @errors=grep(/^ *ERROR.*/, @gridding_output);
  close GRIDDING_PROCESS;
  croak "  Bad return from goes gridding software\n" if ($#errors gt -1);
  print "  Done Gridding!\n";
  my $local_gridded_file=$grid_path.$gridding_output[$#gridding_output];
  chomp $local_gridded_file;
  chdir $start_dir  || print "  Couldn't go back to initial dir $start_dir\n";
  $local_gridded_file;
}

#=====================================================
# parsegoesfilename
#
#=====================================================
sub parsegoesfilename {

  my $start_dir = Cwd::cwd(); 

  my $goesfile=shift || croak "Can't get goes file name\n";
  
  # old format (stranded, for now)
  #          1         2         3         4
  #01234567890123456789012345678901234567890
  #GOES94_970140700-8403-%185,25,245,65%.dat
  #

  # Qscat/Seawinds era format
  #          1         2         3         4
  #01234567890123456789012345678901234567890
  #GOES094-1998060320-%185,025,245,065%.dat
  #

  
  my $satnum = substr( $goesfile, 4,  2);
  my $sensor = substr( $goesfile, 6,  1);
  my $year   = substr( $goesfile, 8,  4);
  my $month  = substr( $goesfile, 12, 2);
  my $dom    = substr( $goesfile, 14, 2);
  my $hh     = substr( $goesfile, 16, 2);
  my $limit  = substr( $goesfile, 19, 19);
  # return info
  ($satnum, $sensor, $year, $month, $dom, $hh, $limit);
  
}


#-----------------------------------------------------------
#
#
#
#-----------------------------------------------------------


sub ParseWindFileNames{
    # Takes filenames of the form Qyyyymmdd.Shhmm.Ehhmm and returns
    # a two references to the start time and end time of the files.
    # Each of these times is in 'vaptime' format, i.e. yyyymmddThh:mm

  my (@files, @start_time, @end_time);
  foreach my $file (@_) {
    my ($base,$start,$end) = split( '.',$file);
    next if !$start;
    my $yyyymmdd = substr($base,length($base)-8,8);
    $start = substr( $start, 1,4);
    $end = substr( $end, 1, 4 );
    $start = $yyyymmdd."T".$start;
    $end = $yyyymmdd."T".$end;
    my $start_time = vaptime2systime($start);
    my $end_time = vaptime2systime($end);
      # Add a day to the end time if it's less than start time.
    $end_time +=  86400 if ($start_time>$end_time);
    $start = systime2vaptime( $start_time);
    $end = systime2vaptime( $end_time);
    push @files, $file;
    push @start_time, $start;
    push @end_time, $end;
  }

  my @retarray = \(@files, @start_time, @end_time);
  @retarray;
}


#-----------------------------------------------------------
#
#
#
#-----------------------------------------------------------


sub GetWindFiles{

    # Gets the wind files that have any data between the input start
    # and stop time. Times are in 'vaptime' format yyyymmddThh:mm
    # Since we're usually interested in finding the files in some
    # interval preceeding some particular time (e.g. the previous 24 hours)
    # the second passed parameter is the 'end time' of the time period
    # and the second, optional, parameter, the begining, defaulting to
    # 24 hours. If there are no parameters, the end time defaults to now.
  my $filter = shift @_ || croak "usage files=GetWindFiles(file_filter [endtime startime])\n";
  my $end_time= shift @_ || GetNow() ;
  my $start_time = shift @_ || DeltaTime( $end_time, -24 );
  opendir WINDDIR, $WINDS_DIR || 
    croak "Couldn't open $WINDS_DIR\n";
  my @allfiles=readdir WINDDIR;
  my @windfiles=grep( /^Q*\.*.\*/, @allfiles );
  croak "grep didn't find any wind files \n" if !@windfiles;

  my @junk=ParseWindFileNames(@windfiles);
  @windfiles   = \$junk[0];
  my @file_starts = \$junk[1];
  my @file_ends   = \$junk[2];

  my $start_time_sys=vaptime2systime($start_time);
  my $end_time_sys=vaptime2systime($end_time);
  my ($test_start_time, $test_end_time);
  my @ret_file_array;

  for (my $i=0 ; $i<=$#windfiles ; $i++ ){
    $test_start_time=vaptime2systime( $file_starts[$i] );
    $test_end_time=vaptime2systime( $file_ends[$i] );

    if ($test_start_time >= $start_time_sys ||
	$test_end_time <= $end_time_sys ) {
      push @ret_file_array, $windfiles[$i];
    }
  }

  @ret_file_array;
}

#-----------------------------------------------------------
#
#
#
#-----------------------------------------------------------


sub VapMailErrorMsg{
  if ($vap_is_batch){
    my $errmsg=$_[0] || "Generic Error\n";
    my $subject=$_[1] || "<Generic Vap Error>\n";
    my $addresses=join ", ", @{$vap_defs{'Error_Mail_Address'}};
    open MAIL_MESSAGE, "|mailx -s \'$subject\' $addresses";
    print MAIL_MESSAGE $errmsg;
    close MAIL_MESSAGE;
  }
  1;
}

#-----------------------------------------------------------
# CheckForErrors
# 
#
#-----------------------------------------------------------

sub CheckForErrors{

  my $lock_file=shift or croak "usage: CheckForErrors file\n";

  do{
    VapMailErrorMsg(
	   "cloud_overlay: Couldn't open $lock_file after IDL \n",
			     "ERROR OPEN LOCKFILE");
      croak "Couldn't open $lock_file after IDL \n";} 
    unless open ( LOCK, "<$lock_file");

  my @idlout = <LOCK>;
  close (LOCK);


    # see if there are any lines in the idl output file 
    # with the work ERROR in them. Exit if there are.

  foreach (@idlout) {
    if (/ERROR/) {
      print "Error in IDL processing \n";
      print @idlout;
      my $msg=join "\n",@idlout;
      VapMailErrorMsg($msg,"ERRORINIDLPROC");
      exit;
    }
  }

  unlink $lock_file  ||  print "Couldn't delete $lock_file \n";
  1;
}

#-----------------------------------------------------------
# MoveOverlayOutput
# Copies the overlay output to the WWW directory
#
#-----------------------------------------------------------

sub MoveOverlayOutput{


  my $VAP_OVERLAY_ARCHIVE = $ENV{VAP_OVERLAY_ARCHIVE} ||
    do {
      VapMailErrorMessage("Undefined ENV var (VAP_OVERLAY_ARCHIVE)\n",
				   "UNDEFINED_ENV_VAR");
      croak "Undefined ENV var (VAP_OVERLAY_ARCHIVE)\n";
    };

  my $srcdir = shift or croak "Need source directory!\n";
  my $file_with_output_name = shift or croak "Need file with output name in it!\n";
  my $fullregionname = shift or croak "Need <FULLREGIONNAME>\n";
  my $regionname = shift or croak "Need <REGIONNAME>\n";

  do {VapMailErrorMsg(
				"cloud_overlay: Couldn't CD to $srcdir\n",
				"CD TO SOURCE DIR ERROR");
    croak " Couldn't CD to $srcdir\n\n";} unless chdir $srcdir;


  do {
    VapMailErrorMsg(
			      "cloud_overlay: Can't open $file_with_output_name\n",
			      "AUTO CLOUD OVERLAY OUTPUT FILE OPEN ERROR");
      croak "Can't open $file_with_output_name\n";
    } unless open (PICTURENAME,"<$file_with_output_name");

  my @picturefile=<PICTURENAME>;
  close(PICTURENAME);
  unlink "$file_with_output_name" || 
	print "Couldn't delete $file_with_output_name\n";

    # Read the picture name
  my $bigpicture = $picturefile[0];
  chomp $bigpicture;
  my @ext=(".JPEG",".jpeg",".jpg",".JPG");
  my ($fileroot, $thumbnailroot, $path, $ext);
  ($fileroot, $path, $ext) = fileparse($bigpicture, @ext);


  # Read the thumbnail name
  my $smallpicture = $picturefile[1];
  chomp $smallpicture;
  @ext = (".JPEG.TN",".jpeg.TN",".jpg.TN",".JPG.TN", 
	  ".JPEG.tn", ".jpeg.tn", ".jpg.tn", ".JPG.tn");
  ($thumbnailroot, $path, $ext) = fileparse($smallpicture, @ext);

     #
     #---------------------- the file ------------------------
     #

  my $file="$VAP_OVERLAY_ARCHIVE/$fileroot-$regionname.jpeg";
  print "Renaming $bigpicture to $file\n";
  do {VapMailErrorMsg(
	    "cloud_overlay: Couldn't rename $bigpicture to $file, error = $!\n",
				"PICTURE COPY ERROR");
	  croak "Couldn't rename $bigpicture to $file, error = $!\n";} 
    unless copy($bigpicture,$file);
  unlink $bigpicture;
  chmod 0755, $file || print  "Couldn't chmod $file to a+r\n";


    # create name of file in images archive to be linked to it.

  my $VAP_WWW_TOP = $ENV{VAP_WWW_TOP};
  do {
    VapMailErrorMessage("Undefined ENV var (VAP_WWW_TOP)\n",
				  "UNDEFINED_ENV_VAR");
      croak "Undefined ENV var (VAP_WWW_TOP)\n";
    } unless $VAP_WWW_TOP;

  my $wwwfile="$VAP_WWW_TOP/images/$fullregionname.jpeg";

    #delete it if it exists
  if (-e $wwwfile) {
    unlink ($wwwfile ) || print "Couldn't unlink $wwwfile\n";
  }
    # create the symbolic link to the gif file.
  print "Linking $wwwfile to $file\n";
  symlink( $file, $wwwfile ) || print "Couldn't link $wwwfile to $file \n";


     #
     #---------------------- And its thumbnail ------------------------
     #


    # copy thumbnail to image overlay archive.
  $file= "$VAP_OVERLAY_ARCHIVE/$thumbnailroot-$regionname.TN.jpeg";
  print "Renaming $smallpicture to $file\n";
  do {VapMailErrorMsg(
				"cloud_overlay: Couldn't rename $smallpicture to $file, error = $!\n",
				"THUMBNAIL COPY ERROR") ;
	  croak "Couldn't rename $smallpicture to $file, error = $!\n";} 
    unless copy($smallpicture,$file);
  unlink $smallpicture;
  chmod 0755, $file || print  "Couldn't chmod $file to a+r\n";



    # Create the symbolic name for the thumbnail

  $wwwfile="$VAP_WWW_TOP/images/$fullregionname.TN.jpeg";
    #delete it if it exists
  if (-e $wwwfile) {
    unlink ($wwwfile ) || print "Couldn't unlink $wwwfile\n";
  }

    # create the symbolic link to the gif file.
  print "Linking $wwwfile to $file\n";
  symlink( $file, $wwwfile ) || print "Couldn't link $wwwfile to $file \n";


}


1;

