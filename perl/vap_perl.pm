#!/usr/bin/perl5  
# Vap.pl - Package of perl code  the vap uses
# Time-stamp: <99/08/20 16:28:30 vapuser>
# $Id$
#
# Modification History:
#
# $Log$
# Revision 1.5  1999/04/02 22:29:22  vapuser
# More changes than I care to even think about.
#
# Revision 1.4  1998/10/27 16:27:35  vapuser
# Fixed some problems with getcwd and undefined ARCHIVETOP and GRIDDINGTOP
# variables
#
# Revision 1.3  1998/10/22 21:37:09  vapuser
# To much work to relate. Do a diff if you really care.
#
#
#
package vap_perl;

require Exporter;
@ISA = qw(Exporter);
@EXPORT=qw( $VAP_LIB $VAP_ROOT $VAP_WINDS $VAP_ANIM 
	   $VAP_OVERLAY $VAP_WWW_TOP $ARCHIVETOP 
	   $GRIDDINGTOP $IDLEXE $VAP_OVERLAY_ARCHIVE 
	   $VAP_WWW_TOP auto_movie_defs doy2mday_mon 
	   date2doy date_string  make_yyyymmdd gag grid_goes 
	   getgoesfile fixlonrange vaptime2systime systime2vaptime 
	   ParseWindFileNames GetWindFiles GetNow DeltaTime 
	   ParseVapTime vaptime2idltime );

use Cwd 'chdir', 'getcwd';
use Time::Local;
BEGIN {
    # Get ENV variables

  $VAP_LIB=$ENV{'VAP_LIB'}                  || "/usr/people/vapuser/Qscat/Library";
  $VAP_ROOT   = $ENV{'VAP_ROOT'}            || "/disk2/vap";
  $VAP_WINDS  = $ENV{'VAP_WINDS'}           ||  "/disk3/qscat_winds";
  $VAP_ANIM   = $ENV{'VAP_ANIM'}            || $VAP_ROOT."/anim";
  $VAP_OVERLAY = $ENV{'VAP_OVERLAY'}        || $VAP_ROOT."/overlay";
  $VAP_WWW_TOP = $ENV{'VAP_WWW_TOP'}        || $VAP_ROOT."/www/htdocs";
  $ARCHIVETOP  = $ENV{'VAP_GOES_TOPDIR'}    || $VAP_ROOT."/goes";
  $GRIDDINGTOP = $ENV{'VAP_GOES_GRIDDED_TOPDIR'} || 
      $ARCHIVETOP."gridded_files";
  $IDLEXE=$ENV{'IDLEXE'};
  $VAP_OVERLAY_ARCHIVE = $ENV{'VAP_OVERLAY_ARCHIVE'} ||
      $VAP_WWW_TOP."/images/overlay_archive";

    # MOVIE DEFS
  $auto_movie_defs_file=$VAP_LIB."/auto_movie_defs.dat";
    # Get the Overlay Defaults
  $overlay_defs_file=$VAP_LIB."/overlay_defs";
  require $overlay_defs_file;


}

%sat_num= ('8','9' , # AREA08* are goes 9 files !
	   '9','8' , # AREA09* are goes 8 files !
	   '10','8' , # AREA10* are goes 8 files !
	   );
%sensor_dir = ('1','vis',
	       '2','ir2',
	       '3','ir3',
	       '4','ir4',
	       '5','ir5'
	       );


sub auto_movie_defs {
  # parses the file $VAP_ROOT/auto_movie_defs.dat returns array
  # containing the ROI designations
  # (i.e. 'nepac','nwpac','npac','nwatl' and whatever other regions of
  # interest we may decide to do.
  

  # open the file 
  open (DEFS,"<$auto_movie_defs_file") || die "Can't open $auto_movie_defs_file\n";
  @defs =<DEFS>;
  close DEFS;

  # loop over records in the file taking the 'value' of the 'desig'
  # field in each record. Each record is a string suitable for
  # defining an IDL structure in an IDL 'execute' call, i.e. it looks
  # like { F1:X1, F2:X2, F3:X3 ... }.  So we split on the ',' then
  # find 'desig' and split on the ":"


  foreach $r ( @defs) {
    next if $r =~ /^;.*$/; # next if IDL comment line
    next if $r =~ /^\s*$/; # next if empty line
    @tmp = split(/,/, $r);
    $tmp = $tmp[0];
    @tmp = split(/:/, $tmp);
    push @desigs, $tmp[1];
  }
  @desigs;
}

sub doy2mday_mon{
  $doy=shift;
  $year=shift;
  # cummulative number of days in the year for the end of each month
  @doys = (31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365) ;
  $leapyear=leapyear($year);

  if ($leapyear ==1 && $doy > 60 ) {
    $doy2 -= $leapyear;
  } else {
    $doy2 = $doy;
  }
  if ($doy2<=31) {
    $mon=1;
    $mday=$doy2;
  } else {

    $i=1;
    while ( $doys[$i] < $doy2 && $i<=12) {
      $i++;
    }
    $mday = $doy2 - $doys[$i-1];
    $mon = $i;

  }

  push (@mday_mon, ($mday,$mon) );
  @mday_mon;

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
  $leap=vap_perl::leapyear($year);
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

  local( $start_dir)=Cwd::cwd();
  local( %file_size ) = ();
  local( %file_date ) = ();

  $umask = umask;
  print "Current umask = $umask\n";
  print "Setting umask to 023 \n";

  umask( 023 ) || die "Couldn't reset umask to 023\n";
  $umask = umask;
  print "New umask = $umask\n";
  $tmp = oct(023);
  print "Should equal $tmp\n";

  @months=("Jan","Feb","Mar","Apr","May","Jun","Jul",
	   "Aug","Sep","Oct","Nov","Dec");
  @days=("Sun","Mon","Tue","Wed","Thu","Fri","Sat");

  $VAPIM = $vap_perl::VAP_WWW_TOP."/images/";
  $DOCROOT=$vap_perl::VAP_WWW_TOP;
  
  @wwwfiles = ('daily_nepac.mov',
	       'daily_nwpac.mov',
	       'daily_npac.mov',
	       'daily_nwatl.mov',
	       'daily_indian.mov',
	       'daily_atlhurr.mov',
	       'daily_pachurr.mov',
	       'GOES104NEPAC1.jpeg',
	       'GOES84NWATL1.jpeg',
	       'GMS51WPAC1.jpeg',
	       'GMS51JAPAN1.jpeg',
	       'GMS51FAREAST1.jpeg'
	       );

  @wwwnames = ('nepac_anim',
	       'nwpac_anim',
	       'npac_anim',
	       'nwatl_anim',
	       'indian_anim',
	       'atlhurr_anim',
	       'pachurr_anim',
#	       'npac_evol_anim',
	       'goes104nepac1',
	       'goes84nwatl1',
	       'gms51wpac1',
	       'gms51japan1',
	       'gms51fareast1'

);

  %wwwhash=('nepac_anim'     => 'daily_nepac.mov',
	    'nwpac_anim'     => 'daily_nwpac.mov',
	    'npac_anim'      => 'daily_npac.mov', 
	    'nwatl_anim'     => 'daily_nwatl.mov',
	    'indian_anim'    => 'daily_indian.mov',
	    'atlhurr_anim'   => 'daily_atlhurr.mov',
	    'pachurr_anim'   => 'daily_pachurr.mov',
#	    'npac_evol_anim' => 'daily_npac_evol.mov',
	    'goes104nepac1'  => 'GOES104NEPAC1.jpeg',
	    'goes84nwatl1'   => 'GOES84NWATL1.jpeg',
	    'gms51wpac1'     => 'GMS51WPAC1.jpeg',
	    'gms51japan1'    => 'GMS51JAPAN1.jpeg', 
	    'gms51fareast1'  => 'GMS51FAREAST1.jpeg'
);

  chdir $DOCROOT || die "Couldn't chdir to $DOCROOT";

  
  while( ($key,$value) = each %wwwhash ) {
    $file = $VAPIM.$value;
    if (-e $file) { 

      ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
       $atime,$mtime,$ctime,$junk)=stat($file) || die "Couldn't stat $file\n";;

      ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
       $atime,$mtime,$ctime,$junk)=stat($file) if !$nlink;
      
      $file_size{$key}= $tmp = sprintf( "%d Kb", int( $size*1.e-3+0.5) );
      # convert  mod time
      ($sec,$min,$hour,$mday,$mon, $year,$wday,$yday,$isdst) = localtime($mtime); 
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
  ($sec,$min,$hour,$mday,$mon,
   $year,$wday,$yday,$isdst) = localtime(time);
  
  $day=$days[$wday];
  $month=$months[$mon];
  $mod_date_str = sprintf( "%03s %3s %02d %02d:%02d:%02d %04d PST",
			  $day, $month, $mday, $hour,$min,$sec, $year);

  open(IN ,"index.html.blank");
  @in=<IN>;
  close(IN);
  rename ("index.html", "index.html.old" ) ||
      die "Couldn't rename index.html\n";
  open (OUT, ">index.html") || die "Can't open index.html\n";
  $search_string1 = "(hhmts|";
  $search_string1 .= join("|",@wwwnames);
  $search_string1 .= ")";

  for ($i=0;$i<=$#in;$i++) {
    $rec=$in[$i];
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
	  foreach $k (@wwwnames) {
	    if ( $rec =~ /$k/ ) {
#	    print "key = $k\n";
	      $search_string2=$k."_size";
	      if ($rec =~ /.*$search_string2.*/ ) {
		$rec =~ s/$search_string/$file_size{$k}/;
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
  chdir $start_dir || die "Couldn't go back to $start_dir\n";
  close OUT;
  1;
}


sub fix_time{

  $sec=shift;
  $min=shift;
  $hour=shift;
  $year=shift;
  if ($sec < 10){
    $sec = "0".$sec;
  }
  if ($min < 10) {
    $min = "0".$min;
  }
  if ($hour < 10) {
    $hour = "0".$hour;
  }
  $year += 1900;
  $ret[0]=$sec;
  $ret[1]=$min;
  $ret[2]=$hour;
  $ret[3]=$year;  
  @ret;
}

sub make_yyyymmdd {
  $year=shift;
  $month=shift;
  $mday=shift;
  if ($month < 10) {
    $month = "0".$month;
  }
  if ($mday < 10) {
    $mday = "0".$mday;
  }
  $yyyymmdd=$year.$month.$mday;
  $yyyymmdd;

}

sub make_hhmm{
    $hh=$_[0];
    $mm=$_[1] || 0;
    $hhmm=sprintf("%02d:%02d",$hh,$mm);
    $hhmm;
}

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

  ($sec,$min,$hour,$mday,$mon,$year,$junk)=gmtime(time);
  $yyyymmdd = make_yyyymmdd($year+1900,$mon+1,$mday);
  $hhmm = make_hhmm($hour,$min);
  $now = $yyyymmdd."T".$hhmm;

  $satnum    = shift || "10";
  $sensornum = shift || "4";
  $time      = shift || $now;

  $minlon    = shift || 0;
  $minlat    = shift || 0;
  $maxlon    = shift || 0;
  $maxlat    = shift || 0;

  local($start_dir) = Cwd::cwd(); 

  ($host,$user,$pw) = getgoesarchive();
  die "Can't get goes Archive info\n" unless defined $pw;


    # given the satellite, returns x of AREAx
    # Goes 10 files have the name goes8, goes 8 have names AREA9
    # $sat_num{10} = 8;
   %sat_num= ( '10','8', 
 	      '8','9',  
 	     );

   %sensor_dir = (
          '1','vis',
 	  '2','ir2',
 	  '3','ir3',
 	  '4','ir4',
 	  '5','ir5'
 	  );

  $goes_type="goes".$satnum." ".$sensor_dir{$sensornum};

    # Get ENV variables
  #$ARCHIVETOP=$ENV{'VAP_GOES_TOPDIR'} || "/disk2/vap/goes";
  #$GRIDDINGTOP=$ENV{'VAP_GOES_GRIDDED_TOPDIR'} || "/disk2/vap/goes/gridded_files";

  
    # Get Current Greenwich Mean Time
  ($gsec,$gmin,$ghour,$gmday,$gmon, $gyear,$gwday,$gyday,$gisdst) = gmtime($^T); 
    # Construct the input time.
  @time_parts=split "T", $time;

  if ($#time_parts == 0) {
    $yearmonthday = make_yyyymmdd( $gyear+1900, $gmon+1, $gmday );
    @hhmm= split /:/, $time_parts[0];
  } else {
    $yearmonthday=$time_parts[0];
    @hhmm = split /:/, $time_parts[1];
  }

    # Construct the 'test_time', i.e. the time against which the area file's time 
    # will be compared. 

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
  $test_time = timegm( 0, $tmin, $thour, $tmday, $tmon, $tyear );
  die "Can't convert test time $tyear/$tmon/$tmday $thour\n" 
      if $test_time == -1;

    # Construct file test times. The times in the 
    # area info file are already in GMT.);

    # NB, this loop is reading the 'noaa_area_info' file.
    # Construct the filename for the noaa_area_info file.
  $noaa_area_info_filename=$ARCHIVETOP."/goes".$satnum."/";
  $noaa_area_info_filename.=$sensor_dir{$sensornum}."/noaa_area_info";
    # Open 'info' file
  open(NOAA_AREA_INFO, "<$noaa_area_info_filename") || 
      die "Couldn't open noaa_area_info $noaa_area_info_filename file \n";
  @noaa_area_info = <NOAA_AREA_INFO>;
  close(NOAA_AREA_INFO);

  $min_diff=1e20;
  foreach $rec (@noaa_area_info) {
    next if $rec =~ /^##/;

    @tmp=split(" ", $rec);
    $filename=substr( $tmp[0], 0, 8 );
    $hour = substr( $tmp[6], 0, 2 ); 
    $min = substr( $tmp[6], 2,2 );
    @tmp2=split('/',$tmp[4] );
    if ($#tmp2 < 2) {
      print "  Corrupted data/time for $filename\n";
        # Somehow, this field (mm/dd/yyyy) in the area_info file has
        # been corrupted, so we're going to read the file itself.
      $local_path=$ARCHIVETOP."/goes".$satnum."/".$sensor_dir{$sensornum}."/";
      $ttmp=$local_path.$filename;
      if (open( AREAFILE, "<$ttmp" )){
	read AREAFILE, $hdr, 64*4;
	close AREAFILE;
	@hdr=unpack "L4", $hdr;
	$year=int ($hdr[3]/1000);
	$doy=int ($hdr[3]-$year*1000);
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
    $noaa_file_time=timegm(0,$min,$hour,$mday,$mon,$year-1900);
    $diff=abs( $noaa_file_time-$test_time);
    if ($diff < $min_diff) {
	$area_file=$filename;
	$area_file_time=$noaa_file_time; 
        ($sec1,$min1,$hour1,$mday1,$mon1,$year1)=gmtime( $area_file_time );
        $area_file_time_string =
           sprintf("%04d%02d%02dT%02d:%02d:%02d",
		   $year1+1900,$mon1+1,$mday1,$hour1,$min1,$sec1);
	$min_diff=$diff;
    }

  }
  $diffhrs=$min_diff/3600.;
  print "min_diff=$min_diff, diffhrs=$diffhrs\n";
  die "Closest File ($area_file) is over $diffhrs hours distant!\n" if $diffhrs>2.;

  print "  AREA file for this run = $area_file\n";

  print "    Whose Time is $area_file_time_string\n";
  print "  Checking to see whether it's already here\n";

  $off=4;

  $tmp=substr( $area_file,$off,2 );
  $sat=substr( $area_file, $off, 2 );
  $sen=substr( $area_file, $off+2, 1);
  $filenum = substr( $area_file, $off+3, 2);

  # Construct path and file name on remote server.
  $remote_path = "goes".$satnum."/".$sensor_dir{$sensornum}."/";

  if ( $remote_path =~ m#^g# ) {
    $tmp = "/".$remote_path ;
  } else {
    $tmp = $remote_path;
  }

    # Construct the path and filename for the AREA file 
    # on this (haifung) machine.
    # While we're at it, might as well produce the path 
    # to the gridding files.
  $local_path=$ARCHIVETOP.$tmp;
  $gridding_path=$GRIDDINGTOP.$tmp;
  $local_area_file=$local_path.$area_file;
  $test2 = 1.e20;
  if (-e $local_area_file) {
 
        # the file exists on this machine. Make sure the local area_info
        # file is up to date by running mkai in the target
        # directory. See if the area file there is the same one.

    $cwd=Cwd::getcwd();
    chdir $local_path || die "Couldn't cd to $local_path to run mkai\n";
    $t=Cwd::getcwd();
    print "Now in $t\n";
    $exe_str = "/usr/people/vapuser/bin/mkai ";
    if ($user =~ /root/){
      ($name,$passwd,$uid,$gid,$quota,$comment,$gcos,$dir,$shell) = 
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
    $r=system( $exe_str )/256;
    if ($r != 0) {
      print "   Some kind of error in mkai\n";
    }
    chdir $cwd || die "Couldn't cd back to $cwd\n";
    print "Done running mkai\n";

        # read the local area_info file, see if the area file we have
        # is from the same time.

    $local_ainf = $local_path."/area_info";
    open ( INF, "<$local_ainf") || die "Couldn't open $local_ainf\n";
    @ainf=<INF>;
    close (INF);

    foreach $r (@ainf) {
      next if $r =~ /^##/;
      @tmp=split(" ", $r);
      next if substr($tmp[0],0,8) !~ /$area_file/;

      @tmp=split(" ", $r);

      $hour = substr( $tmp[6], 0, 2 ); 

      @tmp2=split('/',$tmp[4] );

      $mon=$tmp2[0]-1;
      $mday=$tmp2[1];
      $year=substr($tmp2[2],0,4);
      $local_area_file_time=timegm(0,0,$hour,$mday,$mon,$year-1900);      
      $test2 = abs($local_area_file_time - $area_file_time);
      last;
    }
  }  

    # test2 is in seconds
  if ( $test2 >= 3600 ) { # more than 1 hour difference.

      # either we don't have this area file, or it isn't the right one.
      # go to the archive are retrieve it.

    print "   Not here! Retrieving $area_file from the NOAA archive \n";
    $file=getgoesfile( $area_file);

    die "Error retrieving $area_file\n" if (!file);
      
        # If we're running this as root, change owner/group and
        # permissions

    $user=$ENV{'USER'};
    if ($user =~ /root/){
      ($name,$passwd,$uid,$gid,$quota,$comment,$gcos,$dir,$shell) = 
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
      $iminlon=int $minlon;
      $iminlat=int $minlat;
      $imaxlon=int $maxlon;
      $imaxlat=int $maxlat;
      if ($iminlon==0 && $iminlat==0 && $imaxlon==0 && $imaxlat==0) {
	  if ($satnum == 10) {
	      $iminlon=160;
	      $iminlax=0;
	      $imaxlon=-70;
	      $imaxlat=70;
	  } elsif ($satnum == 8) {
	      # Don't know what then limits are on Goes 8 files.
	  } else {
	      #Job security
	  }
      }
      $goes_type=$satnum.$sensornum;
      $tyear=$year;
      $tmonth=$month+1;
      $grid_file_name=sprintf( "GOES%03d-%04d%02d%02d%02d-%%%04d,%03d,%04d,%03d%%.dat", 
			      $goes_type, $tyear, $tmonth, $mday, $hour, 
			      $iminlon, $iminlat, $imaxlon, $imaxlat );
      
      
      $local_gridded_file=$gridding_path.$grid_file_name;


      # construct the name of the gridded file.
      print "   Testing for presence of a grid file matching file glob\n   $local_gridded_file\n";
      if (-e $local_gridded_file) {
	  print "  $grid_file already exists, I'll return this file instead\n";
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
  $local_gridded_file=grid_goes( $gridding_path, $area_file, 
				$minlon, $minlat, $maxlon, $maxlat );
  print "local gridded_file = $local_gridded_file\n";
  die "Error Gridding area file $area_file\n" if !$local_gridded_file;
  $local_gridded_file;

} # End GAG



sub byctime {
  $ctime{$a} <=> $ctime{$b};
}

sub pgl { 
  #
  # pgl : Parse goes log
  #
  #
  $logfile=shift(@_);;
  open( LOGFILE, "<$logfile");
  while (<LOGFILE>) {

    if ( /^Sorry, cannot find it.\s$/) {
      $newfile = "NO_SUCH_FILE";
      $skip_rename=1;
    } else {
      if (/^::[\s]+output to /){
	@tmp=split(" ");
	$oldfile=$tmp[$#tmp];
	@tmp = split( /\./, $oldfile );
	$file = $tmp[0];
      }

      if (/^[\s]+minlat minlon[\s]+/){
	@tmp=split(" ");
	$minlat = $tmp[3];
	$minlon = $tmp[4];
	if ($minlon < 0) {
	  $minlon += 360 ;
	}
      }

      if (/^[\s]+maxlat maxlon[\s]+/){
	@tmp=split(" ");
	$maxlat = $tmp[3];
	$maxlon = $tmp[4];
	if ($maxlon < 0) {
	  $maxlon += 360 ;
	}
      }


      if (/precision (deg)/){
	@tmp=split(" ");
	$plat = $tmp[$3];
	$plon = $tmp[$4];
      }

      if (/^:: Now read in/) {
	@tmp = split( " " );
	$areafile = $tmp[$#tmp-1];
	@tmp = split( "/", $areafile );
	$areafile = $tmp[$#tmp];
	$last4 = substr( $areafile, 4, 4 );
      }
    }
  } # end while
  if (!$skip_rename) {
    $limstr = "%".$minlon.",".$minlat.",".$maxlon.",".$maxlat."%";
    #$time=substring( $time, 0, 4 );
    $newfile=$file."-".$last4."-".$limstr.".dat";
    rename( $oldfile, $newfile) || die " couldn't rename $oldfile to $newfile \n";
    chmod 0755, $newfile || print "Couldn't chmod 0755 on $newfile \n";

  }
  $newfile;

}
sub subtract_one_day{
    


    # input the month as the real (1 indexed) month, not the 0 indexed
    # monster from the tm struct!

  $year=shift;
  $mon=shift;
  $mday=shift;
  $leap_year = ($year % 100 != 0) && ($year % 4 == 0);
  if ($year % 400 == 0) {
    $leap_year = 1;
  }
  @days_in_month=(31,28,31,30,31,30,31,31,30,31,30,31);
  
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
  if ($mday < 10) {
    $mday = "0".$mday;
  }
  if ($mon < 10) {
    $mon = "0".$mon;
  }

   # return modified date
  ($year,$mon,$mday);      
}

sub add_one_day{
    


    # input the month as the real (1 indexed) month, not the 0 indexed
    # monster from the tm struct!

  $year=shift;
  $mon=shift;
  $mday=shift;
  $leap_year = ($year % 100 != 0) && ($year % 4 == 0);
  if ($year % 400 == 0) {
    $leap_year = 1;
  }
  @days_in_month=(31,28,31,30,31,30,31,31,30,31,30,31);
  
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
  if ($mday < 10) {
    $mday = "0".$mday;
  }
  if ($mon < 10) {
    $mon = "0".$mon;
  }

   # return modified date
  ($year,$mon,$mday);      
}

sub getgoesarchive{ 

  local($start_dir) = Cwd::cwd(); 

  $goes_info_file=shift || "/usr/people/vapuser/Qscat/Library/goes_archive";
  open(ARCHIVE_INFO, "<$goes_info_file") || die "Can't open $goes_info_file file\n";
  @info=<ARCHIVE_INFO>;
  $host=$info[0];
  chop $host;
  $user=$info[1];
  chop $user;
  $pw=$info[2];
  chop $pw;
  # return the info.
  ($host, $user, $pw);
}

sub parsegoesfilename {

  local($start_dir) = Cwd::cwd(); 

  $goesfile=shift || die "Can't get goes file name\n";
  
  # old format (stranded, for now)
  #          1         2         3         4
  #01234567890123456789012345678901234567890
  #GOES94_970140700-8403-%185,25,245,65%.dat
  #

  # new format
  #          1         2         3         4
  #01234567890123456789012345678901234567890
  #GOES094-1998060320-%185,025,245,065%.dat
  #

  $satnum = substr( $goesfile, 4,  2);
  $sensor = substr( $goesfile, 6,  1);
  $year   = substr( $goesfile, 8,  4);
  $month  = substr( $goesfile, 12, 2);
  $dom    = substr( $goesfile, 14, 2);
  $hh     = substr( $goesfile, 16, 2);
  $limit  = substr( $goesfile, 19, 19);
  # return info
  ($satnum, $sensor, $year, $month, $dom, $hh, $limit);
  
}
sub leapyear{

  $year=shift;
  $leap_year = ($year % 100 != 0) && ($year % 4 == 0);
  if ($year % 400 == 0) {
    $leap_year = 1;
  }
  $leap_year;
}


sub getgoesfile{

  require Net::FTP;
  local($start_dir) = Cwd::cwd(); 
  ($host,$user,$pw) = getgoesarchive();
  die "Can't get goes Archive info\n" unless defined $pw;

  %sat_num= ( '8', '10', # AREA8* are goes 10 files !
	     '9',  '8',   # AREA9* are goes 8 files !
	     );
  
  %sensor_dir = ('1','vis',
		 '2','ir2',
		 '3','ir3',
		 '4','ir4',
		 '5','ir5'
		 );
  
  $area_file=shift;
  if ($area_file =~ /^A/) {
    ;
  } else {
    $area_file="AREA".$area_file
      }
  $off=4;
  $tmp=substr( $area_file,$off,2 );
  $sat=substr( $area_file, $off, 1 );
  $sen=substr( $area_file, $off+1, 1);
  $filenum = substr( $area_file, $off+2, 2);

  $remote_path = "goes".$sat_num{$sat}."/".$sensor_dir{$sen}."/";
  $remote_file = $remote_path.$area_file;
  
  print "remote_path = $remote_path\n";
  print "remote_file = $remote_file\n";
  $local_path = shift;
  if (!$local_path) {
    if ( $remote_path =~ m#^g# ) {
	$tmp = "/".$remote_path ;
      } else {
	$tmp = $remote_path;
      }
    print "tmp = $tmp\n";
    $local_path=$ARCHIVETOP.$tmp;
  }
  $local_file=$local_path.$area_file;

  
  $ftp = Net::FTP->new( $host ) || die "  Can't open new \n";
  $ftp->login ($user, $pw )     || die "  Can't login\n";
  $ftp->binary                  || die "  Can't go to binary\n";
  print "  Getting $remote_file and sending it to $local_file\n";
  $ftp->get($remote_file,$local_file) || die "  Can't get file\n";
  $ftp->quit                          || die "  Can't close\n";
  $local_file;
  
}

sub grid_goes {

  local($start_dir) = Cwd::cwd(); 

  $grid_path = shift @_ || 
      die "Usage grid_goes path area_file [ minlon [ minlat [ maxlon [ maxlat ]]]]\n";
  $area_file = shift @_ ||
      die "Usage grid_goes path area_file [ minlon [ minlat [ maxlon [ maxlat ]]]]\n";;
  $minlon = shift @_ || 0;
  $minat  = shift @_ || 0 ;
  $maxlon = shift @_ || 0 ;
  $maxat  = shift @_ || 0 ;

  chdir $grid_path || die "   Can't CD to $grid_path\n";
  print "  Preparing to grid area file $area_file\n";

  if ($minlon != 0 || $minlat != 0 || $maxlon != 0 || $maxlat != 0) {
    $minlon2=$minlon;
    $maxlon2=$maxlon;
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
  @gridding_output = <GRIDDING_PROCESS>;
  print join "\n", @gridding_output;
  @errors=grep(/^ *ERROR.*/, @gridding_output);
  close GRIDDING_PROCESS;
  die "  Bad return from goes gridding software\n" if ($#errors gt -1);
  print "  Done Gridding!\n";
  $local_gridded_file=$grid_path.$gridding_output[$#gridding_output];
  chop $local_gridded_file;
  chdir $start_dir  || print "  Couldn't go back to initial dir $start_dir\n";
  $local_gridded_file;
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

sub ParseWindFileNames{
    # Takes filenames of the form Qyyyymmdd.Shhmm.Ehhmm and returns
    # a two references to the start time and end time of the files.
    # Each of these times is in 'vaptime' format, i.e. yyyymmddThh:mm

  foreach $file (@_) {
    ($base,$start,$end) = split( '.',$file);
    next if !$start;
    $yyyymmdd = substr($base,length($base)-8,8);
    $start = substr( $start, 1,4);
    $end = substr( $end, 1, 4 );
    $start = $yyyymmdd."T".$start;
    $end = $yyyymmdd."T".$end;
    $start_time = vaptime2systime($start);
    $end_time = vaptime2systime($end);
      # Add a day to the end time if it's less than start time.
    $end_time +=  86400 if ($start_time>$end_time);
    $start = systime2vaptime( $start_time);
    $end = systime2vaptime( $end_time);
    push @files, $file;
    push @start_time, $start;
    push @end_time, $end;
  }

  @retarray = \(@files, @start_time, @end_time);
  @retarray;
}

sub GetWindFiles{

    # Gets the wind files that have any data between the input start
    # and stop time. Times are in 'vaptime' format yyyymmddThh:mm
    # Since we're usually interested in finding the files in some
    # interval preceeding some particular time (e.g. the previous 24 hours)
    # the first passed parameter is the 'end time' of the time period
    # and the second, optional, parameter, the begining, defaulting to
    # 24 hours. If there are no parameters, the end time defaults to now.

  $end_time= shift @_ || GetNow() ;
  $start_time = shift @_ || DeltaTime( $end_time, -24 );
  opendir WINDDIR, $VAP_WINDS || die "Couldn't open $VAP_WINDS\n";
  @allfiles=readdir WINDDIR;
  @windfiles=grep( /^Q*\.*.\*/, @allfiles );
  die "grep didn't find any wind files \n" if !$windfiles;

  @junk=ParseWindFileNames(@windfiles);
  @windfiles   = \$junk[0];
  @file_starts = \$junk[1];
  @file_ends   = \$junk[2];

  $start_time_sys=vaptime2systime(start_time);
  $end_time_sys=vaptime2systime(end_time);

  for ($i=0 ; $i<=$#windfiles ; $i++ ){
    $test_start_time=vaptime2systime( $file_starts[$i] );
    $test_end_time=vaptime2systime( $file_ends[$i] );

    if ($test_start_time >= start_time_sys ||
	$test_end_time <= end_time_sys ) {
      push @ret_file_array, $windfiles[$i];
    }
  }

  @ret_file_array;
  

}

sub GetNow {
  @now=systime2vaptime( gmtime(time) );
  @now;
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

sub ParseVapTime{
  $vaptime=shift;
  
  @time_parts=split('T',$vaptime);
  $year=substr($time_parts[0],0,4);
  $month=substr($time_parts[0],4,2);
  $day=substr($time_parts[0],6,2);
  @tmp=split(":",$time_parts[1]);
  $hh=$tmp[0];
  $mm=$tmp[1];
  ($mm,$hh,$day,$month,$year);

}


sub vaptime2idltime{
  @parsed_time=ParseVapTime( $_[0] );
  $idltime=join('/',reverse @parsed_time);
  $idltime
}

1;
