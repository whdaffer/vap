#!/usr/bin/perl5  -d
# Vap.pl - Package of perl code  the vap uses
# Time-stamp: <98/09/03 10:48:48 vapuser>
# $Id$
#
# Modification History:
#
#  24-Jul-1998: Changed place where we get GOES files. (from 14 to 25)
#
#
package vap_perl;

use Cwd 'chdir';
use Time::Local;


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
  
  # get defaults filename, default to $VAP_ROOT/auto_movie_defs.dat
  $root = $ENV{'VAP_ROOT'};
  $defs_file= shift || $root."/auto_movie_defs.dat" ;

  # open the file 
  open (DEFS,"<$defs_file") || die "Can't open $defs_file\n";
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
  # cummulative number of days in the year for the first of each month
  @doys = (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ) ;
  if ($leap_year ==1 && $doy > 60 ) {
    $doy2 -= $leap_year;
  } else {
    $doy2 = $doy;
  }
  $i=0;
  while ( $doys[$i] < $doy2) {
    $i++;
  }
  $mday = $doy2 - $doys[$i-1];
  $mon = $i;
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
  $leap=vap_perl'leapyear($year);
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

  local($start_dir)=Cwd::cwd();
  local( %file_size ) = ();
  local( %file_date ) = ();

  $umask = umask;
#  print "Current umask = $umask\n";
#  print "Setting umask to 023 \n";

  umask( 023 ) || die "Couldn't reset umask to 023\n";
  $umask = umask;
#  print "New umask = $umask\n";
  $tmp = oct(023);
#  print "Should equal $tmp\n";

  @months=("Jan","Feb","Mar","Apr","May","Jun","Jul",
	   "Aug","Sep","Oct","Nov","Dec");
  @days=("Sun","Mon","Tue","Wed","Thu","Fri","Sat");

  $root=$ENV{'VAP_ROOT'};
  if ($root eq "" ) {
    die "VAP_ROOT isn't defined\n";
  }

  $VAPIM = $root."/www/htdocs/images/";
  $DOCROOT=$root."/www/htdocs/";
  
  @wwwfiles = ('daily_nepac.mov',
	       'daily_nwpac.mov',
	       'daily_npac.mov',
	       'daily_nwatl.mov',
	       'daily_indian.mov',
	       'pac_overlay_a.gif',
	       'pac_overlay_d.gif'
	       );

  @wwwnames = ('nepac_anim',
	       'nwpac_anim',
	       'npac_anim',
	       'nwatl_anim',
	       'indian_anim',
	       'npac_evol_anim',
	       'pgoa',
	       'pgod');

  %wwwhash=('nepac_anim'     => 'daily_nepac.mov',
	    'nwpac_anim'     => 'daily_nwpac.mov',
	    'npac_anim'      => 'daily_npac.mov', 
	    'nwatl_anim'     => 'daily_nwatl.mov',
	    'indian_anim'       => 'daily_indian.mov',
	    'npac_evol_anim' => 'daily_npac_evol.mov',
	    'pgoa'           => 'pac_overlay_a.gif',
	    'pgod'           => 'pac_overlay_d.gif');

  chdir $DOCROOT || die "Couldn't chdir to $DOCROOT";

  
  while( ($key,$value) = each %wwwhash ) {
    $file = $VAPIM.$value;
    if (-e $file) { 
      $tmp = (stat($file))[7] || die "Couldn't stat $file\n";
      $tmp = int($tmp*1.e-3);
      $file_size{$key}= $tmp."Kb";

      ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
       $atime,$mtime,$ctime,$junk)=stat($file);

      # convert  mod time
      ($sec,$min,$hour,$mday,$mon, $year,$wday,$yday,$isdst) = localtime($mtime); 
      $day=$days[$wday];
      $month=$months[$mon];
      @tt=&fix_time( $sec,$min, $hour, $year );
      $sec=$tt[0];
      $min=$tt[1];
      $hour=$tt[2];
      $year=$tt[3];

      # construct the file date 
      $file_date{$key} = "$day $month $mday $hour:$min:$sec $year PST";

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
  @tt=&fix_time( $sec,$min, $hour, $year );
  $sec=$tt[0];
  $min=$tt[1];
  $hour=$tt[2];
  $year=$tt[3];
  
  $mod_date_str = "$day $month $mday $hour:$min:$sec $year PST";
  $test_str = "<DAY MMM DD HH:MM:SS YYYY>";
  $lends = length($test_str);

  open(IN ,"index.html.blank");
  @in=<IN>;
  close(IN);
  rename ("index.html", "index.html.old" ) ||
      die "Couldn't rename index.html\n";
  open (OUT, ">index.html") || die "Can't open index.html\n";

  for ($i=0;$i<=$#in;$i++) {
    $rec=$in[$i];
    if ($rec !~ /^\s*<!--\s*<!--/) {
      if ($rec =~ /(anim|size|pgoa|pgod|hhmts)/) {
#       print "Found something, rec = $rec\n";
	if ($rec =~ /hhmts start/) {
	  print OUT $rec;
	  print OUT "Last Modified: $mod_date_str\n";
	  $i += 2;
	} else {
	  foreach $k (@wwwnames) {
	    if ( $rec =~ /$k/ ) {
#	    print "key = $k\n";
	      $search_string=$k."_size";
	      if ($rec =~ /.*$search_string.*/ ) {
		$rec =~ s/$search_string/$file_size{$k}/;
		$in[$i] = $rec;
		last ;
#	      print "After sub, rec = \n";
	      } elsif ($rec =~ /.*$k start/ ) {
#	      print "file mod time \n";
		print OUT $rec;
		print OUT "Created $file_date{$k} </br></br>\n";
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

sub make_hhmmss{
    $hh=$_[0];
    if ($hh<10){
	$hh="0".$hh;
    }
    $mm=$_[1] || "00";
    $ss=$_[2] || "00";
    $hhmmss=$hh.":".$mm.":".$ss;
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

  # Time is specified as yyyymmddThh:mm:ss with each unspecified
  # quantity defaulting to the current time.
  # e.g. 0404T03 is 4 of April 1998 at 3 oclock.
  # 
  #

  ($sec,$min,$hour,$mday,$mon,$year,$junk)=localtime(time);
  $yyyymmdd = make_yyyymmdd($year+1900,$mon+1,$mday);
  $hhmmss = make_hhmmss($hour,$min,$sec);
  $now = $yyyymmdd."T".$hhmmss;

  $satnum    = shift || "10";
  $sensornum = shift || "4";
  $time      = shift || $now;

  $minlon    = shift || 0;
  $minlat    = shift || 0;
  $maxlon    = shift || 0;
  $maxlat    = shift || 0;

  local($start_dir) = Cwd::cwd(); 

  ($host,$user,$pw) = vap_perl'getgoesarchive();
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
  $ARCHIVETOP=$ENV{'VAP_GOES_TOPDIR'} || "/disk2/vap/goes";
  $GRIDDINGTOP=$ENV{'VAP_GOES_GRIDDED_TOPDIR'} || "/disk2/vap/goes/gridded_files";

  
    # Get Current Local Time
  ($lsec,$lmin,$lhour,$lmday,$lmon, $lyear,$lwday,$lyday,$lisdst) = localtime($^T); 
    # Get Current Greenwich Mean Time
  ($gsec,$gmin,$ghour,$gmday,$gmon, $gyear,$gwday,$gyday,$gisdst) = gmtime($^T); 
    # Construct the input time.
  @time_parts=split "T", $time;

  if ($#time_parts == 0) {
    $yearmonthday = make_yyyymmdd( $lyear+1900, $lmon+1, $lmday );
    @hhmmss=split /:/, $time_parts[0];
  } else {
    $yearmonthday=$time_parts[0];
    @hhmmss= split /:/, $time_parts[1];
  }
  
    # Construct the 'test_time', i.e. the time against which the file's time 
    # will be compared. 

  $tyear=substr($yearmonthday,0,4)-1900;
  $tmon=substr($yearmonthday,4,2)-1;
  $tmday=substr($yearmonthday,6,2);

  $tyday=date2doy( $tyear, $tmon, $tmday );
  $thour=$hhmmss[0] || $lhour;
  $tmin =$hhmmss[1] || $lmin;
 
  $junk=sprintf("%04d%02d%02dT%02d:%02d:%02d",
             $tyear+1900,$tmon,$tmday,$thour,$tmin,$tsec);
  print "Looking for a $goes_type file from around local time $junk\n";

    # Convert to secs since the test time to GMT
  $test_time = timelocal( 0, $tmin, $thour, $tmday, $tmon, $tyear );
  die "Can't convert test time $tyear/$tmon/$tmday $thour\n" 
      if $test_time == -1;


  ($sec1,$min1,$hour1,$mday1,$mon1,$year1)=gmtime($test_time);
  $equivalent_gmt_time=sprintf("%04d%02d%02dT%02d:%02d:%02d",
         $year1+1900,$mon1+1,$mday1,$hour1,$min1,$sec1);
  print "  Which will be about GMT $equivalent_gmt_time\n";


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
      $year=$tmp2[2]-1900;
    }
    $file_time=timegm(0,$min,$hour,$mday,$mon,$year);
    $diff=abs( $file_time-$test_time);
    if ($diff < $min_diff) {
	$area_file=$filename;
	$area_file_time=$file_time; 
        ($sec1,$min1,$hour1,$mday1,$mon1,$year1)=gmtime( $area_file_time );
        $area_file_time_string =
           sprintf("%04d%02d%02dT%02d:%02d:%02d",$
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

    $cwd=$ENV{'PWD'};
    chdir $local_path || die "Couldn't cd to $local_path to run mkai\n";
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

    $r=system( $exe_str )/256;
    if ($r != 0) {
      print "   Some kind of error in mkai\n";
    }
    chdir $cwd || die "Couldn't cd back to $cwd\n";


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
      $year=$tmp2[2]-1900;
      $local_area_file_time=timegm(0,0,$hour,$mday,$mon,$year);      
      $test2 = abs($local_area_file_time == $file_times[$closest]);
      last;
    }
  }  


  if ( $test2 >= 1/24. ) {

      # either we don't have this area file, or it isn't the right one.
      # go to the archive are retrieve it.

    print "   Not here! Retrieving $area_file from the NOAA archive \n";
#     $ftp = Net::FTP->new( $host ) || die "  Can't open new \n";
#     $ftp->login ($user, $pw ) || die "  Can't login\n";
#     $ftp->binary || die "  Can't go to binary\n";
#     $ftp->get($remote_area_file,$local_area_file) ||  die "  Can't get file\n";
#     $ftp->quit || die "  Can't close\n";
#     print "  Done retrieving file!\n";

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
      $tyear=$year+1900;
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

sub local2gmtime{
    # Assumes the input array is suitable for passing to timelocal, 
    # which has the prototype
    # 
    # $time = timelocal($sec,$min,$hours,$mday,$mon,$year);


    # Find the offset between local time and GM time.
  ($sec,$min,$hour,$mday,$mon, $year,$wday,$yday,$isdst) = localtime(time); 
  $ltime = timelocal($sec,$min,$hour,$mday,$mon, $year);
  $gtime = timegm($sec,$min,$hour,$mday,$mon, $year);
  $diff= $gtime-$ltime;
  
  
  $time = timegm( @_ ) -$diff;
  return $time;

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

  if (minlon != 0 || minlat != 0 || maxlon != 0 || maxlat != 0) {
    $minlon2=$minlon;
    $maxlon2=$maxlon;
    $minlon2 -= 360 if ($minlon >= 180);
    $maxlon2 -= 360 if ($maxlon >= 180);
    $exe_string=sprintf( "grid_goes -f %s -l %04d,%03d,%04d,%03d", 
	    $local_area_file, $minlon2, $minlat, $maxlon2, $maxlat );
  } else {
      $exe_string="grid_goes -f $area_file";
  }
  open ( GRIDDING_PROCESS, "$exe_string |" );
  @gridding_output = <GRIDDING_PROCESS>;
  @errors=grep(/^ *ERROR.*/, @gridding_output);
  close GRIDDING_PROCESS;
  die "  Bad return from goes gridding software\n" if ($#errors gt -1);
  print "  Done Gridding!\n";
  $local_gridded_file=$grid_path.$gridding_output[0];
  chop $local_gridded_file;
  chdir $start_dir  || print "  Couldn't go back to initial dir $start_dir\n";
  $local_gridded_file;
}
1;


