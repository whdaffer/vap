#!/usr/bin/perl5  
# Vap.pl - Package of perl code  the vap uses
# Time-stamp: <98/09/01 09:49:23 vapuser>
# $Id$
#
# Modification History:
#
#  24-Jul-1998: Changed place where we get GOES files. (from 14 to 25)
#
#
package vap_perl;

use Cwd 'chdir';


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

sub gag {
  # Usage: gag [a | d ]
  #
  # gag=GetAndGrid : gets the Goes9 ir4 file closest to 7 and 20 and
  # grids it, putting the output in /disk2/vap/overlay.
  #
  # a = ascending, will get file close to 07:00 GMT (the default)
  #
  # d = descending, will get file close to 20:00 GMT
  #
  #
  # This perl script parses the file 'noaa_area_info' in the goes9 ir4 directory
  # in our mirror of the noaa goes site and returns the name of the file
  # closest to the 7:00 GMT (if arg 1 = a) or the one closest to 20:00 (if arg 1 = d)
  #
  #
  
  $start_dir=Cwd::cwd();
  require Net::FTP;
  #$host="orbit25i.nesdis.noaa.gov";
  #$user="mspencer";
  #$pw="goes&nscat";

  ($host,$user,$pw) = vap_perl'getgoesarchive();
  die "Can't get goes Archive info\n" unless defined $pw;



#   %sat_num= ( '8','9' , # AREA8* are goes 9 files !
# 	      '9','8' , # AREA9* are goes 8 files !
# 	     );
#   %sensor_dir = ('1','vis',
# 	  '2','ir2',
# 	  '3','ir3',
# 	  '4','ir4',
# 	  '5','ir5'
# 	  );

  # AM or PM?
  $type=shift;
  if ($type =~ /^a.*/i) {
    $time=7/24.;
  } else {
    $time=20/24.;
  }
  $file_test_time = $time + 2.5/24.;
  
  # Open 'info' file
  open(INFO, "</disk2/vap/goes/goes10/ir4/noaa_area_info") || 
      die "Couldn't open noaa_area_info file \n";
  @info = <INFO>;
  close(INFO);
  
  # Get Current time
  ($sec,$min,$hour,$mday,$mon, $year,$wday,$yday,$isdst) = gmtime(time); 
  $yday += 1;
  if ($hour/24. < $file_test_time ) {
    print " Too early to try for todays file, check for yesterdays file\n";
    $yday -= 1;
  }

  # Construct test time and file test times.
  $test_time = $yday + $time;
  foreach $rec (@info) {
    next if $rec =~ /^##/;
    @tmp=split(" ", $rec);
    push( @filename, substr( $tmp[0], 0, 8 ) );
    push( @doy, $tmp[18] );
    $hour = substr( $tmp[6], 0, 2 )/24.; 
    push( @hour, $hour );
  }
  for ($i=0;$i<=$#doy;$i++){
    push( @test, $doy[$i] + $hour[$i]);
  }

  # Find index of Goes file closest to our time.
  $below=0;
  for ($i=0;$i<=$#test;$i++){
    $x = $test[$i];
    if ( $x <= $test_time ) {
      $below=1;
      if ($x == $test_time ) {
	$index=$i;
	last;
      }
    } else {
      if ($below == 1) {
	$index=$i-1;
	last;
      }
      $below=0;
    }
  }
  $matching_test_time = $test[$index];
  $matching_hour = $hour[$index];
  $matching_doy = $doy[$index];
  if ($matching_doy < 10) {
    $matching_doy = "00".$matching_doy;
  }
  if ($matching_doy >= 10 && 
      $matching_doy < 100  ) {
    $matching_doy = "0".$matching_doy;
  }

  # Construct Area Filename
  $area_file = $filename[$index];
  if ($area_file =~ /^A/) {
    ;
  } else {
    $area_file="AREA".$area_file
  }

  print "AREA file for this run = $area_file\n";
  print "Checking to see whether it's already here\n";

  $off=4;

  $tmp=substr( $area_file,$off,2 );
  $sat=substr( $area_file, $off, 1 );
  $sen=substr( $area_file, $off+1, 1);
  $filenum = substr( $area_file, $off+2, 2);

  # Construct path and file name on remote server.
  $remote_path = "goes".$sat_num{$sat}."/".$sensor_dir{$sen}."/";
  $remote_file = $remote_path.$area_file;

  if ( $remote_path =~ m#^g# ) {
    $tmp = "/".$remote_path ;
  } else {
    $tmp = $remote_path;
  }

  # Construct the local path and filename
  $local_path="/disk2/vap/goes".$tmp;
  $local_file=$local_path.$area_file;
  $test2 = $matching_test_time - 1.;
  if (-e $local_file) {
    # run ainf to get latest info about archive
    $cwd=$ENV{'PWD'};
    chdir $local_path || die "Couldn't cd to $local_path to run mkai\n";
    $exe_str = "/usr/people/vapuser/bin/mkai ";
    if ($user =~ /root/){
      ($name,$passwd,$uid,$gid,$quota,$comment,$gcos,$dir,$shell) = 
	  getpwnam( "vapuser") ;
      if ($uid && $gid ) {
	chown $uid,$gid, 'area_info' || 
	    print "Couldn't change owner,group of area_info\n";
      } else {
	print " Failure in getpwnam, Changing permissions to rwxrwxrwx, root owned\n";
	chmod 0777, $local_file;
      }
    }

    $r=system( $exe_str )/256;
    if ($r != 0) {
      print "Some kind of error in mkai\n";
    }
    chdir $cwd || die "Couldn't cd back to $cwd\n";
    # check to see if it's the from the same time
    $local_ainf = $local_path."/area_info";
    open ( INF, "<$local_ainf") || die "Couldn't open $local_ainf\n";
    @ainf=<INF>;
    close (INF);
    foreach $r (@ainf) {
      next if $r =~ /^##/;
      @tmp=split(" ", $r);
      next if substr($tmp[0],0,8) !~ /$area_file/;
      $doy = $tmp[18];
      $hour = substr( $tmp[6], 0, 2 )/24.; 
      $test=$doy + $hour;
      $test2 = $test-$matching_test_time;
      if ($test2 < 0.) {
	$test2 *= -1;
      }
      last;
    }
  }  


  #
  # -- construct the command line to rgoes 
  #


  $verbose=0;
  %sensor_num = ( 'vis', '0',
		  'ir2', '1',
		  'ir3', '2',
		  'ir4', '3',
		 );
  @sensor_name = ( 'vis','ir2','ir3','ir4');

  %satellite_num=('8','9', # AREA9* are GOES 8 files !!!
		  '9','8',     # AREA8* are GOES 9 files!!!
		  );
  $archive_root = $ENV{'VAP_ROOT'}."/goes"; # top of archive tree
  $scriptdir = "/disk2/vap/overlay/daily/";
  $sat=9;
  $sensor=ir4;
  $limits=" 25 65 -175 -115";
  
  $lim_str="%185,25,245,65%";
  
  $filename=$area_file;
  $path="/disk2/vap/goes/goes9/ir4/";
  $root=$archive_root;


  # Retrieve the AREA file.
  if ( $test2 >= 1/24. ) {
    print "Retrieving $area_file from the NOAA archive \n";
    $ftp = Net::FTP->new( $host ) || die "Can't open new \n";
    $ftp->login ($user, $pw ) || die "Can't login\n";
    $ftp->binary || die "Can't go to binary\n";
    $ftp->get($remote_file,$local_file) ||  die "Can't get file\n";
    $ftp->quit || die "Can't close\n";

    $user=$ENV{'USER'};
    if ($user =~ /root/){
      ($name,$passwd,$uid,$gid,$quota,$comment,$gcos,$dir,$shell) = 
	  getpwnam( "vapuser") ;
      if ($uid && $gid ) {
	chown $uid,$gid, $local_file || 
	    print "Couldn't change owner,group of $file\n";
      } else {
	print " Failure in getpwnam, Changing permissions to rwxr-xr-x, root owned\n";
	chmod 0755, $local_file;
      }
    }
  } else {

    print "$area_file already exists in our archive\n";
    print "Checking to see if a grid file exists also\n";
    # construct the name of the file that should exist in the 
    # overlay directory
    $hour=$matching_hour*24.;
    if ($hour < 10 ) {
      $hour = "0".$hour;
    }
    $hour .= "*";
    $last4 =substr( $area_file, 4,4);
    ($sec,$min,$junk,$mday,$mon, $year,$wday,$yday,$isdst) = localtime(time); 
    $grid_file = "GOES94_".$year.$matching_doy.$hour."-".$last4."-".$lim_str.".dat";
    $test_grid_file = "/disk2/vap/overlay/daily/".$grid_file;
    print " Testing for presence of a grid file matching file glob $test_grid_file\n";
    open( TEST, " ls $test_grid_file |");
    @in = <TEST>;
    close TEST;
    $test = $in[0] eq "";

    if ( $test != 1) {
      $tmp = $in[0];
      chop $tmp;
      @tmp = split( "/",$tmp );
      $grid_file = $tmp[$#tmp];
      print "$grid_file already exists, I'll return this file instead\n";
      # Not he prettiest way to code this, I'll think of something later.
      return $grid_file;
    }

  }

  # ----------------------------------------------------
  # Create script to run the 'goes' gridding program 
  # and execute that script.
  # ----------------------------------------------------

  chdir $scriptdir || die "Couldn't change directory to $scriptdir\n";


  $script = $scriptdir."rgoes";
  $goestype = '94';
  $time = $time*24;
  $script = $script."-".$goestype."-".$time."-".$lim_str.".scr";

  open (SCRIPT, ">$script");
  print SCRIPT "$path\n";
  print SCRIPT "$filename\n";
  print SCRIPT "$limits";
  close (SCRIPT);

  $log = $script.".log";
  $exe_string = "/usr/people/vapuser/bin/run_goes  $script  $log";
  $retval = system( $exe_string )/256;
  $gridfile = &pgl( $log );
  $skip_rename=0;
  chdir $start_dir  || die "Couldn't go back to initial dir $start_dir\n";
  $gridfile;


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
1;

