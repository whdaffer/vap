#!/usr/bin/perl5 
#
# Purpose: Creates animation based on latest 26 hours of wind data for
# input region of interest (ROI)'
#
# Usage: auto_movie roi time
#
#   where roi equals one of 'nepac','nwpac','npac', 'nwatl' or 'indian'
#   and time is of the form 'yy/mm/dd/hh'
# 
# $Log$
#
# 
#
#
require 5.000;

use Cwd 'chdir';

$rcsid="$Id$";

$newdir = "/usr/people/vapuser/perl";
push ( @INC, $newdir );
require vap_perl;
$ENV{'PATH'}='/usr/sbin:/usr/bsd:/sbin:/usr/bin:/bin:/usr/bin/X11:/usr/etc/:/usr/freeware/bin:/usr/people/vapuser/bin:/usr/people/daffer:/usr/people/daffer/bin:/usr/people/vapuser/bin:/usr/people/vapuser/perl:';

@ROIS = vap_perl::auto_movie_defs();
#
#

if ($#ARGV >= 0) {
  $roi=shift;
} else {
  $roi='NEPAC';
}
$roi =~ tr/a-z/A-Z/; # upcase everything

$froi = $roi;
$froi =~ tr/A-Z/a-z/; # for filename stuff

@test_roi = grep( $_ =~ /$roi/, @ROIS);

$test_roi=$test_roi[0];
if ($#ARGV == 0) {
  $date_time=shift;
}

if ( $test_roi) {

  
  # kill any IDL sessions currently running
  $pstr = "ps -ef | grep bin.sgi/idl | grep -v grep | grep -v lmgrd";

  open(PS,"$pstr |") || die "Couldn't get idl sessions\n";
  @p = <PS>;
  close(PS);
  if ($#p>-1) {
    print "Processes running IDL are @p\n";
    foreach $r (@p) {
      @tmp=split(" ", $r); 
      push @pids, $tmp[1];
    }
    $cnt = kill -9, @pids;
    if ($cnt == 0) {
      die "Couldn't kill IDL sessions";
    }
  }

  $user=$ENV{'USER'};
    # create idl lock file
  $lock_file="/tmp/".$user.".auto_movie.lock";
  print "lockfile = $lock_file\n";
  open(LOCK,">$lock_file") || die "Couldn't open $lock_file\n";
  close(LOCK);
    # create temporary IDL pro file
  $tmp_pro = "/tmp/tmp_auto_movie.pro";
  open( TMP_PRO,">$tmp_pro") || die "Couldn't open $tmp_pro\n";
    # write code to tmp file
  if ($date_time) {
    $exe_str = "auto_movie,\'$date_time\',roi=\'$roi\'\n";
  } else {
    $exe_str = "auto_movie,roi=\'$roi\'\n";
  }
  print TMP_PRO $exe_str;
  print TMP_PRO "exit\n";
  close( TMP_PRO );
  
    # Call IDL with tmp file as argument
  @args = ("/usr/local/rsi/idl_4/bin/idl", "/tmp/tmp_auto_movie.pro");

    # check to see if the IDL run was okay.
  $r = system( @args );
  $r /= 256;
  if ($r != 0) {
      die "Error in IDL\n";
  }

    # rename tmp file, just to be safe.
  $newname = $tmp_pro.".old";
  rename( $tmp_pro, $newname) || print "Couldn't rename $tmp_pro\n";

  open ( LOCK, "<$lock_file") || die "Couldn't open $lock_file after IDL \n";
  @idlout = <LOCK>;
  close (LOCK);

  unlink($lock_file) || print "Couldn't delete $lock_file \n";

  # see if there are any lines in the idl output file with the work ERROR in them.
  # exit if there are.
  local(@grepout)=();
  @grepout=grep (/ERROR/, @idlout );
  if ($#grepout > -1) {
    print @idlout;
    die "Errors in IDL run\n";
  }
  $start_dir=Cwd::cwd(); # get current directory;
  
  if ($date_time) {
    @tmp = split( '/',$date_time);
    $year=$tmp[0];
    $mon=$tmp[1];
    $mday=$tmp[2];
    $hour=$tmp[3];
  } else {
    ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = 
	localtime(time);
    $mon += 1;
    if ($mon < 10) {
      $mon = "0".$mon;
    }
    if ($mday < 10) {
      $mday = "0".$mday;
    }
    if ($hour < 10) {
      $hour = "0".$hour;
    }
  }
  $date_string = $year.$mon.$mday.$hour;

    # construct the old file name
  $oldfile="./daily_".$froi.".mov";
    # Construct the new file name
  $newfile="daily_".$froi."_".$date_string.".mov";
    # Construct the path to the new location
  $root_path = $ENV{'VAP_ROOT'};
  $newpath = $root_path.'/www/htdocs/images/mov_archive/'.$froi."/daily/";
  $oldpath = $root_path.'/animate/'.$froi."/daily/";
  $newfile = $newpath.$newfile;

    # chmod on the movie and gwind.001 file to a+r
  chdir $oldpath || die "Can't chdir to $newpath\n";
  chmod 0755, $oldfile || die "Can't chmod of $oldfile\n";
  print "Renaming $oldfile to $newfile\n";
  rename( $oldfile, $newfile ) || 
      die "Can't rename $oldfile to $newfile, $!\n";

    # Now do the same for the gwind file
  chmod 0755, "gwind.001" || die "Can't chmod of gwind.001\n";
    # rename both files to new dir
  $new_gwind_file = $newpath."gwind.001.gif";
  print "Renaming gwind.001 to $new_gwind_file.\n";
  rename( "./gwind.001", $new_gwind_file ) || 
      die "Can't rename gwind.001 to $new_gwind_file, $!\n";
  
    # create a symlink from the images subdir to this new movie and it's thumbnail.
  $link_file = $root_path.'/www/htdocs/images/daily_'.$froi.".mov";
  print "Linking $link_file to $newfile\n";
    #delete link_file if it exists
  if (-e $link_file) {
    unlink ($link_file ) || die "Couldn't unlink $link_file\n";
  }
  symlink(  $newfile, $link_file ) || die "Can't link $link_file to $newfile, $!\n";  

    # Now do the thumbnail.
   $link_file = $root_path.'/www/htdocs/images/gwind_'.$froi.".001.gif";
   if (-e $link_file) {
     unlink ($link_file ) || die  "Couldn't unlink $link_file, $!\n";
   }
   symlink( $new_gwind_file , $link_file ) || 
     die "Can't link $link_file to $new_gwind_file, $!\n";  
  
    # go to the htdocs dir 
  chdir $root_path.'/www/htdocs' || die "Can't chdir to www/htdocs\n";
  
    # update index.html
  print "Updating index.html\n";
  vap_perl::date_index();

    # chmod a+r index.html
  chmod 0755, "index.html" || die "Can't chmod a+r index.html\n";
  
    # Go back to starting dir
  chdir $start_dir || die "Can't chdir $start_dir\n";
  print "Done\n";

} else {
  die "ILLEGAL ROI: $roi, legal values are @ROIS\n";
}


