#!/usr/bin/perl5 
#
# Purpose: Creates animation based on latest 26 hours of wind data for
# input region of interest (ROI)'
#
# Usage: auto_movie [roi [time]]
#
#   where roi equals one of 'nepac','nwpac','npac', 'nwatl' 
#   or 'indian' and time is of the form 'yy/mm/dd/hh/mm' 
#   (i.e. what has become known as 'vaptime')
# 
# $Log$
# Revision 1.6  1999/02/27 00:44:43  vapuser
# Added code to handle the destination WWW dir
# being on a different (possibly NFS mounted) directory.
#
# Revision 1.5  1998/11/25 22:38:27  vapuser
# Changed pertinent 'renames' to 'copy's
#
# Revision 1.4  1998/10/22 21:16:20  vapuser
# Made it ready to run in Qscat land
#
# Revision 1.3  1998/10/17 00:23:15  vapuser
# Changed 'require vap_perl' to 'use vap_perl'
#
# Revision 1.2  1998/10/02 22:59:56  vapuser
# Added a require vap_perl
#
# Revision 1.1  1998/10/02 22:59:18  vapuser
# Initial revision
#
#
# 
#
# $Id$
#
require 5.000;
use vap_perl;
use Cwd 'chdir';
use File::Copy ;
require vap_perl;

($sec,$min,$hour,$mday,$mon,
    $year,$wday,$yday,@isdst)=localtime($^T);
$year += 1900;
$mon+= 1;

$mon  = ($mon  <10) ?  "0$mon"  : "$mon";
$mday = ($mday <10) ?  "0$mday" : "$mday";
$hour = ($hour <10) ?  "0$hour" : "$hour";
$min  = ($min  <10) ?  "0$min"  : "$min";

$date_time_string="$year$mon$mday$hour$min";
$NowAsVapTime="$year/$mon/$mday/$hour/$min";
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
$froi =~ tr/A-Z/a-z/; # filenames are lowercase.

@test_roi = grep( $_ =~ /$roi/, @ROIS);

if ( $test_roi[0]) {

  $test_roi=$test_roi[0];
  if ($#ARGV == 0) {
    $date_time=shift;
  }

  
  $user=$ENV{'USER'};
    # create idl lock file
  $lock_file="/tmp/".$user.".auto_movie.lock";
  print "lockfile = $lock_file\n";
  open(LOCK,">$lock_file") || die "Couldn't open $lock_file\n";
  print LOCK "$$\n" ;
  close(LOCK);

    # create temporary IDL pro file

  $idl_tmp_file = "/tmp/tmp_auto_movie.pro";
  print "Writing IDL command to $idl_tmp_file\n";
  open( IDL_TMP_FILE,">$idl_tmp_file") || die "Couldn't open $idl_tmp_file\n";
    # write code to tmp file
  if ($date_time) {
    $exe_str = "auto_movie,\'$date_time\',roi=\'$roi\'";
  } else {
    $exe_str = "auto_movie,\'$NowAsVapTime\',roi=\'$roi\'";
  }

  # Add the PID
  $exe_str .= ",pid=$$\n";
  $exe_str .= "exit\n";
  print IDL_TMP_FILE $exe_str;
  print "Calling IDL with string $exe_str\n";

  close( IDL_TMP_FILE );
  
    # Call IDL with tmp file as argument
  $exe_str=$vap_perl::IDLEXE." ".$idl_tmp_file;
    # check to see if the IDL run was okay.
  $r = system( $exe_str );
 if ($r != 0) {
    die " Error in IDL\n";
  }

    # rename tmp file, just to be safe.
  $newname = "$idl_tmp_file.$date_time_string";
  rename( $idl_tmp_file, $newname) || print "Couldn't rename $idl_tmp_file to $newname\n";

  open ( LOCK, "<$lock_file") || die "Couldn't open $lock_file after IDL \n";
  @idlout = <LOCK>;
  close (LOCK);

  rename ($lock_file, "$lock_file.$date_time_string");

  # see if there are any lines in the idl output file with the word ERROR in them.
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
    $date_string = "$year$mon$mday$hour$min";

  } else {
    $date_string = $date_time_string;
  }

    # construct the old file name
  $oldfile="./daily_".$froi.".mov";

    # Construct the new file name
  $newfile="daily_".$froi."_".$date_string.".mov";

    # Construct the path to the new location in the WWW tree
  $newpath = "$vap_perl::VAP_WWW_TOP/images/mov_archive";
  $oldpath = "$VAP_ANIM/$froi/daily";
  $newfile = "$newpath/$newfile";

    # chmod on the movie and gwind.001 file to a+r
  chdir $oldpath || die "Can't chdir to $newpath\n";

    # Make it rw-r--r--
  chmod 0644, $oldfile || die "Can't chmod of $oldfile\n";
  print "Renaming $oldfile to $newfile\n";
  # rename( $oldfile, $newfile ) || 
  copy( $oldfile, $newfile ) || 
      die "Can't rename $oldfile to $newfile, $!\n";

    # Now do the same for the gwind file
  chmod 0644, "gwind.001" || die "Can't chmod of gwind.001\n";

    # rename both files to new dir
  $new_gwind_file = "$newpath/gwind_$froi.001.gif";

  print "Renaming gwind.001 to $new_gwind_file.\n";
   # rename( "./gwind.001", $new_gwind_file ) || 
  copy( "./gwind.001", $new_gwind_file ) || 
      die "Can't rename gwind.001 to $new_gwind_file, $!\n";
  
    # create a symlink from the images subdir to this new movie and it's thumbnail.
  $link_file = "$vap_perl::VAP_WWW_TOP/images/daily_$froi.mov";
  print "Linking $link_file to $newfile\n";
    #delete link_file if it exists
  if (-e $link_file) {
    print "Deleting $link_file\n";
    unlink ($link_file ) || die "Couldn't unlink $link_file\n";
  }
  if (-l $link_file) {
    print "Deleting $link_file\n";
    unlink ($link_file ) || die "Couldn't unlink $link_file\n";
  }
  symlink(  $newfile, $link_file ) || die "Can't link $link_file to $newfile, $!\n";  

    # Now do the thumbnail.
   $link_file = "$vap_perl::VAP_WWW_TOP/images/gwind_$froi.001.gif";
   if (-e $link_file) {
     print "Deleting $link_file\n";
     unlink ($link_file ) || die  "Couldn't unlink $link_file, $!\n";
   }
   if (-l $link_file) {
     print "Deleting $link_file\n";
     unlink ($link_file ) || die  "Couldn't unlink $link_file, $!\n";
   }
   symlink( $new_gwind_file , $link_file ) || 
     die "Can't link $link_file to $new_gwind_file, $!\n";  
  
    # go to the htdocs dir 
  chdir $vap_perl::VAP_WWW_TOP || die "Can't chdir to $vap_perl::VAP_WWW_TOP\n";
  
    # update index.html
  print "Updating index.html\n";
  vap_perl::date_index();

    # chmod a+r index.html
  chmod 0644, "index.html" || die "Can't chmod a+r index.html\n";
  
    # Go back to starting dir
  chdir $start_dir || die "Can't chdir $start_dir\n";
  print "Done\n";

} else {
  die "ILLEGAL ROI: $roi, legal values are @ROIS\n";
}


