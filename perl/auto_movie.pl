#!/usr/bin/perl
#
# $Id$
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
# Revision 1.9  2001/08/06 18:34:18  vapuser
# Added "use lib '/usr/people/vapuser/perl';"
#
# Revision 1.8  2001/02/09 18:30:06  vapuser
# Added calls to vap_perl::VapMailErrorMsg.  Also changed the way
# auto_movie.pl gets the name of the output .mov file. auto_movie.pro
# now writes it out to tmpfiledir/auto_movie_mov_filename and
# auto_movie.pl reads that name in, just like cloud_overlay {perl/idl}
# duo.
#
#
# Revision 1.7  2000/02/09 18:22:43  vapuser
# Took out '/nodate' switch in call to auto_movie.pro.
#
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
#
#
require 5.000;
use lib $ENV{VAP_SFTWR_PERL};
#use vap_perl;
use VapUtil;
use Cwd 'chdir';
use File::Copy ;

$|=0; #auto flush the buffers

print "=================================================================================== \n";
my $localtime=localtime($^T);
my $gmt=gmtime($^T);

print "Starting auto_movie.pl\nLocal Time: $localtime,\nGMT       : $gmt\n";

my $roi = shift or die "Usage: auto_movie.pl ROI\n";

my ($sec,$min,$hour,$mday,$mon,
    $year,$wday,$yday,@isdst)=gmtime($^T);
$year += 1900;
$mon+= 1;

my $date_time_string=sprintf("%04d%02d%02d%02d%02d",
			  $year,$mon,$mday,$hour,$min);
my $NowAsVapTime=sprintf("%04d/%02d/%02d/%02d/%02d",
		      $year,$mon,$mday,$hour,$min);
my @ROIS = auto_movie_defs();
#
#

  # Uppercase stuff and keep the filename lowercase.

$roi =~ tr/a-z/A-Z/; # upcase everything
$froi = $roi;
$froi =~ tr/A-Z/a-z/; # filenames are lowercase.

do {
  VapMailErrorMsg(
		  "ILLEGAL ROI: $roi, legal values are @ROIS\n",
		  "ILLEGAL ROI");
  die "ILLEGAL ROI: $roi, legal values are @ROIS\n";
} unless (@test_roi = grep( $_ =~ /$roi/, @ROIS));


my $date_time=shift || $NowAsVaptime;

  
my $user=$ENV{'USER'};
  # create idl lock file
my $tmpfiledir=$ENV{VAP_OPS_TMPFILES}."/tmpfiles";
my $lock_file="$tmpfiledir/$user.auto_movie_$froi.lock";
print "lockfile = $lock_file\n";
open(LOCK,">$lock_file") || 
do { VapMailErrorMsg(
   "$0: Couldn't open $lock_file\n","LOCK FILE OPEN ERROR");
  die "Couldn't open $lock_file\n"};
print LOCK "$$\n" ;
close(LOCK);

  # create temporary IDL pro file

my $idl_tmp_file = "$tmpfiledir/tmp_auto_movie_$froi.pro";
print "Writing IDL command to $idl_tmp_file\n";
open( IDL_TMP_FILE,">$idl_tmp_file") || 
    do {VapMailErrorMsg("auto_movie: Couldn't open $idl_tmp_file\n",
			       "IDL TMPFILE OPEN ERROR");
	 die "Couldn't open $idl_tmp_file\n"};
  # write code to tmp file
my $exe_str = "auto_movie,\'$date_time\',roi=\'$roi\'";

# Add the PID
$exe_str .= ",pid=$$\n";
$exe_str .= "exit\n";
print IDL_TMP_FILE $exe_str;
print "Calling IDL with string $exe_str\n";

close( IDL_TMP_FILE );

  # Call IDL with tmp file as argument
$exe_str=$IDLEXE." ".$idl_tmp_file;
  # check to see if the IDL run was okay.
my $r = system( $exe_str );
if ($r != 0) {
  VapMailErrorMsg( "$0:\n Error in IDL\n","ERROR CALLING IDL" );
  die " Error in IDL\n";
} 


  # rename tmp file, just to be safe.
$newname = "$idl_tmp_file.$date_time_string";
rename( $idl_tmp_file, $newname) || 
    print "Couldn't rename $idl_tmp_file to $newname\n";

open ( LOCK, "<$lock_file") || 
    {vap_perl::VapMailErrorMsg("$0:\n Couldn't open $lock_file after IDL \n",
			       "LOCK FILE OPEN ERROR") &&
				   die "Couldn't open $lock_file after IDL \n"};
@idlout = <LOCK>;
close (LOCK);

rename ($lock_file, "$lock_file.$date_time_string");

# see if there are any lines in the idl output file with the word ERROR in them.
# exit if there are.
my @grepout=();
@grepout=grep (/ERROR/, @idlout );
if ($#grepout > -1) {
  my $msg="$0:\n Errors in IDL processing\n";
  $msg.= join "\n", @idlout;
  print @idlout;
  do {VapMailErrorMsg($msg,"ERRORS IN IDL PROC") &&
       die "Errors in IDL run\n"};
}
my $start_dir=Cwd::cwd(); # get current directory;

  # retrieve the name of the .mov file.
my $auto_mov_filename="$tmpfiledir/auto_movie_mov_filename_$froi";
open MOVNAME, "<$auto_mov_filename" || 
do {VapMailErrorMsg("$0:\n Couldn't open $auto_mov_filename\n",
		    "MOV FILE OPEN ERROR") &&
		      die "Couldn't open $auto_mov_filename \n"};

my @name=<MOVNAME>;
close MOVNAME;
unlink $auto_mov_filename;
my $oldfile=$name[0];
chomp $oldfile;

  # Construct the path to the new location in the WWW tree
$newpath = "$VAP_WWW_TOP/images/mov_archive";
$oldpath = "$VAP_ANIM/$froi/daily";
$newfile = "$newpath/$oldfile";

  # chmod on the movie and gwind.001 file to a+r
chdir $oldpath || 
    do {vap_perl::VapMailErrorMsg("$0:\n Can't chdir to $oldpath\n",
				  "CD ERROR $oldpath");
      die "Can't chdir to $oldpath\n"};

  # Make it rw-r--r--
chmod 0644, $oldfile || 
    do {vap_perl::VapMailErrorMsg("$0:\n Can't chmod of $oldfile\n",
				  "CHMOD ERROR $oldfile");
      die "Can't chmod of $oldfile\n"};

  # copy it to the WWW tree.
print "Renaming $oldfile to $newfile\n";
copy( $oldfile, $newfile ) || 
    do {vap_perl::VapMailErrorMsg("$0:\n Can't rename $oldfile to $newfile, $!\n",
				  "RENAME ERROR <MOV>");
      die "Can't rename $oldfile to $newfile, $!\n"};

  # Delete the source .mov file
unlink($oldfile) || 
    do {vap_perl::VapMailErrorMsg("$0:\n Can't delete $oldfile1\n",
				  "RM ERROR $oldfile" );
      die "Can't delete $oldfile\n"};

  # Now chmod the gwind file
chmod 0644, "gwind.001" || 
    do {vap_perl::VapMailErrorMsg("$0:\n Can't chmod of gwind.001\n",
				  "CHMOD ERROR gwind.001" );
      die "Can't chmod of gwind.001\n"};

  # And CP it to the WWW tree
$new_gwind_file = "$newpath/gwind_$froi.001.gif";

print "Renaming gwind.001 to $new_gwind_file.\n";
copy( "./gwind.001", $new_gwind_file ) ||
    do {vap_perl::VapMailErrorMsg("$0:\n Can't rename gwind.001 to $new_gwind_file, $!\n",
				  "RENAME ERROR <gwind.001>");
      die "Can't rename gwind.001 to $new_gwind_file, $!\n"};

  # create a symlink from the images subdir to this new movie 
$link_file = "$vap_perl::VAP_WWW_TOP/images/daily_$froi.mov";
print "Linking $link_file to $newfile\n";

  #delete link_file if it exists
if (-e $link_file || -l $link_file) {
  print "Deleting $link_file\n";
  unlink ($link_file ) || 
      do {vap_perl::VapMailErrorMsg("$0:\n Couldn't unlink $link_file\n",
				    "ERROR DEL LINK_FILE <.MOV>");
	die "Couldn't unlink $link_file\n"};
}

  # do the symlink
symlink(  $newfile, $link_file ) || 
    do {vap_perl::VapMailErrorMsg("Can't link $link_file to $newfile, $!\n",
				  "ERROR LINKING FILES <.MOV>");
      die "Can't link $link_file to $newfile, $!\n"};  

  # Now do the same for the gwind.001 file.
 $link_file = "$vap_perl::VAP_WWW_TOP/images/gwind_$froi.001.gif";
 if (-e $link_file || -l $link_file) {
  print "Deleting $link_file\n";
  unlink ($link_file ) || 
      do {vap_perl::VapMailErrorMsg("$0:\n Couldn't unlink $link_file\n",
				    "ERROR DEL LINK_FILE <.gif>");
	die "Couldn't unlink $link_file\n"};
}
 symlink( $new_gwind_file , $link_file ) || 
   die "Can't link $link_file to $new_gwind_file, $!\n";  

  # go to the htdocs dir 
chdir $vap_perl::VAP_WWW_TOP || 
    do {vap_perl::VapMailErrorMsg("$0:\n Can't chdir to $vap_perl::VAP_WWW_TOP\n",
				  "ERROR CD TO VAP_WWW_TOP");
      die "Can't chdir to $vap_perl::VAP_WWW_TOP\n"};

  # update index.html
print "Updating index.html\n";
vap_perl::date_index();

  # chmod 0644 index.html
chmod 0644, "index.html" || 
    do {vap_perl::VapMailErrorMsg("$0:\n Can't chmod 0644 index.html\n",
				  "ERROR CHMOD 0644 INDEX.HTML");
      die "Can't chmod 0644 index.html\n"};

  # Go back to starting dir
chdir $start_dir || 
    {vap_perl::VapMailErrorMsg("$0:\n Can't chdir $start_dir\n", 
			       "CD ERROR TO START DIR") &&
	  die "Can't chdir $start_dir\n"};
$T=time;
$el=$T-$^T;
print "Elapsed time: $el seconds\n";
print "Done\n";
print "=================================================\n";

exit 0;


