#!/usr/bin/perl5  
# 
# Time-stamp: <98/09/03 11:44:09 vapuser>
# $Id$
# auto_goes_overlay.pl - perl script to automatically create goes overlay plots
#


require vap_perl;
use Cwd 'chdir';


# turn off buffering so that the system flushes 
# the output buffer on every write
$!=1; 

# cron gives a very impoverished environment, set up what we need.
$user=$ENV{'USER'};
$ENV{'VAP_ROOT'}='/disk2/vap/';
$ENV{'VAP_OVERLAY'} = '/disk2/vap/overlay/';
$ENV{'IDL_PATH'} = "+/usr/people/vapuser/idl:+/usr/people/daffer/idl:+/disk2/vap:+/usr/local/rsi/idl_4/lib:+/usr/local/rsi/idl_4/examples";
$ENV{'VAP_WINDS'}='/disk3/winds/';

print "2\n";

print "USER=$user\n";
print "VAP_ROOT=$ENV{'VAP_ROOT'}\n";
print "VAP_OVERLAY=$ENV{'VAP_OVERLAY'}\n";
print "IDL_PATH=$ENV{'IDL_PATH'}\n";
print "VAP_WINDS=$ENV{'VAP_WINDS'}\n";

$umask = umask;
print "Current umask = $umask\n";
print "Setting umask to 023 \n";

umask( 023 ) || die "Couldn't reset umask to 023\n";
$umask = umask;
print "New umask = $umask\n";
$tmp=oct(023);
print "Should equal  $tmp\n";

$lock_file="/tmp/".$user.".pac_goes_overlay.lock";
open (LOCK,">$lock_file") || die "Couldn't open $lock_file \n";
close (LOCK);

print "3\n";

# Determine the type of overlay to make, ascending or descending.
# def = ascending
$type=shift;
if ($type eq "") {
  $type="a";
}

# determine the type of wind data, nudged (=def) or non-nudged
$nudged = 1;
$nn=shift;
if ($nn ne "" ) {
  $nudged=0;
}

# Open and process info in the VAPRC file
# open (VAPRC, "</disk2/vap/.vaprc");
# @env=<VAPRC>;
# close VAPRC;

# $r=system("source /disk2/vap/.vaprc");
# foreach $r (@env){
#   if ($r =~ /IDL/) {
#     # have to treat IDL_PATH a bit differently
#     @tmp0=split( " ",$r );
    
#     @tmp = split(/:/, $tmp0[2]);
#     foreach $rr (@tmp){
#       if ($rr =~ /\$/ ){
# 	$tt=$ENV{ substr( $rr, 1, length( $rr)-1 ) }
#       } else {
# 	$tt = $rr;
#       }
#       push @idl_path, $tt;
#     }
    
#     $idl_path = join(":", @idl_path );
#     $ENV{'IDL_PATH'} = $idl_path;

#   } else {
#     @tmp=split(" ", $r);
#     if ($tmp[2] =~ /\$/) {
#       @tmp2=split("/", $tmp[2]);
#       $tmp2[0] = $ENV{ substr( $tmp2[0], 1, length($tmp2[0])-1 ) };
#       $tmp[2] = join( "/", @tmp2 );
#     }
#   }
#   $ENV{$tmp[1]} = $tmp[2];
# }

print "4\n";
$overlay_dir=$ENV{'VAP_OVERLAY'}."/daily/";

# chdir to the overlay dir
chdir $overlay_dir  || die "Couldn't go to overlay dir $overlay_dir\n";

# kill any IDL sessions currently running
$pstr = "ps -ef | grep bin.sgi/idl | grep -v grep | grep -v lmgrd";

open(PS,"$pstr |") || die "Couldn't get idl sessions\n";
@p = <PS>;
close(PS);
if ($#p>-1) {
  foreach $r (@p) {
    @tmp=split(" ", $r); 
    push @pids, $tmp[1];
  }
  $cnt = kill -9,@pids;
  if ($cnt == 0) {
    print "Couldn't kill IDL sessions";
  }
}

print "5\n";
# Get and grid goes file.
$goes_file = vap_perl'gag( $type );

print "6\n";
# get the time of the grid file from the grid file name

@tmp = split( "/", $goes_file );
$goes_file = $tmp[$#tmp];


open(IDLFILE,">auto_pac_goes_overlay.pro") || 
  die "Couldn't open auto_pac_goes_overlay\n";

($sec,$min,$hour,$mday,$mon, $year,$wday,$yday,$isdst) = localtime(time); 
$mon += 1;

$hour = "20";
if ($type eq 'a') {
  $hour="07";
}

# Since GAG might've determined that it's too early to do today's file
# and that we should look for yesterdays data, we will get the day of
# the month and the month from the name of the goes file, rather than
# from the local time

#          1         2         3         4
#01234567890123456789012345678901234567890
#GOES94_970140700-8403-%185,25,245,65%.dat
#
($satnum, $sensor, $year, $month, $dom, $hh, $limit) = 
    vap_perl'parsegoesfilename($goes_file);
print "7\n";
$date_time=$year."/".$month."/".$dom."/".$hh;
$exe_str = "pac_goes_overlay,\'$goes_file\',\'$date_time\'";
if ($nudged != 1) {
  $exe_str .= ",wpath = \'/disk3/winds2/\'";
}
print IDLFILE "$exe_str\n";
print IDLFILE "exit\n";
close IDLFILE;

$r=system( "/usr/local/rsi/idl_5.1/bin/idl auto_pac_goes_overlay.pro")/256;
if ($r != 0) {
  die " Error in IDL\n";
}
unlink "auto_pac_goes_overlay" || die "Couldn't unlink auto_pac_goes_overlay\n";

open ( LOCK, "<$lock_file") || die "Couldn't open $lock_file after IDL \n";
@idlout = <LOCK>;
close (LOCK);

unlink($lock_file) || print "Couldn't delete $lock_file \n";

# see if there are any lines in the idl output file with the work ERROR in them.
# exit if there are.
foreach $r (@idlout) {
  if ($r =~ /ERROR/) {
    print "Error in IDL processing \n";
    print @idlout;
    exit;
  }
}
print "8\n";
# goes_overlay gives the output gif file a name based on the input grid file name
# get that name and rename the file to the standard 'pac_overlay_$type.gif'
open (GIFNAME,"</tmp/auto_pac_goes_overlay_gif_file") || 
    die "Can't open /tmp/auto_pac_output_file\n";
@giffile=<GIFNAME>;
close(GIFNAME);
unlink "/tmp/auto_pac_goes_overlay_gif_file" || 
    die "Couldn't delete /tmp/auto_pac_goes_overlay_gif_file\n";

print "9\n";
$gif=$giffile[0];
chop($gif);
@tmp=split("/",$gif);
$gif = $tmp[$#tmp];
@tmp = split( /\./, $gif );
$root = $tmp[0];

$file="/disk2/vap/www/htdocs/images/po_archive/".$root."_".$type;
if ($nudged==0) {
  $file .= "_nn";
}
$file .= ".gif";

print "Renaming $gif to $file\n";
rename( $gif,$file ) ||
    die "Couldn't rename $gif to $file, error code = $!, error string = $@\n";
chmod 0755, $file || print  "Couldn't chmod $file to a+r\n";

print "10\n";
# create name of file in images archive to be linked to it.
$wwwfile="/disk2/vap/www/htdocs/images/pac_overlay_".$type;
if ($nudged != 1) {
  $wwwfile .= "_nn";
}
$wwwfile .= ".gif";

#delete it if it exists
if (-e $wwwfile) {
  unlink ($wwwfile ) || print "Couldn't unlink $wwwfile\n";
}
# create the symbolic link to the gif file.
print "Linking $wwwfile to $file\n";
symlink( $file, $wwwfile ) || print "Couldn't link $wwwfile to $file \n";

# go to where the index.html is
chdir "/disk2/vap/www/htdocs" || die "Couldn't go to htdocs\n";
print "11\n";
# update the dates in that file
print "Updating index.html\n";
vap_perl'date_index();

# make sure it's readible.
chmod 0755, "index.html" ||  die "Couldn't chmod index.html to a+r\n";








