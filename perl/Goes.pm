#!/usr/bin/perl
#
# $Id$
#
#
# This module handles the Goes files at the Noaa archive.
#
#
# Modification Log:
#
# $Log$
# Revision 1.1  2001/02/09 18:46:34  vapuser
# Initial revision
#
# 
#
package Goes;
@ISA = qw(Exporter);
@EXPORT=qw(getAreaFile gag );

use lib getenv('VAP_SFTWR_TOP')."/vap/perl";
use Cwd 'chdir', 'getcwd';
use Time::Local;
use Net::FTP;
use Carp;
use VapUtil;
use Vapdefs;


BEGIN {

  %satnum2areanum = (10 => 8,
		     8 => 9);

  %areanum2satnum = (8 => 10, 
		     9 => 8 );

  %satloc2satnum = (WEST=>10,
		    EAST=>8);
  
  %sensornum2dir = ('1','vis',
		    '2','ir2',
		    '3','ir3',
		    '4','ir4',
		    '5','ir5'
		    );

  # AREA files more than 45 minutes from input time 
  # are ignored.

  $_min_time_diff = 45*60;

}


sub gag{
  
  local( $time, $absflag, $minlon, $minlat, $maxlon, $maxlat,
	$gridpath, $startdir, $griddedfile);

  $usage="Usage: gag satellite, sensornum, time, absflag, minlon, minlat, maxlon, maxlat";
  die "$usage\n" if $#_ < 1;
  $time=$_[2] || systime2idltime(time());
  $absflag= $_[3] || 1;
  $minlon = $_[4] || 0;
  $minlat = $_[5] || 0;
  $maxlon = $_[6] || 0;
  $maxlat = $_[7] || 0;
  local($areafile)=getAreaFile($_[0], $_[1], $time, $absflag );
  $gridpath = constructGriddingDir($_[0], $_[1]);
  $startdir=getcwd();
  chdir $gridpath || croak "Can't CD to $gridpath\n";
  $griddedfile = grid( $areafile, $minlon, $minlat, $maxlon, $maxlat );
  chdir $startdir || carp "Can't CD to $startdir\n";
  $griddedfile = "$gridpath/$griddedfile";
}

sub grid{ 

  local( $areafile, $minlon, $minlat, $maxlon, $maxlat );
  $areafile = shift @_ ||
      die "Usage: grid areafile [ minlon [ minlat [ maxlon [ maxlat ]]]]\n";

    ## The areafile input to this routine should be the FULLY
    ## QUALIFIED name. However, the C routine grid_goes wants the
    ## directory and file separately, so we have to split it up into
    ## the constituent parts.

  ($dir, $file) = $areafile =~ /(.*)\/(AREA\d+)/;

  $minlon = shift @_ || 0 ;
  $minlat = shift @_ || 0 ;
  $maxlon = shift @_ || 0 ;
  $maxlat = shift @_ || 0 ;

  ($minlon, $maxlon) = rectifyLon( $minlon, $maxlon);

#  if ($minlon != 0 || $minlat != 0 || $maxlon != 0 || $maxlat != 0) {
#    $minlon -= 360 if ($minlon >= 180);
#    $maxlon -= 360 if ($maxlon >= 180);
#  }

  $gridfile=AlreadyGridded($areafile, $minlon, $minlat, $maxlon, $maxlat );
  return $gridfile if $gridfile;


  if ($dir) {
    $exe_string=sprintf( "grid_goes -d %s -f %s -l %04d,%03d,%04d,%03d", 
			$dir, $file, $minlon, $minlat, $maxlon, $maxlat );
  } else {
    $exe_string=sprintf( "grid_goes -f %s -l %04d,%03d,%04d,%03d", 
			$file, $minlon, $minlat, $maxlon, $maxlat );
  }

  print "  Preparing to grid area file $areafile\n";


  print "About to open gridding processes with exe string\n";
  print "$exe_string\n";
  open ( GRIDDING_PROCESS, "$exe_string |" );
  @gridding_output = <GRIDDING_PROCESS>;
  print join "\n", @gridding_output;
  @errors=grep(/^ *ERROR.*/, @gridding_output);
  close GRIDDING_PROCESS;
  croak "  Bad return from goes gridding software\n" if ($#errors gt -1);
  print "  Done Gridding!\n";
  $local_gridded_file="$gridding_output[$#gridding_output]";
  chop $local_gridded_file;
  $local_gridded_file;
}


sub getAreaFile{
  local($satellite,$sensor,$vaptime,$localfile,$localdir,
	$file,$diff,$mindiff,$time,$test_time);
  $satellite = getSatnum($_[0]);
  $sensor    = $_[1] || croak "(param 2) Need sensor number [1,2,3,4,5] 1=vis, N=irN\n";
  $vaptime   = $_[2] || croak "(param 3) Need vaptime (yyyy/mm/dd/hh/mm (in UT))\n";
  $absflag   = $_[3] || 1;

  ($year,$month,$day,$hour,$min,$sec) = split( "/", $vaptime );
  $test_time = timegm( 0, $min, $hour, $day, $month-1, $year-1900 );

  ($file, $time,$mindiff,$remoteflag) = 
      NearestAreaFile($satellite, $sensor, $test_time, $absflag);

  if ($mindiff < 45*60 ) {
    if ($remoteflag) {
      $localfile=fetchAREAFile($file);
    } else {
      $localdir=constructLocalDir($satellite, $sensor);
      $localfile="$localdir/$file";
    }
  } else {
    $hh=$mindiff/3600;
    croak "Nearest AREA file ($file) is $hh hours distant! -- Aborting\n";
  }
  $localfile;
}

sub constructLocalDir{
  $satellite = getSatnum($_[0]);
  $sensor    = $_[1] || croak "(param 2) Need sensor number [1,2,3,4,5] 1=vis, N=irN\n";
  
  $sensordir=$sensornum2dir{$sensor};
  local($dir) = "$ARCHIVETOP/goes$satnum/$sensordir";
  $dir;
}

sub constructRemoteDir{
  $satellite = getSatnum($_[0]);
  $sensor    = $_[1] || croak "(param 2) Need sensor number [1,2,3,4,5] 1=vis, N=irN\n";
  $sensordir=$sensornum2dir{$sensor};
  local($dir) = "goes$satnum/$sensordir";
  $dir;
}

sub constructGriddingDir{
  $satellite = getSatnum($_[0]);
  $sensor    = $_[1] || croak "(param 2) Need sensor number [1,2,3,4,5] 1=vis, N=irN\n";
  $sensordir=$sensornum2dir{$sensor};
  local($dir) = "$GRIDDINGTOP/goes$satnum/$sensordir";
  $dir;
}

sub getSatnum{
  $satellite = $_[0] ||
      croak "(param 1) Need Satellite location (west|east) or Satellite number (10|8)\n";
  $satellite =~ tr/[a-z]/[A-Z]/;
  if ($satellite =~ /(WEST|EAST)/) {
    $satnum=$satloc2satnum{$1}; 
  } else {
    $satnum=$satellite;
  }
  $satnum;
}


sub getLocalAINF {
  local( $dir, $local_ainf);
  $dir=constructLocalDir(@_);
  $local_ainf="$dir/area_info";
  open AINF, "<$local_ainf" || croak "Can't open $local_ainf\n";
  @lainf = <AINF>;
  close AINF;
  @lainf;
}

sub fetchRemoteAINF {
  local( $dir, $remote_ainf);
  $startdir=getcwd();
  $localdir=constructLocalDir(@_);
  $local_file="$localdir/noaa_area_info";
  chdir $localdir || croak "Can't cd to $localdir\n";

  $remotedir=constructRemoteDir(@_);
  $remote_file="$remotedir/area_info";
  ($host,$user,$pw) = getgoesarchive();
  
  $ftp = Net::FTP->new( $host ) || croak "  Can't open new connection to $host\n";
  $ftp->login ($user, $pw )     || croak "  Can't login to $host using $user/$pw\n";
  $ftp->binary                  || croak "  Can't go to binary\n";
  print "  Getting $remote_file and sending it to $local_file\n";
  $ftp->get($remote_file,$local_file) || croak "  Can't get $remote_file\n";
  $ftp->quit                          || croak "  Can't close ftp connection\n";
  $local_file;
}

sub getRemoteAINF{

  ## Usage: $remote_ainf=fetchRemoteAINF(satellite_number, sensor_number);

  # The name is a bit of a misnomer. This routine fetches the remote
  # ainf file from the Noaa server and then reads it from the local
  # machine. So the variable $remote_ainf actually refers to a file on
  # this machine.

  $remote_ainf=fetchRemoteAINF($_[0], $_[1]);
  open RAINF, "<$remote_ainf" || croak "Can't open $remote_ainf\n";
  @rainf= <RAINF>;
  close RAINF;
  @rainf;
}


sub getgoesarchive{ 

  $goes_info_file=shift || "/usr/people/vapuser/Qscat/Library/goes_archive";
  open(ARCHIVE_INFO, "<$goes_info_file") || croak "Can't open $goes_info_file file\n";
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
  
sub getParseAINF{
 ## usage: %filetimes=getParseAINF(satellite_number, sensor_number);

  @ainf=getRemoteAINF($_[0], $_[1]);

    # Now parse the file
  for (@ainf){
    chop;
    next if /^##.*/;
    ($file,$date,$time,$doy) = 
  /^(AREA\d+):\s+Data\s+Taken\s+on\s+(.*),\s+at\s+(\d+)\s+Hours:.*,\s+DOY\s+=\s+(\d+)/;
    if ($file =~ /AREA\d{2}00/){

        ## There are problems with the output from the NOAA version of
        ## ainf for the xx00 file. Don't know why, but we can't use
        ## the information in the area info file, we must actually
        ## open and read the file. If it isn't here, fetch it first.
      $testfile="$localdir/$file";
      if (-e  $testfile ) {
	if (open XX00FILE, "<$testfile"){
	    read XX00FILE, $hdr, 64*4;
	    close XX00FILE;
	    @hdr=unpack "L4", $hdr;
	    $year=int ($hdr[3]/1000);
	    $doy=int ($hdr[3]-$year*1000);
	    ($day, $month) = doy2mday_mon($doy, $year);
	    $hour=int ($hdr[4]/100);
	    $min=int ($hdr[4]-$hour*100);	  
	    $timegm=timegm(0,$min,$hour,$day,$month-1,$year-1900);
	} else {
	  print "Can't open the XX00 file $testfile -- skipping it!\n";
	  $timegm=0;
	}
      } else {
	print "XX00 File $testfile isn't in our archive -- skipping it!\n";
	$timegm=0 ;
      }
    } else {
      ($month,$day,$year) = split "/",$date;
      ($hour,$min) = $time =~ /(\d{2,2})(\d{2,2})/;
      $timegm=timegm(0,$min,$hour,$day,$month-1,$year-1900);
    }
    $filetimes{$file} = $timegm;
  }
  %filetimes;
}

sub NearestAreaFile{
  local( $satellite, $sensor, $testtime, $absflag, 
	$diff, $mindiff, $remoteflag, $localdir );

  $satellite = $_[0] || croak "(param 1) Need Satellite location (west|east)\n";
  $sensor    = $_[1] || croak "(param 2) Need sensor number [1,2,3,4,5] 1=vis, N=irN\n";
  $testtime  = $_[2] || croak "(param 3) Need testtime (unix time)\n";
  $absflag   = $_[3] || 1;

  $localdir=constructLocalDir($satellite, $sensor);
  $remoteflag=1;

  $mindiff = 1.e10;
  %filetimes=getParseAINF($satellite,$sensor);
  foreach $k (keys %filetimes){
    $diff = $testtime-$filetimes{$k};
    $diff = abs($diff) if $absflag;

    if ($diff > 0) {
      if ($diff < $mindiff) {
	$mindiff=$diff;
	$file=$k;
	$time=$filetimes{$k};
      }
    }
  }
  $remoteflag = checkLocalAreaFile("$localdir/$file", $testtime);
  ($file, $time, $mindiff, $remoteflag );

}

sub fetchAREAFile{

  local( $areafile, $num, $sens, $ftp, $localdir, 
	$remotedir, $remote_file, $local_file, $host, $user, $pw);

  $areafile=shift || croak "(param 1) Need AREAFILE\n";

  ($num,$sens) = $areafile =~ /^AREA(\d)(\d)(.*)/;
  $satellite=$areanum2satnum{$num};
  $localdir=constructLocalDir($satellite,$sens);
  $remotedir=constructRemoteDir($satellite,$sens);

  $remotefile="$remotedir/$areafile";
  $localfile="$localdir/$areafile";
      
  ($host,$user,$pw) = getgoesarchive();
  croak "Can't get goes Archive info\n" unless defined $pw;

  print "  Getting $remotefile and sending it to $localfile\n";
  
  $ftp = Net::FTP->new( $host )       || croak "  Can't open new connection to $host\n"; 
  $ftp->login ($user, $pw )           || croak "  Can't login\n";                        
  $ftp->binary                        || croak "  Can't go to binary\n";                 
  $ftp->get($remotefile,$localfile)   || croak "  Can't get file $remotefile\n";
  $ftp->quit                          || croak "  Can't close\n";
  $localfile;
}

sub setMinDiff{
  if ($_[0]) {
    $_min_time_diff=shift;
    } else 
    { carp "Taking default time difference of 45 minutes!\n";
     $_min_time_diff = 45;
    }
  $_min_time_diff;
}

sub AlreadyGridded {
  local($areafile, $minlon, $minlat, $maxlon, $maxlat, $gridfile, $gridpath, $usage );
  $usage="Usage: $gridfile=AlreadyGridded($areafile,$minlon,$minlat,$maxlon,$maxlat)\n";

  $areafile = shift || croak $usage;
  $minlon   = shift || 0 ;
  $minlat   = shift || 0 ;
  $maxlon   = shift || 0 ;
  $maxlat   = shift || 0 ;

  ($path, $stuff, $areanum, $sensornum, @rest) = 
      $areafile =~ /(.*)\/(AREA)(\d{1,1})(\d{1,1})(\d{2,2})/;
  $satnum = $areanum2satnum{$areanum};
  $gridpath=constructGriddingDir($satnum, $sensornum);

  
  if ($minlon==0 && $minlat==0 && $maxlon==0 && $maxlat==0) {
      if ($satnum == 10) {
	  $minlon=160;
	  $minlax=0;
	  $maxlon=-70;
	  $maxlat=70;
      } elsif ($satnum == 8) {
	  # Don't know what then limits are on Goes 8 files.
	croak "What are the limits for GoesEast, anyway?\n";
      } else {
	  #Job security
	croak "How the hell did I get here?!?\n";
      }
  }
  ($year,$month,$day,$hour,$min) = readAreaFileHdr($areafile);
  $t = sprintf("GOES%02d%1d-%04d%02d%02d%02d-%%%04d,%03d,%04d,%03d%%.dat", 
	       $satnum, $sensornum,$year, $month, $day, $hour, 
	       $minlon, $minlat, $maxlon, $maxlat );
  $gridfile = $t if (-e $t);
}

sub readAreaFileHdr{

  local($areafile,@hdr,$year,$doy,$day,$month,$hour,$min);
  $areafile=shift || 
      croak "Usage: (year,month,day,hour,min) = readAreaFileHdr(areafile)\n";
  open AREAFILE, "<$areafile" || croak "Can't open $areafile\n";
  read AREAFILE, $hdr, 64*4;
  close AREAFILE;
  @hdr=unpack "L64", $hdr;
  $year=int ($hdr[3]/1000);
  $doy=int ($hdr[3]-$year*1000);
  $year += 1900;
  ($day, $month) = doy2mday_mon($doy, $year);
  $hour=int ($hdr[4]/10000);
  $min=int ($hdr[4]/100)-$hour*100;
  ($year,$month,$day,$hour,$min);
}

sub rectifyLon{
  $minlon=shift || croak "Need MinLon!\n";
  $maxlon=shift || croak "Need MaxLon!\n";
  $minlon -= 360 if $minlon > 180;
  $maxlon -= 360 if $maxlon > 180;
  ($minlon, $maxlon);
}

sub checkLocalAreaFile{
  $areafile = shift || 
      croak "Usage: $remoteflag=checkLocalAreaFile( $areafile, $filetime)\n";
  $areafiletime = shift || 
      croak "Usage: $remoteflag=checkLocalAreaFile( $areafile,$filetime )\n";

  if (-e $areafile ) {
    ($year,$month,$day,$hour,$min) = readAreaFileHdr( $areafile );
    $time=timegm( 0, $min, $hour, $day, $month-1, $year-1900);
    $remoteflag = $areafiletime-$time > 60*60;
  } else {
    $remoteflag=1;
  }

  $remoteflag;
}

1;
