#/usr/bin/perl
#
# $Id$
#
# Object Oriented version of Goes.pm
# For handling Goes AREA file transfers and gridding.
#
# Author: William H. Daffer
#
# Modifications:
#
# $Log$
#
#
use strict;
use vars qw(@ISA $VERSION);
use Cwd 'chdir', 'getcwd';
use Time::Local;
use Net::FTP;
use Carp;
use vap_util;

$VERSION = "1.1" # $Id$
@ISA     = qw(Exporter);

#use vars qw();

sub new {
  my $class = shift;
  my $satdesig = shift || die "Need Satellite designation (east|west) or (10|8)\n";
  my $sensordesig= shift || die "Need sensor designation ([1,2,3,4,5] or [vis, irN]\n";
  my $time=shift || systime2idltime(time());

  $VAP_LIB=$ENV{'VAP_LIB'}                  || "/usr/people/vapuser/Qscat/Library";
    # Get the Overlay Defaults
  $overlay_defs_file=$VAP_LIB."/overlay_defs";
  require $overlay_defs_file;

    # Get generic VAP processing Defaults
  $vap_defs_file=$VAP_LIB."/vap_defs";
  require $vap_defs_file;

    # Check for interactivity.
  $_is_batch = !defined($ENV{'TERM'});

  %satnum2areanum = (10 => 8,
		     8 => 9);

  %areanum2satnum = (8 => 10, 
		     9 => 8 );

  %satloc2satnum = (west=>10,
		    east=>8);
  
  %sensornum2dir = ('1','vis',
		    '2','ir2',
		    '3','ir3',
		    '4','ir4',
		    '5','ir5'
		    );



  my $self = {SATDESIG => $satdesig,
	      SENSORDESIG => $sensordesig, 
	      TIME => $time,
	      _MIN_TIME_DIFF => 45*60 };

  $self->{SATNUM} = getSatNum();
  $self->{SENSORDIR} = getSensorDir();
  $self->{LOCALDIR} = constructLocalDir();  
  $self->{REMOTEDIR} = constructRemoteDir();  
  $self->{IDLTIME} = $time;
  $self->{TIME} = idltime2systime($time);
  
  bless $self, $class;
}





sub gag{
  $usage="Usage: gag satellite, sensornum, time, minlon, minlat, maxlon, maxlat";
  die "$usage\n" if $#_ < 1;
  $time=$_[2] || systime2idltime(time());
  local($areafile)=getAreaFile($_[0], $_[1], $time );
  $minlon = $_[3] || 0;
  $minlat = $_[4] || 0;
  $maxlon = $_[5] || 0;
  $maxlat = $_[6] || 0;
  $gridpath = constructGriddingDir($_[0], $_[1]);
  $startdir=getcwd();
  chdir $gridpath || croak "Can't CD to $gridpath\n";
  $griddedfile = grid( $areafile, $minlon, $minlat, $maxlon, $maxlat );
  chdir $startdir || carp "Can't CD to $startdir\n";
  $griddedfile;
}

sub grid{ 

  $areafile = shift @_ ||
      die "Usage: grid areafile [ minlon [ minlat [ maxlon [ maxlat ]]]]\n";;
  $minlon = shift @_ || 0 ;
  $minat  = shift @_ || 0 ;
  $maxlon = shift @_ || 0 ;
  $maxat  = shift @_ || 0 ;

  print "  Preparing to grid area file $areafile\n";

  if ($minlon != 0 || $minlat != 0 || $maxlon != 0 || $maxlat != 0) {
    $minlon2=$minlon;
    $maxlon2=$maxlon;
    $minlon2 -= 360 if ($minlon >= 180);
    $maxlon2 -= 360 if ($maxlon >= 180);
    $exe_string=sprintf( "grid_goes -f %s -l %04d,%03d,%04d,%03d", 
	    $areafile, $minlon2, $minlat, $maxlon2, $maxlat );
  } else {
      $exe_string="grid_goes -f $areafile";
  }
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
  my $self = shift;
  local($satellite,$sensor,$vaptime,$localfile,$localdir,
	$file,$diff,$mindiff,$time,$test_time);
  $satnum = $self->{SATNUM} || getSatNum();
  $sensornum = $self->{SENSORNUM} || getSensorNum();
  $idltime   = shift || $self->{IDLTIME}


  ($year,$month,$day,$hour,$min,$sec) = split( "/", $vaptime );
  $test_time = timegm( 0, $min, $hour, $day, $month-1, $year-1900 );

  ($file, $time,$mindiff,$remoteflag) = 
      NearestAreaFile($satnum, $sensor, $test_time);

  if ($mindiff < 45*60 ) {
    if ($remoteflag) {
      $localfile=fetchAREAFile($file);
    } else {
      $localdir=constructLocalDir($satnum, $sensor);
      $localfile="$localdir/$file";
    }
  } else {
    $hh=$mindiff/3600;
    croak "Nearest AREA file ($file) is $hh hours distant! -- Aborting\n";
  }
  $localfile;
}

sub constructLocalDir{
  my $self = shift;
  $satnum= $self->{SATNUM} || getSatNum();
  $sensordir= $self->{SENSORDIR} || getSensorDir();
  local($dir) = ($self->{LOCALDIR} = "$ARCHIVETOP/goes$satnum/$sensordir");
  $dir;
}

sub constructRemoteDir{
  my $self=shift;
  $satnum= $self->{SATNUM} || getSatNum();
  $sensordir= $self->{SENSORDIR} || getSensorDir();
  local($dir) = ($self->{REMOTEDIR} = "goes$satnum/$sensordir");
  $dir;
}

sub constructGriddingDir{
  my $self=shift;
  $satnum= $self->{SATNUM} || getSatNum();
  $sensordir= $self->{SENSORDIR} || getSensorDir();
  local($dir) = ($self->{GRIDDINGDIR} = "$GRIDDINGTOP/goes$satnum/$sensordir");
  $dir;
}

sub getSatNum{
  my $self=shift;
  $satellite = $self->{SATDESIG}
      croak "(param 1) Need Satellite location (west|east) or Satellite number (10|8)\n";
  if ($satellite =~ /(west|east)/) {
    $satnum=$satloc2satnum{$satellite}; 
  } else {
    $satnum=$satellite;
  }
  $self->{SATNUM} = $satnum;
  $satnum;
}

sub getSensorDir{
  my $self=shift;
  if ($self->{SENSORDESIG} =~ /vis|ir?/) {
    $self->{SENSORNUM} = (($self->{SENSORDESIG} =~ /vis|ir(\d)/) || 1);
#     if ($self->{SENSORDESIG} =~ /vis/) {
#       $self->{SENSORNUM} = 1;
#       local($dir) = ($self->{SENSORDIR} = $self->{SENSORDESIG}); 
#     } else {
#       $self->{SENSORNUM}= ($self->{SENSORDESIG} =~ /ir(\d)/);
#       local($dir) = ($self->{SENSORDIR} = $self->{SENSORDESIG}); 
#    }
  } else {
    $self->{SENSORNUM} = $self->{SENSORDESIG};
    local($dir) = ($self->{SENSORDIR} = $sensornum2dir{ $self->{SENSORNUM} });
  }
  $dir
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
  $ftp->login ($user, $pw )     || croak "  Can't login\n";
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
    if ($file =~ /AREA??00/){

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
	  carp "Can't open the XX00 file $testfile -- skipping it!\n";
	  $timegm=0;
	}
      } else {
	carp "XX00 File $testfile isn't in our archive -- skipping it!\n";
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
  $absflag   = $_[3] || 0;

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
  $remoteflag = 0 if (-e "$localdir/$file");
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


  ($host,$user,$pw) = getgoesarchive();
  croak "Can't get goes Archive info\n" unless defined $pw;

  $remotefile="$remotedir/$areafile";
  $localfile="$localdir/$areafile";
  
  $ftp = Net::FTP->new( $host ) || croak "  Can't open new connection to $host\n";
  $ftp->login ($user, $pw )     || croak "  Can't login\n";
  $ftp->binary                  || croak "  Can't go to binary\n";
  print "  Getting $remotefile and sending it to $localfile\n";
  $ftp->get($remotefile,$localfile) || croak "  Can't get file $remotefile\n";
  $ftp->quit                          || croak "  Can't close\n";
  $localfile;
}

sub setMinDiff{
  $_min_time_diff=shift || 
  { carp "Taking default time difference of 45 minutes!\n";
    45;}
}
1;
