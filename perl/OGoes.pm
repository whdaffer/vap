#/usr/bin/perl -w
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
# Revision 1.2  2002/05/07 20:40:36  vapdev
# Set -w and `use strict' and then fixing bugs. Start trying to standardize
# the methods used.
#
# Revision 1.1  2001/02/09 18:49:15  vapuser
# Initial revision
#
# Usage:
#
# Methods: 
#
#  new:
#
#  $goesobj = OGoes->new( SAT    => (10|8),
#                         SENSOR => ("vis"|"irN"),
#                         [TIME => yyyy/mm/dd/hh/mm,
#                          LIMITS => \[minlon, minlat, maxlon, maxlat],
#                          ABSFLAG => 1 ] );
#
#
#
#  'TIME' will default to current time (in GMT) if it isn't passed in.
#  'LIMITS' will default to [0,0,0,0], which to the gridding software
#           means 'the whole file.'
#
#  Set 'ABSFLAG' if you want all time comparisons to be done in
#  absolute value. Otherwise the comparisons are done such that any
#  files found have times less than or equal to the input (or
#  defaulted) time.
#
#
#
package OGoes;
use strict;
use vars qw(@ISA $VERSION);
use Cwd 'chdir', 'getcwd';
use Time::Local;
use Net::FTP;
use Carp;
use VapUtil;


$OGoes::VERSION = "0.9";
%OGoes::satnum2areanum = (10 => 8,
			  8 => 9);

%OGoes::areanum2satnum = (8 => 10, 
			  9 => 8 );

%OGoes::satloc2satnum = (WEST=>10,
			 EAST=>8);

%OGoes::sensornum2name = ('1','vis',
			 '2','ir2',
			 '3','ir3',
			 '4','ir4',
			 '5','ir5'
			 );


%OGoes::sensorname2num = reverse %OGoes::sensornum2dir



#----------------------------------------------------------
#
#----------------------------------------------------------


sub new {
  my $class = shift;

  my $self = {@_};
  $self->{VAP_LIB}=$ENV{'VAP_LIBRARY'}  || 
      croak "Environmental variable VAP_LIBRARY is UNDEFINED\n";

    # Get the Overlay Defaults



#    $overlay_defs_file="$VAP_LIB/overlay_defs";
#    if (-e "$overlay_defs_file") {
#      local $/=undef;
#      open FILE, "$overlay_defs_file" or croak "Can't open $overlay_defs_file: $!\n";
#      eval {require $overlay_defs_file} || croak "require $overlay_defs_file FAILED!: $!\n";
#    }

    # Get generic VAP processing Defaults
  $self->{VAP_DEFS_FILE} ="$VAP_LIB/vap_defs";
  eval {require $self->{VAP_DEFS_FILE} } || 
    croak "require of ".$self->{VAP_DEFS_FILE} ." FAILED!: $!\n";

    # Check for interactivity.
  my $self->{_IS_BATCH} = !defined($ENV{'TERM'});
  my $self->{_MIN_DIFF} = 1.5*60;
  $self->{LOCALDIR} = constructLocalDir();
  $self->{REMOTEDIR} = constructRemoteDir();
  $self->{SYSTIME} = idltime2systime($time);
  $self->{SENSORNUM} = $OGOES::sensorname2num{$self->{SENSOR}};
  $self->{REMOTE_ARCHIVE_INFO} = VapUtil::getgoesarchive();
  my $limits = $self->{LIMITS} || [0,0,0,0];

  croak "Input Keys SAT and SENSOR are required!\n" 
    unless (exists($self->{SAT}) && exists($self->{SENSOR}));
    
  bless $self, ref($class) || $class;
  my @tmp=$self->rectifylon(${$limits}[0], ${$limits}[2]);
  ${$self->{LIMITS}}[0] = $tmp[0];
  ${$self->{LIMITS}}[2] = $tmp[1];
  return, $self;
}


#----------------------------------------------------------
#
#----------------------------------------------------------


sub rectifylon{
  croak "usage: \@fixedLon = rectifylon(\$minlon, $maxlon)\n";
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




#----------------------------------------------------------
#
#----------------------------------------------------------


sub gag{
  my $self=shift;
  my $systime=$self->{SYSTIME}
  my $areafile=getAreaFile();
  my @limit = @{$self->{LIMITS}};
  my $minlon = $limit[0];
  my $minlat = $limit[1];
  my $maxlon = $limit[2];
  my $maxlat = $limit[3];
  my $gridpath = $self->griddingPath();
  my $startdir=getcwd();
  chdir $gridpath || croak "Can't CD to $gridpath\n";
  my $griddedfile = grid( $areafile, $minlon, $minlat, $maxlon, $maxlat );
  chdir $startdir || carp "Can't CD to $startdir\n";
  my $griddedfile;
}


#----------------------------------------------------------
#
#----------------------------------------------------------


sub grid{ 
  my $self=shift;
  my $areafile = shift @_ ||
      croak "Usage: grid areafile [ minlon [ minlat [ maxlon [ maxlat ]]]]\n";;
  my $minlon = shift @_ || 0 ;
  my $minat  = shift @_ || 0 ;
  my $maxlon = shift @_ || 0 ;
  my $maxat  = shift @_ || 0 ;

  print "  Preparing to grid area file $areafile\n";

  my $exe_string;
  if ($minlon != 0 || $minlat != 0 || $maxlon != 0 || $maxlat != 0) {
    my $minlon2=$minlon;
    my $maxlon2=$maxlon;
    my $minlon2 -= 360 if ($minlon >= 180);
    my $maxlon2 -= 360 if ($maxlon >= 180);
    $exe_string=sprintf( "grid_goes -f %s -l %04d,%03d,%04d,%03d", 
	    $areafile, $minlon2, $minlat, $maxlon2, $maxlat );
  } else {
    $exe_string="grid_goes -f $areafile";
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
  my $local_gridded_file="$gridding_output[$#gridding_output]";
  chop $local_gridded_file;
  my $local_gridded_file;
}

#----------------------------------------------------------
# GetAREAFile
#----------------------------------------------------------

sub GetAREAFile{
  my $self = shift;
  my( $satellite,$sensor,$vaptime,$localfile,$localdir,
      $file,$diff,$mindiff,$time,$test_time, $remoteflag);
  ($file, $time,$mindiff,$remoteflag) = 
    NearestAREAFile();

  if ($mindiff < $self->{_MIN_DIFF_TIME) {
    if ($remoteflag) {
      $localfile=FetchAREAFile($file);
    } else {
      $localdir=LocalDir();
      $localfile="$localdir/$file";
    }
  } else {
    my $hh=$mindiff/3600;
    print "Nearest AREA file ($file) is $hh hours distant! -- Aborting\n";
  }
  $localfile;
}


#----------------------------------------------------------
# NearestAREAFile
# Find the AREA file nearest in time to $self->{TIME}
# Fetch it from Paul's FTP site, if necessary.
#
#----------------------------------------------------------


sub NearestAREAFile{
  my $self=shift;

  my ( $satellite, $sensor, $testtime, $absflag, 
	$diff, $mindiff, $remoteflag, $localdir );
  $localdir=LocalDir();
  $remoteflag=1;

  $mindiff = 1.e10;
  $filetimes=GetAndParseAINF();
  while (my ($k, $v) = each %{$filetimes}){
    $diff = $testtime-$v;
    $diff = abs($diff) if $absflag;

    if ($diff > 0) {
      if ($diff < $mindiff) {
	$mindiff=$diff;
	$file=$k;
	$time=$v;
      }
    }
  }
  $remoteflag = 0 if (-e "$localdir/AREA$file");
  $self->{NEAREST}->{FILE} = $file;
  $self->{NEAREST}->{TIME} = $time;
  $self->{NEAREST}->{DIFF} = $mindiff;
  $self->{NEAREST}->{REMOTE} = $remoteflag;
  ($file, $time, $mindiff, $remoteflag );

}

#----------------------------------------------------------
#GetAndParseAINF
#
# Retrieve and Parse the NOAA AINF file. This file resides on Paul's
# NOAA FTP site.
#
#----------------------------------------------------------


sub GetAndParseAINF{
 ## usage: 1|0 = getParseAINF(satellite_number, sensor_number);

  my $self=shift;
  my (@ainf, $file, $date, $time, $doy, $testfile, $localdir, 
      @hdr, $yer, $doy, $day, $month, $hour, $min, $timegm, %filetimes);


  $self->FetchRemoteAINF();
    # Now parse the file
  $self->ParseAINF();

  1;
}

sub ParseAINF{
  
  # opens the input file and parses the records therein. Puts the
  # info in $self->{AINF} hash.
  #
  # There's one special case: the AREAxx00 files have a problem with
  # the output from the NOAA version of ainf, (don't ask me why) so in
  # the case of this file we first check the local `area_info' file,
  # and if the data isn't in there we read the header itself if the
  # AREA file is here. If the AREA file isn't local we leave markers
  # in the $self->{AINF} hash to indicate its absence.

  # The $self->{AINF} hash has the AREA file number as the key and the
  # time of its data (as Unix time) as the value.

  my $self=shift;
  my $file=$self->{REMOTE_AINF_FILE};
  open FILE, "<$file" or croak "Can't open $file:$!\n";
  while (<FILE>){
    my $timegm=0;
    chomp;
    next if /^##.*/;
    ($filenum,$month, $day, $year,$time,$doy) = 
  m|^(AREA\d+):\s+Data\s+Taken\s+on\s+(\d+)/(\d+)/(\d+),\s+at\s+(\d+)\s+Hours:.*,\s+DOY\s+=\s+(\d+)|;
    if ($filenum =~ /AREA??00/){

        ## There are problems with the output from the NOAA version of
        ## ainf for the xx00 file. Don't know why, but we can't use
        ## the information in the area info file, we must actually
        ## open and read the file. If it isn't here, fetch it first.
      $testfile="$localdir/AREA$filenum";
      if (-e  $testfile ) {
	$timegm = $self->ReadXX00File($testfile);
      } else {
	print "XX00 File $testfile isn't in our archive -- skipping it!\n";
      }
    } else {
      ($hour,$min) = $time =~ /(\d{2,2})(\d{2,2})/;
      $timegm=timegm(0,$min,$hour,$day,$month-1,$year-1900);
    }
    $self->{AINF}->{$file} = $timegm;
  }
  1;
}

#---------------------------------------------------------- 
# ReadXX00File - Get the data time for an xx00 file. 
#
#
# For some strange reason the noaa versio of `ainf' outputs garbage
# for files with this name.  First check to see if the information for
# this AREA file is in the local area_info file. If it is, use that,
# otherwise open and read the header. We don't get the lat/lon
# resolution this way, but we don't really use it anyway.
#
#----------------------------------------------------------


sub ReadXX00File{
  my ($self, $testfile) = @_;
  my ($hdr, @hdr, $year, $doy, $day, $month,
      $hour, $min);
  my $timegm=0;

  my ($name, $path) = fileparse($testfile);
  my $exe_string = "grep $name ". $self->LocalDir(). "/area_info";

  if ($string = `$exe_string`) {

   ($filenum,$month, $day, $year,$time,$doy) = $self->ParseAINFLine($string);
   ($day, $month) = VapUtil::doy2mday_mon($doy, $year);
   $timegm = timegm(0,$min,$hour,$day,$month-1,$year-1900);

  } else {

    if (open XX00FILE, "<$testfile") {
      read XX00FILE, $hdr, 64*4;
      close XX00FILE;
      @hdr=unpack "L4", $hdr;
      $year=int ($hdr[3]/1000);
      $doy=int ($hdr[3]-$year*1000);
      ($day, $month) = VapUtil::doy2mday_mon($doy, $year);
      $hour=int ($hdr[4]/100);
      $min=int ($hdr[4]-$hour*100);
      $timegm=timegm(0,$min,$hour,$day,$month-1,$year-1900);
    } else {
      print "Can't open the XX00 file $testfile -- skipping it!\n";
    }

  }

  $timegm;

}


#----------------------------------------------------------
# ParseAINFLine - Parse one line from an AINF file
#----------------------------------------------------------

sub ParseAINFLine{
  my $self=shift;
  my $line = shift;
  ($filenum,$month, $day, $year,$time,$doy) = 
    ($line =~ m|^(AREA\d+):\s+Data\s+Taken\s+on\s+
                 (\d+)/(\d+)/(\d+),\s+at\s+(\d+)\s+Hours:.*,\s+
                 DOY\s+=\s+(\d+)|x;
}
#----------------------------------------------------------
# LocalAINF - Read the local AINF file. 
# Return the data in an array
#----------------------------------------------------------


sub LocalAINF {
  my $self=shift;
  $dir = $self->{LOCALDIR}
  my ($dir, $local_ainf);
  $local_ainf="$dir/area_info";
  open AINF, "<$local_ainf" || croak "Can't open $local_ainf\n";
  @lainf = <AINF>;
  close AINF;
  @lainf;
}

#----------------------------------------------------------
# FetchRemoteAINF - Fetch the local AINF file.
# Put it in LocalDir
# Return the name of the file
#----------------------------------------------------------


sub FetchRemoteAINF {
  my $self=shift;
  my ($startdir, $localdir, $local_file, 
      $dir, $remotedir, $remote_file, $host, $user, $pw,
      $remote_ainf);
  $startdir=getcwd();
  $localdir=LocalDir();
  $local_file="$localdir/noaa_area_info";
  chdir $localdir || croak "Can't cd to $localdir\n";

  $remotedir=RemoteDir();
  $remote_file="$remotedir/area_info";

  my $host = $self->{ARCHIVE_INFO}->{HOST};
  my $user = $self->{ARCHIVE_INFO}->{USER};
  my $pw   = $self->{ARCHIVE_INFO}->{PW};

  my $ftp = Net::FTP->new( $host ) || croak "  Can't open new connection to $host\n";
  $ftp->login ($user, $pw )     || croak "  Can't login\n";
  $ftp->binary                  || croak "  Can't go to binary\n";
  print "  Getting $remote_file and sending it to $local_file\n";
  $ftp->get($remote_file,$local_file) || croak "  Can't get $remote_file\n";
  $ftp->quit                          || croak "  Can't close ftp connection\n";
  $self->{NOAA_AINF_FILE} = $local_file;
}

#----------------------------------------------------------
# 
#----------------------------------------------------------


sub getRemoteAINF{

  ## Usage: $remote_ainf=fetchRemoteAINF(satellite_number, sensor_number);

  # The name is a bit of a misnomer. This routine fetches the remote
  # ainf file from the Noaa server and then reads it from the local
  # machine. So the variable $remote_ainf actually refers to a file on
  # this machine.

  my $self=shift;
  my ($remote_ainf, @rainf);

  $remote_ainf=fetchRemoteAINF($_[0], $_[1]);
  open RAINF, "<$remote_ainf" || croak "Can't open $remote_ainf\n";
  @rainf= <RAINF>;
  close RAINF;
  @rainf;
}


#----------------------------------------------------------
# FetchAREAFile
#----------------------------------------------------------


sub FetchAREAFile{

  my $self=shift;

  my ( $areafile, $num, $sens, $ftp, $localdir, 
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

#----------------------------------------------------------
#
#----------------------------------------------------------

sub setMinDiff{
  my $self=shift
  my $self->{_MIN_TIME_DIFF} =shift || 
  { print "Taking default time difference of 45 minutes!\n";
    45;}
}


#----------------------------------------------------------
# Construct the Local directory
#----------------------------------------------------------


sub LocalDir{
  my $self = shift;
  my $satnum= $self->{SAT};
  my $sensordir= $self->{SENSOR};
  my $dir = ($self->{LOCALDIR} = "$VapUtil::ARCHIVETOP/goes$satnum/$sensordir");
}

#----------------------------------------------------------
# Construct the Remote directory
#----------------------------------------------------------


sub RemoteDir{
  my $self=shift;
  my $satnum= $self->{SAT};
  my $sensordir= $self->{SENSOR};
  $dir = ($self->{REMOTEDIR} = "goes$satnum/$sensordir");
}

#----------------------------------------------------------
# Construct the Path where the gridded files go
#----------------------------------------------------------


sub GriddingPath{
  my $self=shift;
  my $satnum= $self->{SAT};
  my $sensordir= $self->{SENSOR};
  $dir = ($self->{GRIDDINGDIR} = "$GRIDDINGTOP/goes$satnum/$sensordir");
}

#----------------------------------------------------------
#
#----------------------------------------------------------


sub SatNum{
  my $self=shift;
  my $satnum;
  my $satellite = $self->{SATDESIG}
      croak "(param 1) Need Satellite location (west|east) or Satellite number (10|8)\n";
  if ($satellite =~ /(west|east)/i) {
    $satnum=$satloc2satnum{$satellite}; 
  } else {
    $satnum=$satellite;
  }
  $self->{SATNUM} = $satnum;
  $satnum;
}

#----------------------------------------------------------
#
#----------------------------------------------------------


sub SensorDir{
  my $self=shift;
  my $dir;
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
    $dir = ($self->{SENSORDIR} = $sensornum2name{ $self->{SENSORNUM} });
  }
  $dir
}


#----------------------------------------------------------
#
#----------------------------------------------------------


1;

