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
# Revision 1.5  2002/08/08 23:26:16  vapdev
# General cleanup, work on BEGIN{} block.
#
# Revision 1.4  2002/08/07 23:42:34  vapdev
# Wrap up conversion to OO
#
# Revision 1.3  2002/07/03 22:37:18  vapdev
# Continued upgrade work
#
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
#                          DELTA => f.g,
#                          LIMITS => [minlon, minlat, maxlon, maxlat],
#                          ABSFLAG => 0|1 ] );
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

=pod

=head1 OGoes.pm


=head2 USAGE: 

   $obj = OGoes->new( SAT => (10|8), 
                      SENSOR => ("vis"|"irN"),
                      [TIME => "yyyy/mm/dd/hh/mm",
                       DELTA => f.g, 
                       LIMITS => [minlon, minlat, maxlon, maxlat],
                       ABSFLAG => 0|1 ] );

=over 4

=item * SAT: The number of the satellite: 10 for GOES 10, 8 for GOES 8
             REQUIRED!

=item * SENSOR: vis, or ir{2,3,4}, as a *string*
                REQUIRED!

=item * TIME: string, "YYYY/MM/DD/HH/MM"
              Defaults to current time.

=item * DELTA: A float. The delta time around TIME to search for
               data. If ABSFLAG is set this interval extends from
               DATETIME-DELTA to DATETIME+DELTA, otherwise, the time
               of the image file must reside in the interval
               DATETIME-DELTA to DATETIME.

               Default = 4.0

=item * ABSFLAG: 0|1. If 1, check the interval
                [DATETIME-DELTA,DATETIME+DELTA]

=item * ERROROBJ: object of type VapError. If not provided by caller,
                  the object creates its own.

=back


=cut


package OGoes;
use strict;
use vars qw($VERSION);
use Cwd 'chdir', 'getcwd';
use Time::Local;
use Net::FTP;
use Carp;
use File::Basename;

BEGIN {
  $VERSION = "0.9";
  croak "ENV var VAP_LIBRARY is undefined\n" 
    unless $ENV{VAP_LIBRARY};
  croak "ENV var VAP_SFTWR_PERL is undefined\n" 
    unless $ENV{VAP_SFTWR_PERL};
}

use lib $ENV{VAP_SFTWR_PERL};
use VapUtil;
use VapError;
@OGoes::ISA = qw(VapError);

#----------------------------------------------------------
#
#----------------------------------------------------------


sub new {
  my $class = shift;

  my $self = {@_};
  $self->{VAP_LIB}=$ENV{'VAP_LIBRARY'};

    # Get the Overlay Defaults

    # Check for interactivity.
  $self->{_IS_BATCH} = !defined($ENV{'TERM'});
  $self->{_MIN_DIFF_TIME} = 1.5*3600;
  $self->{TIME} = VapUtil::systime2idltime(time()) unless $self->{TIME};
  $self->{SYSTIME} = VapUtil::idltime2systime($self->{TIME});
  $self->{SENSORNUM} = $VapUtil::sensorname2num{$self->{SENSOR}} 
    if $self->{SENSOR};
  $self->{SENSOR} = $VapUtil::sensornum2name{$self->{SENSORNUM}} 
    unless $self->{SENSOR};
  $self->{REMOTE_ARCHIVE_INFO} = 
    VapUtil::getgoesarchive("$VapUtil::VAP_LIBRARY/goes_archive");
  my $limits = $self->{LIMITS} || [0,0,0,0];
  $self->{DELTA} = 4.0 unless $self->{DELTA};

  croak "Input Keys SAT and (SENSOR or SENSORNUM)  are required!\n" 
    unless (exists($self->{SAT}) && 
	    (exists($self->{SENSOR}) || exists($self->{SENSORNUM})));
    
  bless $self, ref($class) || $class;
  $self->{LOCALDIR} = $self->LocalDir();
  $self->{REMOTEDIR} = $self->RemoteDir();
  my @tmp=VapUtil::fixlonrange(${$limits}[0], ${$limits}[2]);
  ${$self->{LIMITS}}[0] = $tmp[0];
  ${$self->{LIMITS}}[2] = $tmp[1];
  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};
  return $self;
}



#----------------------------------------------------------
#
#----------------------------------------------------------


sub gag{
  my $self=shift;
  my $systime=$self->{SYSTIME};
  my $areafile=$self->GetAREAFile();
  my @limit = @{$self->{LIMITS}};
  my $minlon = $limit[0];
  my $minlat = $limit[1];
  my $maxlon = $limit[2];
  my $maxlat = $limit[3];
  my $gridpath = $self->GriddingPath();
  my $startdir=getcwd();
  chdir $gridpath || croak "Can't CD to $gridpath\n";
  my $griddedfile = $self->grid( $areafile, $minlon, $minlat, $maxlon, $maxlat );
  chdir $startdir || carp "Can't CD to back to $startdir\n";
  $griddedfile = "$gridpath/$griddedfile";
}


#----------------------------------------------------------
#
#----------------------------------------------------------


sub grid{ 
  my $self=shift;
  my $areafile = shift @_ ||
  croak "Usage: grid areafile [ minlon [ minlat [ maxlon [ maxlat ]]]]\n";
#      $self->{ERROROBJ}->ReportAndDie("Usage: grid areafile [ minlon [ minlat [ maxlon [ maxlat ]]]]\n");
  my $minlon = shift @_ || 0 ;
  my $minlat  = shift @_ || 0 ;
  my $maxlon = shift @_ || 0 ;
  my $maxlat  = shift @_ || 0 ;
  my $local_gridded_file;
  print "  checking for already existing gridded file!\n";
  if (!($local_gridded_file = $self->AlreadyGridded())) {
    print "  Preparing to grid area file $areafile\n";

    my $exe_string;
    if ($minlon != 0 || $minlat != 0 ||
	$maxlon != 0 || $maxlat != 0) {
      $exe_string=sprintf( "grid_goes -f %s -l %04d,%03d,%04d,%03d", 
	      $areafile, $minlon, $minlat, $maxlon, $maxlat );
    } else {
      $exe_string="grid_goes -f $areafile";
    }
    print "About to open gridding processes with exe string\n";
    print "$exe_string\n";
    open ( GRIDDING_PROCESS, "$exe_string |" );
    my @gridding_output = <GRIDDING_PROCESS>;
    close GRIDDING_PROCESS;

    print join "\n", @gridding_output;
    my @errors=grep(/^ *ERROR.*/, @gridding_output);
#    $self->{ERROROBJ}->ReportAndDie("GAG Failed!",
#				    "  Bad return from goes gridding software\n")

    if ($#errors gt -1){ 
      print "  Bad return from goes gridding software\n";
    } else {
      print "  Done Gridding!\n";
      $local_gridded_file="$gridding_output[$#gridding_output]";
      chomp $local_gridded_file;
    }
    $local_gridded_file;
  }
}


#----------------------------------------------------------
# AlreadyGridded 
#
#  Given the general information about the AREA file, check to see if
#  this file has already been gridded.
#----------------------------------------------------------
sub AlreadyGridded {
  my $self=shift;
  # The format of the filename, given in grid_goes.c, is:

  my $format= "GOES%03d_%04d%02d%02d%02d_%04d,%03d,%04d,%03d.dat";

  # We construct this filename and check to see if it exists in the
  # local gridded files repository. Return that name if it does,
  # otherwise return a null value to grid() and it will do the
  # gridding.

  my ($year,$month,$day,$hour,$min,$sec) = 
    split/\//, $self->{NEAREST}->{IDLTIME};
  my $goes_type = $self->{SAT}.$self->{SENSORNUM};
  my ($minlon, $minlat, $maxlon, $maxlat) = @{$self->{LIMITS}};
  my $testfile = sprintf($format,
			 $goes_type, $year, $month, $day, $hour,
			 $minlon, $minlat, $maxlon, $maxlat);
  my $retval = 
    (-e $self->{GRIDDEDPATH} . "$testfile")? $testfile : undef
}

#----------------------------------------------------------
# GetAREAFile
#----------------------------------------------------------

sub GetAREAFile{
  my $self = shift;
  my( $satellite,$sensor,$vaptime,$localfile,$localdir,
      $file,$diff,$mindiff,$time,$test_time, $remoteflag);
  ($file, $time,$mindiff,$remoteflag) = 
    $self->NearestAREAFile();

  if ($mindiff < $self->{_MIN_DIFF_TIME}) {
    if ($remoteflag) {
      $localfile=$self->FetchAREAFile($file);
    } else {
      $localfile=$self->{LOCALDIR}."/$file";
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
	$diff, $mindiff, $remoteflag, $localdir, $filetimes );
  $testtime = $self->{SYSTIME};
  $localdir=$self->{LOCALDIR};
  $remoteflag=1;

  $mindiff = 1.e10;
  $self->GetAndParseAINF() or croak "Can't GetAndParseAINF\n";
  $filetimes = $self->{AINF};
  my ($file, $time);
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
  $self->{NEAREST}->{FILE} = "AREA$file";
  $self->{NEAREST}->{TIME} = $time;
  $self->{NEAREST}->{IDLTIME} = VapUtil::systime2idltime($time);
  $self->{NEAREST}->{DIFF} = $mindiff;
  $self->{NEAREST}->{REMOTE} = $remoteflag;
  ("AREA$file", $time, $mindiff, $remoteflag );

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
  $self->FetchRemoteAINF() or croak "Can't fetch remote AINF\n";
    # Now parse the file
  $self->ParseAINF() or croak "Can't Parse AINF\n";
  1;
}

#----------------------------------------------------------
#ParseAINF
#
# Parse an Area INfo File.
#
#----------------------------------------------------------


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
  my $file=$self->{NOAA_AINF_FILE};
  my ($name, $localdir, $junk) = fileparse($file);
  open FILE, "<$file" or croak "Can't open $file:$!\n";
  my ($filenum,$month, $day, $year,$time,$doy);
  while (<FILE>){
    my $timegm=0;
    chomp;
    next if /^##.*/;
    if (/^AREA..00/) {

        ## There are problems with the output from the NOAA version of
        ## ainf for the xx00 file. Don't know why, but we can't use
        ## the information in the area info file, we must actually
        ## open and read the file. If it isn't here, fetch it first.
      my $testfile= (split /:/)[0];
      ($filenum) = ($testfile =~ /AREA(\d+)/);
      $testfile="$localdir"."$testfile";
      if (-e  $testfile ) {
	$timegm = $self->ReadXX00File($testfile);
      } else {
	print "XX00 File $testfile isn't in our archive -- skipping it!\n";
      }
    } else {
    ($filenum,$month, $day, $year,$time,$doy) = 
      m|^AREA(\d+):\s+Data\s+Taken\s+on\s+(\d+)/(\d+)/(\d+),
      \s+at\s+(\d+)\s+Hours:.*,\s+DOY\s+=\s+(\d+)|x;

      my ($hour,$min) = $time =~ /(\d{2,2})(\d{2,2})/;
      $timegm=timegm(0,$min,$hour,$day,$month-1,$year-1900);
    }
    $self->{AINF}->{$filenum} = $timegm;
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
  my ($hdr, @hdr);
  my $timegm=0;

  my ($filenum, $month, $day, $year, $time, $doy, $hour, $min);

  my ($name, $path, $junk) = fileparse($testfile);
  my $exe_string = "grep $name ". $self->LocalDir(). "/area_info";
  my $string;
  if ($string = `$exe_string`) {

   ($filenum,$month, $day, $year,$time,$doy) = $self->ParseAINFLine($string);
   ($day, $month) = VapUtil::doy2mday_mon($doy, $year);
   ($hour, $min ) = split /:/, $time;
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
  my ($filenum,$month, $day, $year,$time,$doy) = 
    ($line =~ m<^(AREA\d+):\s+Data\s+Taken\s+on\s+
                 (\d+)/(\d+)/(\d+),\s+at\s+(\d+)\s+Hours:.*,\s+
                 DOY\s+=\s+(\d+)>x);
}
#----------------------------------------------------------
# LocalAINF - Read the local AINF file. 
# Return the data in an array
#----------------------------------------------------------


sub LocalAINF {
  my $self=shift;
  my ($dir, $local_ainf);
  $dir = $self->{LOCALDIR};
  $local_ainf="$dir/area_info";
  open AINF, "<$local_ainf" || croak "Can't open $local_ainf\n";
  my @lainf = <AINF>;
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
  $localdir=$self->{LOCALDIR};
  $local_file="$localdir/noaa_area_info";
  chdir $localdir || croak "Can't cd to $localdir\n";

  $remotedir=$self->{REMOTEDIR};
  $remote_file="$remotedir/area_info";

  $host = $self->{REMOTE_ARCHIVE_INFO}->{HOST};
  $user = $self->{REMOTE_ARCHIVE_INFO}->{USER};
  $pw   = $self->{REMOTE_ARCHIVE_INFO}->{PW};

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


sub GetRemoteAINF{

  ## Usage: $remote_ainf=fetchRemoteAINF(satellite_number, sensor_number);

  # The name is a bit of a misnomer. This routine fetches the remote
  # ainf file from the Noaa server and then reads it from the local
  # machine. So the variable $remote_ainf actually refers to a file on
  # this machine.

  my $self=shift;
  my ($remote_ainf, @rainf);

  $remote_ainf=$self->FetchRemoteAINF($_[0], $_[1]);
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
       $remotedir, $remote_file, $local_file, $host, $user, $pw,
       $satellite, $remotefile, $localfile);

  $areafile=shift || croak "(param 1) Need AREAFILE\n";
  ($num,$sens) = $areafile =~ /^AREA(\d)(\d)(.*)/;
  $satellite=$OGoes::areanum2satnum{$num};
  $localdir=$self->{LOCALDIR};
  $remotedir=$self->{REMOTEDIR};

  $host = $self->{REMOTE_ARCHIVE_INFO}->{HOST};
  $user = $self->{REMOTE_ARCHIVE_INFO}->{USER};
  $pw   = $self->{REMOTE_ARCHIVE_INFO}->{PW};

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
  my $self=shift;
  $self->{_MIN_TIME_DIFF} =shift || 
  do { print "Taking default time difference of 45 minutes!\n";
    45};
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
  my $dir = ($self->{REMOTEDIR} = "goes$satnum/$sensordir");
}

#----------------------------------------------------------
# Construct the Path where the gridded files go
#----------------------------------------------------------


sub GriddingPath{
  my $self=shift;
  my $satnum= $self->{SAT};
  my $sensordir= $self->{SENSOR};
  my $dir = ($self->{GRIDDINGDIR} = "$VapUtil::GRIDDINGTOP/goes$satnum/$sensordir");
}

#----------------------------------------------------------
#
#----------------------------------------------------------


sub SatNum{
  my $self=shift;
  my $satnum;
  my $satellite = $self->{SATDESIG} || 
    croak "(param 1) Need Satellite location (west|east) or Satellite number (10|8)\n";
  if ($satellite =~ /(west|east)/i) {
    $satnum=$OGoes::satloc2satnum{$satellite}; 
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
    $dir = ($self->{SENSORDIR} = $OGoes::sensornum2name{ $self->{SENSORNUM} });
  }
  $dir
}


#----------------------------------------------------------
#
#----------------------------------------------------------


1;

