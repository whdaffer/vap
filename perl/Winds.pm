#
# $Id$
#
# Winds.pm
#
# OO Perl Module for finding and manipulating Qscat/SeaWinds data files.
#
#  Methods:
#
#     `new': 
#
#        usage: $objref = Winds->new(FILTER = some_filter, 
#                                  [ ENDTIME = 'yyyy/mm/dd/hh/mm/ss',
#                                  STARTTIME = 'yyyy/mm/dd/hh/mm/ss']);
#
#        Required input: 
#
#              FILTER. This is the REGULAR EXPRESSION used to find wind
#              data. To find only QuikSCAT files, use FILTER = 'QS',
#              to file SeaWinds data only, use FILTER => 'SW ' and to
#              find both, use FILTER => '(QS|SW)'
#
#
#              It must be a REGULAR EXPRESSION, rather than a file
#              glob, because what is searched is an array of filenames
#              which I generate using perl internal routines. I'm not
#              using the shell to find the files! If you put in a
#              shell glob the results will most probably be
#              unexpected.
#
#
#        Optional input: 
#
#              ENDTIME defaults to current time.
#              STARTTIME defaults to endtime - 3 hours.
#
#  For use with the TS and ET module and any other module which needs
#  to be able to find Qscat wind files or get their times.
#
# Author: William H. Daffer
#
# Modification Log:
#
# $Log$
# Revision 1.1  2002/08/08 00:15:14  vapdev
# Initial Revision
#
#
#
package Winds;
use strict;
use lib "$ENV{VAP_SFTWR_PERL}";
use Cwd 'chdir', 'getcwd';
use Time::Local;
use Net::FTP;
use Carp;
use File::Basename;
use VapUtil;


#---------------------------------------------
# Constructor.
# 
#---------------------------------------------
sub new {
  my $class = shift;
  croak "ENV variable VAP_TMPFILES is undefined!\n" 
    unless $ENV{VAP_OPS_TMPFILES};

  my $self= {STARTTIME => VapUtil::systime2idltime($^T-3*3600),
	     ENDTIME => VapUtil::systime2idltime($^T),
	     PATH => $VapUtil::VAP_DATA_TOP,
	     @_};

  croak "usage obj = Winds->new(FILTER=>'filter' \n[,STARTTIME => 'yyy/mm/dd/hh/mm/ss', \nENDTIME=>'yyy/mm/dd/hh/mm/ss']);" unless exists($self->{FILTER});

  
  return bless $self, ref($class) || $class;
}


#---------------------------------------------
#
#---------------------------------------------
sub getWindFiles{

  my $self=shift;
  my ($startime, $endtime, $path, @files, $st, $et, @in_range);

  ($path,@files)=getFileList();
  croak "Can't get file list!\n" unless $#files> -1;
  for (@files) {
    ($st,$et) = getFileTimes($_);
    push @in_range, $_ unless ($st > $self->{ENDTIME} || $et < $self->{STARTTIME});
  }  
  $self->{FILES_IN_RANGE} = \@in_range;
  1;
}


#---------------------------------------------
#
#---------------------------------------------
sub getFileList{
  my $self=shift;
  my $dir=$self->{PATH};
  opendir DIR, "$dir" || croak "Can't open directory $dir\n";
  my $regex = $self->{FILTER}."\\d+\\.S\\d+\\.E\\d+";
  my @files=grep /^$regex/, readdir(DIR);
  closedir DIR;
  @files = sort @files;
  ($dir,@files);
}

#---------------------------------------------
#
#---------------------------------------------

sub getFileTimes{
  my $self=shift;

  my ($name,$path,$year,$month,$day,$hour,
	$min,$start,$end,$starttime,$endtime);

  my $file=shift || 
      croak "Param 1 </path/to/file/QSYYYYMMDD.SHHMM.EHHMM> is REQUIRED!\n";
  ($name,$path) = fileparse($file);
  ($year,$month,$day,$start,$end) = 
      $name =~ /^[QS][SW](\d{4,4})(\d{2,2})(\d{2,2})\.S(\d+)\.E(\d+)$/;
  
  ($hour,$min) = $start =~ /(\d{2,2})(\d{2,2})/;
  $starttime=timegm(0, $min, $hour, $day, $month-1, $year-1900);
  ($hour,$min) = $end =~ /(\d{2,2})(\d{2,2})/;  
  $endtime=timegm(0, $min, $hour, $day, $month-1, $year-1900);
  $endtime += 24*3600 if $endtime < $starttime;
  ($starttime, $endtime);
}


#---------------------------------------------
#
#---------------------------------------------
sub FindClosestInTimeAndDistance{
  my $self=shift;
  my ($lon, $lat, $time, $starttime, $endtime, $tolerance, $ofile, $delflag,
	$randomtag, $idltmpfile, $t0, $time_delta, 
	$year, $month, $day, $hour, $min, $k, $kk, $v, $t, @t, 
	$name, $value, $path);
  my %hash = ();

  $lon=shift || croak "Param 1 <LON> is required\n";
  $lat=shift || croak "Param 2 <LAT> is required\n";
  $time=shift || croak "Param 3 <TIME> is required\n";
  $time_delta=shift || 2;
  $tolerance=shift || 5;
  $delflag = shift;
  $randomtag= makeRandomTag();
  $ofile= shift || $ENV{VAP_OPS_TMPFILES}."/nearto.$randomtag.dat";
  $idltmpfile=$ENV{VAP_OPS_TMPFILES}."/runnearto.$randomtag.pro";

  ($year,$month,$day,$hour,$min) = split /\//, $time;
  $t0=timegm(0,$min,$hour,$day,$month-1,$year-1900);
  $starttime = VapUtil::systime2idltime($t0-$time_delta*3600);
  $endtime   = VapUtil::systime2idltime($t0+$time_delta*3600);

  open IDLTMPFILE, ">$idltmpfile" || die "Can't open $idltmpfile\n";
  print IDLTMPFILE "lon=$lon\n";
  print IDLTMPFILE "lat = $lat\n";
  print IDLTMPFILE "starttime = \'$starttime\'\n";
  print IDLTMPFILE "endtime = \'$endtime\'\n";
  print IDLTMPFILE "tolerance = $tolerance\n";
  print IDLTMPFILE "ofile=\'$ofile\'\n";
  print IDLTMPFILE "ret=nearto(lon,lat,starttime,endtime,ofile=ofile)\n";
  print IDLTMPFILE "exit\n";
  close IDLTMPFILE;

  my $r=system( "$VapUtil::IDLEXE $idltmpfile")/256;
  croak "Bad return from system( $VapUtil::IDLEXE $idltmpfile)\n" if $r != 0;

  unlink $idltmpfile || warn "Couldn't unlink($idltmpfile)\n";
  croak "Can't find $ofile!\n" if (! -e $ofile) ;

  open OFILE, "<$ofile" || croak"Can't reopen $ofile\n";
  my $first=1;

  while (<OFILE>){
    chomp;
    last if /^-+\s+ERROR\s+-+.*$/;
    my ($k,$v) = split /:/;
    $k =~ s/\s+//g;
    $v =~ s/\s+//g;

      # Gonna use a little trick here. The format of this file is:


#      FILE    : /disk5/winds/qscat/Rnoaa/QS20000911.S1139.E1340
#      ROWTIME : 2000/09/11/13/37
#      LOCATION:  234.89    30.91
#      DISTANCE:    0.13
#      INSWATH:        1
#      ------------------------ 
#      FILE    : /disk5/winds/qscat/Rnoaa/QS20000911.S1300.E1520
#      ROWTIME : 2000/09/11/13/37
#      LOCATION:  234.91    30.90
#      DISTANCE:    0.13
#      INSWATH:        1
#      ------------------------ 


    # This code will split on the ':' and then assign $$k = $v.
    # but $k will be one of the keywords 
    # FILE, ROWTIME, LOCATION, DISTANCE OR INSWATH so the effect is to assign
    # $FILE = $v or $ROWTIME = $v...
    # these are then put into the hash using $FILE as the key.
    #
    # Pretty slick, eh?

    my ($FILE, $ROWTIME, $LOCATION, $DISTANCE, $INSWATH, $path, $suffix);
    if (!$v) {
      if (!$first){
	$hash{$FILE}{ROWTIME}  = $ROWTIME;
	$hash{$FILE}{LOCATION} = $LOCATION;
	$hash{$FILE}{DISTANCE} = $DISTANCE;
	$hash{$FILE}{INSWATH}  = $INSWATH;
      }
    } else {
      (($v,$path, $suffix) = fileparse($v)) if /^FILE.*:.*/;
      $$k = $v;
    }
    $first=0;
  }
  close OFILE;

  unlink $ofile if $delflag;


  my @keys=keys %hash;
  my (@files, @ret, @times);
  if ($#keys==0) {
    @files=$self->Bracket($path, $keys[0]);
    @ret=($path, $hash{$keys[0]}{ROWTIME}, @files);
  } elsif ($#keys > 0) {

    foreach my $key (keys %hash){
      ($year,$month,$day,$hour,$min)=split "/", $hash{$key}{ROWTIME};
      push @times, 
	join("|", $key,timegm( 0, $min, $hour, $day, $month-1, $year-1900));
    }

    @times = sort @times;

    foreach my $f (@times){
      my ($file,$t)=split(/\|/, $f);
      push @files, $file;
      push @t, $t;
    }

    @files=$self->Bracket($path, @files);
    $time=systime2idltime(($t[0]+$t[$#t])/2);
    @ret=($path, $time, @files);

  }
  @ret;
}


#---------------------------------------------
#
#---------------------------------------------
sub Bracket{
  my $self=shift;
  my $first = shift || croak "Param 2, The FIRST file, I need it!\n";
  my $last = shift || $first;

  # given the input path and files, find the one before the first in
  # the list and the one after the last.
  # requires that getFileList returns the files sorted.

  my $path = shift || croak "Param 1, the path, I need it!\n";
  my @keepfiles = @_;

  my( $name, @files);
  ($path, @files) = $self->getFileList($path);

  if (@files) {
    my $i=-1;
    for (@files) {
      if (/$first/ && $i){
	push @keepfiles, $files[$i] ;
      }
      if (/$last/ && $i+2 <= $#files) {
	push @keepfiles, $files[$i+2];
	last;
      }
      $i++;
    }
  }
  @keepfiles = sort @keepfiles;
}


#---------------------------------------------
#
#---------------------------------------------
sub getBefore{
  my $self=shift;
  my @files = $self->BrackFile(@_);
  my $ret=$files[0];
}


#---------------------------------------------
#
#---------------------------------------------
sub getAfter{
  my $self=shift;
  my @files = $self->BrackFile(@_);
  my $ret=$files[1];
}


#---------------------------------------------
#
#---------------------------------------------
# sub bydate{
#   @aa=split(/\|/, $a);
#   @bb=split(/\|/, $b);
#   $bb[1] <=> $aa[1];
#}
1;

