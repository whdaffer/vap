#
# $Id$

=pod



=head1 Winds.pm

 OO Perl Module for finding and manipulating Qscat/SeaWinds data files.

=head2 Methods:

=over 4

=item * new: 

        usage: $objref = Winds->new(FILTER = some_filter, 
                                  [ ENDTIME = 'yyyy/mm/dd/hh/mm/ss',
                                  STARTTIME = 'yyyy/mm/dd/hh/mm/ss']);


=over 6

=head2  Arguments (all in the `keyword => value' format)

        Required input: 

=item *  FILTER: This is the REGULAR EXPRESSION used to find wind
                 data. It is *NOT* a file glob! 

              To find only QuikSCAT files, use FILTER = 'QS',
              to file SeaWinds data only, use FILTER => 'SW ' and to
              find both, use FILTER => '(QS|SW)'


             It must be a REGULAR EXPRESSION, rather than a file
             glob, because what is searched is an array of filenames
             which I generate using perl internal routines. I'm not
             using the shell to find the files! If you put in a
             shell glob the results will most probably be
             unexpected.


       Optional input: 

=item * DELTA: the number of hours back from ENDTIME to go, default = 3.

=item * ENDTIME defaults to current time.

=item * STARTTIME defaults to endtime - DELTA hours.

=back

=back

  For use with the TS and ET module and any other module which needs
  to be able to find Qscat wind files or get their times.


=cut 

# Modification Log:
#
# $Log$
# Revision 1.2  2002/08/08 23:28:54  vapdev
# Removed some 'my' shadowing.
#
# Revision 1.1  2002/08/08 00:15:14  vapdev
# Initial Revision



package Winds;
use strict;
use Carp;

BEGIN  {

  croak "ENV variable VAP_OPS_TMPFILES is undefined!\n" 
    unless $ENV{VAP_OPS_TMPFILES};

  croak "ENV variable VAP_SFTWR_PERL is undefined!\n" 
    unless $ENV{VAP_SFTWR_PERL};
}

use lib "$ENV{VAP_SFTWR_PERL}";
use Cwd 'chdir', 'getcwd';
use Time::Local;
#use Net::FTP;
use File::Basename;
use VapUtil;
use VapError;
@Winds::ISA = qw/VapError/;


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

  
  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};
  bless $self, ref($class) || $class;

  $self->_croak("usage obj = Winds->new(FILTER=>'filter' \n[,STARTTIME => 'yyy/mm/dd/hh/mm/ss', \nENDTIME=>'yyy/mm/dd/hh/mm/ss']);", 
		"Winds: initialization error!") unless $self->{FILTER};

  return $self;
}

=pod

=over 4

=item * @files_in_time_range = getWindFiles;

        Returns array of files within the timerange defined by STARTTIME/ENDTIME 
        matching input FILTER

=back

=cut


#---------------------------------------------
#
#---------------------------------------------
sub getWindFiles{

  my $self=shift;
  my ($startime, $endtime, $path, @files, $st, $et, @in_range);

  ($path,@files)=getFileList();
  $self->_croak("ERROR: Can't get file list!\n",
		"Winds: ERROR from getFileList") unless $#files> -1;
  for (@files) {
    ($st,$et) = getFileTimes($_);
    push @in_range, $_ unless ($st > $self->{ENDTIME} || $et < $self->{STARTTIME});
  }  
  $self->{FILES_IN_RANGE} = \@in_range;
  @in_range;
}

=pod

=over 4

=item * ($dir, @files) = getFileList()

        Returns an array of which the first element is the directory
        where the files given in the remainder of the array live.

=back

=cut


#---------------------------------------------
#
#---------------------------------------------
sub getFileList{
  my $self=shift;
  my $dir=$self->{PATH};
  opendir DIR, "$dir" || 
    $self->_croak("Can't open directory $dir\n",
		  "Winds: Error opening $dir");
  my $regex = $self->{FILTER}."\\d+\\.S\\d+\\.E\\d+";
  my @files=grep /^$regex/, readdir(DIR);
  closedir DIR;
  @files = sort @files;
  ($dir,@files);
}

=pod

=over 4

=item * (starttime, endtime ) = getFileTimes(file)

        Returns the start and end time (as determined by parsing the
        filename) of the input file, or (undef,undef) if parameter is
        missing or non-existent or some other problem occured.

        It's assumed the format of the filename is
        (QS|SW)yyyymmdd.Shhmm.Ehhmm.

=back

=cut

#---------------------------------------------
#
#---------------------------------------------

sub getFileTimes{
  my $self=shift;

  my ($name,$path,$year,$month,$day,$hour,
	$min,$start,$end,$starttime,$endtime);

  my $file=shift;
  if (!$file) {
    print "Param 1 </path/to/file/QSYYYYMMDD.SHHMM.EHHMM> is REQUIRED!\n";
    return ($starttime,$endtime);
  }
  if (! -e $file) {
    print "$file doesn't exist!\n";
    return ($starttime,$endtime);
  }    

  ($name,$path) = fileparse($file);
  ($year,$month,$day,$start,$end) = 
      $name =~ /^[QS][SW](\d{4,4})(\d{2,2})(\d{2,2})\.S(\d+)\.E(\d+)$/;

  if (! ($year && $month && $day && $start && $end) ) {
    print "Misformated filename for $file!\n"; 
    return ($starttime,$endtime);
  }

  ($hour,$min) = $start =~ /(\d{2,2})(\d{2,2})/;
  $starttime=timegm(0, $min, $hour, $day, $month-1, $year-1900);
  ($hour,$min) = $end =~ /(\d{2,2})(\d{2,2})/;  
  $endtime=timegm(0, $min, $hour, $day, $month-1, $year-1900);
  $endtime += 24*3600 if $endtime < $starttime;
  ($starttime, $endtime);
}


=pod

=over 4

=item * ($path, $time, @files) = 
               FindClosestInTimeAndDistance(lon,lat,time 
                                      [,time_delta, tolerance])

        Returns the `files' located in directory `path' which bracket
        those files closest in time and distance to the location
        `lon',`lat'. The time range searched for is `time' +/-
        `time_delta' (which defaults to 2 hours) and the distance is
        `tolerance' degrees (default=5)

        This routine constructs an IDL batch file, then calls IDL
        with this batchfile as the argument. The batchfile invokes the
        IDL file `nearto.pro' which produces an outputfile which this
        routine then parses for the returned information.

        The `$time' reported on output is either the time of closest
        approach (if only one file satistifes closeness criteria, or
        it's the average of the times of the two bracketing files, if
        there's more than one file satifying the 'closeness' criteria.

        It returns `undef's if there are errors.



=back

=cut

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

  $lon=shift; 
  $lat=shift; 
  $time=shift;
  $time_delta=shift || 2;
  $tolerance=shift || 5;
  $delflag = shift;

  if (! ($lon && $lat && $time) ){
    print "parameters lon,lat and time are REQUIRED!\n";
    return (undef, undef, undef);
  }
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
  $self->_croak("Error running IDL $idltmpfile\n",
		"ERROR batch IDL") if $r != 0;

  unlink $idltmpfile || warn "Couldn't unlink($idltmpfile)\n";
  croak "Can't find $ofile!\n" if (! -e $ofile) ;

  open OFILE, "<$ofile" || 
    $self->_croak("Can't reopen $ofile\n",
		  "Winds: OPEN ERROR on $ofile");
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

      # Only one file. Find the file immediately preceeding and
      # following it.

    @files=$self->Bracket($path, $keys[0]);
    @ret=($path, $hash{$keys[0]}{ROWTIME}, @files);

  } elsif ($#keys > 0) {

      # more than one!

      # Go through and get the GMT time for each file, this time is
      # the `rowtime' at closest approach to `lat',`lon'.

    foreach my $key (keys %hash){
      ($year,$month,$day,$hour,$min)=split "/", $hash{$key}{ROWTIME};
      push @times, 
	join("|", $key,timegm( 0, $min, $hour, $day, $month-1, $year-1900));
    }

    @times = sort @times;

#     foreach my $f (@times){
#       my ($file,$t)=split(/\|/, $f);
#       push @files, $file;
#       push @t, $t;
#     }

    my ($file, $t) = split(/\|/,$times[0]);
    push @files,$file;
    ($file, $t) = split(/\|/,$times[$#times]);
    push @files,$file;

    @files=$self->Bracket($path, @files);
    $time=systime2idltime(($t[0]+$t[$#t])/2);
    @ret=($path, $time, @files);

  }
  @ret;
}

=pod

=over 4

=item * @files = Bracket($path,$firstfile [,$lastfile]);

        Returns an array containing the file in directory `$path'
        prior, in time, to `$firstfile' and after `$lastfile.'
        `$lastfile' defaults to `$firstfile.'

        Returns (undef, undef) on error.

=back

=cut


#---------------------------------------------
#
#---------------------------------------------
sub Bracket{

  my $self=shift;
  my $path = shift;
  my $first = shift;
  my $last = shift || $first;

  if (! ($path && $first)){
    print "usage: (before_first,after_last) = Bracket(path,first [,last]);\n";
    return (undef, undef);
  }

  # given the input path and files, find the one before the first in
  # the list and the one after the last.
  # requires that getFileList returns the files sorted.

  my @keepfiles = ();

  my ($name, @files);
  ($path, @files) = $self->getFileList($path);

  if (@files) {
    my $i=-1;
    for (@files) {
      if (/$first/ && $i>=0){
	push @keepfiles, $files[$i] ;
      }
      if (/$last/ && $i <= ($#files-2)) {
	push @keepfiles, $files[$i+2];
	last;
      }
      $i++;
    }
  }
  @keepfiles = sort @keepfiles;
}


=pod

=over 4

=item * $file=getBefore( $path, $first, $last);

        Returns the file in `$path' immediately prior to `$first'

        This is just a wrapper to Bracket

=back

=cut

#---------------------------------------------
#
#---------------------------------------------
sub getBefore{
  my $self=shift;
  my @files = $self->BrackFile(@_);
  my $ret=$files[0];
}


=pod

=over 4

=item * $after = getAfter($path,$first[,$last])

        Returns the file in `$path' immediately after `$last' (which
        may default to `$first'

        This is just a wrapper to Bracket()

=back

=cut

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

=pod 

=head2  Author: William H. Daffer


=cut 
