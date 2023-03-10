=head1 NAME

  OGms5.pm -- OO replacement for Gms5.pm

=head2 SYNOPSIS

  ogms = OGms5->new([TYPE => 'type',
                    "MACHINE=>"machine",
                    REMOTE_TOPDIR => "/dir/on/remote/machine",
                    LOCAL_TOPDIR => "/dir/on/local/machine",
                    DATETIME=>"yyyy/mm/dd/hh/mm/ss",
                    DELTA=>f.g]);

=head2 KEYS

=over 4

=item * TYPE: The type of sensor IR{1,2,3} or 'vis'. Currently it's ir1.

=item * MACHINE: The internet address of the machine with the GMS5
                 data. (default is read from the file
                 $VAP_LIBARY/gms5_archive, currently it's
                 rsd.gsfc.nasa.gov)


=item * REMOTE_TOPDIR: Directory on remote machine from which the GMS5
                 data archive descends. All motion on the remote
                 machine is relative to this base directory. Default
                 is in $VAP_LIBARY/gms5_archive_oo.


=item * LOCAL_TOPDIR: the local equivalent of REMOTE_TOPDIR. Default
                 is in the environmental variable VAP_GMS_TOPDIR but
                 this field is set in the gms5_archive_oo file.

=itme * DATETIME: The time around which to look for data. The objects
                 considers those files which are within DELTA of
                 DATETIME. If ABSFLAG == 0 then only files in the
                 range of DATETIME - DELTA are considered, otherwise
                 the range DATETIME +/- DELTA is searched, i.e. the
                 filetime returned may be later than the DATETIME

=item * DELTA: the `timerange' in which we look for data. Files must
                 be closer to DATETIME than this number of hours to
                 count as 'found.'  If ABSFLAG == 1, the difference
                 used in the test is DIFF=abs(DATETIME-DELTA),
                 otherwise it's DIFF=DATETIME-DELTA and 
                 DIFF must be >0.

                 Default = 6.0. May be entered as a float.


=item * INTERESTING_GMS5_PRODUCTS: A reference to an array listing the
                 items (directories, really) currently thought
                 interesting to check and/or retrieve. This is
                 defaulted to [ir1, grid, grida] in
                 gms5_archive_oo. For each datetime, a file for each
                 of these directories will be checked and/or fetched,
                 depending on the method and various routines will
                 fail if one of the directories is missing the file
                 for that datetime, so have a care what you put into
                 this array. The two grid directores (grid and grida)
                 contain a file used in navigating the GMS5 image, but
                 only one is needed for any particular image. The
                 files in the `grid' directory allow one to navigate
                 the data if your area of interest is between 80
                 degrees East longitude, 160 degrees West. Those in
                 the `grida' covers 70 East to 150 West.

                 See

                   ftp://rsd.gsfc.nasa.gov/pub/Weather/GMS-5/hdf/grid/README

                 for a full description.

                 It would be nice to include logic to differentiate 
                 between the two, so that we only had to check the one or
                 the other, but I can't see an easy way to do it. 

                 This state of affairs has the unfortunate consequence
                 that a job may fail because, for example, the
                 relevant `grida' file isn't in the remote archive
                 even though the job only requires the `grid' file.

=back

The directory layout on the local machine must match that on the
remote matchine (up to the the leading segment of the full path, which
is differentiated by the values of the internal hash keys
REMOTE_TOPDIR and LOCAL_TOPDIR, which itself depends on the
environmental variable VAP_GMS_TOP. 

=cut

package OGms5;
use strict;
use vars qw/$gms5_defs/;
use Net::FTP;
use Cwd 'chdir', 'getcwd';
use Carp;
use Time::Local;

BEGIN {

  croak "ENV var VAP_LIBRARY is undefined!\n" 
    unless $ENV{'VAP_LIBRARY'};

  croak "ENV var VAP_GMS_TOPDIR is undefined\n" unless 
    $ENV{'VAP_GMS_TOP'};

  croak "ENV var VAP_SFTWR_PERL is undefined\n" unless 
    $ENV{VAP_SFTWR_PERL};

}

use lib $ENV{VAP_SFTWR_PERL};
use lib $ENV{VAP_LIBRARY};
require "gms5_archive_oo" or croak "Can't `require' gms5_archive_oo!\n";
use VapUtil;
use VapError;


#=============================================================
#
# new
#
#=============================================================
sub new {
  my $class = shift;
  my $self={};
  my $h = {@_};

  while (my ($k,$v) = each %{$gms5_defs}){
    $self->{$k} = $v;
  }

  while (my ($k,$v) = each %{$h}){
    $self->{$k} = $v;
  }
  undef $gms5_defs;
  undef $h;
  $self->{TYPE} = "ir1" unless $self->{TYPE};
  $self->{TYPE} = lc $self->{TYPE};
  $self->{STARTDIR}=getcwd();
  $self->{USER}=$ENV{'USER'};
  my $host = $ENV{HOSTNAME} || `hostname`;
  $self->{LOCAL_HOST}=$host . ".jpl.nasa.gov";
  $self->{DELTA} = 6 unless $self->{DELTA};
  bless  $self, ref($class) || $class;
  $self->{ERROROBJ}->_croak( "Need KEY INTERESTING_GMS5_PRODUCTS\n",
		 "$0->new ERROR!")
    unless exists $self->{INTERESTING_GMS5_PRODUCTS};

  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};
  my $remote_host = $self->{REMOTE_HOST};
  $self->{FTPOBJ} = Net::FTP->new("$remote_host") || 
    $self->{ERROROBJ}->_croak("$0:Error creating FTP object for $remote_host!\n",
		  "Error Initializing FTP object");

  my $user = "anonymous";
  my $password = $self->{USER}."\@".$self->{LOCAL_HOST};
  $self->{FTPOBJ}->login($user, $password) ||
    $self->{ERROROBJ}->_croak("$0:Error in FTP login",
		  "$0:Error Logging on to $remote_host using\n" . 
		  "$user and $password!\n");
  if ($self->GetAllFileLists()) {
    return $self;
  } else {
    return undef;
  }
}

#=============================================================
#
#
#
#=============================================================

sub GetAllFileLists{
  my $self=shift;
  $self->{GETALLFILELISTS}->{LATESTGET} = time();
  for (@{$self->{INTERESTING_GMS5_PRODUCTS}}){
    $self->GetListing( "$_" ) or 
      $self->{ERROROBJ}->_croak("Can't get listing for $_\n",
		    "$0:Listing Error!");
  }
  1;
}

=pod

head1 GetListing

head2 Synopsis: GetListing(directory);

  `directory' is relative to the remote top directory as specified to
  the OGms5 constructure or defaulted in $VAP_LIBRARY/gms5_archive_oo

  This FTPs to the directory input in the call on the remote machine
  and does a listing, saving the output to the that directories mirror
  on the local machine, in the file archive.filelist.


=cut


#=============================================================
#
#
#
#=============================================================

sub GetListing{
  my $self=shift;
  my $dir=shift or croak "Usage: obj->get(directory)\n";
  my $glob = shift || "*.Z";
  my $savedir = getcwd();
  chdir $self->{LOCAL_TOPDIR} or 
    $self->{ERROROBJ}->_croak("Can't CD to ".$self->{LOCAL_TOPDIR}."\n",
		  "$0:CD error");
  chdir $dir;
  my $remote_dir = $self->{REMOTE_TOPDIR}. "/hdf/$dir";
  $remote_dir .= "/4km" if ($dir =~ /^ir|vis/i);


  my $ftp = $self->{FTPOBJ};
  $ftp->cwd($remote_dir) or  $self->{ERROROBJ}->_croak("Can't CD to $remote_dir\n", 
					   "$0:CD error");
  my @list = $ftp->ls($glob);
  @list = join("\n",@list);
  open FILE ,">archive.filelist";
  print FILE @list;
  close FILE;
  chdir $savedir;
  1;
}

=pod


head1 GetIntersection

head2 SYNOPSIS @datetime = GetIntersection(type);

  `type' is the sensor (i.e. 'ir1','ir2','ir3', or 'vis') and it must
  be in the array $self->{INTERESTING_GMS5_PRODUCTS}'

  It returns the set of datetimes (strings of the form yymmddhhmm)
  which have files in the two grid directory (grid and grida) as well
  as a file for the specified type.

=cut


#=============================================================
#
#
#
#=============================================================


sub GetIntersection{
  my $self=shift;
  my $type=shift || $self->{TYPE} || 
    $self->{ERROROBJ}->_croak(
               ["Usage: obj->GetIntersection([type])",
		"Can't find the type you want!\n"],
		  "OGms5:GetIntersetion, Usage error");

  my @interesting = @{$self->{INTERESTING_GMS5_PRODUCTS}};
  if (!grep(/$type/i,@interesting)) {
    my $msg = "$type is not in the array of INTERESTING GMS5 products!";
    $msg .= "Perhaps you should have a look at that variable!\n";
    $msg .= "See $VAP_LIBRARY/gms5_archive_oo\n";
    my $subject="$type NOT IN ". join(" ",@interesting);
    $self->{ERROROBJ}->_croak($msg,$subject);
  }
  my $savedir=getcwd();

  chdir $self->{LOCAL_TOPDIR} || 
    $self->{ERROROBJ}->_croak("Can't CD to ".$self->{LOCAL_TOPDIR}."\n",
		  "CD Error");
  chdir $type or 
    $self->{ERROROBJ}->_croak("Can't CD to ".$self->{LOCAL_TOPDIR}."$type\n",
		  "CD Error");
  open ARCHIVE, "<archive.filelist";
  my @files= <ARCHIVE>;
  close ARCHIVE;
  chdir "..";

  my %filecnt=();
  my $dircnt=1;
  foreach my $file (@files) {
    my $tmp=substr($file,0,10);
    $filecnt{$tmp}++;
  }

  my @dirs=qw(grid grida);

  foreach my $dir (@dirs) {
    chdir $dir || $self->{ERROROBJ}->_croak ("Can't CD to $dir\n",
				 "CD Error!\n");;
    open ARCHIVE, "<archive.filelist";
    @files= <ARCHIVE>;
    close ARCHIVE;
    chdir "..";
    $dircnt++;
    foreach my $file (@files) {
      my $tmp=substr($file,0,10);
      $filecnt{$tmp}++;
    }
  }

  my @datetimes;
  foreach my $datetime ( keys %filecnt ){
    push @datetimes, $datetime if ($filecnt{$datetime} == $dircnt);
  }  

  chdir $savedir;
  $self->{INTERSECTION} = \@datetimes;
  @datetimes;
}

=pod

=head1 ===========================================================

=head2 GetAll

=head2 Usage: GetAll([datetime])


  Retrieves the file specified by datetime for each of the
  _interesting products_, i.e. the directories listed in
  $self->{INTERESTING_GMS5_PRODUCTS}, unless the file is already here.

=cut


#=============================================================
#
#
#
#=============================================================

sub GetAll{
  my $self=shift;
  my $datetime = shift || $self->{DATETIME};
  $self->{DATETIME} = $datetime;
  $self->{ERROROBJ}->_croak("Datetime hasn't been set yet!\n",
		"No Datetime set in GetAll\n") unless $self->{DATETIME};

  if ($self->CheckAll()) {
    for (@{$self->{INTERESTING_GMS5_PRODUCTS}}){
      $self->Get("$_","$datetime") or 
	$self->{ERROROBJ}->_croak("Can't get $datetime in $_\n",
		      "$0:Retrieve Error");
    }
  }
  1;
}


#=============================================================
#
#
#
#=============================================================

sub Get{
  my $self=shift;
  my $dir=shift;
  my $datetime=shift || $self->{DATETIME};
  $self->{DATETIME} = $datetime;  
  $dir = $self->{REMOTE_TOPDIR}."/$dir";
  my $savedir = getcwd();
  chdir $dir ||
    $self->{ERROROBJ}->_croak("Can't CD to $dir\n",
		  "$0:CD Error");
    # The assumption here is that the file is named $datetime.hdf.Z
  my $ftp=$self->{FTPOBJ};
  $ftp->binary("$datetime.hdf.Z") or 
    $self->{ERROROBJ}->_croak("Can't get $datetime.hdf.Z\n",
		  "$0:Ftp Fetch Error!");
  chdir $savedir;
  1;
}


#=============================================================
#
#
#
#=============================================================

sub CheckAll{
  my $self=shift;
  my $datetime=shift || $self->{DATETIME};
  $self->{DATETIME} = $datetime;  
  return 1 if $self->CheckLocal;
  my $ftp=$self->{FTPOBJ};
  foreach (@{$self->{INTERESTING_GMS5_PRODUCTS}}){
    $self->{ERROROBJ}->_croak("$datetime not in $_\n",
		  "$0: Intersection problem!") 
      unless $ftp->list("$datetime.hdf.Z");
  }
  1;
}


#=============================================================
#
#
#
#=============================================================

sub CheckLocal{
  my $self=shift;
  my $datetime=shift || $self->{DATETIME};
  $self->{DATETIME} = $datetime;
  return scalar( grep(/$datetime/,
		      defined($self->{DATETIMES})?
		      @{$self->{DATETIMES}}: 
		      $self->GetIntersection($datetime))) != 0;
}


#============================================
#
#GetClosest(['yyyy/mm/dd/hh/mm'])
#
# Gets the set of files closest to the input time
#
#============================================

=pod

=head1 ===================================================

=head2 GetClosest

=head2 Usage: GetClosest([yyyy/mm/dd/hh/mm])


  Retrieves the time of the file closest to datetime.  If datatime
  isn't supplied in the call, $self->{DATETIME} is used. (The whole
  process fails if this key hasn't been set). If it is, this time is
  used and $self->{DATETIME} is replaced.


=cut


sub GetClosest{
  my $self=shift ;
  my $idltime=shift ||  $self->{DATETIME};
  $self->{DATETIME} = $idltime;
  $self->{ERROROBJ}->_croak("$0:DATETIME hasn't been set yet!\n",
		    "$0:GetClosest: no DATETIME") unless $self->{DATETIME};
  my $absflag=shift || 0;
  my $time=idltime2systime($idltime);
  my $tt=time();
  if (! exists($self->{GETALLFILELISTS}->{LASTGET}) || 
      ($time - $self->{GETALLFILELISTS}->{LASTGET}) > $self->{DELTA} ) {
    $self->GetAllFileLists;
  }
  my @datetimes = $self->GetIntersection();
  if (!@datetimes){
    $self->{ERROROBJ}->Report(["OGms5: Intersection problem",
			       "Probably missing grid/grida files!\n"],
			      'INFO');
    return(undef, undef);
  }

  my ($diff,$datetime,$mindiff);
  if (@datetimes) {
    $mindiff=1.e10;
    for (@datetimes) {
      my $gmstime=$self->DateTime2SysTime($_);
      $diff=$absflag? abs($time-$gmstime): ($time-$gmstime);
      if ($diff > 0 && $diff < $mindiff && $diff < $self->{DELTA} ) {
	$mindiff = $diff;
	$datetime=$_;
      }
    }
  }
  ($datetime, $mindiff);
}


#=============================================================
#
#
#
#=============================================================

sub DateTime2SysTime{
  my $self=shift;
  my $datetime = shift || $self->{DATETIME};
  my ($year,$month,$day,$hour,$min) = 
    ($datetime =~ /(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})/);
  $year += 2000;
  my $timegm=timegm( 0, $min, $hour, $day, $month-1, $year);
}


#=============================================================
#
#
#
#=============================================================

DESTROY { 
  my $self=shift;
  $self->{FTPOBJ}->close();
}

1;

