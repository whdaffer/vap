=head1 NAME

  Animate.pm -- Object to handle the animation of QuikSCAT/SeaWinds data
                (Replaces auto_movie.pl)

=head2 SYNOPSIS

  $anim_obj = Animate->new( REGION => region,
                            TIME => time,
                            DELTA => delta);


=head2 KEYS

=over 4 

=item * REGION: The 'region'. Must be one of the regions
                defined in the IDL readible file
                $VAP_LIBRARY/auto_movie_defs.dat.

                No default!

=item * TIME: The end time of the time range in which to gather data
                 for interpolation and animation. Default = start time
                 of script run

=item * DELTA: The number of hours to go backward from TIME. If this
                 isn't passed in, it is defaulted in the IDL script
                 that this object calls.


=back
=cut
package Animate;
use strict;
use Carp;
use Cwd;
use vars qw/$VERSION $usage/;

# Make sure we can get the Perl code we need.
BEGIN{
  $VERSION = "0.9";
  croak "ENV var VAP_LIBRARY is undefined\n" 
    unless $ENV{VAP_LIBRARY};
  croak "ENV var VAP_SFTWR_PERL is undefined\n" 
    unless $ENV{VAP_SFTWR_PERL};
  croak "ENV var VAP_OPS_ANIM is undefined\n" 
    unless $ENV{VAP_OPS_ANIM};
  $usage = "$0: obj=Animate->new(REGION=>region [,TIME=>time,DELTA=>DELTA])\n";
}
use VapUtil qw(&auto_movie_defs);
use VapError;

#==================================================
#
#==================================================

sub new {
  my $class = shift;
  my $self={@_};
  croak $usage unless exists $self->{REGION};
  $self->{DEFAULTS_FILE} = $ENV{VAP_LIBRARY}."/auto_movie_defs.dat";
  bless $self, ref($class) || $class;

  # Set the Error reporting object.
  $self->{ERROROBJ} = VapError->new() or
    $self->_croak "Error creating VapError object!\n";


  # Read the defaults file
  $self->ReadDefsFile;
  # Make sure this is a valid region

  $self->_croak("Invalid REGION! ". $self->{REGION} .
		" valid regions are \n". 
		join("\n",@{$self->{ROIS}} ). "\n",
		"$0:INVALID REGION")
    unless grep($self->{REGION},@{$self->{ROIS}});


  # Set time and working dir, make sure the latter exists and that we
  # can CD to it.

  $self->{TIME} = shift || systime2idltime(GetNow());
  $self->{STARTTIME} = $^T;

  $self->{WORKING_DIR} = $ENV{VAP_OPS_ANIM}."/". lc($self->{REGIION});
  my $working_dir = $self->{WORKING_DIR};
  $self->_croak("$0:Can't find working directory $working_dir\n",
		"$0:NONEXISTENT WORKING DIR")
    unless (-e $working_dir);
  chdir $working_dir or $self->_croak("$0:Can't CD to $working_dir",
				      "$0: CD error!");

  $self->{TMPFILE_DIR} = $ENV{VAP_OPS_TMPFILES};
  $self->_croak("TMPFILE directory doesn't exist!\n",
		"$0: NONEXISTENT TMPFILE DIRECTORY!")
    unless (-e $self->{TMPFILE_DIR});
  
  # Return new object
  return $self;
}


#======================================================================
# CreateLockFile:
#  Create the lock file. This file has the PID of the process that
#  created it as a check against another job picking up the wrong file
# ======================================================================
sub CreateLockFile {

  my $self=shift;
  my $dir=$self->{TMPFILE_DIR};
  my $user = $self->{USER};
  my $roi=$self->{REGION};
  my $froi = lc $roi;
  my $file="$dir/$user.auto_movie_$froi.$$.lock";
  open FILE, ">$file" or 
    $self->_croak("$0: Error opening $file: $!\n",
		  "FILE OPEN ERROR");
  print FILE "$$\n";
  close FILE;
  $self->{LOCKFILE} = $file;
  1;
}


#======================================================================
# CreateTmpFile:
#   Create the tmp IDL .pro file that will be run to create the movie.
#======================================================================
sub CreateTmpFile{

  my $self=shift;
  my $user = $self->{USER};
  my $roi=$self->{REGION};
  my $froi = lc $roi;
  my $file="$user.auto_movie_$froi.$$.pro";
  open FILE, ">$file" or 
    $self->_croak("$0: Error opening $file: $!\n",
		  "FILE OPEN ERROR");
  my $date_time = $self->{TIME};
  my $exe_str = "auto_movie,\'$date_time\',roi=\'$roi\'";
  $exe_str .= ",pid=$$, lockfile = '". $self->{LOCKFILE}. "'\n";
  $exe_str .= "exit\n";
  print FILE $exe_str;
  print "$0:Calling IDL with string\n$exe_str\n";
  $self->{TMPFILE} = $file;
  1;
}


#==================================================================
#Make_Anim: 
#  This subroutine is a wrapper that calls all the other
#  routines; they do the work.
#==================================================================
sub Make_Anim{
  my $self=shift;
  $self->_croak("$0:Error creating Lock File\n",
	       "$0:LOCKFILE CREATION ERROR")
    unless $self->CreateLockFile;
  $self->_croak("$0:Error creating IDL TMP .pro File\n",
	       "$0:IDL TMPFILE CREATION ERROR")
    unless $self->CreateTmpFile;
  my $tmpfile=$self->{TMPFILE};
  my $r=system("idl $tmpfile")/256;
  $self->_croak("$0: IDL runtime error\n",
		"$0: IDL Runtime error") unless
		  $r==0;
  $self->_croak(
         "$0: Errors reported during run of auto_movie.pro\n\n".
		  "Errors are:\n\n".
		  $self->{IDL_ERRORS}."\n",
		  "$0:ERRORS IN AUTO_MOVIE.PRO") if $self->Errors();
}

#==================================================================
# ReadDefsFile:
#  Replacement for VapUtil::auto_movie_defs.
#==================================================================

sub ReadDefsFile{
  my $self = shift;

  # parses the file $VAP_ROOT/auto_movie_defs.dat returns array
  # containing the ROI designations
  # (i.e. 'nepac','nwpac','npac','nwatl' and whatever other regions of
  # interest we may decide to do.
  

  # open the file 
  my $file=shift || $auto_movie_defs_file;
  open (DEFS,"<$file") ||
    $self->_croak("$0: Can't open $file!:$!\n",
		  "$0:OPEN ERROR");
  my @rois = ();
  while (<DEFS>){
    next if /^;.*$/; # IDL comment line
    next if /^\s*$/; # empty line
    my @tmp = split(/,/, $r);
    my $tmp = $tmp[0];
    @tmp = split(/:/, $tmp);
    $tmp = ($tmp[1] =~ s/'//g);
    push @rois, $tmp;
  }
  close DEFS;
  $self->{ROIS} = @rois;
  @rois;
}


#==================================================================
# _croak:
#  Wrapper for errorobject->ReportAndDie
#==================================================================

sub _croak {
  my $self=shift;
  my $msg=shift || "NULL MESSAGE\n";
  my $subject =shift || "NULL SUBJECT";
  my $errobj=$self->{ERROROBJ};
  $errobj->ReportAndDie($subject, $msg);
  1;
}
1;
