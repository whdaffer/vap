=head1 NAME

  Animate.pm -- Object to handle the animation of QuikSCAT/SeaWinds data
                (Replaces auto_movie.pl)

=head2 SYNOPSIS

  $anim_obj = Animate->new( REGION => region,
                            TIME => time,
                            DELTA => delta,
                            WIND_FILTER => filter);

  This object provides methods to write a temporary file executed by
  IDL to create the frames which are then combined by a call to the
  SGI routine `dmconvert.' It then calls the objects needed to move
  the finished product to the webspace and rewrite the webpage.


=head2 CONSTRUCTOR KEYS

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

=item * FILTER: Filter for SeaWinds on QuikSCAT or on ADEOS-II.
                     This filter must look like a typical unix shell
                     file glob. Default = "{QS,SW}*"

=back

=head2 CALLABLE METHODS

=over 4

=item *   new: Creates new object. (discussed above)

=item *   Make_Anim: Actually make the animation

=item *   MoveOutput: Move output to the WWW area

=item *   WriteWebpage: Rewrite the webpage 

=back

=head2 DEPENDENCIES

       Environmental variables: 
          All of these should be defined for the running user by `sourcing' the 
          .vaprc file

	VAP_LIBRARY - Where things like color tables and the defaults 
                      used by the animation process live

	VAP_SFTWR_PERL - Necessary for Perl modules
	VAP_OPS_ANIM   - Work area
	VAP_WWW_TOP    - Top of webspace
	VAP_OPS_TMPFILES - Where to put tmp files.

       Perl modules:

	 Carp
	 Cwd
	 File::Copy
	 File::Basename
	 VapUtil
	 VapError


=cut

#
# $Id$
#
# Modifications:
#
# $Log$
# Revision 1.9  2003/01/16 23:47:53  vapdev
# Continuing work
#
# Revision 1.8  2003/01/04 00:16:21  vapdev
# Continuing work
#
# Revision 1.7  2002/12/17 22:33:20  vapdev
# ongoing work
#
# Revision 1.6  2002/12/03 00:13:28  vapdev
# Ongoing work
#
# Revision 1.5  2002/08/21 18:24:59  vapdev
# Continuing work
#
#
#
package Animate;
use strict;
use Carp;
use Cwd;
use File::Copy;
use File::Basename;
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

  croak "ENV var VAP_WWW_TOP is undefined\n" 
    unless $ENV{VAP_WWW_TOP};

  croak "ENV var VAP_OPS_TMPFILES is undefined\n" 
    unless $ENV{VAP_OPS_TMPFILES};

  $usage = "$0: obj=Animate->new(REGION=>region [,TIME=>time,DELTA=>DELTA])\n";
}
use lib $ENV{VAP_SFTWR_PERL};
use VapUtil; 
use VapError;

#==================================================
#
#==================================================

sub new {
  my $class = shift;
  my $self={USER => $ENV{USER},
	    FILTER => "{QS,SW}*",
	    WWW_TOP => $ENV{VAP_WWW_TOP},
	    @_};
  $self->{DEFAULTS_FILE} = $ENV{VAP_LIBRARY}."/auto_movie_defs.dat";
  bless $self, ref($class) || $class;

    # Set the Error reporting object.
  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};


    # Read the defaults file
  $self->ReadDefsFile;

  if ($self->{GET_REGIONS}) {
    return keys(%{$self->{ROIS}});
  }

  if ($self->{GET_DEFS}) {
    return $self->{ROIS};
  }

  croak $usage unless $self->{REGION};
  my $region = $self->{REGION} = uc $self->{REGION};

    # Make sure this is a valid region
  $self->{ERROROBJ}->_croak("Invalid REGION! ". $self->{REGION} .
		" valid regions are \n". 
		join("\n",keys(%{$self->{ROIS}})). "\n",
		"$0:INVALID REGION")
    unless grep($self->{REGION},keys(%{$self->{ROIS}}));
  

    # Set time and working dir, make sure the latter exists and that we
    # can CD to it.

  $self->getTime unless defined $self->{TIME};
  $self->{STARTTIME} = $^T;

  my $hash = $self->{ROIS}->{$region};
  my $working_dir = $self->{WORKING_DIR} = $hash->{ANIM_PATH};
  my $name0=$self->{NAME0};
  $self->{ERROROBJ}->_croak("$name0:Can't find working directory $working_dir\n",
		"$name0:NONEXISTENT WORKING DIR")
    unless (-e $working_dir);
  chdir $working_dir or $self->{ERROROBJ}->_croak("$name0:Can't CD to $working_dir",
				      "$name0: CD error!");

  $self->{TMPFILE_DIR} = $ENV{VAP_OPS_TMPFILES};
  $self->{ERROROBJ}->_croak("TMPFILE directory doesn't exist!\n",
		$self->{NAME0} . ": NONEXISTENT TMPFILE DIRECTORY!")
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
    $self->{ERROROBJ}->_croak($self->{NAME0} . ": Error opening $file: $!\n",
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
    $self->{ERROROBJ}->_croak($self->{NAME0} . ": Error opening $file: $!\n",
		  "FILE OPEN ERROR");
  my $date_time = $self->{TIME};
  my $exe_str = "auto_movie,\'$date_time\',roi=\'$roi\'";
  $exe_str .= ",pid=$$, lockfile = '". $self->{LOCKFILE}."'";
  $exe_str .= ",windfilter='".$self->{FILTER}."'";
  $exe_str .= ",outbase = 'wind'\n";
  $exe_str .= "exit\n";
  print FILE $exe_str;
  print $self->{NAME0} . ":Calling IDL with string\n$exe_str\n";
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
  $self->{ERROROBJ}->_croak($self->{NAME0} . ":Error creating Lock File\n",
	       $self->{NAME0} . ":LOCKFILE CREATION ERROR")
    unless $self->CreateLockFile;
  $self->{ERROROBJ}->_croak($self->{NAME0} . ":Error creating IDL TMP .pro File\n",
	       $self->{NAME0} . ":IDL TMPFILE CREATION ERROR")
    unless $self->CreateTmpFile;
  my $tmpfile=$self->{TMPFILE};
  my $r=system("idl $tmpfile")/256;
  $self->{ERROROBJ}->_croak($self->{NAME0} . ": IDL runtime error\n",
		$self->{NAME0} . ": IDL Runtime error") unless
		  $r==0;
  $self->{ERROROBJ}->_croak(
         $self->{NAME0} . ": Errors reported during run of auto_movie.pro\n\n".
		  "Errors are:\n\n".
		  $self->{IDL_OUTPUT}."\n",
		  $self->{NAME0} . ":ERRORS IN AUTO_MOVIE.PRO") if $self->CheckForErrors();
  1;
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
  my $file=shift || $self->{DEFAULTS_FILE};
  open (DEFS,"<$file") ||
    $self->{ERROROBJ}->_croak($self->{NAME0} . ": Can't open $file!:$!\n",
		  $self->{NAME0} . ":OPEN ERROR");

  my $lines;
  do {
    local $/=undef;
    $lines = <DEFS>;
  };
  close DEFS;
  my @rois=();
  my @lines = split /^\{\s*/m, $lines;

  # These records, from $VAP_LIBRARY/auto_movie_defs.dat, look like:

  #  { desig: 'NEPAC', webname: 'N.E. Pac',alonpar: [195.,245,1.5], \
  #     alatpar: [30., 60,1.5], wpath: "$VAP_DATA_TOP", \
  #     interp_path: "$VAP_OPS_ANIM/", anim_path: "$VAP_OPS_ANIM/nepac/daily",\
  #     anim_par: [320,240,60], min_nvect: 4000,decimate: 0l, \
  #     CRDecimate: [2,2], ExcludeCols: ''  }

  # where '\' indicates a wrapped line: in the file itself this entire
  # record MUST BE one one line. If it isn't, the IDL will break!
  
  foreach (@lines) {
    #s/\s*//g;
    next unless /^\s*desig/i;
    my @t=split/:/;

    # $t[1] = 'NEPAC', webname. NEPAC is the name of the ROI and we'll
    # use it for the key into the hash we'll ultimately return.

    my ($roi,$k) = split /,/,$t[1];
    $roi =~ s/'//g;
    $roi =~ s/\s+//g;
    $roi = uc($roi);
    $k =~ s/\s+//g;
    my ($kk,$v);
    for (my $i=2; $i<@t; $i++){

        # Now $t[i] = "value[i], keyword[i+1]". value[i] may be an
        # array, which would have a comma in it, so we have to be
        # careful about that. We want to assign value[i] to
        # keyword[i], not keyword[i+1], that's why I saved it before
        # entering the loop and set it at the end. It's a bit
        # convoluted, but it works.

      #$t[$i] =~ s/\s+//g;
      my $tt=reverse($t[$i]);
      my $ii=index $tt, ",";
      $kk=reverse(substr($tt,0,$ii));
      $kk =~ s/\s+//g;
      $v=reverse(substr($tt,$ii+1));
      $v =~ s/}$//g;
      $v =~ s/\s+(.*)\s*/$1/g;
      $v =~ s/'//g;
      $v = eval $v if (index($v,'[') > -1);
      if ($v=~/\$/) {
	$v = deenvvar($v);
      }

      $self->{ROIS}->{$roi}->{uc($k)} = $v;
      $k=$kk;
    }
  }
  @rois = keys %{$self->{ROIS}};
}


#==================================================================
# getOutput
#
# usage: ($movie_file, $extension_of_frames) = $animobj->getOutput;
#
# Gets the output name from the temporary file created by
# auto_movie.pro which holds the name, stores the name of the movie
# and the extension of the individual frame files in the hash, deletes
# that temporary file and returns a list consisting of
# (movie_file,extension)
#
# ==================================================================
sub getOutput {
  my $self=shift;

  my $region=lc $self->{REGION};
  my $file=$self->{TMPFILE_DIR} . "/auto_movie_mov_filename_".$region."_".$$;
  open (FILE, "<$file") ||
    $self->{ERROROBJ}->_croak($self->{NAME0} . ": Error opening $file:$!\n",
		  $self->{NAME0} . ": FILE OPEN ERROR!");
  my $mov_file = <FILE>;
  my $ext=<FILE>;
  chomp $mov_file;
  chomp $ext;
  close FILE;
  unlink $file;
  my $dir=getcwd;
  $mov_file = $self->{MOV_FILE} = "$dir/$mov_file";
  $self->{EXT} = $ext;
  ($mov_file,$ext);
}


#==================================================================
# CheckForErrors
#==================================================================

sub CheckForErrors{
  my $self=shift;
  my $lockfile=$self->{LOCKFILE};
  open LOCKFILE, "<$lockfile" || 
    $self->{ERROROBJ}->_croak($self->{NAME0} . ":Error opening $lockfile:$!\n",
		  $self->{NAME0} . ":FILE OPEN ERROR");
  my $lines;
  do {
    local $/=undef;
    $lines = <LOCKFILE>;
  };
  close LOCKFILE;

  $self->{IDL_OUTPUT} = $lines;
  $lines =~ /ERROR:/;
}

#==================================================================
# getDefs
#
#  Used to get default information for use in VapWebsite.pm
#
#==================================================================

sub getDefs{
  my $self=shift;
  return $self->{ROIS};
}

sub getTime{
  use Time::Local;
  my $self=shift;
  my ($sec,$min, $hour,$mday,$month,$year)=gmtime(time);
  my $time=sprintf("%04d/%02d/%02d/%02d/%02d/%02d",
		$year+1900,$month+1,$mday,$hour,$min,$sec || 0);
  $self->{TIME} = $time;
  $time;
}
1;
