=head1 VapWebsite

=head2 SYNOPSIS

       Object encapsulating code to maintain VAP website.

=head2 USAGE

       my $vapwww = VapWebsite->new(FILE => `file', [EXT => `extension'])

=over 2

=item* FILE: FULLY QUALIFIED name of the file to move. All necessary
             information as to where the file lives should be
             contained in the name. This input is REQUIRED!

=item * EXT: the extension of the files used to make the
             animation. Only used when processing an animation
             product. This enables the object to find the file of the
             first frame of the animation to be used as a clickable
             image in the animation webpages.


=back

       No further initialization is necessary, all such information is
       maintained in a defaults file: $VAP_LIBRARY/website_defs.

       All further manipulation of the website is done through method calls.

=head2 METHODS

=over 4

=item * new: Constructor

=item * UpdateWebsite()

        Move `file' to the correct location in the webspace and update
        the correct webpage. 

=back


=cut

use strict;
use Carp;
use Cwd;
use File::Copy;
use File::Basename;
use vars qw/$VERSION $usage/;

BEGIN {
  $VERSION = "0.9";

  croak "ENV var VAP_LIBRARY is undefined\n" 
    unless $ENV{VAP_LIBRARY};

  croak "ENV var VAP_SFTWR_PERL is undefined\n" 
    unless $ENV{VAP_SFTWR_PERL};

  croak "ENV var VAP_WWW_TOP is undefined\n" 
    unless $ENV{VAP_WWW_TOP};

  my $defs = $ENV{VAP_LIBRARY} . "/VapWebsite_defs";
  croak "Can't find defs file $defs\n" unless (-e $defs);

  $usage = << EOF

    $website = VapWebsite->new(FILE => 'file')

    'file' is the result of some processor (overlay, animation,
             tropical storm overlay). This object moves the file to
             the proper location in the web space and updates the
             proper webpage.

EOF;


}

use lib $ENV{VAP_SFTWR_PERL};
use lib $ENV{VAP_LIBRARY};
require "VapWebsite_defs" or die "Can't `require' VapWebsite_defs\n";
use VapCGI;
use HTML::Table
use LeftNavTable;
use TopNavTable;
use VapUtil;
use VapError;

@VapWebsite::ISA = qw/VapError/;

sub new{
  my $class = shift;
  my $self={@_};
  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};
  bless $self, ref($class) || $class;

  $self->_croak("Need filename!\n",
		"VapWebsite::new No filename!") unless $self->{FILE};

  $self->_croak("file ". $self->{FILE} . " doesn't exist!\n",
		"VapWebsite: non-existent file!") unless (-e $self->{FILE});

  my $file = $self->{FILE};
  my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
      $atime,$mtime,$ctime,$junk)=stat($file);
  $self->_croak("file ". $file . " is empty!\n",
		"VapWebsite: non-existent file!") unless ($size > 0)

  $self->{MTIME} = $mtime;
  $self->{SIZE} = $size

  my $defs = "VapWebsite_defs";
  require $defs or $self->_croak("Can't `require' $defs\n", 
				 "VapWebsite: require ERROR");

  $self->{WEB}->{DEFS} = $website_defs;
  $self->{CGI} = CGI->new(-nodebug=>1);

  $self->parseFilename;
  return $self;
}

=pod

=head1 parseFilename

=head2 Synopsis: 

       Parses the filename to determine what processor created
       it. This determines where it is moved and which webpage is
       modified.

=cut

sub parseFilename{
  my $self=shift;
  my $file = $self->{FILE};

  my @allowed_extensions = @{$self->{DEFAULTS}->{ALLOWED_EXTENTIONS}};
  my %type2webpage = %{$self->{DEFAULTS}->{TYPE2WEBPAGE}};
  my ($name, $path, $ext) = fileparse($file,@allowed_extensions);
  $self->{NAME} = $name;
  $self->{PATH} = $path;
  my @fields=();
  if ($ext =~ /(jpeg|jpg)/i){
    if ($name =~ /-(TYP|DEP|STO)-/) {

        # ---- It's a tropical storm image ----------
        # The format for these files is 
        #
        #      SAT-STORMTYPE-NAME-DATE-TYPE.jpg
        #
        #    where 
        #
        #        SAT = GOES(8|10) or GMS5
        #        STORMTYPE = TYP|DEP|STO
        #        NAME = the storm's name
        #        DATE = the date of the data used
        #        TYPE = Q|S Q=QuikSCAT.
        #

      my ($region, $stormtype, $name, $date,$insttype) = 
	($name =~ /(\w+)-(\w+)-(\w+)-(\d+)-(\w+)\..*/);
      $self->{TYPE} = "TROPICAL_STORM";
      $self->{DESTINATION} = $ENV{VAP_TS_ARCHIVE};
      $self->{WEBPAGE} = $type2webpage{$self->{TYPE}};
      $self->{WHICH_SEAWINDS} = $insttype;
      my $tropical_storm_defs_file = "tropical_storm_defs_oo";
      my $foo=$ENV{VAP_LIBRARY}. "/tropical_storm_defs_oo";
      require "$tropical_storm_defs_file" ||
	$self->_croak("Can't require $foo\n:$!",
		      "ERROR IN `REQUIRE'");

      $self->{TROPICAL_STORM}->{DEFS} = $tsoo_defs; 


    } else {

        # A regular overlay!

        # Filenames look like
        #
        #   GOES10_IR4_YYYYMMDDHHMM_A,B,C,D_TYPE_region.jpeg
        #
        #               -- or --
        #
        #   GMS5_IR1_YYYYMMDDHHMM_A,B,C,D_TYPE_region.jpeg
        #
        # where `type' = S or Q, to distinquich SeaWinds on ADEOS-II
        # from SeaWinds on QuikSCAT, respectively, and `region' is the
        # last portion of the name given to the regions defined in
        # $VAP_LIBRARY/overlay_defs_oo, e.g. NEPAC_1. This 'region'
        # plus the other parts of the name uniquely determine the
        # satellite/sensor/region information.

      my ($number,@junk);
      my ($sat, $sensor, $date, $limits, $type, $region) = split(/_/,$name);
      ($region,$number,@junk) = $region =~ /(\w+)_(\d)?\.\w+/;
      $self->{TYPE} = "OVERLAY";
      $self->{REGION} = join('_',($sat,$sensor2num{$sensor},$region));
      $self->{REGION_NUM} = $number;
      $self->{COMPRESSED_NAME} = "$sat.$sensor.$type.$region";
      $self->{WHICH_SEAWINDS} = $type;
      $self->{DESTINATION} = $ENV{VAP_OVERLAY_ARCHIVE};
      $self->{SYMLINK} = $ENV{VAP_WWW_TOP} . 
	"/images/" . $self->{COMPRESSED_NAME};
      $self->{WEBPAGE} = $type2webpage{$self->{TYPE}} "_$insttype.html";

      my $defsfile = "overlay_defs_oo";
      my $msgdefsfile = $ENV{VAP_LIBRARY} . "/overlay_defs_oo";
      $self->_croak("Can't find defaults file $msgdefsfile!\n",
		    "CAN'T FIND DEFS FILE!") unless (! -e $msgdefsfile );
      do { require "$defsfile"; } or 
	$self->_croak("Can't `require $msgdefsfile\n",
		      "ERROR in `REQUIRE' of DEFS!");
      $self->{OVERLAY}->{DEFS} = $overlay_defs;

    }

  } else {

      # It's a movie: 
      # Files look like
      #
      #     Region_yyyymmddhhmm.mov
      #
      # Where 'region' is one of the regions defined in the defaults
      # file: $VAP_LIBRARY/auto_movie_defs.dat
      #
      # We also need the first frame, whose name will be
      # `wind.001.ext.' So we check for the $self->{EXT} field here.
      #

    $self->_croak("No EXT field in hash!\n","MISSING EXT FIELD!");
    my ($region, $date) = $name =~ /(\w+)_(\w+)/;
    $self->{TYPE} = "ANIMATION";
    $self->{REGION} = $region;
    $self->{DATE} = $date
    $self->{DESTINATION} = $ENV{VAP_ANIM_ARCHIVE};
    $self->{WEBPAGE} = $type2webpage{$self->{TYPE}};
    my $destdir = $self->{DESTINATION};
    $self->{SYMLINK} = $ENV{VAP_WWW_TOP} . "/images/$region.$ext";

    my $defsfile = $ENV{VAP_LIBRARY}. "/auto_movie_defs.dat";
    $self->_croak("Body::new. Empty file:\n$defsfile",
		  "Can't find $defsfile") if (! (-e $defsfile));
    my $hash = VapUtil::auto_movie_defs($defsfile);
    $self->{ANIMATION}->{DEFS} = $hash;

  }

}

=pod

=head1 $status = moveFile()

=head2 Synopsis:

       Move file to website. Store information about the file in the
       object for later use.

=cut

sub moveFile{

  use File::Copy;

  my $self=shift;
  my $file=$self->{FILE};
  my $type = $self->{TYPE};
  my $region = $self->{REGION};
  my $destination = $self->{DESTINATION} . "/". $name;
  $self->{WWWFILE} = $destination;
  copy $src, $destination or 
    $self->_croak( ["Error copying", $src, "to ", $destination],
		   "VapWebside::movefile. Copy Error!");

  my $src = $self->{FILE};

  if ($type =~ /OVERLAY/i){
    my $symlink = $self->{SYMLINK};
    $self->redoSymlink($destination, $symlink);
  } elsif ($type =~ /ANIMATION/i) {
    my $ext = $self->{EXT}
    my ($basename, $path) = fileparse($file);
    my $first_frame = $path . "/wind.001." . $ext;
    my $wwwframe = $self->{WWW_TOP}. "/images/$region.001.$ext";
    my $self->{WWWFRAME} = $wwwframe;

    unlink $wwwframe;
    copy($file, $wwwmov ) or 
      $self->_croak("$0: Error copying $file to\n$wwwmov:$!\n",
		   "$0: COPY ERROR");
    copy($first_frame, $wwwframe ) or 
      $self->_croak("$0: Error copying $first_frame to $wwwframe:$!\n",
		   "$0: COPY ERROR");

    # Delete the symlink from the images subdirectory to the real file
    # in mov_archive.  currently this link points from, e.g.,
    # /images/daily_nepac.mov to
    # /mov_archive/daily_nepac_200208271234.mov to
    # /images/nepac.mov. Then create a new link to the newest version of
    # $region.mov and $region.001.ext, where ext is the type of file 

    # First the movie itself.
    my $symlink = $self->{WWW_TOP}."/images/$region.mov";
    $self->redoSymlink($self->{WWWFILE},$symlink)

    # Now the first frame of the movie.
    $symlink = $self->{WWW_TOP}."/images/$region.001.$ext";
    $self->redoSymlink($self->{WWWFRAME}, $wwwframe);

    unlink $file;
    unlink $first_frame;

  } else {
    my $msg="Unknown type <$type>\n";
    my $msg2 = $self->{ERROROBJ}->{MSG};
    if ($msg2) {
      if (ref($msg2) eq 'ARRAY') {
	$msg2 = join("\n", @{$msg});
      } 
    } else {
      $msg .= $msg2;
    }
    $self->_croak($msg, "Unknown product type!");
  }
  1;

}

sub redoSymlink{
  my ($self,$file, $symlink) = @_;

  if (-e $symlink) {
    unlink $symlink or 
      $self->_croak("$0: Can't unlink $region.mov:$!\n",
		    "$0: DELETE ERROR!");
  }
  symlink $file,$symlink or 
    $self->_croak("$0: Can't symlink $region.mov to $wwwmov!:$!\n",
		  "$0: DELETE ERROR!");
  1;
}


=pod

=head1

=head2

=cut


sub UpdateWebsite{
  my $self=shift;
  my $status = $self->moveFile();
  $msg = "Unable to move $file to webspace!\n";

  my $msg2 = $self->{ERROROBJ}->{MSG};
  if ($msg2) {
    if (ref($msg2) eq 'ARRAY') {
      $msg2 = join("\n", @{$msg});
    } 
  } else {
    $msg .= $msg2;
  }
  $self->_croak($msg, "VapWebsite: CP ERROR") unless $status;

  my $dir=$ENV{VAP_WWW_TOP} . "/images";
  opendir DIR, "$dir" or 
    $self->_croak("VapWebsite: Can't open images directory!\n",
		  "Error opening directory!");
  my @imagedir_files = grep(!/\.\.?/, readdir(DIR));
  closedir DIR;

  my $type=$self->{TYPE};
  if ($type =~ /OVERLAY/i){
    @imagedir_files = grep(/?:(jpg|JPG|jpeg|JPEG)$/, @imagedir_files);
    foreach (@imagedir_files){
      my $file= "$dir/$_";
      my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,
	  $size,$atime,$mtime,$ctime,$junk)=stat($file);
    }
    $self->{WEBSITE}->{$file} = [$size/1024.0, $mtime];
      # Regular overlay
    my $q=$self->{CGI};
    my @order = @{$self->{DEFS}->{OVERLAY}->{ORDER}};
    my $title=$self->{DEFS}->{OVERLAY}->{TITLE};
    my $h1=$self->{DEFS}->{OVERLAY}->{HEADING1};
    my $nrows = @order;
    my $table = HTML::Table(-rows => $nrows, -cols=>1);
    
    foreach (@order){
      
    }
  } elsif ($type =~ /TROPICAL_STORM/i) {

      # Tropical Storm.


  } else {

      # Animation!

    my @order = @{$self->{DEFS}->{ORDER}->{ANIMATION}};
    my $nrows = @order;
    my $table = HTML::Table(-rows => $nrows, -cols=>1);
    @imagedir_files = grep(/?:(mov|MOV)$/, @imagedir_files);
    foreach (@order){
    }

  }
}

1;
