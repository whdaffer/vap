=head1 VapWebsite

=head2 SYNOPSIS

       Object encapsulating code to maintain VAP website.

=head2 USAGE

       my $vapwww = VapWebsite->new(FILE => `file',
                                    PROCESSOR_DEFAULTS = hash [,
                                    errorobj = VapErrorObject])


=over 2

=item * FILE: FULLY QUALIFIED name of the file to move. All necessary
             information as to where the file lives should be
             contained in the name. This input is REQUIRED!

=item * PROCESSOR_DEFAULTS: a hash containing the `defaults' used by
                            whatever particular processor was
                            used. This is the hash returned by the
                            processor->getDefs method. For the two
                            overlay processors (Overlay.pm and OTS.pm)
                            it's what's in the file
                            VAP_LIBRARY/{overlay,tropical_storms}_defs_oo,
                            since these are perl eval'able files. For
                            Animate.pm it's the information contained
                            in VAP_LIBRARY/auto_movie_defs.dat, after
                            some perl processing.

=item * ERROROBJ: An object returned from VapError->new. Optional.
                  it's created if it hasn't been passed in.

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
package VapWebsite;
use strict;
use Carp;
use Cwd;
use File::Copy;
use File::Basename;
use vars qw/$VERSION $usage $website_defs $tsoo_defs $overlay_defs/;

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

  $usage = << "EOF";

    obj = VapWebsite->new(FILE => 'file',PROCESSOR_DEFAULTS => hash
                          [,errorobj=errorobject])

    'FILE'   : the result of some processor (overlay, animation,
             tropical storm overlay). This object moves the file to
             the proper location in the web space and updates the
             proper webpage.

    PROCESSOR_DEFAULTS: the defaults used by whatever processor created FILE,
    that is, the contents of files VAP_LIBRARY/{overlay_defs_oo,
    tropical_storm_defs_oo or auto_movie_defs.dat (suitably munged by
    Animate.pm, of course, since this last file isn't perl eval'able).

EOF


}

use lib $ENV{VAP_SFTWR_PERL};
use lib $ENV{VAP_LIBRARY};
require "VapWebsite_defs" or die "Can't `require' VapWebsite_defs\n";
use VapCGI;
use HTML::Table;
use LeftNavTable;
use TopNavTable;
use VapUtil;
use VapError;

sub new{
  my $class = shift;
  my $self={@_};
  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};
  bless $self, ref($class) || $class;

  $self->{ERROROBJ}->_croak("Need filename!\n",
		"VapWebsite::new No filename!") unless $self->{FILE};

  $self->{ERROROBJ}->_croak("file ". $self->{FILE} . " doesn't exist!\n",
		"VapWebsite: non-existent file!") unless (-e $self->{FILE});

  my $file = $self->{FILE};
  my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
      $atime,$mtime,$ctime,$junk)=stat($file);
  $self->{ERROROBJ}->_croak("file ". $file . " is empty!\n",
		"VapWebsite: non-existent file!") unless ($size > 0);

  $self->{errorobj}->_croak("Processor defaults weren't passed!\n",
			    "VapWebsite: No processor defaults!") 
    unless $self->{PROCESSOR_DEFAULTS};
  $self->{MTIME} = $mtime;
  $self->{SIZE} = int($size/1024.0 + 0.5);

  my $defs = "VapWebsite_defs";
  require $defs || $self->{ERROROBJ}->_croak("Can't `require' $defs\n", 
				 "VapWebsite: require ERROR");

  $self->{WEB}->{DEFS} = $website_defs;
  $self->{CGI} = VapCGI->new(-nodebug=>1);

  my ($name0, $path0) = fileparse($0);
  $self->{NAME0} = $name0;
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

  my @allowed_extensions = @{$self->{WEB}->{DEFS}->{ALLOWED_EXTENSIONS}};
  my %type2webpage = %{$self->{WEB}->{DEFS}->{TYPE2WEBPAGE}};
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
      my $year = strsum($date,0,4);
      $self->{TYPE} = "TROPICAL_STORM";
      $self->{DESTINATION} = $ENV{VAP_TS_ARCHIVE} . "/$year";
      $self->{WEBPAGE} = $type2webpage{$self->{TYPE}} . ".html";
      $self->{WHICH_SEAWINDS} = $insttype;

        # Just to make the code a little more readible!
      $self->{TROPICAL_STORM}->{DEFS} = $self->{PROCESSOR_DEFAULTS};
      delete $self->{PROCESSOR_DEFAULTS};

    } else {

        # A regular overlay!

        # Filenames look like
        #
        #   GOES_10_IR4_YYYYMMDDHHMM_A,B,C,D_region_type.jpeg
        #
        #               -- or --
        #
        #   GMS_5_IR1_YYYYMMDDHHMM_A,B,C,D_region_type.jpeg
        #
        # where `type' = S or Q, to distinquich SeaWinds on ADEOS-II
        # from SeaWinds on QuikSCAT, respectively, and `region' is the
        # last portion of the name given to the regions defined in
        # $VAP_LIBRARY/overlay_defs_oo, e.g. NEPAC_1. This 'region'
        # plus the other parts of the name uniquely determine the
        # satellite/sensor/region information.

      my ($sat, $satnum, $sensor, $date, $limits, $region,$num,$type) = 
	split(/_/,$name);
      $self->{SYMLINK} = $ENV{VAP_WWW_TOP} ."/images/" . 
	join("_", ($sat, $satnum, $sensor, $region, $num,$type)) . "$ext";
      $self->{TYPE} = "OVERLAY";
      $self->{REGION} = join('_',($sat,$satnum,
				  $VapUtil::sensorname2num{lc($sensor)},
				  $region, $num));
      $self->{WHICH_SEAWINDS} = $type;
      $self->{DESTINATION} = $ENV{VAP_OVERLAY_ARCHIVE};
      $self->{WEBPAGE} = $type2webpage{$self->{TYPE}} . "_$type.html";

      $self->{OVERLAY}->{DEFS} = $self->{PROCESSOR_DEFAULTS};
      delete $self->{PROCESSOR_DEFAULTS};

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

    $self->{ERROROBJ}->_croak("No EXT field in hash!\n","MISSING EXT FIELD!")
      unless $self->{EXT};
    my ($region, $date) = $name =~ /(\w+)_(\w+)/;
    $self->{TYPE} = "ANIMATION";
    $self->{REGION} = uc $region;
    $self->{DATE} = $date;
    $self->{DESTINATION} = $ENV{VAP_ANIM_ARCHIVE};
    $self->{WEBPAGE} = $type2webpage{$self->{TYPE}} .".html";
    my $destdir = $self->{DESTINATION};
    $self->{ANIMATION}->{DEFS} = $self->{PROCESSOR_DEFAULTS};
    delete $self->{PROCESSOR_DEFAULTS};

  }

}

=pod

=head1 moveFile



=head2 Synopsis:

       $status = moveFile()

       Move file to website. Store information about the file in the
       object for later use.

=cut

sub moveFile{

  use File::Copy;

  my $self=shift;
  my $file=$self->{FILE};
  my ($name, $path) = fileparse($file);
  my $type = $self->{TYPE};
  my $region = $self->{REGION};
  my $destination = $self->{DESTINATION} . "/". $name;

  $self->{WWWFILE} = $destination;
  my $src = $self->{FILE};
  copy $src, $destination || 
    $self->{ERROROBJ}->_croak( "Error copying $src to  $destination",
		   "VapWebside::moveFile. Copy Error!");


    # This following section pertains only to OVERLAYS and ANIMATIONS,
    # as they require the creation of symlinks. The TROPICAL_STORM
    # files just get copied to their location directory and the
    # webpage makes reference to them directly.


  if ($type =~ /OVERLAY/i) {

    my $symlink = $self->{SYMLINK};
    $self->redoSymlink($destination, $symlink);

  } elsif ($type =~ /ANIMATION/i) {

    my $ext = $self->{EXT} || 
      $self->{ERROROBJ}->_croak("In moveFile: No extension for Animation!",
				"Error in VapWebsite::moveFile. No extension!");

    my $first_frame = $self->{PATH} . "/wind.001." . $ext;
    my $wwwframe = $ENV{VAP_WWW_TOP}. "/images/$region.001.$ext";
    $self->{WWWFRAME} = $wwwframe;

    unlink $wwwframe;
    copy($first_frame, $wwwframe ) or 
      $self->{ERROROBJ}->_croak($self->{NAME0} . ": Error copying $first_frame to $wwwframe:$!\n",
		   $self->{NAME0} . ": COPY ERROR");

    # Delete the symlink from the images subdirectory to the real file
    # in mov_archive.  currently this link points from, e.g.,
    # /images/daily_nepac.mov to
    # /mov_archive/daily_nepac_200208271234.mov to
    # /images/nepac.mov. Then create a new link to the newest version of
    # $region.mov and $region.001.ext, where ext is the type of file 

    # First the movie itself.
    my $symlink = $ENV{VAP_WWW_TOP}."/images/$region.mov";
    $self->redoSymlink($self->{WWWFILE},$symlink);

    unlink $file;

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
    $self->{ERROROBJ}->_croak($msg, "Unknown product type!");
  }
  1;

}

=pod

=head1 redoSymlink

=head2 Usage:

      $status = redoSymlink($file, $symlink)

      Create a symlink from $file to $symlink, deleting $symlink if it
      already exists. Meant to create the symlink, which is what the
      webpage points refers to, with the actual file to be displayed.

      The routine 'dies' (by means of a call to _croak) if
      unsuccesful, so if it returns at all, it succeeded.



=cut


sub redoSymlink{
  my ($self,$file, $symlink) = @_;

  if (-e $symlink) {
    unlink $symlink || 
      $self->{ERROROBJ}->_croak($self->{NAME0} . ": Can't unlink $symlink:$!\n",
		    $self->{NAME0} . ": DELETE ERROR!");
  }

  # Want to make the symlink as short as possible, for readibility and
  # security concerns, so here we remove all portions of the path that
  # are common between $file and $symlink. The assumption is that
  # $file resides in a subdirectory below the directory where $symlink
  # is, which should be true provided someone didn't mess with the
  # definitions of the environmental variables!

  # Note, this will only work if both files are *FULLY QUALIFIED*!

  my ($filename, $filepath) = fileparse($file);
  my ($symlinkname, $symlinkpath) = fileparse($symlink);

  my @filepathcomponents = split /\//, $filepath;
  my @symlinkpathcomponents = split /\//,$symlinkpath;

  my $relpath = join "/", @filepathcomponents[ @symlinkpathcomponents .. $#filepathcomponents ];
  $file = "./$relpath/$filename";

  symlink $file,$symlink || 
    $self->{ERROROBJ}->_croak($self->{NAME0} . ": Can't symlink $symlink to $file!:$!\n",
		  $self->{NAME0} . ": SYMLINK ERROR!");
  1;
}

=pod

=head1 updataWebsite

=head2 Usage;

  $status = updateWebsite;

  Moves the files to the webspace, creating the appropriate symbolic
  links and creates the proper body page for this product.

  This routine dies if there's a failure so if it returns, it succeeded.


=cut



sub updateWebsite{
  my $self=shift;
  my $ext = shift || $self->{EXT};
  my $file=$self->{FILE};

    # Move the file to the website, update the links.

  my $status = $self->moveFile();
  my $msg = "Unable to move $file to webspace!\n";

  my $msg2 = $self->{ERROROBJ}->{MSG};
  if ($msg2) {
    if (ref($msg2) eq 'ARRAY') {
      $msg2 = join("\n", @{$msg});
    } 
    $msg .= $msg2;
  }
  $self->{ERROROBJ}->_croak($msg, "VapWebsite: CP ERROR") unless $status;

  my $dir=$ENV{VAP_WWW_TOP} . "/images";
  chdir $dir || 
    $self->{ERROROBJ}->_croak("Can't CD to $dir!\n",
			      "VapWebsite: CD error!");
  opendir DIR, "." ||
    $self->{ERROROBJ}->_croak("VapWebsite: Can't open images directory!\n",
		  "Error opening directory!");
  my @imagedir_files = grep(!/^\.\.?/, readdir(DIR));
  closedir DIR;

  my $order = {OVERLAY=>$self->{WEB}->{DEFS}->{OVERLAY}->{ORDER},
	       ANIMATION=>$self->{WEB}->{DEFS}->{ANIMATION}->{ORDER},
	       TROPICAL_STORM=>$self->{WEB}->{DEFS}->{TROPICAL_STORM}->{ORDER}};

  my $type=$self->{TYPE};

    # With the exception of TROPICAL_STORM type, which has to be
    # handled differently, here find the mtime of each of the files
    # for this type, store the information in $self, and then use it
    # to rewrite this particular webpage.

  my $bodytable;
  if ($type =~ /TROPICAL_STORM/i) {

      # Tropical Storm.
    my @order = @{$order->{TROPICAL_STORM}};


  } elsif ($type =~ /OVERLAY/i) {

      #------------- Regular overlay -------------------------


    my $re = $self->{WHICH_SEAWINDS} . "\\.(?:jpg|JPG|jpeg|JPEG)\$";

    @imagedir_files = grep(/$re/, @imagedir_files);

    foreach (@imagedir_files){

        # All of these files are links to the actual files, which live
        # in $VAP_OVERLAY_ARCHIVE, but stat() stats what the link is
        # pointing to, not the link itself.

      my $region = $self->Overlayfilename2region($_);
      if ($self->goodLink($_)){
	my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,
	    $size,$atime,$mtime,$ctime,$junk)=stat($_);

	$self->{WEBSPACE}->{$region} = [$_,int($size/1024.0 + 0.5), $mtime];
      } else {
	$self->{ERROROBJ}->Report("./images/$_ is stale! Deleting!",'INFO');
	unlink $_;
      }
    }



    my $q=$self->{CGI};
    my @order = @{$order->{OVERLAY}};

	      
    my $title=$self->{WEB}->{DEFS}->{OVERLAY}->{TITLE};
    my $keywords =$self->{WEB}->{DEFS}->{OVERLAY}->{META}->{KEYWORDS};
    my $h1=$self->{WEB}->{DEFS}->{OVERLAY}->{HEADING1};

      #### ===== here we start building the page ======= ###


    $self->startPage($title,$keywords,$h1,0,$order);


    my $nrows = @order;
    $bodytable = HTML::Table->new(-rows => $nrows, 
				  -cols=>1,
				 -width=>"80\%");
    my $row=1;

    foreach (@order){

      if ($self->{WEBSPACE}->{$_}){
	my ($file,$size,$mtime)=@{$self->{WEBSPACE}->{$_}};
	my $name = $self->{OVERLAY}->{DEFS}->{$_}->{WEB}->{NAME};
	my $string = "Overlay for the ";
	my $createstring = "Created: " . scalar(localtime($mtime));
	my $sizestring .= "Size: $size (Kb)";
	my $linelink = $q->a({-href=>"/images/$file",
			     -name=>"$_",
			     -align=>'left'},
			     $name );
	my $img = $q->img({-src=>"/images/$file",
			  -valign=>'top',
			   -width=>'200',
			   -height=>'150'});
	my $alt = join( " ", split(/_/,$_));
	my $clickableimage = $q->a({-href=>"/images/$file",
				    -alt=>"$alt",
				    -valign=>'top',
				    -width=>'200',
				    -height=>'150'}, $img);


	my $content = $q->p({-align=>'left'}, "$string $linelink");
	#$content .= $q->p({-align=>'left'},$createstring);
	#$content .= $q->p({-align=>'left'},$sizestring);
	$content .= "$createstring\n$sizestring";
	$content .= $q->p({-align=>'left'}, $clickableimage);

	$bodytable->setCell($row++, 1, $content);
      }
    }

  } else {

      # ================= Animation! =======================


    my @movies = grep(/mov$/i, @imagedir_files);
    my @first_frames = grep(/.*\.001\..*/,@imagedir_files);
    foreach (@movies){
      my $region = $self->Animfilename2region($_);
      my @firstframe = grep(/$region/,@first_frames);
        # There should be no or one file that has the form region.001.ext
      my $first_frame;
      if (@firstframe) {
	$first_frame = $firstframe[0];
      } else {
	# if there's none, it hardly makes any difference what we put here.
	$first_frame="$region.001.jpeg";
      }
      if ($self->goodLink($_)){
	my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,
	    $size,$atime,$mtime,$ctime,$junk)=stat($_);
	$self->{WEBSPACE}->{$region} = [$_, int($size/1024.0 + 0.5), $mtime, $first_frame];
      } else {
	$self->{ERROROBJ}->Report("./images/$_ is stale! Deleting!",'INFO');
	unlink $_;
      }
    }


    my @order = @{$order->{ANIMATION}};
    my $nrows = @order;
    $bodytable = HTML::Table->new(-rows => $nrows, 
				  -cols=>1,
				  -width=>"80\%");
    my $row=1;

    my $q=$self->{CGI};
    my $title=$self->{WEB}->{DEFS}->{ANIMATION}->{TITLE};
    my $keywords =$self->{WEB}->{DEFS}->{ANIMATION}->{META}->{KEYWORDS};
    my $h1=$self->{WEB}->{DEFS}->{ANIMATION}->{HEADING1};

      #### ===== here we start building the page ======= ###


    $self->startPage($title,$keywords,$h1,0,$order);

    foreach (@order){

      if ($self->{WEBSPACE}->{$_}) {
	my ($file,$size,$mtime, $first_frame)=@{$self->{WEBSPACE}->{$_}};

	my $string = "Animation for the ";
	my $createstring = "Created: " . scalar(localtime($mtime));
	my $sizestring = "Size: $size (Kb)";
	my $name = $self->{ANIMATION}->{DEFS}->{$_}->{WEBNAME};
	my $linelink = $q->a({-href=>"/images/$file",
			     -name=>"$_",
			     -align=>'left'},
			     $name );
	my $img = $q->img({-src=>"/images/$first_frame",
			  -valign=>'top'});

	my $alt = join( " ", split(/_/,$_));
	my $clickableimage = $q->a({-href=>"/images/$file",
				    -alt=>"$alt",
				    -valign=>'top'}, $img);


	my $content = $q->p({-align=>'left'}, $string . $linelink);
	#$content .= $q->p({-align=>'left'},$createstring);
	#$content .= $q->p({-align=>'left'},$sizestring);
	$content .= "$createstring\n$sizestring";
	$content .= $q->p({-align=>'left'}, $clickableimage);
	$bodytable->setCell($row++, 1, $content);
      }
    }
  }

  $self->{HTML} = $self->endPage($bodytable);

  # Now the page is complete. All we have to do is replace the old
  # page with the new one!

  my $newwebpage = $ENV{VAP_WWW_TOP} . "/" . $self->{WEBPAGE} . ".tmp";
  open WEBPAGE,">$newwebpage" || 
    $self->{ERROROBJ}->_croak("Can't open $newwebpage!",
			      "VapWebsite::updateWebpage -- OPEN ERROR!");
  print WEBPAGE $self->{HTML} . "\n";
  close WEBPAGE;
  my $oldwebpage = $ENV{VAP_WWW_TOP} . "/" . $self->{WEBPAGE};
  rename $newwebpage, $oldwebpage ||
    $self->{ERROROBJ}->_croak("Can't rename $newwebpage ot $oldwebpage!",
			      "VapWebsite::updateWebpage -- RENAME ERROR!");
  1;
}

# =pod

# =head1 filename2region

# =head2 Usage:

#        $region = filenam2region( $file );

#        Returns the `region' of the input filename, which must reside
#        in the websapce and be fully qualified, since the initial
#        determination about the file is made by the path) This `region'
#        is the `key' to the information about the file. For regular
#        overlays, it's the `key' into the overlay_defs_oo hash
#        (e.g. GOES_10_4_NEPAC_1), in the case of Tropical storms, it's
#        a key in the ...{WEB}->{... subhash of $tsoo_defs, defined in
#        tropical_storm_defs_oo, (e.g. {WEB}->{GOESEAST}.) In the case
#        of the animations, it's one of the values of 'desig' field in
#        the structure defined in auto_movie_defs.dat. These three files
#        are found in $VAP_LIBRARY.

#        As is the case with the other routines, this subroutine only
#        returns on succes, otherwise it `dies.'


# =cut

# sub filename2region{
#   my ($self, $file)= @_;

#   $self->{ERROROBJ}->_croak("Missing file parameter!\n", 
# 		"VapWebsite::filename2region: Argument Error");
#   my $name = $self->{NAME};
#   my $path = $SELF->{PATH}
#   $self->{ERROROBJ}->_croak("File is not fully qualified!\n$file\n", 
# 		  "VapWebsite::filename2region: NO PATH ") if (!$path);
#   my $overlay = $ENV{VAP_OVERLAY_ARCHIVE};
#   my $anim = $ENV{VAP_ANIM_ARCHIVE};
#   my $ts = $ENV{VAP_TS_ARCHIVE};

#   if ($path eq $overlay){
#     # plain old overlay


#         # Filenames look like
#         #
#         #   GOES10_IR4_YYYYMMDDHHMM_A,B,C,D_TYPE_region.jpeg
#         #
#         #               -- or --
#         #
#         #   GMS5_IR1_YYYYMMDDHHMM_A,B,C,D_TYPE_region.jpeg
#         #
#         # where `type' = S or Q, to distinquich SeaWinds on ADEOS-II
#         # from SeaWinds on QuikSCAT, respectively, and `region' is the
#         # last portion of the name given to the regions defined in
#         # $VAP_LIBRARY/overlay_defs_oo, e.g. NEPAC_1. This 'region'
#         # plus the other parts of the name uniquely determine the
#         # satellite/sensor/region information.

#       my $region = Overlayfilename2region($name);

#   } elsif ($path eq $anim){
#     # animation

#       # Files look like
#       #
#       #     Region_yyyymmddhhmm.mov
#       #
#       # Where 'region' is one of the regions defined in the defaults
#       # file: $VAP_LIBRARY/auto_movie_defs.dat
#       #
#       # We also need the first frame, whose name will be
#       # `wind.001.ext.' So we check for the $self->{EXT} field here.
#       #

#       my $region = $self->Animfilename2region($name);

#   } elsif ($path eq $ts) {
#     # Tropical storm

#         # ---- It's a tropical storm image ----------
#         # The format for these files is 
#         #
#         #      SAT-STORMTYPE-NAME-DATE-TYPE.jpg
#         #
#         #    where 
#         #
#         #        SAT = GOES(8|10) or GMS5
#         #        STORMTYPE = TYP|DEP|STO
#         #        NAME = the storm's name
#         #        DATE = the date of the data used
#         #        TYPE = Q|S Q=QuikSCAT.
#         #

#       my $region= $self->TSfilename2region($name);


#   } else {
#     $self->{ERROROBJ}->_croak("Unknown path!\n$path\n",
# 		  "VapWebsite::filename2region: Path error!");
#   }
  
#   1;
# }


=pod

=head1 TSfilename2region

=head2 Usage: $region = $self->TSfilename2region($file)

       Parses the filename to return the webspage `region' this file
       will live in.

=cut

sub TSfilename2region{
  my ($self, $file) = @_;
  my ($region, $stormtype, $name, $date,$insttype) = 
    ($file =~ /(\w+)-(\w+)-(\w+)-(\d+)-(\w+)\..*/);
  $region;
}


=pod

=head1 Animfilename2region

=head2 Usage: $region = Animfilename2region($file)

       Parses the filename to find which webspace `regin' this file is in.

=cut


sub Animfilename2region{

  # files look like nepac.mov
  my ($self, $name) = @_;
  my @allowed_extensions = @{$self->{WEB}->{DEFS}->{ALLOWED_EXTENSIONS}};
  my ($region, $path, $ext) = fileparse($name, @allowed_extensions);
  uc($region);
}



=pod

=head1 Overlayfilename2region

=head2 Usage: $region = Overlayfilename2region($file)

       Parses the filename to find which webspace `regin' this file is
       in.

=cut


sub Overlayfilename2region{

  # names look like GOES_10_IR4_NEPAC_1_Q.jpeg

  my ($self, $name) = @_;
  my ($sat, $satnum, $sensor, $region, $num, $type) = split(/_/,$name);
  $region = join("_", ($sat, $satnum, $VapUtil::sensorname2num{lc($sensor)}, $region, $num));
}

=pod

=head1 startPage

=head2 Usage: $self->startPage($title, $keywords, $h1,$isTropicalStorm, $order );

=over 4

=item * $title: The title of this webpage

=item * $keyword: reference to an array that will go into the `meta' tag.

=item * $h1: The level 1 header of this page

=item * $isTropicalStorm: flag, to tell whether this is a tropical
                          storm page.

=item * $order: A hash containing the order to put the item in the navigation bar.

=back

=cut


sub startPage{
  my ($self, $title, $keywords, $h1,$isTropicalStorm, $order) = @_;
  my $q=$self->{CGI};

  my $html = $q->start_html(
		 -title=>$title,
		 -meta=>{"Keywords" => $keywords},
		# -background=>"/images/fuji7.gif",
		 -style=>{
		      -code=>'A:link {text-decoration:none;},A:visited {text-decoration:none;}'
		     },
		 -link=>"336699", -alink=>"003366",-vlink=>"336699",-marginheight=>"0",
		 -topmargin=>"0", -leftmargin=>"0", -onload=>"init()", -onresize=>"redo()",
		 -script=>{-language=>"javascript", 
			   -code=>"var _OLD_ONERROR = window.onerror; window.onerror=null"},
		 -script=> {-language=>"javascript", 
			    -src=>"/js/ua.js"},
		 -script=> {-language=>"javascript1.2", 
			    -src=>"/js/base.js"},
		 -script=> {-language=>"javascript1.2", 
			    -src=>"/js/breadcrumbs.js"},
		 -script=> {-language=>"javascript",
			    -code=>"
                           if (navigator.version < 5)
                                 window.onerror = _OLD_ONERROR = defaultOnError;
                           else
                              window.onerror = _OLD_ONERROR;
                           "},
		 -script=>{-language=>"JavaScript", 
			   -code=>"function blockError(){return true;}
                              window.onerror = blockError;"},
		 -script => { -language=>"JavaScript1.2", -src=>"/js/xbStyle.js"},
		 -script => { -language=>"JavaScript1.2", 
			      -src=>"/js/xbCollapsibleLists.js"},
		 -script => { -language=>"JavaScript1.2", 
			      -src=>"/js/viewSource.js"},
		 -script => { -language=>"JavaScript", 
			      -code=> "function init() { return true; }"} 
		);

    # Outermost table, contains the entire content of the page.

  my $outsidetable = HTML::Table->new(-rows=>3,
				      -cols=>2,
				      -align=>"left",
				      -width=>"100\%",
				     -border=>1);

  $outsidetable->setColWidth(1,"20\%");

    # For the top navbar
  my $topnavtable=TopNavTable->new();

    # For the left navbar
  my $leftnavtable=LeftNavTable->new(-isTropicalStorm => $isTropicalStorm,
				     #-width=>"20\%",
				     WEBHOST => $self->{WEB}->{DEFS}->{WEBHOST},
				     ORDER => $order);

  my $toptablehtml = $topnavtable->getTable;
  $toptablehtml .=  $q->h1($h1);
  $outsidetable->setCell(1,1,$toptablehtml);
  $outsidetable->setCellColSpan(1,1,2);
  $outsidetable->setCell(2,1,$leftnavtable->getTable);
  $outsidetable->setCellVAlign(2,1,'top');

    # We can't convert to the table to HTML just yet, because we have
    # to put the `body' table in it first.

  $self->{OUTSIDETABLE} = $outsidetable;
  $self->{HTML} = $html;
  1;
}

=pod

=head1 endPage

=head2 usage: $finalhtml = $self->endPage($bodytable)

       Given the HTML::Table `$bodytable', generate all the HTML
       needed to end the page. Returns the finished html code as a
       string, which can then be written to the file.

=cut 

sub endPage{
  my ($self,$body) = @_;
  $self->setBody($body);
  my $html = $self->{HTML} . $self->{OUTSIDETABLE}->getTable;
  $html .= $self->imagemaps;
  my $q = $self->{CGI};
  $html .= $q->end_html;
}


=pod

=head1 setBody

=head2 Usage: $self->setBody($bodytable)

       Sets the appropriate cell in the outermost table with the html
       table generated from the call to the input variables `getTable'
       method. Returns 1.

=cut

  
sub setBody{
  my ($self,$body) = @_;
  $self->{OUTSIDETABLE}->setCell(2,2,$body->getTable);
  1;
}


=pod

=head1 imagemaps

=head2 usage: $maps =$self->imagemaps;

       Returns the HTML code implementing the various imagemaps used
       in the top navbar.

=cut

sub imagemaps{
  my $self=shift;
  my $q=$self->{CGI};
  my $maps = $q->map({-name=>"nasa-home", 
		    -area=>[
			    {-ALT=>"NASA",
			     -SHAPE=>"circle",
			     -COORDS=>"56,36,31",
			     -HREF=>"http://www.nasa.gov",
			     -TARGET=>"_new"},
			    {-ALT=>"'Winds' Home Page",
			     -COORDS=>"159,6,380,88",
			     -HREF=>"http://winds.jpl.nasa.gov/index.html",
			     -SHAPE=>"RECT",
			     -TARGET=>"_new"}
			   ]
		   });
  $maps .= $q->map( {-name=>"jpl-caltech",
		     -area=>[ {-ALT=>"Jet,Propulsion,Laboratory",
			       -COORDS=>"6,60,159,73",
			       -HREF=>"http://www.jpl.nasa.gov",
			       -TARGET=>"_new",
			       -SHAPE=>"RECT"
			      },
			      {-ALT=>"California,-Institute of Technology",
			       -COORDS=>"14,72,159,87",
			       -HREF=>"http://www.caltech.edu",
			       -TARGET=>"_new",
			       -SHAPE=>"RECT"
			      }
			    ]
		    }
		  );

  $maps;
}


=pod

=head1 goodLink

=head2 usage: 1|0 = goodLink($symlink);

       Returns 1 if the input $symlink points to a an existing file.
       If the input file is not a symbolic link or if some system
       error occurs, or, if it *is* a symbolic link but the target
       file doesn't exist, this routine returns 0.

       This routine assumes that you've set your directory to the
       directory in which the symlink resides, so that relative links
       are handled correctly.

=cut

sub goodLink{
  my ($self, $link) = @_;
  my $test = readlink($link);
  return 0 if !$test || (! -e $test);
  return 1;
}
1;
