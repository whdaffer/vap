=head1 VapWebsite

=head2 SYNOPSIS

       Object encapsulating code to maintain VAP website.

=head2 USAGE

       my $vapwww = VapWebsite->new(FILE => `file', [EXT => `extension'])

=over 2

=item * FILE: FULLY QUALIFIED name of the file to move. All necessary
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

    obj = VapWebsite->new(FILE => 'file')

    'file' is the result of some processor (overlay, animation,
             tropical storm overlay). This object moves the file to
             the proper location in the web space and updates the
             proper webpage.

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
		"VapWebsite: non-existent file!") unless ($size > 0);

  $self->{MTIME} = $mtime;
  $self->{SIZE} = $size;

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

      my ($number,$ext);
      my ($sat, $satnum, $sensor, $date, $limits, $region,$type) = split(/_/,$name);
      ($type,$ext) =~ split ".", $type;
      $self->{SYMLINK} = $ENV{VAP_WWW_TOP} ."/images/" . 
	join("_", ($sat, $satnum, $sensor, $region, $type)) . "$ext";
      $self->{TYPE} = "OVERLAY";
      $self->{REGION} = join('_',($sat,$satnum,
				  $VapUtil::sensorname2num{$sensor},
				  $region));
      $self->{WHICH_SEAWINDS} = $type;
      $self->{DESTINATION} = $ENV{VAP_OVERLAY_ARCHIVE};
      $self->{WEBPAGE} = $type2webpage{$self->{TYPE}} . "_$type.html";

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
    $self->{DATE} = $date;
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
  copy $src, $destination or 
    $self->_croak( ["Error copying", $src, "to ", $destination],
		   "VapWebside::movefile. Copy Error!");


  if ($type =~ /OVERLAY/i){

    my $symlink = $self->{SYMLINK};
    $self->redoSymlink($destination, $symlink);

  } elsif ($type =~ /ANIMATION/i) {

    my $ext = $self->{EXT};
    my ($basename, $path) = fileparse($file);
    my $first_frame = $path . "/wind.001." . $ext;
    my $wwwframe = $self->{WWW_TOP}. "/images/$region.001.$ext";
    $self->{WWWFRAME} = $wwwframe;

    unlink $wwwframe;
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
    $self->redoSymlink($self->{WWWFILE},$symlink);

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

=pod

=head1 redoSymlink

=head2 Usage:

      $status = redoSymlink($file, $symlink)

      Create a symlink from $file to $symlink, deleting $symlink if it
      already exists. Meant to create the symlink, which is what the
      webpage points refers to, with the actual file to be displayed.

      The routine 'dies' if unsuccesfi, so if it returns at all, it
      succeeded.



=cut


sub redoSymlink{
  my ($self,$file, $symlink) = @_;

  if (-e $symlink) {
    unlink $symlink or 
      $self->_croak("$0: Can't unlink $symlink:$!\n",
		    "$0: DELETE ERROR!");
  }
  symlink $file,$symlink or 
    $self->_croak("$0: Can't symlink $symlink to $file!:$!\n",
		  "$0: SYMLINK ERROR!");
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



sub UpdateWebsite{
  my $self=shift;
  my $file=$self->{FILE};
    # Move the file to the website, update the links.

  my $status = $self->moveFile();
  my $msg = "Unable to move $file to webspace!\n";

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

    # With the exception of TROPICAL_STORM type, which has to be
    # handled differently, here find the mtime of each of the files
    # for this type, store the information in $self, and then use it
    # to rewrite this particular webpage.

  my $html;
  if ($type =~ /TROPICAL_STORM/i) {

      # Tropical Storm.

  } elsif ($type =~ /OVERLAY/i){

      # Regular overlay

    my $re = $self->{WHICH_SEAWINDS} . "\?:.*_\\.((jpg|JPG|jpeg|JPEG)\$";

    @imagedir_files = grep(/$re/, @imagedir_files);

    foreach (@imagedir_files){

        # All of these files are links to the actual files, which live
        # in $VAP_OVERLAY_ARCHIVE, but stat() stats what the link is
        # pointing to, not the link itself.

      my $file= "$dir/$_";
      my $region = $self->Overlayfilename2region($_);
      my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,
	  $size,$atime,$mtime,$ctime,$junk)=stat($file);

      my $self->{WEBSPACE}->{$region} = [$_,$size/1024.0, $mtime];


    }



    my $q=$self->{CGI};
    my @order = @{$self->{DEFS}->{OVERLAY}->{ORDER}};
    my $title=$self->{DEFS}->{OVERLAY}->{TITLE};
    my $keywords =$self->{DEFS}->{OVERLAY}->{META}->{KEYWORDS};
    my $h1=$self->{DEFS}->{OVERLAY}->{HEADING1};

      #### ===== here we start building the page ======= ###


    $html=$self->startPage($q,$title,$keywords,$h1);


    my $nrows = @order;
    my $table = HTML::Table(-rows => $nrows, -cols=>1);
    my $row=1;

    foreach (@order){

      my ($file,$size,$mtime)=$self->{WEBSPACE}->{$_};
      my $name = $self->{OVERLAY}->{DEFS}->{$_}->{WEB}->{NAME};
      my $string = "Overlay for the ";
      my $statstring = "Created: " . scalar(localtime($mtime)) . "\n";
      $statstring .= "Size: $size (Kb)\n";
      my $linelink = $q->a({-href=>"images/$file",
			   -name=>"'" . $_ . "'",
			   -align=>'left'},
			   $name );
      my $img = $q->img({-src=>"/images/$file",
			-valign=>'top'});

      my $clickableimage = $q->a({-href=>"images/$file",
				  -alt=>"'" . split(/_/,$_) . "'",
				  -valign=>'top',
				  -width=>'200',
				  -height=>'150',}, $img);


      my $p1 = $q->p({-align=>'left'}, "$string$linelink\n$statstring\n");
      my $p2 = $q->p({-align=>'left'}, $clickableimage);
      my $content = "$p1\n$p2\n";

      $table->setCell($row++, 1, $content);
    }

    $html .= $table;
    $html .= $q->end_html;

  } else {

      # Animation!

    @imagedir_files = grep(/mov$/i, @imagedir_files);
    foreach (@imagedir_files){
      my $file = "$dir/$_";
      my $region = Animfilename2region($_);
      my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,
	  $size,$atime,$mtime,$ctime,$junk)=stat($file);
      my $self->{WEBSPACE}->{$region} = [$_, $size/1024.0, $mtime];
    }


    my @order = @{$self->{DEFS}->{ANIMATION}->{ORDER}};
    my $nrows = @order;
    my $table = HTML::Table(-rows => $nrows, -cols=>1);
    my $row=1;

    my $q=$self->{CGI};
    my $title=$self->{DEFS}->{OVERLAY}->{TITLE};
    my $keywords =$self->{DEFS}->{OVERLAY}->{META}->{KEYWORDS};
    my $h1=$self->{DEFS}->{OVERLAY}->{HEADING1};

      #### ===== here we start building the page ======= ###


    $html=$self->startPage($q,$title,$keywords,$h1);

    foreach (@order){

      my ($file,$size,$mtime)=$self->{WEBSPACE}->{$_};

      my $string = "Animation for the ";
      my $statstring = "Created: " . scalar(localtime($mtime)) . "\n";
      $statstring .= "Size: $size (Kb)\n";
      my $name = $self->{ANIMATION}->{DEFS}->{$_};
      my $linelink = $q->a({-href=>"/images/$file",
			   -name=>"'" . $_ . "'",
			   -align=>'left'},
			   $name );
      my $frame = "/images/". $_ . ".001.frame.mov";
      my $img = $q->img({-src=>"'$frame'",
			-valign=>'top'});

      my $clickableimage = $q->a({-href=>"/images/$file",
				  -alt=>"'" . split(/_/,$_) . "'",
				  -valign=>'top'}, $img);


      my $p1 = $q->p({-align=>'left'}, "$string$linelink\n$statstring\n");
      my $p2 = $q->p({-align=>'left'}, $clickableimage);
      my $content = "$p1\n$p2\n";

      $table->setCell($row++, 1, $content);
    }

    $html .= $table . $q->end_html;
  }

  $html;
}

=pod

=head1 filename2region

=head2 Usage:

       $region = filenam2region( $file );

       Returns the `region' of the input filename, which must reside
       in the websapce and be fully qualified, since the initial
       determination about the file is made by the path) This `region'
       is the `key' to the information about the file. For regular
       overlays, it's the `key' into the overlay_defs_oo hash
       (e.g. GOES_10_4_NEPAC_1), in the case of Tropical storms, it's
       a key in the ...{WEB}->{... subhash of $tsoo_defs, defined in
       tropical_storm_defs_oo, (e.g. {WEB}->{GOESEAST}.) In the case
       of the animations, it's one of the values of 'desig' field in
       the structure defined in auto_movie_defs.dat. These three files
       are found in $VAP_LIBRARY.

       As is the case with the other routines, this subroutine only
       returns on succes, otherwise it `dies.'


=cut

sub filename2region{
  my ($self, $file)= @_;

  $self->_croak("Missing file parameter!\n", 
		"VapWebsite::filename2region: Argument Error");
  my ($name, $path ) = fileparse($file);
  $self->_croak("File is not fully qualified!\n$file\n", 
		  "VapWebsite::filename2region: NO PATH ") if (!$path);
  my $overlay = $ENV{VAP_OVERLAY_ARCHIVE};
  my $anim = $ENV{VAP_ANIM_ARCHIVE};
  my $ts = $ENV{VAP_TS_ARCHIVE};

  if ($path eq $overlay){
    # plain old overlay


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

      my $region = Overlayfilename2region($name);

  } elsif ($path eq $anim){
    # animation

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

      my $region = $self->Animfilename2region($name);

  } elsif ($path eq $ts) {
    # Tropical storm

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

      my $region= $self->TSfilename2region($name);


  } else {
    $self->_croak("Unknown path!\n$path\n",
		  "VapWebsite::filename2region: Path error!");
  }
  
  1;
}

sub TSfilename2region{
  my ($self, $name) = @_;
  my ($region, $stormtype, $name, $date,$insttype) = 
    ($name =~ /(\w+)-(\w+)-(\w+)-(\d+)-(\w+)\..*/);
  $region;
}

sub Animfilename2region{
  my ($self, $name) = @_;
  my ($number, @junk);
  my ($sat, $sensor, $date, $limits, $type, $region) = split(/_/,$name);
      ($region,$number,@junk) = $region =~ /(\w+)_(\d)?\.\w+/;
  $region;
}

sub Overlayfilename2region{
  my ($self, $name) = @_;
  my ($sat, $satnum, $sensor, $date, $limits, $region, $junk) = split(/_/,$name);
  $region = join("_", ($sat, $satnum, $sensornum2name{$sensor}, $region));
}

sub startPage{
  my ($self, $q, $title, $keywords, $h1) = @_;
  my $html = $q->start_html(
		 -title=>$title,
		 -meta=>{"Keywords" => $keywords},
		 -background=>"file:///disk2/vap/www/htodcs/images/fuji7.gif",
		 -style=>{
		      -code=>'A:link {text-decoration:none;},A:visited {text-decoration:none;}'
		     },
		 -link=>"336699", -alink=>"003366",-vlink=>"336699",-marginheight=>"0",
		 -topmargin=>"0", -leftmargin=>"0", -onload=>"init()", -onresize=>"redo()",
		 -script=>{-language=>"javascript", 
			   -code=>"var _OLD_ONERROR = window.onerror; window.onerror=null"},
		 -script=> {-language=>"javascript", 
			    -src=>"file:///usr/people/vapdev/development/vap/html/js/ua.js"},
		 -script=> {-language=>"javascript1.2", 
			    -src=>"file:///usr/people/vapdev/development/vap/html/js/base.js"},
		 -script=> {-language=>"javascript1.2", 
			    -src=>"file:///usr/people/vapdev/development/vap/html/js/breadcrumbs.js"},
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
		 -script => { -language=>"JavaScript1.2", -src=>"file:///usr/people/vapdev/development/vap/html/js/xbStyle.js"},
		 -script => { -language=>"JavaScript1.2", 
			      -src=>"file:///usr/people/vapdev/development/vap/html/js/xbCollapsibleLists.js"},
		 -script => { -language=>"JavaScript1.2", 
			      -src=>"file:///usr/people/vapdev/development/vap/html/js/viewSource.js"},
		 -script => { -language=>"JavaScript", 
			      -code=> "function init() { return true; }"} 
		);
  $html .= $q->h1($h1);
}
1;
