=head1 VapWebsite

=head2 SYNOPSIS

       Object encapsulating code to maintain VAP website.

=head2 USAGE

       my $vapwww = VapWebsite->new(FILE => `file' | UPDATE=>product,
                                    PROCESSOR_DEFAULTS => hash 
                                  [,errorobj = VapErrorObject])


=over 2

=item * FILE: FULLY QUALIFIED name of the file to move. All necessary
             information as to where the file lives should be
             contained in the name. 



=item * UPDATE: name of a Product. This option allows for updating 
                webpages without actually having to run a processor.

        UPDATE must equal one of the `regions' used in calling one of
        the particular processors, i.e. one of those inputs that
        follow the --region switch.

        Examples are: GOES_10_4_NEPAC_1 for overlays, NEPAC for
        animations and GOESWEST for tropical storms.

        FILE or UPDATE is REQUIRED!

=item * WINDFILTER: When using UPDATE to update overlay or tropical
                    storm webpagers, one needs the WINDFILTER keyword
                    to further differentiate between QuikSCAT and
                    SeaWinds webpages.


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
use Time::Local;
use Data::Dumper;

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

    obj = VapWebsite->new(

                      (FILE => 'file',

                          -- or --

                       UPDATE=> 'product_type'[, WINDFILTER=>'filter',])
                          ,PROCESSOR_DEFAULTS => hash
                          [,errorobj=errorobject])

    -- where --

    'FILE'   is the result of some processor (overlay, animation,
             tropical storm overlay). This object moves the file to
             the proper location in the web space and updates the
             proper webpage.

            -- or, to just update the website for a particular product --

    UPDATE and, depending on the product, WINDFILTER

    WINDFILTER is required for those updates (the overlays) that
    require further differentiation between products based on QuikSCAT
    ro SeaWinds data.

    One or the other is required, either FILE alone or UPDATE and
    possibly WINDFILTER.

    The 'product_type' is one of OVERLAY, ANIMATION or TROPICAL_STORM.


    PROCESSOR_DEFAULTS: the defaults used by whatever processor
                        created FILE, that is, the contents of files
                        overlay_defs_oo, tropical_storm_defs_oo or
                        auto_movie_defs.dat, contained in VAP_LIBRARY.


EOF


}

use lib $ENV{VAP_SFTWR_PERL};
use lib $ENV{VAP_LIBRARY};
#require "VapWebsite_defs" or die "Can't `require' VapWebsite_defs\n";
use VapCGI;
use HTML::Table;
use LeftNavTable;
use TopNavTable;
use BottomNavTable;
use Overlay;
use Animate;
use OTS;
use VapUtil;
use VapError;

sub new{
  my $class = shift;
  my $self={@_};
  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};
  bless $self, ref($class) || $class;

  my $defs = "VapWebsite_defs";
  my $defsfile = $ENV{VAP_LIBRARY} . "/$defs";
  open DEFSFILE, "<$defsfile" or 
    $self->{ERROROBJ}->_croak("Can't open $defsfile\n", 
				 "VapWebsite: open ERROR");
  do {
    local $/=undef;
    my $lines = <DEFSFILE>;
    eval $lines;
  };
  close DEFSFILE;

  $self->{WEB}->{DEFS} = $website_defs;
  $self->{CGI} = VapCGI->new(-nodebug=>1);
  $self->{BOTTOMTABLE}= BottomNavTable->new;

  if ($self->{UPDATE}){

    my $update = $self->{UPDATE};

    if ($update =~ /OVERLAY|TROPICAL/ && !$self->{WINDFILTER}) {
      $self->{ERROROBJ}->_croak("OVERLAY|TROPICAL_STORM: Need windfilter!\n",
	   "VapWebsite::new (update) No windfilter for overlay products!") 
    }

    my %type2webpage = %{$self->{WEB}->{DEFS}->{TYPE2WEBPAGE}};

    if ($update =~ /OVERLAY/) {
      my $insttype = $self->{WINDFILTER};
      my $overlay_defs = Overlay->new(GET_DEFS => 1,
				     ERROROBJ => $self->{ERROROBJ});
      $self->{OVERLAY}->{DEFS} = $overlay_defs;
      $self->{WHICH_SEAWINDS} = $insttype;
      $self->{TYPE} = "OVERLAY";
      $self->{DESTINATION} = $ENV{VAP_OVERLAY_ARCHIVE};
      $self->{WEBPAGE} = $type2webpage{$self->{TYPE}} . "_$insttype.html";

    } elsif ($update =~ /TROPICAL_STORM/) {


      my $insttype = $self->{WINDFILTER};
      my $tsdefs = OTS->new(GET_DEFS => 1,
			   ERROROBJ=>$self->{ERROROBJ});
      $self->{WHICH_SEAWINDS} = $insttype;
      $self->{TROPICAL_STORM}->{DEFS} = $tsdefs;

      my ($sec,$min,$hour,$month,$year) = localtime(time);
      $year += 1900;
      $self->{YEAR} = $year;
	$self->{TYPE} = "TROPICAL_STORM";
      $self->{DESTINATION} = $ENV{VAP_TS_ARCHIVE} . "/$year";
      $self->{WHICH_SEAWINDS} = $insttype;
	$self->{WEBPAGE} = 
	  $type2webpage{$self->{TYPE}} . "_" . $insttype . ".html";

      $self->read_ts_manifest;

    } elsif ($update =~ /ANIMATION/) {

      my $animdefs = Animate->new(GET_DEFS => 1,
				 ERROROBJ=>$self->{ERROROBJ});
      $self->{ANIMATION}->{DEFS} = $animdefs;
      $self->{TYPE} = "ANIMATION";
      $self->{DESTINATION} = $ENV{VAP_ANIM_ARCHIVE};
      $self->{WEBPAGE} = $type2webpage{$self->{TYPE}} .".html";

    } elsif ($update =~ /INDEX/){

      $self->{TYPE} = "INDEX";
      $self->{WEBPAGE} = $type2webpage{$self->{TYPE}} . ".html";

    } else {
      $self->{ERROROBJ}->_croak("Unknown update product: <$update>!\n",
		   "VapWebsite::new (update) Unknown update product!");
    }

  } else {

      # Not and 'UPDATE'. need a filename!

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

    my ($name0, $path0) = fileparse($0);
    $self->{NAME0} = $name0;
    $self->parseFilename;

  }

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
    if ($name =~ /-(TYP|HUR|CYC|DEP|STO)-/) {

        # ---- It's a tropical storm image ----------
        # The format for these files is 
        #
        #      SAT-STORMTYPE-NAME-DATE-TYPE.jpg
        #
        #    where 
        #
        #        SAT = GOES(8|10) or GMS5
        #        STORMTYPE = CYC|HUR|TYP|DEP|STO
        #        NAME = the storm's name
        #        DATE = the date of the data used
        #        TYPE = Q|S Q=QuikSCAT.
        #

      my ($region, $stormtype, $name, $date,$insttype) = split /-/,$name;
      $self->{REGION} = $region;

      my $year = substr($date,0,4);
      $self->{YEAR} = $year;

        # Just to make the code a little more readible!
      $self->{TROPICAL_STORM}->{DEFS} = $self->{PROCESSOR_DEFAULTS};
      delete $self->{PROCESSOR_DEFAULTS};

        # If we're close to the new year, we have to look at the
        # previous year too!

      my $startofyear = timelocal(1,0,0,1,0,$year-1900);
      my $active_time = $self->{WEB}->{DEFS}->{TS_ACTIVE_TIME};
      $self->read_ts_manifest;

      #$self->{ALSO_CHECK} = $year-1 
	#if (($^T - $startofyear) <= $active_time);

      $self->{TYPE} = "TROPICAL_STORM";
      $self->{DESTINATION} = $ENV{VAP_TS_ARCHIVE} . "/$year";
      $self->{WHICH_SEAWINDS} = $insttype;
      $self->{WEBPAGE} = 
	$type2webpage{$self->{TYPE}} . "_" . $insttype . ".html";


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
  } elsif ($type =~ /TROPICAL_STORM/) {
    ;
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

  if (!$self->{UPDATE}) {
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

  }

    # +++ CD to the images directory +++

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
  my $bodytable;

    # With the exception of TROPICAL_STORM type, which has to be
    # handled differently, here find the mtime of each of the files
    # for this type, store the information in $self, and then use it
    # to rewrite this particular webpage.


  if ($type =~ /TROPICAL_STORM/i) {

      # --------------- Tropical Storm ------------------


    my @order = @{$order->{TROPICAL_STORM}};
    my $ts_defs = $self->{TROPICAL_STORM}->{DEFS};
    my $stormranking2description = $ts_defs->{STORMRANKING2DESCRIPTION};
    my $ts_web = $ts_defs->{WEB};
    my $hash = $self->getTSlist;
    my $nrows=@order;
    $bodytable = HTML::Table->new(-rows=>$nrows,
				  -cols=>1,
				  -width=>"80\%");
    my $q=$self->{CGI};
    my $title=$self->{WEB}->{DEFS}->{TROPICAL_STORM}->{TITLE};
    my $keywords =join ",", @{$self->{WEB}->{DEFS}->{TROPICAL_STORM}->{META}->{KEYWORDS}};
    my $h1=$self->{WEB}->{DEFS}->{TROPICAL_STORM}->{HEADING1};
    my $h2=$self->{WHICH_SEAWINDS} =~ /Q/? "QuikSCAT data":"Data from SeaWinds on ADEOS-II ";


    $self->startPage($title,$keywords,$h1,$type,$order,$h2);
    my $row=1;
    my ($table_this_storm,$table_this_region);

    foreach my $region (@order){
      if ($hash->{$region}) {

	my $n_storms_in_region = keys(%{$hash->{$region}});
	my $caption = $q->a({-name=>"$region"},$ts_web->{$region}->{NAME});
	$table_this_region = HTML::Table->new( -rows=>$n_storms_in_region,
					       -cols=>1,
					     -border=>1);
	$table_this_region->setCaption($caption,'TOP');

	my @sorted_storms = $self->sortby_time_last_seen($hash->{$region});

	my $region_row=1;

	foreach my $name (@sorted_storms){
	  my $this_storm = $hash->{$region}->{$name};
	  my @files=@{$this_storm->{FILES}};
	  my $n_storm_rows = @files;
	  $table_this_storm=HTML::Table->new( -rows=>$n_storm_rows,
					      -cols=>1);
	  $table_this_storm->setCaption($name,'TOP');
	  my $storm_row=1;

	  foreach my $f (@files){
	    my ($sat2, $storm_type2, $name2, $date2,$year2, $type2) = 
	      parsename($f);
	    my $file_in_archive = $ENV{VAP_TS_ARCHIVE} . "/$year2/$f";
	    my ($size,$mtime) = (stat($file_in_archive))[7,9];

	    $size = int($size/1024);

	    my ($y, $m, $d, $h, $mm) = 
	      ($date2 =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/);

	    $date2 = "$y/$m/$d/$h/$mm";
	    my $string = $stormranking2description->{$storm_type2};
	    $string .= " $date2";
	    my $alt = "$sat2 $storm_type2 $name2 $date2 $type2";
	    my $createstring = "Created: " . scalar(localtime($mtime));
	    my $sizestring .= "Size: $size (Kb)";
	    my $infostring = $q->font({-size=>'-1'},
				      $createstring . $q->br() . $sizestring);
	    my $linelink = $q->a({-href=>"/images/storms_archive/$year2/$f",
#				 -name=>"$_",
				 -align=>'top'},
				 $string);
	    my $img = $q->img({-src=>"/images/storms_archive/$year2/$f",
			      -align=>'left',
			       -width=>'200',
			       -height=>'150'});
	    my $clickableimage = $q->a({-href=>"/images/storms_archive/$year2/$f",
					-alt=>"$alt",
					-valign=>'middle',
					-width=>'200',
					-height=>'150'}, $img);


	    my $content = $q->br() . $q->p({-align=>'left'}, "$linelink\n$clickableimage");
	    $content .= $infostring;
	    $table_this_storm->setCellVAlign($storm_row,1,'top');
	    $table_this_storm->setCell($storm_row++,1,$content);

	  } # end loop over files in storm

	  $table_this_region->setCell($region_row++,1,$table_this_storm->getTable);
	  $table_this_storm=undef;

	} # end loop over storms in region

	$bodytable->setCellVAlign($row,1,'top');
	$bodytable->setCell($row++,1,
			    $table_this_region->getTable);


	$table_this_region=undef
      }
    }




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
    my $keywords = join ",", @{$self->{WEB}->{DEFS}->{OVERLAY}->{META}->{KEYWORDS}};
    my $h1=$self->{WEB}->{DEFS}->{OVERLAY}->{HEADING1};
    my $h2=$self->{WHICH_SEAWINDS} =~ /Q/? "QuikSCAT data":"Data from SeaWinds on ADEOS-II ";

      #### ===== here we start building the page ======= ###


    $self->startPage($title,$keywords,$h1,$type,$order,$h2);


    my $nrows = @order;
    $bodytable = HTML::Table->new(-rows => $nrows, 
				  -cols=>1,
				 -width=>"80\%");
    my $row=1;

    foreach (@order){

        # $self->{WEBSPACE} is the list of files in the webspace that
        # need entries in the webpage.

      if ($self->{WEBSPACE}->{$_}){
	my ($file,$size,$mtime)=@{$self->{WEBSPACE}->{$_}};
	my $name = $self->{OVERLAY}->{DEFS}->{$_}->{WEB}->{NAME};
	#my $string = "Overlay for the ";
	my $createstring = "Created: " . scalar(localtime($mtime));
	my $sizestring .= "Size: $size (Kb)";
	my $infostring = $q->font({-size=>'-1'},"$createstring" . $q->br() . "$sizestring");
	my $linelink = $q->a({-href=>"/images/$file",
			     -name=>"$_",
			     -align=>'top'},
			     $name );
	my $img = $q->img({-src=>"/images/$file",
			  -align=>'left',
			   -width=>'200',
			   -height=>'150'});
	my $alt = join( " ", split(/_/,$_));
	my $clickableimage = $q->a({-href=>"/images/$file",
				    -alt=>"$alt",
				    -valign=>'middle',
				    -width=>'200',
				    -height=>'150'}, $img);


	#my $content = $q->p({-align=>'left'}, "$linelink\n$clickableimage$infostring\n");
	#$content .= $q->br() . $q->br();
	my $content = $q->br() . $q->p({-align=>'left'}, "$linelink\n$clickableimage\n");
	$content .= $infostring;
	#$content .= $q->p({-align=>'left'},$createstring);
	#$content .= $q->p({-align=>'left'},$sizestring);
	#$content .= "$createstring\n$sizestring\n$clickableimage";
	#$content .= $q->p({-align=>'left'}, $clickableimage);

	$bodytable->setCell($row++, 1, $content);
      }
    }

  } elsif ($type =~ /ANIMATION/) {

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
    my $keywords = join ",", @{$self->{WEB}->{DEFS}->{ANIMATION}->{META}->{KEYWORDS}};
    my $h1=$self->{WEB}->{DEFS}->{ANIMATION}->{HEADING1};

      #### ===== here we start building the page ======= ###


    $self->startPage($title,$keywords,$h1,$type,$order);

    foreach (@order){

      if ($self->{WEBSPACE}->{$_}) {
	my ($file,$size,$mtime, $first_frame)=@{$self->{WEBSPACE}->{$_}};
	s/'//g;
	#my $string = "Animation for the ";
	my $createstring = "Created: " . scalar(localtime($mtime));
	my $sizestring = "Size: $size (Kb)";
	my $infostring = $q->font({-size=>'-1'}, 
				  "$createstring" . $q->br() . "$sizestring");
	my $name = $self->{ANIMATION}->{DEFS}->{$_}->{WEBNAME};
	$name =~ s/'//g;
	my $linelink = $q->a({-href=>"/images/$file",
					 -name=>"$_",
					 -align=>'top'},
					$name );
	my $img = $q->img({-src=>"/images/$first_frame",
			  -align=>'left'});

	my $alt = join( " ", split(/_/,$_));
	my $clickableimage = $q->a({-href=>"/images/$file",
				    -alt=>"$alt",
				    -valign=>'middle'}, $img);


	my $content = $q->br() . $q->p({-align=>'left'}, "$linelink\n$clickableimage");
	$content .= $infostring;
	$bodytable->setCell($row++, 1, $content);
      }
    }
  } elsif ($type =~ /INDEX/){

    $self->startPage("SeaWinds Daily Wind Report",
		     "Scatterometry, cloud_imagery, overlay, animation," . 
		     "SeaWinds, QuikSCAT, ADEOS-II",
		     "", "OVERLAY",$order);

    $bodytable = HTML::Table->new(-rows => 1, 
				  -cols=>1,
				 -width=>"80\%");
    my $body= <<"EOF";

<p>Welcome to the Value Added Products webpage for QuikSCAT and SeaWinds
on ADEOS-II. This page is merely a filler, you should probably bookmark
the pages you want to view routinely, so that you can go directly there.
I'll present a short explanation on this page for those who've never been
here. There's more specific information concerning the products on this
webpage located on the <a href="/info.html">information</a> page
and more information on the SeaWinds instruments and the two missions carrying
them can be perused at the <a href="http://winds.jpl.nasa.gov">Scatterometer
Project homepage</a> . If we encounter any item of particular interest,
we may put it on the <a href="/disk5/vapdev/www/htdocs/special.html">special
projects</a> page. The status of the various instruments can be found
on the <a href="/disk5/vapdev/www/htdocs/status.html">status</a>
page.
<br>
<br>
<br>
<h2>Synopsis</h2>
This collection of pages contain the output from several processors of
two broad types: 'overlays' and 'animations.'
<br>
<h3>Overlays</h3>

The 'overlays' combine the cloud imagery from several geosynchronous
satellites with SeaWinds scatterometry. They fall into two seperate
types, the simple overlay, which is region specific and 'tropical
storm,' were we try to follow cyclonic storms a little more
closely. Since the geosyncrhonous satellites from which we get our
cloud imagery are region specific, all of the overlays, regardless of
type, have some region dependency. These regions are, roughly: North
Atlantic (GOES8) , North-Eastern Pacific (GOES10) and, in the western
Pacific (GMS5). We'll occasionally experience outages from these
satellites, sometimes extended outages. The general rule is: if either
the wind or the cloud data exists, an overlay will be made.



<p>The overlays are further divided by the source of the scatterometry
data used in the overlay, e.g., there's an <a href="http://overlay_Q.html">QuikSCAT overlay</a>page and an <a href="http://overlay_S.html">SeaWinds overlay page</a>

<h3>Animations</h3>

The animations take data from both scatterometers, create a <a
href="/glossary.html#synoptic">synoptic</a> <a
href="/glossary.html#interpolated">interpolated</a>
field and then create an animation from that field. In this animation,
the windspeed is conveyed by the color of the backgroup, blue is low
windspeed and magenta high, and the direction of the wind by the
motion of the arrows in the animation.  Despite the fact that this
animation shows things moving, this is not a 'time series.' The wind
field here is static, in the sense that at each location in the
picture the direction and speed of the wind doesn't change.

EOF

    $bodytable->setCell(1, 1, $body);    

  } else {
    $self->{ERROROBJ}->_croak("Unknown type! ($type)",
			      "VapWebsite: updateWebsite: unknown type!");
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

=head2 Usage: $self->startPage($title, $keywords, $h1,$type, $order );

=over 4

=item * $title: The title of this webpage

=item * $keyword: reference to an array that will go into the `meta' tag.

=item * $h1: The level 1 header of this page

=item * $type: TROPICAL_STORM or OVERLAY or ANIMATION. what type of product it is.

=item * $order: A hash containing the order to put the item in the navigation bar.

=back

=cut


sub startPage{
  my ($self, $title, $keywords, $h1,$type, $order, $h2) = @_;
  my $q=$self->{CGI};

  my $html = $q->start_html(
		 -title=>$title,
		 -meta=>{"Keywords" => $keywords},
		 -background=>"/images/fuji7.gif",
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
				      -width=>"100\%");


  $outsidetable->setColWidth(1,"15\%");
  $outsidetable->setCellHeight(2,1,"300");

    # For the top navbar
  my $topnavtable=TopNavTable->new();

    # For the left navbar
  my $leftnavtable=LeftNavTable->new(TYPE => $type,
				     #-width=>"20\%",
				     WEBHOST => $self->{WEB}->{DEFS}->{WEBHOST},
				     ORDER => $order);

  my $toptablehtml = $topnavtable->getTable;
  $toptablehtml .=  $q->h1($h1);
  $toptablehtml .= $q->h2($h2) if $h2;
  $toptablehtml .= $q->a({-name=>"TOP"},"");
  $outsidetable->setCell(1,1,$toptablehtml);
  $outsidetable->setCellColSpan(1,1,2);
  $outsidetable->setCell(2,1,$leftnavtable->getTable);
  $outsidetable->setCellVAlign(2,1,'top');
  $outsidetable->setCell(3,1,$self->{BOTTOMTABLE}->getTable);
  $outsidetable->setCellColSpan(3,1,2);
  delete $self->{BOTTOMTABLE};
  $outsidetable->setCellColSpan(3,1,2);
  $outsidetable->setCellVAlign(3,1,'TOP');
  $outsidetable->setCellAlign(3,1,'CENTER');
  #$outsizetable->setCellStyle();

  my $font=$q->font({-face=>"Helvetica",-size=>'-3'},
		    "Last Modified: " . localtime());
  $outsidetable->setCell(4,1,$font);
  $outsidetable->setCellColSpan(4,1,3);
  $font=$q->font({-face=>"Helvetica",-size=>'-3'},
		    "CL 02-2959");

  $outsidetable->setCell(5,1,$font);
  $outsidetable->setCellColSpan(4,1,3);

    # We can't convert to the table to HTML just yet, because we have
    # to put the `body' table in it first. So we save it in the hash for later.

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

sub write_ts_manifest{
  my $self=shift;
  my $manifest = $self->{TROPICAL_STORM}->{MANIFEST};
  my $dumper = Data::Dumper->new([$manifest],[qw(manifest)]);
  my $manifest_file = $self->{WEB}->{DEFS}->{TS_MANIFEST};
  my $dir=getcwd;
  my ($name, $path ) = fileparse($manifest_file);
  chdir $path or 
    $self->{ERROROBJ}->_croak(["Trying to write TS manifest file",
			       "Couldn't CD to $path!"],
			      "VapWebsite: TS, CD error");
  rename $name, "$name.old";
  open MANIFEST, ">$name" or 
    $self->{ERROROBJ}->_croak(["Error opening TS manifest file for write",
			       "$manifest_file\n"],
			     "VapWebsite:TS. Write TS manifest error!");
  print MANIFEST $dumper->Dump;
  close MANIFEST;
  chdir $dir;
  1;
}

sub read_ts_manifest{
  my $self=shift;
  my $manifest_file = $self->{WEB}->{DEFS}->{TS_MANIFEST};
  my $manifest;
  open MANIFEST, "<$manifest_file" or 
    $self->{ERROROBJ}->_croak(["Error reading TS manifest file",
			       "$manifest_file\n"],
				 "Error reading TS MANIFEST");
  do {
    local $/=undef;
    my $lines = <MANIFEST>;
    eval $lines;
  };

  $self->{TROPICAL_STORM}->{MANIFEST} = $manifest;
  1;
}


sub parsename{
  # The files look like GMS5-CYC-01B-200010161251-X.jpeg
  my $file=shift;
  die unless $file;
  my @tmp=split /\./, $file;
  my ($sat, $storm_type, $name, $date,$type)  = split /-/, $tmp[0];
  my ($year, $month, $day, $hour, $min) = $date =~ 
    /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/;
  ($sat, $storm_type, $name, $date,$year, $type);
}

sub bydate{
  my ($sata, $storm_typea, $namea, $datea,$yeara,$typea) = parsename($a);
  my ($satb, $storm_typeb, $nameb, $dateb, $yearb,$typeb) = parsename($b);
  $datea <=> $dateb;
}


sub getTSlist{

    # return the list of storms which will make up the webpage.

  my $self=shift;
  my $manifest = $self->{TROPICAL_STORM}->{MANIFEST};
  my $active_time = $self->{WEB}->{DEFS}->{TS_ACTIVE_TIME};
  my $type_re = ".*-" . $self->{WHICH_SEAWINDS} . "\.jpeg";
  if (!$self->{UPDATE}){
    my $file = $self->{NAME};
    my ($sat, $storm_type, $name, $date,$year, $type) = 
      parsename($file);
    if ($manifest->{$year}->{$name}->{FILES}){
      my @files=@{$manifest->{$year}->{$name}->{FILES}};
      push @files, $file . ".jpeg";
      @files = sort bydate, @files;
      @{$manifest->{$year}->{$name}->{FILES}} = @files;
    } else {
      push @{$manifest->{$year}->{$name}->{FILES}}, $file . ".jpeg";
    }
    $manifest->{$year}->{$name}->{LAST_SEEN} = $self->{MTIME};
    $manifest->{$year}->{$name}->{LAST_SEEN_DATETIME} = 
      scalar(localtime($self->{MTIME}));
    $manifest->{$year}->{ACTIVE}=1;
    $manifest->{$year}->{$name}->{ACTIVE} = 1;

      # We've added the new files ot the manifest. Now save the manifest
      # back to the disk file.

    $self->write_ts_manifest;
  }

  my $storms_hash = ();

    # Loop over the `year's in manifest file. Those more than 1 year
    # old can be set inactive automatically (if they haven't been
    # already). We have to check last year's storms because we may be
    # close to the turn of the year and a storm may have persisted
    # across New Years, or it may be within `active_time' of the
    # current run, in which case we still want to display the
    # storm. At the end of these loops we should have a hash that
    # contains the list of the storms which will make up this page.

  while (my ($year, $year_hash) = each %{$manifest} ) {
    if ($self->{YEAR} - $year > 1){
      if ($year_hash->{ACTIVE}) {
 	  # These storms are all more than a year old, so they can't
	  # be active.
	while (my ($name, $this_storm) = each %{$year_hash} ){
	  $this_storm->{ACTIVE} = 0;
	}
	$year_hash->{ACTIVE} = 0;
      }
    } else {
      
      my $there_are_active_storms = 0;
      while (my ($name, $this_storm) = each %{$year_hash} ){
	next if ref($this_storm) ne "HASH";
	if (($^T-$this_storm->{LAST_SEEN}) > $active_time){
	  $this_storm->{ACTIVE} = 0;
	} else {
	  $there_are_active_storms=1;
	  $storms_hash->{$year}->{$name} = $this_storm;
	  $year_hash->{ACTIVE}=1;
	}
      }
      $year_hash->{ACTIVE} = 0 unless $there_are_active_storms;
    }
  }

    # The hash `storm_hash' still has a 'year' dimension, which might
    # have more than one year in it, if we're close to the turn fo the
    # year. It would be nice to get rid of. What we really want is the
    # hash arranged by 'region' and then by 'storm.' Also, we only
    # want files of the same 'seawinds' origin (Q/S) as the current
    # product. That's what we'll do now.

  my $final_hash = ();
  while (my ($year, $year_hash) = each %{$storms_hash}){
    while (my ($name, $this_storm) = each %{$year_hash}){
      next if ref($this_storm) ne "HASH";      
      my @files = grep /$type_re/, @{$this_storm->{FILES}};
      if (@files){
	foreach my $f (@files){
	  my $region = $self->TSfilename2region($f);
	  push @{$final_hash->{$region}->{$name}->{FILES}}, $f;
	  $final_hash->{$region}->{$name}->{LAST_SEEN} = 
	    $this_storm->{LAST_SEEN};

	}
      }
    }
  }
  $storms_hash = undef;

    # Ahh, but the files in the final hash may not be sorted!


  my @regions = keys %{$final_hash};
  foreach my $region (@regions){
    my $regions_hash = $final_hash->{$region};
    my @storms = keys %{$regions_hash};
    foreach my $name (@storms){
      my @files = @{$regions_hash->{$name}->{FILES}};
      if (@files>1){
	@files = sort bydate @files;

  	  # bydate sorts them in ascending order! I want them to
  	  # appear in the page in DESCENDING order, so that the latest
  	  # image is at the top!

	@files = reverse @files;
	my $last_seen = $regions_hash->{$name}->{LAST_SEEN};

	  # When I tried to simple reset fields within these subhashs,
	  # I ran into some strange things in this code, which seemed
	  # to be ghost references to empty hash locations.  So to be
	  # safe I delete this key to make sure the hash is clean.

	delete $regions_hash->{$name};
	
	@{$regions_hash->{$name}->{FILES}} = @files;
	$regions_hash->{$name}->{LAST_SEEN} = $last_seen;
      }
    }
  }
  $self->{TS_FINAL_HASH} = $final_hash;
  $final_hash;
}

sub sortby_time_last_seen{
  my $self=shift;
  my $hash=shift;
  %VapWebsite::name_vs_lastseen = ();
  while (my ($k, $v) = each %{$hash}){
    $VapWebsite::name_vs_lastseen{$k} = $v->{LAST_SEEN};
  }
  my @sortedkeys = sort bylastseen keys(%VapWebsite::name_vs_lastseen);
  
}

sub bylastseen {
  $VapWebsite::name_vs_lastseen{$b} <=> $VapWebsite::name_vs_lastseen{$a};
}
1;
