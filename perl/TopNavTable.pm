# $Id$
#
# $Log$
#
#
package TopNavTable;
use strict;
use Carp;
use HTML::Table;
@TopNavTable::ISA= qw/HTML::Table/;
use CGI;
use VapUtil;

sub new {

  my $class = shift;
  my $self=$class->SUPER::new(-border=>"0",
			     -cellpadding=>"0",
			     -cellspacing=>"0");
  
  # Get the defaults for the various VAP products, as well as for the
  # website itself

  #website defaults:

  # Some useful variables

  my ($row, $col, $key, $value, $content, $link, $img, $q);

    # The outermost table, which will contain the entire navbar. Each
    # of the subsequent tables will cells in this table.
  $q=CGI->new();
  $self->setCell(1,1,$q->img({-src=>"images/blackbar_left.jpg",-width=>"217",
		   -height=>"19",-border=>"0", -alt=>"JPL Navbar"}));
  $self->setCellAlign(1,1,"LEFT");
  $self->setCellVA(1,1,"TOP");
  $self->setCellWidth(1,1,"217");

    # The JPL Home pick
  $img = $q->img({-SRC=>"images/blackbar_jplhome_off.jpg", -WIDTH=>"65", -HEIGHT=>"19",
		  -BORDER=>"0",-ALT=>"JPL Home"});
  $content = $q->a({-href=>"http://www.jpl.nasa.gov",-target=>"_new"},$img);
  $self->setCell(1,2,$content);
  $self->setCellAlign(1,2,"LEFT");
  $self->setCellVA(1,2,"TOP");
  $self->setCellWidth(1,2,"65");

    # The Earth pick
  $img = $q->img({-SRC=>"images/blackbar_earth_off.jpg", -WIDTH=>"48", -HEIGHT=>"19",
		  -BORDER=>"0",-ALT=>"Earth"});
  $content = $q->a({-href=>"http://www.jpl.nasa.gov/earth/earth_index.html",-target=>"_new"},$img);
  $self->setCell(1,3,$content);
  $self->setCellAlign(1,3,"LEFT");
  $self->setCellVA(1,3,"TOP");
  $self->setCellWidth(1,3,"48");

    # The Solar system pick
  $img = $q->img({-SRC=>"images/blackbar_solarsys_off.jpg", -WIDTH=>"86", -HEIGHT=>"19",
		  -BORDER=>"0",-ALT=>"Solar System"});
  $content = $q->a({-href=>"http://www.jpl.nasa.gov/solar_system/solar_system_index.html",
		    -target=>"_new"},$img);
  $self->setCell(1,4,$content);
  $self->setCellAlign(1,4,"LEFT");
  $self->setCellVA(1,4,"TOP");
  $self->setCellWidth(1,4,"86");


    # The Stars pick
  $img = $q->img({-SRC=>"images/blackbar_solargal_off.jpg", -WIDTH=>"103", -HEIGHT=>"19",
		  -BORDER=>"0",-ALT=>"Stars &amp Galaxies"});
  $content = $q->a({-href=>"http://www.jpl.nasa.gov/stars_galaxies/stars_galaxies_index.html",
		    -target=>"_new"},$img);
  $self->setCell(1,5,$content);
  $self->setCellAlign(1,5,"LEFT");
  $self->setCellVA(1,5,"TOP");
  $self->setCellWidth(1,5,"103");


  $img = $q->img({-SRC=>"images/blackbar_tech_off.jpg", -WIDTH=>"80", -HEIGHT=>"19",
		  -BORDER=>"0",-ALT=>"Technology"});
  $content = $q->a({-href=>"http://www.jpl.nasa.gov/technology/technology_index.html",
		    -target=>"_new"},$img);
  $self->setCell(1,6,$content);
  $self->setCellAlign(1,6,"LEFT");
  $self->setCellVA(1,6,"TOP");
  $self->setCellWidth(1,6,"80");

  $self->setCell(1,7,$q->img({-src=>"images/spacer.gif",-width=>"303",
		   -height=>"19",-border=>"0", -alt=>"JPL Navbar"}));
  $self->setCellVA(1,7,"TOP");
  $self->setCellWidth(1,7,"303");
  $self->setCellBGColor(1,7, "#000000")

  $self->setCell(1,8,$q->img({-src=>"images/blackbar_right1.jpg",-width=>"80",
		   -height=>"19",-border=>"0", -alt=>"JPL Navbar"}));
  $self->setCellVA(1,8,"TOP");
  $self->setCellAlign(1,8,"RIGHT");
  $self->setCellWidth(1,8,"80");
  $self->setCellBGColor(1,8, "#000000")



  return bless $self, ref($class) || $class;
}

1;
