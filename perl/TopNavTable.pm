# $Id$
#
# $Log$
# Revision 1.2  2002/12/04 23:56:20  vapdev
# Ongoing work
#
# Revision 1.1  2002/12/04 00:59:41  vapdev
# Ongoing work
#
#
#
package TopNavTable;
use strict;
use Carp;
use HTML::Table;
@TopNavTable::ISA= qw/HTML::Table/;
use VapUtil;

sub new {

  my $class = shift;

  #
  my $self=$class->SUPER::new(-border=>"0",
			     -padding=>"0",
			     -spacing=>"0");
  
  # Get the defaults for the various VAP products, as well as for the
  # website itself

  #website defaults:

  # Some useful variables

  my ($row, $col, $key, $value, $content, $link, $img, $q);

    # The outermost table, which will contain the entire navbar. Each
    # of the subsequent tables will cells in this table.
  $q=CGI->new();

  


  #=*-=*-=*-=*-=*- The top part of the top navbar =*-=*-=*-=*-=*-=*-


    #   i.e. The little 'navbar' stuff (home, earth, solar system, ... etc)
  my $topnavbar = HTML::Table->new(-border=>"0",-spacing=>"0",-padding=>"0");

  $topnavbar->setCell(1,1,$q->img({-src=>"/images/blackbar_left.jpg",-width=>"217",
		   -height=>"19",-border=>"0", -alt=>"JPL Navbar"}));
  $topnavbar->setCellAlign(1,1,"LEFT");
  $topnavbar->setCellVAlign(1,1,"TOP");
  $topnavbar->setCellWidth(1,1,"217");
  $topnavbar->setBorder("0");
  $topnavbar->setCellSpacing("0");
  $topnavbar->setCellPadding("0");

    # The JPL Home pick
  $img = $q->img({-SRC=>"/images/blackbar_jplhome_off.jpg", -WIDTH=>"65",
		  -HEIGHT=>"19",
		  -BORDER=>"0",-ALT=>"JPL Home"});
  $content = $q->a({-href=>"http://www.jpl.nasa.gov",-target=>"_new"},$img);
  $topnavbar->setCell(1,2,$content);
  $topnavbar->setCellAlign(1,2,"LEFT");
  $topnavbar->setCellVAlign(1,2,"TOP");
  $topnavbar->setCellWidth(1,2,"65");

    # The Earth pick
  $img = $q->img({-SRC=>"/images/blackbar_earth_off.jpg", -WIDTH=>"48", 
		  -HEIGHT=>"19",
		  -BORDER=>"0",-ALT=>"Earth"});
  $content = $q->a({-href=>"http://www.jpl.nasa.gov/earth/earth_index.html",-target=>"_new"},$img);
  $topnavbar->setCell(1,3,$content);
  $topnavbar->setCellAlign(1,3,"LEFT");
  $topnavbar->setCellVAlign(1,3,"TOP");
  $topnavbar->setCellWidth(1,3,"48");

    # The Solar system pick
  $img = $q->img({-SRC=>"/images/blackbar_solarsys_off.jpg", -WIDTH=>"86", 
		  -HEIGHT=>"19",
		  -BORDER=>"0",-ALT=>"Solar System"});
  $content = $q->a({-href=>"http://www.jpl.nasa.gov/solar_system/solar_system_index.html",
		    -target=>"_new"},$img);
  $topnavbar->setCell(1,4,$content);
  $topnavbar->setCellAlign(1,4,"LEFT");
  $topnavbar->setCellVAlign(1,4,"TOP");
  $topnavbar->setCellWidth(1,4,"86");


    # The Stars pick
  $img = $q->img({-SRC=>"/images/blackbar_starsgal_off.jpg", -WIDTH=>"103", 
		  -HEIGHT=>"19",
		  -BORDER=>"0",-ALT=>"Stars &amp Galaxies"});
  $content = $q->a({-href=>"http://www.jpl.nasa.gov/stars_galaxies/stars_galaxies_index.html",
		    -target=>"_new"},$img);
  $topnavbar->setCell(1,5,$content);
  $topnavbar->setCellAlign(1,5,"LEFT");
  $topnavbar->setCellVAlign(1,5,"TOP");
  $topnavbar->setCellWidth(1,5,"103");


  $img = $q->img({-SRC=>"/images/blackbar_tech_off.jpg", -WIDTH=>"80", 
		  -HEIGHT=>"19",
		  -BORDER=>"0",-ALT=>"Technology"});
  $content = $q->a({-href=>"http://www.jpl.nasa.gov/technology/technology_index.html",
		    -target=>"_new"},$img);
  $topnavbar->setCell(1,6,$content);
  $topnavbar->setCellAlign(1,6,"LEFT");
  $topnavbar->setCellVAlign(1,6,"TOP");
  $topnavbar->setCellWidth(1,6,"80");

  $topnavbar->setCell(1,7,$q->img({-src=>"/images/spacer.gif",-width=>"150",
		   -height=>"19",-border=>"0", -alt=>"JPL Navbar"}));
  $topnavbar->setCellVAlign(1,7,"TOP");
  $topnavbar->setCellWidth(1,7,"150");
  $topnavbar->setCellBGColor(1,7, "#000000");

  $topnavbar->setCell(1,8,$q->img({-src=>"/images/blackbar_right1.jpg",-width=>"80",
		   -height=>"19",-border=>"0", -alt=>"JPL Navbar"}));
  $topnavbar->setCellVAlign(1,8,"TOP");
  $topnavbar->setCellAlign(1,8,"RIGHT");
  $topnavbar->setCellWidth(1,8,"80");
  $topnavbar->setCellBGColor(1,8, "#000000");

#=*-=*-=*-=*-=*- The main part of the banner! =*-=*-=*-=*-=*-=*-=*-=*-

  my $banner = HTML::Table->new(-border=>"0",-spacing=>"0",-padding=>"0");


    # The 'winds' logo
  $img=$q->img({-src=>"/images/winds_banner_left.jpg", -WIDTH=>"415",
		-HEIGHT=>"90",-BORDER=>"0",
		-ALT=>"Winds: Measuring Ocean Winds from Space",
		-USEMAP=>"#nasa-home"});
  $banner->setCell(1,1,$img);
  $banner->setWidth(1,1,"415");
  $banner->setCellAlign(1,1,"left");
  $banner->setCellVAlign(1,1,"top");

    # A spacer
  $img=$q->img({-src=>"/images/spacer.gif", -WIDTH=>"150",
		-HEIGHT=>"90",-BORDER=>"0",
		-ALT=>"Winds: Measuring Ocean Winds from Space"});
  $banner->setCell(1,2,$img);
  $banner->setCellBGColor(1,2,"#FFFFFF");
  $banner->setWidth(1,2,"150");
  $banner->setCellAlign(1,2,"left");
  $banner->setCellVAlign(1,2,"top");

    # Another spacer/gradient
  $img=$q->img({-src=>"/images/winds_banner_middle.jpg", -WIDTH=>"103",
		-HEIGHT=>"90",-BORDER=>"0",
		-ALT=>"Winds: Measuring Ocean Winds from Space"});
  $banner->setCell(1,3,$img);
  $banner->setWidth(1,3,"103");
  $banner->setCellAlign(1,3,"left");
  $banner->setCellVAlign(1,3,"top");

    # The ADEOS-II spacecraft and caltech imagemap.

  $img=$q->img({-src=>"/images/winds_banner_right1.jpg", -WIDTH=>"161",
		-HEIGHT=>"90",-BORDER=>"0",
		-ALT=>"Winds: Measuring Ocean Winds from Space",
		-USEMAP=>"#jpl-caltech"});
  $banner->setCell(1,4,$img);
  $banner->setWidth(1,4,"161");
  $banner->setCellAlign(1,4,"right");
  $banner->setCellVAlign(1,4,"top");


#=*-=*-=*-=*-=*-=*- The bottom spacer bar =*-=*-=*-=*-=*-=*-=*-=*-

  my $bottom = HTML::Table->new(-border=>"0",-spacing=>"0",-padding=>"0");


  $img=$q->img({-SRC=>"/images/winds_divider_left.jpg",
	    -WIDTH=>"415",-HEIGHT=>"5",-BORDER=>"0",
	-ALT=>"Winds: Measuring Ocean Winds from Space"});
  $bottom->setCell(1,1,$img);
  $bottom->setWidth(1,1,"415");
  $bottom->setCellAlign(1,1,"left");
  $bottom->setCellVAlign(1,1,"top");

  $img=$q->img({-SRC=>"/images/winds_divider_middle.gif",
	     -WIDTH=>"150",-HEIGHT=>"5",-border=>"0",
		-ALT=>"Winds: Measuring Ocean Winds from Space"});

  $bottom->setCell(1,2,$img);
  $bottom->setCellBGColor(1,2,"#5786B3");
  $bottom->setWidth(1,2,"150");
  $bottom->setCellAlign(1,2,"left");
  $bottom->setCellVAlign(1,2,"top");

  $img=$q->img({-SRC=>"/images/winds_divider_right.jpg" ,
		-WIDTH=>"264",-HEIGHT=>"5",-BORDER=>"0",
		-ALT=>"Winds: Measuring Ocean Winds from Space"});
  $bottom->setCell(1,3,$img);
  $bottom->setWidth(1,3,"264");
  $bottom->setCellAlign(1,3,"right");
  $bottom->setCellVAlign(1,3,"top");

#=-*=-*=-*=-*=-* Now assemble the whole navbar =-*=-*=-*=-*=-*=-*=-*=-*=-*

  $self->setCell(1,1,$topnavbar->getTable);
  #$self->setCellSp
  $self->setCell(2,1,$banner->getTable);
  $self->setCell(3,1,$bottom->getTable);
  return bless $self, ref($class) || $class;
}

1;
