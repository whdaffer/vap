#!/usr/bin/perl -w
#
# $Id$
#
# Modifications:
#
# $Log$
# Revision 1.5  2002/12/04 01:19:07  vapdev
# Ongoing work
#
#
#
use strict;
use CGI;
use HTML::Table;
use LeftNavTable;
use TopNavTable;
use VapCGI;
my $q=VapCGI->new(-nodebug=>1);
my $body=VapCGI->new(-nodebug=>1);

my $html=$q->start_html(
	    -title=>"Haifung Winds stuff",
	    -meta=>{"Keywords" =>
				qw/Radar Scatterometry SeaWinds 
				   Winds Oceanography/},
	     -background=>"file:///disk2/vap/www/htodcs/images/fuji7.gif",
	     -style=>{
		      -code=>'A:link {text-decoration:none;},A:visited {text-decoration:none;}'
		     },
	     -link=>"336699", -alink=>"003366",-vlink=>"336699",-marginheight=>"0",
	     -topmargin=>"0", -leftmargin=>"0", -onload=>"init()", -onresize=>"redo()",
	     -script=>{-language=>"javascript", -code=>"var _OLD_ONERROR = window.onerror; window.onerror=null"},
	     -script=> {-language=>"javascript", -src=>"file:///usr/people/vapdev/development/vap/html/js/ua.js"},
	     -script=> {-language=>"javascript1.2", -src=>"file:///usr/people/vapdev/development/vap/html/js/base.js"},
	     -script=> {-language=>"javascript1.2", -src=>"file:///usr/people/vapdev/development/vap/html/js/breadcrumbs.js"},
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
	     -script => { -language=>"JavaScript1.2", -src=>"file:///usr/people/vapdev/development/vap/html/js/xbCollapsibleLists.js"},
             -script => { -language=>"JavaScript1.2", -src=>"file:///usr/people/vapdev/development/vap/html/js/viewSource.js"},
	     -script => { -language=>"JavaScript", 
               -code=> "function init() { return true; }"} 
);







  # Outermost table, contains the entire content of the page.

my $outsidetable = HTML::Table->new(-rows=>3,
				    -cols=>2,
				    -align=>"left",
				    -width=>"100\%",
				   -border=>1);
  # For the top navbar

#my $topbody = HTML::Table->new(-rows=>1,
#			       -cols=>3);
my $topnavtable = TopNavTable->new();

  # For the left navbar
my $leftnavtable = LeftNavTable->new(-width=>"20\%");


# my @navcol = ($q->a({-href=>"#ATLANTIC"},"Atlantic"),
# 	      $q->a({-href=>"#EASTPAC"},"East Pacific"),
# 	      $q->a({-href=>"#WESTPAC"},"West Pacific"));

# $leftnavtable -> addCol(@navcol);

  # The table in the middle of the page.
my $mainbodytable = HTML::Table->new(-rows=>6,
				      -cols=>1,
				      -width=>"80\%",
 				      -border=>1);

my $bodyhtml = $body->h1("Welcome to my test page");
$bodyhtml .= $body->a({-href=>"http://haifung.jpl.nasa.gov",
		      -name=>"#HAIFUNG"},
		      "Haifung web site");
$bodyhtml .= $body->p("Just some more test to check out the formatting");
#$bodyhtml .= $body->img({-src=>
#			"file:///disk2/vap/www/htdocs/images/fuji7.gif",
#			-alt=>"Fuji",
#			-align=>"left"});

$mainbodytable->setCell(1,1,$bodyhtml);
  # Top navbar
$outsidetable->setCell(1,1,$topnavtable->getTable);
$outsidetable->setCellColSpan(1,1,3);
  # Side navbar
$outsidetable->setCell(2,1,$leftnavtable->getTable);
  # main content
$outsidetable->setCell(2,2,$mainbodytable->getTable);
my $tables=$outsidetable->getTable;

$html .= $tables;
$html .= $q->map({-name=>"nasa-home", 
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
$html .= $q->map( {-name=>"jpl-caltech",
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


$html .= $q->end_html();
my $a=0;
print "$html\n";
exit;
