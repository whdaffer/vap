#!/usr/bin/perl -w
#
# $Id$
#
# Modifications:
#
# $Log$
#
#
use strict;
use CGI;
use HTML::Table;
use LeftNavTable;

my $q=CGI->new(-nodebug=>1);
my $body=CGI->new(-nodebug=>1);

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

my $topbody = HTML::Table->new(-rows=>1,
			       -cols=>3);

  # For the left navbar
my $leftnavtable = LeftNavTable->new();


# my @navcol = ($q->a({-href=>"#ATLANTIC"},"Atlantic"),
# 	      $q->a({-href=>"#EASTPAC"},"East Pacific"),
# 	      $q->a({-href=>"#WESTPAC"},"West Pacific"));

# $leftnavtable -> addCol(@navcol);

  # The table in the middle of the page.
my $mainbodytable = HTML::Table->new(-rows=>6,
				      -cols=>1,
				      -width=>"60\%",
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

$outsidetable->setCell(2,1,$leftnavtable->getTable);
$outsidetable->setCell(2,2,$mainbodytable->getTable);
my $tables=$outsidetable->getTable;

$html .= $tables;
$html .= $q->end_html();
my $a=0;
print "$html\n";
exit;
