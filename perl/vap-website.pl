#!/usr/bin/perl -w
#
use strict;
use CGI;
use HTML::Table;
use LeftNavTable;

my $q=CGI->new(-nodebug=>1);
my $body=CGI->new(-nodebug=>1);

my $html=$q->start_html(-title=>"Haifung Winds stuff",
			-meta=>{"Keywords" =>
				qw/Radar Scatterometry SeaWinds 
                                    Winds Oceanography/});

  # The outer most table. All content is contained in this table
my $outsidetable = HTML::Table->new(-rows=>3,
				    -cols=>2,
				    -align=>"left",
				    -width=>"100\%",
				   -border=>1);

  # Contains the top navigation bar.
my $topbody = HTML::Table->new(-rows=>1,
			       -cols=>3);

  # The left nav table
my $leftnavtable = LeftNavTable->new(-rows=>6,
				    -cols=>1);

my @navcol = ($q->a({-href=>"#ATLANTIC"},"Atlantic"),
	      $q->a({-href=>"#EASTPAC"},"East Pacific"),
	      $q->a({-href=>"#WESTPAC"},"West Pacific"));

$leftnavtable -> addCol(@navcol);

my $mainbodytable = HTML::Table->new(-rows=>6,
				      -cols=>1,
				      -width=>"60\%",
 				      -border=>1);
my $bodyhtml = $body->h1("Welcome to my test page");
$bodyhtml .= $body->a(-href=>"http://haifung.jpl.nasa.gov",
		      -name=>"#HAIFUNG",
		      "Haifung web site");
$bodyhtml .= $body->p();
$bodyhtml .= "Just some more test to check out the formatting";
$bodyhtml .= $body->img(-src=>
			"file://" . $ENV{'VAP_WWW_TOP'} . 
			"/images/fuji.gif",
			-alt=>"Fuji",
			-align=>"left");

$mainbodytable->setCell(1,1,$bodyhtml);

$outsidetable->setCell(2,1,$leftnavtable->getTable);
$outsidetable->setCell(2,2,$mainbodytable->getTable);
my $tables=$outsidetable->getTable;

$html .= $tables;
$html .= $q->end_html();
my $a=0;
print "$html\n";
exit;
