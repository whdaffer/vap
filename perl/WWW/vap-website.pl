#!/usr/bin/perl -w
#
use CGI;

my $q=CGI->new();
my $html=$q->start_html(-title=>"Haifung Winds stuff",
			-meta=>{"Keywords" =>
				"Scatterometry SeaWinds Winds Oceanography"});
use HTML::Table;
my $outsidetable = HTML::Table->new(-rows=>3,
				    -cols=>2,
				    -align=>"left",
				    -width=>"100\%",
				   -border=>1);

my $topbody = HTML::Table->new(-row=>1,
			       -cols=>3);

my $leftnavtable = Vap::LeftNavTable->new()

@navcol = ($q->a({-href=>"#ATLANTIC"},"Atlantic"),
	   $q->a({-href=>"#EASTPAC"},"East Pacific"),
	   $q->a({-href=>"#WESTPAC"},"West Pacific"));

$leftnavtable -> addCol(@navcol);

my $rightbodytable = HTML::Table->new(-rows=>6,
				      -cols=>1,
				      -width=>"60\%",
				      -border=>1);

$outsidetable->setCell(2,1,$leftnavtable->getTable);
$outsidetable->setCell(2,2,$rightbodytable->getTable);
my $tables=$outsidetable->getTable;

$html .= $tables;
$html .= $q->end_html();
my $a=0;
print "$html\n";
exit;
