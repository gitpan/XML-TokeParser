#!perl -w
use strict;
use XML::TokeParser;
my $t;
my $p=XML::TokeParser->new('tokeparser.xml',Noempty=>1) or die $!;
while ($p->get_tag('title') && $p->get_text('/title') ne 'METHODS') {
  ;
}
$p->get_tag('list');
while (($t=$p->get_tag()->[0]) ne '/list') {
  if ($t eq 'item') {
    $p->get_tag('itemtext');
    print $p->get_text('/itemtext'),"\n";
    $p->get_tag('/item');
  }
  else {
    $p->get_tag('/list');
  }
}
