package XML::TokeParser;

use strict;
use vars qw($VERSION);
use Carp;
use XML::Parser;

$VERSION = '0.01';

sub new {
  my $class=shift;
  my $source=shift;
  my %args=(Noempty=>0,Latin=>0,Catalog=>0,@_);
  my $self={output=>[],EOF=>0};
  $self->{noempty}=delete $args{Noempty};
  $self->{latin}=delete $args{Latin};
  my $catname=delete $args{Catalog};
  my $parser=XML::Parser->new(%args) or croak "$!";
  $parser->setHandlers(
       Start=>\&start,End=>\&end,Char=>\&char,Proc=>\&proc,Comment=>\&comment
    );
  if ($catname) {
    require XML::Catalog;
    my $catalog=XML::Catalog->new($catname) or croak "$!";
    $parser->setHandlers(ExternEnt=>$catalog->get_handler($parser));
  }
  $self->{parser}=$parser->parse_start(TokeParser=>$self) or croak "$!";  
  if (ref($source) eq 'SCALAR') {
    $self->{src}=$source;
    $self->{src_offset}=0;
  }
  elsif (ref($source)=~/^IO:|^GLOB$/) {
    $self->{srcfile}=$source;
  }
  else {
    require IO::File;
    $self->{srcfile}=IO::File->new($source,'r') or return undef;
    $self->{opened}=1;
  }
  bless $self,$class;
}

sub DESTROY {
  my $self=shift;
  $self->{srcfile}->close() if $self->{opened};
  $self->{parser}=undef;
}

sub get_token {
  my $self=shift;
  $self->parsechunks();
  my $token=shift @{$self->{output}};
  while ($self->{noempty} && $token && $token->[0] eq 'T' && $token->[1]=~/^\s*$/) {
    $self->parsechunks();
    $token=shift @{$self->{output}};
  }
  $token;
}

sub unget_token {
  my $self=shift;
  push @{$self->{output}},@_;
}

sub get_tag {
  my ($self,$tag)=@_;
  my $token;
  while ($token=$self->get_token()) {
    my $type=shift @$token;
    next unless $type=~/[SE]/;
    substr($token->[0],0,0)='/' if $type eq 'E';
    last unless (defined($tag) && $token->[0] ne $tag);
  }
  $token;
}

sub get_text {
  my ($self,$tag)=@_;
  my $text="";
  my $token;
  while ($token=$self->get_token()) {
    my $type=$token->[0];
    if ($type eq 'T') {
      $text.=$token->[1];
    }
    elsif ($type=~/[SE]/) {
      my $tt=$token->[1];
      $tt="/$tt" if $type eq 'E';
      last if (!defined($tag) || $tt eq $tag);
    }
    elsif ($type eq 'PI') {
      last;
    }
  }
  $self->unget_token($token) if $token;
  $text;
}

sub get_trimmed_text {
  my $self=shift;
  my $text=$self->get_text(@_);
  $text=~s/^\s+//;
  $text=~s/\s+$//;
  $text=~s/\s+/ /g;
  $text;
}

sub parsechunks {
  my ($self)=@_;
  my $buf;
  while (
      (!@{$self->{output}} || $self->{output}[-1][0] eq 'T')
      && !$self->{EOF}) {
    if (exists $self->{src}) {
      $buf=substr(${$self->{src}},$self->{src_offset},4096);
      $self->{src_offset}+=4096;
    }
    else {
      read($self->{srcfile},$buf,4096);
    }
    if (length($buf)==0) {
      $self->{EOF}=1;
      $self->{parser}->parse_done();
    }
    else {
      $self->{parser}->parse_more($buf);
    }
  }
}

sub start {
  my ($parser,$element,@attrs)=@_;
  my $self=$parser->{TokeParser};
  push @{$self->{output}},
    ['S',$self->nsname($element),{},[],$parser->original_string()];
  while (@attrs) {
    my ($name,$val)=(shift @attrs,shift @attrs);
    $name=$self->nsname($name);
    $val=$self->encode($val);
    $self->{output}[-1][2]{$name}=$val;
    push @{$self->{output}[-1][3]},$name;
  }
}

sub end {
  my ($parser,$element)=@_;
  my $self=$parser->{TokeParser};
  push @{$self->{output}},
    ['E',$self->nsname($element),$parser->original_string()];
}

sub char {
  my ($parser,$text)=@_;
  my $self=$parser->{TokeParser};
  $text=$self->encode($text);
  if (@{$self->{output}} && $self->{output}[-1][0] eq 'T') {
    $self->{output}[-1][1].=$text;
    $self->{output}[-1][-1].=$parser->original_string();
  }
  else {
    push @{$self->{output}},
      ['T',$text,$parser->original_string()];
  }
}

sub proc {
  my ($parser,$target,$value)=@_;
  my $self=$parser->{TokeParser};
  push @{$self->{output}},
    ["PI",$self->encode($target),$self->encode($value),$parser->original_string()];
}

sub comment {
  my ($parser,$text)=@_;
  my $self=$parser->{TokeParser};
  push @{$self->{output}},
    ["C",$self->encode($text),$parser->original_string()];
}

sub nsname {
  my ($self,$name)=@_;
  my $parser=$self->{parser};
  if ($parser->{Namespaces}) {
    my $ns=$parser->namespace($name)||'';
    $name="{$ns}".$name;
  }
  return $self->encode($name);    
}

sub encode {
  my ($self,$text)=@_;
  if ($self->{latin}) {
    $text=~s{([\xc0-\xc3])(.)}{
      my $hi = ord($1);
      my $lo = ord($2);
      chr((($hi & 0x03) <<6) | ($lo & 0x3F))
     }ge;
  }
  $text;
}

1;
__END__

=head1 NAME

XML::TokeParser - Simplified interface to XML::Parser

=head1 SYNOPSIS

  use XML::TokeParser;

  #parse from file
  my $p=XML::TokeParser->new('file.xml')

  #parse from open handle
  open IN,'file.xml' or die $!;
  my $p=XML::TokeParser->new(\*IN,Noempty=>1);

  #parse literal text
  my $text='<tag xmlns="http://www.omsdev.com">text</tag>';
  my $p=XML::TokeParser->new(\$text,Namespaces=>1);

  #read next token
  my $token=$p->get_token();

  #skip to <title> and read text
  $p->get_tag('title');
  $p->get_text();

  #read text of next <para>, ignoring any internal markup
  $p->get_tag('para');
  $p->get_trimmed_text('/para');


=head1 DESCRIPTION

XML::TokeParser provides a procedural ("pull mode") interface to XML::Parser
in much the same way that Gisle Aas' HTML::TokeParser provides a procedural
interface to HTML::Parser.  XML::TokeParser splits its XML input up into
"tokens," each corresponding to an XML::Parser event.

A token is a reference to an array whose first element is an event-type 
string and whose last element is the literal text of the XML input that 
generated the event, with intermediate elements varying according to the 
event type:

=over 4

=item Start tag

The token has five elements: 'S', the element's name, a reference to a hash 
of attribute values keyed by attribute names, a reference to an array of 
attribute names in the order in which they appeared in the tag, and the 
literal text.

=item End tag

The token has three elements: 'E', the element's name, and the literal text.

=item Character data (text)

The token has three elements: 'T', the parsed text, and the literal text.  
All contiguous runs of text are gathered into single tokens; there will 
never be two 'T' tokens in a row.

=item Comment

The token has three elements: 'C', the parsed text of the comment, and the 
literal text.

=item Processing instruction

The token has four elements: 'PI', the target, the data, and the literal 
text.

=back

The literal text includes any markup delimiters (pointy brackets, 
<![CDATA[, etc.), entity references, and numeric character references and 
is in the XML document's original character encoding.  All other text is in 
UTF-8 (unless the Latin option is set, in which case it's in ISO-8859-1) 
regardless of the original encoding, and all entity and character 
references are expanded.

If the Namespaces option is set, element and attribute names are prefixed 
by their (possibly empty) namespace URIs enclosed in curly brackets and 
xmlns:* attributes do not appear in 'S' tokens.

=head1 METHODS

=over 4

=item $p = XML::TokeParser->new($input, [options])

Creates a new parser, specifying the input source and any options.  If 
$input is a string, it is the name of the file to parse.  If $input is a 
reference to a string, that string is the actual text to parse.  If $input 
is a reference to a typeglob or an IO::Handle object corresponding to an 
open file or socket, the text read from the handle will be parsed.

Options are name=>value pairs and can be any of the following:

=over 4

=item Namespaces

If set to a true value, namespace processing is enabled.

=item ParseParamEnt

This option is passed on to the underlying XML::Parser object; see that 
module's documentation for details.

=item Noempty

If set to a true value, text tokens consisting of only whitespace (such as 
those created by indentation and line breaks in between tags) will be 
ignored.

=item Latin

If set to a true value, all text other than the literal text elements of 
tokens will be translated into the ISO 8859-1 (Latin-1) character encoding 
rather than the normal UTF-8 encoding.

=item Catalog

The value is the URI of a catalog file used to resolve PUBLIC and SYSTEM 
identifiers.  See XML::Catalog for details.

=back

=item $token = $p->get_token()

Returns the next token, as an array reference, from the input.  Returns 
undef if there are no remaining tokens.

=item $p->unget_token($token,...)

Pushes tokens back so they will be re-read.  Useful if you've read one or 
more tokens to far.

=item $token = $p->get_tag( [$token] )

If no argument given, skips tokens until the next start tag or end tag 
token. If an argument is given, skips tokens until the start tag or end tag 
(if the argument begins with '/') for the named element.  The returned 
token does not include an event type code; its first element is the element 
name, prefixed by a '/' if the token is for an end tag.

=item $text = $p->get_text( [$token] )

If no argument given, returns the text at the current position, or an empty 
string if the next token is not a 'T' token.  If an argument is given, 
gathers up all text between the current position and the specified start or 
end tag, stripping out any intervening tags (much like the way a typical 
Web browser deals with unknown tags).

=item $text = $p->get_trimmed_text( [$token])

Like get_text(), but deletes any leading or trailing whitespaces and 
collapses multiple whitespace (including newlines) into single spaces.

=back

=head1 DIFFERENCES FROM HTML::TokeParser

Uses a true XML parser rather than a modified HTML parser.

Text and comment tokens include extracted text as well as literal text.

PI tokens include target and data as well as literal text.

No tokens for declarations.

No "textify" hash.

=head1 EXAMPLES

=head2 Print method signatures from the XML version of this PODpage

  #!/usr/bin/perl -w
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
      $p->get_tag('/list');  # assumes no nesting here!
    }
  }

=head1 AUTHOR

Eric Bohlman (ebohlman@omsdev.com)

Copyright (c) 2001 Eric Bohlman. All rights reserved. This program
is free software; you can redistribute it and/or modify it under the same
terms as Perl itself.

=head1 SEE ALSO

  XML::Parser
  XML::Catalog
  HTML::TokeParser

=cut
