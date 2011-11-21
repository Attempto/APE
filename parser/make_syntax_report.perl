# This file is part of the Attempto Parsing Engine (APE).
# Copyright 2008-2010, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
#
# The Attempto Parsing Engine (APE) is free software: you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later version.
#
# The Attempto Parsing Engine (APE) is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE. See the GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License along with the Attempto
# Parsing Engine (APE). If not, see http://www.gnu.org/licenses/.


# ACE Syntax Report generator
#
# This Perl script converts APE grammar files into HTML, preserving only the comments, i.e.
# it extracts the lines that start with a comment symbol (%) from:
# * grammar.fit
# * grammar_functionwords.fit
# * grammar_contentwords.fit
#
# For comments which you don't want to have in the output, use "/* ... */" or "%% ...".
#
# This script also generates a table of contents and saves it into a file 'syntax_report.toc'.
# The content of 'syntax_report.toc' is placed right after the title of the resulting HTML.
# This however needs two runs in the general case. During the first run the table of contents
# is generated (or an existing 'syntax_report.toc' is updated), during the second run the
# updated 'syntax_report.toc' is placed into the report.
#
# @author Tobias Kuhn
# @author Kaarel Kaljurand
# @version 2008-03-17
#
# Usage:
#
# cat grammar.fit grammar_functionwords.fit grammar_contentwords.fit | perl make_syntax_report.perl

use strict;
use Getopt::Long;

my $syntax_report_toc = 'syntax_report.toc';
my $title_numbers = [0, 0, 0, 0, 0];

my $message = "This report was generated automatically from the following files:
				<code>grammar.fit</code>,
				<code>grammar_functionwords.fit</code>,
				<code>grammar_contentwords.fit</code>.";

my $name_set = {};
my $href_set = {};

my $closetag = "";  # = "</p>": "p"-tag is open that will be closed implicitly
                    # = " ": "pre"- or "blockquote"-tag is open that has to be closed explicitly
                    # = "": no such tag is open

my $str = "";

my $label;
my $section;

&make_header;
&make_toc;

open (TOC, '>', $syntax_report_toc) or die "Can't open $syntax_report_toc: $!";
while(<STDIN>) {
	next if !/^%/;
	next if /^%%/;
	
	s{&}{&amp;}g;
	s{-->}{&rarr;}g;
	s{<}{&lt;}g;
	s{>}{&gt;}g;
	
	s{^%}{};
	s{^===.*}{};


	if ( m{^ *\\section\{(.*)\}} ) {
		$str = &generate_number($title_numbers, 0);
	}
	elsif ( m{^ *\\subsection\{(.*)\}} ) {
		$str = &generate_number($title_numbers, 1);
	}
	elsif ( m{^ *\\subsubsection\{(.*)\}} ) {
		$str = &generate_number($title_numbers, 2);
	}


	if ( s{^ *\\version\{(.*)\}}{<p>$message Version: $1</p>} ) {
	} elsif ( s{^\s*-([0-9]+[a-z]?)------------.*}{$closetag<pre><a name="$1"><b>$1</b>\n} ) {
		# start of a grammar rule box
		$name_set->{$1} = 1;
		$closetag = " ";
	} elsif ( s{^\s*--------------.*}{</a></pre>\n} ) {
		# end of a grammar rule box
		$closetag = "";
	} elsif ( s{&gt;&gt;\|}{$closetag<pre>} ) {
		# start of a code block
		$closetag = " ";
	} elsif ( s{&gt;&gt;}{$closetag<blockquote><p>} ) {
		# start of a quote
		$closetag = " ";
	} elsif ( s{^ *\\section\{(.*)\}}{$closetag<h2><a name='--label--'>$str $1</a></h2>} ) {
		$section = $1;
		$label = $1;
		$label =~ s/[^a-zA-Z0-9]//g;  # labels are not allowed to contain spaces
		s/--label--/$label/;
		print TOC "$str <a href='#$label'>$section</a>\n";
		$closetag = "";
	} elsif ( s{^ *\\subsection\{(.*)\}}{$closetag<h3><a name='--label--'>$str $1</a></h3>} ) {
		$section = $1;
		$label = $1;
		$label =~ s/[^a-zA-Z0-9]//g;  # labels are not allowed to contain spaces
		s/--label--/$label/;
		print TOC "$str <a href='#$label'>$section</a>\n";
		$closetag = "";
	} elsif ( s{^ *\\subsubsection\{(.*)\}}{$closetag<h4><a name='--label--'>$str $1</a></h4>} ) {
		$section = $1;
		$label = $1;
		$label =~ s/[^a-zA-Z0-9]//g;  # labels are not allowed to contain spaces
		s/--label--/$label/;
		print TOC "$str <a href='#$label'>$section</a>\n";
		$closetag = "";
	} elsif ( $closetag eq "</p>" && s{^\s*$}{<\/p><p>} ) {
		# empty line -> new paragraph
		$closetag = "</p>";
	} elsif ( $closetag eq "" ) {
		# start new paragraph
		s{^(.*)$}{<p>$1};
		$closetag = "</p>";
	}
	
	# end of a code block
	if ( s{\|&lt;&lt;}{</pre>} ) {
		$closetag = "";
	}
	
	# end of a quote
	if ( s{&lt;&lt;}{</p></blockquote>} ) {
		$closetag = "";
	}
	
	# symbol anchor
	if ( s{^ ([a-zA-Z0-9\-_']+)(.*) &rarr;}{<span><a name="$1">$1</a><\/span>$2 &rarr;} ) {
		$name_set->{$1} = 1;
	}
	
	if (/ Example:/) {
		s{ Example:}{<span class="u">Example:</span>};
		s/([^\\])\[/$1<span style="color: #888">[/g;
		s/([^\\])\]/$1]<\/span>/g;
		s/\\\[/[/g;
		s/\\\]/]/g;
	}
	
	# reference to rule (by rule number)
	while ( s/\[\[([0-9]+[a-z]?)\]\]/<tt><a href="#$1">$1<\/a><\/tt>/ ) {
		$href_set->{$1} = 1;
	}
	
	# reference to symbol
	if (/^\t/) {
		while ( s/([\s(])([A-Z][a-zA-Z0-9\-_']*)/$1<tt><a href="#$2">$2<\/a><\/tt>/ ) {
			$href_set->{$2} = 1;
		}
	}
	
	# labels in XHTML are not allowed to contain an apostrophe
	# (we use apostrophes only at the end of symbols)
	s{'">}{Bar">}g;
	
	# references to features
	s{-([A-Z]+)}{<code><a href="#Features">-$1</a></code>}g;
	s{\+([A-Z]+)}{<code><a href="#Features">+$1</a></code>}g;
	
	# italics
	s/([^\\])_([a-zA-Z0-9]+)_/$1<em>$2<\/em>/g;
	s/_\|/<em>/g;
	s/\|_/<\/em>/g;
	s/\\_/_/g;
	
	# bold
	s/([^\\])\*([a-zA-Z0-9]+)\*/$1<strong>$2<\/strong>/g;
	s/\*\|/<strong>/g;
	s/\|\*/<\/strong>/g;
	s/\\\*/\*/g;
	
	# code
	s/([^\\])=([a-zA-Z0-9]+)=/$1<code>$2<\/code>/g;
	s/=\|/<code>/g;
	s/\|=/<\/code>/g;
	s/\\=/=/g;
	
	s{\\lor}{&or;}g;
	s{\\land}{&and;}g;
	s{\\forall}{&forall;}g;
	s{\\exists}{&exist;}g;
	s{\\rightarrow}{&rArr;}g;
	s{---}{&mdash;}g;
	s{--}{&ndash;}g;
	
	s{BUG}{<span style="color: red">BUG</span>};
	
	print;
}

close TOC or die "Can't close $syntax_report_toc: $!";
&make_footer;
&show_warnings($name_set, $href_set);

exit;

sub make_header
{
print <<EOF;
<?xml version="1.0" encoding="ISO-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">

<head>
<title>ACE 6.6 Syntax Report</title>

<style type="text/css">
.bug { background-color: #fdd }
h1, h2, h3, h4, h5, h6 { font-family: sans-serif }
td { border: 1px solid black; padding: 0.3em 0.3em 0.3em 0.3em }
thead { font-weight: bold; background-color: #4b4; font-variant: small-caps }
table { font-size: 90%; empty-cells: show; border-collapse: collapse }
pre { padding: 1em 1em 1em 1em; background-color: #eee; border: 1px solid silver }
body { font-family: serif }
address { border-top: 1px solid black; margin-top: 2em; padding-top: 0.5em }
span.u { text-decoration: underline }
a[href]:link {color: black; text-decoration: none; }
a[href]:visited {color: black; text-decoration: none; }
a[href]:hover {color: blue; text-decoration: none; }
</style>
</head>

<body>

<h1>ACE 6.6 Syntax Report</h1>
EOF
}


sub make_toc
{
	if (-e $syntax_report_toc) {
		print "<pre id='toc'>\n";
		open (TOC, $syntax_report_toc) or die "Can't open $syntax_report_toc: $!";
		while (<TOC>) {
			print;
		}
		close TOC or die "Can't close $syntax_report_toc: $!";
		print "</pre>\n";
	}
}


sub make_footer
{
print <<EOF;
</body>
</html>
EOF
}

sub show_warnings
{
	my $name_set = shift;
	my $href_set = shift;

	for (keys %{$name_set}) {
		if (! (defined $href_set->{$_})) {
			warn "declared but not linked: " . $_, "\n";
		}
	}

	for (keys %{$href_set}) {
		if (! (defined $name_set->{$_})) {
			warn "broken link: " . $_, "\n";
		}
	}
}


sub increase_title_numbers {
	my $numbers = shift;
	my $pos = shift;

	$numbers->[$pos]++;
}


sub reset_title_numbers {
	my $numbers = shift;
	my $pos = shift;

	for (my $i = $pos; defined $numbers->[$i]; $i++) {
		$numbers->[$i] = 0;
	}
}


sub title_numbers_tostring {
	my $title_numbers = shift;
	my @numbers = @{$title_numbers};
	my $str = shift @numbers;
	foreach (@numbers) {
		if ($_ eq "0") {
			last;
		}
		else {
			$str = $str . "." . $_;
		}
	}

	return $str;
}


sub generate_number {
	my $numbers = shift;
	my $level = shift;
	&increase_title_numbers($numbers, $level);
	&reset_title_numbers($numbers, $level + 1);
	return &title_numbers_tostring($numbers);
}
