
# Extracts words from an ACE text.

# TODO
# * Show also multi-word units. Useful for detecting repeated phrases.

use strict;
use warnings;

$/ = undef;
$_ = <STDIN>;

# Remove comments
s{/\*\*[^*]*\*/}{}sg;
s{/\*[^*]*\*/}{}sg;
s{#.*}{}g;

s{\.}{ \. }g;
s{,}{ , }g;
s{'s}{ 's }g;
s{\s+}{ }g;
s{^\s+}{};
s{\s+$}{};

my @words = split /\s+/;

my $words = {};

foreach (@words) {
	$words->{$_}++;
}

foreach (sort keys %{$words}) {
	print $words->{$_}, "\t", $_, "\n";
}
