my $s = 0;
my $c = 0;

while(<STDIN>) {
	$s += $_;
	$c++;
}

print $c, "\t", $s, "\n";
