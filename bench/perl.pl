#! /usr/bin/perl

use Time::HiRes qw(time);

sub find_it {
    my $buf = shift;
    $buf =~  /indecipherable|undecipherable/; 
    return $-[0]
}


open IN,"test-data";
undef $/;
my $buf = <IN>;

my $len = find_it($buf);
study $buf;

my $start = time();
for(0..999) {
    find_it($buf)==$len or die;
}
print "$len " . (time()-$start) . "\n"
