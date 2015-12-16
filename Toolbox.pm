#!/usr/bin/perl

package Toolbox;

use strict;
use warnings;

our $VERSION    =  0.0.1;
#our @ISA        =  qw(Exporter);
our @EXPORT     =  qw();
our @EXPORT_OK  =  qw(use_the_schwartz file_to_array_ref array_ref_to_file convert_baseN uniquify_list);

sub use_the_schwartz {
    # human-sort an array (passed by reference)
    # Schwartzian transform-based alpha-numeric sort with 
    # padding for numeric parts to ensure proper numeric
    # sorting
    # 
    # Faster than Pryor on fire!
    
    my ($unsorted_ref)  =  @_;

    # first, do a pre-sort
    @$unsorted_ref  =  sort { $a cmp $b } @$unsorted_ref;
    #&grt($unsorted_ref);

    # Characterize numeric content for sprintf padding

    my $maxwidth  =  0;
    # nested for() loop characterization pass
    # slightly slower than nested map{} functions
    # benchmark score:  57 microseconds
    #for my $unsorted (@$unsorted_ref) {
    #    my @u  =  $unsorted  =~ m/([a-z]+|[0-9]+)/ig;
    #    for my $section (@u) {
    #        if($section =~ m/^\d+$/) {
    #            $maxwidth  =  length($section) if(length($section) > $maxwidth);
    #        }
    #    }
    #}
    
    # nested map{} function characterization pass
    # slightly (a few microseconds) faster than nested for() loops
    # benchmark score:  53 microseconds
    #@$unsorted_ref  =  map { my @u  =  $_   =~ m/([a-z]+|[0-9]+)/ig;
    #                         @u     =  map { my $w  =  length($_); 
    #                                         if($_ =~ m/^\d+$/) { 
    #                                             $maxwidth  =  $w if($w > $maxwidth);
    #                                         }
    #                                       } @u;
    #                       } @$unsorted_ref;

    # hybrid approach 1 characterization pass
    # outer for() loop, inner map
    # approximately equal to nested for() loops
    # benchmark score:  57 microseconds
    #for my $unsorted (@$unsorted_ref) {
    #    my @u  =  $unsorted  =~ m/([a-z]+|[0-9]+)/ig;
    #    @u     =  map { my $w  =  length($_);
    #                    if($_ =~ m/^\d+$/) { 
    #                        $maxwidth  =  $w if($w > $maxwidth);
    #                    }
    #                  } @u;
    #}
    
    # hybrid approach 2 characterization pass
    # outer map{} function, inner for() loop
    # fastest!  slightly faster than nested map{} functions
    # benchmark score:  52 microseconds
    @$unsorted_ref  =  map { my @u  =  $_   =~ m/([a-z]+|[0-9]+)/ig;
                             for my $section (@u) {
                                 if($section =~ m/^\d+$/) {
                                     $maxwidth  =  length($section) if(length($section) > $maxwidth);
                                 }
                             }
                             $_;
                           } @$unsorted_ref;


    # Help us, Obiwan (Randal) Schwartz!  You're our only hope.
    # Do the transform...it is your destiny.  (Search your 
    # feelings.  You know it to be true.)
    # 
    # this translates each item in an array ref into a 
    # properly-sortable form, removing underscores and 
    # non-wordlike characters, and sprintf'ing numeric 
    # content to a fixed width.  the original array ref
    # is then sorted by the translated versions of the
    # values
    
    # <BORROW FROM="Mastering Algorithms with Perl;John Orwant, Jarkko Hietaniemi & John Macdonald;1st Ed.;Ch. 4, p. 111;O'Reilly" PART=1>
    @$unsorted_ref  =   map  { $_->[0] }
                        sort { $a->[1] cmp $b->[1] }
                        map  { my $d  =  lc; 
                               $d     =~ s/[\W_]+//g;
                               # </BORROW>
                               # <MINE PURPOSE="zero-padding numeric substrings to guarantee numeric (not ASCII-betic) sort or numeric portions">
                               my @d  =  $d  =~ m/([a-z]+|[0-9]+)/ig;
                               my @e;
                               for my $e (@d) {
                                   if($e  =~ m/^\d+$/) {
                                       $e  =  sprintf("%0$maxwidth" . "d", $e);
                                   }
                               }
                               $d  =  join('', @d);
                               # </MINE>
                               # <BORROW FROM="Mastering Algorithms with Perl;John Orwant, Jarkko Hietaniemi & John Macdonald;1st Ed.;Ch. 4, p. 111;O'Reilly" PART=2>
                               [ $_, $d ]
                             }
                        @$unsorted_ref;
                        # </BORROW>
}

sub file_to_array_ref {
    my ($file)  =  @_;
    open(FILE, "<$file") or die("Cannot open file $file for reading: $!");
    my @contents;
    while(my $line  =  <FILE>) {
        chomp($line);
        push(@contents, $line);
    }
    close(FILE);
    return \@contents;
}

sub array_ref_to_file {
    my ($array_ref, $file, $mode)  =  @_;
    open(FILE, "$mode$file") or die("Cannot create or open for writing file $file in mode $mode: $!");
    my $line  =  0;
    while(defined($array_ref->[$line])) {
        print FILE "$array_ref->[$line]\n";
        $line++;
    }
    close(FILE);
    my $exit  =  0;
    $exit++ if(!-e $file);
    $exit++ if(-z $file and scalar(@$array_ref));
    return $exit;
}

sub convert_baseN
{
    # use baseN and unbaseN
    my ($num, $from_base, $to_base) = @_;
    if(($from_base && 
        ($from_base !~ m/^\d+$/ || $from_base < 2 || $from_base > 64)) ||
        ($to_base &&
        ($to_base !~ m/^\d+$/ || $to_base < 2 || $to_base > 64)))
    {
        return "Error: base must be at least 2 and not more than 64\n";
    }
    my @base_chars = (0 ... 9);
    push(@base_chars, ('a' ... 'z'));
    push(@base_chars, chr(241));
    push(@base_chars, ('A' ... 'Z'));
    push(@base_chars, chr(209));
    my @base_vals  = (0 ... 63);
    my (%from_base_map, %to_base_rev_map);
    my ($i, $ret);
    $ret = $num if($from_base == 10);
    die("Base values must be between 2 and 64") if($from_base < 2  ||
                                                   $from_base > 64 ||
                                                   $to_base   < 2  ||
                                                   $to_base   > 64);
    unless($from_base == 10 || $from_base < 2 || $from_base > 64)
    {
        $i = 0;
        while($base_vals[$i] < $from_base)
        {
            $from_base_map{$base_chars[$i]} = $base_vals[$i];
            $i++;
        }
        $ret = &unbaseN(\%from_base_map, $num);
    }
    unless($to_base == 10 || $to_base < 2 || $to_base > 64)
    {
        $i = 0;
        while(defined($base_vals[$i]) && $base_vals[$i] < $to_base )
        {
            $to_base_rev_map{$base_vals[$i]} = $base_chars[$i];
            $i++;
        }
        $ret = &baseN(\%to_base_rev_map, $ret);
    }
    return $ret;
}

sub baseN
{
    my ($rmap, $num) = @_;
    my $base = scalar(keys %{$rmap});
    return $rmap->{$num} if($rmap->{$num});
    my $k = int($num/$base);
    my $b = $num % $base;
    my $E = &baseN($rmap, $k);
    return $E . $rmap->{$b};
}

sub unbaseN
{
    my ($map, $undone) = @_;
    my $valid = "a-z" . chr(241) . "A-Z" . chr(209);
    return 0 if( $undone !~ m/^[$valid\d]+$/);
    my $base       = scalar(keys %{$map});
    my $scale      = length($undone);
    my $digital    = $undone =~ m/^\d+$/;
    my @bits        = split(//, sprintf("%0$scale" . "s", $undone));
    my $bit        = sprintf('%01s', shift @bits);
    $undone        = join("", @bits);
    $scale         = scalar(@bits);
    my $multiplier = $base**$scale;
    my $done         += int($map->{$bit}) * $multiplier;
    $done         += &unbaseN($map, $undone);
    return $done;
}

sub uniquify_list {
    my ($list)  =  @_;
    # benchmark score:  28 microseconds
    #my (%uniq, @return);
    #for my $l (@$list) {
    #    if(!exists($uniq{$l})) {
    #        $uniq{$l}++;
    #        push(@return, $l);
    #    }
    #}
    #return \@return;

    # benchmark score:  19 microseconds
    my %uniq;
    @$list  =  map { my $l = $_; if(!exists($uniq{$l})) { $uniq{$l}++; $l; }; } @$list;
    return $list;
}

1;
