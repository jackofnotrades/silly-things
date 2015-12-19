#!/usr/bin/perl

use strict;
use warnings;
use Digest::MD5 qw(md5_hex);
use Time::HiRes qw(usleep);
use Getopt::Long;

## TODO
##    add map specs for V, H, and F maps to neighborhood subs

my $opts = {};
GetOptions($opts,
           's|size=i',
           'b|start_state=s',
           't|timing=i',
           'i|iterations=i',
           'l|live_cell=s',
           'd|dead_cell=s',
           'm|map=s' => sub { my (undef, $map) = @_; &help("Invalid map specification: $map\n", 2) if($map !~ m/^(t(or(oid|us))?|v(ertical( wrap)?)?|h(orizontal( wrap)?)?|f(lat))$/i) },
           'n|neighborhood=s' => sub { my (undef, $neighborhood) = @_; &help("Invalid neighborhood specification: $neighborhood", 2) if($neighborhood !~ m/^(m(oore)?|e(xtended( moore)?)?|c(onway)?|v(on neumann)?|n(eumann))$/i) },
           'u|states_file=s',
           'r|results_file=s',
           'x|do_not_cycle',
           'q|log',
           'z|no_output',
           'h|u|?|help|usage' => sub { &help('', 0) });

&GOL(@ARGV);

sub GOL {
    my ($size,       $start_state, 
        $timing,     $iterations,
        $live_cell,  $dead_cell,
        $shape,      $neighborhood,
        $state_file, $results_file)  =  ($opts->{'s'}, $opts->{'b'}, $opts->{'t'}, $opts->{'i'}, $opts->{'l'},
                                         $opts->{'d'}, $opts->{'m'}, $opts->{'n'}, $opts->{'u'}, $opts->{'r'});
    srand();
    $size  =  8 if(!defined($size));
    if(defined($start_state)                 and
       $start_state                          and
       length($start_state) !=  ($size ** 2)    ) {
        &help("Invalid starting state $start_state for map size $size x $size: " . (length($start_state) < ($size ** 2) ? "argument too short\n" : "argument too long\n") . "\nArgument must be " . ($size ** 2) . " characters long (" . '#' x ($size ** 2) . ")\n", 2);
    }
    $state_file        =  "gol_start_states.dat" if(!defined($state_file)   || $state_file   eq '');
    $results_file      =  "gol_outcomes.dat"     if(!defined($results_file) || $results_file eq '');
    my $cells          =  $size ** 2;
    my $possibilities  =  1; # accumulates possible states to help determine if all starting states have been exhausted
    my (%states, $tried_patterns);
    for my $c (1 .. $cells) {
        $possibilities += &n_choose_k($cells, $c);
    }
    if(!defined($start_state) || $start_state eq '') {
        $start_state  =  join('', (map { int(rand(2)); } (1 .. ($size**2))));
        if($opts->{'q'} and -f $state_file) {
            open(STATES, "<$state_file") or die("Cannot open recorded start states file $state_file: $!");
            while(my $line = <STATES>) {
                $line =~ m/([01]+)/;
                $states{$1}++;
            }
            close(STATES);
        }
        if(exists($states{$start_state})) {
            print "." unless($opts->{'z'});
            $tried_patterns++;
            my %tried;
            $tried{$start_state}++;
            while(exists($states{$start_state})) {
                print "." unless($opts->{'z'});
                $tried_patterns++;
                $tried{$start_state}++;
                if((keys %tried) == $possibilities) {
                    die("Exhausted all $possibilities possible combos for $cells\-cell ($size\-sided) map");
                }
                $start_state  =  join('', (map { int(rand(2)); } (1 .. ($size**2))));
            }
            $states{$start_state}++;
            print "\n" unless($opts->{'z'});
        } else {
            $states{$start_state}++;
        }
    }
    $timing          =  1 if(!$timing);
    $iterations      =  ($size ** 4) if(!defined($iterations));
    $live_cell       =  '0' if(!defined($live_cell));
    $dead_cell       =  ' ' if(!defined($dead_cell));
    print "Start State:\t$start_state\n" unless($opts->{'z'});
    my @start_state  =  split('', $start_state, (-1));
    my (@universe, @next_universe, %patterns);
    # populate universe
    for my $x (0 .. ($size - 1)) {
        for my $y (0 .. ($size - 1)) {
            $universe[$x][$y]  =  shift @start_state;
        }
    }
    # serialize pattern for memory
    $patterns{'"' . md5_hex(join('', map { my $x = $_; join('', @{$x})} @universe)) . '"'}++;
    # run iterations
    my %neighborhoods = ('M' => \&moore, # Moore
                         'C' => \&conway, # Conway
                         'E' => \&extended);  # extended Moore
    
    my ($i, $done) = (0,);
    print '=' x $size . " Epoch $i\n\n" . join('', map { my $x = $_; join('', map { $_ eq 0 ? $dead_cell : $live_cell; } @{$x}) . "\n" } @universe) . "\n\n" unless($opts->{'z'});
    my (%seen, @seen, $cycling);
    while(!$done) {
        # apply Game Of Life rules at each step
        # display and pause
        for my $x (0 .. ($size - 1)) {
            for my $y (0 .. ($size - 1)) {
                # neighborhood of cell at $x, $y
                my @positions = ([($x - 1) < 0 ? ($size - 1) : ($x - 1), $y], 
                                 [($x + 1) > ($size - 1) ? 0 : ($x + 1), $y],
                                 [$x, ($y + 1) > ($size - 1) ? 0 : ($y + 1)],
                                 [$x, ($y - 1) < 0 ? ($size - 1) : ($y - 1)],
                                 [($x + 1) > ($size - 1) ? 0 : ($x + 1), ($y + 1) > ($size - 1) ? 0 : ($y + 1)],
                                 [($x + 1) > ($size - 1) ? 0 : ($x + 1), ($y - 1) < 0 ? ($size - 1) : ($y - 1)],
                                 [($x - 1) < 0 ? ($size - 1) : ($x - 1), ($y + 1) > ($size - 1) ? 0 : ($y + 1)],
                                 [($x - 1) < 0 ? ($size - 1) : ($x - 1), ($y - 1) < 0 ? ($size - 1) : ($y - 1)]);
                my $sum = 0;
                my $state  =  $universe[$x][$y];
                for my $p (@positions) {
                    #print "$p->[0]\t$p->[1]\n";
                    $sum += $universe[$p->[0]][$p->[1]];
                }
                if($state) {
                    # was alive
                    $next_universe[$x][$y]  =  $sum > 3 || $sum < 2 ? '0' : '1';
                                                                    #  ^     ^ 
                                                                    #  |     still alive
                                                                    #  now dead
                } else {
                    # was dead
                    $next_universe[$x][$y]  =  $sum == 3 ? 1 : '0';
                                                         # ^    ^
                                                         # |    still dead
                                                         # resurrected
                }
            }
        }
        @universe = @next_universe;
        usleep($timing);
        if($iterations !~ m/^inf(init(y|e))?$/i) {
            $done++ if($i == ($iterations - 1));
        }
        my $pat   =  join('', map { my $x = $_; join('', @{$x})} @universe);
        my $seen  =  $size > 5 ? $patterns{md5_hex($pat)}++ : $patterns{$pat}++;
        my $live_cells;
        map { $live_cells += $_ } split(//, $pat);
        if($seen) {
            push(@seen, $pat) if(!exists($seen{$pat}));
            $seen{$pat}++;
        }
        print '=' x $size . " Epoch " . ++$i . ($seen ?  $cycling ? " [cycling]" : " [seen]" : '') . ($live_cells ? '' : "[dead]")  ."\n\n" . join('', map { my $x = $_; join('', map { $_ eq 0 ? $dead_cell : $live_cell } @{$x}) . "\n" } @universe) . "\n\n" unless($opts->{'z'});
        $done++ if(!$live_cells or $seen > 1);
        if($done) {
            my $fold  =  sub { my $op = shift; my $term = shift; map { $term = eval("$term$op$_") } @_; return $term };
            my @results;
            if(-f $results_file and $opts->{'q'}) {
                unless($cycling) {
                    open(RESULTS, "<$results_file") or die("Cannot open results file $results_file for reading: $!");
                    while(my $line = <RESULTS>) {
                        $line =~ s/\s*$//;
                    push(@results, $line);
                    }
                    close(RESULTS);
                }
            }
            unless($cycling or !$opts->{'q'}) {
                open(RESULTS, ">$results_file") or die("Cannot open or create results file $results_file for editing: $!");
                if(@results) {  
                    for my $r (@results) {
                        print RESULTS "$r\n";
                    }
                }
            }
            my $stable_epoch;
            if((keys %seen) > 1 and $live_cells) {
                $stable_epoch  =  ($i - &$fold('+', 0, (values %seen)));
                unless($cycling or !$opts->{'q'}) {
                    print RESULTS "$stable_epoch epochs to stable cycle (". join(",", @seen) . ")\tStarting pattern:  $start_state\n";
                    open(STATES, ">$state_file") or die("Cannot open recorded start states file $state_file for editing: $!");
                    for my $s (sort { $a cmp $b } keys %states) {
                        print STATES "$s\n";
                    }
                    close(STATES);
                    close(RESULTS);
                }
                $cycling++;
                $done = 0 unless($i == $iterations or $opts->{'x'});
            } elsif($live_cells) {
                my $stable_epoch = ($i - ($seen + 1));
                unless(!$opts->{'q'}) { 
                    print RESULTS "$stable_epoch epochs to stable pattern ($pat)\tStarting pattern:  $start_state" . ($tried_patterns ? "\tTried $tried_patterns patterns before reaching a novel one\n" : "\n");
                }
            } else {
                unless(!$opts->{'q'}) {
                    print RESULTS "$i epochs to population death ($pat)\tStarting pattern:  $start_state" . ($tried_patterns ? "\tTried $tried_patterns patterns before reaching a novel one\n" : "\n");
                }
            }
            close(RESULTS) unless($cycling or !$opts->{'q'});
            unless($cycling or !$opts->{'q'}) {
                open(STATES, ">$state_file") or die("Cannot open recorded start states file $state_file for editing: $!");
                for my $s (sort { $a cmp $b } keys %states) {
                    print STATES "$s\n";
                }
                close(STATES);
            }
        }
    }
}

sub moore {
    # create a Moore neighborhood
    my ($x, $y, $size, $shape) = @_;
    my %shapes = ('T' => sub { return ([($x-1) < 0 ? ($size-1) : ($x-1), $y], 
                                       [($x+1) > ($size-1) ? 0 : ($x+1), $y],
                                       [$x, ($y+1) > ($size-1) ? 0 : ($y+1)],
                                       [$x, ($y-1) < 0 ? ($size-1) : ($y-1)],
                                       [($x+1) > ($size-1) ? 0 : ($x+1), ($y+1) > ($size-1) ? 0 : ($y+1)],
                                       [($x+1) > ($size-1) ? 0 : ($x+1), ($y-1) < 0 ? ($size-1) : ($y-1)], 
                                       [($x-1) < 0 ? ($size-1) : ($x-1), ($y+1) > ($size-1) ? 0 : ($y+1)],
                                       [($x-1) < 0 ? ($size-1) : ($x-1), ($y-1) < 0 ? ($size-1) : ($y-1)]); 
                              },
                  'V' => sub {},
                  'H' => sub {},
                  'F' => sub {});
    return (&{$shapes{$shape}($x,$y,$size)});
}

sub conway {
    # create a Conway/Von Neumann neighborhood
    my ($x, $y, $size, $shape) = @_;
    my %shapes = ('T' => sub { return ([($x-1) < 0 ? ($size-1) : ($x-1), $y],
                                       [($x+1) > ($size-1) ? 0 : ($x+1), $y],
                                       [$x, ($y+1) > ($size-1) ? 0 : ($y+1)],
                                       [$x, ($y-1) < 0 ? ($size-1) : ($y-1)]);
                              },
                  'V' => sub {},
                  'H' => sub {},
                  'F' => sub {});
    return (&{$shapes{$shape}($x,$y,$size)});
}

sub extended {
    # create an extended Moore neighborhood
    my ($x, $y, $size, $shape) = @_;
    my %shapes = ('T' => sub { return ([($x-1) < 0 ? ($size-1) : ($x-1), $y],
                                       [($x+1) > ($size-1) ? 0 : ($x+1), $y],
                                       [$x, ($y+1) > ($size-1) ? 0 : ($y+1)],
                                       [$x, ($y-1) < 0 ? ($size-1) : ($y-1)],
                                       [($x-2) < 0 ? ($size-2) : ($x-2), $y],
                                       [($x+2) > ($size-1) ? 0 : ($x+2), $y],
                                       [$x, ($y+2) > ($size-1) ? 0 : ($y+2)],
                                       [$x, ($y-2) < 0 ? ($size-1) : ($y-2)],
                                       # still need (x+/-1,y+/-1), (x+/-1,y+/-2),
                                       #            (x+/-2,y+/-1), (x+/-2,y+/-2)
                                       [($x+1) > ($size-1) ? 0 : ($x+1), ($y+1) > ($size-1) ?    0 : ($y+1)],
                                       [($x+1) > ($size-1) ? 0 : ($x+1), ($y-1) < 0 ? ($size-    1) : ($y-1)],
                                       [($x-1) < 0 ? ($size-1) : ($x-1), ($y+1) > ($size-1) ?    0 : ($y+1)],
                                       [($x-1) < 0 ? ($size-1) : ($x-1), ($y-1) < 0 ? ($size-    1) : ($y-1)],

                               );
                              },
                  'V' => sub {},
                  'H' => sub {},
                  'F' => sub {});
    return (&{$shapes{$shape}($x,$y,$size)});
}

sub n_choose_k {
    my ($n, $k)  =  @_;
    my $N        =  $n;
    my $K        =  $k;
    my ($i, $j)  =  ($n, $k);
    for (($n - ($k + 1)) .. $n) {
        $i--;
        $N *= $i if($i);
    }
    for (1 .. $k) {
        $j--;
        $K *= $j if($j);
    }
    return ($K / $N);
}

sub help {
    my ($message, $exit)  =  @_;
    print $message;
    print<<"!";

    USAGE:  $0 [[option 1] [option 2] ...]

    All arguments are optional; defaults are used for those omitted

    OPTIONS:

        -s              Size of grid (s x s)
       --size           Default is 8 (64 cells)

        -b              Starting state as a string of 1's and 0's
       --start_state    Length must be equal to -s squared
                        Default is randomly chosen (avoiding 
                        already encountered patterns as recorded
                        in file specified by -u, if -q enabled)

        -i              Number of iterations to perform
       --iterations     Program will exit if stable pattern (not
                        cycle) or dead map is encountered before
                        this
                        Default is scaled to -s
                        
        -t              Time between epochs in microseconds
       --timing         Default is 125000

        -l              Character or string to represent live cells
       --live_cell      Should be same length as -d
                        Default is \'0\'

        -d              Character or string to represent dead cells
       --dead_cell      Should be same length as -l
                        Default is \' \'

        -m              Map type to use
       --map            Valid options are:
                            t/toroid/torus
                            v/vertical/vertical wrap
                            h/horizontal/horizontal wrap
                            f/flat
                        Default is t
                        
        -n              Neighborhood type to use
       --neighborhood   Valid options are:
                            m/Moore
                            e/Extended/Extended Moore
                            c/n/v/Conway/Neumann/von Neumann
                        Default is m

        -u              "Used" states file
       --states_file    Log of already-encountered starting states
                        Only used if -q option is enabled
                        Default is \'gol_starting_states.dat\' in
                        the current directory

        -r              Results file
       --results_file   Log of the starting state, ending state, and
                        number of epochs to reach that state
                        Ending state can be dead, stable (static) 
                        pattern, or stable cycle
                        Only used if -q option is enabled
                        Default is \'gol_results.dat\' in the current
                        directory
                        
       -x               Exit once a cycle is detected (and recorded, if
      --do_not_cycle    -q is enabled)

       -q               Log results, including reading used starting
      --log             states to avoid repeating them (unless -b was
                        explicitly specified)
                        Default is off

       -z               Do not print anything to STDOUT
      --no_output       Does not affect -q option

       -h               Print this help message and exit
       -u
       -?
      --help
      --usage

!
    exit($exit);
}
