# Check all assertions in all scripts supplied as arguments
#
# JBS 12 November 1997

# Added Refusal and Acceptance data outputing, changed outputing of
# data to accomodate Michael's BEHAVIOUR tag.
# Purpose of file is still exactly as described above.




set fdr_library $env(FDRTCLLIBRARY)

source "$fdr_library/fdrDirect.tcl"
source "$fdr_library/fdr2_remote.tcl"
# Parse the assertion and call the corresponding check

proc checkTerm {sess term} {
    global current_file
    global assert_number
    set len [llength $term]
    set first [lindex $term 0]
    switch -exact $first {
	"_REFINE" {
	    if { $len == 5 } {
		set model [lindex $term 1]
		set spec [lindex $term 2]
		set impl [lindex $term 3]
		puts "\nChecking $spec \[$model= $impl"
		return [checkRefinement $sess $spec $model $impl]
	    }
	}
	"_PROPERTY" {
	    if { $len == 4 } {
		set proc [lindex $term 1]
		set prop [lindex $term 2]
		set model "FD"
		regexp {\[([A-Z]+)\]} $prop all model
		puts "\nChecking $proc $prop"
		switch -glob $prop {
		    *deadlock*free* {
			return [checkDeadlock $sess $proc $model]
		    }
		    *livelock*free* {
			return [checkLivelock $sess $proc]
		    }
		    *divergence*free* {
			return [checkLivelock $sess $proc]
		    }
		    *deterministic* {
			return [checkDeterminism $sess $proc $model]
		    }
		}
	    }
	    if { $len == 6 } {
		set proc [lindex $term 1]
		set prop [lindex $term 2]
		set arg  [lindex $term 3]
		puts "\nChecking $proc $prop $arg"
		switch -glob $prop {
		    *locally*noninterfering* {
			return [checkLNI $sess $proc $arg]
		    }
		    *fair*diverg* {
			return [checkFairlyDivergent $sess $proc $arg]
		    }
		    *\[*=* {
			set proc1 [lindex $term 1]
			set refines [lindex $term 2]
			set proc2 [lindex $term 3]
			set prop  [lindex $term 4]
			set model "bad"
			set qualified "$current_file-$assert_number"
			regexp {\[([A-Z]*)=} $refines all model
			if { $model == ""  || $model == "T" || $model == "F" } {
			    switch -glob $prop {
				*prob* {
				    puts "\nChecking $proc1 $refines $proc2 $prop"
				    return [checkProbabilistic $sess $proc1 $proc2 $model $qualified]
				}
			    }
			}
		    }
		}
	    }
	    if { $len == 8 } {
		set proc1 [lindex $term 1]
		set refines [lindex $term 2]
		set proc2 [lindex $term 3]
		set prop  [lindex $term 4]
		set arg [lindex $term 5]
		set model "bad"
		regexp {\[([A-Z]*)=} $refines all model
		if { $model == "" } {
		    switch -glob $prop {
			{*tau*priority*} {
			    puts "\nChecking $proc1 $refines $proc2 $prop $arg"
			    return [checkTauPriority $sess $proc1 $proc2 $arg]
			}
		    }
		} elseif { $model == "T" } {
		    switch -glob $prop {
			{*tau*priority*} {
			    puts "\nChecking $proc1 $refines $proc2 $prop $arg"
			    return [checkTauPriority $sess $proc1 $proc2 $arg]
			}
			{*watchdog*} {
			    set qualified "$current_file-$assert_number"
			    puts "\nChecking $proc1 $refines $proc2 $prop $arg"
			    return [checkWatchdog $sess $proc1 $model $proc2 $arg $qualified]
			}
		    }
		}
		if { $model == "F" } {
		    switch -glob $prop {
			{*watchdog*} {
			    set qualified "$current_file-$assert_number"
			    puts "\nChecking $proc1 $refines $proc2 $prop $arg"
			    return [checkWatchdog $sess $proc1 $model $proc2 $arg $qualified]
			}
		    }
		}
	    }
	}
    }
    return "unknown"
}

proc reportResultShort {res negate} {
    global include_refusals
    if [results_are_structured] {
	set rest [lrange $res 1 end]
	set res  [lindex $res 0]
    }
    if {$negate} {
	switch -exact $res {
	    "xtrue"   { set res "xfalse" }
	    "true"    { set res "false" }
	    "xfalse"  { set res "xtrue" }
	    "false"   { set res "true" }
	}
    }
    puts $res
}

# Report (negating if needed) a result

proc reportResult {res negate} {
    global include_refusals
    if [results_are_structured] {
	set rest [lrange $res 1 end]
	set res  [lindex $res 0]
    }
    if {$negate} {
	switch -exact $res {
	    "xtrue"   { set res "xfalse" }
	    "true"    { set res "false" }
	    "xfalse"  { set res "xtrue" }
	    "false"   { set res "true" }
	}
    }
    puts $res
    if [results_are_structured] {
	foreach witness $rest {
	    if { $include_refusals } {
		set i 0
		set j 0
		if { [ string range [lindex [lindex $witness $i] 0] 0 8]  == "BEHAVIOUR"} {
		    incr i
		    set j 0
		} else {
		    puts "This error should not occur"
		    puts "The data structure returned by the main fdr code"
		    puts "is not of the form expected."
		    puts "(bracketing lost somewhere, can no longer tell which"
		    puts "traces/refusals/acceptances go with which path)"
		}
		while { $i < [llength $witness] } {
		    # Loop INVARIANT: Have output all data up to element
		    #before previous "BEHAVIOUR", or all data if i==[llength witness]

		    if { [ string range [lindex [lindex $witness $i] 0] 0 8]  == "BEHAVIOUR" || [expr $i+1] == [llength $witness] } {
			set k $i
			if { [string range [lindex [lindex $witness $k] 0] 0 8]  == "BEHAVIOUR" } { set k [expr $k - 1] }
			set example [lrange $witness [expr $j + 1 ] $k]
			set label [lindex [ lindex $witness $j] 0]
			puts "BEGIN $label"
			foreach record $example {
			    set label [lindex $record 0]
			    set lines [join [lrange $record 1 end] "\n"]
			    puts "BEGIN $label\n$lines\nEND $label"
			}
			set label [lindex [ lindex $witness $j] 0]
			puts "END $label\n"
			set j $i
		    }
		    incr i
		}
	    } else {
		foreach record $witness {
		    set label [lindex $record 0]
		    set lines [join [lrange $record 1 end] "\n"]
		    puts "BEGIN $label\n$lines\nEND $label"
		}
	    }
	}
    }
    switch -exact $res {
	"xtrue"   { return 0 }
	"true"    { return 0 }
	"xfalse"  { return 1 }
	"false"   { return 1 }
    }
    return 0
}
# Parse the assertions returned by the engine
# Don't worry if you don't understand this

set assert_number 0

proc checkList {sess terms} {
  global assert_number
  set assert_number 0
  set count 0
  set collect {}
  set ret 0
  foreach x $terms {
    lappend collect $x
    if { [string range $x 0 0] == "_" } {
      if { $x == "_END" } {
        incr count -1
        if { $count == 0 } {
          set negate 0
          while { [lindex $collect 0] == "_NOT" } {
            set negate [expr ! $negate]
            set len [llength $collect]
            set collect [lrange $collect 1 [expr $len - 2]]
          }
	  incr assert_number
          set ret [expr $ret + [reportResult [checkTerm $sess $collect] $negate]]
          set collect { }
        }
      } else {
        incr count
      }
    }
  }
  return $ret
}

# Check all the assertions in a script

set current_file {}

proc checkAllAssertions {f} {
  global current_file
  set current_file "$f"
  puts "Filename: $f"
  puts "Checking $f"
  set s [load $f]
  return [checkList $s [$s assertions]]
}

set current_file {}

# This procedure defined an extra flag '-singleAssertion' for fdr2 in Batch modus.
# This option makes possible checking of specific assertions from file.
# Example: fdr2 batch -singleAssertion "Refinement P FD Q" $src_file

proc checkSingleAssertion {f assertion type} {
  global current_file
  set current_file "$f"
  set s [load $f]
  set ret 0
  set negate 0
  if {$type == "Refinement" } {
     set spec [lindex $assertion 0]
     set model [lindex $assertion 1]
     set impl [lindex $assertion 2]
     set ret [expr $ret + [reportResult [checkRefinement $s "$spec" "$model" "$impl"] $negate] ]
  } elseif {$type == "Deadlock"} {
     set proc [lindex $assertion 0]
     set model [lindex $assertion 1]
     puts "Deadlock $assertion"
     set ret [expr $ret + [reportResult [checkDeadlock $s "$proc" "$model"] $negate] ]
  } elseif {$type == "Determinism"} {
     set proc [lindex $assertion 0]
     set model [lindex $assertion 1]
     puts "Determinism $assertion"
     set ret [expr $ret + [reportResult [checkDeterminism $s "$proc" "$model"] $negate] ]
  } else {
     # must be Livelock
     set proc [lindex $assertion 0]
     puts "Livelock $assertion"
     set ret [expr $ret + [reportResult [checkLivelock $s "$proc"] $negate] ]
  }
  return $ret

}

# Check all the command line arguments

set len [llength $argv]

for { set i 0 } { $i < $len } { incr i } {
  set f [lindex $argv $i]
  if { $f == "-max" } {
    incr i
    if { $i < $len } {
      config max_errors [lindex $argv $i]
    }
  } elseif { $f == "-depth" } {
    incr i
    if { $i < $len } {
      set report_depth [lindex $argv $i]
    }
  } elseif { $f == "-trace" } {
    set include_traces 1
  } elseif { $f == "-refusals" } {
    set include_traces 1
    set include_refusals 1
  } elseif {$f == "-singleAssertion"} {
    incr i
    if {$i < $len} {
        set strArg {}
        set type [lindex $argv $i]
        incr i
        set spec [lindex $argv $i]
        lappend strArg $spec
        incr i
        if {$type != "Livelock"} {
            set model [lindex $argv $i]
            lappend strArg $model
            incr i
        }
        if {$type == "Refinement"} {
           set impl [lindex $argv $i]
           lappend strArg $impl
           incr i
        }
        if {$i < $len} {
            if {[lindex $argv $i] == "--turnoffSupercompilation"} {
                config supercompile 0
                incr i
            }
            if {[lindex $argv $i] == "--turnoffBisim"} {
                config leaf_bisimulation 0
                incr i
            }
        }
        exit [checkSingleAssertion [lindex $argv $i] "$strArg" $type]
    }
  } elseif {$f == "-turnoffOptions"} {
      incr i
      if {$i < $len} {
          if {[lindex $argv $i] == "--turnoffSupercompilation"} {
                config supercompile 0
                incr i
            }
            if {[lindex $argv $i] == "--turnoffBisim"} {
                config leaf_bisimulation 0
                incr i
            }
      }
      exit [checkAllAssertions [lindex $argv $i]]
  } else {
      exit [checkAllAssertions $f]
  }
}

