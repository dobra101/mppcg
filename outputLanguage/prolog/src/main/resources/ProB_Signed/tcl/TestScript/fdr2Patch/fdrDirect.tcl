#####################################################################
# Drive the engine directly for simple FDR integration
#
# To get started:
#   cd $FDRHOME
#   bin/fdr2tix -insecure -nowindow
#   source lib/fdr/fdrDirect.tcl
#
# Load a script using
#   set s [load "demo/abp.csp"]
#
# Print the result of a refinement check using
#   puts [checkRefinement $s "SPEC" "FD" "make_system(RCVA)"]
#
# Finish
#   exit
#
# See below for other checks and descriptions of the results.
#
# (-insecure -nowindow starts fdr2tix without opening a X11 window
# and defining commands inside a slave interpreter.  It is *nothing*
# to do with the licensing software.  You still require a valid
# licence to use fdr2 in this way.)
######################################################################


# Routines to map model names to internal option flags


# Added output of (and formating functions for) Acceptance(s) and Refusals data.

proc flag {model} {
  if { $model == "T" } {
    return "-t"
  } else {
    return [flag_fd $model]
  }
}

proc flag_fd {model} {
  switch $model {
    R       { return "-r" }
    RD      { return "-rd" }
    V       { return "-v" }
    VD      { return "-vd" }
    F       { return "-f" }
    FD      { return "" }
    ""      { return "" }
    default { return "bad" }
  }
}

proc results_are_structured { } {
  global include_traces
  return $include_traces
}

# Load a script into a fresh session

proc load { name } {
  set sess [session]
  $sess load [file dirname $name] [file tail $name]
  return $sess
}

proc formatRefusals {string} {
    set x [string trim $string]
    regsub -all "\014" $x "\n" x
    set res {}
    set x [ string range $x 1 [expr [string length $x] -2 ]]
    foreach e [split $x "\n"] {
	lappend res $e
    }
    return $res
}

proc formatAcceptances {string} {
    set x [string trim $string]
    regsub -all "\014" $x "\n" x
    set res {}
    set x [ string range $x 1 [expr [string length $x] -2 ]]
    foreach e [split $x "\n"] {
	lappend res $e
    }
    return $res
}


proc explodeTrace { string } {
  set x [string trim $string]
  regsub ^<(.*)>$ $x {\1} x
  regsub -all "\014" $x "\n" x
  set res {}
  foreach e [split $x "\n"] {
    lappend res [string trim $e]
  }
  return $res
}

proc extractBehaviour { behaviour label } {
    global include_traces
    global include_refusals
    if { $include_refusals } {
	set res [list [list "BEHAVIOUR $label"]]
	foreach pair [split [string trim [$behaviour describe] "\v"] "\v"] {
	    set vals [split $pair "\t"]
	    set key [string trim [lindex $vals 0]]
	    set txt [string trim [lindex $vals 1]]
	    switch -glob $key {
		"*Allows*" {
		    if { $include_traces } {
			set part [list "ALLOWS" ]
			set Texample [concat $part [explodeTrace $txt]]
			lappend res $Texample
		    }
		}
		"*Performs*" {
		    if { $include_traces } {
			set part [list "TRACE"]
			set Texample [concat $part [explodeTrace $txt]]
			lappend res $Texample
		    }
		}
		"*Accepts*" {
		    if { $include_refusals } {
			set part [list "ACCEPTANCES"]
			lappend res [concat $part [formatAcceptances $txt]]
		    }
		}
		"*Refuses*" {
		    if { $include_refusals } {
			set part [list "REFUSALS"]
			lappend res [concat $part [formatRefusals $txt]]
		    }
		}
		# Can extend to other components as needed
	    }
	}
    } else {
	set res {}
	if { $include_traces } {
	    foreach pair [split [string trim [$behaviour describe] "\v"] "\v"] {
		set vals [split $pair "\t"]
		set key [string trim [lindex $vals 0]]
		set txt [string trim [lindex $vals 1]]
		switch -glob $key {
		    "*Allows*" {
			set part [list "ALLOWS" ]
			lappend res [concat $part [explodeTrace $txt]]
		    }
		    "*Performs*" {
			set part [list "TRACE $label"]
			lappend res [concat $part [explodeTrace $txt]]
		    }
		}
		# Can extend to other components as needed
	    }
	}
    }
    $behaviour delete
    return $res
}

proc formatBehaviour { witness actor behaviour } {
    global include_refusals
    if { $include_refusals } {
	set res {}
	lappend res [extractBehaviour $behaviour "example=$witness process=$actor"]
	return $res
    } else {
	return [list [extractBehaviour $behaviour "example=$witness process=$actor"]]
    }
}

# Collect traces down the tree to a given ply

proc formatBehaviours { depth witness actor tree path } {
  global report_depth
  set res {}
  if { $depth != $report_depth } {
    set label "example=$witness process=$actor path=$path"
    append res " "
    append res [extractBehaviour [$tree behaviour $path] $label]
    for {set c 0} {$c < [$tree children $path]} {incr c} {
      append res " "
      append res [formatBehaviours [expr $depth + 1] $witness $actor $tree "$path $c"]
    }
  }
  return $res
}

proc collectResults { hyp } {
    global include_traces
    global report_depth
    set res [$hyp assert]
    if { [string index $res 0] == "x" } {
	if { [results_are_structured] } {
	    set res [list $res]
	    set dbg [$hyp debug]
	    set witnesses [$dbg witnesses]
	    set actors    [$dbg actors]
	    for {set w 0} {$w < $witnesses} {incr w} {
		for {set a 0} {$a < $actors} {incr a} {
		    if { $report_depth == 0 } {
			set res [concat $res [formatBehaviour $w $a [$dbg attribution $w $a]]]
		    } else {
			set tree [$dbg debugtree $w $a]
			lappend res [formatBehaviours 0 $w $a $tree [$tree root]]
			$tree delete
		    }
		}
	    }
	    $dbg delete
	}
    }
    $hyp delete
    return $res
}

# Carry out the basic kinds of check
#
# Possible returns
#   "bad"       model supplied to check was incorrect
#   "true"      result is true
#   "xtrue"     result is true and can sensibly be debugged
#   "false"     result is false
#   "xfalse"    result is false and can sensibly be debugged
#   "broken"    check completed, but result was unsound
#
# An error will, in general, just crash the Tcl interpeter.
#
# If include_traces is non-zero, then results will also include
# traces for each relevant actor and witness.

proc checkRefinement { sess p1 model p2 } {
  set flag [flag $model]
  if { $flag != "bad" } {
    puts "String: $p1 $model $p2"
    set spec [$sess compile $p1 $flag]
    set impl [$sess compile $p2 $flag]
    set hyp [$spec refinedby $impl $flag]
    $spec delete
    $impl delete
    return [collectResults $hyp]
  } else {
    return "bad"
  }
}

proc checkLNI { sess p h } {
  set impl [$sess compile $p "-f"]
  set high [$sess compile "CHAOS($h)\\(diff(Events,$h))" "-f"]
  set hyp [$impl locallynoninterfering $high]
  $impl delete
  $high delete
  return [collectResults $hyp]
}

proc checkFairlyDivergent { sess p events } {
  set ev [$sess compile "CHAOS($events)\\(diff(Events,$events))" "-f"]
  set proc [ $sess compile $p ]
  set hyp [$proc fairlydivergent $ev]
  $proc delete
  return [collectResults $hyp]
}

proc checkTauPriority { sess p1 p2 tocks } {
    set ts "CHAOS($tocks)\\(diff(Events,$tocks))"
    set spec [$sess compile_tau_priority $p1 $ts]
    set impl [$sess compile_tau_priority $p2 $ts]
    set hyp [$spec refinedby $impl "T"]
    $spec delete
    $impl delete
    return [collectResults $hyp]
}

proc checkProbabilistic { sess p1 p2 model source } {
  set flag [flag $model]
  if { $flag == "-f" || $flag == "-t" } {
    set spec [$sess compile $p1 $flag]
    set impl [$sess compile $p2 $flag]
    set hyp [$spec probabilistic $impl $flag "$source"]
    $spec delete
    $impl delete
    return [collectResults $hyp]
  } else {
    return "bad"
  }
}

proc checkWatchdog { sess p1 model p2 compress source } {
  set flag [flag $model]
  if { $flag == "-f" || $flag == "-t" } {
    set spec [$sess compile $p1 $flag]
    set impl [$sess compile $p2 $flag]
    set hyp [$spec watchdog $impl $flag $compress "$source"]
    $spec delete
    $impl delete
    return [collectResults $hyp]
  } else {
    return "bad"
  }
}

proc checkDeadlock { sess p model } {
  set flag [flag_fd $model]
  if { $flag != "bad" } {
    set proc [$sess compile $p $flag]
    set hyp [$proc deadlockfree $flag]
    $proc delete
    return [collectResults $hyp]
  } else {
    return "bad"
  }
}

proc checkDeterminism { sess p model } {
  set flag [flag_fd $model]
  if { $flag != "bad" } {
    set proc [$sess compile $p $flag]
    set hyp [$proc deterministic $flag]
    $proc delete
    return [collectResults $hyp]
  } else {
    return "bad"
  }
}

proc checkLivelock { sess p } {
  set proc [$sess compile $p]
  set hyp [$proc livelockfree]
  $proc delete
  return [collectResults $hyp]
}

# configure some sensible default options
config report none
config supercompile 1
config leaf_bisimulation 1
config max_errors 1
# config compact wk

global include_traces
set include_traces 0

global include_refusals
set include_refusals 0

global report_depth
set report_depth 0

