# btracer_pure_procedures.tcl
# Pure Procedures for the btracer module.
# The purpose of this module is to decouple the pure TCL code (code without side-effects) from btracer.tcl in order to make it more convenient for testing the TCL source code with the tcltest package.


# Procedures for the CSP Assertion viewer

proc isRefinementAssertion {str} {
   if {[lsearch -regexp $str {(\[T=|\[F=|\[FD=|\[R=|\[RD=)}] > -1} {
         return true
   }
   return false
}

proc SwapProcesses {str} {
   set l [split [string map {" \[T= " $ " \[F= " $ " \[FD= " $ " \[R= " $ " \[RD= " $} $str] \$]
   set nextToLastL [expr {[string length [lindex $l 1]]-1}]
   set nextToLastR [expr {[string length [lindex $l 0]]-1}]
   set left_proc  [string range [lindex $l 1] 0 $nextToLastL]
   set right_proc [string range [lindex $l 0] 0 $nextToLastR]
   regexp {(\[T=|\[F=|\[FD=|\[R=|\[RD=)} $str match ref_op
   return "$left_proc $ref_op $right_proc"
}

proc procFindCommonElement {list1 list2 list3} {
     if {$list1 == {}} {
         return $list1
     } else {
         foreach i $list1 {
           set str1 [lindex [split $i \.] 0]
           if {$i != "tau" && [lsearch -exact $list2 $i] == -1 && [lsearch -regexp $list3 $str1] != -1} {
           return [list $i]
           }
         }
         # no common element found
         return [lsort -unique $list1]
     }
}

proc procGetModelStringForTheTraceDebuggerTitle {str} {
     switch -regexp $str {
         {.*LTL:.*} { return "LTL" }
         {.*CTL:.*} { return "CTL" }
         {.*\[T=.*} { return "Trace Refinement" }
         {.*\[F=.*} { return "Failures Refinement" }
         {.*\[FD=.*} { return "Failures-divergence Refinement" }
         {.*\[R=.*} { return "Refusals Refinement" }
         {.*\[RD=.*} { return "Refusals-divergence Refinement" }
         {.*deadlock.*} {return "Deadlock Free"}
         {.*livelock.*} {return "Livelock Free"}
         {.*deterministic.*} {return "Deterministic"}
         default {return "Unknown"}
     }
}


proc procGetSubString {list cutAtSymbol} {
    set result {}
    set len [llength $list]
    for {set i 0} {$i < $len} {incr i} {
        set el [lindex $list $i]
        if {$el != $cutAtSymbol} {
		lappend result $el
	} else {
		break
	}
    }
    return $result
}

# procedures for CSP Assertion's Trace Debugger of the CSP Assertion Viewer

proc procPrettyPrintCSPTrace {list} {
    set len [llength $list]
    set i 0
    set result {}
    while {$i < $len} {
	if {[lindex $list $i] == "go:"} {
		incr i
		lappend result [lindex $list $i]
		set step 1
	} elseif {[lindex $list $i] == "refuse:"} {
		incr i
		set refused_set [procGetSubString [lrange $list $i $len] "go:"]
                set step [llength $refused_set]
		set refused_set_string [string map {" " "," "Sigma" "\u03A3" "bullet" "\u25CF"} [join $refused_set]]
		if {[lsearch -regexp $refused_set_string {(\u03A3|\u25CF)}] > -1} {
			lappend result $refused_set_string
		} else {
			lappend result "\{$refused_set_string\}"
		}
	} else {
		lappend result [lindex $list $i]
		set step 1
	}
	incr i $step
    }
    return $result
}


proc procLimitStringLength {str len} {
   if {[string length $str] > 30} {
       set str [string range $str 0 29]
       append str "..."
   }
   return $str
}

proc returnASpecificNumberOfElements {list numb} {
   if {[llength $list] >= $numb} {
     set lres {}
     set tau_event false

     for {set i 0} {$i < $numb} {incr i} {
          set el [lindex $list $i]
          if {$el != ""} {
              lappend lres $el
          }
     }
     lappend lres "..."
     return $lres
   } else {
     return $list
   }
}

proc ldelete { list value } {
   set ix [lsearch -exact $list $value]
   if {$ix >= 0} {
       return [lreplace $list $ix $ix]
   } else {
       return $list
   }
}

proc procIsAnyOfTheSubstringsInTheString {str args} {
  if {[llength $args] == 0} {
       return true
  } else {
       foreach i $args {
            if {[string first $i $str] > -1} {
                return true
            }
       }
       return false
  }
}

proc makePrettyTimeFormat {ti} {
    set millisec [expr {round($ti)}]
    set min [expr {$millisec/60000}]
    set rem [expr {$millisec%60000}]
    set sec [format "%6.2d" [expr {$rem/1000}]]
    set rem1 [expr {$rem%1000}]
    set ms [format "%6.3d" $rem1]
    return [string map {" " ""} "$min m$sec .$ms s"]
}

