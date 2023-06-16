## check if the argument 'str' is a refinement assertion.
proc isRefinement {str} {
   set c [expr {[string first "\[F=" $str] + [string first "\[T=" $str] + \
           [string first "\[FD=" $str]} >= 0 ]
   return $c
}

## check if the argument 'str' is a determinism assertion.
proc isDeterminism {str} {
  if [regexp {([^:]+)\s*:\[(.*)} $str match proc type] {
      return [expr { [string first "deterministic" $type] >=0} ]
  } else {
     return 0
  }
}

## check if the argument 'str' is a deadlock assertion.
proc isDeadlock {str} {
  if [regexp {([^:]+)\s*:\[(.*)} $str match proc type] {
      return [expr { [string first "deadlock" $type] >= 0} ]
  } else {
      return 0
  }
}

## check if the argument 'str' is a livelock assertion.
proc isLivelock {str} {
  if [regexp {([^:]+)\s*:\[(.*)} $str match proc type] {
      return [expr { [expr { [string first "livelock" $type] >= 0}] || \
             [expr { [string first "divergence" $type] >= 0}] } ]
  } else {
      return 0
  }
}

## splits the refinement expression in three parts
proc splitRefinementAssertionString {exp} {
    regexp {\[([FDT]+)=} $exp all Model
    set str [split [string map {" " "" "\[T=" $ "\[F=" $ "\[FD=" $} $exp] \$]
    set Spec [lindex $str 0]
    set Impl [lindex $str end]
    return "$Spec $Model $Impl"
}

## splits the determinism or deadlock expression in two parts
proc splitDAssertion {exp} {
  regexp {([^:]+):\[(.*)} $exp match Spec type
  set proc [string map {" " ""} $Spec]
  if [regexp {\[([FD]+)\]} $type all model] {
      return "$proc $model"
   } else {
      return "$proc FD"
   }
}

## extracts the process identifier from the livelock expression 'exp'
proc splitLivelockAssertion {exp} {
  regexp {([^:]+)\s*:\[(.*)} $exp match Spec ignore
  return [string map {" " ""} $Spec]
}

## Build the command strings that will be later executed in the TestScript.tcl
proc getExpressionCommands {exp Negated f ProBDir FDRDir timeout path fdr_path} {
    set Spec ""
    set Model ""
    set Impl ""
    set setTimeOutFlag2 ""
    set setTimeOutFlag3 ""
    if {$timeout > 0} {
        set setTimeOutFlag2 "--timeout $timeout >> result_prob.txt 2>> error_occurred.txt"
        set setTimeOutFlag3 "--timeout $timeout >> result_prob_1.txt 2>> error_occurred_1.txt"
    } else {
        set setTimeOutFlag2 ">> result_prob.txt 2>> error_occurred.txt"
        set setTimeOutFlag3 ">> result_prob_1.txt 2>> error_occurred_1.txt"
    }
    if {$fdr_path == ""} {
        set cmd1 [list exec fdr2 batch -singleAssertion]
    } else {
        set cmd1 [list exec $fdr_path/fdr2 batch -singleAssertion]
    }
    if {$path == ""} {
        set cmd2 [list exec probcli $f]
        set cmd3 [list exec probcli $ProBDir]
    } else {
        set cmd2 [list exec $path/probcli $f]
        set cmd3 [list exec $path/probcli $ProBDir]
    }
    if [isRefinement $exp] {
        set SplitedExpression [splitRefinementAssertionString $exp]
        set Spec [lindex $SplitedExpression 0]
        set Model [lindex $SplitedExpression 1]
        set Impl [lindex $SplitedExpression 2]
        set cmd1 [concat $cmd1 "Refinement $Spec $Model $Impl $FDRDir >> result.txt"]
        set cmd2 [concat $cmd2 {-cspref $Spec "\[$Model=" $Impl}]
        set cmd3 [concat $cmd3 {-cspref $Spec "\[$Model=" $Impl}]
        set Assertion "$Spec \[$Model= $Impl"
    } elseif [isDeterminism $exp] {
        set SplitedExpression [splitDAssertion $exp]
        set Spec [lindex $SplitedExpression 0]
        set Model [lindex $SplitedExpression 1]
        set cmd1 [concat $cmd1 "Determinism $Spec $Model $FDRDir >> result.txt"]
        set cmd2 [concat $cmd2 "-cspdeterminism $Spec $Model"]
        set cmd3 [concat $cmd3 "-cspdeterminism $Spec $Model"]
        set Assertion "$Spec :\[ deterministic \[$Model\]\]"
    } elseif [isDeadlock $exp] {
        set SplitedExpression [splitDAssertion $exp]
        set Spec [lindex $SplitedExpression 0]
        set Model [lindex $SplitedExpression 1]
        set cmd1 [concat $cmd1 "Deadlock $Spec $Model $FDRDir >> result.txt"]
        set cmd2 [concat $cmd2 "-cspdeadlock $Spec $Model"]
        set cmd3 [concat $cmd3 "-cspdeadlock $Spec $Model"]
        set Assertion "$Spec :\[ deadlock free \[$Model\]\]"
    } elseif [isLivelock $exp] {
        set Spec [splitLivelockAssertion $exp]
        set cmd1 [concat $cmd1 "Livelock $Spec $FDRDir >> result.txt"]
        set cmd2 [concat $cmd2 "-csplivelock $Spec"]
        set cmd3 [concat $cmd3 "-csplivelock $Spec"]
        set Assertion "$Spec :\[ livelock free\]"
    } else {
       error "This type of assertions is not supported by ProB: $exp."
    }
    set cmd2 [concat $cmd2 $setTimeOutFlag2]
    set cmd3 [concat $cmd3 $setTimeOutFlag3]
    if {$Negated == "yes"} {
         set Assertion "not $Assertion"
    }
    return [list $Spec $Model $Impl $cmd1 $cmd2 $cmd3 $Assertion]
}


proc getCheckAssertionTime {file} {
    set fileId [open $file r]
    set lines [split [read $fileId] \n]
    set RuntimeShowed false
    foreach line $lines {
         if [regexp {^(Runtime:) \s*([0-9]+) \s*(ms)$} $line all str ti ms] {
                 set AssertionCheckTime $ti
                 set RuntimeShowed true
                 break
                 ## Check-Assertion-Time appeares only once in $file
             }
    }
    close $fileId
    if {$RuntimeShowed} {
        return $AssertionCheckTime
    } else {
        return 0
        #error "Runtime doesn't appear in the result file $file."
    }
}

## filters result from file
proc getFDR2Result {file Negated} {
    if [file exists $file] {
       set fileId [open $file r]
       set FDR2Res [lindex [split [read $fileId] \n] 1]
       set FDR2Res [join [string tolower [string map {x ""} $FDR2Res]]]
       close $fileId
       if {$Negated == "yes"} {
            if {$FDR2Res == "true"} {
                 set FDR2Res "false"
            } elseif {$FDR2Res == "false"} {
                 set FDR2Res "true"
            } else {
                  #don't change result
            }
        }
        return $FDR2Res
     } else {
        puts "File $file does not exist."
        exit
     }
}

## procedure recognize a different errors that can occure during the execution of the test script
proc falselyCspAssertionErrorOnly {error_output} {
     if {![file exists $error_output]} {
          puts "File $error_output does not exist, there is no error in this assertion check occurred."
          exit
     }
     set fileId [open $error_output r]
     set lines [split [read $fileId] \n]
     foreach line $lines {
          ##puts "The Line: $line"
          if {$line == "! Error while parsing CSP-M: "} {
               return "Parse Error!!!"
          } elseif {$line == "! time_out"} {
               return "Timeout Error!!!"
          } elseif [regexp {\*\*\* Timeout occurred:\s* [0-9]+} $line match] {
               return "Forced Timeout Error!!!"
          } elseif {$line == "! Not an animatable CSP Process: "} {
               set process [lindex $lines [expr {[lsearch $lines $line] +1}]]
               return "Not an animatable CSP Process: $process."
          }
     }
     close $fileId
     return "true"
}

proc getProBResult {error_file result_file Negated} {
     if {[file size $error_file] == 0} {
         ## file is empty, no error occurred (i.e. Assertion check passed)
         set ProBRes "true"
     } else {
         set resultFileRes [falselyCspAssertionErrorOnly $result_file]
         if {$resultFileRes != "true"} {
             set isCspAssertionError $resultFileRes
         } else {
             set isCspAssertionError [falselyCspAssertionErrorOnly $error_file]
         }
         if {$isCspAssertionError == "true"} {
             set ProBRes "false"
         } else {
             set ProBRes $isCspAssertionError
         }
     }
     if {$Negated == "yes"} {
         if {$ProBRes == "true"} {
             set ProBRes "false"
         } elseif {$ProBRes == "false"} {
             set ProBRes "true"
         } else {
             # don't change ProBRes
         }
     }
     return $ProBRes
}

## the argument 'ti' is the time in microseconds
proc makePrettyTimeFormat {ti} {
    set microsec [expr {round($ti)}]
    set min [expr {$microsec/60000000}]
    set rem [expr {$microsec%60000000}]
    set sec [format "%6.2d" [expr {$rem/1000000}]]
    set rem1 [expr {$rem%1000000}]
    set ms [format "%6.3d" [expr {round($rem1/double(1000))}]]
    return [string map {" " ""} "$min m$sec .$ms s"]
}

## the argument 'ti' is the time in microseconds
proc timeInSecondsOnly {ti} {
    set sec [expr {$ti/double(1000000)}]
    return "$sec"
}

proc summarizeTime {timeList} {
    set summTime 0
    foreach i $timeList {
         set summTime [expr {$summTime +$i}]
    }
    return [makePrettyTimeFormat $summTime]
}

## filters all assertion declarations from the .csp file 'f'
## returns a list from the filtered assertions without the key word assert
proc collectAllAssertionsFromFile {f} {
   set fileAssertionsList {}
   ## file must exist
   if [file exists $f] {
      if {[file isfile $f] && [expr {[file extension $f] == ".csp"}]} {
          set fileId [open $f r]
          ## we pick all declarations from file through the 'split' command
          foreach line [split [read $fileId] \n] {
               ## there can be some comments after the assertions, we ignore them
               if [regexp {^\s*(assert) \s*(.*)\s*(--.*|-\})} $line all a exp comment] {
                     lappend fileAssertionsList $exp
               }
               if [regexp {^\s*(assert) \s*(.*)\s*} $line all a exp1] {
                     ## guarantees that no commented assertions will be picked
                     if {[string first "--" $exp1] == -1} {
                          lappend fileAssertionsList $exp1
                     }
               }
          }
          close $fileId
          return $fileAssertionsList
       } else {
          error "No .csp file inputed!"

       }
   } else {
       error "File does not exist!"

   }
}

## Procedure collects directory-recursive all .csp files and
## saves these in a list which will be returned at the end of
## this procedure
## @args: startDir (the directory we start to collect the .csp files)
## and  recursivityDeep (the deepness of the recursion)
proc collectCspFilesFromDirectory {startDir recursivityDeep} {
    ## saving the current directory
    set pwd [pwd]
    set fileNameList {}
    ## $startDir must be a directory otherwise it doesn't make sense
    if [file isdirectory $startDir] {
        if [catch {cd $startDir} err] {
            puts stderr $err
            return
        }
        puts "Matching .csp files from directory $startDir..."
        ## collect all .csp Files directory $startDir
        foreach match [glob -nocomplain -- *.csp] {
            puts "Adding filePath of .../$match into fileNameList..."
            lappend fileNameList [file join $startDir $match]
        }
        ## if the recursivity deep is not setted or the Test Script
        ## didn't reach the bottom level that were given by the -deep flag
        ## otherwise no more recursion.
        if {$recursivityDeep != 0} {
           set recursivityDeep [expr {$recursivityDeep - 1}]
           foreach file [glob -nocomplain *] {
               if [file isdirectory $file] {
                  set fList [collectCspFilesFromDirectory [file join $startDir $file] $recursivityDeep]
                  set fileNameList [concat $fileNameList $fList]
               }
           }
        }
        ## changing back to the primary directory
        cd $pwd
        puts "Files to be tested:"
        foreach i $fileNameList {
              puts "$i"
        }
        return $fileNameList
    } else {
        error "The path $startDir is not a directory!!!"
    }
}

proc DeleteTemporaryTxtFiles {TemporaryFiles} {
    foreach file $TemporaryFiles {
        if [file exists $file] {
            file delete $file
        }
    }
}

proc meanMeasurements {measurements} {
    if {[llength $measurements] == 0} {
        # no mean to be calculated
        return -1
    }
    set commandTime 0
    set len [llength $measurements]
    for {set i 0} {$i < $len} {incr i} {
         incr commandTime [lindex $measurements $i]
    }
    return [expr {$commandTime/double($len)}]
}

proc standardDeviation {mean measurements} {
    set len [llength $measurements]
    set quadratSumm 0
    foreach i $measurements {
         set quadratSumm [expr {$quadratSumm + (($i - $mean)**2)}]
    }
    if {$len > 1} {
        return [expr {sqrt($quadratSumm/double($len-1))}]
    } else {
        return 0.0
    }
}

proc measurementsResultsList {cmd repeats commandNumber} {
    set Results {}
    for {set i 0} {$i < $repeats} {incr i} {
         set now [clock microseconds]
         set t [lindex [time {catch {eval $cmd} result} 1] 0]
         set after [clock microseconds]
         if {$commandNumber == 2} {
             if {[expr {[falselyCspAssertionErrorOnly result_prob.txt] != "true"}] || \
                 [expr {[falselyCspAssertionErrorOnly error_occurred.txt] != "true"}] } {
                 # there is an error in the refinement chech or check is just timeouted
                 # we break at this step and the results are irrelevant for this test
                 # because we are only interested in tests that make sence
                 return {}
             }
         }
         lappend Results $t
         lappend Results [expr {$after - $now}]
    }
    puts "Repeats : $repeats"
    puts "Reeesults : $Results"
    return $Results
}

## The table of the t-Quantils for estimating the confidence intervall of the sample tests
proc readTQuantilsTable {} {
     global tQuantilsTable
     if {![file exists t_Quantils_table.txt]} {
          error "$file does not exists, please check the directory."
     } else {
          set fileId [open t_Quantils_table.txt r]
          set lines [split [string trimright [read $fileId] \n] \n]
          set i 1
          foreach line $lines {
              foreach coef [split $line] percent {50 75 80 90 95 98 99 99_8} {
                   set tQuantilsTable($percent,$i) $coef
               }
               if {$i <= 30} {incr i}
               if {$i == 31} {set i infinity}
          }
          close $fileId
     }
}

proc getTQuantilFromTable {interval n} {
     global tQuantilsTable
     ##puts [array get tQuantilsTable]
     if {$n > 30} {
        set n "infinity"
     }
     set interv $interval
     if {$interval < 50} {
         set interv 50
     } elseif {$interval > 99 && [expr {[string is double -strict $interval] ||\
                                        [string is integer -strict $interval]}]} {
         set interv 99_8
     } else {
         foreach under {50 75 80 90 95 98} upper {75 80 90 95 98 99} {
             if {$interval > $under && $interval < $upper} {
                 set interv $upper
                 puts "The given large $interval for the Confidence interval will be set on $upper large."
                 break
             }
         }
     }
     set index "$interv,$n"
     if [catch {set val $tQuantilsTable($index)}] {
         puts "Error by reading from tQuantilsTable. Setting automatically the confidence interval on large 95%."
         return 1.960
     }
     return $val
}

proc getConfindenceInterval {interval measurements} {
    set n [llength $measurements]
    set quantil [getTQuantilFromTable $interval $n]
    set mean [meanMeasurements $measurements]
    set st_deviation [standardDeviation $mean $measurements]
    set interval_length [expr {$quantil*$st_deviation/(double(sqrt($n)))}]
    if {[expr {$mean - $interval_length}] < 0} {
         return [list 0 [expr {$mean + $interval_length}]]
    } else {
         return [list [expr {$mean - $interval_length}] [expr {$mean + $interval_length}]]
    }
}

proc addNewTestCase {file fileName assertion time result error} {
    set fileId [open $file a]
    if {$result == "false"} {
        puts $fileId "<testcase classname=\"FDR_ProCSP Assertion Check\" name=\"$fileName, $assertion\" time=\"$time\">\n\
                          <error type=\"error\" message=\"$error\"/>\n\
                      </testcase>"
    } else {
        puts $fileId "<testcase classname=\"FDR_ProCSP Assertion Check\" name=\"$fileName, $assertion\" \
                       message=\"$fileName, Assertion: $assertion, Error: $error\" time=\"$time\"/>"
    }
    close $fileId
}

proc cleanTmpFiles {directory} {
    foreach pl_file [glob -nocomplain *.pl] {
        if [file exists $pl_file] {
             file delete $pl_file
        }
    }
    foreach txt_file [glob -nocomplain *.txt] {
        if [file exists $txt_file] {
             file delete $txt_file
        }
    }
}

proc printFile {file} {
    set fileId [open $file r]
    set output [split [read $fileId] \n]
    puts "Output of $file:"
    puts $output
    close $fileId
}

proc isRealError {file} {
    set fileId [open $file r]
    set lines [split [read $fileId] \n]
    foreach line $lines {
          if {$line == "! *** error occurred ***" || \
              $line == "! An error occurred !"} {
               return true
          }
    }
    return false
}


global tQuantilsTable
array set tQuantilsTable {}
readTQuantilsTable
