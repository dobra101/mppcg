#!/usr/bin/env tclsh8.5

## This is a Test Script for testing evaluating of assertions in CSP Specifications.
## The main goal is to make a time analysis between our tool ProCSP and the FDR2-tool and to compare the performance of the both.
## This Script requires an 'excel' package that I downloaded from tcl.tk internet site.
## For now we use this package for creating .xml files compatible with the Microsoft Excel and Open Office programs.
## Later I will maybe implement an own package for creating and manipulating .xls files.

## We can call this script with additional arguments. The following flags and arguments by calling the TestScript.tcl file
## are required:
## -f followed by the .csp file path or -r followed by the directory path for testing of all files from this directory
## -o followed by the name of the .xml file where you want the result to be written. (Don't have to exists)
## -repeats followed by the number of tests you want to be repeated for which test.

## The -o and -repeats flags are optional flags. If you don't specify the output file then the results of the tests will
## be written in the file default.xml and in the directory where the TestScript.tcl is founded. By not giving the number of repeats
## for which test, then this will be automatically set on 30.

## Via the -deep flag we can define the recursivity deep of the Test Script for getting test files from the directories.
## -deep 0 : The Test Script tests files only from the current directory
## -deep 1 : The Test Script tests files only from the current directory and the directories which are a level deeper.
## -deep n : and so on...
#package require excel
source TestScriptProcedures.tcl
source excel/excelPackage.tcl
global dir FDRDir ProBDir
global recursive recurivityDeep
global numb TimeOut ConfidenceLevel
global glob_probcli_Path cspm_path fdr_path
global resultsDir
set resultsDir ""; set cspm_path ""
set glob_probcli_Path ""; set fdr_path ""
set state flag
set FDRDir "./";set ProBDir "./";set dir "./"
set numb 30; set recursive no; set recursivityDeep -1; set fileName "default.xml"
set TimeOut 0
set ConfidenceLevel 95
set book [excel::createWorkbook]
set timeListFDR {0};set timeListProB {0};set timeListProBWithoutParsing {0};set timeListProBWithoutParsingAndCompiling {0}
set probArray {}

set worksheet1 [excel::createWorkSheet $book "Assertions Results."]
set worksheet2 [excel::createWorkSheet $book "Time-Assertion Results."]
set worksheet3 [excel::createWorkSheet $book "Time-Assertion Measuring in Seconds."]
set worksheet4 [excel::createWorkSheet $book "Statistical Measuring"]

foreach arg $argv {
    switch -- $state {
         flag {
            switch -glob -- $arg {
                -f  {set state fdir}
                -r  {set state rdir}
                -o  {set state fname}
                -repeats {set state num}
                -deep {set state deep}
                -timeout {set state timeout}
                -interval {set state interval}
                -path {set state path}
                -path_cspm {set state cspm}
                -path_fdr2 {set state fdr}
                -path_results {set state results}
                default {puts "Unknown flag $arg." ; exit}
             }
         }
         fdir {
            set tmp $arg
            if [file isfile $tmp] {
                if {[expr {[file extension $tmp] == ".csp"}] || \
                    [expr {[file extension $tmp] == ".cspm"}]} {
                      set dir $tmp
                      set state flag
                } else {
                      puts "This Test Script can only handle Csp Specifications: \
                             Files with the extension '.csp' or '.cspm'."
                      exit
                }
            } else {
                puts "File $arg doesn't exist. If you want to run this Test Script \
                       recursively (e.g. testing multiple files from a directory) then \
                       you have to use the '-r' flag instead of '-f'."
                exit
            }
         }
         fname {
            set tmp $arg
            if [file exists $tmp] {
                 puts "Warning: This file $tmp will be overwritten."
            }
            set fileName $tmp
            set state flag
         }
         num {
            set numb $arg
            if {[string is integer -strict $numb]} {
                set state flag
            } else {
                puts "The Argument after the -repeats flag is not an integer value."
                exit
            }
         }
         rdir {
            set tmp $arg
            if {[file exists $tmp]} {
                if {[file isdirectory $tmp]} {
                   set recursive yes
                   set dir $tmp
                   set state flag
                } else {
                   puts "You can use the flag -r only with a directory path."
                   exit
                }
            } else {
                puts "This directory or file '$tmp' doesn't exist."
                exit
            }
         }
         deep {
            set tmp $arg
            if {[string is integer -strict $tmp]} {
                if {$tmp < 0} {
                    puts "The recursivity deep parameter must be not negative."
                    exit
                }
                if { [file isdirectory $dir] && [expr {$dir != "./"}] } {
                    set recursivityDeep $tmp
                    set state flag
                } else {
                    puts "The flag -deep can only be used when the -r flag is used."
                    exit
                }
            } else {
                    puts "The Argument after the -deep flag is not an integer value."
                    exit
            }
        }
        timeout {
            set tmp $arg
            if {[string is integer -strict $tmp]} {
                if {$tmp < 0} {
                    puts "The timeout argument must be not negative."
                    exit
                 }
                 if {$tmp <= 30} {
                    puts "Be aware, the timeout time is too short and there are maybe tests that need more time for execution."
                 }
                 set TimeOut $tmp
                 set state flag
            } else {
                    puts "The Argument after the -deep flag is not an integer value."
                    exit
            }
        }
        interval {
            set tmp $arg
            if {[string is integer -strict $tmp]} {
                if {$tmp < 0} {
                    puts "The confidence interval argument must be not negative."
                    exit
                }
                set ConfidenceLevel $tmp
                set state flag
            } else {
                puts "The argument is not a number, the confidence interval will be automatically setted on 95%."
                set ConfidenceLevel 95
            }
     }
     cspm {
          set cspm_path $arg
          set state flag
     }
     path {
          set glob_probcli_Path $arg
          set state flag
     }
     fdr {
          set fdr_path $arg
          set state flag
     }
     results {
          set resultsDir $arg
          set state flag
     }
  }
}

proc runFileTests {exp worksheet1 worksheet2 worksheet3 worksheet4 f resultsFile} {
    global dir FDRDir ProBDir
    global timeListFDR timeListProB
    global timeListProBWithoutParsing timeListProBWithoutParsingAndCompiling
    global probArray ConfidenceLevel glob_probcli_Path
    global numb TimeOut fdr_path
    set ErrorToReport ""
    set FDR2Res ""
    set ProBRes ""
    set Negated "no"
    if [regexp {^(not) (.*)$} $exp all n r_exp] {
        set Negated "yes"
        set exp $r_exp
    }
    DeleteTemporaryTxtFiles {result_prob.txt result.txt error_occurred.txt error_occurred_1.txt}
    set CommandList [getExpressionCommands $exp $Negated $f $ProBDir $FDRDir $TimeOut $glob_probcli_Path $fdr_path]
    foreach var {Spec Model Impl cmd1 cmd2 cmd3 Assertion} i {0 1 2 3 4 5 6} {
         eval {set $var [lindex $CommandList $i]}
    }
    puts "Checking Assertion \'$Assertion\' with the FDR2 tool"
    puts "Command: $cmd1"
    set cmd1ResultsList {}
    for {set i 0} {$i < $numb} {incr i} {
         if [file exists result.txt] {
             file delete result.txt
         }
         set now [clock clicks -microseconds]
         catch {eval $cmd1}
         set after [clock clicks -microseconds]
         lappend cmd1ResultsList [expr {$after - $now}]
    }
    set t1 [meanMeasurements $cmd1ResultsList]
    set cmd1ConfInterval [getConfindenceInterval $ConfidenceLevel $cmd1ResultsList]
    #set t1 [lindex [time {catch {eval $cmd1} result1 } $numb] 0]
    puts "Checking Assertion \'$Assertion\' with the ProB tool"
    puts "Command: $cmd2"
    set cmd2ResultsList {}
    set assertionTimeResultsList {}
    for {set i 0} {$i < $numb} {incr i} {
       if [file exists result_prob.txt] {
           file delete result_prob.txt
       }
       set now [clock clicks -microseconds]
       catch {eval $cmd2}
       set after [clock clicks -microseconds]
       if {[expr {[falselyCspAssertionErrorOnly result_prob.txt] != "true"}] ||\
           [expr {[falselyCspAssertionErrorOnly error_occurred.txt] != "true"}] } {
                set ErrorToReport [falselyCspAssertionErrorOnly error_occurred.txt]
                set cmd2ResultsList {0}
                set assertionTimeResultsList {0}
                break
       }
       lappend cmd2ResultsList [expr {$after - $now}]
       lappend assertionTimeResultsList [expr {1000*[getCheckAssertionTime result_prob.txt]}]
       #file delete result_prob.txt
    }
    foreach var {AssertionCheckWithCompileTime AssertionCheckTime} intrv {cmd2ConfInterval assTConfInterval} \
                              value [list $cmd2ResultsList $assertionTimeResultsList] {
          eval {set $var [meanMeasurements $value]}
          eval {set $intrv [getConfindenceInterval $ConfidenceLevel $value]}
    }
    puts "Command (withoutParsing): $cmd3"
    set cmd3ResultsList {}
    for {set i 0} {$i < $numb} {incr i} {
       set now [clock clicks -microseconds]
       catch {eval $cmd3}
       set after [clock clicks -microseconds]
       if {[expr {[falselyCspAssertionErrorOnly result_prob_1.txt] != "true"}] ||\
           [expr {[falselyCspAssertionErrorOnly error_occurred_1.txt] != "true"}] } {
                set cmd3ResultsList {0}
                break
       }
       lappend cmd3ResultsList [expr {$after - $now}]
       file delete result_prob_1.txt
    }
    set t3 [meanMeasurements $cmd3ResultsList]
    set cmd3ConfInterval [getConfindenceInterval $ConfidenceLevel $cmd3ResultsList]
    #set t3 [lindex [time {catch {eval $cmd3} result3 } $numb] 0]
    foreach var {timeListFDR timeListProB timeListProBWithoutParsing timeListProBWithoutParsingAndCompiling} value \
              [list $t1 $AssertionCheckWithCompileTime $t3 $AssertionCheckTime] {
          eval {lappend $var $value}
    }
    #set StandardDeviation [getStandardDeviation $probArray $AssertionCheckWithCompileTime]
    puts "Reading results..."
    set FDR2Res [getFDR2Result result.txt $Negated]
    #printFile result_prob.txt
    #printFile error_occurred.txt
    set ProBRes [getProBResult error_occurred.txt result_prob.txt $Negated]
    puts "Writting results on worksheet 1..."
    excel::addRow $worksheet1 [list "$Assertion" "$FDR2Res" "$ProBRes"]
    set reportTime [expr {($t1 + $AssertionCheckWithCompileTime + $t3)/1000000}]
    if {$FDR2Res == $ProBRes} {
         ## put the results of the time measuring only if the results of the asertions check
         ## are equal
          foreach var {ti1 ti2 ti3 ti4} value [list $t1 $AssertionCheckWithCompileTime $t3 $AssertionCheckTime] {
              eval {set $var [makePrettyTimeFormat $value]}
          }
          foreach var {tiSec1 tiSec2 tiSec3 tiSec4} value [list $t1 $AssertionCheckWithCompileTime $t3 $AssertionCheckTime] {
              eval {set $var [timeInSecondsOnly $value]}
          }
          foreach upper {ti1_u ti2_u ti3_u ti4_u} lower {ti1_l ti2_l ti3_l ti4_l} \
                   value [list $cmd1ConfInterval $cmd2ConfInterval $cmd3ConfInterval $assTConfInterval] {
              eval {set $lower [makePrettyTimeFormat [lindex $value 0]]}
              eval {set $upper [makePrettyTimeFormat [lindex $value 1]]}
          }
          puts "Writting time results on worksheet 2..."
          excel::addRow $worksheet2 [list "$Assertion" "$ti1" "$ti2" "$ti3" "$ti4"]
          excel::addRow $worksheet3 [list "$Assertion" "$tiSec1" "$tiSec4" "$tiSec2" "$tiSec3"]
          excel::addRow $worksheet4 [list "$Assertion" "$ti1" "$ti1_l" "$ti1_u" "$ti2" "$ti2_l" "$ti2_u"\
                                  "$ti3" "$ti3_l" "$ti3_u" "$ti4" "$ti4_l" "$ti4_u"]
          addNewTestCase $resultsFile [file tail $f] $Assertion $reportTime "PASSED" ""
    } else {
          if {$ErrorToReport == ""} {
              set ErrorToReport "Different assertion results (of FDR2 and ProB) for the Assertion: \'$Assertion\'.\
                               FDR2 says: $FDR2Res and ProB says: $ProBRes."
          }
          addNewTestCase $resultsFile [file tail $f] $Assertion $reportTime "false" "$ErrorToReport"
    }
    DeleteTemporaryTxtFiles {result_prob.txt result.txt error_occurred.txt error_occurred_1.txt}
}

proc testAssertionsFromSpecificFile {book worksheet1 worksheet2 worksheet3 worksheet4 assertions f} {
    global dir FDRDir ProBDir
    global timeListFDR timeListProB
    global timeListProBWithoutParsing timeListProBWithoutParsingAndCompiling
    global numb ConfidenceLevel glob_probcli_Path cspm_path resultsDir
    ## get the current directory
    set pwd [pwd]; set toDirectory [file dirname $f]
    DeleteTemporaryTxtFiles {result_prob.txt result.txt error_occurred.txt error_occurred_1.txt}
    ## we change to current file directory
    set resultsName [lindex [split [file tail $f] \.] 0]
    set resultsName "$resultsName.xml"
    set ResultName [file join $resultsDir $resultsName]
    set resultsFileId [open "$ResultName" w]
    puts $resultsFileId "<testsuite>"
    close $resultsFileId
    puts "Picked file name for the junit results: $ResultName"
    if [catch {cd $toDirectory} err] {
        puts stderr $err
        return
    }
    set FDRDir $f;set ProBDir "$f.pl"
    puts "dir: $dir";puts "ProBDir: $ProBDir";puts "FDR2Dir: $FDRDir"
    excel::addRow $worksheet1 [list $f];excel::addRow $worksheet2 [list $f];excel::addRow $worksheet3 [list $f]
    excel::addRow $worksheet4 [list $f]
    puts "Parsing $dir Specification with the cspm tool..."
    set CmdCSPM ""; set CmdCompile ""; set CmdParseAndCompile ""
    if {$cspm_path == ""} {
        set CmdCSPM [list exec cspm translate $f --prologOut=$ProBDir]
    } else {
        set CmdCSPM [list exec $cspm_path/cspm translate $f --prologOut=$ProBDir]
    }
    set parseTime [lindex [time {catch {eval $CmdCSPM}} $numb] 0]
    puts "Compiling $ProBDir Prolog File..."
    if {$glob_probcli_Path == ""} {
        set CmdComple [list exec probcli $ProBDir]
        set CmdParseAndCompile [list exec probcli $f 2>> compile_error.txt]
    } else {
        set CmdComple [list exec $glob_probcli_Path/probcli $ProBDir]
        set CmdParseAndCompile [list exec $glob_probcli_Path/probcli $f 2>> compile_error.txt]
    }
    catch {eval $CmdParseAndCompile} compilingResult
    if [isRealError compile_error.txt] {
       ## Something went wrong, we terminate the further execution of the script only for this file and we continue with the next test file.
       puts $compilingResult
       excel::addRow $worksheet1 [list "An error by running probcli command occurred: Parse or Rename Error (see output on the command line)."]
       addNewTestCase $ResultName [file tail $f] "no_Assertion" "no_time" "false" \
                        "An error by running probcli command occurred: Parse or Rename Error (see output on the command line)."
       set resultsFileId [open "$ResultName" a]
       puts $resultsFileId "</testsuite>"
       close $resultsFileId
       if [file exists compile_error.txt] {
           file delete compile_error.txt
       }
       return [llength $assertions]
    }
    set compilingTime [lindex [time {catch {eval $CmdCompile}} $numb] 0]
    puts "Parsing and Compiling $f with probcli..."
    set parseAndCompilingTime [lindex [time {catch {eval $CmdParseAndCompile}} $numb] 0]
    ## Now we don't need to parse everytime an .csp specification when we execute probcli
    foreach var {parseTime cTime parseAndCompilingTime} value [list $parseTime $compilingTime $parseAndCompilingTime] {
          eval {set $var [makePrettyTimeFormat $value]}
    }
    excel::addRow $worksheet1 [list "Assertion:" "FDR2_Tool" "ProB"]
    excel::addRow $worksheet2 [list "Assertion:" "FDR2_Tool" "ProB" "ProB without Parsing" "ProB Assertion Check Time"]
    excel::addRow $worksheet3 [list "Assertion:" "FDR2_Tool" "ProB Assertion Check Time" "ProB" "ProB without Parsing"]
    excel::addRow $worksheet4 [list "Assertion:" "FDR2_Tool" "Lower CL" "Upper CL" \
                                                 "ProB" "Lower CL" "Upper CL" \
                                                 "ProB without Parsing" "Lower CL" "Upper CL" \
                                                 "ProB Assertion Check Time" "Lower CL" "Upper CL"]
    foreach exp $assertions {
          puts "Running Assertion Check for $exp..."
          runFileTests $exp $worksheet1 $worksheet2 $worksheet3 $worksheet4 $f $ResultName
    }
    foreach var {summ1 summ2 summ3 summ4} value [list $timeListFDR $timeListProB $timeListProBWithoutParsing\
                                                      $timeListProBWithoutParsingAndCompiling] {
          eval {set $var [summarizeTime $value]}
    }
    excel::addRow $worksheet2 [list "Summarized Time:" "$summ1" "$summ2" "$summ3" "$summ4"]
    puts "Writting Parsing and Compiling time results on worksheet 2..."
    excel::addRow $worksheet2 [list ""]
    excel::addRow $worksheet2 [list ""]
    excel::addRow $worksheet4 [list ""]
    excel::addRow $worksheet4 [list ""]
    excel::addRow $worksheet2 [list "Parse Time" "" "$parseTime"]
    excel::addRow $worksheet2 [list "Analyzing/Compiling Time" "" "$cTime"]
    excel::addRow $worksheet2 [list "Parse and Analyzing/Compiling Time" "" "$parseAndCompilingTime"]
    excel::addRow $worksheet4 [list "Confidence Interval at confindence level $ConfidenceLevel"]
    cleanTmpFiles [file dirname $f]
    cd $pwd
    set resultsFileId [open "$ResultName" a]
    puts $resultsFileId "</testsuite>"
    close $resultsFileId
    return [llength $assertions]
}

proc runTestScript {book worksheet1 worksheet2 worksheet3 worksheet4} {
    global recursive dir recursivityDeep
    global timeListFDR timeListProB timeListProBWithoutParsing timeListProBWithoutParsingAndCompiling
    set statList {}
    if {$recursive == "no"} {
       set AssertionsList [collectAllAssertionsFromFile $dir]
       puts "The following Assertions will be tested: $AssertionsList."
       set numAssertions [testAssertionsFromSpecificFile $book \
                     $worksheet1 $worksheet2 $worksheet3 $worksheet4 $AssertionsList $dir]
       lappend statList "$numAssertions $dir"
    } else {
       set fileNameList [collectCspFilesFromDirectory $dir $recursivityDeep]
       foreach file $fileNameList {
            puts "Starting of Testing assertions from file $file..."
            set AssertionsList [collectAllAssertionsFromFile $file]
            puts "The following Assertions will be tested: $AssertionsList."
            set numAssertions [testAssertionsFromSpecificFile $book \
                     $worksheet1 $worksheet2 $worksheet3 $worksheet4 $AssertionsList $file]
            lappend statList "$numAssertions $file"
            set timeListFDR {0};set timeListProB {0}
            set timeListProBWithoutParsing {0};set timeListProBWithoutParsingAndCompiling {0}
            for {set i 0} {$i < 4} {incr i} {
                excel::addRow $worksheet1 [list ""];excel::addRow $worksheet2 [list ""];excel::addRow $worksheet3 [list ""]
                excel::addRow $worksheet4 [list ""]
            }
       }
    }
    return $statList
}

set startTime [clock micro]
set testFilesStatistic [runTestScript $book $worksheet1 $worksheet2 $worksheet3 $worksheet4]
set numbOfTestFiles [llength $testFilesStatistic]
set numbOfAssertions 0
foreach a $testFilesStatistic {
    puts $a
    set numbAssFromFile [lindex $a 0]
    set numbOfAssertions [expr {$numbOfAssertions + $numbAssFromFile}]
}
# get the excel as xml
set xml [excel::asXml $book]
set fd [open "$fileName" "w"]
puts "Generating the $fileName file."
puts $fd $xml
close $fd
set endTime [clock micro]
set AllTime [makePrettyTimeFormat [expr {$endTime - $startTime}]]
puts "**********************************************************"
puts "**********************************************************"
puts "There were $numbOfTestFiles files tested with overall $numbOfAssertions Assertions:\n"
foreach el $testFilesStatistic {
     set el_1 [file tail [lindex $el 1]]
     set el_2 [lindex $el 0]
     puts "$el_1: $el_2 Assertions."
}
puts "__________________________________________________________"
puts "Repeats pro sample: $numb."
puts "The overall time for executing this script was: $AllTime."
