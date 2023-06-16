# benchmarks.tcl

namespace eval ::bench:: {
    namespace export procBenchmark
    namespace export procSymBenchmark
    namespace export procLawCheckTest
    namespace export procCSPMBenchmark
    namespace export procRegressionTest
}

#puts "Generating Benchmarks"

global bench_MC_maxnodes
set bench_MC_maxnodes 100

proc ::bench::procSingleBenchmark {traceCheck modelCheck findInvViol} {
   procSingleBenchmarkFullOpts $traceCheck $modelCheck $findInvViol 0
}

proc ::bench::procSingleBenchmarkFullOpts {traceCheck modelCheck findInvViol findAssViol} {
   global strFilename
   if [prolog spec_file_has_been_successfully_loaded] {
    puts "Run Tcl/Tk Benchmark $strFilename ($traceCheck $modelCheck $findInvViol $findAssViol)"
    procSingleBenchmarkWoErrCheck $traceCheck $modelCheck $findInvViol $findAssViol
    procInsertHistoryOptionsState
    procShowErrors
  } else {
    puts "### No specification file has been successfully loaded (for file: $strFilename)"
    puts "### Skipping Test"
  }
}

proc ::bench::procSingleBenchmarkWoErrCheck {traceCheck modelCheck findInvViol findAssViol} {
    global strFilename
    global logFile
    global bench_MC_maxnodes
    putlog ""
    putlog "% Benchmarking: $strFilename"
    procAddTestingEntry "\nTreating $strFilename"

	global mcFindDeadlocks mcFindInvViolations mcSearchForNewErrors mcFindStateErrors
	if {$modelCheck} {
     puts "% Model checking benchmark system"
     set por off
     set pge off
     set mctime [time {prolog tcltk_model_check($bench_MC_maxnodes,ErrRes,0,$findInvViol,0,$findAssViol,$mcFindStateErrors,0,$por,$pge,360000,PrologTime)} 1]
     putlog "% Model Checking Time: $mctime"
     set mctimems [lindex [split $mctime] 0]
     set CURDATEH [clock format [clock seconds] -format "%Y%m%d%H"]
     putlog "mc('$strFilename',$CURDATEH,$mctimems)."
     puts "MC Time: $mctimems microsecs, Prolog: $prolog_variables(PrologTime) millisecs"
     procAddTestingEntry "Model checking time: $mctimems microsecs, Prolog: $prolog_variables(PrologTime) millisecs"
     #set Result $prolog_variables(ErrRes)
     if {$findInvViol && [prolog tcltk_goto_an_invariant_violation]} {
          procInsertHistoryOptionsState
          tkErrorBox "Invariant violation found in $strFilename!"
     }
     puts "Computing coverage"
     prologmnf tcltk_compute_coverage(Res)
     # obtain a list of conjuncts with their status
	 set Result $prolog_variables(Res)
	 putlog "/* $Result */"
	}
	if {$traceCheck} {
     puts "% Trace checking benchmark system"
     putlog "% Trace Checking Time: [time {procCheckTraceFile2 0 1} 1]"
     procAddTestingEntry "Trace Checking finished"
	}
}

proc ::bench::procZTest {} {
    prologmnf proz:startzconversiontests
    procShowErrors
}



proc ::bench::procSymBenchmark {} {
   global strFilename
   if [procSpecFileSuccessfullyLoaded] {
     set UP [Dialog_Prompt "Enter Max Size of deferred sets:"]
     if {$UP != ""} {
       procSymBenchmark2 1 $UP
     }
   } else {
       tkErrorBox "Open a B Machine first."
   }
}

proc ::bench::procSymBenchmark2 {lowbound upbound} {
    global strFilename forceRecompile
    global logFile bench_MC_maxnodes
    set cpy $bench_MC_maxnodes
    set bench_MC_maxnodes 100000
    global version
    set backforceRecompile $forceRecompile
    set forceRecompile 0

    procStartTesting sym_bench

    for {set i $lowbound} {$i <= $upbound} {incr i} {
       putlog "Size of unspecified deferred sets: $i"
       puts "Size of unspecified deferred sets: $i"
       prologmnf preferences:set_preference(globalsets_fdrange,$i)
       procGenericLoadFile
       procSingleBenchmark 0 1 1
       prologmnf tcltk_compute_coverage(Res)
       puts "$prolog_variables(Res)"
    }
    procEndTesting

    set forceRecompile $backforceRecompile
    set bench_MC_maxnodes cpy
}



proc ::bench::procStartTesting {type} {
    global testing_mode logFile version testing_startTime
    set ProBDir [procGetEnviron2 'PROB_SOURCE_DIR' "~/svn_root/NewProB/src/"]

    prologmnf preferences:backup_preferences
    prologmnf preferences:reset_for_benchmarking
    prologmnf preferences:deactivate_recent_documents
	set failed_tests ""
	set detailed_errors ""
    set testing_mode true
    if {1 == 1} {
       set logFile ""
    } else {
       set CURDATE [clock format [clock seconds] -format "%Y%m%d_%H"]
       puts "opening logfile $ProBDir/benchmark_history/$type$CURDATE.pl"
       set logFile [open "$ProBDir/benchmark_history/$type$CURDATE.pl" w 0600]
       fconfigure $logFile -encoding utf-8
    }
    putlog "% ProB $version"
    set testing_startTime [clock clicks -milliseconds]
	global batch_mode
	if {!$batch_mode} {
		destroy .testinfo
		set f .testinfo
		CreateTextDialog $f "Regression Testing" "Regression Testing Progress" 0 0
		$f.frmSource.text configure -state normal
		$f.frmSource.text delete 0.0 end
		$f.frmSource.text configure -state disabled
	    Dialog_WaitVisible $f $f.frmSource
	}
}

proc ::bench::procGetEnviron {ENV} {
   procGetEnviron2 $ENV ""
}
proc ::bench::procGetEnviron2 {ENV DEFAULT} {
   # we could use global env instead
    if [prolog system:environ($ENV,Dir)] {
      return $prolog_variables(Dir)
    } else {
      tkErrorBox "Environment variable $ENV not set.\nUsing '$DEFAULT'."
      return "$DEFAULT"
    }
}

proc ::bench::procAddTestingEntry {Msg} {
	global batch_mode
	if {!$batch_mode} {
		set f .testinfo
		$f.frmSource.text configure -state normal
		$f.frmSource.text insert end "$Msg\n"
		$f.frmSource.text see end
		$f.frmSource.text configure -state disabled
	}
}
proc ::bench::procEndTesting {} {
    global testing_mode logFile version failed_tests detailed_errors testing_endTime
    set testing_endTime [clock clicks -milliseconds]
    if {$logFile != ""} {
       close $logFile
       set logFile ""
    }
    set testing_mode false
    procShowErrors
    procShowFailedTests
    prologmnf preferences:revert_preferences
    prologmnf preferences:activate_recent_documents
}


proc ::bench::procShowFailedTests {} {
   global testing_mode strFilename
   global failed_tests detailed_errors testing_endTime testing_startTime
   global batch_mode
   set testing_Time [expr $testing_endTime - $testing_startTime]
 destroy .testinfo
   if {$failed_tests==""} {
	    tkMessageBox "All tests were successful.\nDuration: $testing_Time milliseconds"
   } else {
       append failed_tests ";; Duration: $testing_Time milliseconds;; DETAILS:; $detailed_errors"
       #puts "Details: $detailed_errors"
	   set Result [split [string trimright $failed_tests {;}] \;]
	   if {$batch_mode} {
	       puts "Failed Tests:\n$Result\n"
	       prolog assert_cli_error_occurred(failed_tests)
	   } else {
	       procShowList $Result "Failed Tests" "The following tests failed:"
	   }
	   set failed_tests ""
	   set detailed_errors ""
   }
}

proc ::bench::procLawCheckTest {full} {
    global strFilename version
    global bench_MC_maxnodes
    set NewProBExDir [procGetEnviron2 'PROB_EX_DIR' "~/git_root/prob_examples/public_examples/"]
    set bench_MC_maxnodes_bak $bench_MC_maxnodes
    if {$full==true} {
       set bench_MC_maxnodes 10000
    } else {
       set bench_MC_maxnodes 1000
    }

    procStartTesting lawcheck

    set strFilename "$NewProBExDir/B/Benchmarks/BoolLaws.mch"
    procGenericLoadFile
    procSingleBenchmarkFullOpts 0 1 1 1
    set strFilename "$NewProBExDir/B/Laws/BoolWithArithLaws.mch"
    procGenericLoadFile
    procSingleBenchmarkFullOpts 0 1 1 1
    set strFilename "$NewProBExDir/B/Laws/BoolLaws_SetCompr.mch"
    procGenericLoadFile
    procSingleBenchmarkFullOpts 0 1 1 1
    set strFilename "$NewProBExDir/B/Laws/BoolLaws_SetComprCLPFD.mch"
    procGenericLoadFile
    procSingleBenchmarkFullOpts 0 1 1 1

    set strFilename "$NewProBExDir/B/Laws/ExplicitComputations.mch"
    procGenericLoadFile
    procSingleBenchmarkFullOpts 1 1 1 1
    #procCheckDebugProperties

    set strFilename "$NewProBExDir/B/Laws/SubstitutionLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Laws/StringLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Laws/INTEGERSET_Laws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Laws/NatRangeLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Laws/RelLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Laws/ArithmeticLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/SetLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/SetSetLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/SetRelLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/SeqLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Laws/FunLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/SetLawsNat.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Laws/SetLawsNatural.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/FunLawsWithLambda.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/EventBPrologPackages/Laws/SetLaws.eventb"
    procLoadEventBPackage
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/EventBPrologPackages/Laws/LawsEventBSpecific.eventb"
    procLoadEventBPackage
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/NewSyntax/RecSetLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    procEndTesting

    set bench_MC_maxnodes $bench_MC_maxnodes_bak
}


proc ::bench::procRegressionTest {} {
    global strFilename
    procStartTesting regression

    global bench_MC_maxnodes
    set bench_MC_maxnodes_bak $bench_MC_maxnodes
    set bench_MC_maxnodes 100

    set NewProBExDir [procGetEnviron2 'PROB_EX_DIR' "~/git_root/prob_examples/public_examples/"]
    set NewProBPrivateExDir [procGetEnviron2 'PROB_PEX_DIR' "~/git_root/prob_examples/examples/"]


    set strFilename "$NewProBExDir/B/Benchmarks/Chapter_10/Safes.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    # set strFilename "$NewProBExDir/B/ErrorMachines/CstDefError1.mch"
    # procGenericLoadFile
    # procExpectErrors 2

    set strFilename "$NewProBExDir/B/FeatureChecks/INCLUDES_USES_SEES/M2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    # procCheckInvStatus 1 # "Trace has been changed"

    set strFilename "$NewProBExDir/B/FeatureChecks/StrangeNames/initialise_machine.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/FeatureChecks/DEFINITIONS/DEFAA12.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/FeatureChecks/DEFINITIONS/DefDouble.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/FeatureChecks/DEFINITIONS/TestMacroPrio.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/FeatureChecks/SetOperators/TestInterNatural.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/FeatureChecks/SetOperators/TestDifficultSetDifference.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/FeatureChecks/SetOperators/TestPartition.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/FeatureChecks/SetOperators/TestPartition2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/FeatureChecks/SetOperators/TestPartition3.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Tester/PartialEnumeratedSet.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/PartialEnumeratedSet2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/SELECT_CASE.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/SELECT_ELSE_CHECK.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/TestAssoc.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/SetComprehension.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/Tester/NastySetComprehensions.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/Ticktest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/TwoDimensionalArray.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/ThreeDimensionalArray.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/ForAllSet.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/SquareRoot.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/SquareRootBigger.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tester/SquareRootFunction.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1


    set strFilename "$NewProBExDir/B/Tickets/227/file_system.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Tickets/Fraikin1/BecomeSuchThat_tmp.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/Tickets/Boland1/Microwave.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    # set strFilename "$NewProBExDir/EventBTrans/SteveWright_VM/RegMach4.mch"
    # procGenericLoadFile
    # procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/TestSubset.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/TestSubset2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/TestStrictSubset.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/TestDisjoint.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/TestDisjunction.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/TestImplicationArith.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/TestEquivArith.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/LargeAndSmallSets.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/ForallConstraints.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/ImageTestSimple.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/RangeRelTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/DomRelTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/ConstraintPropagation/TimeModelling.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/DisEqualityCheck.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/DisEqualityLarge.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/SetLogInefficiencies.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/EnumerateFunctionTests.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/EnumerateFunctionTests2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/EnumerateFunctionTests3.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/EnumerateFunctionTests4.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/EnumerateFunctionTests5.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/EnumerateFunctionTests6.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/TestFD_Domain_Narrowing.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/Symbolic/TestIDPropagation.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0


    set strFilename "$NewProBExDir/B/PerformanceTests/SAT_Tests/SimpleArith.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/PerformanceTests/SAT_Tests/ArithDisjunctionComplicated1.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/PerformanceTests/SAT_Tests/ArithDisjunctionComplicated2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/PerformanceTests/SATLIB/uf20-903.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/SATLIB/uf20-903_MYBOOL.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/DeferredSetsLarge.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/PerformanceTests/OverridePerfTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/PerformanceTests/RestrictionTests.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/LargeSequences.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/SubsetChecks.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/PerformanceTests/LargeSets/PartialFunCheck.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/PerformanceTests/LargeSets/SeqCheck.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/PerformanceTests/CardTotFunExpansionTests.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/PerformanceTests/CardTotFunExpansionTests2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/PerformanceTests/ComplementMaximalSet.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 6 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/DomRangeClosures.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Demo/countdown.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Demo/BirthdayPuzzle.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Demo/TrainTorchPuzzle.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Benchmarks/SimpleConstants.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/EventB/SimpleRodinTypeCheckTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/EventB/SimpleSETasQVAR.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Benchmarks/BoolLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1
    set strFilename "$NewProBExDir/B/Laws/BoolWithArithLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1
    set strFilename "$NewProBExDir/B/Laws/BoolLaws_SetCompr.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1
    set strFilename "$NewProBExDir/B/Laws/BoolLaws_SetComprCLPFD.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Laws/StringLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/SetLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Laws/RelLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/SeqLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/SeqTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Benchmarks/SeqTest2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Laws/FunLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/SetLawsNat.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Laws/SetLawsNatural.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/FunLawsWithLambda.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Laws/INTEGERSET_Laws.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/spec.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    puts "SKIPPING TEST: Functions.mch - TO DO: enable again"
    # set strFilename "$NewProBExDir/B/Benchmarks/Functions.mch"
    # procGenericLoadFile
    # procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/CartesianNat.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/Tester/SimpleSetComprTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0


    set strFilename "$NewProBPrivateExDir/B/Other/Stephane/ideal_behavr_conf_mdl.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1


    set strFilename "$NewProBPrivateExDir/B/Other/Passos/Clean.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Other/Valerio/SigmaProblem.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1


    set strFilename "$NewProBExDir/B/NewSyntax/FunMultipleArgs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/ExistentialTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/Pairs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter12/Team.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/PrjTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/AllInjFunsTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Tester/TotalFunTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/TotalFunTest2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/TotalFunTest3.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/TotalFunTest4.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/PartialFunTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/PartialFunTest2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/PartialFunTest3.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/PartialFunTest4.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/EnumerationChooseTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/ParallelProduct.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/LargeSets.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/LargeRelationSetTests.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/EnumerationSpeed.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/EnumerationSpeed2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/EnumerationSpeed3.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/PerfManyVarsCsts200.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/TotalFunEqualityTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/TotalBijInverse.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/PartialFunInverse.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/TwoBinFunctions.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/PerformanceTests/TwoBinFunctionsSET.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/Alternate.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/PerformanceTests/PartitionIntoComponentsTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/TestLastSeqRev.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/PartialFunNATURAL.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/PartialFunCartesianNATURAL.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/PartialFunOverRel.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/IterateTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Demo/SortByPermutation.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Demo/SortByPermutation5.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Demo/SortByPermutationPF.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/ParityFunction.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/SurjEnumCst.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/Sort.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Compilers/LLParsingStagedEfficient.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Compilers/LLParsing.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Puzzles/FibonacciMorphism.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Puzzles/BlocksWorld.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Puzzles/BlocksWorldGeneric.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Puzzles/RushHour.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/STUPS2_KURS/FileSystem.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/FeatureChecks/InclusionChecks/Double3.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    # set strFilename "$NewProBExDir/B/Tester/CaseStmt.mch"
    # procGenericLoadFile
    # procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/SubstSetInTests.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/SymmetryReduction/RelationRelation.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0


    set strFilename "$NewProBExDir/B/FeatureChecks/INCLUDES_USES_SEES/ParameterChecks/M1.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/FeatureChecks/RenameParameterTests/Customer.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Other/Evans/LetWithAndProblem.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Siemens/TestConcreteVariables.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Siemens/TestConcreteConstants.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Siemens/TestString.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Siemens/Test/GroundPropTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/FeatureChecks/RenameParameterTests/TwoCounters.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/FeatureChecks/RenameParameterTests/FourCounters.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/FeatureChecks/RenameParameterTests/ExtCounter.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/FeatureChecks/RenameParameterTests/ExtPromCounter.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/FeatureChecks/RenameParameterTests/TwoExtCounters.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/FeatureChecks/RenameParameterTests/FourExtCounters.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/STUPS2_KURS/SudokuSETS9.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Simple/TwoPurses.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/Simple/dijkstra.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Mathematical/Sieve.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Mathematical/SieveAlternate.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Mathematical/Sieve_WithMin.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Mathematical/GraphColouring.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Mathematical/GraphIso/CheckGraphIsomorphism.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/Mathematical/TLA/TLA_Graph.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/TLA/GraphIso.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/TLA/GraphIsoEnum.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Mathematical/LeaderElectionRing.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/Mathematical/PostCorrespondence.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Puzzles/NQueens.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/Puzzles/NQueens50.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/PerformanceTests/CardPropagation/CardProp.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/FeatureChecks/RenameParameterTests/MyTwoDatas.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Satpathy/SystemOnChip/Router.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Puzzles/MagicSquare3.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Puzzles/MagicSquareSimple.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Puzzles/TwoQueensSevenKnights.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Puzzles/SudokuAsConstant.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Puzzles/SudokuAsConstantVeryHard.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Other/Schneider/AHBDecoderStateV1.1.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Abrial/Earley/earley_4_v2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    # has invariant violation

    set strFilename "$NewProBExDir/B/Special/PO_ModelChecking/earley_3_original.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Abrial/Earley/earley_3_v6.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/NatRangeParsingTest.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBPrivateExDir/B/Rodin/Enbacka/MacTimes.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Other/Srivastava/Simple_Unicast_Broadcast1.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Rodin/USB_ClearSy/USB_1.ref"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Rodin/USB_ClearSy/USB_3.ref"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/ClearSy/spec_Cruise/Cruise1.ref"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/ClearSy/JCRE/opcodes_i.imp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/ClearSy/control/M1_i.imp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/ClearSy/control/M1.ref"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/L4MicroKernel/L4API/API.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/Chapter13/AllocateR.ref"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/Benchmarks/Chapter13/Booksr.ref"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/Benchmarks/Chapter13/Booksrr.ref"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBPrivateExDir/B/Abrial/Queue/m1.ref"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBPrivateExDir/B/Abrial/Press/m13.ref"
    procGenericLoadFile
    procSingleBenchmark 1 0 0


    set strFilename "$NewProBPrivateExDir/B/Stouls/These/Canal_Communication_V3/Canal_Communication_V3.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBPrivateExDir/B/Stouls/These/Canal_Communication_V3/Canal_Communication_V3_R2.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBPrivateExDir/B/Stouls/These/MachineChocolat/cr2.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBPrivateExDir/B/Stouls/These/Parking/parking_r2.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBPrivateExDir/B/Stouls/These/DEMONEY/CycleDeVieAppletRaffineEnDEMONEY/OpenPlatform_WithActions_r1.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBPrivateExDir/B/Stouls/These/DEMONEY/TransEc_EtatCanal_SecuEc/ModeleGS_R1.ref"
    procGenericLoadFile
    procSingleBenchmark 0 1 1


    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter12/PortR.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Implementations/Mul/MulSimpleImp_ProB.imp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Implementations/Mul/MulSimpleImp100_ProB.imp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Implementations/MaxDevelopment/IncludeVector0.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Implementations/MaxDevelopment/IncludeScalar0.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Implementations/MaxDevelopment/MaxImp.imp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Implementations/ConcreteVars/MI.imp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Implementations/Power/AExpBImpl.imp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Implementations/Division/DivisionImplOk.imp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Siemens/AdaptedBModelPropCheck/utlb_srv_constantes_i.imp"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Implementations/MaxDevelopment/MaxImpVector.imp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/ClearSy/turbomeca/pld_i.imp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter12/Towns.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter12/TownsR.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter12/TownsRR.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter12/TownsRRR.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1


    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter16/Fifo.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter16/RobustFifo.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter16/PositionCounter.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter16/Archive.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter17/Sortarray.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter17/Array.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter17/Priorityqueue.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/B/SchneiderBook/Chapter17/Heaparray.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Cansell/FORMATS07_contention_model_ProB/cont3.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Cansell/decode.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Cansell/tarNI3E/m4.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBPrivateExDir/B/Cansell/tarNI3E/m6.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBPrivateExDir/B/Cansell/tarNI3E/m9.ref"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Alstom/DivBy0_bool.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/B/Alstom/emi.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Alstom/exemple7_occuper_zone.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBPrivateExDir/B/Alstom/exemple7.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBPrivateExDir/B/Alstom/exemple12/exemple12.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBPrivateExDir/B/Alstom/DataValidationProject/Tickets/Burdy3/Rule_DB_General_0006.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0


    set strFilename "$NewProBExDir/Z/Daniel/watergate.tex"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/Z/Woodcock/InnerMember.tex"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/Z/Daniel/Praxis/network2.tex"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

   # set strFilename "$NewProBPrivateExDir/B/Siemens/TestDEFINITIONS.mch"
   # procGenericLoadFile
   # procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/NewSyntax/RecTest.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/NewSyntax/RecTest2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/JensBendisposto/Schleuse/Schleusen_Records.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/NewSyntax/SET_Game_Rec.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/NewSyntax/RecSetLaws.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/EventBPrologPackages/Test1.0/Testid_mch.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/EventBPrologPackages/Test1.0/Testprj_mch.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/EventBPrologPackages/Test1.0/TestPartition_mch.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/EventBPrologPackages/AnimB_Tests/test1_mch.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/EventBPrologPackages/MLift_mch.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/EventBPrologPackages/sieve.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/EventBPrologPackages/ProofDirected/model/mc1_mch.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/EventBPrologPackages/ProofDirected/model/mc1_mch_v2.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/EventBPrologPackages/TrafficLight/tlm_0.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/EventBPrologPackages/TrafficLight/tlm_1.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/EventBPrologPackages/TrafficLight/tlm_3.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/EventBPrologPackages/so_cr_end_states_eventb/mchoreography.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/RodinModels/Huffman/HuffmanM_mch.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/RodinModels/SAP/TUCC_Example/v3/m_wodel123_partner_behaviour.eventb"
    procLoadEventBPackage
    procAddDefaultCSPFile
    procSingleBenchmark 1 0 0

    #set strFilename "$NewProBPrivateExDir/RodinModels/Deploy/Bosch/CruiseControl/CrsCtl_m4.eventb"
    #procLoadEventBPackage
    #procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/RodinModels/Deploy/Bosch/CruiseControl/CrsCtl_m2.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/RodinModels/Bosch/CrCtl_0.5.0_v3/Usec3_simplified_mch.eventb"
    procLoadEventBPackage
    procSingleBenchmark 1 0 0


    set strFilename "$NewProBExDir/CSP/mydemos/microwave.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0


    set strFilename "$NewProBExDir/CSPB/Turner/SetTest.mch"
    procGenericLoadFile
    procAddDefaultCSPFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSPB/Dining/Dining.mch"
    procGenericLoadFile
    procAddDefaultCSPFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSPB/trafficLight/myMachine.mch"
    procGenericLoadFile
    procAddDefaultCSPFile
    procSingleBenchmark 1 0 0


   # set strFilename "$NewProBExDir/CSPB/McEwan/BoolTest.csp"
   # procGenericLoadFile
   # procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/CSPB/McEwan/BoolTest.mch"
    procGenericLoadFile
    procAddDefaultCSPFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Demo/Simpson/Simpson_Four_Slot_CSP.mch"
    procGenericLoadFile
    procAddDefaultCSPFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Tickets/315/ANN_Neuron.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBPrivateExDir/RodinModels/PaulSimon/ca_m03_mch.eventb"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    #set strFilename "$NewProBPrivateExDir/RodinModels/PaulSimon/ca_m01_mch.eventb"
    #procGenericLoadFile
    #procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/SAP/ABAP_Constants/zmu.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Siemens/AdaptedBModelPropCheck/acs_as_env_cfg_ipart.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/cbtc_mes_as_env_inv_easitf_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 127 0 0
    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/cbtc_mes_as_env_inv_etors_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 142 0 0
    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/cbtc_mes_as_env_inv_stors_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 148 1 0
    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/par1_mes_as_env_inv_etors_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 125 0 0
    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/par1_pans_as_env_inv_etors_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 188 0 0
    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/par1_pans_as_env_inv_etors_dist_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 184 0 0
    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/par1_pans_as_env_inv_mesitf_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 182 0 0
    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/par1_pans_as_env_inv_pasitf_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 177 1 0
    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/par1_pans_as_env_inv_se_dj_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 176 2 0
    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/par1_pans_as_env_inv_ss_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 192 0 0
    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/par1_pans_as_env_inv_stors_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 192 0 0
    set strFilename "$NewProBPrivateExDir/B/Siemens/Canarsie_PAR1_PAL/par1_pans_as_env_inv_tcs_bs.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    # has issue with EXPAND_FORALL_UPTO; see test 792
    #procAnalysePredCheck assertions 191 8 0


    set strFilename "$NewProBPrivateExDir/B/Siemens/BAC9_ZC/pas_as_env_inv_ld.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    procAnalysePredCheck assertions 27 0 0


    set strFilename "$NewProBExDir/B/PerformanceTests/InfiniteClosures.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/InfiniteClosures2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/InfiniteClosures3.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/SAT_Tests/IntervalSMTProblem.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/SAT_Tests/IntervalSMTProblem2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/B/PerformanceTests/SAT_Tests/ReificationCompTests.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Alstom/axl3/axl3.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Alstom/ixl/lausanne.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Alstom/cbtc/actions_cbtc.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Alstom/signal/tms_signal_0001.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Alstom/ssp/tms_ssp_0004.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBPrivateExDir/B/Alstom/gfpd/tms_signal_0001.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    procZTest

    procEndTesting
    set bench_MC_maxnodes $bench_MC_maxnodes_bak
}


proc ::bench::procCSPMBenchmark {} {
    global strFilename forceRecompile
    global version
    set backforceRecompile $forceRecompile
    set forceRecompile 0
    global bench_MC_maxnodes
    set bench_MC_maxnodes_bak $bench_MC_maxnodes
    set bench_MC_maxnodes 20000

    set NewProBExDir [procGetEnviron2 'PROB_EX_DIR' "~/git_root/prob_examples/public_examples/"]
    set NewProBPrivateExDir [procGetEnviron2 'PROB_PEX_DIR' "~/git_root/prob_examples/examples/"]

    procStartTesting csp_regression

    set strFilename "$NewProBExDir/CSP/mydemos/Edd/UnionBoolTest.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/slicing/UnionCheck.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/slicing/UnionCheck.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/microwave.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/simple/exception_operator.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleGenGen.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleSeqComp.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleInterruptTimeout.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    # procCheckStatespaceWithFDR  fails due to non-omega semantics of FDR
    set strFilename "$NewProBExDir/CSP/mydemos/SimpleInterruptTimeout2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR
    set strFilename "$NewProBExDir/CSP/evans/InterruptExample_NonRec.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/evans/UnderscoreInput.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/simple/check_set_compr_pat.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/Buffer.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/Buffer_hide.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleRenaming.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/LambdaSimple.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/LambdaComplex.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleRenaming2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleIfThenElse.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/ComplicatedSync.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/CompareWithFDR/ComplicatedSync.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/CSP/mydemos/CompareWithFDR/ComplicatedSync2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSP/mydemos/ComplicatedChannelGuards.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleAlphaPar.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleLinkedParallel.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleCompLinkedPar.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleLinkedParallel2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/ComplicatedLinkedParallel.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/curry_test.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/ListPat.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/ContrivedLambda.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/QuestionMarkTuple.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/RecordDotTest.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/RecordDotTest2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/RenameRecordPattern.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR
    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/RenameRecordPattern2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/RecursiveDatatype.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSP/mydemos/ReplicatedInterleave.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/ReplicatedInterleaveSetDef.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSP/mydemos/subtype_ex.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/subtype_nametype_ex.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/nametype_test.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/nametype_test2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/TestDotPat.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/StrangeAgents.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/NastyNonDet.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/evans/NastyLambda.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/CSP/evans/KeyNET_protocol.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0
    set strFilename "$NewProBExDir/CSP/evans/KeyNETv2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSP/evans/LetWithEmptySet.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/LetMultipleEquations.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/CSPOZ/CurriedNATupleFun.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/CSPOZ/CurriedNATupleFunNested.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/CSPOZ/Kleine_probtest.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/other/Kleine_Head.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/other/Kleine/misusing-lists.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSP/other/Kleine/interrupt-bug.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSP/other/Kleine_Head_complex.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/other/Kleine/bag.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/other/Kleine/timeout_test.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/other/Kleine_print.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/other/KleineScheduler.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/other/Kleine/RenamingRecordTest.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/other/Kleine/philosophers.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/other/Kleine/coz-example.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/other/Kleine/stcc-timeout-ext.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/other/ComplicatedProjectionLet.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/other/ComplicatedDotProjectionLet.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR
    set strFilename "$NewProBExDir/CSP/Casper/SimpleRecordReadFromChannel.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR


    set strFilename "$NewProBExDir/CSP/Stolz/mangle.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/other/Ivo/replicates.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR
    set strFilename "$NewProBExDir/CSP/other/Ivo/replicates2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/LetFunctionPassedOut.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/dot_tests/SimpleDotTest.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/GenericBuffer1.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/FibGen.cspm"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/PairSimple.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/PairMedium.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/PatMatchPair.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/Marc/HigherOrder.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/Marc/HanoiSimple.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 0
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/LetTestsChannel.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/LetMultipleFuns.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/LetTests.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/EnumerationTests.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/SetTests.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SetCompTests.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/SetCompAdvanced.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SetCompWithLambda.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SeqCompTests.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SequenceComprTests2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleCompRenaming.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SeqTests.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SeqRangeTests.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/ReplicatedAlphParallel.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/ReplicatedSequential.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/BigUnionInterTests.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleTransparent.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleInterleaveSkipTest.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleInterleaveSkipTest2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleCHAOS.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleSubsets.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimplePatMatch.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SetCompComplicated.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SetCompComplicated2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/BigUnionInterChannelTest.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleReplicated.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleRepAlphParallel.csp"
    procGenericLoadFile
    procSingleBenchmark 0 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/SimpleReplicatedLinkedParallel.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/ReplicatedSharing.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/NameWithApostrophe.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/RepHalfIntegerDivisionTest.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR


    set strFilename "$NewProBExDir/CSP/mydemos/demo/peterson.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/demo/Peterson_v2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/demo/prize.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/demo/buses.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/demo/bankv4.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/demo/basin_olderog_bank.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR
    set strFilename "$NewProBExDir/CSP/mydemos/demo/basin_olderog_bank_nolambda.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR


    set strFilename "$NewProBExDir/CSP/mydemos/demo/JavaCSP/SequentialRouter.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR
    set strFilename "$NewProBExDir/CSP/mydemos/demo/JavaCSP/CorrectRouter.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/demo/hanoi.fix.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/demo/hanoi.fixv2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/CompareWithFDR/ICFEM/scheduler.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/CompareWithFDR/sieve_fdr.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/fontaine/funbench/funBench.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/fontaine/RecDataTypeFunctions.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/demo/roscoe_chapter4_bs.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/demo/roscoe_chapter4.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/mydemos/demo/abp_chapter5_roscoe.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSP/mydemos/demo/roscoe_section2-1.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSP/mydemos/demo/roscoe_section2-2.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/demo/dudcounter_roscoe_chapter8.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/CompareWithFDR/GenPrime.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/CompareWithFDR/ICFEM/scheduler0_1.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/CompareWithFDR/ICFEM/scheduler0_6.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/demo/crossing.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBPrivateExDir/CSP/Winter/miniAlvey.csp"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/CSP/mydemos/demo/mbuff.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    # procCheckStatespaceWithFDR

    procEndTesting

    set forceRecompile $backforceRecompile
    set bench_MC_maxnodes $bench_MC_maxnodes_bak
    puts "******************************"
    puts "Finished CSP-M Regression Test"
    puts "******************************"
}



proc ::bench::procBenchmark {} {
    global strFilename forceRecompile
    global version
    set backforceRecompile $forceRecompile
    set forceRecompile 0

    set NewProBExDir [procGetEnviron2 'PROB_EX_DIR' "~/git_root/prob_examples/public_examples/"]

    procStartTesting b_bench

    set strFilename "$NewProBExDir/B/Benchmarks/CarlaTravelAgencyErr.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Benchmarks/RouteIsSeq.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Benchmarks/Sets2.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Benchmarks/Doors.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Benchmarks/DSP0.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 0

    set strFilename "$NewProBExDir/B/Benchmarks/DSP0_complicated_initialisation.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 0

    set strFilename "$NewProBExDir/B/Benchmarks/DSP0_complicated_property.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 0

    set strFilename "$NewProBExDir/B/Benchmarks/CSM.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/scheduler.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/phonebook7.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/tictac.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 0

    set strFilename "$NewProBExDir/B/Benchmarks/Teletext_bench.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    set strFilename "$NewProBExDir/B/Benchmarks/Cruise_finite1.mch"
    procGenericLoadFile
    procSingleBenchmark 0 1 1

    procEndTesting

    set forceRecompile $backforceRecompile
}
