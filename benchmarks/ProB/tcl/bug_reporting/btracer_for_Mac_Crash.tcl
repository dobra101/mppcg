# (c) 2009-2011 Lehrstuhl fuer Softwaretechnik und Programmiersprachen,
# Heinrich Heine Universitaet Duesseldorf
# This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html)

# A version to exhibit Mac OS Tcl/Tk bugs with Active Tcl 8.6 & 8.5.9.2

# Simply start the file using wish and then choose the File -> Open menu

# ------------------------------------------------------------
# ProB TCL file (from btracer_tcl.pl)
# ------------------------------------------------------------
# --------
# procedure to initialise menu section
# --------
proc prolog {x} {
 puts "prolog $x"
 return 1
}
   global prolog_variables
   set prolog_variables(V1) "V1"
   set prolog_variables(V2) "V2"
   set prolog_variables(V3) "V3"
   set prolog_variables(PV) "PV"
   set prolog_variables(Suffix) "Suffix"
   set prolog_variables(LCD) "LCD"
   set prolog_variables(Dir) "Dir"
   set prolog_variables(SYMMODE) "off"
   global app_dir tcl_dir lib_dir
   set app_dir "."
   set tcl_dir "."
   set lib_dir "."

   global batch_mode
   set batch_mode 0
   if {$tcl_version<8.4} {
	    tkErrorBox "You have Tcl/Tk Version $tcl_version.\nProB may not work correctly.\nPlease install version 8.4 or newer."
   }
   # puts "Tcl Version $tcl_version"
   global version shortversion prologversion
   prolog version:version(V1,V2,V3,Suffix)
   set shortversion "$prolog_variables(V1).$prolog_variables(V2).$prolog_variables(V3)"
   set version "$shortversion-$prolog_variables(Suffix)"
   prolog current_prolog_flag(version,PV)
   set prologversion "$prolog_variables(PV)"
   global revision lastchangeddate
   set revision "R??"
   prolog version:revision(Rev)
   # set revision $prolog_variables(Rev)
   prolog version:lastchangeddate(LCD)
   set lastchangeddate "??"

   global normal_user expert_user
   set normal_user 0
   set expert_user 0
   global show_error_if_no_transition
   set show_error_if_no_transition 1
   global strFilename logFile currentRefSpecFile cspstrFilename curFileDisplayed
   set  strFilename ""
   set  logFile ""
   set curFileDisplayed "NONE"
   global forceRecompile
   set forceRecompile 1
   set  currentRefSpecFile ""
   set  cspstrFilename ""
   global testing_mode failed_tests detailed_errors
   set testing_mode false
   set failed_tests ""
   set detailed_errors ""
   global only_eventb_menues
set only_eventb_menues [list]
proc procGUI_Menu {} {
    set prolog_variables(SYMMODE) "off"
    set prolog_variables(PSV) "1"
    set prolog_variables(PSV2) "1"
    global normal_user
    #set normal_user [prolog preferences:get_preference(user_not_beginner,true)]
    global expert_user
    #set expert_user [prolog preferences:get_preference(user_is_an_expert_with_accessto_source_distribution,true)]
    global plugin_menu_index
    set plugin_menu_index -1
#    frame .frmMenu

    # top level menu bar
    # option add *tearoff 0

    menu .frmMenu -tearoff 0
    # -tearoff 0 was removed for Leopard
    . config -menu .frmMenu
    foreach m {File Edit Animate Verify Analyse Preferences Debug Files Help} {
        set $m [menu .frmMenu.mnu$m -tearoff 0]
        .frmMenu add cascade -label $m -menu .frmMenu.mnu$m
    }



    # -------------------- file menu
#    menu .frmMenu.mnuFile -tearoff 0
    set AccKey [procGetAccKey]

    .frmMenu.mnuFile add command -label "New..." -underline 0 -command procNewFile -accelerator $AccKey+N
    .frmMenu.mnuFile add command -label "Open..." -underline 0 -command procOpenFile -accelerator $AccKey+O
    .frmMenu.mnuFile add command -label "Reopen" -underline 0 -command procReOpenFile -accelerator $AccKey+R -state disabled
    .frmMenu.mnuFile add command -label "Save" -underline 0 -command procSaveFile -accelerator $AccKey+S -state disabled
    .frmMenu.mnuFile add command -label "Save and Reopen" -command procSaveAndReopenFile -state disabled
    .frmMenu.mnuFile add command -label "Save As..." -command procSaveAsFile -state disabled
    .frmMenu.mnuFile add sep
    .frmMenu.mnuFile add command -label "Save Statespace" -command procSaveState -state disabled
    .frmMenu.mnuFile add command -label "Load Saved Statespace" -command procLoadState -state disabled
    .frmMenu.mnuFile add sep
    if {$normal_user} {
		.frmMenu.mnuFile add command -label "Open in External Editor" -command procOpenFileInEditor -state disabled -accelerator $AccKey+E
		.frmMenu.mnuFile add cascade -label "Open Special" \
				-menu .frmMenu.mnuFile.mnuOpenSpecial
		.frmMenu.mnuFile add sep
    }
    .frmMenu.mnuFile add cascade -label "Recent Files" \
				-menu .frmMenu.mnuFile.mnuRecentDocuments
    .frmMenu.mnuFile add sep
    .frmMenu.mnuFile add command -label "Quit" -underline 0 -command procDoQuit -accelerator $AccKey+Q


     .frmMenu.mnuEdit add command -label "Undo" -command procundo -accelerator $AccKey+Z -state disabled
     .frmMenu.mnuEdit add command -label "Redo" -command procredo -state disabled
    .frmMenu.mnuEdit add sep
    .frmMenu.mnuEdit add command -label "Cut" -command {event generate .frmSource.text <<Cut>>} -accelerator $AccKey+X
    .frmMenu.mnuEdit add command -label "Copy" -underline 0 -command {event generate .frmSource.text <<Copy>>} -accelerator $AccKey+C
    .frmMenu.mnuEdit add command -label "Paste" -command {event generate .frmSource.text <<Paste>>} -accelerator $AccKey+V
    .frmMenu.mnuEdit add sep
    .frmMenu.mnuEdit add command -label "Goto Line..." -command {GotoLineSelect}
    .frmMenu.mnuEdit add sep
    .frmMenu.mnuEdit add cascade -label "Unicode Options" \
                 -menu .frmMenu.mnuEdit.mnuUnicode -state disabled

    menu .frmMenu.mnuEdit.mnuUnicode -tearoff 0
          .frmMenu.mnuEdit.mnuUnicode add command -label "To Unicode Symbols" \
                  -command toUnicodeCommand
          .frmMenu.mnuEdit.mnuUnicode add command -label "Remove Unicode Symbols" \
                  -command fromUnicodeCommand

    # -------------------- Open Special menu
    menu .frmMenu.mnuFile.mnuOpenSpecial -tearoff 0
   .frmMenu.mnuFile.mnuOpenSpecial add command -label "Use CSP File to Guide B..." -command procAddCSPFile
   .frmMenu.mnuFile.mnuOpenSpecial add command -label "Use Default CSP File" -underline 7 -command procAddDefaultCSPFile -accelerator $AccKey+A
   #.frmMenu.mnuFile.mnuOpenSpecial add command -label "Add XTL File..." -command procAddXTLFile

    # -------------------- Open Special menu
    menu .frmMenu.mnuFile.mnuRecentDocuments -tearoff 0
    procRebuildRecentDocumentsMenu

    # -------------------- Animate menu
#    menu .frmMenu.mnuAnimate -tearoff 0
    .frmMenu.mnuAnimate add command -label "Reset" -command procReset
    .frmMenu.mnuAnimate add sep
    .frmMenu.mnuAnimate add command -label "Random Animation (10)" -command procRand
    .frmMenu.mnuAnimate add command -label "Random Animation..." -command procRandAny
    .frmMenu.mnuAnimate add sep
    .frmMenu.mnuAnimate add command -label "View Statespace" \
       -underline 11 -command procDisplayVisitedStates -state disabled -accelerator $AccKey+D
     .frmMenu.mnuAnimate add cascade -label "View" \
            -menu .frmMenu.mnuAnimate.mnuDisp
    if {$normal_user} {
    .frmMenu.mnuAnimate add sep
    .frmMenu.mnuAnimate add command -label "Execute an Operation ..." -command procExecuteOperation
    .frmMenu.mnuAnimate add sep
    .frmMenu.mnuAnimate add command -label "Jump to an Open State" -underline 0 \
              -command procJumpToAnOpenNode -accelerator $AccKey+J
     if {$expert_user} {
    .frmMenu.mnuAnimate add command -label "Jump to a not fully explored State" \
              -command procJumpToAMaxReachedNode
     }
    .frmMenu.mnuAnimate add sep
		# .frmMenu.mnuAnimate add command -label "Show Trace to Current State..."\
				-command {procFindTrace "no"}
		.frmMenu.mnuAnimate add command -label "Show Shortest Trace to Current State"\
				-command {procFindTrace}
		#.frmMenu.mnuAnimate add command -label "Execute Trace to Current State..." -command procExecTrace
		# .frmMenu.mnuAnalyse add sep
		.frmMenu.mnuAnimate add command -label "Show Current B State"\
				-command {procShowCurrentState}
     if {$expert_user} {
    .frmMenu.mnuAnimate add command -label "Slowly Execute Trace to Current State" \
               -command {procReExecuteTraceToCurNode}
     }
    }


         #Display sub menu
    menu .frmMenu.mnuAnimate.mnuDisp -tearoff 0
    .frmMenu.mnuAnimate.mnuDisp add command -label "Shortest Trace to Current State" \
       -command {procDisplayTraceToCurrentState "yes"} -state normal
    .frmMenu.mnuAnimate.mnuDisp add command -label "History to Current State" \
       -command {procDisplayTraceToCurrentState "no"} -state normal
    .frmMenu.mnuAnimate.mnuDisp add command -label "Current State" \
       -command {procDisplayCurrentState "no"} -state normal
    if {$normal_user} {
    .frmMenu.mnuAnimate.mnuDisp add command -label "Current State as Graph" \
       -command {procDisplayCurrentState "yes"} -state normal
    .frmMenu.mnuAnimate.mnuDisp add sep
    .frmMenu.mnuAnimate.mnuDisp add command -label "Transition Diagram for Custom Expression..." \
       -command {procDisplayTransitionDiagram} -state normal
    .frmMenu.mnuAnimate.mnuDisp add sep
    .frmMenu.mnuAnimate.mnuDisp add command -label "Signature-Merge Reduced Statespace" \
       -command {procDisplayReducedSigMerge} -state normal -accelerator $AccKey+K
    .frmMenu.mnuAnimate.mnuDisp add command -label "DFA Reduced Statespace" \
       -command {procDisplayReducedDFA} -state normal
    .frmMenu.mnuAnimate.mnuDisp add command -label "Select Operations & Arguments for Reduction" \
        -command procShowArgChooser; ## Argument chooser for DFA
    .frmMenu.mnuAnimate.mnuDisp add sep
    .frmMenu.mnuAnimate.mnuDisp add command -label "Subgraph for Invariant Violation" \
       -command {procSubgraph "invariant"} -state normal
    .frmMenu.mnuAnimate.mnuDisp add command -label "Subgraph of Nodes Satisfying GOAL" \
       -command {procSubgraph "goal"} -state normal
     if {$expert_user} {
		.frmMenu.mnuAnimate.mnuDisp add sep
		.frmMenu.mnuAnimate.mnuDisp add command -label "Current State as Custom Graph" \
		   -command {procDisplayCurrentState "custom"} -state normal
       }
   }

    # -------------------- Verify menu
#    menu .frmMenu.mnuVerify -tearoff 0
    .frmMenu.mnuVerify add command -label "Model Check..." -underline 0 -command procModelCheck -accelerator $AccKey+M
   # .frmMenu.mnuVerify add cascade -label "Search Settings" \
        -menu .frmMenu.mnuVerify.mnuMCOptions
    .frmMenu.mnuVerify add sep
    .frmMenu.mnuVerify add command -label "Find State Satisfying Predicate..." \
        -command procAdvancedFind  -accelerator $AccKey+F
    .frmMenu.mnuVerify add command -label "Find State Enabling an Operation..." \
        -command {procFindEnabledOp}
    .frmMenu.mnuVerify add command -label "Find a Non-Deterministic State" \
        -command procJumpToNonDetNode
    .frmMenu.mnuVerify add command -label "Find a Non-Resetable State" \
        -command procJumpToNonResetableNode
    .frmMenu.mnuVerify add command -label "Check LTL Formula..." \
        -underline 6 -command procLtl -accelerator $AccKey+L
    .frmMenu.mnuVerify add command -label "Check LTL Assertions" \
        -command {CheckLtlAssertions}
    .frmMenu.mnuVerify add command -label "Check CTL Formula..." \
        -command procCtl
		.frmMenu.mnuVerify add sep
	   .frmMenu.mnuVerify add command -label "Trace Refinement Check..." \
		           -command procRefinementCheck
	   .frmMenu.mnuVerify add command -label "Save State for Later Refinement Check" \
		           -command procSaveSpecStateForRefinement
    if {$normal_user} {
		.frmMenu.mnuVerify add sep
        .frmMenu.mnuVerify add cascade -label "Constraint Based Checking" \
            -menu .frmMenu.mnuVerify.mnuCBC
		.frmMenu.mnuVerify add sep
        .frmMenu.mnuVerify add cascade -label "External Tools" \
            -menu .frmMenu.mnuVerify.mnuRef
		.frmMenu.mnuVerify add sep
        .frmMenu.mnuVerify add cascade -label "Trace Checking" \
            -menu .frmMenu.mnuVerify.mnuTrace
		if {$expert_user} {
			.frmMenu.mnuVerify add sep
			.frmMenu.mnuVerify add cascade -label "B-Linda Grid" \
				-menu .frmMenu.mnuVerify.mnuLinda
        }
    }

         #CBC menu
    menu .frmMenu.mnuVerify.mnuCBC -tearoff 0
	.frmMenu.mnuVerify.mnuCBC add command -label "Check Invariant Preservation for Operations" \
		-command procConstraintBasedCheck
	.frmMenu.mnuVerify.mnuCBC add command -label "Check Invariant for Specific Operation..." \
		-command procOpConstraintBasedCheck
    .frmMenu.mnuVerify.mnuCBC add sep
	.frmMenu.mnuVerify.mnuCBC add command -label "Generate a Valid State" \
		-command {prolog tcltk_constraint_find_valid_state; procInsertHistoryOptionsState}
    .frmMenu.mnuVerify.mnuCBC add sep
	.frmMenu.mnuVerify.mnuCBC add command -label "Find Deadlock" \
		-command {procFindDeadlockedState}
	.frmMenu.mnuVerify.mnuCBC add command -label "Find Deadlock Satisfying Predicate..." \
		-command {procFindDeadlockedStateWithPred}

         #Refinement Check menu
    menu .frmMenu.mnuVerify.mnuRef -tearoff 0
	 .frmMenu.mnuVerify.mnuRef add command -label "Typecheck" \
		-command procTypeCheckFile  -accelerator $AccKey+B
		if {$normal_user} {
		 .frmMenu.mnuVerify.mnuRef add command -label "Typecheck Internal Representation" \
			-command procTypeCheck_PPF_BFile
		}
		.frmMenu.mnuVerify.mnuRef add sep
	 .frmMenu.mnuVerify.mnuRef add command -label "Save State to FDR/CSP File" \
		-command procSaveStateToFDR
	 .frmMenu.mnuVerify.mnuRef add command -label "Check Statespace with FDR" \
		-underline 22 -command procCheckStatespaceWithFDR -accelerator $AccKey+U
     .frmMenu.mnuVerify.mnuRef add sep
	 .frmMenu.mnuVerify.mnuRef add command -label "Save State to Spin/Promela File" \
		-command procSaveStateToSPIN
	 .frmMenu.mnuVerify.mnuRef add command -label "Save State to Never Claim" \
		-command procSaveStateToNeverClaim
	 .frmMenu.mnuVerify.mnuRef add command -label "Print Event-B model as classical B" \
		-command procPrintAsEventB -state disabled
     global only_eventb_menues
    lappend only_eventb_menues [list .frmMenu.mnuVerify.mnuRef [.frmMenu.mnuVerify.mnuRef index end]]

	if {$expert_user} {
		.frmMenu.mnuVerify.mnuRef add sep
		.frmMenu.mnuVerify.mnuRef add command -label "Kodkod..." \
			-command procKodkod
	}
         #Trace Check menu
    menu .frmMenu.mnuVerify.mnuTrace -tearoff 0
     .frmMenu.mnuVerify.mnuTrace add command -label "Save History to Trace File" \
        -command procSaveTraceFile
     .frmMenu.mnuVerify.mnuTrace add command -label "Check Trace from Trace File" \
        -underline 6 -command {procCheckTraceFile} -accelerator $AccKey+T
     .frmMenu.mnuVerify.mnuTrace add sep
     .frmMenu.mnuVerify.mnuTrace add command -label "Check Trace from other File..." \
			-command procCheckOtherTraceFile
    if {$expert_user} {
     .frmMenu.mnuVerify.mnuTrace add command -label "Generate FDR Check from History..." \
			-command {prologmnf b_trace_checking:print_trace_as_fdr_check}
     }
         #Linda Grid menu
    menu .frmMenu.mnuVerify.mnuLinda -tearoff 0
     .frmMenu.mnuVerify.mnuLinda add command -label "Use a B-Linda Server for Model Checking" \
        -command procStartLindaServer
     .frmMenu.mnuVerify.mnuLinda add command -label "Stop Server" \
        -command procStopLindaServer
      .frmMenu.mnuVerify.mnuLinda add sep
     .frmMenu.mnuVerify.mnuLinda add command -label "Serve B-Linda Server on this Machine" \
        -command procServeLindaServer
     .frmMenu.mnuVerify.mnuLinda add command -label "Serve another B-Linda Server..." \
        -command procServeSpecificLindaServer



    # -------------------- Analyse menu
#    menu .frmMenu.mnuAnalyse -tearoff 0
    .frmMenu.mnuAnalyse add command -label "Compute Coverage" -underline 14 -command {procComputeCoverage 0} -accelerator $AccKey+G
    .frmMenu.mnuAnalyse add command -label "Show Typing" -command procShowTyping
     .frmMenu.mnuAnalyse add sep
     .frmMenu.mnuAnalyse add cascade -label "Analyse Predicate" \
            -menu .frmMenu.mnuAnalyse.mnuDebugPred
     .frmMenu.mnuAnalyse add cascade -label "Analyse Graphically" \
            -menu .frmMenu.mnuAnalyse.mnuVisualise
     .frmMenu.mnuAnalyse add sep
     .frmMenu.mnuAnalyse add command -label "View Machine Hierarchy" \
        -underline 13 -command procViewModuleHierarchy -accelerator $AccKey+H
    if {$normal_user} {
        .frmMenu.mnuAnalyse add sep
        .frmMenu.mnuAnalyse add cascade -label "Activate Plugin" \
            -menu .frmMenu.mnuAnalyse.mnuPlugins -state disabled
        set plugin_menu_index [.frmMenu.mnuAnalyse index end]
		.frmMenu.mnuAnalyse add sep
        .frmMenu.mnuAnalyse add cascade -label "ProTest" \
            -menu .frmMenu.mnuAnalyse.mnuTest
		  if {$expert_user} {
			 .frmMenu.mnuAnalyse add sep
             .frmMenu.mnuAnalyse add command -label "Compute Coverage and Enabling" -underline 14 -command {procComputeCoverage 1}
			 .frmMenu.mnuAnalyse add command -label "Interactive Debug Invariant..." -command {prologmnf interactive_debug_invariant}
			 .frmMenu.mnuAnalyse add command -label "Analyze Hash Collisions" -command {prologmnf analyze_hash_collisions}
			 .frmMenu.mnuAnalyse add sep
			 # .frmMenu.mnuAnalyse add command -label "Slice (experimental)" -command procSliceProgramPoint
                         .frmMenu.mnuAnalyse add cascade -label "Slicing" \
                              -menu .frmMenu.mnuAnalyse.mnuSlicer
#			 .frmMenu.mnuAnalyse add command -label "AbsInt (experimental)" -command procAbsInt
			 .frmMenu.mnuAnalyse add command -label "Show Enable Graph (experimental)" -command procFlowEnableGraph
			 .frmMenu.mnuAnalyse add command -label "Show Enable Graphs (experimental)" -command procFlowEnableGraphs
			 .frmMenu.mnuAnalyse add command -label "Show Base Guide Automaton (experimental)" -command procBaseGuide
			 .frmMenu.mnuAnalyse add command -label "Show Event Guide Automaton (experimental)" -command procEventGuide
             .frmMenu.mnuAnalyse add command -label "Show Predicate Analysis of Properties" -command procShowPropertiesAnalysis
		   }
    }

    # -------------------- Analyse menu
    menu .frmMenu.mnuAnalyse.mnuDebugPred -tearoff 0
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Invariant" -underline 0 -command {procAnalyseInvariant} -accelerator $AccKey+I
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Properties" -command {procAnalysePred properties}
    if {$expert_user} {
        .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Debug Properties" -command {procDebugProperties}
    }
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Assertions" -command {procAnalyseAssertions} -accelerator $AccKey+\]
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Debug Operation PRE..." -command procDebugOperation
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Custom Predicate..." -command procAnalyseCustomPredicate
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "GOAL" -command procAnalyseDEFGOAL  -accelerator $AccKey+\[
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Deadlock PO" -command {procAnalysePred deadlock}



    # -------------------- Visualise menu
    menu .frmMenu.mnuAnalyse.mnuVisualise -tearoff 0
	 .frmMenu.mnuAnalyse.mnuVisualise add command -label "Invariant" -command {procVisualiseInvariant}
	 .frmMenu.mnuAnalyse.mnuVisualise add command -label "Properties" -command {procVisualiseProperties}
	 .frmMenu.mnuAnalyse.mnuVisualise add command -label "Assertions" -command {procVisualiseAssertions}
	 .frmMenu.mnuAnalyse.mnuVisualise add command -label "Operation PRE..." -command procVisualiseOperationPre
	 .frmMenu.mnuAnalyse.mnuVisualise add command -label "Custom Predicate..." -command procVisualiseCustomPredicate
	  if {$normal_user} {
	     .frmMenu.mnuAnalyse.mnuVisualise add command -label "GOAL" -command {provVisualizeDEFGOAL}
	 .frmMenu.mnuAnalyse.mnuVisualise add command -label "Deadlock PO" -command {procVisualiseInvariantOrOpPRE "_deadlockpo_"}
	  }
    # -------------------- Plugins menu
    menu .frmMenu.mnuAnalyse.mnuPlugins -tearoff 0

    # -------------------- ProTest menu
    menu .frmMenu.mnuAnalyse.mnuTest -tearoff 0
	   if {$normal_user} {
		.frmMenu.mnuAnalyse.mnuTest add command -label "View Testcase Determinized Graph" \
		   -command {procTestCaseGraph} -state normal
		.frmMenu.mnuAnalyse.mnuTest add command -label "Compute Testcases" \
		   -command {prologmnf compute_testcases} -state normal
	        .frmMenu.mnuAnalyse.mnuTest add sep
	        .frmMenu.mnuAnalyse.mnuTest add command -label "SAP global test case generation..." \
		   -command procSAPTestcases
	        .frmMenu.mnuAnalyse.mnuTest add command -label "SAP local test case generation..." \
		   -command procSAPLocal
	   }

    # -------------------- Flow menu
#  menu .frmMenu.mnuAnalyse.mnuFlow -tearoff 0
#		.frmMenu.mnuAnalyse.mnuFlow add command -label "Calculate Enable Graph" \
#		   -command {procFlowEnableGraph} -state normal


    # -------------------- Slice menu
    menu .frmMenu.mnuAnalyse.mnuSlicer -tearoff 0
		.frmMenu.mnuAnalyse.mnuSlicer add command -label "Highlight Slice"\
                   -command procSliceProgramPoint
		.frmMenu.mnuAnalyse.mnuSlicer add command -label "Generate Executable Slice MEB" \
                   -command procSliceExecutableMEB
		.frmMenu.mnuAnalyse.mnuSlicer add command -label "Generate Executable Slice CEB" \
                   -command procSliceExecutableCEB
		.frmMenu.mnuAnalyse.mnuSlicer add sep
		.frmMenu.mnuAnalyse.mnuSlicer add command -label "View MEB Graph" \
                   -command {procVisualizeSliceGraph "CSCFG_slice_MEB"}
		.frmMenu.mnuAnalyse.mnuSlicer add command -label "View CEB Graph" \
                   -command {procVisualizeSliceGraph "CSCFG_slice_CEB"}
		.frmMenu.mnuAnalyse.mnuSlicer add command -label "View Compacted Graph" \
                   -command {procVisualizeSliceGraph "CSCFG_compact"}


    # -------------------- Debug menu
#    menu .frmMenu.mnuDebug -tearoff 0
    global debugMode
    .frmMenu.mnuDebug add command -label "Report a Bug..." -command "procReportBug"
    .frmMenu.mnuDebug add sep
    .frmMenu.mnuDebug add command -label "Perform Self Check" \
        -command {procPerformSelfCheck}
    if {$expert_user} {
		.frmMenu.mnuDebug add command -label "Perform Verbose Self Check" \
			-command {procPerformVerboseSelfCheck}
		.frmMenu.mnuDebug add command -label "Perform Self Check for Module..." \
			-command {procPerformSelfCheckForModule}
        .frmMenu.mnuDebug add command -label "Show Internal Representation" -command procShowInternalRep
		.frmMenu.mnuDebug add sep
		.frmMenu.mnuDebug add checkbutton -label "Debugging Info" \
			 -variable debugMode -offvalue 0 -onvalue 1 -command procUpdateDebuggingMode
		global typeCheckMode
		.frmMenu.mnuDebug add checkbutton -label "Runtime Type Checking" \
			 -variable typeCheckMode -offvalue 0 -onvalue 1 -command procUpdateDebuggingMode
		if {$expert_user} {
		    global traceUponError
		    .frmMenu.mnuDebug add checkbutton -label "Trace Upon Error" \
			 -variable traceUponError -offvalue 0 -onvalue 1 -command procUpdateDebuggingMode
			 .frmMenu.mnuDebug add sep
			 .frmMenu.mnuDebug add command -label "Perform Benchmark Test" \
				-command {procBenchmark}
			 .frmMenu.mnuDebug add command -label "Perform Regression Test" \
				-command {procRegressionTest}
			 .frmMenu.mnuDebug add command -label "Perform Quick Mathematical Laws Check" \
				-command {procLawCheckTest "false"}
			 .frmMenu.mnuDebug add command -label "Perform Full Mathematical Laws Check" \
				-command {procLawCheckTest "true"}
			 .frmMenu.mnuDebug add command -label "Perform CSP-M Benchmark Test" \
				-command {procCSPMBenchmark}
			 .frmMenu.mnuDebug add command -label "Perform Symmetry Benchmark" \
				-command {procSymBenchmark}
			 .frmMenu.mnuDebug add sep
			 .frmMenu.mnuDebug add command -label "Show specialized invariants..." \
				-command {procShowSpecializedInvariants}
		 }
		 .frmMenu.mnuDebug add sep
		 .frmMenu.mnuDebug add command -label "Print Prolog statistics" \
			-command {prolog (nl,statistics,nl)}
		 .frmMenu.mnuDebug add command -label "Reset Statespace with statistics" \
			-command {prolog state_space_initialise_with_stats; procInsertHistoryOptionsState}
     }


    # -------------------- Preferences menu
#    menu .frmMenu.mnuPreferences -tearoff 0
    .frmMenu.mnuPreferences add cascade -label "Font" \
            -menu .frmMenu.mnuPreferences.mnuFont
    .frmMenu.mnuPreferences add cascade -label "Symmetry" \
            -menu .frmMenu.mnuPreferences.mnuSym
    .frmMenu.mnuPreferences add sep
    .frmMenu.mnuPreferences add command -label "Animation Preferences..." \
          -command {procSetPreferences "animation"}
    .frmMenu.mnuPreferences add cascade -label "Graphical Viewer" \
            -menu .frmMenu.mnuPreferences.mnuGraphViewer
    .frmMenu.mnuPreferences add command -label "Graphical Viewer Preferences..." \
          -command {procSetPreferences "dot"}
    .frmMenu.mnuPreferences add command -label "Syntax Highlighting Preferences..." \
          -command {procSyntaxColouringPreferences}
    .frmMenu.mnuPreferences add command -label "CSP Preferences..." \
          -command {procSetPreferences "csp_prefs"}
    if {$normal_user} {
    .frmMenu.mnuPreferences add command -label "Advanced Preferences..." \
          -command {procSetPreferences "advanced"}
    }
    .frmMenu.mnuPreferences add sep
    .frmMenu.mnuPreferences add cascade -label "User Mode" \
            -menu .frmMenu.mnuPreferences.mnuMode
    .frmMenu.mnuPreferences add cascade -label "Configurations" \
            -menu .frmMenu.mnuPreferences.mnuConfigs
	if {$expert_user} {
    .frmMenu.mnuPreferences add sep
    .frmMenu.mnuPreferences add command -label "Absint Preferences..." \
          -command {procSetPreferences "abstract_interpreter"}
    .frmMenu.mnuPreferences add command -label "Hidden Preferences..." \
          -command {procSetPreferences "hidden"}
    .frmMenu.mnuPreferences add command -label "Print Non-Default Preferences" \
          -command {prolog preferences:print_non_default_preferences}
    }

         #font menu
    menu .frmMenu.mnuPreferences.mnuFont -tearoff 0
    global chosefontval
    foreach font {"Courier 9" "Courier 10" "Courier 12" "Courier 14" "Courier 16" "Courier 18" "Courier 20" "Courier 24"} {
	.frmMenu.mnuPreferences.mnuFont add radiobutton -label "$font"  -variable chosefontval -value $font \
	       -command "changeFont {$font}"
    }
         #font menu
    menu .frmMenu.mnuPreferences.mnuSym -tearoff 0
    global chosensymmode
    prolog preferences:get_preference(symmetry_mode,SYMMODE)
    set chosensymmode $prolog_variables(SYMMODE)
    #puts "SymmMode $chosensymmode"
    foreach mode {"off" "flood" "nauty" "hash"} {
	.frmMenu.mnuPreferences.mnuSym add radiobutton -label "$mode"  -variable chosensymmode -value $mode \
	       -command "prolog preferences:set_preference(symmetry_mode,$mode); procReOpenFile"
    }
    .frmMenu.mnuPreferences.mnuSym add sep
    .frmMenu.mnuPreferences.mnuSym add command -label "(will reload machine)" -state disabled
     # User mode menu

     menu .frmMenu.mnuPreferences.mnuGraphViewer -tearoff 0
    global chosen_dot_mode
    prologmnf preferences:get_preference(dot_use_ps_viewer,PSV)
    prologmnf preferences:get_preference(dot_use_alterate_dot_viewer,PSV2)
    if {$prolog_variables(PSV)==true} {
       set chosen_dot_mode 1
    } elseif {$prolog_variables(PSV2)==false} {
       set chosen_dot_mode 2
    } else {
       set chosen_dot_mode 3
    }
	.frmMenu.mnuPreferences.mnuGraphViewer add radiobutton -label "PostScript"  -variable chosen_dot_mode -value 1 \
	       -command "prologmnf preferences:set_preference(dot_use_ps_viewer,true)"
	.frmMenu.mnuPreferences.mnuGraphViewer add radiobutton -label "Dot Viewer"  -variable chosen_dot_mode -value 2 \
	       -command "prologmnf preferences:set_preference(dot_use_ps_viewer,false); prologmnf preferences:set_preference(dot_use_alterate_dot_viewer,false)"
	.frmMenu.mnuPreferences.mnuGraphViewer add radiobutton -label "Alternate Dot Viewer"  -variable chosen_dot_mode -value 3 \
	       -command "prologmnf preferences:set_preference(dot_use_ps_viewer,false); prologmnf preferences:set_preference(dot_use_alterate_dot_viewer,true)"

    menu .frmMenu.mnuPreferences.mnuConfigs -tearoff 0
	.frmMenu.mnuPreferences.mnuConfigs add command -label "MININT/MAXINT (32bit)" \
	       -command "prologmnf preferences:set_preference(maxint,2147483647); prologmnf preferences:set_preference(minint,-2147483648)"

    menu .frmMenu.mnuPreferences.mnuMode -tearoff 0
	.frmMenu.mnuPreferences.mnuMode add radiobutton -label "Beginner"  -variable normal_user -value 0 \
	       -command "procUpdateMenu"
	.frmMenu.mnuPreferences.mnuMode add radiobutton -label "Normal"  -variable normal_user -value 1 \
	       -command "procUpdateMenu"


    # -------------------- Help menu
#    menu .frmMenu.mnuHelp -tearoff 0
    .frmMenu.mnuHelp add command -label "About ProB" -command "procAboutProB"
    .frmMenu.mnuHelp add command -label "Go to ProB website" -command {proc_open_url "http://www.stups.uni-duesseldorf.de/ProB/"}
    .frmMenu.mnuHelp add command -label "Check for Updates" -command "procCheckForUpdates"
    .frmMenu.mnuHelp add sep
    .frmMenu.mnuHelp add command -label "Summary of B Syntax" -command "procBInfo"
    .frmMenu.mnuHelp add command -label "Summary of CSP Syntax" -command "procCSPInfo"
    .frmMenu.mnuHelp add command -label "Summary of Supported Z" -command "procZInfo"
    .frmMenu.mnuHelp add sep
    .frmMenu.mnuHelp add command -label "Verification Info" -command "procCheckingInfo"
    .frmMenu.mnuHelp add sep
    .frmMenu.mnuHelp add command -label "Install AtelierB 4 Plugin..." -command "installAtelierBPlugin"


    .frmMenu.mnuFiles add command -label "No File Opened" -state disabled


    # ----- fix the menu bar
#    pack append . .frmMenu {top fillx}

}

# raise an error message only if we are in batch mode
proc assertBatchError {ERRMSG} {
      global batch_mode
 	  if {$batch_mode} {
	       puts "\n### Batch mode error:\n$ERRMSG\n\n"
	       prolog assert_cli_error_occurred(batch)
	   }
}

proc tkErrorBox {msg} {
  global batch_mode
   global testing_mode strFilename failed_tests detailed_errors
  if {$batch_mode} {
     puts "### An Error occurred\n$msg"
     prolog assert_cli_error_occurred(tkErrorBox)
  } elseif {$testing_mode==true} {
       append failed_tests $strFilename "; 1 Error Message ;;"
       append detailed_errors $strFilename ";  " $msg ";;"
  } else {
     tk_messageBox -parent . -icon error -message $msg
  }
}
proc tkErrorBoxNoParent {msg} {
  global batch_mode
   global testing_mode strFilename failed_tests detailed_errors
  if {$batch_mode} {
     puts "### An Error occurred\n$msg"
     prolog assert_cli_error_occurred(tkErrorBoxNoParent)
  } elseif {$testing_mode==true} {
       append failed_tests $strFilename "; 1 Error Message ;;"
       append detailed_errors $strFilename ";  " $msg ";;"
  } else {
     tk_messageBox -icon error -message $msg
  }
}
proc tkMessageBox {msg} {
  global batch_mode
  if {$batch_mode} {
     puts "$msg"
  } else {
     tk_messageBox -parent . -message $msg
  }
}
proc tkMessageBoxNoParent {msg} {
  global batch_mode
  if {$batch_mode} {
     puts "$msg"
  } else {
     tk_messageBox -message $msg
  }
}

proc tkYesNoMessageWarningBox {Msg Title} {
  global batch_mode
  if {$batch_mode} {
     return "no"
     prolog assert_cli_error_occurred(tkYesNoMessageWarningBox)
  } else {
     return [tk_messageBox -default yes\
                  -message "$Msg" -title "$Title" -type yesno -icon warning -parent .]
  }
}

proc procToUnicode {} {
    global strFilename
    set csp_ext [file extension $strFilename]
    if { $csp_ext == ".csp" || $csp_ext == ".cspm" } {
    # Enable "Unicode Options" Menu when the loaded File one of the Extensions .csp or .cspm has
          .frmMenu.mnuEdit entryconfigure 9 -state normal
    } else {
          .frmMenu.mnuEdit entryconfigure 9 -state disabled
    }
}

proc toUnicodeCommand {} {
    global strFilename
    set addUnicodeOption "--addUnicode="
    prolog "cspm_file_from_to_unicode('$strFilename', '$addUnicodeOption')"
    procReOpenFile
}

proc fromUnicodeCommand {} {
    global strFilename
    set removeUnicodeOption "--removeUnicode="
    prolog "cspm_file_from_to_unicode('$strFilename', '$removeUnicodeOption')"
    procReOpenFile
}

proc GotoLineSelect {} {
   set LineNumber [Dialog_Prompt "Show Line Number:"]
   while {![string is integer $LineNumber]} {
    tkErrorBox "Invalid input. Please enter a valid number."
    set LineNumber [Dialog_Prompt "Show Line Number:"]
   }
   if {$LineNumber != ""} {
    GotoLine $LineNumber
   }
}

proc GotoLine {lineNo} {
    set BG   "lightgreen"	;# default background color for text
    set FG   "lightgray"	;# default foreground color for text
	.frmSource.text tag remove hilite 0.0 end
	.frmSource.text tag add hilite $lineNo.0 $lineNo.1000
	.frmSource.text tag configure hilite -background $BG
	# -foreground $FG
	.frmSource.text yview -pickplace [expr $lineNo-4]
}

proc procGetAccKey {} {
   return "Command"
   if [prolog tools:host_platform(windows)] {
	     return "Control"
	} elseif [prolog tools:host_platform(darwin)] {
	     return "Command"
	} else {
	     return "Control"
	     # Meta ?
	}
}

proc procSetCommandKeyShortCuts {} {
   set CMD [procGetAccKey]

   bind all <$CMD-a> {procAddDefaultCSPFile}
   bind all <$CMD-A> {procAddDefaultCSPFile}
   bind all <$CMD-b> {procTypeCheckFile}
   bind all <$CMD-b> {procTypeCheckFile}
   bind all <$CMD-C> {event generate .frmSource.text <<Copy>>}
   bind all <$CMD-d> {procDisplayVisitedStates}
   bind all <$CMD-D> {procDisplayVisitedStates}
   bind all <$CMD-f> {procAdvancedFind}
   bind all <$CMD-F> {procAdvancedFind}
   bind all <$CMD-e> {procOpenFileInEditor}
   bind all <$CMD-E> {procOpenFileInEditor}
   bind all <$CMD-g> {procComputeCoverage 0}
   bind all <$CMD-G> {procComputeCoverage 0}
   bind all <$CMD-h> {procViewModuleHierarchy}
   bind all <$CMD-H> {procViewModuleHierarchy}
   bind all <$CMD-i> {procAnalyseInvariant}
   bind all <$CMD-I> {procAnalyseInvariant}
   bind all <$CMD-j> {procJumpToAnOpenNode}
   bind all <$CMD-J> {procJumpToAnOpenNode}

   bind all <$CMD-k> {procDisplayReducedSigMerge}
   bind all <$CMD-K> {procDisplayReducedSigMerge}
   bind all <$CMD-l> {procLtl}
   bind all <$CMD-L> {procLtl}
   bind all <$CMD-m> {puts "CMD-m"; procModelCheck}
   bind all <$CMD-M> {puts "CMD-M"; procModelCheck}
   bind all <$CMD-n> {procNewFile}
   bind all <$CMD-N> {procNewFile}
   bind all <$CMD-o> {procOpenFile}
   bind all <$CMD-O> {procOpenFile}

   bind all <$CMD-q> {procDoQuit}
   bind all <$CMD-Q> {procDoQuit}
   bind all <$CMD-r> {procReOpenFile}
   bind all <$CMD-R> {procReOpenFile}

   bind all <$CMD-s> {procSaveFile}
   bind all <$CMD-S> {procSaveFile}
   bind all <$CMD-t> {procCheckTraceFile}
   bind all <$CMD-T> {procCheckTraceFile}
   bind all <$CMD-u> {procCheckStatespaceWithFDR}
   bind all <$CMD-U> {procCheckStatespaceWithFDR}
   bind all <$CMD-V> {event generate .frmSource.text <<Paste>>}

   bind all <$CMD-X> {event generate .frmSource.text <<Cut>>}

   bind all <$CMD-bracketleft>  {procAnalyseDEFGOAL}
   bind all <$CMD-bracketright> {procAnalyseAssertions}

}

proc procUpdateMenu {} {
    global normal_user
    if {$normal_user} {
      prologmnf preferences:set_preference(user_not_beginner,true)
    } else {
      prologmnf preferences:set_preference(user_not_beginner,false)
    }
    tkMessageBox "You need to restart ProB for these settings to take effect."
}

global forceQuit
set forceQuit 0

proc procQuit {} {
   global forceQuit
   global app_dir
   if {! $forceQuit} {
     procSavePrefs
     procCheckIfSaved
   }
}
proc procDoQuit {} {
   global forceQuit
   # puts "procDoQuit"
   procQuit
   set forceQuit 1
   # puts "destroy ."
   destroy .
}

proc procResetFilesMenu {} {
   global CSP_File_Already_Added
   .frmMenu.mnuFiles delete 0 999
   set CSP_File_Already_Added 0
}

proc procAddNewFileToFilesMenu {FileToShow Type} {
   global CSP_File_Already_Added
   if {$Type=="CSP"} {
      if {$CSP_File_Already_Added} {
       .frmMenu.mnuFiles entryconfigure last -label "$FileToShow"
       .frmMenu.mnuFiles entryconfigure last -command "procShowSpecificFile {$FileToShow} {$Type}"
       # Assumption: CSP File always last and only one CSP file
      } else {
        set CSP_File_Already_Added 1
       .frmMenu.mnuFiles add sep
       .frmMenu.mnuFiles add command -label "$FileToShow" -command "procShowSpecificFile {$FileToShow} {$Type}"
      }
   } else {
     #set lastlabel [.frmMenu.mnuFiles entrycget last -label]
     .frmMenu.mnuFiles add command -label "$FileToShow" -command "procShowSpecificFile {$FileToShow} {$Type}"
   }
}

proc procShowSpecificFile {FileToShow Type} {
  procCheckIfSaved
  procShowSourceCode $FileToShow
  if {$Type=="B"} {
     procDoSyntaxColouring .frmSource.text
     procEnableSourceCodeEditing
  } elseif {$Type=="CSP"} {
     procDoCSPSyntaxColouring  .frmSource.text
     procEnableSourceCodeEditing
  }
  procResetCodeModified
}

# -------

proc procRebuildRecentDocumentsMenu {} {
    return
    # set lasti [.frmMenu.mnuFile.mnuRecentDocuments cget end]
    # puts "Rebuild: lasti $lasti"

    .frmMenu.mnuFile.mnuRecentDocuments delete 0 999

    prologmnf get_recent_documents(List)
    set ii 0
    set AccKey [procGetAccKey]
    foreach i $prolog_variables(List) {
       if {$ii < 1} {
         .frmMenu.mnuFile.mnuRecentDocuments add command -label "$i" -command "procOpenSpecificFile {$i}" -accelerator "$AccKey+$ii"
          bind all <Meta-$ii> "procOpenSpecificFile {$i}"
          bind all <Command-$ii> "procOpenSpecificFile {$i}"
          bind all <Control-$ii> "procOpenSpecificFile {$i}"
          incr ii
       } else {
         .frmMenu.mnuFile.mnuRecentDocuments add command -label "$i" -command "procOpenSpecificFile {$i}"
       }
    }
    .frmMenu.mnuFile.mnuRecentDocuments add sep
    .frmMenu.mnuFile.mnuRecentDocuments add command -label "Clear Menu" \
          -command "prologmnf preferences:clear_recent_documents ; procRebuildRecentDocumentsMenu"
}

# -------

proc procEnableReopen {} {
    global strFilename
    global version normal_user
	# enable ReOpen MenuItem
	set fileTail [file tail $strFilename]
	.frmMenu.mnuFile entryconfigure 2 -state normal \
	   -label "Reopen $fileTail"

    if {$normal_user} {
      .frmMenu.mnuFile entryconfigure 10 -state normal \
	     -label "Open $fileTail in External Editor"
	 }

    if [prolog animation_mode(Mode),plugins:is_registered_plugin(Mode,Name)] {
        wm title . "$prolog_variables(Name): $version \[$fileTail\]"
    } else {
     set ext [file extension $strFilename]
     if {$ext == ".tex"} {
		   wm title . "ProZ:  $version: \[$fileTail\]"
		} elseif {$ext == ".cspm" || $ext == ".csp"} {
		   wm title . "ProCSP: $version \[$fileTail\]"
		} elseif {$ext == ".pml" || $ext == ".prom"} {
		   wm title . "ProM: $version \[$fileTail\]"
		} elseif {$ext == ".P"} {
		   wm title . "ProXTL $version: \[$fileTail\]"
		} else {
		   wm title . "ProB $version: \[$fileTail\]"
		}
    }
}
proc procEnableItemsAfterOpeningFile {} {
    global expert_user normal_user

	procEnableReopen

	.frmMenu.mnuFile entryconfigure 5 -state normal
	.frmMenu.mnuFile entryconfigure 7 -state normal
	.frmMenu.mnuFile entryconfigure 8 -state normal
	if {$normal_user} {
	 .frmMenu.mnuFile entryconfigure 10 -state normal
	}


    # enable Display State
    .frmMenu.mnuAnimate entryconfigure 5 -state normal
    .frmMenu.mnuAnimate entryconfigure 6 -state normal

	     .frmMenu.mnuEdit entryconfigure 0 -state disabled
	     .frmMenu.mnuEdit entryconfigure 1 -state disabled

    procUpdateMenusFromPrologInfo
}
proc procDisableItemsAfterClosingFile {} {
	.frmMenu.mnuFile entryconfigure 5 -state disabled
	.frmMenu.mnuFile entryconfigure 7 -state disabled
	.frmMenu.mnuFile entryconfigure 10 -state disabled

    .frmMenu.mnuAnimate entryconfigure 5 -state disabled
    .frmMenu.mnuAnimate entryconfigure 6 -state disabled


    procResetFilesMenu
}




# -------
# procedure to initialise info section
# -------
proc procGUI_Info {} {
    frame .frmInfo -borderwidth .1c  -relief groove
    if [prolog get_preference(use_small_window,true)] {
        if [prolog get_preference(tk_show_source_pane,true)] {
		  set pane_height 12
	    } else {
	      set pane_height 18
	    }
		set pane_width 25
    } else {
		if [prolog get_preference(tk_show_source_pane,true)] {
		  set pane_height 15
	    } else {
	      set pane_height 22
	    }
		set pane_width 30
    }

    # ------ History
    frame .frmInfo.frmHisto -relief groove
    label .frmInfo.frmHisto.label -text History -relief groove
    scrollbar .frmInfo.frmHisto.scrolly -command ".frmInfo.frmHisto.list yview"
    scrollbar .frmInfo.frmHisto.scrollx -command ".frmInfo.frmHisto.list xview" -orient h
    listbox .frmInfo.frmHisto.list -yscroll ".frmInfo.frmHisto.scrolly set" \
        -xscroll ".frmInfo.frmHisto.scrollx set" -setgrid 1 -height $pane_height -width $pane_width -bg white -selectmode extended
    pack .frmInfo.frmHisto.label -side top -fill x
    pack .frmInfo.frmHisto.scrolly -side right -fill y
    pack .frmInfo.frmHisto.scrollx -side bottom -fill x
    pack .frmInfo.frmHisto.list -side left -expand 1 -fill both


    # ------ States
    frame .frmInfo.frmState -relief groove
    label .frmInfo.frmState.label -text StateProperties -relief groove
    # set StatusFrame .frmInfo.frmStatus
    global StatusFrame
    set StatusFrame .frmInfo.frmState.status
    frame $StatusFrame -relief groove
      # label $StatusFrame.invlbl -text "INVARIANT:" -fg blue -font "Courier 9"
      global InvKO InvOK InvUnknown InvTimeout Timeout Maxreach MaxreachEmpty tcl_dir
      if [file exists "$tcl_dir/icons/InvUnknown.gif"] {
        image create photo InvUnknown -format gif -fil "$tcl_dir/icons/InvUnknown.gif"
        image create photo InvOK -format gif -fil "$tcl_dir/icons/InvOK.gif"
        image create photo InvKO -format gif -fil "$tcl_dir/icons/InvKO.gif"
        image create photo InvTimeout -format gif -fil "$tcl_dir/icons/InvTimeout.gif"
        image create photo Timeout -format gif -fil "$tcl_dir/icons/Timeout.gif"
        image create photo TimeoutEmpty -format gif -fil "$tcl_dir/icons/TimeoutEmpty.gif"
        image create photo Maxreach -format gif -fil "$tcl_dir/icons/Maxreach.gif"
        image create photo MaxreachEmpty -format gif -fil "$tcl_dir/icons/MaxreachEmpty.gif"
        image create photo MaxreachSym -format gif -fil "$tcl_dir/icons/MaxreachSym.gif"
        image create photo BackEnabled -format gif -fil "$tcl_dir/icons/BackEnabled.gif"
        # image create photo BackDisabled -format gif -fil "$tcl_dir/icons/BackDisabled.gif"
        image create photo ForwardEnabled -format gif -fil "$tcl_dir/icons/ForwardEnabled.gif"
        # image create photo ForwardDisabled -format gif -fil "$tcl_dir/icons/ForwardDisabled.gif"
        image create photo WindowSubIcon -format gif -fil "$tcl_dir/icons/prob_sub_128.gif"
      } else {
        image create photo InvUnknown
        image create photo InvOK
        image create photo InvKO
        image create photo Timeout
        image create photo TimeoutEmpty
        image create photo Maxreach
        image create photo MaxreachEmpty
        image create photo MaxreachSym
        image create photo BackEnabled
        image create photo ForwardEnabled
      }
      button $StatusFrame.inv -text OK -image InvUnknown -relief flat -command {procAnalyseInvariant}
      #button $StatusFrame.maxreached -image MaxreachEmpty\
          -command {tkMessageBox "Possibly not all enabled operations were computed. Increase Max Number of Initialisations/Operations in the Animation Preferences and re-load the machine."}
      # label $StatusFrame.lbl2 -text "STATUS:" -font "Courier 9"
      label $StatusFrame.lbl2 -text "State Properties"
      button $StatusFrame.timeout -image TimeoutEmpty\
           -command {procTimeOutButton}
      pack   $StatusFrame.inv -side left
      #pack $StatusFrame.maxreached -side left
      pack $StatusFrame.lbl2 -side left -padx 15 -fill x
      pack $StatusFrame.timeout -side right  -padx 15
    scrollbar .frmInfo.frmState.scrolly -command ".frmInfo.frmState.list yview"
    scrollbar .frmInfo.frmState.scrollx -command ".frmInfo.frmState.list xview" -orient h
    listbox .frmInfo.frmState.list -yscroll ".frmInfo.frmState.scrolly set" \
        -xscroll ".frmInfo.frmState.scrollx set" -setgrid 1 -height $pane_height -width $pane_width -bg white -selectmode extended
    bind .frmInfo.frmState.list <Button1-ButtonRelease> procShowStateDetail
    # pack .frmInfo.frmState.label -side top -fill x
    pack $StatusFrame -side top -fill x
    pack .frmInfo.frmState.scrolly -side right -fill y
    pack .frmInfo.frmState.scrollx -side bottom -fill x
    pack .frmInfo.frmState.list -side left -expand 1 -fill both

    # ------ Available options
    frame .frmInfo.frmPerform -relief groove
    global OpStatusFrame
    set OpStatusFrame .frmInfo.frmPerform.status
    frame $OpStatusFrame -relief groove
    label $OpStatusFrame.label -text EnabledOperations
    # -relief groove
      button $OpStatusFrame.maxreached -image MaxreachEmpty\
          -command {procMaxreachedButtonAction}
      button $OpStatusFrame.backtrack -image BackEnabled -command procPerformBacktrack
      button $OpStatusFrame.forward -image ForwardEnabled -command procPerformForward
      label $OpStatusFrame.empty -text {}
      pack $OpStatusFrame.maxreached -side left
      pack $OpStatusFrame.label -side left -padx 15 -fill x
      pack $OpStatusFrame.empty -side right -padx 5
      pack $OpStatusFrame.forward -side right
      pack $OpStatusFrame.backtrack -side right

    scrollbar .frmInfo.frmPerform.scrolly -command ".frmInfo.frmPerform.list yview"
    scrollbar .frmInfo.frmPerform.scrollx -command ".frmInfo.frmPerform.list xview" -orient h
    listbox .frmInfo.frmPerform.list -yscroll ".frmInfo.frmPerform.scrolly set" \
         -xscroll ".frmInfo.frmPerform.scrollx set" -setgrid 1 -height $pane_height -width $pane_width -bg white
    bind .frmInfo.frmPerform.list <ButtonRelease> {procPerformOptionSingleClick}
    bind .frmInfo.frmPerform.list <Double-1> {procPerformOption}
    pack $OpStatusFrame -side top -fill x
    pack .frmInfo.frmPerform.scrolly -side right -fill y
    pack .frmInfo.frmPerform.scrollx -side bottom -fill x
    pack .frmInfo.frmPerform.list -side left -expand 1 -fill both

    bind .frmInfo.frmPerform.list <Button-3> {procPerformContextMenu %X %Y}


    # ------- pack all top frames
    pack .frmInfo.frmState .frmInfo.frmPerform .frmInfo.frmHisto -side left -expand yes -fill both
}

proc procCreateOperationsPopupMenu {hasTrace} {
    destroy .operationspopup
    menu .operationspopup -tearoff 0
    if $hasTrace {
        set state normal
    } else {
        set state disabled
    }
    .operationspopup add command -label "Show Event Trace" -command procShowEventTrace -state $state
}

proc procPerformContextMenu {X Y} {
    set listy [winfo rooty .frmInfo.frmPerform.list]
    set ycoord [expr $Y - $listy]
    set index [.frmInfo.frmPerform.list nearest $ycoord]
    .frmInfo.frmPerform.list selection clear 0 end
    .frmInfo.frmPerform.list selection set $index
    .frmInfo.frmPerform.list activate $index
    set hasTrace [prolog tcltk_has_eventtrace($index)]
    procCreateOperationsPopupMenu $hasTrace
    tk_popup .operationspopup $X $Y
}

proc procShowEventTrace {} {
    set sel [.frmInfo.frmPerform.list curselection]
    if [expr [llength $sel] == 1] {
        puts "selection: $sel"
        if [prolog tcltk_show_eventtrace($sel,Result)] {
            set Result $prolog_variables(Result)
            procShowText $Result "Event Trace"
        } else {
            tkErrorBox "Internal Error: Could not display event trace."
        }
    }
}

proc procShowStateDetail {} {
    set sel [.frmInfo.frmState.list curselection]
    if [expr [llength $sel] == 1] {
	if [prolog tcltk_get_detailed_state_error($sel,ErrorMsg)] {
	    procShowText $prolog_variables(ErrorMsg) "Detailed transition error"
	}
    }
}

proc procTimeOutButton {} {
    prologmnf "preferences:get_preference(time_out,CurTO)"
	set Result $prolog_variables(CurTO)
	prologmnf "eclipse_interface:ops_with_timeout(OpList)"
	set List $prolog_variables(OpList)
    set ans [tkYesNoMessageWarningBox "A timeout ($Result ms) has occurred while computing enabled operations ($List).\nRecompute this state without a timeout?" "Timeout!"]
    if { $ans == "yes" } {
		prologmnf tcltk_mark_current_node_to_be_recomputed_wo_timeout
		global show_error_if_no_transition
		set show_error_if_no_transition 1
		procInsertHistoryOptionsState
	}
}

proc procCheckForPlugins {} {
    global plugin_menu_index
    if {$plugin_menu_index >= 0} {
        .frmMenu.mnuAnalyse.mnuPlugins delete 0 end
        if [prolog plugins:tcl_plugin_is_available_for_mode] {
            prolog plugins:tcl_available_plugins_for_mode(Plugins)
            set plugins $prolog_variables(Plugins)
            foreach p $plugins {
                set pluginid [lindex $p 0]
                set pluginname [lindex $p 1]
                .frmMenu.mnuAnalyse.mnuPlugins add command -label "$pluginname" \
                    -command "procActivatePlugin $pluginid"
            }
            set new_state normal
        } else {
            set new_state disabled
        }
        .frmMenu.mnuAnalyse entryconfigure $plugin_menu_index -state $new_state
    }
}

proc procActivatePlugin {id} {
    prolog plugins:start_plugin_for_current_mode($id)
    procInsertHistoryOptionsState
    procShowErrors
}

# -------
# procedure to initialise source code section
# -------
proc procGUI_Source {} {
    set prolog_variables(PrefFont) "Times 12"
    set prolog_variables(UseForCols) "1"

    if [prolog get_preference(tk_show_source_pane,true)] {
		if [prolog get_preference(use_small_window,true)] {
			set source_height 17
		} else {
			set source_height 23
		}
		# ------- code source frames
		frame .frmSource -borderwidth .1c -relief groove
		scrollbar .frmSource.scrolly -command ".frmSource.text yview"
		scrollbar .frmSource.scrollx -command ".frmSource.text xview" -orient h
		text .frmSource.text -yscroll ".frmSource.scrolly set" -xscroll ".frmSource.scrollx set" \
			-setgrid 1 -height $source_height -state disabled
                .frmSource.text edit modified 0
		pack .frmSource.scrolly -side right -fill y
		pack .frmSource.scrollx -side bottom -fill x
		pack .frmSource.text -expand 1 -fill both
    } else {
		set source_height 0
		frame .frmSource -borderwidth .1c -relief groove
		text .frmSource.text -height $source_height -state disabled
		# pack .frmSource.text -expand 1 -fill both
    }


    #set font to monospaced font (windows defaults to times else)
    #prologmnf "get_preference(font_size,PrefFont)"
    #set chosenfont $prolog_variables(PrefFont)
    #changeFont $chosenfont

    bind .frmSource.text <Destroy> {procQuit}

    #  proxy all insert calls to the filter frame to keep modified state saved
    rename .frmSource.text ..frmSource.text
    proc .frmSource.text args {
        if [regexp {^(ins|del).*} [lindex $args 0]] { procSetCodeModified }
	    uplevel ..frmSource.text $args
    }

    focus -force .frmSource.text
    .frmSource.text configure -undo 1

   bind .frmSource.text <<Modified>>  procupdateState
}
    proc procupdateState {args} {

      # Check the modified state and update the label
      if { [.frmSource.text edit modified] } {
	     .frmMenu.mnuEdit entryconfigure 0 -state normal
	     .frmMenu.mnuEdit entryconfigure 1 -state normal
      } else {
	     .frmMenu.mnuEdit entryconfigure 0 -state disabled
	     .frmMenu.mnuEdit entryconfigure 1 -state disabled
      }
   }
   proc procundo {} {
      # edit undo throws an exception when there is nothing to
      # undo. So catch it.
      global undosperformed
      if { [catch {.frmSource.text edit undo}] } {
         bell
      } else {
          incr undosperformed
          .frmMenu.mnuEdit entryconfigure 1 -state normal
      }
   }

   proc procredo {} {
      # edit redo throws an exception when there is nothing to
      # undo. So catch it.
      if { [catch {.frmSource.text edit redo}] } {
         bell
      } else {
        global undosperformed
        set undosperformed [expr $undosperformed-1]
        if {$undosperformed==0} {
           .frmMenu.mnuEdit entryconfigure 1 -state disabled
        }
      }
   }

   proc procresetundo {} {
     global undosperformed
     set undosperformed 0
	 .frmMenu.mnuEdit entryconfigure 0 -state disabled
	 .frmMenu.mnuEdit entryconfigure 1 -state disabled
     # Reset the modified state
     .frmSource.text edit modified 0
     # Clear the undo stack
	 .frmSource.text edit reset
     .frmSource.text configure -undo 1
   }

proc changeFont {chosenfont} {
    global chosefontval
    # puts "Changing Font: $chosefontval $chosenfont"
    set chosefontval $chosenfont

   .frmSource.text configure -font $chosenfont
    prologmnf "get_preference(use_font_size_for_columns,UseForCols)"
    set useforcolumns $prolog_variables(UseForCols)
   if {$useforcolumns == "true"} {
      .frmInfo.frmHisto.label configure -font $chosenfont
      .frmInfo.frmHisto.list configure -font $chosenfont
      .frmInfo.frmPerform.label configure -font $chosenfont
      .frmInfo.frmPerform.list configure -font $chosenfont
      .frmInfo.frmState.label configure -font $chosenfont
      .frmInfo.frmState.list configure -font $chosenfont
   }
    prolog "set_preference(font_size,'$chosenfont')"
}


# -------
# procedure to initialise main GUI
# -------

proc procInitPreferences {} {
    global mcPerformBreadthFirst mcFindDeadlocks mcFindInvViolations
    global mcSearchForNewErrors mcFindGOAL
    set mcPerformBreadthFirst 0
    set mcFindDeadlocks 1
    set mcFindInvViolations 1
    set mcSearchForNewErrors 1
    set mcFindGOAL 0
    set mcFindAssViolations 0

    global mc_prompt
    set mc_prompt(result) "100000"

    global ltldata
    set ltldata(result) "1000"
    set ltldata(mode) "init"
}

proc procGUI_WindowIcon {} {
    global tcl_dir tcl_version
   if {$tcl_version>=8.5} {
    if [file exists "$tcl_dir/icons/prob_128.gif"] {
	  image create photo WindowIcon -format gif -fil "$tcl_dir/icons/prob_128.gif"
	  wm iconphoto . WindowIcon
    }
    }
}

proc procInitGUI {} {
    global version
    wm title . "ProB $version"
    wm iconname . "ProB"
#g    frame .frmMenu

    procGUI_WindowIcon
    procGUI_Menu
    procGUI_Info
    procGUI_Source
    procSetCommandKeyShortCuts

   # Pane_Create .frmSource .frmInfo -percent 0.6 -orient vertical -in .main

    # ------- arrange all top frames
    pack .frmInfo .frmSource -side bottom -expand yes -pady 1 -fill both


   # wm geometry . 80x50

    procAboutProBInSourceFrame
}


proc procGetEnviron {ENV} {
   # we could use global env instead
    if [prolog system:environ($ENV,Dir)] {
      return $prolog_variables(Dir)
    } else {
      tkErrorBox "Environment variable $ENV not set."
      return ""
    }
}

proc procLawCheckTest {full} {
    global strFilename version
    global bench_MC_maxnodes
    set ProBDir [procGetEnviron 'PROB_SOURCE_DIR']
    set NewProBExDir [procGetEnviron 'PROB_EX_DIR']
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

    set strFilename "$NewProBExDir/B/Laws/ExplicitComputations.mch"
    procGenericLoadFile
    procSingleBenchmarkFullOpts 1 1 1 1
    procCheckDebugProperties

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



proc procRegressionTest {} {
    global strFilename
    procStartTesting regression

    global bench_MC_maxnodes
    set bench_MC_maxnodes_bak $bench_MC_maxnodes
    set bench_MC_maxnodes 100

    set ProBDir [procGetEnviron 'PROB_SOURCE_DIR']
    set NewProBExDir [procGetEnviron 'PROB_EX_DIR']
    set NewProBPrivateExDir [procGetEnviron 'PROB_PEX_DIR']


    set strFilename "$NewProBExDir/B/Benchmarks/Chapter_10/Safes.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/ErrorMachines/MultiAssignInit_bug.mch"
    procGenericLoadFile
    procSingleBenchmarkWoErrCheck 1 0 0 0
    procExpectErrors 1

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

    set strFilename "$NewProBExDir/B/Tester/CaseStmt.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/Tester/SubstSetInTests.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/SymmetryReduction/RelationRelation.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0


    set strFilename "$NewProBExDir/B/FeatureChecks/INCLUDES_USES_SEES/ParameterChecks/M1.mch"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

    set strFilename "$NewProBExDir/B/RenameParameterTests/Customer.mch"
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

    set strFilename "$NewProBExDir/B/RenameParameterTests/TwoCounters.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/RenameParameterTests/FourCounters.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/RenameParameterTests/ExtCounter.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/RenameParameterTests/ExtPromCounter.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/RenameParameterTests/TwoExtCounters.mch"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/B/RenameParameterTests/FourExtCounters.mch"
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

    set strFilename "$NewProBExDir/B/RenameParameterTests/MyTwoDatas.mch"
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

  #  set strFilename "$ProBDir/cia/examples/Bakery_new.cia"
  #  procLoadCSPFile
  #  procSingleBenchmark 1 0 0


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
    set strFilename "$NewProBPrivateExDir/RodinModels/PaulSimon/ca_m01_mch.eventb"
    procGenericLoadFile
    procSingleBenchmark 1 0 0

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
    procAnalysePredCheck assertions 191 8 0


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


proc procCSPMBenchmark {} {
    global strFilename forceRecompile
    global version
    set backforceRecompile $forceRecompile
    set forceRecompile 0
    global bench_MC_maxnodes
    set bench_MC_maxnodes_bak $bench_MC_maxnodes
    set bench_MC_maxnodes 20000

    set ProBDir [procGetEnviron 'PROB_SOURCE_DIR']
    set NewProBExDir [procGetEnviron 'PROB_EX_DIR']
    set NewProBPrivateExDir [procGetEnviron 'PROB_PEX_DIR']

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

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/ContrivedLambda.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1
    procCheckStatespaceWithFDR

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/RecordDotTest.csp"
    procGenericLoadFile
    procSingleBenchmark 1 1 1

    set strFilename "$NewProBExDir/CSP/FDRFeatureTests/RecordDotTest2.csp"
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


proc procZTest {} {
    prologmnf proz:startzconversiontests
    procShowErrors
}


proc procBenchmark {} {
    global strFilename forceRecompile
    global version
    set backforceRecompile $forceRecompile
    set forceRecompile 0

    set ProBDir [procGetEnviron 'PROB_SOURCE_DIR']
    set NewProBExDir [procGetEnviron 'PROB_EX_DIR']

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

proc procSymBenchmark {} {
   global strFilename
   if {$strFilename != ""} {
     set UP [Dialog_Prompt "Enter Max Size of deferred sets:"]
     if {$UP != ""} {
       procSymBenchmark2 1 $UP
     }
   } else {
       tkErrorBox "Open a B Machine first."
   }
}

proc procSymBenchmark2 {lowbound upbound} {
    global strFilename forceRecompile
    global logFile bench_MC_maxnodes
    set cpy $bench_MC_maxnodes
    set bench_MC_maxnodes 100000
    global version
    prolog system:environ('PROB_SOURCE_DIR',Dir)
    set backforceRecompile $forceRecompile
    set forceRecompile 0
    set ProBDir $prolog_variables(Dir)

    procStartTesting sym_bench

    #set strFilename "$ProBDir/Machines/Benchmarks/scheduler.mch"

    for {set i $lowbound} {$i <= $upbound} {incr i} {
       puts $logFile "Size of unspecified deferred sets: $i"
       puts "Size of unspecified deferred sets: $i"
       prologmnf preferences:set_preference(globalsets_fdrange,$i)
       procGenericLoadFile
       procSingleBenchmark 0 1 1
       prologmnf tcltk_compute_coverage(Res)
       puts "$prolog_variables(Res)"
       puts $logFile "-----------"
    }
    procEndTesting

    set forceRecompile $backforceRecompile
    set bench_MC_maxnodes cpy
}

global bench_MC_maxnodes
set bench_MC_maxnodes 100

proc procSingleBenchmarkWoErrCheck {traceCheck modelCheck findInvViol findAssViol} {
    global strFilename
    global logFile
    global bench_MC_maxnodes
    puts $logFile ""
    puts $logFile "% Benchmarking: $strFilename"
	global mcFindDeadlocks mcFindInvViolations mcSearchForNewErrors
	if {$modelCheck} {
     puts "% Model checking benchmark system"
     set mctime [time {prolog tcltk_model_check($bench_MC_maxnodes,ErrRes,0,$findInvViol,0,$findAssViol,0,360000,PrologTime)} 1]
     puts $logFile "% Model Checking Time: $mctime"
     set mctimems [lindex [split $mctime] 0]
     set CURDATEH [clock format [clock seconds] -format "%Y%m%d%H"]
     puts $logFile "mc('$strFilename',$CURDATEH,$mctimems)."
     puts "MC Time: $mctimems microsecs, Prolog: $prolog_variables(PrologTime) millisecs"
     #set Result $prolog_variables(ErrRes)
     if {$findInvViol && [prolog tcltk_goto_an_invariant_violation]} {
          procInsertHistoryOptionsState
          tkErrorBox "Invariant violation found in $strFilename!"
     }
     puts "Computing coverage"
     prologmnf tcltk_compute_coverage(Res)
     # obtain a list of conjuncts with their status
	 set Result $prolog_variables(Res)
	 puts $logFile "/* $Result */"
	}
	if {$traceCheck} {
     puts "% Trace checking benchmark system"
     puts $logFile "% Trace Checking Time: [time {procCheckTraceFile2 0 1} 1]"
	}
}

proc procSingleBenchmark {traceCheck modelCheck findInvViol} {
   procSingleBenchmarkFullOpts $traceCheck $modelCheck $findInvViol 0
}
proc procSingleBenchmarkFullOpts {traceCheck modelCheck findInvViol findAssViol} {
   if [prolog spec_file_has_been_successfully_loaded] {
    procSingleBenchmarkWoErrCheck $traceCheck $modelCheck $findInvViol $findAssViol
    procInsertHistoryOptionsState
    procShowErrors
  } else {
    puts "### No specification file has been successfully loaded"
    puts "### Skipping Test"
  }
}

proc procCheckInvStatus {iNVVIOLATED} {
   if [prolog tcltk_get_status($iNVVIOLATED,_,_)] {
     puts "Invariant violated = $iNVVIOLATED"
   } else {
     tkErrorBox "Invariant status ($iNVVIOLATED) not as expected !"
   }
}
proc procPerformSelfCheck {} {
  global curFileTypeOpened
  if {$curFileTypeOpened != "None"} {
      tkErrorBox "The Self-Check can only be performed before opening a machine.\nRestart ProB and do the self-check after startup."
  } else {
      if [prolog perform_self_check] {
        tkMessageBox "Self-Check was successful."
      } else {
        procShowErrors
      }
  }
}

proc procPerformVerboseSelfCheck {} {
  global curFileTypeOpened
  if {$curFileTypeOpened != "None"} {
      tkErrorBox "The Self-Check can only be performed before opening a machine.\nRestart ProB and do the self-check after startup."
  } else {
      if [prolog perform_verbose_self_check] {
        tkMessageBox "Self-Check was successful."
      } else {
        procShowErrors
      }
  }
}

proc procPerformSelfCheckForModule {} {
  global curFileTypeOpened
  if {$curFileTypeOpened != "None"} {
      tkErrorBox "The Self-Check can only be performed before opening a machine.\nRestart ProB and do the self-check after startup."
  } else {
      prolog get_module_list(ML)
      set ModuleName [procChooseElementFromList $prolog_variables(ML) "Module" "no" "" "var" "true"]
      puts "ModuleName $ModuleName"
      if [prolog perform_self_check('$ModuleName')] {
        tkMessageBox "Self-Check was successful."
      } else {
        procShowErrors
      }
  }
}


# -------
# procedure to open a file, and then execute it on prolog
# -------

proc procNewFile {} {
    global strFilename
    procCheckIfSaved
    set types {
    	{"B Machine Files"		{.mch}	}
    	{"B Refinements"		{.ref}	}
    	{"B Implementations"	{.imp}	}
    	{"Event-B System Files"		{.sys}	}
    	{"All B Files"	{.mch .ref .imp .sys}	}
	    {"All files"		*}
    }
    # show the dialog box
    set oldstrFilename $strFilename
    proc_getSaveFile $types
    if {$strFilename != ""} {
        # Now create an empty Machine File
        set MachName [file rootname [file tail $strFilename]]
        set ext [file extension $strFilename]
		set fileid [open "$strFilename" w]
		if {$ext == ".cspm" || $ext == ".csp"} {
		  puts $fileid "-- $MachName\nMAIN = SKIP\n"
		} elseif {$ext == ".tex"} {
		  puts $fileid "\\documentclass[a4paper]{article}\n\\usepackage{fuzz}\n\\begin{document}\n\\end{document}\n"
		} elseif {$ext == ".P"} {
		  puts $fileid "start(a).\ntrans(lock,a,b).\ntrans(unlock,b,a).\nprop(X,X).\nprop(c,unsafe).\n"
		} else {
          puts $fileid "MACHINE $MachName\nSETS\n ID={aa,bb}\nCONSTANTS iv\nPROPERTIES\n iv:ID\nVARIABLES xx\nINVARIANT\n xx:ID\nINITIALISATION xx:=iv\nOPERATIONS\n  Set(yy) = PRE yy:ID THEN xx:= yy END\nEND\n"
        }
        close $fileid
        procGenericLoadFile
    } else {
        set strFilename $oldstrFilename
    }
}

proc proc_getSaveFile { types } {
    global strFilename curFileTypeOpened
    # show the dialog box
   if [prolog animation_mode(cspm)] {
      set untitled "Untitled.csp"
   } elseif [prolog animation_mode(xtl)] {
      set untitled "Untitled.P"
   } else {
      set untitled "Untitled.mch"
   }
    set machinesPath [getMachinesPath]
    if {$machinesPath != "" && [file isdirectory $machinesPath]} {
         set strFilename [tk_getSaveFile -filetypes $types -initialdir $machinesPath -initialfile "$untitled" -parent . ]
    } else {
         set strFilename [tk_getSaveFile -filetypes $types -initialfile "$untitled" -parent . ]
    }
    if {$strFilename != ""} {
	# remember the directory for next time
	setMachinesPath [file dirname $strFilename]
    }

}
# -------
# procedure to open a file, and then execute it on prolog
# -------
proc procOpenSpecificFile {theFileName} {
    global strFilename
    set strFilename $theFileName
    procCheckIfSaved
    procGenericLoadFile
}
proc procOpenFile {} {
    global strFilename curFileTypeOpened
    procCheckIfSaved
    set btypes {
			{"All B and CSP Files"	{.mch .ref .imp .sys .csp .eventb}	}
			{"B Machine Files"		{.mch}	}
			{"B Refinements"		{.ref}	}
			{"B Implementations"	{.imp}	}
    	    {"Event-B System Files"		{.sys}	}
    }
    set otherTypes {
        {"Other Formalisms (CSP,Z,XTL)"	{.csp .tex .P .cspm .pml .prom .eventb .smv}	}
    }
    if {$curFileTypeOpened == "B" || $curFileTypeOpened == "CSP" ||$curFileTypeOpened == "None" || $curFileTypeOpened == "EVENTB"} {
        set types [concat $btypes $otherTypes]
    } else {
		set types [concat $otherTypes $btypes]
    }
    # append extensions for plugins
    prolog plugins:tcl_plugin_file_extensions(Ext)
    set types [concat $types $prolog_variables(Ext)]
    # append all files
    lappend types {"All files" *}
    # show the dialog box
    set oldstrFilename $strFilename
    proc_getOpenFile $types
    procToUnicode
    if {$strFilename != ""} {
        procGenericLoadFile
    } else {
        set strFilename $oldstrFilename
    }
}

proc procConfigureEventBMenues {state} {
    global only_eventb_menues
    foreach menuentry $only_eventb_menues {
        set menu [lindex $menuentry 0]
        set index [lindex $menuentry 1]
        $menu entryconfigure $index -state $state
    }
}

proc procGenericLoadFile {} {
    global strFilename
    if [file exists $strFilename] {
        prolog preferences:add_recent_document('$strFilename')
        procRebuildRecentDocumentsMenu

        procConfigureEventBMenues disabled

		set ext [file extension $strFilename]
		if {$ext == ".tex"} {
		   procLoadZFile
		} elseif {$ext == ".cspm" || $ext == ".csp"} {
		   procLoadCSPMFile
		} elseif {$ext == ".pml" || $ext == ".prom"} {
		   procLoadPromelaFile
		} elseif {$ext == ".smv"} {
		   procLoadSmvFile
		} elseif {$ext == ".P"} {
		   procLoadXTLFile
		} elseif {$ext == ".eventb"} {
		   procLoadEventBPackage
           procConfigureEventBMenues normal
		} elseif [prolog plugins:is_plugin_file_extension('[escapeChars $ext]',Id)] {
            procLoadPluginFile $prolog_variables(Id)
        } else {
           procLoadBFile
		}
    } else {
	    tkErrorBox "File does not exist: $strFilename.\nCannot open it."
    }
}


proc procLoadEventBPackage {} {
    global strFilename curFileTypeOpened
  if {$strFilename != ""} {
        prologmnf tcltk_clear_machine
		procResetOptions "Opening..."
        set curFileTypeOpened "EVENTB"
        if [prolog tcltk_load_packaged_eventb_file('$strFilename')] {
            procShowErrors
			procInitLoadedBMachine
		   if [prolog bmachine:b_show_machine_representation(Res,true)] {
				procShowSourceCodeFromCodes .frmSource.text $prolog_variables(Res)
				procDoSyntaxColouring .frmSource.text
		   } else {
				procShowSourceCode $strFilename
				procDoEventBSyntaxColouring .frmSource.text
				tkErrorBox "Internal Error: Could not display internal representation for EventB model."
		   }
        } else {
           procShowErrors
	       procClearOptionsStateHistory
           procShowSourceCode $strFilename
		   procDoEventBSyntaxColouring .frmSource.text
        }
		procResetCodeModified
		procResetFilesMenu
		global expert_user
		if {$expert_user} {
		  .frmSource.text configure -state normal
		} else {
		  .frmSource.text configure -state disabled
		}
      procCheckForPlugins
    }
}


proc proc_getOpenFile { types } {
    global strFilename
    # show the dialog box
    set machinesPath [getMachinesPath]
    if {$machinesPath != "" && [file isdirectory $machinesPath]} {
         set strFilename [tk_getOpenFile -filetypes $types -initialdir $machinesPath -parent . ]
    } else {
         set strFilename [tk_getOpenFile -filetypes $types -parent . ]
    }
    if {$strFilename != ""} {
	#remember the directory for next time
	setMachinesPath [file dirname $strFilename]
    }

}


proc procTreatCommandLineArgs {} {
   global strFilename batch_mode
   if [prolog tcltk_get_command_line_option(batch_mode)] {
      puts "Going into batch mode"
      set batch_mode 1
   } else {
      set batch_mode 0
   }
   if [prolog tcltk_get_command_line_option(run_selfcheck)] {
      procPerformSelfCheck
   }
   if [prolog tcltk_get_command_line_option(run_benchmarks)] {
      puts "Running Benchmarks"
      procBenchmark
   }
   if [prolog tcltk_get_command_line_option(run_regression_tests)] {
      puts "Running Regression Tests"
      procRegressionTest
   }
   if [prolog tcltk_get_command_line_option(run_full_law_tests)] {
      procLawCheckTest "true"
   }
   if [prolog tcltk_get_command_line_option(run_quick_law_tests)] {
      procLawCheckTest "false"
   }
   if [prolog tcltk_get_command_line_option(run_cspm_tests)] {
      procCSPMBenchmark
   }
   if [prolog tcltk_get_command_line_file_to_open(X)] {
	  set strFilename $prolog_variables(X)
      if {$strFilename != ""} {
         procDisableItemsAfterClosingFile
         procGenericLoadFile
      }
   }
   if [prolog tcltk_get_command_line_tcltk_commands(Cmds)] {
      set nrargs [llength $prolog_variables(Cmds)]
      for {set i 0} {$i < $nrargs} {incr i 1} {
		set Command [lindex $prolog_variables(Cmds) $i]
		puts "Executing ProB Command Line $i/$nrargs: $Command"
		$Command
	  }
   }
}


proc procLoadXTLFile {} {
    procCheckIfSaved
    global strFilename curFileTypeOpened cspstrFilename
    if {$strFilename != ""} {
        set curFileTypeOpened "XTL"
        set cspstrFilename ""
        global version
        prolog tcltk_open_xtl_file('$strFilename')
            # the file exists, so load it.
        procShowSourceCode $strFilename
        procDoXTLSyntaxColouring .frmSource.text
        procFinishLoading
    }
}
proc procAddXTLFile {} {
    global strFilename
    set types {
    	{"XTL Trans/Prop Files"		{.P}	}
	    {"All files"		*}
    }
    # show the dialog box
    proc_getOpenFile $types
    if {$strFilename != ""} {
        prolog tcltk_add_xtl_file('$strFilename')
            # the file exists, so load it.
        procShowSourceCode $strFilename
	    prologmnf tcltk_initialise
        procInsertHistoryOptionsState
	    procEnableItemsAfterOpeningFile
    }
}

proc CallFuzz {FUZZ FUZZLIB FuzzName} {
   puts "call $FUZZ -p $FUZZLIB >$FuzzName"
    global strFilename curFileTypeOpened cspstrFilename
	    if {[catch {exec  "$FUZZ" -d -l -p "$FUZZLIB" $strFilename >$FuzzName} errid]} {
           HighlightLineErr $errid ":"
           tkErrorBox "Error while executing fuzz.\nError: $errid"
           return 0
        } else {
           return 1
        }
}
proc HighlightLineErr {errid AfterNr} {
   set idx [string first " line " $errid]
   if {$idx != -1} {
	 set start [expr $idx+6]
	 set end [string first "$AfterNr" $errid $start]
	 if {$end != -1} {
	     set end [expr $end-1]
		 set linenr [string range $errid $start $end]
		 # puts "Found line: $linenr ($start - $end)"
		 procHighlightLine $linenr
		 .frmSource.text see "$linenr.0"
		 return 1
	 }
   }
   return 0
}

proc HighlightBCompErr {errid} {
   set idx [string first ":" $errid]
   if {$idx != -1} {
	 set start [expr $idx+1]
	 set end [string first ":" $errid $start]
	 if {$end != -1} {
	     set end [expr $end-1]
		 set linenr [string range $errid $start $end]
		 # puts "Found line: $linenr ($start - $end)"
		 procHighlightLine $linenr
		 .frmSource.text see "$linenr.0"
		 return 1
	 }
   }
   return 0
}
proc CallFuzzNoLib {FUZZ FuzzName} {
   puts "call $FUZZ >$FuzzName"
    global strFilename curFileTypeOpened cspstrFilename
	    if {[catch {exec  "$FUZZ" -d -l $strFilename >$FuzzName} errid]} {
           HighlightLineErr $errid ":"
           tkErrorBox "Error while executing fuzz ($FUZZ).\nError: $errid"
           return 0
        } else {
           return 1
        }
}

proc procLoadZFile {} {
    global strFilename curFileTypeOpened cspstrFilename
    if {$strFilename != ""} {
            set curFileTypeOpened "Z"
            set cspstrFilename ""
            # puts "Opening Z file $strFilename"
            set rootName [file rootname $strFilename]
            set FuzzName {}
            append FuzzName $rootName ".fuzz"
            if [file exists $FuzzName] {
               file stat $strFilename mchStats
               file stat $FuzzName xmlStats
               if {$xmlStats(mtime) < $mchStats(mtime)} {
                 # recompilation needed
                 set recompileRequired 1
               } else {
                 # normally recompilation needed
                 if {$xmlStats(size)<1} {
                    # file was empty
                    set recompileRequired 1
                 } else {
                    set recompileRequired 0
                 }
               }
            } else {
              set recompileRequired 1
            }
            procShowSourceCode $strFilename
            procResetCodeModified

            if {$recompileRequired} {
              if [file exists $FuzzName] { file delete $FuzzName }
              global lib_dir
              if [file exists "$lib_dir/fuzzlib"] {
                   if {[prolog tools:host_platform(windows)]} {
                      set ok [CallFuzz "$lib_dir/fuzz.exe" "$lib_dir/fuzzlib" $FuzzName]
                   } else {
                      set ok [CallFuzz "$lib_dir/fuzz" "$lib_dir/fuzzlib" $FuzzName]
                   }
              } elseif [prolog preferences:get_preference(path_to_fuzz,PFuzzCmd)] {
	             set Cmd $prolog_variables(PFuzzCmd)
	             if {[prolog tools:host_platform(windows)]} {
	                  set fuzz_dir [file dirname $Cmd]
                      set ok [CallFuzz $Cmd $fuzz_dir/fuzzlib $FuzzName]
                 } else {
                      set ok [CallFuzzNoLib $Cmd $FuzzName]
                 }
	          } else {
                      set ok [CallFuzzNoLib "fuzz" $FuzzName]
              }
            } else {
              set ok 1
            }

            procDoZedSyntaxColouring .frmSource.text

            if {$ok && [file exists $FuzzName]} {
                # the file exists, so load it.
                set success [prolog tcltk_open_z_file('$FuzzName')]
                procShowErrors
                if $success {
                    procInitLoadedBMachine
                } else {
                    tkErrorBox "Loading fuzz file failed"
                }
            } elseif {$ok} {
                tkErrorBox "An error occurred while parsing $strFilename. Be sure to have fuzz 2000 installed."
            }
        procResetFilesMenu
        procEnableSourceCodeEditing
        procCheckForPlugins
    }
}

proc procLoadSmvFile {} {
    global strFilename curFileTypeOpened lib_dir cspstrFilename
    if {$strFilename != ""} {
        set curFileTypeOpened "SMV"
        set cspstrFilename ""
        set smvstrFilename {}
        append smvstrFilename $strFilename ".pl"
        if [file exists $smvstrFilename] { file delete $smvstrFilename }
        set MacJava6 "/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Commands/java"
        if [file exists $MacJava6] {set MyJava $MacJava6} else {set MyJava "java"}
        exec $MyJava -jar $lib_dir/smv2pterm.jar $strFilename $smvstrFilename

        procShowSourceCode $strFilename
        if [prolog tcltk_open_smv_file('$smvstrFilename')] {
            # the file exists, so load it.
        } else {
          tkErrorBox "Parsing of SMV file failed!\nConsult error messages."
        }
        procDoSmvSyntaxColouring  .frmSource.text
	    procFinishLoading
    }
}
proc procLoadPluginFile {id} {
    global strFilename curFileTypeOpened cspstrFilename
    if {$strFilename != ""} {
        set curFileTypeOpened $id
        set cspstrFilename ""
        set pfilename [escapeChars $strFilename]
        procShowSourceCode $strFilename
        if [prolog plugins:load_plugin_file('$id','$pfilename')] {
            # ok
        } else {
            tkErrorBox "Parsing of failed!\nConsult error messages."
        }
        procDoPluginSyntaxColouring $id .frmSource.text
        procFinishLoading
    }
}

proc procLoadPromelaFile {} {
    global strFilename curFileTypeOpened lib_dir app_dir cspstrFilename
    if {$strFilename != ""} {
        set curFileTypeOpened "Promela"
        set cspstrFilename ""
        set promelastrFilename {}
        append promelastrFilename $strFilename ".pl"
        if [file exists $promelastrFilename] { file delete $promelastrFilename }
        set MacJava6 "/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Commands/java"
        if [file exists $MacJava6] {set MyJava $MacJava6} else {set MyJava "java"}
        # puts "exec $MyJava -jar $lib_dir/Promela.jar $strFilename $app_dir"
        exec $MyJava -jar $lib_dir/Promela.jar $strFilename $app_dir

	    if [file exists $promelastrFilename] {
			procShowSourceCode $strFilename
			if [prolog tcltk_open_promela_file('$promelastrFilename')] {
				# the file exists, so load it.
			} else {
			  tkErrorBox "Loading of parsed Promela file failed!\nConsult error messages."
			}
			procDoPromelaSyntaxColouring  .frmSource.text
			procFinishLoading
	    } else {
		   tkErrorBox "Parsing of Promela file failed!\nNote: the parser requires Java6 or newer."
	       procShowErrors
	    }
    }
}

proc procLoadCSPMFile {} {
    global strFilename curFileTypeOpened cspstrFilename
    if {$strFilename != ""} {
        set curFileTypeOpened "CSP"
        set cspstrFilename ""

        procShowSourceCode $strFilename
        if [prolog tcltk_open_cspm_file('$strFilename')] {
            # the file exists, so load it.
        } else {
          tkErrorBox "Parsing of CSP-M file failed!\nConsult error messages."
        }
        procDoCSPSyntaxColouring  .frmSource.text
	    #procShowCSPMSyntaxErrors
	    procFinishLoading
    }
}

proc procFinishLoading {} {
    global show_error_if_no_transition
    set show_error_if_no_transition 1
	procResetCodeModified
	procResetFilesMenu
	procShowErrors
	# Show errors that occur during loading
	prologmnf tcltk_initialise
	procInsertHistoryOptionsState
	procEnableItemsAfterOpeningFile
	procEnableSourceCodeEditing
	procShowErrors
    procCheckForPlugins
}


proc procAddDefaultCSPFile {} {
    global cspstrFilename strFilename curFileTypeOpened
    if {$curFileTypeOpened!= "B" && $curFileTypeOpened!= "Z" && $curFileTypeOpened!= "EVENTB"} {
          tkErrorBox "You can only use a CSP Process to guide B or Z Machines.\nOpen a B Machine or Z Spec first."
    } else {
            procCheckIfSaved
			set cspstrFilename {}
			append cspstrFilename [file rootname $strFilename] ".csp"
			procAddCSPFileToBFile
    }
}
proc procAddCSPFile {} {
    global cspstrFilename curFileTypeOpened
    if {$curFileTypeOpened!= "B" && $curFileTypeOpened!= "Z"} {
          tkErrorBox "You can only use a CSP Process to guide B or Z Machines.\nOpen a B Machine or Z Spec first."
    } else {
        procCheckIfSaved
		set types {
			{"CSP Files"		{.csp .cspm}	}
			{"All files"		*}
		}
		# show the dialog box
		proc_getOpenCSPFile $types
		if {$cspstrFilename != ""} {
			procAddCSPFileToBFile
		}
    }
}
proc procAddCSPFileToBFile {} {
    global strFilename cspstrFilename curFileTypeOpened
    if {$cspstrFilename != "" && [file exists $cspstrFilename]} {
		wm title . "ProCSB: \[[file tail $strFilename]+[file tail $cspstrFilename]\]"
		procAddNewFileToFilesMenu $cspstrFilename "CSP-M"
		if [prolog tcltk_add_csp_file('$cspstrFilename')] {
			# the file exists, so load it.
		} else {
		  tkErrorBox "Parsing of CSP-M file failed!\nConsult error messages."
		}
		procShowErrors
		prologmnf tcltk_initialise
		procInsertHistoryOptionsState
    } else {
          tkErrorBox "CSP-M file does not exist:'$cspstrFilename'"
    }
}
proc proc_getOpenCSPFile { types } {
    global cspstrFilename
    # show the dialog box
    set machinesPath [getMachinesPath]
    if {$machinesPath != "" && [file isdirectory $machinesPath]} {
         set cspstrFilename [tk_getOpenFile -filetypes $types -initialdir $machinesPath -parent . ]
    } else {
         set cspstrFilename [tk_getOpenFile -filetypes $types -parent . ]
    }
    if {$cspstrFilename != ""} {
	#remember the directory for next time
	setMachinesPath [file dirname $cspstrFilename]
    }

}


proc procLoadBFile {} {
    global strFilename curFileTypeOpened currentRefSpecFile cspstrFilename version
    if [file exists $strFilename] {
       set curFileTypeOpened "B"
       set currentRefSpecFile ""
       set cspstrFilename ""
       procEnableReopen
       global version

	   prologmnf tcltk_clear_machine
	   procShowSourceCode $strFilename
	   procDoSyntaxColouring .frmSource.text
	   procResetCodeModified
	   procResetFilesMenu
	   procEnableSourceCodeEditing
	   procResetOptions "Opening..."
	   if [prolog tcltk_open_b_file('$strFilename')] {
		   set rootName [file rootname $strFilename]
		   procAddNewFileToFilesMenu $strFilename "B"
		   procShowErrors
		   procInitLoadedBMachine
	   } else {
	       # Syntax Error or other problem with main machine
	       procShowErrors
	       procClearOptionsStateHistory
	   }
       procCheckForPlugins
    } else {
	tkErrorBox "File $strFilename does not exist."
    }
}

proc procInitLoadedBMachine {} {
    global show_error_if_no_transition
    set show_error_if_no_transition 1
	prologmnf tcltk_initialise
	procShowErrors

	procInsertHistoryOptionsState
	procEnableItemsAfterOpeningFile
}








proc procRemoveHighlightExecutedOperation {} {
      .frmSource.text tag remove executedTag 1.0 end
      .frmSource.text tag remove executedauxTag 1.0 end
      .frmSource.text tag remove executedControlFlowTag 1.0 end
	  .frmSource.text tag configure executedTag -foreground darkgreen
	  .frmSource.text tag configure executedTag -background lightblue
	  .frmSource.text tag configure executedauxTag -foreground gray40
	  .frmSource.text tag configure executedauxTag -background gold
	  .frmSource.text tag configure executedControlFlowTag -foreground black
	  .frmSource.text tag configure executedControlFlowTag -background chartreuse3
}
proc procHighlightExecutedOperation {line col endline endcol} {
      if {[.frmSource.text compare "$endline.$endcol" >= end]} {
          puts "Highlight location $endline.$endcol out of bounds"
          # happens when wrong file is shown, we need location info to be file aware
      } else {
          # puts "Highlight line:$line, col:$col, endline:$endline, endcol:$endcol."
		  set loffset 0
		  set lChr [.frmSource.text get "$line.$col+$loffset chars"]
		  set roffset 1
		  set rChr [.frmSource.text get "$endline.$endcol - $roffset chars"]
		  # erase leading and trailing whitespace
		  while {$lChr != "" && [string is space $lChr]} {
			 incr loffset
			 set lChr [.frmSource.text get "$line.$col + $loffset chars"]
		  }
		  set start "$line.$col + $loffset chars"
		  while {$rChr != "" && [string is space $rChr]} {
			 incr roffset
			 set rChr [.frmSource.text get "$endline.$endcol - $roffset chars"]
		  }
		  set roffset [expr $roffset-1]
		  set endl "$endline.$endcol - $roffset chars"
		   # puts "NewleftChar = '$lChr' $start -> $endl '$rChr'"
		  if {($lChr == "\[") && ($endline > $line || $endcol>$col+2)} {
			# is text describing shared channels
			.frmSource.text tag add executedauxTag $start $endl
		  } elseif {$endline==$line && $col==$endcol-1 && ($lChr=="\[" || $lChr=="\]" || $lChr=="\|")} {
			# is a single control flow character from external, internal choice or interleave
			.frmSource.text tag add executedControlFlowTag $start $endl
		  } else {
			.frmSource.text tag add executedTag $start $endl
		  }
		  .frmSource.text see "$line.0"
      }
}

proc procRemoveHighlightSlice {} {
      .frmSource.text tag remove errcoltag 1.0 end
      .frmSource.text tag remove errtag 1.0 end
      .frmSource.text tag remove sliceTag 1.0 end
}

proc procRemoveHighlightSliceCEB {} {
      .frmSource.text tag remove errcoltag 1.0 end
      .frmSource.text tag remove errtag 1.0 end
      .frmSource.text tag remove sliceTagCEB 1.0 end
}

proc procHighlightSlice {line col endline endcol} {
	  .frmSource.text tag add sliceTag $line.$col $endline.$endcol
	  .frmSource.text tag configure sliceTag -foreground darkgreen
	  .frmSource.text tag configure sliceTag -background lightblue
      .frmSource.text see "$line.0"
}

proc procHighlightSliceCEB {line col endline endcol} {
	  .frmSource.text tag add sliceTagCEB $line.$col $endline.$endcol
          .frmSource.text tag configure sliceTagCEB -underline true
      .frmSource.text see "$line.0"
}

proc procHighlightLine {errlinenr} {
	  .frmSource.text tag add errtag $errlinenr.0 $errlinenr.end
	  .frmSource.text tag configure errtag -foreground darkred
	  .frmSource.text tag configure errtag -background yellow
}

proc procUnderlineColumn {errlinenr errcolnr} {
	  .frmSource.text tag add errcoltag $errlinenr.[expr $errcolnr-1] $errlinenr.$errcolnr
	  .frmSource.text tag configure errcoltag -underline true
	  .frmSource.text tag configure errcoltag -foreground darkblue
	  .frmSource.text tag configure errcoltag -background yellow
}

proc procUnderlineColumnFromTo {errlinenr errcolnr endline endcol} {
	  .frmSource.text tag add errcoltag $errlinenr.$errcolnr $endline.$endcol
	  .frmSource.text tag configure errcoltag -underline true
	  .frmSource.text tag configure errcoltag -foreground darkblue
	  .frmSource.text tag configure errcoltag -background yellow
}


proc procSyntaxColouringPreferences {} {
   procSetPreferences "syntax_highlighting"
   # Missing: check if changes were made !!
   procDoSyntaxColouring .frmSource.text
}

proc procDoSyntaxColouring {textWidget} {
  procUpdateSyntaxColours $textWidget
  # set lasti [$textWidget index end]
  # puts "last $lasti"
  if {[$textWidget compare "0.0 + 50000 chars" < end]} {
     prolog "tools:print_message('More than 50,000 chars')"
  }
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {
      # puts "marking comments"
	  procMarkRegExpressionPair $textWidget {/\*} {\*/} syntax_comment

      # puts "marking syntax_type"
	  set pattern {[0-9]*\s*\.\.\s*[0-9]*|(-->)|(-->>?)|(>->>?)|(\+->>?)|(>\+>>?)|<->|(?=\m)(POW(1?)|(scope_\w*)|(SET_PREF_\w*)|(FORCE_SYMMETRY_\w*)|FIN(1?)|perm|i?seq1?|BOOL|struct|STRING|INT(EGER)?|NAT(URAL)?1?)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_type
	  # puts "marking syntax_logical"
	  set pattern {&|(?=\m)(not|or|TRUE|FALSE|bool|GOAL|ANIMATION_IMG\w*|ANIMATION_STR\w*|ANIMATION_FUNCTION_DEFAULT|ANIMATION_FUNCTION|HEURISTIC_FUNCTION|ASSERT_LTL\w*)(?=\M)|<?=>|!|#}
	  procMarkRegExpression $textWidget $pattern syntax_logical

	  # puts "marking syntax_assignment"
	  set pattern {<--|:=|==?(?!>)|\|\||::}
	  procMarkRegExpression $textWidget $pattern syntax_assignment
	  # puts "marking syntax_operator 1"
	  # note: maplet |-> is not matched to avoid performance problem with large relations in properties,...
	  set pattern {\|->|>=|<=(?!>)|%|\*\*|~|/?<<?:|/(:|=)|<\+|><|^|<-(?!-|>)|<<?\||\|>>?|{}|{|}|\[|\]|\|\[\]|<>|^|(/\|?\\)|(\\\|?/)|\'|(?=\m)(SIGMA|PI|MININT|MAXINT|pred|succ|id|INTER|UNION|card|dom|ran|max|min|union|inter|size|mod|rev|conc|front|tail|first|last|rec|closure1|closure|iterate|prj(1|2))(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_operator
	  # puts "marking syntax_keyword"
	  set pattern {MACHINE(\s*\w\w*)?|END\s*\Z|(?=\m)(OPERATIONS|EVENTS|ASSERTIONS|INITIALI(S|Z)ATION|SEES|PROMOTES|USES|INCLUDES|IMPORTS|REFINES|EXTENDS|REFINEMENT|CSP_CONTROLLER|SYSTEM|MODEL|IMPLEMENTATION|INVARIANT|CONCRETE_VARIABLES|ABSTRACT_VARIABLES|VARIABLES|PROPERTIES|CONSTANTS|ABSTRACT_CONSTANTS|CONCRETE_CONSTANTS|CONSTRAINTS|SETS|DEFINITIONS)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_keyword
	  # puts "marking syntax_control_keyword"
	  set pattern {(?=\m)(skip|LET|BE|VAR|IN|ANY|WHILE|DO|VARIANT|ELSIF|IF|THEN|ELSE|EITHER|CASE|SELECT|ASSERT|WHEN|PRE|BEGIN|END|CHOICE|WHERE|OR|OF)(?=\M)|;}
	  procMarkRegExpression $textWidget $pattern syntax_control_keyword

	  #puts "marking syntax_unsupported"
	  set pattern {invariant_violated|(?=\m)(both_true_false|timeout(_false|_true)?|left|right|infix|arity|subtree|son|father|rank|mirror|sizet|postfix|prefix|sons|top|const|btree|tree|fnc|rel)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_unsupported
  }
}


proc procDoEventBSyntaxColouring {textWidget} {
  procUpdateSyntaxColours $textWidget
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {

	  procMarkRegExpressionPair $textWidget {/\*} {\*/} syntax_comment

	  set pattern {(?=\m)(event_b_model|event_b_context|sees|variables|invariant|refines|theorems|events|extends|constants|axioms|sets|deferred_set|event)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_keyword

	  set pattern {(?=\m)(integer|natural1_set|natural_set|couple)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_type

	  set pattern {(?=\m)(subset|member|not_member|set_subtraction|set_extension|range|domain|card|add|multiplication|div|interval|boolean_true|boolean_false)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_operator

	  set pattern {(?=\m)(conjunct|less_equal|greater|equal|not_equal|greater_equal|less)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_logical

	  set pattern {(?=\m)(assign|becomes_element_of)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_assignment

	  set pattern {(?=\m)(none|identifier)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_comment
  }
}

proc procDoXTLSyntaxColouring {textWidget} {
  procUpdateSyntaxColours $textWidget
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {

	  procMarkRegExpressionPair $textWidget {/\*} {\*/} syntax_comment

	  set pattern {(?=\m)(start|trans|prop)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_keyword

	  set pattern {(?=\m)(true|fail|atomic|nonvar|var|functor|op|is|ground|member|append|length)(?=\M)|=}
	  procMarkRegExpression $textWidget $pattern syntax_type

	  set pattern {:-|!|-->|;|\.}
	  procMarkRegExpression $textWidget $pattern syntax_assignment

	  set pattern {%(.*)}
	  procMarkRegExpression $textWidget $pattern syntax_comment
  }
}

proc procDoPluginSyntaxColouring {id textWidget} {
    procUpdateSyntaxColours $textWidget
    if [prolog preferences:get_preference(do_syntax_highlighting,true)] {
        if [prolog plugins:tcl_plugin_syntax_colouring($id,Rules)] {
            set rules $prolog_variables(Rules)
            foreach r $rules {
                 set type [lindex $r 0]
                if {$type == "pair"} {
                    set begin [lindex $r 1]
                    set end [lindex $r 2]
                    set colouring [lindex $r 3]
                    procMarkRegExpressionPair $textWidget $begin $end $colouring
                } elseif {$type == "expression"} {
                    set pattern [lindex $r 1]
                    set colouring [lindex $r 2]
                    procMarkRegExpression $textWidget $pattern $colouring
                }
            }
        }
    }
}

proc procDoZedSyntaxColouring {textWidget} {
  procUpdateSyntaxColours $textWidget
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {

	  procMarkRegExpressionPair $textWidget {/\*} {\*/} syntax_comment

	  set pattern {\\(\\|noindent|begin{document}|end{document}|usepackage{fuzz}|documentclass)}
	  procMarkRegExpression $textWidget $pattern syntax_comment

	  set pattern {(\\(begin{schema}|end{schema}|begin{zed}|end{zed}))|\\(?=\m)(where|Delta)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_keyword

	  set pattern {\\{|\\}|\\(notin|in|cup|cap|subseteq|setminus|emptyset)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_operator

	  set pattern {\\(power)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_type

	  set pattern {\\land(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_logical

	  set pattern {:=|=}
	  procMarkRegExpression $textWidget $pattern syntax_assignment

	  # set pattern {\\(begin{axdef}|end{axdef})}
	  # procMarkRegExpression $textWidget $pattern syntax_unsupported
  }
}

proc procDoCSPSyntaxColouring {textWidget} {
  procUpdateSyntaxColours $textWidget
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {
	  procMarkRegExpressionPair $textWidget {\{-} {-\}} syntax_comment

	  set pattern {;;|((?=\m)(agent|MAIN|channel|datatype|subtype|nametype|machine|Events)(?=\M))}
	  procMarkRegExpression $textWidget $pattern syntax_operator

	  set pattern {(!|\?|->|\[\]|\|~\||\|\|\||;|STOP|SKIP|CHAOS|/\\|\[>|@)}
	  procMarkRegExpression $textWidget $pattern syntax_type

	  set pattern {((?=\m)(if|then|else|@@|let|within)(?=\M))|\{|\}|<-|<->|\[\||\|\]|\[|\]|\\}
	  procMarkRegExpression $textWidget $pattern syntax_keyword

	  set pattern {((?=\m)(true|false|length|null|head|tail|concat|set|Set|Seq|elem|empty|card|member|union|diff|inter|Union|Inter|not|and|or|mod)(?=\M))|\*|\+|/|==|\!=|>|<|<=|>=|=<|&&|\|\|}
	  procMarkRegExpression $textWidget $pattern syntax_logical

	  set pattern {(?=\m)(assert|transparent|diamond)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_assignment

	  set pattern {--(.*)}
	  procMarkRegExpression $textWidget $pattern syntax_comment

	  set pattern {(?=\m)(external|extensions|productions)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_unsupported
  }
}

proc procDoSmvSyntaxColouring {textWidget} {
  procUpdateSyntaxColours $textWidget
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {

      procMarkRegExpressionPair $textWidget {/\*} {\*/} syntax_comment

	  set pattern {(?=\m)(boolean)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_type

	  set pattern {((?=\m)(MODULE|VAR|SPEC|INPUT|OUTPUT|ASSIGN|module)(?=\M))}
	  procMarkRegExpression $textWidget $pattern syntax_keyword

	  set pattern {(?=\m)(case|esac|next|init)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_assignment

	  set pattern {((?=\m)(full|nfull|true|empty|nempty)(?=\M))\!|&|\^|\?|\~}
	  procMarkRegExpression $textWidget $pattern syntax_logical

	  set pattern {--(.*)}
	  procMarkRegExpression $textWidget $pattern syntax_comment
  }
}

proc procDoPromelaSyntaxColouring {textWidget} {
  procUpdateSyntaxColours $textWidget
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {

      procMarkRegExpressionPair $textWidget {/\*} {\*/} syntax_comment

	  set pattern {\?|!|->|;|-(-)?|\+(\+)?|\*|::|\]|\[|((?=\m)(atomic|d_step|run|printf)(?=\M))}
	  procMarkRegExpression $textWidget $pattern syntax_operator

	  set pattern {(?=\m)(mtype|chan|active|proctype|of|byte|init|bool|int|typedef)(?=\M)|#define|\{|\}}
	  procMarkRegExpression $textWidget $pattern syntax_type

	  set pattern {((?=\m)(if|fi|else|goto|skip|do|od|for|rof|break|unless|provided)(?=\M))}
	  procMarkRegExpression $textWidget $pattern syntax_keyword

	  set pattern {(?=\m)(assert|timeout)(?=\M)|::|--|(=(?=\M))}
	  procMarkRegExpression $textWidget $pattern syntax_assignment

	  set pattern {((?=\m)(full|nfull|true|empty|nempty)(?=\M))|==|\!=|>|<|<=|>=|=<|&&|\|\|}
	  procMarkRegExpression $textWidget $pattern syntax_logical

	  set pattern {%(.*)}
	  procMarkRegExpression $textWidget $pattern syntax_comment
      # puts "Phase 6"

	  set pattern {(?=\m)(extern|inline)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_unsupported
  }
}

proc procDoSyntaxColouringForAnalyseInvariant {textWidget} {
  procDoSyntaxColouring $textWidget

  set pattern {(?=\m)(false|unknown|both_true_and_false|undefined)(?=\M)}
  procMarkRegExpression $textWidget $pattern syntax_unsupported

}

proc procUpdateSyntaxColours {textWidget} {
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {
      prolog preferences:get_preference(sh_type_colour,Colour)
	  set col $prolog_variables(Colour)
	  $textWidget tag configure syntax_type -foreground $col
      prolog preferences:get_preference(sh_logical_colour,Colour)
	  set col $prolog_variables(Colour)
	  $textWidget tag configure syntax_logical -foreground $col
      prolog preferences:get_preference(sh_assignments_colour,Colour)
	  set col $prolog_variables(Colour)
	  $textWidget tag configure syntax_assignment -foreground $col
      prolog preferences:get_preference(sh_operators,Colour)
	  set col $prolog_variables(Colour)
	  $textWidget tag configure syntax_operator -foreground $col
      prolog preferences:get_preference(sh_top_level_keywords,Colour)
	  set col $prolog_variables(Colour)
	  $textWidget tag configure syntax_keyword -foreground $col
      prolog preferences:get_preference(sh_control_keywords,Colour)
	  set col $prolog_variables(Colour)
	  $textWidget tag configure syntax_control_keyword -foreground $col
      prolog preferences:get_preference(sh_comments,Colour)
	  set col $prolog_variables(Colour)
	  $textWidget tag configure syntax_comment -foreground $col
      prolog preferences:get_preference(sh_unsupported_background,Colour)
	  set col $prolog_variables(Colour)
	  $textWidget tag configure syntax_unsupported -background $col
  } else {
	  $textWidget tag configure syntax_type -foreground black
	  $textWidget tag configure syntax_logical -foreground black
	  $textWidget tag configure syntax_assignment -foreground black
	  $textWidget tag configure syntax_operator -foreground black
	  $textWidget tag configure syntax_keyword -foreground black
	  $textWidget tag configure syntax_control_keyword -foreground black
	  $textWidget tag configure syntax_comment -foreground black
	  $textWidget tag configure syntax_unsupported -background white
  }

}

proc procMarkRegExpression {textWidget regpattern tag_to_mark} {
  global nrChars
  set nrMatches 0
  set nrChars 0
  set startClock [clock clicks -milliseconds]
  # puts "search ExpressionPair $regpattern"
  set start [$textWidget search -count nrChars -regexp -- $regpattern 1.0 end]
  # puts "searched $start count $nrChars"
  # search does not seem to do multiline matches !!!?
  # if we have more than 1000 matches: stop; performance will be bad
  while {($start != "") && ([clock clicks -milliseconds]-$startClock<1000)} {
    # puts "start:$start nrChars:$nrChars"
    $textWidget tag add $tag_to_mark $start "$start +$nrChars chars"
    incr nrChars
    if {[$textWidget compare "$start +$nrChars chars" < end]} {
      set start [$textWidget search -count nrChars -regexp -- $regpattern "$start +$nrChars chars" end]
    } else { set start "" }
  }
  if {$start!=""} {
     prolog "tools:print_message('Too much syntax colouring; bailing out')"
  }
}


proc procMarkRegExpressionPair {textWidget begregpattern endregpattern tag_to_mark} {
  global nrChars nrChars2
  set nrChars2 0
  set nrChars 0
  # puts "search ExpressionPair $begregpattern"
  set begstart [$textWidget search -count nrChars -regexp -- $begregpattern 1.0 end]
  # puts "searched $begstart count $nrChars"
  # search does not seem to do multiline matches !!!?
  while {$begstart != ""} {
    incr nrChars
    if {[$textWidget compare "$begstart +$nrChars chars" < end]} {
      set endstart [$textWidget search -count nrChars2 -regexp -- $endregpattern "$begstart +$nrChars chars" end]
      if {$endstart != ""} {
       # we found a pair
        $textWidget tag add $tag_to_mark $begstart "$endstart +$nrChars2 chars"
        if {[$textWidget compare "$endstart +$nrChars2 chars" < end]} {
           set begstart [$textWidget search -count nrChars -regexp -- $begregpattern "$endstart +$nrChars2 chars" end]
        } else {set begstart ""}
      } else {
        puts "Unclosed comment !"
        $textWidget tag add $tag_to_mark $begstart end
        set begstart ""
      }
    } else {
      set begstart ""
    }
  }
}

# --------------------------------
# SAVING TO FILE
# --------------------------------

proc procSaveAsFile {} {
    global strFilename curFileDisplayed
    if {$curFileDisplayed != $strFilename} {
	      tkErrorBox "Can only save main file $strFilename."
    } else {
		procCheckIfSaved
		set types {
			{"B Machine Files"		{.mch}	}
			{"B Refinements"		{.ref}	}
			{"B Implementations"	{.imp}	}
    	    {"Event-B System Files"		{.sys}	}
			{"All B Files"	{.mch .ref .imp .sys}	}
			{"All files"		*}
		}
		# show the dialog box
		set oldstrFilename $strFilename
		proc_getSaveFile $types
		if {$strFilename != ""} {
			set curFileDisplayed $strFilename
			procSaveFile
			procEnableItemsAfterOpeningFile
		} else {
			set strFilename $oldstrFilename
		}
    }
}
proc procSaveFile {} {
     # not yet fully tested !
    global strFilename curFileTypeOpened
    if {$curFileTypeOpened == "EVENTB"} {
       tkErrorBox "EventB Packaged can not be saved & edited from within ProB."
    } elseif {$strFilename != ""} {
		global curFileDisplayed
		if {$curFileDisplayed != $strFilename} {
		  # puts "Saving $curFileDisplayed (main file: $strFilename)"
		}
		set text [.frmSource.text get 1.0 "end - 1 chars"]
		#output to file here instead
		set fid [open $curFileDisplayed w]
		puts -nonewline  $fid $text
		# seems to add extra blank lines at the end ?!
		close $fid
		procResetCodeModified
    } else {
       tkErrorBox "No file open. Cannot save."
    }
}
proc procResetCodeModified {} {
    .frmMenu.mnuFile entryconfigure 3 -state disabled
    .frmMenu.mnuFile entryconfigure 4 -state disabled
    .frmSource.text edit modified 0
    procresetundo
}
proc procSetCodeModified {} {
    #global bModified
    #set bModified 1
    # puts "code modified"
    .frmMenu.mnuFile entryconfigure 3 -state normal
    .frmMenu.mnuFile entryconfigure 4 -state normal
    .frmSource.text edit modified 1
}
proc procCheckIfSaved {} {
    global curFileDisplayed curFileTypeOpened
    set bModified [.frmSource.text edit modified]
    if { $bModified && $curFileTypeOpened != "EVENTB"} {
	   set ans [tk_messageBox -default yes -message "Source File $curFileDisplayed is not saved.  Save changes to disk?" \
	            -title "Warning" -type yesno -icon warning]
	   #puts "ans: $ans"
	   if { $ans == "yes" } {
	     procSaveFile
	   }
    }
}
proc procSaveAndReopenFile {} {
  procSaveFile
  procReOpenFile
}

# ---------------------------------------------- reload
proc procReOpenFile {} {
    global strFilename curFileTypeOpened
    set ext [file extension $strFilename]
    if {$strFilename != ""} {
        procCheckIfSaved
         # Turn off undo. We do not want to be able to undo
         # the loading of a file

		if [file exists $strFilename] {
			.frmSource.text configure -undo 0
			if {$curFileTypeOpened == "B"} {
			   procLoadBFile
			} elseif {$curFileTypeOpened == "Z"} {
			   procLoadZFile
			} elseif {$curFileTypeOpened == "XTL"} {
			   procLoadXTLFile
			} elseif {$curFileTypeOpened == "CSP"} {
			   procLoadCSPMFile
			} elseif {$curFileTypeOpened == "Promela"} {
			   procLoadPromelaFile
			} elseif {$curFileTypeOpened == "EVENTB"} {
			   procLoadEventBPackage
            } elseif [prolog plugins:is_registered_plugin($curFileTypeOpened)] {
               procLoadPluginFile $curFileTypeOpened
			} else {
			   tkErrorBox "Unknown file type: $curFileTypeOpened"
			}
		} else {
		  tkErrorBox "File $strFilename no longer exists or has been moved."
		}
    } else {
      tkErrorBox "No file to open"
    }
}

# ---------------------------------------------


# -------
# procedure to reset the animation
# -------
proc procReset {} {
	    prologmnf  tcltk_reset
        procInsertHistoryOptionsState
}

# ----------------------------------
# procedure to do a random animation
# ----------------------------------
proc procRand {} {
    for {set i 1} {$i < 11} {incr i} {
     if [prolog tcltk_random_perform] {
       procInsertHistoryOptionsState
     } else {set i 11}   }
}
# ----------------------------------
proc procRandAny {} {
	global prompt Depth count done
	destroy .search
	set f .search
	if [Dialog_Create $f "Random Execution:" -borderwidth 10] {
		message $f.msg -text "Enter maximum nr of operations: " -aspect 1000
		entry $f.entry -textvariable prompt(result)
		set b [frame $f.buttons]
		pack $f.msg $f.entry $f.buttons -side top -fill x
		pack $f.entry -pady 5
		button $b.ok -text Check -command {set prompt(ok) 1}
		button $b.cancel -text Cancel \
			-command {set prompt(ok) 0}
		pack $b.ok -side left
		pack $b.cancel -side right

		label $f.msg2 -text "Waiting"
		pack $f.msg2 -side bottom -fill x

		bind $f.entry <Return> {set prompt(ok) 1 ; break}
		bind $f.entry <Control-c> {set prompt(ok) 0 ; break}
	} else {
	    set b $f.buttons
	    wm title $f "Searching:"
	    $f.msg configure -text "Enter maximum nr of operations: " -aspect 1000
	}
    $f.msg2 configure -text ""
    set done 0
    set prompt(ok) 0
    $b.ok configure -text Run
    $b.ok configure -state normal

	Dialog_Wait_Prepare $f prompt(ok) $f.entry

    while {$done != 1} {
	  $b.ok configure -state normal
	  set prompt(ok) 0

	  tkwait variable prompt(ok)

	 if {$prompt(ok)} {
		set Depth $prompt(result)
		set prompt(ok) 1
		$f.msg2 configure -text "Random... "
		set count 0
		set done 0
		$b.ok configure -state disabled

		random_step
		if {!($done)} {
		  tkwait variable done
		}
	  } else {
	    set done 1
	  }
	}
	Dialog_Wait_Finish $f
	Dialog_Dismiss $f
}

proc random_step {} {
	global prompt
	global count Depth done

    if {($prompt(ok)) && ($count < $Depth)} {
      .search.msg2 configure -text "Random... $count / $Depth"
      set count [expr $count+1]
      # puts $count

         if [prolog tcltk_random_perform] {
              procInsertHistoryOptionsState
           if [prolog tcltk_current_state_invariant_violated] {
              tkErrorBox "Invariant Violation found!"
              set done 1
           } else {
              after 1 random_step
           }
          } else {
          set count $Depth
          if [prolog {tcltk_get_status(INVVIOLATED,MAXREACHED,1)}] {
             tkErrorBox "Possible Deadlock found (timeout occurred)."
          } else {
             tkErrorBox "Deadlock found!"
          }
          set done 1
          }

    } else {
      if {!($done)} {
         set done 2
         if {$prompt(ok)} {
         .search.msg2 configure -text "$count operations executed"
         } else {
         .search.msg2 configure -text "User cancelled after $count"
         }
      }
      return
    }
}

# -------
# procedure to search for invalid states


# -------
# procedure to display a trace to current node
# -------

proc procExecTrace {} {
   prologmnf tcltk_execute_trace_to_current_node

   procInsertHistoryOptionsState
}

proc procFindTrace {} {
    global ok
    destroy .trace
    set f .trace
	if [Dialog_Create $f "Find Trace" -borderwidth 10] {
		message $f.msg -text "Trace to current node:" -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        scrollbar $f.frmSource.scrollx -command "$f.frmSource.text xview" -orient h
        text $f.frmSource.text -yscroll "$f.frmSource.scrolly set" -xscroll "$f.frmSource.scrollx set" \
         -setgrid 1 -height 18 -state disabled
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.scrollx -side bottom -fill x
        pack $f.frmSource.text -expand 1 -fill both

		set b [frame $f.buttons]
		pack $f.msg $f.frmSource $f.buttons -side top -fill x
		pack $f.frmSource -pady 5 -expand 1 -fill both
		button $b.ok -text Done -command {set ok 1}
		pack $b.ok -side right

		bind $f.frmSource <Return> {set ok 1 ; break}
		bind $f.frmSource <Control-c> {set ok 1 ; break}
	}

    prologmnf tcltk_find_shortest_trace_to_current_node(Res)

    # obtain a list of operations to execute to reach current node

	set Result $prolog_variables(Res)
    # puts "Result $Result"

    $f.frmSource.text configure -state normal
    $f.frmSource.text delete 0.0 end

    foreach i $Result {
       # puts "i $i"
       $f.frmSource.text insert end $i
       $f.frmSource.text insert end "\n"
    }

    # $f.frmSource.text configure -state disabled

    set ok 0
	Dialog_Wait $f ok $f.frmSource
	Dialog_Dismiss $f
}

# ---------------------------------------------------------------
# PREFERENCES
# ---------------------------------------------------------------


proc procSavePrefs {} {
  global app_dir
  if [file writable "$app_dir/ProB_Preferences.pl"] {
   prolog preferences:save_preferences('$app_dir/ProB_Preferences.pl')
  } else {
   # save in local directory
   prolog preferences:save_preferences('~/ProB_Preferences.pl')
  }
}

proc procLoadPreferences {} {
  global app_dir
  if [prolog tcltk_load_commandline_prefs_file] {
     # user specified prefs file
  } elseif [file writable "$app_dir/ProB_Preferences.pl"] {
     prolog preferences:load_preferences('$app_dir/ProB_Preferences.pl')
  } elseif [prolog file_systems:file_exists('~/ProB_Preferences.pl')] {
     prolog preferences:load_preferences('~/ProB_Preferences.pl')
  } else {
     prolog preferences:load_preferences('$app_dir/ProB_Preferences.pl')
  }
  procShowErrors
}

proc procSetPreferences {prefcategory} {
    global curprefcategory
    set curprefcategory $prefcategory
	prologmnf preferences:backup_preferences
    global ok
    destroy .trace
    set f .trace
	if [Dialog_Create $f "View/Edit Preferences" -borderwidth 10] {
		message $f.msg -text "List of Preferences" -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        listbox $f.frmSource.text -yscroll "$f.frmSource.scrolly set" \
         -setgrid 1 -height 10 -width 60 -bg white
        bind $f.frmSource.text <ButtonRelease-1> {procPerformPrefOption}
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.text -expand 1 -fill both

		set b [frame $f.buttons]
	    set c [frame $f.checks]
		pack $f.msg $f.frmSource $f.checks $f.buttons -side top -fill x
		pack $f.frmSource -pady 5 -expand 1 -fill both
		global lastiOption boolPrefVal
		set lastiOption 0
		set boolPrefVal 0
		radiobutton $c.bool -text True -variable boolPrefVal  -value 1 -state disabled
		radiobutton $c.bool2 -text False -variable boolPrefVal  -value 0 -state disabled
		message $c.valmsg -text "Value: " -aspect 1000
		global textPrefVal
		set textPrefVal 0
		entry $c.val -textvariable textPrefVal -width 40 -state disabled
		button $c.pick -text Pick -command {procPickValue} -state disabled
		pack $c.valmsg $c.val $c.bool $c.bool2 $c.pick -side left
		button $b.ok -text Finished -command {set ok 1; procPerformSetPrefOption}
		button $b.cancel -text Cancel -command {set ok 2}
		button $b.resetall -text "Reset all to defaults" -command {procResetPrefToDefaults}
		pack $b.ok $b.cancel $b.resetall -side right

		bind $f.frmSource <Return> {set ok 1 ; procPerformSetPrefOption; break}
		bind $f.frmSource <Control-c> {set ok 1 ; break}
	}

    prologmnf get_preferences_list($prefcategory,Res)
	set Result [split [string trimright $prolog_variables(Res) {;}] \;]
    foreach i $Result {
       $f.frmSource.text insert end $i
    }

    # $f.frmSource.text configure -state normal

    set ok 0
	Dialog_Wait $f ok $f.frmSource
	Dialog_Dismiss $f
	if {$ok==1} {
	  global app_dir
	  procSavePrefs
	} else {
	  prologmnf preferences:revert_preferences
	}
	# procInsertHistoryOptionsState
}
proc procResetPrefToDefaults {} {
    global curprefcategory
    procPerformSetPrefOption
    prologmnf reset_to_defaults($curprefcategory)
    procPerformGetPrefOption
}
proc procPerformPrefOption {} {
    global curprefcategory
    procPerformSetPrefOption
    procPerformGetPrefOption
}
proc procPickValue {} {
    global curprefcategory
    global lastiOption textPrefVal boolPrefVal
    if {$lastiOption > 0} {
        prologmnf get_ith_preference_type($curprefcategory,$lastiOption,Type)
        prologmnf get_ith_preference_value($curprefcategory,$lastiOption,Val)
        set curtype $prolog_variables(Type)
        set curval $prolog_variables(Val)
        # puts "curtype $curtype"
        if {$curtype == "rgb_color"} {
            set colres [tk_chooseColor -initialcolor $curval -parent .trace]
            if {$colres != ""} {
              # puts "color: $colres"
              set textPrefVal $colres
            }
        } elseif {$curtype == "path"} {
            set pathres [tk_getOpenFile -parent .trace]
            if {$pathres != ""} {
              # puts "color: $colres"
              set textPrefVal $pathres
            }
        } else {
          tkErrorBox "You cannot pick for this type"
        }
    }
}
proc procPerformGetPrefOption {} {
    global curprefcategory
    global lastiOption
    set iOption [.trace.frmSource.text curselection]
    if {$iOption != ""} {
        incr iOption
        prologmnf get_ith_preference_value($curprefcategory,$iOption,Val)
        set lastiOption $iOption
        global textPrefVal
        global boolPrefVal
        set textPrefVal $prolog_variables(Val)
        if [prolog get_ith_preference_type($curprefcategory,$iOption,bool)] {
           if {$textPrefVal=="true"} {set boolPrefVal 1} else {set boolPrefVal 0}
            # set boolPrefVal [expr $textPrefVal=="true"]
           .trace.checks.val configure -state disabled
           .trace.checks.bool configure -state normal
           .trace.checks.bool2 configure -state normal
           .trace.checks.pick configure -state disabled

        } else {
            set boolPrefVal 2
           .trace.checks.val configure -state normal
           .trace.checks.bool configure -state disabled
           .trace.checks.bool2 configure -state disabled
            if [prolog get_ith_preference_type($curprefcategory,$iOption,rgb_color)] {
               .trace.checks.pick configure -state normal
            } elseif [prolog get_ith_preference_type($curprefcategory,$iOption,path)] {
               .trace.checks.pick configure -state normal
            } else {
               .trace.checks.pick configure -state disabled
           }
        }
    }
}
proc procPerformSetPrefOption {} {
    global curprefcategory
    global lastiOption textPrefVal boolPrefVal
    if {$lastiOption>0} {
        prologmnf get_ith_preference_type($curprefcategory,$lastiOption,PType)
        set preftype $prolog_variables(PType)
        if {$preftype=="bool"} {
           if {$boolPrefVal == 1} {
              prologmnf set_ith_preference_value($curprefcategory,$lastiOption,true)
            } else {
              prologmnf set_ith_preference_value($curprefcategory,$lastiOption,false)
            }
        } elseif {$preftype=="string" || $preftype=="rgb_color" || $preftype=="path"} {
           prolog set_ith_preference_value($curprefcategory,$lastiOption,'$textPrefVal')
        } else {
           if {![prolog set_ith_preference_value($curprefcategory,$lastiOption,$textPrefVal)]} {
               tkErrorBoxNoParent "Could not set $curprefcategory preference to $textPrefVal"
           }
        }
   }
}

# -------
# procedure to display coverage of operation information
# -------

proc procComputeCoverage0 {} {
  procComputeCoverage 0
}
proc procComputeCoverage {full} {
    if {$full} {
        prologmnf tcltk_compute_coverage_and_enabling(Res)
    } else {
        prologmnf tcltk_compute_coverage(Res)
    }
    # obtain a list of conjuncts with their status

	set Result $prolog_variables(Res)
	global batch_mode
	if {$batch_mode} {
	   puts "Coverage Statistics:\n$Result"
	} else {
	   procDisplayCoverage $Result
	}
}
proc procDisplayCoverage {Result} {
    global ok
	destroy .coverage
    set f .coverage
	if [Dialog_Create $f "Analysing Coverage" -borderwidth 10] {
		message $f.msg -text "Coverage for operations:" -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        scrollbar $f.frmSource.scrollx -command "$f.frmSource.text xview" -orient h
        text $f.frmSource.text -yscroll "$f.frmSource.scrolly set" -xscroll "$f.frmSource.scrollx set" \
         -setgrid 1 -height 18 -state disabled
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.scrollx -side bottom -fill x
        pack $f.frmSource.text -expand 1 -fill both

		set b [frame $f.buttons]
		pack $f.msg $f.frmSource $f.buttons -side top -fill x
		pack $f.frmSource -pady 5 -expand 1 -fill both
		button $b.ok -text Done -command {set ok 1}
		pack $b.ok -side right

		bind $f.frmSource <Return> {set ok 1 ; break}
		bind $f.frmSource <Control-c> {set ok 1 ; break}
	}



    $f.frmSource.text configure -state normal
    $f.frmSource.text delete 0.0 end

    foreach i $Result {
       $f.frmSource.text insert end $i
       $f.frmSource.text insert end "\n"
    }

    # $f.frmSource.text configure -state disabled

    set ok 0
	Dialog_Wait $f ok $f.frmSource
	Dialog_Dismiss $f
}


# -------
# procedure to analyse the invariant in the current state
# -------
proc procDebugOperation {} {
   global OpName
   set OpName [procChooseOperationOrINITIALISATION]
   if {$OpName != ""} {
	  procDebugProperties1 $OpName 0
   }
}

proc procChooseOperationOrINITIALISATION {} {
   if {[prolog current_state_corresponds_to_initialised_b_machine]} {
      return [procChooseOperation]
   } elseif {[prolog current_state_corresponds_to_setup_constants_b_machine]} {
      return {@INITIALISATION}
   } else {
      return {@PROPERTIES}
   }
}

proc procVisualiseOperationPre {} {
   global OpName strFilename
    if {$strFilename != ""} {
	   # set OpName [Dialog_Prompt "Enter operation name"]
	   set OpName [procChooseOperationOrINITIALISATION]
	   if {$OpName != ""} {
		   procVisualiseInvariantOrOpPRE $OpName
		   procShowErrors
	   }
    } else {
       tkErrorBox "No File open. Cannot visualise operation enabling condition."
    }
}


proc procChooseOperation {} {
    return [procChooseOperation2 "no" "" "var" "true"]
}
proc procChooseOperation2 {ChkBox ChkBoxMsg ChkBoxVar FilterPred} {
    prologmnf findall(Name,b_top_level_operation(Name),LN)

    return [procChooseElementFromList $prolog_variables(LN) "Operations" $ChkBox $ChkBoxMsg $ChkBoxVar $FilterPred]
}
proc procChooseElementFromList {List ElName ChkBox ChkBoxMsg ChkBoxVar FilterPred} {
    global ok
    destroy .trace
    set f .trace
    prologmnf findall(Name,b_top_level_operation(Name),LN)

	if [Dialog_Create $f "Choose $ElName" -borderwidth 10] {
		message $f.msg -text "List of $ElName" -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        listbox $f.frmSource.text -yscroll "$f.frmSource.scrolly set" \
         -setgrid 1 -height 9 -width 40 -bg white
        bind $f.frmSource.text <Double-1> {set ok 1}
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.text -expand 1 -fill both

		set b [frame $f.buttons]
		pack $f.msg $f.frmSource $f.buttons -side top -fill x
		pack $f.frmSource -pady 5 -expand 1 -fill both

		if {$ChkBox == "yes"} {
		  global $ChkBoxVar
		  checkbutton $f.mychckbox -text "$ChkBoxMsg" -variable $ChkBoxVar \
		    -offvalue 0 -onvalue 1
		  pack $f.mychckbox -side top
		}
		button $b.ok -text Select -command {set ok 1}
		button $b.cancel -text Cancel -command {set ok 2}
		pack $b.ok $b.cancel -side right

		bind $f.frmSource <Return> {set ok 1 ; break}
		bind $f.frmSource <Control-c> {set ok 2 ; break}
	}

	foreach i $List {
	   if {$FilterPred=="true" || [prolog "'$FilterPred'($i)"]} {
		 $f.frmSource.text insert end $i
	   } else {
		 $f.frmSource.text insert end "($i : not covered)"
	   }
	}

    # $f.frmSource.text configure -state normal

    set ok 0
	Dialog_Wait $f ok $f.frmSource
	Dialog_Dismiss $f
	set sel [.trace.frmSource.text curselection]
	if {$ok==1} {
	  set res [lindex $List $sel]
	  # puts "Selection $sel : $res"
	  return $res
	} else {
	  return ""
	}
}


proc procExecuteOperation {} {
   global OpName
   # set OpName [Dialog_Prompt "Enter operation name"]

   if {[prolog b_machine_has_constants_or_properties] && [prolog current_expression(root,_,_)]} {
       set OpName {$setup_constants}
   } else {
       set OpName [procChooseOperationOrINITIALISATION]
   }
   if {$OpName != ""} {
        puts "Getting op paras"
		prolog b_get_machine_operation_parameter_names('$OpName',LP)
        puts "$prolog_variables(LP)"
		procExecuteSpecificOperation $prolog_variables(LP)
   }
}

proc procExecuteSpecificOperation {Parameters} {
       global OpName
       global ok
		destroy .trace
		set f .trace
		if [Dialog_Create $f "Enter Parameters for $OpName" -borderwidth 10] {
			message $f.msg -text "Parameters for $OpName" -aspect 1000
			# ------- code source frames
			frame $f.frmVals -borderwidth .1c -relief groove

			set b [frame $f.buttons]
			pack $f.msg $f.frmVals $b -side top -fill x

			button $b.ok -text Execute -command {set ok 1}
			button $b.cancel -text Cancel -command {set ok 2}
			pack $b.ok $b.cancel -side right

			bind $f.frmVals <Return> {set ok 1 ; break}
			bind $f.frmVals <Control-c> {set ok 2 ; break}

			foreach i $Parameters {
			     global exopvars
				 message $f.frmVals.valmsg$i -text "$i:" -aspect 1000
				 entry $f.frmVals.val$i -textvariable exopvars(textVal$i) -width 40 -state normal
				 pack $f.frmVals.valmsg$i $f.frmVals.val$i -side top
			}
			global precondval
			message $f.frmVals.premsg -text "Additional PRE:" -aspect 1000
			entry $f.frmVals.preval -textvariable precondval -width 40 -state normal
			pack $f.frmVals.premsg $f.frmVals.preval -side top
		}


		# $f.frmVals.text configure -state normal

		set ok 0
		Dialog_Wait $f ok $f.frmVals
		Dialog_Dismiss $f
		if {$ok==1} {
		  # puts "Executing $OpName"
		  if {$precondval != ""} {
		      set PreCond $precondval
		  } else {
		      set PreCond "(TRUE=TRUE)"
		  }
		  foreach i $Parameters {
			     set val $exopvars(textVal$i)
			     # puts "Parameter $i = $val"
			     if {$val != ""} {
			        if {$PreCond == "(TRUE=TRUE)"} {
			           set PreCond "($i = $val)"
			        } else {
			           append PreCond "& ($i = $val)"
			        }
			     }
		  }
		  puts "PreCond2 $PreCond"

		 if [prolog tcltk_add_user_executed_operation('$OpName','$PreCond')] {
			procShowErrors
			procInsertHistoryOptionsState
		 } else {
		    procShowErrors
			tkErrorBox "Could not execute operation $OpName\nExtra Pre-Condition: $PreCond."
		 }
		}
}

proc prologmnf {Query} {
   return
   # checks that a prolog query does not fail (mnf)
   set PQ "prolog $Query"
   # puts "exec prolog $Query"
   if [uplevel 1 $PQ] {
      # puts "done prolog $Query"
      return 1
   } else {
      tkErrorBox "Prolog Query unexpectedly failed: $Query"
      return 0
   }
}


proc procCheckDebugProperties {} {
      global batch_mode
      set saved_mode $batch_mode
      set batch_mode 1
      procDebugProperties
      set batch_mode $saved_mode
}
proc procDebugProperties {} {
   procDebugProperties1 "@PROPERTIES" 0
}
proc procDebugInitialisation {} {
   procDebugProperties1 "@INITIALISATION" 0
}
proc procDebugProperties1 {OpName UnsatCore} {
    puts "procDebugProperties $OpName"
    set ExtraBtn "Minimize..."
    set UnsatCoreFile "Debug"
    if {$UnsatCore} {
	   set ans [tk_messageBox -default no \
	            -message "Precise minimisation of unsatisfiable predicates (may take a long time)?" \
	            -title "Minmise Unsatisfiable Predicates" -type yesnocancel -icon question -parent .]
	   if { $ans == "yes" } {
	      set Unsat "full"
          set UnsatCoreFile "UnsatCore"
          # set ExtraBtn ""
	   } elseif { $ans == "no" } {
	      set Unsat "fast"
          set UnsatCoreFile "FastUnsatCore"
	   } else {
	      return
	   }
    } else {
       set Unsat "false"
    }
    if [prolog tcltk_debug_properties_or_op('$OpName',$Unsat,Res,Satisfiable)] {
        if {$prolog_variables(Satisfiable)==false} {
           assertBatchError "Property Unsatisfiable"
        } else {
           set ExtraBtn "none"
        }
		set Result [split [string trimright $prolog_variables(Res) {;}] \;]
		procShowErrors
		if {$OpName == "@PROPERTIES"} {
		   set Msg1 "Constraints and Properties"
		   set Msg2 "the CONSTRAINTS and PROPERTIES clauses"
		} else {
		   set Msg1 "$OpName Enabling Condition"
		   set Msg2 "the Operation $OpName"
		}
		set Cmd "procDebugProperties1 $OpName 1"
		procShowList3 $Result "Debugging $Msg1" "Debugging $Msg2:" 1 0 $ExtraBtn $Cmd "$UnsatCoreFile.txt"
    }
}

proc procCheckNumber {Msg Expected Real} {
	if {$Real != $Expected && $Expected>=0} {
	   tkErrorBox "Unexpected Number of $Msg.\nExpected: $Expected\nGot: $Real"
	}
}

proc procAnalysePred {TYPE} {
   procAnalysePredCheck $TYPE -1 -1 -1
}

proc procAnalysePredCheck {TYPE ExpTot ExpFal ExpUnk} {
    prolog tcltk_analyse_option($TYPE,Desc)
    if [prologmnf tcltk_analyse($TYPE,Res,Total,False,Unknown)] {
		set Result [split [string trimright $prolog_variables(Res) {;}] \;]
		# puts "Showing results"
		procShowErrors
		set Tot $prolog_variables(Total)
		procCheckNumber "conjuncts" $ExpTot $Tot
		set Fal $prolog_variables(False)
		procCheckNumber "false conjuncts" $ExpFal $Fal
		set Unk $prolog_variables(Unknown)
		procCheckNumber "unknown conjuncts" $ExpUnk $Unk
		procShowList2 "$Result ------ SUMMARY ------ Analysed:$Tot False:$Fal Unknown:$Unk" "Analysing $prolog_variables(Desc)" "(Non-Typing) Conjuncts of $prolog_variables(Desc):" 1 0 "PredicateAnalysis.txt"
    }
}
proc procAnalyseInvariant {} {
    procAnalysePred invariant
}
proc procAnalyseProperties {} {
    procAnalysePred properties
}
proc procAnalyseAssertions {} {
    procAnalysePred assertions
}

proc procAnalyseDEFGOAL {} {
  if [prolog b_reset_machine_goal_from_DEFINITIONS] {
     procAnalyseGOAL
  } else {
       tkErrorBox "Could not obtain GOAL predicate from DEFINITIONS."
  }
}
proc procAnalyseGOAL {} {
    if [prologmnf tcltk_analyse_goal(Res)] {
        procShowErrors
		# obtain a list of conjuncts with their status
		set Result [split [string trimright $prolog_variables(Res) {;}] \;]
		procShowList2 $Result "Analysing GOAL" "Conjunct of the GOAL:" 1 0 "Goal_Analysis.txt"
    }
}



proc procShowTyping {} {
    if [prologmnf tcltk_show_typing_of_variables_and_constants(Res)] {
        procShowErrors
		set Result [split [string trimright $prolog_variables(Res) {;}] \;]
		procShowList2 $Result "Analysing Typing" "Typing of Constants and Variables:" 1 0 "Typing.txt"
    }
}

proc procShowCurrentState {} {
  if [prolog tcltk_show_current_state(TB)] {
     puts "current: $prolog_variables(TB)"
	set Result [split $prolog_variables(TB) \n]
     procShowList "$Result" "Current B State" "Current B State"
  } else {
         tkErrorBox "Please set up constants first"
  }
}

proc procShowList {Result WindowTitleMsg1 Msg2} {
    procShowList2 $Result $WindowTitleMsg1 $Msg2 0 0 "ProB_Output.txt"
}

proc procShowList2 {Result WindowTitleMsg1 Msg2 SyntaxHighlight BugReportButton DefaultSaveName} {
  procShowList3 $Result $WindowTitleMsg1 $Msg2 $SyntaxHighlight $BugReportButton none none $DefaultSaveName
}
proc procShowList3 {Result WindowTitleMsg1 Msg2 SyntaxHighlight BugReportButton ExtraButtonName ExtraCmd DefaultSaveName} {
  global batch_mode testing_mode
  if {$batch_mode || $testing_mode==true} {
     puts "$WindowTitleMsg1\n$Msg2\n\n$Result"
  } else {
     global strFilename
	 set rn [file rootname [file tail $strFilename]]
	 set us "_"
     procShowList4 $Result $WindowTitleMsg1 $Msg2 $SyntaxHighlight $BugReportButton $ExtraButtonName $ExtraCmd "$rn$us$DefaultSaveName"
  }
}
proc procShowList4 {Result WindowTitleMsg1 Msg2 SyntaxHighlight BugReportButton ExtraButtonName ExtraCmd DefaultSaveName} {
    global ok
	destroy .invariant
	# destroy added for Mac TclTk
    set f .invariant
	if [Dialog_Create $f $WindowTitleMsg1 -borderwidth 10] {
		message $f.msg -text $Msg2 -aspect 1000
		global tcl_dir
        # image create photo checkKO -format gif -fil "$tcl_dir/icons/CheckOK.gif"
        # label $f.okko -text OKKO -image checkKO

        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        scrollbar $f.frmSource.scrollx -command "$f.frmSource.text xview" -orient h
        text $f.frmSource.text -yscroll "$f.frmSource.scrolly set" -xscroll "$f.frmSource.scrollx set" \
         -setgrid 1 -height 18 -state disabled
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.scrollx -side bottom -fill x
        pack $f.frmSource.text -expand 1 -fill both

		set b [frame $f.buttons]
		pack $f.msg $f.frmSource $f.buttons -side top -fill x
		pack $f.frmSource -pady 5 -expand 1 -fill both
		if {$BugReportButton} {
		  button $b.bug -text "Report as Bug..." -command {set ok 2}
		  pack $b.bug -side left
		}
		if {$ExtraButtonName!="none"} {
		  button $b.extra -text "$ExtraButtonName" -command {set ok 4}
		  pack $b.extra -side left
		}
		button $b.save -text "Save..." -command {set ok 3}
		pack $b.save -side left
		button $b.ok -text Done -command {set ok 1}
		pack $b.ok -side right

		bind $f.frmSource <Return> {set ok 1 ; break}
		bind $f.frmSource <Control-c> {set ok 1 ; break}
	}

    $f.frmSource.text configure -state normal
    $f.frmSource.text delete 0.0 end
    foreach i $Result {
       $f.frmSource.text insert end $i
       $f.frmSource.text insert end "\n"
    }
    if {$SyntaxHighlight} {
       procDoSyntaxColouringForAnalyseInvariant $f.frmSource.text
    }
    # puts "Finished Syntaxhighlighting"
    # $f.frmSource.text configure -state disabled
    # keep normal instead of disabled: this allows users to select and copy the list

    set ok 0
	Dialog_Wait $f ok $f.frmSource
	Dialog_Dismiss $f
	if {$BugReportButton && $ok==2} {
	   procReportDBug "Errors:$Result"
	} elseif {$ok==3} {
	    set types {
	        {"Text File" {.txt} }
			{"All files"		*}
		}
	    set DEBUGFILE [tk_getSaveFile -filetypes $types -initialfile $DefaultSaveName -parent . ]
		if {$DEBUGFILE != ""} {
		   # puts "Opening File: $DEBUGFILE"
		   set channel [open "$DEBUGFILE" "w+"]
			foreach i $Result {
			   puts $channel $i
			}
		   close $channel
       }
	} elseif {$ok==4} {
	   eval $ExtraCmd
	   procShowErrors
	}
}

proc procShowText {Result Msg1} {
    global ok
	destroy .invariant
	# destroy added for Mac TclTk
    set f .invariant
	if [Dialog_Create $f $Msg1 -borderwidth 10] {
		#message $f.msg -text $Msg2 -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        scrollbar $f.frmSource.scrollx -command "$f.frmSource.text xview" -orient h
        text $f.frmSource.text -yscroll "$f.frmSource.scrolly set" -xscroll "$f.frmSource.scrollx set" \
         -setgrid 1 -height 18 -state disabled
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.scrollx -side bottom -fill x
        pack $f.frmSource.text -expand 1 -fill both

		set b [frame $f.buttons]
		pack $f.frmSource $f.buttons -side top -fill x
		pack $f.frmSource -pady 5 -expand 1 -fill both
		button $b.save -text "Save..." -command {set ok 3}
		pack $b.save -side left
		button $b.ok -text Done -command {set ok 1}
		pack $b.ok -side right

		bind $f.frmSource <Return> {set ok 1 ; break}
		bind $f.frmSource <Control-c> {set ok 1 ; break}
	}


    procShowSourceCodeFromCodes $f.frmSource.text $Result
    $f.frmSource.text configure -state normal

    set ok 0
	Dialog_Wait $f ok $f.frmSource

	if {$ok==3} {
	    set types {
	        {"Text File" {.txt} }
			{"All files"		*}
		}
	    set DEBUGFILE [tk_getSaveFile -filetypes $types -initialfile "ProB_Output.txt" -parent . ]
		if {$DEBUGFILE != ""} {
		    puts "Opening File: $DEBUGFILE"
		   set channel [open "$DEBUGFILE" "w+"]
			foreach i $Result {
			   puts -nonewline $channel [format %c $i]
			}
		   close $channel
       }
	}

	Dialog_Dismiss $f
}

proc procShowLogFile {sFileName Msg1 Msg2} {
    global ok
	destroy .invariant
	# destroy added for Mac TclTk
    set f .invariant
	if [Dialog_Create $f $Msg1 -borderwidth 10] {
		message $f.msg -text $Msg2 -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        scrollbar $f.frmSource.scrollx -command "$f.frmSource.text xview" -orient h
        text $f.frmSource.text -yscroll "$f.frmSource.scrolly set" -xscroll "$f.frmSource.scrollx set" \
         -setgrid 1 -height 18 -state disabled
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.scrollx -side bottom -fill x
        pack $f.frmSource.text -expand 1 -fill both

		set b [frame $f.buttons]
		pack $f.msg $f.frmSource $f.buttons -side top -fill x
		pack $f.frmSource -pady 5 -expand 1 -fill both
		button $b.ok -text Done -command {set ok 1}
		pack $b.ok -side right

		bind $f.frmSource <Return> {set ok 1 ; break}
		bind $f.frmSource <Control-c> {set ok 1 ; break}
	}


    $f.frmSource.text configure -state normal
    $f.frmSource.text delete 0.0 end

    set fid [open $sFileName r]
    $f.frmSource.text insert 0.0 [read $fid]
    close $fid
    # $f.frmSource.text configure -state disabled

    set ok 0
	Dialog_Wait $f ok $f.frmSource
	Dialog_Dismiss $f
}

proc procAbsInt {} {
   if [prolog animation_mode(b)] {
       prolog popup(X)
       set res $prolog_variables(X)
       procShowList $res "test1" "test2"
   } else {
       tkErrorBoxNoParent "Can only be applied to B specifications."
   }
}
proc procShowSpecializedInvariants {} {
   if [prolog animation_mode(b)] {
	   global OpName
	   set OpName [procChooseOperation]
	   if {$OpName != ""} {
		   prolog bmachine:tcltk_get_specialized_invariand_for_op($OpName,X)
		   set res $prolog_variables(X)
		   procShowList $res "Specialized Invariant" "Specialized Invariant"
       }
   } else {
       tkErrorBoxNoParent "Can only be applied to B specifications."
   }
}


proc procVisualizeSliceGraph {slicetype} {
  global app_dir
  set rootName "$app_dir/src/cia/$slicetype"
  # we should change the location of those files and generate them on demand, as for visited states
  append dotName $rootName ".dot"
  append psName $rootName ".ps"
  procVisualiseDotFile $dotName $psName
}

proc procSliceProgramPoint {} {
    global strFilename
    if [prolog animation_mode(cspm)] {
           procRemoveHighlightSlice
           procRemoveHighlightSliceCEB
	   set cursel [.frmSource.text tag ranges sel]
	   puts "Selection: $cursel"
	   if {[llength $cursel]==2} {
		  set from [lindex $cursel 0]
		  set to [lindex $cursel end]
		  set fl [split [lindex $cursel 0] {.}]
		  set from_line [lindex $fl 0]
		  set from_col [lindex $fl end]
		  set tl [split [lindex $cursel end] {.}]
		  set to_line [lindex $tl 0]
		  set to_col [lindex $tl end]
		  if [prolog slicer_csp:slice_from_program_point($from_line,$from_col,$to_line,$to_col,'$strFilename',X,Y,XE,YE,XCEB,YCEB,XECEB,YECEB,Reachable)] {
                  set LineCEB $prolog_variables(XCEB)
                  set ColCEB $prolog_variables(YCEB)
                  set EndLineCEB $prolog_variables(XECEB)
                  set EndColCEB $prolog_variables(YECEB)
                  set iNbElemCEB [llength $LineCEB]
		  set Line $prolog_variables(X)
                  set Col $prolog_variables(Y)
                  set EndLine $prolog_variables(XE)
                  set EndCol $prolog_variables(YE)
                  set iNbElem [llength $Line]
                  set ReachableTCL $prolog_variables(Reachable)
                  if {$ReachableTCL == false} {tkMessageBox "Selected symbol is not reachable."}
                  #else {
	                  for {set iCEB 0} {$iCEB < $iNbElemCEB} {incr iCEB} {
	               		procHighlightSliceCEB [lindex $LineCEB $iCEB] [lindex $ColCEB $iCEB] [lindex $EndLineCEB $iCEB] [lindex $EndColCEB $iCEB]
	            	  }
	                  for {set i 0} {$i < $iNbElem} {incr i} {
	               		procHighlightSlice [lindex $Line $i] [lindex $Col $i] [lindex $EndLine $i] [lindex $EndCol $i]
	            	  }
                  #}
                  # procHighlightSlice $Line $Col $EndLine $EndCol
          }
		  procShowErrors
	   } else {
		   tkMessageBox "Please first select an event inside a CSP Process inside the source text."
	   }
   } else {
       tkErrorBox "This command can only be used in CSPM mode."
   }
}

proc procSliceExecutableMEB {} {
    global strFilename
    if [prolog animation_mode(cspm)] {
	   set cursel [.frmSource.text tag ranges sel]
	   puts "Selection: $cursel"
	   if {[llength $cursel]==2} {
		  set from [lindex $cursel 0]
		  set to [lindex $cursel end]
		  set fl [split [lindex $cursel 0] {.}]
		  set from_line [lindex $fl 0]
		  set from_col [lindex $fl end]
		  set tl [split [lindex $cursel end] {.}]
		  set to_line [lindex $tl 0]
		  set to_col [lindex $tl end]
		  if [prolog slicer_csp:slice_from_program_point_executableMEB($from_line,$from_col,$to_line,$to_col,'$strFilename',NewF)] {
			  procShowErrors
			  procExecutePathPrefOnFile path_to_text_editor emacs {External Text Editor} Advanced $prolog_variables(NewF)
          } else {
		      tkErrorBox "Internal error: Slicing failed"
              procShowErrors
          }

	   } else {
		   tkMessageBox "Please first select an event inside a CSP Process inside the source text."
	   }
   } else {
       tkErrorBox "This command can only be used in CSPM mode."
   }
}

proc procSliceExecutableCEB {} {
    global strFilename
    if [prolog animation_mode(cspm)] {
	   set cursel [.frmSource.text tag ranges sel]
	   puts "Selection: $cursel"
	   if {[llength $cursel]==2} {
		  set from [lindex $cursel 0]
		  set to [lindex $cursel end]
		  set fl [split [lindex $cursel 0] {.}]
		  set from_line [lindex $fl 0]
		  set from_col [lindex $fl end]
		  set tl [split [lindex $cursel end] {.}]
		  set to_line [lindex $tl 0]
		  set to_col [lindex $tl end]
		  if [prolog slicer_csp:slice_from_program_point_executableCEB($from_line,$from_col,$to_line,$to_col,'$strFilename',NewF)] {
			  procShowErrors
			  procExecutePathPrefOnFile path_to_text_editor emacs {External Text Editor} Advanced $prolog_variables(NewF)
		  } else {
		      tkErrorBox "Internal error: Slicing failed"
		      procShowErrors
		  }
	   } else {
		   tkMessageBox "Please first select an event inside a CSP Process inside the source text."
	   }
   } else {
       tkErrorBox "This command can only be used in CSPM mode."
   }
}





# -------
# procedure to search for invariant violations using open search
# -------

proc procModelCheck {} {
	global mc_prompt_ok Depth count done
	destroy .search
	# destroy re-added for Mac TclTk
	set f .search
    global mcPerformBreadthFirst mcFindDeadlocks mcFindInvViolations mcSearchForNewErrors
	global mcFindAssViolations mcFindGOAL mcStopWhenFullCoverage
	puts "Dialog_Create $f"
	if [Dialog_Create $f "Model Check:" -borderwidth 10] {
	    puts "done Dialog_Create $f"
		# message $f.msg -text "Enter max. nr of nodes to check:" -aspect 1000
		# entry $f.entry -textvariable mc_prompt(result)
		label $f.msg2 -text "Waiting"
		# ttk::progressbar $f.progress -orient horizontal -length 100 -mode indeterminate -maximum 1000
		set b [frame $f.buttons]
		set op [frame $f.options]
		# pack $f.msg $f.entry $f.buttons $f.options $f.msg2 $f.progress -side top -fill x
		pack $b $op $f.msg2 -side top -fill x
		# pack $f.entry -pady 5

		button $b.ok -text "Model Check" -command {set mc_prompt_ok 1}
		button $b.cancel -text Cancel -command {set mc_prompt_ok 0}
		pack $b.ok -side left
		pack $b.cancel -side right
		bind $f <Return> {set mc_prompt_ok 1 ; break}
		bind $f <Control-c> {set mc_prompt_ok 0 ; break}
	    puts "procModelCheck PP1"

		# set bfop [frame $op.bf -borderwidth .3c -relief flat]
		# label $bfop.lbl -text "Search:"
		# radiobutton $bfop.bf0 -text "Mixed" -variable mcPerformBreadthFirst  -value 0
		# radiobutton $bfop.bf1 -text "Breadth" -variable mcPerformBreadthFirst  -value 1
		# radiobutton $bfop.bf2 -text "Depth" -variable mcPerformBreadthFirst  -value 2
		# pack $bfop -side top
		# pack $bfop.lbl $bfop.bf0 $bfop.bf1 $bfop.bf2 -side left

		menubutton $op.bfmenu -text "Search Strategy" -menu $op.bfmenu.m -relief raised -borderwidth .1c
		pack $op.bfmenu -side top
		menu $op.bfmenu.m -tearoff 0
		prolog tcltk_dbf_modes(DBFNAMES)
		for {set i 0} {$i < [llength $prolog_variables(DBFNAMES)]} {incr i 1} {
				set NM [lindex $prolog_variables(DBFNAMES) $i];
				$op.bfmenu.m add radiobutton -label $NM  -variable mcPerformBreadthFirst -value $i
				# puts "Mode $NM  -- value $i"
			}
	    puts "procModelCheck PP2"

		#checkbutton $op.bf -text "Breadth First" -variable mcPerformBreadthFirst -offvalue 0 -onvalue 1
		checkbutton $op.deadlocks -text "Find Deadlocks" -variable mcFindDeadlocks
		checkbutton $op.invariants -text "Find Invariant Violations" -variable mcFindInvViolations

        global curFileTypeOpened cspstrFilename
		if {[prolog bmachine:b_get_machine_goal(_)]} {
		   checkbutton $op.findGOAL -text "Find defined GOAL" -variable mcFindGOAL
		} elseif {$curFileTypeOpened == "CSP" || $cspstrFilename != ""} {
		   checkbutton $op.findGOAL -text "Find event on goal CSP channel" -variable mcFindGOAL
		} else {
		   checkbutton $op.findGOAL -text "Find GOAL (from DEFINITIONS)" -variable mcFindGOAL -state disabled
		}
	    puts "procModelCheck PP3"
		set mcFindAssViolations 0
		set ASS [AssertionName]
	    if {[prolog bmachine:b_get_assertions_from_machine(\[\])]} {
             checkbutton $op.assertions -text "Find B $ASS Violations" -variable mcFindAssViolations -state disabled
		} else {
             checkbutton $op.assertions -text "Find $ASS Violations" -variable mcFindAssViolations
		}

	    if {[prolog operation_name_not_yet_covered(_)]} {
		     checkbutton $op.stopcoverage -text "Stop when all Operations covered" -variable mcStopWhenFullCoverage
		} else {
		     set mcStopWhenFullCoverage 0
		     checkbutton $op.stopcoverage -text "Stop when all Operations covered" -variable mcStopWhenFullCoverage -state disabled
		}
		if {![prolog tcltk_exists_an_open_node]} {
		   set mcSearchForNewErrors 0
		}
		checkbutton $op.already -text "Search for new Errors" -variable mcSearchForNewErrors
		pack $op.deadlocks $op.invariants $op.assertions $op.findGOAL $op.stopcoverage $op.already -side top -fill x
	    puts "procModelCheck PP4"
	} else {
	    wm title $f "Model Check:"
	    # $f.msg configure -text "Enter max. nr of nodes to check:" -aspect 1000
	    set b $f.buttons
	    puts "procModelCheck Alternative"
	}
    $f.msg2 configure -text ""
    global batch_mode
    set done 0
    set mc_prompt_ok 0
    $b.ok configure -text "Model Check"
    $b.ok configure -state normal

	    puts "Dialog_Wait_Prepare $f"
	Dialog_Wait_Prepare $f mc_prompt_ok $f.buttons
	    puts "done Dialog_Wait_Prepare $f done=$done"

    while {$done != 1} {
	    puts "enter while done=$done"
	  $b.ok configure -state normal

      if {$batch_mode} {
        set mc_prompt_ok 1
      } else {
        set mc_prompt_ok 0
	    puts "vwait variable mc_prompt_ok"
        vwait mc_prompt_ok
      }
	    puts "done tkwait"

	  # .search.progress start 50
	  global doInspectExistingNodes
	  if {!$mcSearchForNewErrors} {
	     set doInspectExistingNodes 1
	  } elseif [prolog tcltk_state_space_only_has_root_node] {
	     set doInspectExistingNodes 1
	     # in this case: force looking at the root node for state errors
	  } else {
	     set doInspectExistingNodes 0
	  }
	  if {$mc_prompt_ok && $doInspectExistingNodes} {
		    puts "Searching existing nodes"
		    if [prolog tcltk_search_among_existing_nodes(ErrRes,$mcFindDeadlocks,$mcFindInvViolations,$mcFindGOAL,$mcFindAssViolations)] {
				# puts "finished"
				set Result $prolog_variables(ErrRes)
				if {$Result == "all"} {
				  .search.msg2 configure -text "No error among existing nodes"
				  set done 0
				} else {
					if {$Result == "goal_found"} {
					   tkMessageBoxNoParent "Existing state satisfying GOAL predicate was found."
					} else {
					   tkErrorBoxNoParent "Error state was found for existing node:  $Result"
					}
					procInsertHistoryOptionsState
					set done 1
				}
	        } else {
			  .search.msg2 configure -text "Internal error occurred"
			  set done 0
	          tkErrorBox "error occurred !!"
	        }
	    }
	   # .search.progress stop
	    if {$mc_prompt_ok && ($done != 1)} {
		  # puts "Searching new nodes"
		  # set Depth $mc_prompt(result)
		  prolog tcltk_set_dbf_mode($mcPerformBreadthFirst)
		  $f.msg2 configure -text "Searching... "
		  set mc_prompt_ok 1
		  set count 0
		  set done 0
		  $b.ok configure -state disabled
		  global stepval
		  prologmnf animation_mode(MODE)
		  set curMode $prolog_variables(MODE)
		  if {$curMode == "cspm" || $curMode == "csp" || $curMode == "xtl"} {
		      set stepval 1000
		  } else {
		      set stepval 100
		  }
		  search_loop

		  if {!($done)} {
		    tkwait variable done
		  }
	     } else {
	       set done 1
	     }
	}
	Dialog_Wait_Finish $f
	Dialog_Dismiss $f
	procShowErrors
}

proc search_loop {} {
	global mc_prompt
	global count done stepval found_or_all_visited
	global mcFindDeadlocks mcFindInvViolations mcSearchForNewErrors doInspectExistingNodes mcFindGOAL
	global searchResult

    set sst "Total time for model checking step: [time {search_step} 1]"
    prolog "statistics(memory_used,MEM)"
    set MB [expr $prolog_variables(MEM)/1048576]
	prolog "tools:print_message('$sst (used memory: $MB MBytes)')"

     if {$found_or_all_visited} {
        if {$searchResult == "all"} {
            if [prolog tcltk_find_max_reached_node(_)] {
              set msg "\nNote: not all transitions were computed (increase the max. number of enablings and initialisations shown)."
            } else {
              set msg ""
            }
			if [prolog state_space:operation_name_not_yet_covered(R)] {
				  set msg "\nNote: some operations are not covered (e.g., $prolog_variables(R)).$msg"
			}
			if [prolog tcltk_model_checking_imprecise] {
				  set msg "\nNote: Hash markers could be imprecise for this machine.$msg"
			}
			append msg [getUnboundedWarnings]
            if {$doInspectExistingNodes} {
                  tkMessageBoxNoParent "No error state found, ALL nodes visited.$msg"
            } else {
                  if [prolog state_error_exists] {
                     set msg "\nNote: some existing nodes do contain errors.\nRe-run the model checker (with changed options) and inspect existing nodes.$msg"
                  }
                  tkMessageBoxNoParent "No error state found, all NEW nodes visited.$msg"
              }
            } else {
				if {$searchResult == "goal_found"} {
				   tkMessageBoxNoParent "State satisfying GOAL predicate was found."
			   } elseif {$searchResult == "full_coverage"} {
				   tkMessageBoxNoParent "All operations are now covered by at least one transition.\nUse Compute Coverage to obtain more details."
				} else {
				   tkErrorBoxNoParent "Error state was found for new node:  $searchResult"
				}
				procInsertHistoryOptionsState
	        }
     } else {
      # puts "finished search_loop"
        if {!($done)} {
         set done 2
         if {$mc_prompt_ok} {
         .search.msg2 configure -text "No error so far, $count nodes visited"
         # tk_messageBox -message "No error state found, $count nodes visited"
         } else {
         .search.msg2 configure -text "User cancelled after $count"
         # tk_messageBox -message "No error state found, user cancelled, $count nodes visited"
         }
      }}
      return

}


proc search_step {} {
	global mc_prompt_ok
	global count done stepval found_or_all_visited
	global mcFindDeadlocks mcFindInvViolations mcSearchForNewErrors mcFindGOAL
	global mcFindAssViolations mcStopWhenFullCoverage
	global searchResult

	set found_or_all_visited 0
    while {($mc_prompt_ok) && (!$found_or_all_visited)} {
      .search.msg2 configure -text "Searching... $count"
      # .search.progress step $stepval
      update
      set count [expr $count+$stepval]
      # puts $count

      prolog tcltk_model_check($stepval,ErrRes,$mcFindDeadlocks,$mcFindInvViolations,$mcFindGOAL,$mcFindAssViolations,$mcStopWhenFullCoverage,500,_)

       set searchResult $prolog_variables(ErrRes)
       set ll [llength $searchResult]

       if {$ll==2} {
          # timeout occurred
          set count [expr $count-$stepval+[lindex $searchResult end]]
       } elseif {$searchResult != "no"} {
	        # force exit of loop
	        set found_or_all_visited 1
	        set done 1
	   }
    }
}



proc Dialog_Wait_Prepare {top varName {focus {}}} {
	upvar $varName var

	# Poke the variable if the user nukes the window
	bind $top <Destroy> [list set $varName $var]

	# Grab focus for the dialog
	puts "focus=$focus"
	if {[string length $focus] == 0} {
	    puts "setting focus $top"
		set focus $top
	}
    global dw_old
	set dw_old [focus -displayof $top]
	focus $focus
	    puts "tkwait visibility $top"
	 tkwait visibility $top
	    puts "grab $top"
	 grab $top
}

proc Dialog_Wait_Finish {top} {
    global dw_old
	catch {grab release $top}
	focus $dw_old
}

# -------

proc procSAPTestcases {} {
    global prompt
    global mc_prompt
    set finished 0
    prologmnf sap:tcl_get_events_preselected(Events,Presel)
    set prompt(sap_max_depth) 10
    set prompt(sap_eventsel) $prolog_variables(Presel)
    if { [catch {set $prompt(sap_filename)}] } {set prompt(sap_filename) "sap_testcases.xml"}
    while {!$finished} {
	set chosen [SAPDialog $prolog_variables(Events)]
	if {[llength $chosen] > 0} {
	    set clist [join $chosen ","]
	    set target [escapeChars $prompt(sap_target_predicate)]
	    set maxdepth $prompt(sap_max_depth)
	    set maxnodes $mc_prompt(result)
	    set filename [escapeChars $prompt(sap_filename)]
	    if [string is integer -strict $maxdepth] {
		if [string is integer -strict $maxnodes] {
		    if [prolog sap:tcl_generate_testcases(\[$clist\],'$target',$maxdepth,$maxnodes,'$filename',NumberOfTestcases)] {
			procShowErrors
			set numberTC $prolog_variables(NumberOfTestcases)
			tkMessageBox "Generated $numberTC test cases."
			set finished 1
		    } else {
			procShowErrors
			tkMessageBox "Generating test cases failed."
		    }
		} else {
		    tkMessageBox "maximum number of nodes is not a number"
		}
	    } else {
		tkMessageBox "maximum depth is not a number"
	    }
	} else {
	    set finished 1
	}
    }
}
proc SAPDialog {events} {
    global prompt
    global mc_prompt
    set result [list]
    destroy .sap
    if [Dialog_Create .sap "SAP test case generation" -borderwidth 10] {
	message .sap.filemsg -aspect 1000 \
	    -text "file for saving test cases:"
	frame .sap.file
	entry .sap.file.filename -textvariable prompt(sap_filename)
	button .sap.file.browse -text Browse -command {SAPFile}
	pack .sap.file.filename .sap.file.browse -side left

	message .sap.predmsg -aspect 1000 \
	    -text "predicate to identify target states:"
	entry .sap.pred -textvariable prompt(sap_target_predicate)
	message .sap.eventmsg -aspect 1000 \
	    -text "please select events that should be covered:"
    frame .sap.events -borderwidth .1c -relief groove
    scrollbar .sap.events.scrolly -command ".sap.events.list yview"
	listbox .sap.events.list -selectmode multiple -yscroll ".sap.events.scrolly set" -height 10 -setgrid 1
    pack .sap.events.scrolly -side right -fill y
    pack .sap.events.list -expand 1 -fill both
	foreach e $events {
	    .sap.events.list insert end $e
	}
	.sap.events.list selection clear 0
	foreach i $prompt(sap_eventsel) {
	    .sap.events.list selection set $i
	}
	message .sap.maxmsg -aspect 1000 \
	    -text "maximum search depth:"
	entry .sap.max -textvariable prompt(sap_max_depth)
	message .sap.maxnodemsg -aspect 1000 \
	    -text "maximum number of new states:"
	entry .sap.maxnodes -textvariable mc_prompt(result)

	frame .sap.buttons
	button .sap.buttons.ok -text OK -command {set prompt(ok) 1}
	button .sap.buttons.cancel -text Cancel -command {set prompt(ok) 0}
	pack .sap.buttons.ok -side right
	pack .sap.buttons.cancel -side right

	pack .sap.file .sap.predmsg .sap.pred .sap.eventmsg .sap.events .sap.maxmsg .sap.max \
	    .sap.maxnodemsg .sap.maxnodes .sap.buttons -fill x -fill y
	set prompt(ok) 0
	Dialog_Wait .sap prompt(ok) .sap.events.list
	Dialog_Dismiss .sap

	if {$prompt(ok)} {
	    set result [.sap.events.list curselection]
	}
    }
    return $result
}
proc SAPFile {} {
    global prompt
    set types {
	{"XML Files" {.xml} }
    }
    set prompt(sap_filename) [tk_getSaveFile -filetypes $types -initialfile $prompt(sap_filename) -parent .]
}

proc procSAPLocal {} {
    global prompt
    global mc_prompt
    set prompt(sap_global) ""
    set prompt(sap_local) ""
    if {[SAPlocalDialog]} {
	set lfile [escapeChars $prompt(sap_local)]
	set gfile [escapeChars $prompt(sap_global)]
	if [prolog sap:tcl_create_local_testsuite('$gfile','$lfile',$mc_prompt(result),Sat,Unsat)] {
	    procShowErrors
	    set numsat $prolog_variables(Sat)
	    set numunsat $prolog_variables(Unsat)
	    if {$numunsat > 0} {
		set msg "Generated local traces for $numsat test cases, no traces found for $numunsat test cases."
	    } else {
		set msg "Generated local traces for all $numsat test cases."
	    }
	    tkMessageBox $msg
	} else {
	    procShowErrors
	    tkMessageBox "Generating test cases failed."
	}
    }

}
proc SAPlocalDialog {} {
    global prompt
    global mc_prompt
    set result 0
    destroy .saplocal
    if [Dialog_Create .saplocal "SAP local test case generation" -borderwidth 10] {

	message .saplocal.filemsg -aspect 1000 \
	    -text "file with global test cases"

	frame .saplocal.gfile
	entry .saplocal.gfile.filename -textvariable prompt(sap_global)
	button .saplocal.gfile.browse -text Browse -command {SAPGlobalFile}
	pack .saplocal.gfile.filename .saplocal.gfile.browse -side left

	message .saplocal.filemsl -aspect 1000 \
	    -text "file for local test cases"

	frame .saplocal.lfile
	entry .saplocal.lfile.filename -textvariable prompt(sap_local)
	button .saplocal.lfile.browse -text Browse -command {SAPLocalFile}
	pack .saplocal.lfile.filename .saplocal.lfile.browse -side left

	message .saplocal.maxnodemsg -aspect 1000 \
	    -text "maximum number of new states:"
	entry .saplocal.maxnodes -textvariable mc_prompt(result)

	frame .saplocal.buttons
	button .saplocal.buttons.ok -text OK -command {set prompt(ok) 1}
	button .saplocal.buttons.cancel -text Cancel -command {set prompt(ok) 0}
	pack .saplocal.buttons.ok -side right
	pack .saplocal.buttons.cancel -side right

	pack .saplocal.filemsg .saplocal.gfile .saplocal.filemsl .saplocal.lfile \
	    .saplocal.maxnodemsg .saplocal.maxnodes .saplocal.buttons -fill x

	set prompt(ok) 0
	Dialog_Wait .saplocal prompt(ok) .saplocal.gfile.filename
	Dialog_Dismiss .saplocal

	set result $prompt(ok)
    }
    return $result
}
proc SAPGlobalFile {} {
    global prompt
    set types {
	{"XML Files" {.xml} }
    }
    set prompt(sap_global) [tk_getOpenFile -filetypes $types -initialfile $prompt(sap_global) -parent .]
}
proc SAPLocalFile {} {
    global prompt
    set types {
	{"XML Files" {.xml} }
    }
    set prompt(sap_local) [tk_getSaveFile -filetypes $types -initialfile $prompt(sap_local) -parent .]
}

# -------

proc procShowInternalRep {} {
   if [prolog specfile:get_internal_representation(Res)] {
        set Result $prolog_variables(Res)
        procShowText $Result "Internal Representation"
   } else {
       tkErrorBox "Internal Error: Could not display internal representation."
   }
}

proc procPrintAsEventB {} {
   if [prolog bmachine:b_show_eventb_as_classicalb(Res,true)] {
        set Result $prolog_variables(Res)
        procShowText $Result "Event-B as Classical B"
   } else {
       tkErrorBox "Internal Error: Could not display B representation."
   }
}


proc procShowPropertiesAnalysis {} {
   if [prolog predicate_analysis:print_analysed_properties(Res)] {
        set Result $prolog_variables(Res)
        procShowText $Result "Predicate Analysis of the Properties"
   } else {
       tkErrorBox "Internal Error: Could not apply predicate analysis."
   }
}

proc procStartTesting {type} {
    global testing_mode logFile version testing_startTime
    prolog system:environ('PROB_SOURCE_DIR',Dir)
    set ProBDir $prolog_variables(Dir)
    prologmnf preferences:backup_preferences
    prologmnf preferences:reset_for_benchmarking
    prologmnf preferences:deactivate_recent_documents
	set failed_tests ""
	set detailed_errors ""
    set testing_mode true
    set CURDATE [clock format [clock seconds] -format "%Y%m%d_%H"]
    set logFile [open "$ProBDir/benchmark_history/$type$CURDATE.pl" w 0600]
    puts $logFile "% ProB $version"
    set testing_startTime [clock clicks -milliseconds]
}

proc procEndTesting {} {
    global testing_mode logFile version failed_tests detailed_errors testing_endTime
    set testing_endTime [clock clicks -milliseconds]
    close $logFile
    set testing_mode false
    procShowErrors
    procShowFailedTests
    prologmnf preferences:revert_preferences
    prologmnf preferences:activate_recent_documents
}

proc procShowFailedTests {} {
   global testing_mode strFilename
   global failed_tests detailed_errors testing_endTime testing_startTime
   global batch_mode
   set testing_Time [expr $testing_endTime - $testing_startTime]
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

proc procShowErrors {} {
   global testing_mode strFilename failed_tests detailed_errors
   if [prolog real_error_occurred] {
      set ERRWARN "error(s)"
   } else {
      set ERRWARN "warning(s)"
   }
   if [prolog get_all_errors(Res)] {
   # replace \n by newline ?!
    # regsub {\\n} $prolog_variables(Res) "\n" PRes
    while {[prolog get_error_span_linecol(Line,Col,EL,EC)]} {
          set line $prolog_variables(Line)
          # puts "ErrorSpan: $line $prolog_variables(Col)"
		  procHighlightLine $line
		  procUnderlineColumnFromTo $line $prolog_variables(Col) $prolog_variables(EL) $prolog_variables(EC)
		  .frmSource.text see $line.0
    }
    prolog reset_errors
	set Result [split [string trimright $prolog_variables(Res) {;}] \;]
    if {$testing_mode==true} {
       set errs [llength $Result]
       append failed_tests $strFilename ";  " $errs " $ERRWARN ;;"
       append detailed_errors $strFilename ";  " $prolog_variables(Res) ";;"
       # procShowList2 $detailed_errors "Error Messages" "The following errors occurred:" 0 1
    } else {
        global batch_mode
        if {$batch_mode} {
           puts "The following errors occurred:\n$Result\n"
	       prolog assert_cli_error_occurred(procShowErrors)
        } else {
             procShowList2 $Result "Error Messages" "The following errors occurred:" 0 1 "Errors.txt"
        }
    }
   }
}

proc procExpectErrors {n} {
   prologmnf count_errors_occurred(_,NrErrs)
   if {$n==0} {
       procShowErrors
   } elseif [prologmnf get_all_errors(Res)] {
	   set err_len $prolog_variables(NrErrs)
       if {$err_len != $n} {
	        set Result [split [string trimright $prolog_variables(Res) {;}] \;]
            procShowList $Result "Error Messages" "Expected $n errors but got $err_len :"
       }
   } else {
       tkErrorBox "Expecting $n errors: none happened!"
   }
}

proc procReExecuteTraceToCurNode {} {
    if [prolog tcltk_slowly_execute_trace_to_current_node] {
       procInsertHistoryOptionsState
    } else {
       tkErrorBox "Internal Error: could not execute history."
    }
}

proc procJumpToAnOpenNode {} {
    if [prolog tcltk_goto_an_open_node] {
       procInsertHistoryOptionsState
    } else {
       tkMessageBox "No open node, ALL nodes have been visited."
    }
}

proc procJumpToAMaxReachedNode {} {
    if [prolog tcltk_goto_max_reached_node] {
       procInsertHistoryOptionsState
    } else {
       tkMessageBox "ALL transitions have been computed for all nodes so far."
    }
}


proc procJumpToNonResetableNode {} {
    if [prolog tcltk_goto_a_non_resetable_node] {
       procInsertHistoryOptionsState
    } else {
      if [prolog tcltk_exists_an_open_node] {
         tkMessageBox "All nodes visited so far are resetable (can reach an initial state)."
      } else {
         tkMessageBox "All nodes are resetable (can reach an initial state)."
      }
    }
}

proc procJumpToNonDetNode {} {
    if [prolog tcltk_goto_a_non_deterministic_node] {
       procInsertHistoryOptionsState
    } else {
      if [prolog tcltk_exists_an_open_node] {
         tkMessageBox "All nodes visited so far are deterministic (i.e., the same operation always leads to the same resulting state)."
      } else {
         tkMessageBox "All nodes are deterministic (i.e., the same operation always leads to the same resulting state)."
      }
    }
}

# -------
# procedure to search for custom predicate
# -------

proc procAdvancedFind {} {
   if [EnterGOALFormula] {
	   prolog tcltk_search_among_existing_nodes(ErrRes,0,0,1,0)
	   procShowErrors
	   if {$prolog_variables(ErrRes) == "goal_found"} {
		   tkMessageBox "State satisfying your GOAL formula was found."
		   procInsertHistoryOptionsState
	   } else {
		   if [prolog tcltk_exists_an_open_node] {
		   tkMessageBox "No node visited so far satisfies your GOAL formula.
You can explore more (unvisited) nodes by using Temporal Model Check and ticking the 'Find defined GOAL' box."
		   } else {
		   tkMessageBox "No node satisfies your GOAL formula."
		   }
	   }
   }
}

proc EnterGOALFormula {} {
   EnterGOALFormulaWithCheckBtn ""
}
proc EnterGOALFormulaWithCheckBtn {CheckBtn1} {
   global Goal
   set Goal [Dialog_Promptww "Enter GOAL Formula (use B syntax):" 70 $CheckBtn1]
   if {$Goal != ""} {
       set EGoal [escapeChars $Goal]
       set success [prolog b_set_machine_goal('$EGoal')]
       procShowErrors
       return $success
   } else {
      return 0
   }
}

proc procFindEnabledOp {} {
   global OpName FromCurrent
   set OpName [procChooseOperation2 "yes" "Search from Current State" "FromCurrent" "operation_name_covered"]
   if {$OpName != ""} {
      if [prolog tcltk_goto_state_enabling_operation('$OpName',$FromCurrent)] {
           procInsertHistoryOptionsState
      } else {
		  if [prolog tcltk_exists_an_open_node] {
		     if {$FromCurrent} {
			    tkMessageBox "No node visited so far and reachable from the current state enables '$OpName'.\nUse the 'Model Check' command to explore more nodes."
			 } else {
			    tkMessageBox "No node visited so far enables '$OpName'.\n Use the 'Model Check' command to explore more nodes."
			 }
		  } else {
		     if {$FromCurrent} {
			    tkMessageBox "No node reachable from the current state enables '$OpName'."
			 } else {
			     tkMessageBox "No node enables '$OpName'."
			 }
		  }
      }
   }
}



proc procKodkod {} {
    if [prolog find_kodkod_replacements(Options,Errors)] {
	set selection [KodkodDialog $prolog_variables(Options) $prolog_variables(Errors)]
	if {[llength $selection] > 0} {
	    set poptions {[}
	    set i 0
	    foreach s $selection {
		if {$i > 0} {append poptions ","}
		append poptions {'} $s {'}
		set i [expr $i + 1]
	    }
	    append poptions {]}
	    prologmnf apply_kodkod_replacements($poptions)
	    tkMessageBox "Predicate replaced by Kodkod-Calls."
	}
    } else {
	   tkErrorBox "Internal error: searching for applicable options failed."
    }
}

# -------
# procedure to enter and check an ltl formula
# -------

proc procLtl {} {
    global ltldata
    set ok [LtlDialog "LTL model checking"\
	"Enter an LTL formula
   use {...} for B predicates,
   G,F,X,U,W,R,true,false,not,&,or and => are part of the supported LTL syntax,
   use e(op) to check if an operation op is enabled,
   use sink to check if no operation is enabled that leads to another state,
   use brackets to check what is the next operation, e.g. \[reset\] => X{db={}},
   Past-LTL is supported: Y,H,O,S,T are the duals to X,G,F,U,R"]

   if {$ok} {
       if {[string is integer $ltldata(result)]} {
	   set formula [escapeChars $ltldata(formula)]

	   if [prolog ltl_model_check('$formula',$ltldata(result),$ltldata(mode),Res)] {
	       procShowErrors
	       set res $prolog_variables(Res)
	       if {$res != "syntax_error"} {
		   if {$res == "no"} {
		       tkErrorBox "Formula FALSE.\nCounterexample found for $ltldata(formula)."
		       procInsertHistoryOptionsState
		   } elseif {$res == "ok"} {
		       tkMessageBox "Formula TRUE.\nNo counterexample found."
		   } elseif {$res == "incomplete"} {
		       tkMessageBox "No counterexample found so far.\nYou can explore more (unvisited) nodes by using Temporal Model Check or running LTL check again."
		   } elseif {$res == "nostart"} {
		       tkErrorBox "Error: No initialisation to start from found."
		   } else {
		       tkErrorBox "Internal error: unexpected result."
		   }
	       }
	   } else {
	       tkErrorBox "Internal error: LTL model checking failed."
	   }
	   procShowErrors
       } else {
	      tkErrorBox "Invalid input: $ltldata(result). Please enter a number."
       }
   }
}



proc procCtl {} {
   global Ctl
    set Ctl [Dialog_Promptww "Find Witness for CTL formula (&,or,e(Op),ExUy,EXx,AXx,EX\[Op\]x,EFx,AGx)" 70 ""]
   set Msg1 "\nYou can explore more (unvisited) nodes by using Temporal Model Check or running CTL check again."

   if {$Ctl != ""} {
       set Ctl [escapeChars $Ctl]
       if [prolog ctl_model_check('$Ctl',Res)] {
	   procShowErrors
	   set res $prolog_variables(Res)
	   if {$res != "syntax_error"} {
	       if {$res == true} {
		   if [prolog state_space:max_nr_of_new_nodes_limit_not_reached] {
		       tkMessageBox "Formula TRUE.\nWitness found for $Ctl."
		   } else {
		       tkMessageBox "Potential witness found for $Ctl.\nNot all nodes explored!$Msg1"
		   }
	       } else {
		   if [prolog state_space:max_nr_of_new_nodes_limit_not_reached] {
		       tkErrorBox "Formula FALSE.\nCounterexample found for $Ctl."
		   } else {
		       tkErrorBox "??TODO?? Potential counterexample found for $Ctl.\nNot all nodes explored!$Msg1"
		   }
	       }
	   }
       } else {
	      tkErrorBox "Model checking failed.$Msg1"
       }
       procInsertHistoryOptionsState
       procShowErrors
   }
}

# procedure to code forward slashes and quotes for transmission to Prolog
proc escapeChars {str} {return [string map {"\\" "\\\\" "'" "\\'"} $str]}

proc AssertionName {} {
     global curFileTypeOpened
     if {$curFileTypeOpened == "EVENTB"} { return "THEOREM" } else { return "ASSERTION" }
}

proc procConstraintBasedCheck {} {
       prolog temporary_set_preference(use_smt_mode,true)
	   if [prologmnf tcltk_constraint_based_check(ErrRes)] {
			procShowList2 $prolog_variables(ErrRes) "Constraint Based Checking" "Constraint Based Check for the Operations:" 1 0 "CBC_out.txt"
			 procInsertHistoryOptionsState
       }
       prolog reset_temporary_preference(use_smt_mode)
}

proc procOpConstraintBasedCheck {} {
   global OpName
   set OpName [procChooseOperation]
   if {$OpName != ""} {
           prolog temporary_set_preference(use_smt_mode,true)
	       if [prolog tcltk_constraint_based_check_op('$OpName',ErrRes)] {
			   set Result $prolog_variables(ErrRes)
			   if {$Result == "ok"} {
			       set msg [getUnboundedWarnings]
				   tkMessageBox "No error state found for $OpName.$msg"
			   } else {
				   tkErrorBox "Error state was found for $OpName:  $Result"
			   }
			   procInsertHistoryOptionsState
            } else {
               tkErrorBox "Internal error."
            }
            prolog reset_temporary_preference(use_smt_mode)
	}
}
proc procFindDeadlockedState {} {
           prolog temporary_set_preference(use_smt_mode,true)
	       if [prolog tcltk_constraint_find_deadlock_state(Res)] {
				procInsertHistoryOptionsState
				if {$prolog_variables(Res)=="time_out"} {
                   tkMessageBox "Time-out occurred during Deadlock checking\n (using 10 times time-out specified in Animation Preferences)."
				} else {
                   tkMessageBox "Deadlocking State Satisfying Invariant Found!"
				}
            } else {
			   set msg [getUnboundedWarnings]
               tkMessageBox "No Deadlock Counterexample Found.$msg"
            }
            prolog reset_temporary_preference(use_smt_mode)
}
proc procFindDeadlockedStateWithPred {} {
   global dialog_checkbutton1
   if [EnterGOALFormulaWithCheckBtn "Filter Unsatisfiable Guards"] {
           prolog temporary_set_preference(use_smt_mode,true)
	       if [prolog tcltk_constraint_find_deadlock_state_with_goal($dialog_checkbutton1,Res)] {
				procInsertHistoryOptionsState
				if {$prolog_variables(Res)=="time_out"} {
                   tkMessageBox "Time-out occurred during Deadlock checking\n (using 10 times the time-out specified in Animation Preferences)."
				} else {
                   tkMessageBox "Deadlocking State Satisfying Goal Predicate and Invariant Found!"
				}
            } else {
			   set msg [getUnboundedWarnings]
               tkMessageBox "No Deadlock Counterexample (Satisfying Goal Predicate) Found.$msg"
            }
            prolog reset_temporary_preference(use_smt_mode)
   }
}

proc getUnboundedWarnings {} {
	if [prolog unfixed_deferred_set(DS)] {
		  set msg "\nNote: Some SETS are potentially unbounded (e.g.,$prolog_variables(DS))."
	} else {
		  set msg ""
	}
	return $msg
}

# ------------------------------------------------------------
# ------------------------------------------------------------

# -------
# procedure to open a file, and put it on the source code
# -------
proc procShowSourceCode {sFileName} {
    global curFileDisplayed
    .frmSource.text configure -state normal
    .frmSource.text delete 0.0 end
    # open file
    set fid [open $sFileName r]
    .frmSource.text insert 0.0 [read $fid]
    close $fid
    .frmSource.text configure -state disabled
    set curFileDisplayed $sFileName
}
proc procShowSourceCodeFromString {sString} {
    .frmSource.text configure -state normal
    .frmSource.text delete 0.0 end
    .frmSource.text insert 0.0 $sString
    .frmSource.text configure -state disabled
}
proc procShowSourceCodeFromCodes {widget codes} {
    $widget configure -state normal
    $widget delete 0.0 end
    set src ""
    foreach i $codes {
        if {$i>255} {
            # inserting unicode on Mac makes the Text Widget incredibly slow
            if {$i==8469} {
                append src "NATURAL"
            } else {
                append src "???"
            }
        } else {
            append src [format %c $i]
        }
    }
    $widget insert end $src
    $widget configure -state disabled
}

proc procEnableSourceCodeEditing {} {
    if [prolog preferences:get_preference(allow_source_code_editing,true)] {
      .frmSource.text configure -state normal
      #.frmMenu.mnuFile entryconfigure 3 -state normal
    }
}

# -------
# procedure get new options from prolog and insert them to the listbox
# -------

proc procInsertHistoryOptionsState {} {
    #puts "procInsertHistoryOptionsState"
    procInsertHistory
    # puts "Inserting State"
    procInsertState
    # puts "Inserting Options"
    procInsertOptions
    # puts "Inserting State Errors"
    procInsertStateErrors
    # puts "Done procInsertHistoryOptionsState"
}
proc procClearOptionsStateHistory {} {
        .frmInfo.frmPerform.list delete 0 end
        .frmInfo.frmState.list delete 0 end
        .frmInfo.frmState.list delete 0 end
}

proc procResetOptions {arg} {
       global StatusFrame OpStatusFrame
        .frmInfo.frmPerform.list delete 0 end
        .frmInfo.frmPerform.list insert 0 $arg
           $StatusFrame.inv configure -state disabled
           $StatusFrame.inv configure -image InvUnknown
           $StatusFrame.timeout configure -image TimeoutEmpty
           $StatusFrame.timeout configure -state disabled
           $OpStatusFrame.maxreached configure -image MaxreachEmpty
           $OpStatusFrame.maxreached configure -state disabled
}
proc procInsertOptions {} {
    # Insert Options into Enabled Operations pane
    procResetOptions "Computing..."
    if [prolog {tcltk_get_options(Options)}] {
         procUpdateStatusFrame
         procShowErrors
        .frmInfo.frmPerform.list delete 0 end
        # obligatory to split into list because listbox fail to detect complex structure from prolog
        set listOptions $prolog_variables(Options)
        set iNbElem [llength $listOptions]

		for {set i 0} {$i < $iNbElem} {incr i} {
			.frmInfo.frmPerform.list insert $i [lindex $listOptions $i]
		}

	   prologmnf tcltk_get_options_dest_info(DestInfoOptions)
       set listOptions $prolog_variables(DestInfoOptions)
       set iNbElem [llength $listOptions]
	   for {set i 0} {$i < $iNbElem} {incr i} {
	        set destinfo [lindex $listOptions $i]
	        if {$destinfo == "skip"} {
			  .frmInfo.frmPerform.list itemconfigure $i -foreground "midnight blue"
			} elseif {$destinfo == "open"} {
			  .frmInfo.frmPerform.list itemconfigure $i -foreground DarkOliveGreen4
			} elseif {$destinfo == "invariant_violated"} {
			  .frmInfo.frmPerform.list itemconfigure $i -foreground IndianRed4
			} elseif {$destinfo == "deadlock"} {
			  .frmInfo.frmPerform.list itemconfigure $i -foreground DarkOrange3
			}
		}

       procCheckIfUnsatisfiableProperties
    } else {
         procUpdateStatusFrame
         procShowErrors
    }
}


proc procCheckIfUnsatisfiableProperties {} {
	prologmnf tcltk_get_status(INVVIOLATED,MAXREACHED,TIMEOUT)
    global show_error_if_no_transition
	if {$show_error_if_no_transition && [prolog tcltk_current_node_has_no_real_transition]} {
		set show_error_if_no_transition 0
		if {[prolog b_machine_has_constants_or_properties] && [prolog current_expression(root,_,_)]} {
		   global testing_mode
		   if {[prolog tcltk_current_node_has_partial_transition]} {
			  if {[prolog preference(allow_incomplete_partial_setup_constants,true)]} {
				 set addtxt "some"
			  } else {
				 set addtxt "all"
			  }
			  set ptxt "\n(But I inferred a value for $addtxt CONSTANTS.)"
		   } else {
			  set ptxt ""
		   }
		   if  {$prolog_variables(TIMEOUT)==1} {
		      set cmsg "Timeout occurred while searching for CONSTANTS which satisfy the PROPERTIES!$ptxt"
		   } else {
		      set cmsg "Cannot find CONSTANTS which satisfy the PROPERTIES! The PROPERTIES could be inconsistent.$ptxt"
		   }
		   set ans "no"
		   if {[prolog b_interpreter:tcltk_unsatisfiable_components_exist]} {
		      procQuickDebugProperties $cmsg
		   } elseif {$testing_mode==false && $prolog_variables(TIMEOUT)==1} {
			 set ans [tkYesNoMessageWarningBox "$cmsg\nDo you want to debug the PROPERTIES?" \
					  "Timeout in PROPERTIES"]
		   } elseif {$testing_mode==false} {
			 set ans [tkYesNoMessageWarningBox "$cmsg\nDo you want to debug the PROPERTIES?" \
					  "Unsatisfiable PROPERTIES"]
		   } else {
			 puts "$cmsg"
		   }
		   assertBatchError "$cmsg"
		   #puts "ans: $ans"
		   if { $ans == "yes" } {
			  procDebugProperties
		   }
		} elseif {[prolog current_expression(_,root,_)] || [prolog current_expression(_,concrete_constants(_),_)]} {
		   if {$prolog_variables(TIMEOUT)==1} {
			  tkErrorBox "Timeout occurred while initialising machine!\nClick on the timeout button to recompute without a timeout."
		   } else {
			  if {[prolog b_or_z_mode]} {
				 set ans [tkYesNoMessageWarningBox "Cannot initialise machine!\nDo you want to debug the INITIALISATION?" \
					  "Failing INITIALISATION"]
			     if { $ans == "yes" } {
				    procDebugInitialisation
			     }
			  } elseif {[prolog animation_mode(xtl)]} {
				 tkErrorBox "Cannot start up XTL specification!\nExamine your start/1 predicate."
			  } else {
				 tkErrorBox "Cannot start up specification!"
			  }
		   }
		}
	}
}

proc procQuickDebugProperties {Msg} {
	if {[prolog b_interpreter:tcltk_quick_describe_unsat_properties(Descr)]} {
		 set Cmd "procDebugProperties"
		 set ExtraBtn "Full Debug..."
		 set Result [split [string trimright $prolog_variables(Descr) {;}] \;]
		 procShowList3 $Result "PROPERTIES Info" $Msg 1 0 $ExtraBtn $Cmd "UNSAT_PROPERTIES.txt"
	}
}

proc procMaxreachedButtonAction {} {
    if [SymmetryOn] {
     prolog preferences:get_preference(symmetry_mode,SYMMODE)
       set Msg "Symmetry is turned on ($prolog_variables(SYMMODE)). This may permute the destination state of an operation."
    } else {
       set Msg "Note: Symmetry is off."
    }
    if [prolog {tcltk_get_status(INVVIOLATED,MAXREACHED,TIMEOUT)}] {
		if {$prolog_variables(MAXREACHED)==1} {
			set Msg "Possibly not all enabled operations were computed.\nIncrease Max Number of Initialisations/Operations in the Animation Preferences and re-load the machine."
	   }
   }
   tkMessageBox "$Msg"
}

proc SymmetryOn {} {
    prolog preferences:get_preference(symmetry_mode,SYMMODE)
    if {$prolog_variables(SYMMODE)=="off"} {
      return 0
    } else {
      return 1
    }
}

proc procUpdateStatusFrame {} {
    if [prolog {tcltk_get_status(INVVIOLATED,MAXREACHED,TIMEOUT)}] {
       global StatusFrame OpStatusFrame
       if {$prolog_variables(INVVIOLATED)==1} {
           # $StatusFrame.inv configure -text KO
           # $StatusFrame.inv configure -background red
           $StatusFrame.inv configure -image InvKO
           $StatusFrame.inv configure -state normal
           $StatusFrame.inv configure -cursor question_arrow
       } elseif {$prolog_variables(INVVIOLATED)==2} {
           # $StatusFrame.inv configure -text KO
           # $StatusFrame.inv configure -background red
           $StatusFrame.inv configure -image InvTimeout
           $StatusFrame.inv configure -state normal
           $StatusFrame.inv configure -cursor question_arrow
       } elseif [prolog current_state_corresponds_to_initialised_b_machine] {
           $StatusFrame.inv configure -image InvOK
           $StatusFrame.inv configure -state normal
           $StatusFrame.inv configure -cursor question_arrow
       } else {
           $StatusFrame.inv configure -state disabled
           $StatusFrame.inv configure -image InvUnknown
           $StatusFrame.inv configure -cursor arrow
       }
       if {$prolog_variables(TIMEOUT)==1} {
           $StatusFrame.timeout configure -image Timeout
           $StatusFrame.timeout configure -state normal
       } else {
           $StatusFrame.timeout configure -image TimeoutEmpty
           $StatusFrame.timeout configure -state disabled
       }
       if {$prolog_variables(MAXREACHED)==1} {
           $OpStatusFrame.maxreached configure -image Maxreach
           $OpStatusFrame.maxreached configure -state normal
       } elseif [SymmetryOn] {
           $OpStatusFrame.maxreached configure -image MaxreachSym
           $OpStatusFrame.maxreached configure -state normal
       } else {
           $OpStatusFrame.maxreached configure -image MaxreachEmpty
           $OpStatusFrame.maxreached configure -state disabled
       }
       if [prolog tcltk_can_backtrack] {
           # $OpStatusFrame.backtrack configure -image BackEnabled
           $OpStatusFrame.backtrack configure -state normal
       } else {
           # $OpStatusFrame.backtrack configure -image BackDisabled
           $OpStatusFrame.backtrack configure -state disabled
       }
       if [prolog tcltk_can_forward] {
           # $OpStatusFrame.forward configure -image ForwardEnabled
           $OpStatusFrame.forward configure -state normal
       } else {
           # $OpStatusFrame.forward configure -image ForwardDisabled
           $OpStatusFrame.forward configure -state disabled
       }

    }
}


# -------
# procedure get new state from prolog and insert them to the listbox
# -------
proc procInsertState {} {
    if [prolog {tcltk_get_state(State)}] {
        # puts "state: $prolog_variables(State)"
        .frmInfo.frmState.list delete 0 end
        # obligatory to split into list because listbox fail to detect complex structure from prolog
        set listState $prolog_variables(State)
        set iNbElem [llength $listState]
        # we have to also insert 1 by 1
        for {set i 0} {$i < $iNbElem} {incr i} {
            .frmInfo.frmState.list insert $i [lindex $listState $i]
        }
    }
    procShowErrors
    procUpdateStateViewer
}

# -------
# procedure to get the state's errors from prolog and insert them
# to the listbox
# This is not done in procInsertState, because the operations
# (a source for errors)may not yet have been computed
# -------
proc procInsertStateErrors {} {
    if [prolog {tcltk_get_state_errors(State)}] {
        # obligatory to split into list because listbox fail to detect complex structure from prolog
        set listState $prolog_variables(State)
        set iNbElem [llength $listState]
        # we have to also insert 1 by 1
        for {set i 0} {$i < $iNbElem} {incr i} {
            .frmInfo.frmState.list insert $i [lindex $listState $i]
	    .frmInfo.frmState.list itemconfigure $i -foreground red
        }
    }
}


# -------
# procedure get new history from prolog and insert them to the listbox
# -------
proc procInsertHistory {} {
    if [prolog {tcltk_get_history(History)}] {
        .frmInfo.frmHisto.list delete 0 end
        # obligatory to split into list because listbox fail to detect complex structure from prolog
        set listHistory $prolog_variables(History)
        set iNbElem [llength $listHistory]
        # we have to also insert 1 by 1
        for {set i 0} {$i < $iNbElem} {incr i} {
            .frmInfo.frmHisto.list insert $i [lindex $listHistory $i]
        }
    }
}

# -------
# procedure perform an option
# -------
global lastClicki lastSeeItem
set lastClicki -1
set lastSeeItem 1

proc procPerformOptionSingleClick {} {
    global lastClicki lastSeeItem
    procRemoveHighlightExecutedOperation
    set iOption [.frmInfo.frmPerform.list curselection]
    if {$iOption != ""} {
        incr iOption
        if [prolog tcltk_get_line_col($iOption,X,Y,XE,YE)] {
            set Line $prolog_variables(X)
            set Col $prolog_variables(Y)
            set EndLine $prolog_variables(XE)
            set EndCol $prolog_variables(YE)
            set iNbElem [llength $Line]
            # puts "source loc: option:$iOption line:$Line-$EndLine col:$Col-$EndCol Nb:$iNbElem last:$lastSeeItem\n"
            for {set i 0} {$i < $iNbElem} {incr i} {
               # puts "procHighlight $i"
               procHighlightExecutedOperation [lindex $Line $i] [lindex $Col $i] [lindex $EndLine $i] [lindex $EndCol $i]
            }
            if [prolog tcltk_is_sync_event($iOption)] {
               # Synchronisation happened
	           .frmSource.text tag configure executedTag -background gold2
	        }
	       # Make sure that by repeatedly clicking we cycle through the various positions
		   if {$lastClicki==$iOption} {
			   incr lastSeeItem
			   if {$lastSeeItem>=$iNbElem} {
				   set lastSeeItem 0
			   }
			   set SeeLine [lindex $Line $lastSeeItem]
			   # puts "SeeLine: $SeeLine,  lastSeeItem: $lastSeeItem"
			   .frmSource.text see "$SeeLine.0"
		   } else {
			   set lastSeeItem $iNbElem
			   incr lastSeeItem -1
		   }
        }
        set lastClicki $iOption
    }
}
proc procPerformOption {} {
    set iOption [.frmInfo.frmPerform.list curselection]
    if {$iOption != ""} {
        incr iOption
        if [prolog tcltk_perform($iOption)] {
            procInsertHistoryOptionsState
        }
    }
}

proc procPerformBacktrack {} {
        if [prolog tcltk_backtrack] {
            procInsertHistoryOptionsState
        }
}
proc procPerformForward {} {
        if [prolog tcltk_forward] {
            procInsertHistoryOptionsState
        }
}

# ---------------------------------------------

proc Dialog_Create {top title args} {
	global dialog tcl_version
	puts "Dalog_Create $top $title $args"
	if [winfo exists $top] {
	    puts "winfo exists $top"
		switch -- [wm state $top] {
			normal {
				# Raise a buried window
	            puts "normal"
				raise $top
			}
			withdrawn -
			iconified {
				# Open and restore geometry
				wm deiconify $top
				catch {wm geometry $top $dialog(geo,$top)}
			}
		}
		return 0
	} else {
	    puts "winfo not exists $top"
		eval {toplevel $top} $args
		wm title $top $title
        if {$tcl_version>=8.5} {
            puts "Icon $top"
            wm iconphoto $top WindowSubIcon
        }
		return 1
	}
}
proc Dialog_Wait {top varName {focus {}}} {
	upvar $varName var

	# Poke the variable if the user nukes the window
	bind $top <Destroy> [list set $varName $var]

	# Grab focus for the dialog
	if {[string length $focus] == 0} {
		set focus $top
	}
	set old [focus -displayof $top]
	focus $focus
	catch {tkwait visibility $top}
	catch {grab $top}

	# Wait for the dialog to complete
	tkwait variable $varName
	catch {grab release $top}
	focus $old
}

proc Dialog_Dismiss {top} {
	global dialog
	# Save current size and position
	catch {
		# window may have been deleted
		set dialog(geo,$top) [wm geometry $top]
		wm withdraw $top
	}
}


#
# Example 33-2
# A simple dialog.
#

proc Dialog_Prompt { string } {
   Dialog_Promptww $string 0 ""
}

proc Dialog_Promptww { string wd CheckBox1} {
	global prompt dialog_checkbutton1
	destroy .prompt
	set f .prompt
	if [Dialog_Create $f "Prompt" -borderwidth 10] {
		message $f.msg -text $string -aspect 1000
		if {$wd > 0} {
		   entry $f.entry -textvariable prompt(result) -width $wd
		} else {
		   entry $f.entry -textvariable prompt(result)
		}
		set b [frame $f.buttons]
		pack $f.msg $f.entry $f.buttons -side top -fill x
		pack $f.entry -pady 5
		if {$CheckBox1 != ""} {
		    checkbutton $f.checkbutton1 -text "$CheckBox1" -variable dialog_checkbutton1 -onvalue 1 -offvalue 0
		    pack $f.checkbutton1 -pady 5 -side top
		}
		button $b.ok -text OK -command {set prompt(ok) 1}
		button $b.cancel -text Cancel \
			-command {set prompt(ok) 0}
		pack $b.ok -side left
		pack $b.cancel -side right
		bind $f.entry <Return> {set prompt(ok) 1 ; break}
		bind $f.entry <Control-c> {set prompt(ok) 0 ; break}
	}
	set prompt(ok) 0
	Dialog_Wait $f prompt(ok) $f.entry
	Dialog_Dismiss $f
	if {$prompt(ok)} {
		return $prompt(result)
	} else {
		return {}
	}
}

proc CheckLtlAssertions {} {
  set Cmd1 "b_get_definition_string_list_from_machine('ASSERT_LTL',Res)"
  set Cmd2 "ltl_model_check('\$i',100000,init,Res)"
  CheckAssertions $Cmd1 $Cmd2 "LTL"
}
proc CheckAssertions {GetAssertionListCmd Eval_ith_AssertionCmd TypeOfAssertion} {
	global ok batch_mode
	global InvKO InvOK InvUnknown
    destroy .checkltl
    set f .checkltl
	if [Dialog_Create $f "Check $TypeOfAssertion Assertions" -borderwidth 10] {
		message $f.msg -text "List of ASSERT_LTL Formulas" -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        listbox $f.frmSource.text -yscroll "$f.frmSource.scrolly set" \
         -setgrid 1 -height 11 -width 70 -bg white -selectborderwidth 3 -selectbackground brown \
         -fg darkgreen -highlightcolor blue
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.text -expand 1 -fill both

		set b [frame $f.buttons]
	    set c [frame $f.checks]
		pack $f.msg $f.frmSource $f.checks $f.buttons -side top -fill x
		pack $f.frmSource -pady 5 -expand 1 -fill both
		global lastiOption boolPrefVal
		set lastiOption 0
		message $c.valmsg -text "" -aspect 1000
		button $c.inv -text "" -image InvUnknown -relief flat
		pack $c.inv $c.valmsg -side left
		# button $b.ok -text "Check All LTL Formulas" -command {set ok 1}
		button $b.cancel -text Cancel -command {set ok 2} -state disabled
		pack $b.cancel -side right

		bind $f.frmSource <Control-c> {set ok 2 ; break}
	}
    prolog $GetAssertionListCmd
	set Result [split [string trimright $prolog_variables(Res) {;}] \;]
    foreach i $Result {
       $f.frmSource.text insert end "$i == ?"
    }
    if {$Result==""} {
       $f.frmSource.text insert end "No ASSERT_LTL in the DEFINITIONS"
    }

	catch {tkwait visibility $f}
   set index 0
   set errors 0
   foreach i $Result {
		 $c.valmsg configure -text "Checking $i"
	     $f.frmSource.text delete $index
		 $f.frmSource.text insert $index "-->> $i == CHECKING"
		 $f.frmSource.text see $index
		 $f.frmSource.text activate $index
		 $f.frmSource.text selection set $index $index
		 update
		 if [prolog [subst $Eval_ith_AssertionCmd]] {
			set res $prolog_variables(Res)
			$f.frmSource.text delete $index
		     if {$res == "ok"} {
			 $f.frmSource.text insert $index "$i == TRUE"
		     } elseif {$res == "no"} {
			 $f.frmSource.text insert $index "$i == FALSE"
			 # tkErrorBox "Formula FALSE.\nCounterexample found for $i."
			 # procInsertHistoryOptionsState
			 incr errors 1
			 $f.frmSource.text selection set $index $index
		     } elseif {$res == "syntax_error"} {
			 $f.frmSource.text insert $index "$i == SYNTAX_ERROR"
			 incr errors 1
			 $f.frmSource.text selection set $index $index
		     } elseif {$res == "nostart"} {
			 $f.frmSource.text insert $index "$i == NO_INITIALISATION_FOUND"
			 incr errors 1
			 $f.frmSource.text selection set $index $index
		     } else {
			 $f.frmSource.text insert $index "$i == UNKNOWN_ERROR"
			 incr errors 1
			 $f.frmSource.text selection set $index $index
		     }
		 } else {
		     tkErrorBox "Internal error: LTL model checking failed."
		     incr errors 1
		}
		procShowErrors
		incr index 1
	}
	if {$errors>0} {
	   $c.valmsg configure -text "Errors occurred: $errors !"
	   procInsertHistoryOptionsState
	   $c.inv configure -image InvKO
	   if {$batch_mode} {
	       prolog assert_cli_error_occurred(check_ltl_assertions)
	   }
	} else {
	   $c.valmsg configure -text "All formulas valid"
	   $c.inv configure -image InvOK
	}
	$b.cancel configure -text "Done" -state normal

    set ok 0
	# Dialog_Wait $f ok $f.frmSource
    if {!$batch_mode} {
	   tkwait variable ok
	}
	Dialog_Dismiss $f
	procInsertHistoryOptionsState
}

proc KodkodDialog {options errors} {
    global prompt
    set result [list]
    destroy .kodkod
    if [Dialog_Create .kodkod "Kodkod" -borderwidth 10] {
#	set oldresult $prompt(result)
	set prompt(result) $options
	set prompt(errors) $errors

	message .kodkod.msg -aspect 1000 \
	    -text "Please specify which parts of the B machine should be replaced"

	frame .kodkod.select
	listbox .kodkod.select.list -selectmode multiple -listvariable prompt(result)
	frame .kodkod.select.buttons
	button .kodkod.select.buttons.all -text "Select all" \
	    -command {.kodkod.select.list selection set 0 999999}
	button .kodkod.select.buttons.none -text "Deselect all" \
	    -command {.kodkod.select.list selection clear 0 999999}
	button .kodkod.select.buttons.errors -text "Show Errors" \
	    -command {KodkodErrors}
	pack .kodkod.select.buttons.all .kodkod.select.buttons.none -fill x -side top
	pack .kodkod.select.buttons.errors -fill x -side bottom
	pack .kodkod.select.list -side left -expand 1 -fill x
	pack .kodkod.select.buttons -side right -fill y

	frame .kodkod.buttons
	button .kodkod.buttons.ok -text OK -command {set prompt(ok) 1}
	button .kodkod.buttons.cancel -text Cancel -command {set prompt(ok) 0}
	pack .kodkod.buttons.ok -side left
	pack .kodkod.buttons.cancel -side right
	bind .kodkod <Return> {set prompt(ok) 1 ; break}
	bind .kodkod <Control-c> {set prompt(ok) 0 ; break}

	pack .kodkod.msg .kodkod.select .kodkod.buttons -fill x

	set prompt(ok) 0
	Dialog_Wait .kodkod prompt(ok) .kodkod.select.list
	Dialog_Dismiss .kodkod

	if {$prompt(ok) == 1} {
	    foreach i [.kodkod.select.list curselection] {
		lappend result [lindex $options $i]
	    }
	}
#	set prompt(result) $oldresult
    }
    return $result
}

proc KodkodErrors {} {
    global prompt
    destroy .kodkoderrors
    if [Dialog_Create .kodkoderrors "Kodkod: Errors" -borderwidth 10] {
	message .kodkoderrors.msg -aspect 1000 \
	    -text "The following parts of the specification were not applicable to Kodkod"
	listbox .kodkoderrors.list -listvariable prompt(errors)
	button .kodkoderrors.ok -text OK -command {set prompt(errorsbt) 1}
	pack .kodkoderrors.msg
	pack .kodkoderrors.list -expand 1 -fill both
	pack .kodkoderrors.ok -side left
	bind .kodkod <Return> {set prompt(errorsbt) 1 ; break}
	bind .kodkod <Key-Escape> {set prompt(errorsbt) 0 ; break}
	set prompt(errorsbt) 0
	Dialog_Wait .kodkoderrors prompt(errorsbt) .kodkoderrors.list
	Dialog_Dismiss .kodkoderrors
    }
}

proc LtlDialog {title msg} {
    global ltldata
    destroy .ltl
    if [Dialog_Create .ltl $title -borderwidth 10] {
	message .ltl.msg -text $msg -aspect 1000
	entry .ltl.formula -textvariable ltldata(formula) -width 40

	if {$ltldata(formula) == ""} {
	 	    if [prolog bmachine:b_get_definition_string_from_machine('ASSERT_LTL',Res)] {
	 	        set ltldata(formula) $prolog_variables(Res)
	 	    }
  	}

	frame .ltl.states
	message .ltl.states.msg -text "Max no. of new states:" -aspect 1000
	entry .ltl.states.max -textvariable ltldata(result) -width 9
	pack .ltl.states.msg .ltl.states.max -side left

	frame .ltl.modi
	radiobutton .ltl.modi.init -variable ltldata(mode) -value "init" \
	    -text "Start search in initialisation"
	radiobutton .ltl.modi.starthere -variable ltldata(mode) -value "starthere" \
	    -text "Start search in current state"
	radiobutton .ltl.modi.checkhere -variable ltldata(mode) -value "checkhere" \
	    -text "Start in initialisation, but check formula in current state"
	pack .ltl.modi.init .ltl.modi.starthere .ltl.modi.checkhere -anchor w

	frame .ltl.buttons
	button .ltl.buttons.ok -text OK -command {set ltldata(ok) 1}
	button .ltl.buttons.cancel -text Cancel -command {set ltldata(ok) 0}
	pack .ltl.buttons.cancel -side left
	pack .ltl.buttons.ok -side right
	bind .ltl <Return> {set ltldata(ok) 1 ; break}
	bind .ltl <Control-c> {set ltldata(ok) 0 ; break}

	pack .ltl.msg .ltl.formula .ltl.states .ltl.modi .ltl.buttons -fill x -side top -anchor w
    }
    set ltldata(ok) 0
    Dialog_Wait .ltl ltldata(ok) .ltl.formula
    Dialog_Dismiss .ltl
    if $ltldata(ok) {
	if {[string trim $ltldata(formula)] != ""} {
	    return 1
	} else {
	    return 0
	}
    } else {
	return 0
    }
}

# ------------------------------------------------------------

proc installAtelierBPlugin {} {
	set SELTXT "Application"
	set types {
		{"All files"		*}
	}
        set BIN "StartProB.sh"
	if [prolog tools:host_platform(windows)] {
	     # puts "Windows"
	     set SELTXT "File inside the AtelierB Folder"
	     set apps_path "C:/PROGRA~1/Atelier B/"
             set BIN "ProBWin.exe"
	} elseif [prolog tools:host_platform(darwin)] {
	     puts "Darwin"
	     set apps_path "/Applications/"
	     # set apps_path "/Applications/AtelierB.app/AB/"
	     # if ![file isdirectory $apps_path]
		set types {
			{"Applications"		{.app}	}
			{"All files"		*}
		}
	} else {
	     puts "Other"
	     set apps_path [file normalize "~"]
	     set SELTXT "file (inside the AB directory)"
             set types {
		 {"AtelierB"		{AtelierB}}
		 {"All files"		*}
	     }
	}
	# puts "apps_path: $apps_path"

	if {$apps_path != "" && [file isdirectory $apps_path]} {
		set pathres [tk_getOpenFile -filetypes $types -initialdir $apps_path -title "Select the AtelierB $SELTXT (version 4 final, not beta version)"]
    } else {
        set pathres [tk_getOpenFile -title "Select the AtelierB 4 $SELTXT (final, not beta version)"]
    }
	if {$pathres != ""} {
	  set extdir ""
	  if [file isdirectory "$pathres/AB/bbin"] {
	      # on Mac: go inside the AB directory
	      set apathres "$pathres/AB/"
	  } else {
	       set apathres [file dirname $pathres]
	  }
	  # puts "dirname $apathres"
	  if [file isdirectory "$apathres/bbin"] {
		  if [file isdirectory "$apathres/extensions"] {
			  set extdir "$apathres/extensions"
		  } elseif [file isdirectory "$apathres"] {
			  set extdir "$apathres/extensions"
			  file mkdir $extdir
		  }
	  }
	  if {$extdir != "" && [file isdirectory "$extdir"]} {
            global app_dir
            set menulbl "Animate with ProB (Tcl/Tk)"
            set etoolfile "probtclk.etool"
	        # puts "Writing to $extdir/$etoolfile"
	        # puts "Inserting reference to $app_dir/StartProB.sh"
			set fid [open "$extdir/$etoolfile" w]
			set str "<externalTool category=\"component\"   name=\"ProBTclTk\" label=\"&amp;$menulbl\">"
			puts  $fid $str
			set str "    <toolParameter name=\"editor\" type=\"tool\" configure=\"yes\""
            puts  $fid $str
			set str "   default=\"$app_dir/$BIN\" />"
        	puts  $fid $str
        	puts  $fid "    <command>\${editor}</command>"
			puts  $fid "    <param>\${componentPath}</param>"
			puts  $fid "    </externalTool>"
			close $fid
	        # puts "Closing $extdir/$etoolfile"
	        if {[file exists "$app_dir/StartProB.sh"] || [file exists "$app_dir/ProBWin.exe"]} {
			     tkMessageBox "Installation successful (generated '$extdir/$etoolfile'). Restart AtelierB, right-click on a component and select the command '$menulbl'. Note: in case you move ProB you need to re-install the plugin."
	        } else {
			     tkMessageBox "Installed external tool file '$extdir/$etoolfile'. However, the ProB directory $app_dir does not contain StartProB.sh or ProBWin.exe."
	        }
	  } else {
			 tkErrorBox "Invalid directory: $pathres. Does not contain 'bbin' or 'AB/bbin' subdirectory."
	  }
	}
}


proc procReportBug {} {
  procReportDBug "Please describe your problem."
}
proc procReportDBug {details} {
   global shortversion version prologversion strFilename normal_user expert_user
   prologmnf animation_mode(MODE)
   set curMode $prolog_variables(MODE)
   prologmnf host_platform(PF)
   set plat $prolog_variables(PF)
   set TKVERS "ProB (tcl/tk) $shortversion"
   set SUMM "ProB (tcl/tk) $version BUG"
   set KEYW "UserDiscoveredBug"
   prologmnf get_preferences_list(PLIST)
   # cannot attach info below; URI too large or Python exception in Trac Server
   # set PREFS $prolog_variables(PLIST)
   # set text [.frmSource.text get 1.0 "end - 1 chars"]
   set shortened_details [string range $details 0 5000]
   set DESC "Bug report for ProB $version-$plat.\nUser=($normal_user/$expert_user).Mode=$curMode.\n\n$shortened_details\n\n<<PASTE YOUR SPECIFICATION HERE OR ATTACH BELOW>>"
   # proc_open_url "http://prob:prob@cobra.cs.uni-duesseldorf.de/trac/login/"
   set shorturl "cobra.cs.uni-duesseldorf.de/trac/newticket"
   if {[prolog tools:host_platform(windows)]} {
      set bug_report_url "http://$shorturl"
      # Windows seems to have a problem with long urls
       tkMessageBox "I will try and open $shorturl.\nPlease enter Bug details. Then press 'Create ticket'.\n\nThanks for providing feedback!"
   } else {
      set bug_report_url "http://$shorturl?version=$TKVERS&component=prolog_core&description=$DESC&summary=$SUMM&keywords=$KEYW"
       tkMessageBox "I will try and open $shorturl.\nPlease complete Bug details. Then press 'Create ticket'.\n\nThanks for providing feedback!"
   }
   proc_open_url $bug_report_url
}

proc procCheckForUpdates {} {
    global version
    set prob_update_url "http://www.stups.uni-duesseldorf.de/ProB_Releases/"
    package require http
    prolog "tools:print_message('Opening http connection')"
    set token [::http::geturl "$prob_update_url/current_version.txt"]
    set data [::http::data $token]
    # puts "data $data"
    ::http::cleanup $token
    set webversion [lindex [split $data] 0]
    prolog "tools:print_message('Webversion $webversion')"
    if {![string is digit [string index $webversion 0]]} {
           tkErrorBox "Error connecting to $prob_update_url. Message: $data."
    } elseif {$webversion != $version} {
           global tcl_platform
           set webdate [lindex [split $data] 1]
           if {$tcl_platform(platform) != "unix"} {
               tkMessageBox "Version $webversion of ProB is available as of $webdate (your version is $version).\nGo to $prob_update_url to obtain the update."
           } else {
             tkMessageBox "Version $webversion of ProB is available as of $webdate (your version is $version).\nI will try to open $prob_update_url in browser."
             proc_open_url $prob_update_url
           }
    } else {
           tkMessageBox "Your version of ProB ($version) is up-to-date."
   }
}

proc proc_open_url {url} {
	if {[prolog tools:host_platform(windows)]} {
                puts "Trying to open $url"
            exec rundll32 url.dll,FileProtocolHandler $url &
	    #[eval exec [auto_execok start] [list $url]]
	} elseif {[prolog tools:host_platform(darwin)]} {
		exec open $url
	} else {
	    invokeLinuxBrowser $url
	}
}
proc invokeLinuxBrowser {url} {
        foreach browser {gnome-open htmlview firefox mozilla konqueror netscape} {
            set binary [lindex [auto_execok $browser] 0]
            if {[string length $binary]} {
		puts "Trying $binary $url"
                catch {exec $binary $url &}
                break
            }
        }
 }

proc CreateTextDialog {f title msg is_image with_ok_button} {
	if [Dialog_Create $f $title -borderwidth 10] {
	    if {$is_image} {
           label $f.msg -image $msg
	    } else {
		    message $f.msg -text $msg -aspect 1000
        }
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        text $f.frmSource.text -yscroll "$f.frmSource.scrolly set" \
            -height 18 -width 80 -bg white
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.text -expand 1 -fill both

        frame $f.buttons
        if {$with_ok_button} {
			button $f.buttons.ok -text Finished -command {set ok 1}
			pack $f.buttons.ok -side right
        }
		pack $f.msg $f.frmSource $f.buttons -side top
		pack $f.frmSource -pady 5 -expand yes -fill both

		bind $f.frmSource <Return> {set ok 1 ; break}

	}
}


proc procAboutProB {} {
        global tcl_dir version
		destroy .binfo
		set f .binfo

	    if [file exists "$tcl_dir/icons/prob_logo.gif"] {
			image create photo prob_img -format gif -fil "$tcl_dir/icons/prob_logo.gif"
			CreateTextDialog $f "About ProB" prob_img 1 1
	    } elseif [file exists "./tcl/icons/prob_logo.gif"] {
			image create photo prob_img -format gif -fil "./tcl/icons/prob_logo.gif"
			CreateTextDialog $f "About ProB" prob_img 1 1
		} else {
			CreateTextDialog $f "About ProB" ProB 0 1
		}

		prologmnf prob_licence(Lic)
		$f.frmSource.text insert 0.0 "prolog_variables(Lic)"
		global revision lastchangeddate prologversion tcl_version
        $f.frmSource.text insert 0.0 "ProB $version\n$revision\n$lastchangeddate\n Tcl\Tk $tcl_version\n $prologversion\n"

		set pattern {ProB|\((C|c)\)|Michael Leuschel|-+}
		procMarkRegExpression $f.frmSource.text $pattern about_highlight_tag
		$f.frmSource.text tag configure about_highlight_tag -foreground darkslateblue
		set pattern {http://[a-zA-Z/~_\-\.]*}
		procMarkRegExpression $f.frmSource.text $pattern about_highlight_tag2
		$f.frmSource.text tag configure about_highlight_tag2 -foreground tomato

		$f.frmSource.text configure -state disabled

		set ok 0
		Dialog_Wait $f ok $f.frmSource
		Dialog_Dismiss $f
}
proc procAboutProBInSourceFrame {} {
		procCheckIfSaved
        global version
		prologmnf prob_licence(Lic)
		global revision lastchangeddate prologversion tcl_version
		procShowSourceCodeFromString "ProB $version\n$revision\n$lastchangeddate\n Tcl\Tk $tcl_version\n $prologversion\n prolog_variables(Lic)"

        global tcl_dir
	    if [file exists "$tcl_dir/icons/prob_logo.gif"] {
			image create photo prob_img -format gif -fil "$tcl_dir/icons/prob_logo.gif"
			.frmSource.text configure -state normal
			.frmSource.text insert 1.0 "\n"
			.frmSource.text image create 1.0 -image prob_img
			.frmSource.text configure -state disable
		}

		set pattern {ProB|\((C|c)\)|Michael Leuschel|-+}
		procMarkRegExpression .frmSource.text $pattern about_highlight_tag
		.frmSource.text tag configure about_highlight_tag -foreground darkslateblue
		set pattern {http://[a-zA-Z/~_\-\.]*}
		procMarkRegExpression .frmSource.text $pattern about_highlight_tag2
		.frmSource.text tag configure about_highlight_tag2 -foreground tomato
		procResetCodeModified
}


proc procBInfo {} {
		destroy .bsinfo
		set f .bsinfo
		CreateTextDialog $f "B Syntax" "Summary of supported B notation" 0 0

		$f.frmSource.text configure -state normal
		$f.frmSource.text delete 0.0 end

		prologmnf prob_summary(Summary)
		$f.frmSource.text insert 0.0 $prolog_variables(Summary)

		procDoSyntaxColouring $f.frmSource.text
		$f.frmSource.text configure -state normal

		set ok 0
		# Dialog_Wait $f ok $f.frmSource
		# Dialog_Dismiss $f
}
proc procCSPInfo {} {
		destroy .cspinfo
		set f .cspinfo
		CreateTextDialog $f "CSP Syntax" "Summary of supported CSP notation" 0 0

		$f.frmSource.text configure -state normal
		$f.frmSource.text delete 0.0 end

		prologmnf procsp_summary(Summary)
		$f.frmSource.text insert 0.0 $prolog_variables(Summary)

		procDoCSPSyntaxColouring $f.frmSource.text
		$f.frmSource.text configure -state disabled

		# set ok 0
		# Dialog_Wait $f ok $f.frmSource
		# Dialog_Dismiss $f
}
proc procCheckingInfo {} {
		destroy .binfo
		set f .binfo
		CreateTextDialog $f "Verification Information" "Summary of ProB Verification Tools" 0 1

		$f.frmSource.text configure -state normal
		$f.frmSource.text delete 0.0 end

		prologmnf mc_summary(Summary)
		$f.frmSource.text insert 0.0 $prolog_variables(Summary)

		$f.frmSource.text configure -state disabled

		set ok 0
		Dialog_Wait $f ok $f.frmSource
		Dialog_Dismiss $f
}
proc procZInfo {} {
		destroy .zinfo
		set f .zinfo
		CreateTextDialog $f "ProZ" "Summary of supported Z notation" 0 0

		$f.frmSource.text configure -state normal
		$f.frmSource.text delete 0.0 end

		prologmnf proz_summary(Summary)
		$f.frmSource.text insert 0.0 $prolog_variables(Summary)

		procDoCSPSyntaxColouring $f.frmSource.text
		$f.frmSource.text configure -state disabled

		set ok 0
		# Dialog_Wait $f ok $f.frmSource
		# Dialog_Dismiss $f
}
# ------------------------------------------------------------


proc procOpenFileInEditor {} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
        procExecutePathPrefOnFile path_to_text_editor emacs {External Text Editor} Advanced $strFilename
    } else {
       tkErrorBox "No File open. Cannot Open External Editor."
    }
}


proc procSaveState {} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
		set rootName [file rootname $strFilename]
		set saveName {}
		append saveName $rootName ".saved.P"
		# use P extension so that we can use XTL

		prolog "tcltk_save_state('$saveName')"
    } else {
       tkErrorBox "No File open. Cannot Save State."
    }
}
proc procLoadState {} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
		set rootName [file rootname $strFilename]
		set saveName {}
		append saveName $rootName ".saved.P"

		if [file exists $saveName] {
			if [prolog "tcltk_load_state('$saveName')"] {
			   procInsertHistoryOptionsState
			} else {
			   tkErrorBox "No state has been previously saved.\nUnable to open $saveName."
			}
		} else {
		   tkErrorBox "No state saved yet as $saveName"
		}
    } else {
       tkErrorBox "No File open. Cannot Load State."
    }
}

proc procSaveSpecStateForRefinement {} {
    global strFilename
    set rootName [file rootname $strFilename]
    set saveName {}
    append saveName $rootName "_refine_spec.P"
    # use P extension so that we can use XTL

    prolog "tcltk_save_specification_state_for_refinement('$saveName')"

    if [prolog "tcltk_exists_an_open_node"] {
		   tkMessageBox "Warning: not all nodes have been explored.\nLater refinement check may miss counterexamples.\nContinue model checking until all nodes have been explored to fix this."
    } elseif [prolog "tcltk_find_max_reached_node"] {
		   tkMessageBox "Warning: not all transitions have been computed.\nLater refinement check may miss counterexamples.\nIncrease the Max. Number of Enablings/Initialisations in Animation Preferences and Reopen machine."
    }
}



proc procSetDefaultRefSpecFile {} {
    global currentRefSpecFile strFilename
    if {$currentRefSpecFile == "" && [prolog {bmachine:b_get_machine_refinement_hierarchy([_,_|_])}]} {
        global strFilename
		set dirName [file dirname $strFilename]
		prolog {bmachine:b_get_machine_refinement_hierarchy([_Main,Machine|_Rest])}
		set bmachine_Name $prolog_variables(Machine)

		set subxmlName {}
        append subxmlName $bmachine_Name "_refine_spec.P"
        set currentRefSpecFile [file join $dirName $subxmlName]
   } else {
        set currentRefSpecFile ""
   }
}

proc procRefinementShowCounterExample {Trace} {
  global currentRefSpecFile
  set curT [file tail $currentRefSpecFile]
  procShowList $Trace "Refinement Check Failed" "Trace of the implementation that cannot be matched by $curT:"
  procInsertHistoryOptionsState
}
proc profRefinementShowNoCounterExample {mcPerformSingleFailures} {
  global currentRefSpecFile
  set curT [file tail $currentRefSpecFile]
  if [prolog "tcltk_exists_an_open_node"] {
   tkMessageBox "No counter example found.\nAll traces explored so far are matched by $curT.\nNote: Explore more nodes using model check to check all traces."
  } elseif [prolog "tcltk_find_max_reached_node"] {
   tkMessageBox "No counter example found.\nAll traces explored so far are matched by $curT.\nNote: not all transitions have been computed.
Increase the Max. Number of Enablings/Initialisations in Animation Preferences and Reopen machine."
  } elseif {$mcPerformSingleFailures} {
   tkMessageBox "No counter example exists.\nAll traces (and single failures) of the implementation are matched by $curT.\nNote: ProB does not check the particular gluing invariant you may have provided."
  } else {
   tkMessageBox "No counter example exists.\nAll traces of the implementation are matched by $curT.\nNote: ProB does not check the particular gluing invariant you may have provided."
  }
}
proc procChangeCurRefSpecFile {} {
  global currentRefSpecFile
	set types {
		{"XSB Files"		{.P}	}
		{"Prolog Files"		{.pl .pro}	}
		{"All files"		*}
	}
	# show the dialog box
        set machinesPath [getMachinesPath]
	if {$machinesPath != "" && [file isdirectory $machinesPath]} {
		 set newf [tk_getOpenFile -filetypes $types -initialdir $machinesPath -parent .refcheck -title "Select a _refine_spec.P File"]
	} else {
		 set newf [tk_getOpenFile -filetypes $types -parent .refcheck -title "Select a _refine_spec.P File"]
	}
	if {$newf != ""} {
	    set currentRefSpecFile $newf
	    .refcheck.file.filename configure -text [file tail $currentRefSpecFile]
	    .refcheck.buttons.ok configure -state normal
	}
}
proc procSetRefSpecFile {ancMachine} {
  # puts "setting $ancMachine"
  global currentRefSpecFile
  global strFilename
  set currentRefSpecFile {}
  append currentRefSpecFile [file dirname $strFilename] "/" $ancMachine "_refine_spec.P"
  .refcheck.file.filename configure -text [file tail $currentRefSpecFile]
  .refcheck.buttons.ok configure -state normal
  # puts "set $ancMachine"
}
proc procRefinementCheck {} {
  global strFilename
  if {$strFilename == ""} {
       tkErrorBox "No File open."
  } else {
	global mc_prompt Depth count done
	destroy .refcheck
	set f .refcheck
    global currentRefSpecFile
	procSetDefaultRefSpecFile

    global mcPerformSingleFailures
	if [Dialog_Create $f "Trace Refinement Check:" -borderwidth 10] {
		message $f.msg -text "Enter max. nr of new states to check:" -aspect 1000
		entry $f.entry -textvariable mc_prompt(result)
		label $f.msg2 -text "Waiting"
		set fn [frame $f.file]
		set b [frame $f.buttons]
		set op [frame $f.options]
		pack $f.msg $f.entry $f.file $f.buttons $f.options $f.msg2 -side top -fill x
		pack $f.entry -pady 5

		label $fn.filelabel -text "Specification:"
		message $fn.filename -text [file tail $currentRefSpecFile] -aspect 1000
		#button $fn.pick -text "Choose Specification..." -command {procChangeCurRefSpecFile} -relief raised -overRelief button
		menubutton $fn.pickmenu -text "Choose Specification..." -menu $fn.pickmenu.m -relief raised

		pack $fn.filelabel $fn.filename $fn.pickmenu -side top
		menu $fn.pickmenu.m -tearoff 0
	    if [prolog {b_get_machine_refinement_hierarchy([_|_])}] {
		prolog {b_get_machine_refinement_hierarchy([_Main|AncList])}
		    foreach ancMachine $prolog_variables(AncList) {
		        $fn.pickmenu.m add command -label $ancMachine -command "procSetRefSpecFile {$ancMachine}"
		    }
        $fn.pickmenu.m add sep
		}
        $fn.pickmenu.m add command -label "Other Specification..." -command procChangeCurRefSpecFile

		button $b.ok -text "Refinement Check" -command {set mc_prompt(ok) 1} -state disabled
		button $b.cancel -text Cancel -command {set mc_prompt(ok) 0}
		pack $b.ok -side left
		pack $b.cancel -side right
		bind $f.entry <Return> {set mc_prompt(ok) 1 ; break}
		bind $f.entry <Control-c> {set mc_prompt(ok) 0 ; break}

		checkbutton $op.failures -text "Check Single Failures" -variable mcPerformSingleFailures \
		    -offvalue 0 -onvalue 1
		# checkbutton $op.check_explored -text "Only check explored nodes" -variable mcOnlyCheckExploredNodes
		pack $op.failures -side top -fill x
	} else {
	    wm title $f "Refinement Check:"
	    $f.msg configure -text "Enter max. nr of nodes to check:" -aspect 1000
	    set b $f.buttons
	}
    $f.msg2 configure -text ""

    set done 0
    set mc_prompt(ok) 0
    $b.ok configure -text "Refinement Check"
    if {$currentRefSpecFile != ""} {
        $b.ok configure -state normal
    } else {
        $b.ok configure -state disabled
    }
    Dialog_Wait_Prepare $f mc_prompt(ok) $f.entry

    while {$done != 1} {
	  set mc_prompt(ok) 0
	  tkwait variable mc_prompt(ok)
	  if {$mc_prompt(ok)} {
		  if {![string is integer $mc_prompt(result)]} {
			 tkErrorBox "Invalid input: $mc_prompt(result). Please enter a number."
		  } elseif {$currentRefSpecFile == ""} {
			 tkErrorBox "No Specification File chosen. Choose one first."
	      } elseif {! [file exists $currentRefSpecFile]} {
	         set tl [file tail $currentRefSpecFile]
			 tkErrorBox "The specification file '$tl' does not exist. You have to open the corresponding B machine and 'Save for later refinement check'."
		  } else {
			  set Depth $mc_prompt(result)
			  $f.msg2 configure -text "Checking... "
			  set mc_prompt(ok) 1
			  set done 0
			  $b.ok configure -state disabled
			  refinement_search $currentRefSpecFile $Depth $mcPerformSingleFailures
			  $b.ok configure -state normal

			  if {!($done)} {
				tkwait variable done
			  }
		  }
	  } else { set done 1}
	}
	Dialog_Wait_Finish $f
	Dialog_Dismiss $f
	procShowErrors
 }
}

proc default_trace_refinement_search {} {
    global currentRefSpecFile
	procSetDefaultRefSpecFile
	trace_refinement_search $currentRefSpecFile
}
proc trace_refinement_search {currentRefSpecFile} {
   refinement_search $currentRefSpecFile 10000 0
}

proc refinement_search {currentRefSpecFile Depth mcPerformSingleFailures} {
	global mc_prompt
	global  done
   global batch_mode

	prolog tcltk_load_refine_spec_file('$currentRefSpecFile')

    if [prolog tcltk_refinement_search(ErrRes,$mcPerformSingleFailures,$Depth)] {
		set Result $prolog_variables(ErrRes)
		set done 1
		if {$Result == "all"} {
	        if {!$batch_mode} {
			   .refcheck.msg2 configure -text "No counter example found so far"
			}
			if [prolog "tcltk_exists_an_open_node"] { set done 2 } else { profRefinementShowNoCounterExample $mcPerformSingleFailures }
		  } else {
			if {!$batch_mode} {
			 .refcheck.msg2 configure -text "Counter example found"
			 }
			procRefinementShowCounterExample $Result
		}
	} else {
	  tkErrorBox "Internal error. Refinement Search failed."
	}
    return
}


# ------------------------------------------------------------
# Trace Checking Section
# ------------------------------------------------------------

proc procSaveTraceFile {} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
		set rootName [file rootname $strFilename]
		set saveName {}
		append saveName $rootName ".trace"

		prolog "tcltk_save_history_as_trace_file('$saveName')"
    } else {
       tkErrorBox "No File open. Cannot save current history to trace file."
    }
}
proc procCheckTraceFile {} {
   procCheckTraceFile2 1 0
}
proc procCheckTraceFile2 {display_success_message silent} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
		set rootName [file rootname $strFilename]
		set saveName {}
		append saveName $rootName ".trace"
		if [file exists $saveName] {
            procCheckTraceFileInternal $saveName $silent $display_success_message
		} else {
           tkErrorBox "No trace has been saved for this machine.\nUnable to open $saveName."
		}
    } else {
       tkErrorBox "No file currently open. Cannot check trace."
    }
}

proc procCheckOtherTraceFile {} {
    set types {
    	{"Trace Files"		{.trace}	}
    	{"Prolog Files"		{.pl .pro}	}
	    {"All files"		*}
    }
    # show the dialog box
    set machinesPath [getMachinesPath]
    if {$machinesPath != "" && [file isdirectory $machinesPath]} {
         set strTrFilename [tk_getOpenFile -filetypes $types -initialdir $machinesPath -parent . ]
    } else {
         set strTrFilename [tk_getOpenFile -filetypes $types -parent . ]
    }
    if {$strTrFilename != ""} {
        procCheckTraceFileInternal $strTrFilename 0 1
    }
}

proc procCheckTraceFileInternal {tracefile silent display_success_message} {
    prolog "tcltk_check_sequence_from_file('$tracefile')"

    if [prolog test_error_occurred(_)] {
        # errors will be displayed by procInsertHistoryOptionsState
        # tkMessageBox "Trace checking failed."
    } elseif {$display_success_message} {
        tkMessageBox "Trace checking successful."
    }
    if {$silent==0} { procInsertHistoryOptionsState }
}

# ------------------------------------------------------------
# Visualization Section
# ------------------------------------------------------------

proc procViewModuleHierarchy {} {
    global curFileTypeOpened strFilename
    if {$strFilename=="" || $curFileTypeOpened != "B"} {
       tkErrorBox "Please open a B Machine first."
    } else {
       set rootName [file rootname $strFilename]
       set dotName {}
       set psName {}
       append dotName $rootName ".dot"
       append psName $rootName ".ps"
       prolog "b_machine_hierarchy:print_hierarchy('$dotName')"
       procShowErrors
       procVisualiseDotFile $dotName $psName
    }
}

proc procDisplayVisitedStates {} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".ps"
		prolog "tcltk_print_states_for_dot('$dotName')"
		procVisualiseDotFile $dotName $psName
    } else {
       tkErrorBox "No File open. Cannot display statespace."
    }
}

proc provVisualizeDEFGOAL {} {
  if [prolog b_reset_machine_goal_from_DEFINITIONS] {
     procVisualiseInvariantOrOpPRE "_goal_"
  } else {
       tkErrorBox "Could not obtain GOAL predicate from DEFINITIONS."
  }
}
proc procVisualiseCustomPredicate {} {
   global Goal
   set Goal [Dialog_Promptww "Enter Predicate to Visualize (use B syntax):" 70 ""]
   if {$Goal != ""} {
       set EGoal [escapeChars $Goal]
       set success [prolog b_set_machine_goal('$EGoal')]
       procShowErrors
       if {$success} {
            procVisualiseInvariantOrOpPRE "_goal_"
       }
   }
}
proc procAnalyseCustomPredicate {} {
   global Goal
   set Goal [Dialog_Promptww "Enter Predicate to Analyse (use B syntax):" 70 ""]
   if {$Goal != ""} {
       set EGoal [escapeChars $Goal]
       set success [prolog b_set_machine_goal('$EGoal')]
       procShowErrors
       if {$success} {
            procAnalyseGOAL
       }
   }
}

proc procVisualiseInvariant {} {
    procVisualiseInvariantOrOpPRE ""
}
proc procVisualiseAssertions {} {
	procVisualiseInvariantOrOpPRE "_assertions_"
}
proc procVisualiseProperties {} {
	procVisualiseInvariantOrOpPRE "@PROPERTIES"
}
proc procVisualiseInvariantOrOpPRE {opName} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
        if { $opName == "@PROPERTIES" || $opName == "_goal_" || $opName == "@INITIALISATION" || [prolog current_state_corresponds_to_initialised_b_machine] } {
			set rootName [file rootname $strFilename]
			set dotName {}
			set psName {}
			append dotName $rootName ".dot"
			append psName $rootName ".ps"
			if {$opName == "@PROPERTIES"} {
			   prolog "generate_dot_from_properties('$dotName')"
			} elseif {$opName == "_assertions_"} {
			   prolog "generate_dot_from_assertions('$dotName')"
			} elseif {$opName == "_goal_"} {
			   prolog "generate_dot_from_goal('$dotName')"
			} elseif {$opName == "_deadlockpo_"} {
			   prolog "generate_dot_from_deadlock_po('$dotName')"
			} elseif {$opName != ""} {
			   prolog generate_dot_from_operation('$opName','$dotName')
			} else {
			   prolog "generate_dot_from_invariant('$dotName')"
			}
			procShowErrors
			procVisualiseDotFile $dotName $psName
		 } else {
		   tkErrorBox "Please intialise Machine first."
		 }
    } else {
       tkErrorBox "No File open. Cannot visualise predicate."
    }
}


proc procVisualiseDotFile {dotName psName} {
  if [file exists $dotName] {
    if [prolog preferences:get_preference(dot_use_ps_viewer,true)] {
        if [prolog preferences:get_preference(path_to_dot,PDotCmd)] {
	        set DotCmd $prolog_variables(PDotCmd)
	    } else {set DotCmd dot}
	    # puts "exec $DotCmd -Tps $dotName -o $psName"
	    # use eval in case DotCmd contains arguments
	    if {[catch {exec $DotCmd -Tps $dotName -o $psName} errid]} {
           tkErrorBox "Could not execute dot.\n Be sure that dot is installed and that path '$DotCmd' in the Viewer Preferences is correct.\nError: $errid"
         } else {
            procOpenPSFile $psName
        }
    } else {
       if [prolog preferences:get_preference(dot_use_alterate_dot_viewer,true)] {
        procExecutePathPrefOnFile path_to_dotty2 dotty dotty Viewer $dotName
       } else {
        procExecutePathPrefOnFile path_to_dotty dotty dotty Viewer $dotName
       }
    }
   } else {
       tkErrorBox "Dot file does not exists: '$dotName'."
   }
}


proc procExecutePathPrefOnFile {pathpreference DefaultCmd PathName PrefCategory FileName} {
        if [prolog preferences:get_preference($pathpreference,PDottyCmd)] {
	        set Cmd $prolog_variables(PDottyCmd)
	    } else {set Cmd $DefaultCmd}
        procExecuteCommandOnFile $Cmd $FileName \
           "Be sure that the path for $PathName in the $PrefCategory Preferences is correct."
}

proc procExecuteCommandOnFile {Cmd FileName ExtraErrMsg} {
  set dcmdtail [file extension $Cmd]
  set firstcmd [lindex $Cmd 0]
  if {[prolog tools:host_platform(darwin)] && ($firstcmd == "open" || $firstcmd == "wish") } {
      if {[catch {eval exec $Cmd $FileName &} errid]} {
         # use eval because Cmd is actually a list of commands with options
         tkErrorBox "Could not execute '$Cmd'.\n$ExtraErrMsg\nError: $errid"
      }
  } elseif {$dcmdtail == ".app" && [prolog tools:host_platform(darwin)]} {
	  if {[catch {exec open -a $Cmd $FileName &} errid]} {
		tkErrorBox "Could not open '$Cmd'.\n$ExtraErrMsg\nError: $errid"
	  }
  } else {
      if {[prolog tools:host_platform(windows)] } {
         set FN [file attributes $FileName -shortname]
      } else {
         set FN $FileName
      }
      # puts "trying $Cmd $FileName"
      if {[catch {exec $Cmd $FN &} errid]} {
         tkErrorBox "Could not execute '$Cmd'.\n$ExtraErrMsg\nError: $errid"
      }

  }
}

proc procOpenPSFile {psName} {
     procExecutePathPrefOnFile path_to_ps_viewer gv {Postscript Viewer} Viewer $psName
}

proc procDisplayTransitionDiagram {} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
        set ObsVar [Dialog_Prompt "Enter Variable or Expression to observe:"]
        if {$ObsVar != ""} {
			set rootName [file rootname $strFilename]
			set dotName {}
			set psName {}
			append dotName $rootName ".dot"
			append psName $rootName ".ps"
			prolog "tcltk_print_transition_diagram_for_variable_or_expr('$ObsVar','$dotName')"
			procVisualiseDotFile $dotName $psName
		}
    } else {
       tkErrorBox "No File open."
    }
}

proc procDisplayCurrentState {asgraph} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".ps"
		if {$asgraph=="no"} {
		    prolog "tcltk_print_current_state_for_dot('$dotName')"
		} elseif {$asgraph=="custom"} {
		    prolog "state_custom_dot_graph:tcltk_generate_state_custom_dot_graph('$dotName')"
		} else {
		    # prolog "tcltk_print_current_state_as_graph_for_dot('$dotName')"
		    prolog "graph_canon:print_cstate_graph('$dotName')"
		}
		procVisualiseDotFile $dotName $psName
    } else {
       tkErrorBox "No File open. Cannot display current state."
    }
}
proc procDisplayTraceToCurrentState {shortest} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".ps"
		if {$shortest=="yes"} {
		  prolog "tcltk_print_shortest_trace_to_current_state_for_dot('$dotName')"
		} else {
		  prolog "tcltk_print_history_to_current_state_for_dot('$dotName')"
		}
		procVisualiseDotFile $dotName $psName
    } else {
       tkErrorBox "No File open. Cannot display trace to current state."
    }
}
proc procDisplayReducedSigMerge {} {
  procDisplayReducedState "no"
}
proc procDisplayReducedDFA {} {
  procDisplayReducedState "yes"
}
proc procDisplayReducedState {dfa} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".ps"
		if {$dfa=="yes"} {
		  prolog "reduce_graph_state_space:print_dot_for_dfa_from_nfa('$dotName')"
		} else {
		  prolog "reduce_graph_state_space:print_state_merge_for_dot('$dotName')"
		}
		procVisualiseDotFile $dotName $psName
    } else {
       tkErrorBox "No File open. Cannot display reduced state space."
    }
}

proc procSubgraph {type} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".ps"
		if {$type=="goal"} {
		    prolog "reduce_graph_state_space:	print_subgraph_of_goal_nodes('$dotName')"
		} else {
		    prolog "reduce_graph_state_space:print_subgraph_associated_with_invariant_violations('$dotName')"
		}
		procVisualiseDotFile $dotName $psName
    } else {
       tkErrorBox "No File open. Cannot display subgraph."
    }
}

proc procTestCaseGraph {} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".ps"
		prolog "testcase_generator:generate_testcase_graph('$dotName')"
		procVisualiseDotFile $dotName $psName
    } else {
       tkErrorBox "No File open. Cannot display TestCaseGraph."
    }
}


###########   JENS

proc procFlowEnableGraph {} {
    global strFilename curFileTypeOpened
    if { [prolog "specfile:aninmation_minor_mode(eventb)"] } {
       if {$strFilename != ""} {
		  set rootName [file rootname $strFilename]
		  set dotName {}
		  set psName {}
		  append dotName $rootName "_flow_eg.dot"
		  append psName $rootName ".ps"
		  prolog "flow:create_enable_graph('$dotName')"
		  procShowErrors
		  procVisualiseDotFile $dotName $psName
       } else {
          tkErrorBox "No File open. Cannot display FlowEnableGraph."
       }
    } else {
      tkErrorBox "Flow Graphs are only available for Event-B."
    }
}
proc procFlowEnableGraphs {} {
    global strFilename curFileTypeOpened
    if { [prolog "specfile:aninmation_minor_mode(eventb)"] } {
       if {$strFilename != ""} {
		  set rootName [file rootname $strFilename]
		  set dotName {}
		  set psName {}
		  append dotName $rootName "_flow_eg.dot"
		  append psName $rootName ".ps"
		  prolog "flow:create_enable_graphs('$dotName')"
	      procShowErrors
		  procVisualiseDotFile $dotName $psName
       } else {
          tkErrorBox "No File open. Cannot display FlowEnableGraph."
       }
    } else {
      tkErrorBox "Flow Graphs are only available for Event-B."
    }
}

proc procBaseGuide {} {
    global strFilename curFileTypeOpened
    if { [prolog "specfile:aninmation_minor_mode(eventb)"] } {
       if {$strFilename != ""} {
		  set rootName [file rootname $strFilename]
		  set dotName {}
		  set psName {}
		  append dotName $rootName "_flow_bgg.dot"
		  append psName $rootName ".ps"
		  prolog "flow:print_base_guide('$dotName')"
		  procShowErrors
		  procVisualiseDotFile $dotName $psName
       } else {
          tkErrorBox "No File open. Cannot display FlowEnableGraph."
       }
    } else {
      tkErrorBox "Flow Graphs are only available for Event-B."
    }
}

proc procEventGuide {} {
global strFilename curFileTypeOpened
if { [prolog "specfile:aninmation_minor_mode(eventb)"] } {
   if {$strFilename != ""} {

   set Event [Dialog_Promptww "Enter Event Name:" 70 ""]
    # FIXME: Check existence
   if {$Event != ""} {
	  set rootName [file rootname $strFilename]
	  set dotName {}
	  set psName {}
	  append dotName $rootName "_flow_" $Event "_bgg.dot"
	  append psName $rootName ".ps"
	  prolog "flow:print_event_guide('$dotName','$Event')"
	  procShowErrors
	  procVisualiseDotFile $dotName $psName
   } else {
      tkErrorBox "No Event given. Cannot display Graph."
   }
   } else {
      tkErrorBox "No File open. Cannot display FlowEnableGraph."
   }
} else {
  tkErrorBox "Flow Graphs are only available for Event-B."
}
}



proc procUpdateMenusFromPrologInfo {} {
  global debugMode typeCheckMode traceUponError
   if {[prolog "debug_mode(on)"]} {set debugMode 1} else {set debugMode 0}
   if {[prolog "run_time_type_check_mode(on)"]} {set typeCheckMode 1} else {set typeCheckMode 0}
   if {[prolog "trace_upon_error"]} {set traceUponError 1} else {set traceUponError 0}

    global chosensymmode
    prologmnf preferences:get_preference(symmetry_mode,SYMMODE)
    set chosensymmode $prolog_variables(SYMMODE)
    # puts "SymmMode $chosensymmode"
}

proc procUpdateDebuggingMode {} {
  global debugMode typeCheckMode traceUponError
  if {$debugMode==1} {
      prologmnf "tcltk_turn_debugging_on"
   } else {
      prologmnf "tcltk_turn_debugging_off"
   }
  if {$typeCheckMode==1} {
      prologmnf "turn_on_run_time_type_checks"
   } else {
      prologmnf "turn_off_run_time_type_checks"
   }
  if {$traceUponError==1} {
      prologmnf "turn_trace_upon_error_on"
   } else {
      prologmnf "turn_trace_upon_error_off"
   }
}

proc procSaveStateToFDR {} {
    global strFilename
    set rootName [file rootname $strFilename]
    set cspName {}
    append cspName $rootName "_statespace.csp"
    prolog "tcltk_print_states_for_fdr('$cspName')"
    if [prolog animation_mode(cspm)] {
       procExecutePathPrefOnFile path_to_probe cat {FDR/CSP Viewer} Advanced $cspName
    } else {
       procViewCSPFile $cspName
    }
}
proc procCheckStatespaceWithFDR {} {
    if [prolog animation_mode(cspm)] {
        puts "Checking State Space Compliance with FDR"
		global strFilename
		set rootName [file rootname $strFilename]
		set cspName {}
		append cspName $rootName "_statespace.csp"
		prolog "tcltk_print_states_for_fdr('$cspName')"
		prologmnf preferences:get_preference(path_to_fdr,PCmd)
		set Cmd $prolog_variables(PCmd)
		puts "Calling $Cmd batch $cspName"
        catch {exec $Cmd batch $cspName} errfdrout
        # FDR always writes to stderr
        if [string match {*error*} $errfdrout] {
            tkErrorBox "Refinement checks failed!\nLog: $errfdrout"
            procExecutePathPrefOnFile path_to_fdr fdr2 {FDR} Advanced $cspName
        } else {
           puts "No error occurred."
        }
        puts "Finished Checking State Space Compliance with FDR"
    } else {
       tkErrorBox "This command can only be used in CSPM mode."
    }
}

proc procTypeCheckFile {} {
    global strFilename
   if [prolog animation_mode(cspm)] {
       procTypeCheckCSP
   } elseif [prolog animation_mode(b)] {
       procTypeCheckB $strFilename 1
   } else {
       tkErrorBox "This command can only be used in B or CSPM mode."
   }
}
proc procTypeCheckCSP {} {
    global strFilename
	prologmnf preferences:get_preference(path_to_csp_typechecker,PCmd)
	set Cmd $prolog_variables(PCmd)
	puts "Calling $Cmd $strFilename"

	if {[catch {set tc_res [exec $Cmd $strFilename]} errid]} {
			 set Result [split $errid \n]
			 HighlightLineErr $errid " "
			 procShowList $Result "CSP Type Check of $strFilename" "CSP Type Check FAILED !!"
	} else {
		 set Result [split $tc_res  \n]
		 if [HighlightLineErr $tc_res " "] {
			 procShowList $Result "CSP Type Check of $strFilename" "CSP Type Check found ERROR"
		 } else {
			 procShowList $Result "CSP Type Check of $strFilename" "CSP Type Check SUCCESSFUL"
		 }
	}
}
proc procTypeCheckB {BFilename highlighterr} {
	prologmnf preferences:get_preference(path_to_bcomp,PCmd)
	set Cmd $prolog_variables(PCmd)
	set pwd [pwd]
	set newd [file dirname $BFilename]
	puts "cd $newd"
	cd $newd
	puts "Calling $Cmd -v -a -i $BFilename"

	if {[catch {set tc_res [exec $Cmd -v -a -i $BFilename]} errid]} {
			 set Result [split $errid \n]
			 if {$highlighterr==1} { HighlightBCompErr $errid }
			 procShowList $Result "B Type Check of $BFilename" "B Type Check FAILED !!"
	} else {
		 set Result [split $tc_res  \n]
		 if {$highlighterr==1 && [HighlightBCompErr $tc_res]} {
			 procShowList $Result "B Type Check of $BFilename" "B Type Check found WARNINGS"
		 } else {
			 procShowList $Result "B Type Check of $BFilename" "B Type Check SUCCESSFUL"
		 }
	}
	cd $pwd
}

proc procTypeCheck_PPF_BFile {} {
    global strFilename
    if [prolog animation_mode(b)] {
        set IRName "ProB_Typed_Internal_Representation"
		set tmp_PPF_File "/tmp/$IRName.mch"
		prolog b_write_machine_representation_to_file(all,'$tmp_PPF_File')
		procTypeCheckB $tmp_PPF_File 0
    } else {
       tkErrorBox "This command can only be used in B mode."
    }
}

proc procViewCSPFile {psName} {
   procExecutePathPrefOnFile path_to_probe cat {FDR/CSP Viewer} Advanced $psName
}

proc procSaveStateToSPIN {} {
    global strFilename
    set rootName [file rootname $strFilename]
    set cspName {}
    append cspName $rootName "_statespace.prom"
    prolog "tcltk_print_states_for_spin('$cspName')"
    procOpenSPINFile $cspName
}

proc procSaveStateToNeverClaim {} {
    global strFilename
    set rootName [file rootname $strFilename]
    set neverName {}
    append neverName $rootName "_neverclaim.pml"
    prolog "promela_save_as_neverclaim('$strFilename', '$neverName')"
}

proc procOpenSPINFile {psName} {
   procExecutePathPrefOnFile path_to_spin xspin {Spin/Promela Viewer} Advanced $psName
}



# ------------------------------------------------------------

proc procCreateStateViewer {} {
	# destroy .stateviewer
	set f .stateviewer
	if [Dialog_Create $f "State Graphic Viewer" -borderwidth 10] {
		message $f.msg -text "Graphical Visualisation" -aspect 1000
		frame $f.sviewFrame -borderwidth .1c -relief groove
		#label $f.sviewFrame.label1 -text empty
		#pack $f.sviewFrame.label1 -side right
		pack $f.msg $f.sviewFrame -side top
		pack $f.sviewFrame -pady 5 -expand yes -fill both
	}
}
proc procUpdateStateViewer {} {
   global strFilename tcl_dir
   set f .stateviewer
   if [prolog preferences:get_preference(use_tk_custom_state_viewer,true)] {
	   if [prolog tcltk_get_image_list(Images,Contents)] {
		   if [winfo exists $f] {
			   foreach i [winfo children $f.sviewFrame] {
				  destroy $i
			   }
		   } else {
			   procCreateStateViewer
		   }
		  set imagelist [split $prolog_variables(Images)]
		  set path [file dirname $strFilename]
		  set notfound ""
		  #puts "app_dir: '$app_dir'"
		  foreach {label img} $imagelist {
		    if [file exists $path/$img] {
		        image create photo $label -format gif -fil $path/$img
		    } elseif [file exists $img] {
		        image create photo $label -format gif -fil $img
		    } elseif [file exists $tcl_dir/$img] {
		        image create photo $label -format gif -fil $tcl_dir/$img
		    } else {
		        append notfound " $img"
		    }
		  }
		  if {$notfound != ""} {
		        tkErrorBox "Cannot find gif image(s):$notfound.\nPaths: $path ; $tcl_dir"
		  } else {
			  set contents_list [split $prolog_variables(Contents)]
			  foreach {type framename label} $contents_list {
			     # puts "Generating content $type $framename $label"
				 if {$type=="frame"} {
					 # frame $f.sviewFrame.$framename -borderwidth .1c -relief flat
					 frame $f.sviewFrame.$framename -relief flat
					 pack $f.sviewFrame.$framename -side $label
				 } elseif {[string range $type 0 3]=="text"} {
					 label $f.sviewFrame.$framename.$type -text "$label"
					 pack $f.sviewFrame.$framename.$type -side left
				 } else {
					 label $f.sviewFrame.$framename.$type -image $label
					 pack $f.sviewFrame.$framename.$type -side left
				 }

			  }
		  }
		  procShowErrors
	   } else { destroy $f }
   } else { destroy $f }
}


proc procStartLindaServer {} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
	   #exec sicstus -l linda_server.pl&
	   prolog linda_slave:tcltk_connect_from_file('linda_server.log','$strFilename')
	   puts "Done"
    } else {
       tkErrorBox "No File open. Open a B Machine first."
    }
}

proc procServeLindaServer {} {
   if [prolog linda_slave:tcltk_start_serve_requests_from_file('linda_server.log',Machine)] {
	   global strFilename
	   set strFilename $prolog_variables(Machine)
	   procLoadBFile
	   puts "Start Serving"
	   prolog linda_slave:tcltk_serve_requests
	   puts "Finished"
	   tkMessageBox "Finished Serving Requests."
  } else {
	 tkErrorBox "Could not connect to Linda Server on this machine."
  }
}

proc procServeSpecificLindaServer {} {
  set Server [Dialog_Prompt "Enter Linda Server (Host:Port)"]
  if {$Server != ""} {
      set HostPortList [split $Server :]
      set Host [lindex $HostPortList 0]
      set Port [lindex $HostPortList 1]
      if [prolog linda_slave:tcltk_start_serve_requests('$Host',$Port,Machine)] {
		  global strFilename
		  set strFilename $prolog_variables(Machine)
		  procLoadBFile
		  #puts "Start Serving"
		  prolog linda_slave:tcltk_serve_requests
		  #puts "Finished"
		  tkMessageBox "Finished Serving Requests."
      } else {
         tkErrorBox "Could not connect to $Server."
      }
  }
}

proc procStopLindaServer {} {
   prolog linda_slave:tcltk_stop_server
}


############################## (ent03r) Op/Argument chooser creation and listeners ########
# populate a given listbox with each variable in a prolog query e.g. getHistory(X), :. for each x in X.
	proc procPopulateListBox {listbox query listname} {
		prolog $query
		catch {
			foreach i $prolog_variables($listname) {
				$listbox insert end $i;
			}
		}
	}

	# update the names and colours of operations in this listbox
	proc procUpdateOperationsListBox {{clear_old 0}} {
		global graph_type_rb argChooser_LastOperationClickedIndex;

		if {$clear_old != 0} {
			.argChooser.frame.list delete 0 end;
		}

		# populate the listbox
		procPopulateListBox .argChooser.frame.list "reduce_graph_state_space:get_alphabet_signatures(A)" "A";

		#### code here to initially set the items colour
		# colour the items red if theyre a dont care, black otherwise.
		for {set i 0} {$i < [.argChooser.frame.list size]} {incr i 1} {
			set itemname [.argChooser.frame.list get $i];
			# if its a desired transition for this particular graph
			if [prolog "reduce_graph_state_space:desired_user_transition($graph_type_rb,'$itemname')"] {
				.argChooser.frame.list itemconfigure $i -foreground "black" -selectforeground "black";
			} else {
				.argChooser.frame.list itemconfigure $i -foreground "red" -selectforeground "red";
			}
		}

		# select the last clicked on operation
		if {($argChooser_LastOperationClickedIndex >= 0) 		  && ($argChooser_LastOperationClickedIndex < [.argChooser.frame.list size])} {
			.argChooser.frame.list selection set $argChooser_LastOperationClickedIndex;
			.argChooser.frame.list itemconfigure $argChooser_LastOperationClickedIndex -selectforeground [.argChooser.frame.list itemcget $argChooser_LastOperationClickedIndex -foreground]
		}
	}

	# update the names and colours of arguments in this listbox
	proc procUpdateArgumentsListBox {operation {clear_old 0}} {
		global graph_type_rb argChooser_LastArgumentClickedIndex;

		if {$clear_old != 0} {
			.argChooser.frame.list2 delete 0 end;
		}

		procPopulateListBox .argChooser.frame.list2 "reduce_graph_state_space:assert_all_transition_arg_dont_cares,reduce_graph_state_space:get_transition_arguments_from_source('$operation', Arguments)" "Arguments";

		# colour the items red if theyre a dont care, black otherwise.
		if [prolog "reduce_graph_state_space:transition_arg_dont_cares($graph_type_rb,'$operation',DontCares)"] {
			for {set i 0} {$i < [llength $prolog_variables(DontCares)]} {incr i 1} {
				set element [lindex $prolog_variables(DontCares) $i];
				if {[string compare $element "_"] == 0} {
					.argChooser.frame.list2 itemconfigure $i -foreground "red" -selectforeground "red";
				} else {
					.argChooser.frame.list2 itemconfigure $i -foreground "black" -selectforeground "black";
				}
			}
		}

		# select the last clicked on argument
		if {($argChooser_LastArgumentClickedIndex >= 0) 		  && ($argChooser_LastArgumentClickedIndex < [.argChooser.frame.list2 size])} {
			.argChooser.frame.list2 selection set $argChooser_LastArgumentClickedIndex;
			.argChooser.frame.list2 itemconfigure $argChooser_LastArgumentClickedIndex -selectforeground [.argChooser.frame.list2 itemcget $argChooser_LastArgumentClickedIndex -foreground]
		}
	}

	# tell prolog which arguments we dont care about
	proc procSetDontCares {} {
		# get the last clicked operation
		global argChooser_LastOperationClicked graph_type_rb;

		# get the transition arg dont cares for this operation == Y
		prolog "reduce_graph_state_space:get_transition_arguments_from_source('$argChooser_LastOperationClicked', Arguments)";

		# the new list of dont cares
		set dontCares "\[";

		set elementsInLB [.argChooser.frame.list2 get 0 end];	# the elements in the listbox
		set sizeLB [llength $elementsInLB];

		# loop through the elements in this list box
		for {set i 0} {$i < $sizeLB} {incr i 1} {
			set elementCol [.argChooser.frame.list2 itemcget $i -foreground];
			if {[string compare $elementCol "red"] == 0} {
				append dontCares " '_'";
			} else {
				append dontCares [lindex $elementsInLB $i];
			}

			if {$i < [expr $sizeLB-1]} {
				append dontCares ",";
			} else {
				append dontCares "\]";
			}
		}

		# set the dont cares
		prolog "reduce_graph_state_space:assert_transition_dont_cares($graph_type_rb,'$argChooser_LastOperationClicked', $dontCares)";
	}

	proc procSettingsForDifferentGraph {graphtype} {
		global argChooser_LastOperationClicked;
		if {[string compare $graphtype "signature_merge"] == 0} {
			.argChooser.frame.oparglabel configure -state disabled
			.argChooser.frame.allarg_cb configure -state disabled
			.argChooser.frame.list2 delete 0 end;
		} else {
			.argChooser.frame.list2 delete 0 end;
			if {([string compare $argChooser_LastOperationClicked ""] != 0) 			     && [prolog "reduce_graph_state_space:desired_user_transition($graphtype, '$argChooser_LastOperationClicked')"]} {
				procUpdateArgumentsListBox $argChooser_LastOperationClicked
			}
			.argChooser.frame.oparglabel configure -state normal
			.argChooser.frame.allarg_cb configure -state normal
		}

		# set checkboxes selected or not according to sicstus db
		if [prolog reduce_graph_state_space:use_all_operations($graphtype,yes)] {
			.argChooser.frame.allop_cb select
		} else {
			.argChooser.frame.allop_cb deselect
		}
		if [prolog "reduce_graph_state_space:use_no_arguments($graphtype,yes)"] {
			.argChooser.frame.allarg_cb select
		} else {
			.argChooser.frame.allarg_cb deselect
		}
	}

	proc procUpdateAllOps {} {
		global graph_type_rb allop_cb;
		prologmnf "reduce_graph_state_space:set_use_all_operations($graph_type_rb,$allop_cb)";
		procUpdateOperationsListBox 1;
	}

	proc procUpdateAllArgs {} {
		global graph_type_rb argChooser_LastOperationClicked allarg_cb;
		prologmnf "reduce_graph_state_space:set_use_no_arguments($graph_type_rb,$allarg_cb)";
		procUpdateArgumentsListBox $argChooser_LastOperationClicked 1;
	}

	proc procShowArgChooser {} {
		global argChooser_LastOperationClicked graph_type_rb allop_cb allarg_cb argChooser_LastOperationClickedIndex argChooser_LastArgumentClicked argChooser_LastArgumentClickedIndex;
		set argChooser_LastOperationClicked ""
		set argChooser_LastArgumentClicked ""
		set argChooser_LastOperationClickedIndex -1
		set argChooser_LastArgumentClickedIndex -1
		set graph_type_rb "signature_merge"


		if [prolog "reduce_graph_state_space:use_all_operations($graph_type_rb,yes)"] {
			set allop_cb "yes"
		} else {
			set allop_cb "no"
		}

		if [prolog "reduce_graph_state_space:use_no_arguments($graph_type_rb,yes)"] {
			set allarg_cb "yes"
		} else {
			set allarg_cb "no"
		}


		destroy .argChooser;
		if [Dialog_Create .argChooser "Operation/Argument Chooser"] {
			wm geometry .argChooser +300+300;
			# top label
			label .argChooser.msg -padx 5 -pady 10 -wraplength 4i -justify left -text "Select which operations & arguments to use in Reduced Visted States / Reduced DFA graph. To select, click on an item in the list."
			pack .argChooser.msg -side top

			frame .argChooser.onoff -width 100 -height 50

			label .argChooser.onoff.on -padx 5 -pady 1 -foreground "red" -wraplength 4i -justify left -text "Red = Do not use"
			pack .argChooser.onoff.on -side left
			label .argChooser.onoff.off -padx 5 -pady 1 -foreground "black" -wraplength 4i -justify left -text ",  Black = Use"
			pack .argChooser.onoff.off -side left

			pack .argChooser.onoff -side top


			# bottom buttons
			frame .argChooser.buttons
			pack .argChooser.buttons -side bottom -fill x -pady 2m
			button .argChooser.buttons.finish -text Finish -command {destroy .argChooser}
			pack .argChooser.buttons.finish -side left -expand 1

			# make the main widget frame
			frame .argChooser.frame -borderwidth 10
			pack .argChooser.frame -side top -expand yes -fill y

			# make the scroll bars and listbox
			scrollbar .argChooser.frame.yscroll -command ".argChooser.frame.list yview"
			scrollbar .argChooser.frame.xscroll -orient horizontal 			    -command ".argChooser.frame.list xview"
			listbox .argChooser.frame.list -width 30 -height 10 -setgrid 1 			-yscroll ".argChooser.frame.yscroll set" -xscroll ".argChooser.frame.xscroll set" 			-foreground "black" -selectforeground "black"

			# bind for the operations list box
			bind .argChooser.frame.list <ButtonRelease-1> {
			if {[.argChooser.frame.list size] > 0} {
					global argChooser_LastOperationClicked argChooser_LastOperationClickedIndex graph_type_rb allop_cb;
					set selectedi [.argChooser.frame.list curselection];
					set selectedw [.argChooser.frame.list get $selectedi];
					set argChooser_LastOperationClicked $selectedw;

					# we click on an item in the list once to select it, and then
					# a second time to cause the change
					if {$argChooser_LastOperationClickedIndex != [.argChooser.frame.list curselection]} {
						set argChooser_LastOperationClickedIndex [.argChooser.frame.list curselection];

						set list1itemc [.argChooser.frame.list itemcget $selectedi -foreground];
						if {[string compare $list1itemc ""] == 0} {
							set list1itemc "black";
						}
						if {([string compare $list1itemc "black"] == 0) && ([string compare $graph_type_rb "dfa_abstraction"] == 0)} {
							procUpdateArgumentsListBox $argChooser_LastOperationClicked 1;
						} else {
							.argChooser.frame.list2 delete 0 end;
						}
					} else {
						prologmnf reduce_graph_state_space:user_made_op_selection;

						################## add list item listeners to change colour on click ##################
						set list1itemc [.argChooser.frame.list itemcget $selectedi -foreground];
						if {[string compare $list1itemc ""] == 0} {
							set list1itemc "black";
						}

						if {[string compare $list1itemc "red"] == 0} {
							.argChooser.frame.list itemconfigure $selectedi -foreground "black" -selectforeground "black";
							prolog "(reduce_graph_state_space:desired_user_transition($graph_type_rb,'$selectedw') -> true;assertz(reduce_graph_state_space:desired_user_transition($graph_type_rb,'$selectedw')))";

							if {[string compare $graph_type_rb "dfa_abstraction"] == 0} {
								procUpdateArgumentsListBox $argChooser_LastOperationClicked 1;
							}
						} else {
							.argChooser.frame.list itemconfigure $selectedi -foreground "red" -selectforeground "red";
							prolog "retractall(reduce_graph_state_space:desired_user_transition($graph_type_rb,'$selectedw'))."
							set allop_cb "no";
							prologmnf reduce_graph_state_space:set_use_all_operations($graph_type_rb,$allop_cb);

							.argChooser.frame.list2 delete 0 end;
						}
					}
				}
			}

			# check if user has made any changes to selected operations
			# if not, then all operations are ON by default..
			if [prolog reduce_graph_state_space:user_has_made_op_selection(no)] {
				prologmnf reduce_graph_state_space:set_use_all_operations(signature_merge,yes);
				prologmnf reduce_graph_state_space:set_use_all_operations(dfa_abstraction,yes);
			}
			# set up the operations list box
			procUpdateOperationsListBox 1;

			# make some more scroll bars and listbox
			scrollbar .argChooser.frame.yscroll2 -command ".w.frame.list2 yview";
			scrollbar .argChooser.frame.xscroll2 -orient horizontal 			    -command ".argChooser.frame.list2 xview";
			listbox .argChooser.frame.list2 -width 30 -height 10 -setgrid 1 			    -yscroll ".argChooser.frame.yscroll2 set" -xscroll ".argChooser.frame.xscroll2 set";

			# bind for the arguments list box
			bind .argChooser.frame.list2 <ButtonRelease-1> {
				if {[.argChooser.frame.list2 size] > 0} {
					global argChooser_LastArgumentClicked argChooser_LastArgumentClickedIndex;

					if {$argChooser_LastArgumentClickedIndex != [.argChooser.frame.list2 curselection]} {
						set argChooser_LastArgumentClickedIndex [.argChooser.frame.list2 curselection];
					} else {

						set cursel [.argChooser.frame.list2 curselection];
						set currentcol [.argChooser.frame.list2 itemcget $cursel -foreground];
						prologmnf reduce_graph_state_space:user_made_arg_selection;

						if {[string compare $currentcol ""] == 0} {
							set currentcol "red";
						}
						set unhighlighted "black";
						set highlighted "red";
						if {[string compare $currentcol $unhighlighted] == 0} {
							.argChooser.frame.list2 itemconfigure $cursel -foreground $highlighted -selectforeground $highlighted;
						} else {
							.argChooser.frame.list2 itemconfigure $cursel -foreground $unhighlighted -selectforeground $unhighlighted;
							global allarg_cb graph_type_rb;
							set allarg_cb "no";
							prologmnf "reduce_graph_state_space:set_use_no_arguments($graph_type_rb,no)";
						}
						procSetDontCares;
					}
				}
			}

			# dummy label that provides an empty section
			label .argChooser.frame.label1 -relief flat -padx 5 -pady 5

			label .argChooser.frame.opnamelabel -text "Operation Name"
			label .argChooser.frame.oparglabel -text "Arguments/Return values (for reduced DFA only)"

			radiobutton .argChooser.frame.sig_m_rb -text "Signature Merge" -pady 2 -variable graph_type_rb -value "signature_merge" -command "procSettingsForDifferentGraph signature_merge;procUpdateAllOps"
			radiobutton .argChooser.frame.dfa_a_rb -text "DFA Abstraction" -pady 2 -variable graph_type_rb -value "dfa_abstraction" -command "procSettingsForDifferentGraph dfa_abstraction;procUpdateAllOps" -state disabled
			# TO DO: remove state disabled

			checkbutton .argChooser.frame.allop_cb -text "All operations on" -pady 2 -variable allop_cb -onvalue "yes" -offvalue "no" -command "procUpdateAllOps"
			checkbutton .argChooser.frame.allarg_cb -text "All arguments/return values off" -pady 2  -variable allarg_cb -onvalue "yes" -offvalue "no" -command "procUpdateAllArgs"

			.argChooser.frame.sig_m_rb invoke;

			# I think this bit puts the first lot of scrollbars and listbox into the main frame
			grid .argChooser.frame.opnamelabel -row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
			grid .argChooser.frame.oparglabel -row 0 -column 3 -rowspan 1 -columnspan 1 -sticky news

			grid .argChooser.frame.list -row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
			grid .argChooser.frame.yscroll -row 1 -column 1 -rowspan 1 -columnspan 1 -sticky news
			grid .argChooser.frame.xscroll -row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news

			grid .argChooser.frame.label1 -row 1 -column 2 -rowspan 1 -columnspan 1 -sticky news

			grid .argChooser.frame.list2 -row 1 -column 3 -rowspan 1 -columnspan 1 -sticky news
			grid .argChooser.frame.yscroll2 -row 1 -column 4 -rowspan 1 -columnspan 1 -sticky news
			grid .argChooser.frame.xscroll2 -row 2 -column 3 -rowspan 1 -columnspan 1 -sticky news

			grid .argChooser.frame.sig_m_rb -row 3 -column 0 -columnspan 1 -sticky news
			grid .argChooser.frame.allop_cb -row 3 -column 3 -columnspan 1 -sticky news

			grid .argChooser.frame.dfa_a_rb -row 4 -column 0 -columnspan 1 -sticky news
			grid .argChooser.frame.allarg_cb -row 4 -column 3 -columnspan 1 -sticky news

			grid rowconfig    .argChooser.frame 0 -weight 1 -minsize 0
			grid columnconfig .argChooser.frame 0 -weight 1 -minsize 0
		}
	}
############################# ent03r End Op/Argument Chooser #########################





# ------------------------------------------------------------
# MAIN PROGRAM
# ------------------------------------------------------------

proc procComputeApplicationDirectory {} {
    global app_dir
    global lib_dir
    global tcl_dir
    # puts "procComputeApplicationDirectory"
    prolog pathes:runtime_application_path(Dir)
    set app_dir $prolog_variables(Dir)
    prolog absolute_file_name(prob_lib('.'),LibDir)
    set lib_dir $prolog_variables(LibDir)
    prolog absolute_file_name(prob_tcl('.'),TclDir)
    set tcl_dir $prolog_variables(TclDir)
	prolog "tools:print_message('app_dir=$app_dir; lib_dir=$lib_dir; tcl_dir=$tcl_dir')"
}

proc getMachinesPath {} {
    prolog preferences:get_preference(machines_path,Dir)
    return $prolog_variables(Dir)
}
proc setMachinesPath {dir} {
    if {$dir != ""} {
	prolog preferences:set_preference(machines_path,'$dir')
    }
}

# -------
# procedure initialise everythings
# -------
proc procMainInit {} {
    global lib_dir
    # procComputeApplicationDirectory

    global curFileTypeOpened
    set curFileTypeOpened "None"

    global strFilename cspstrFilename
    set strFilename ""
    set cspstrFilename ""

    # set the first machine (a phonebook)
    prologmnf tcltk_set_initial_machine

    #procInitPreferences
    #procLoadPreferences

    # initialise GUI
    procInitGUI
    # initialise prolog
    prologmnf tcltk_initialise

    # get options by default
    #procInsertHistoryOptionsState

    #procUpdateMenusFromPrologInfo
    global typeCheckMode traceUponError
    global expert_user
    set traceUponError 0
    if {$expert_user && \
     [prolog preferences:get_preference(default_to_runtime_type_checking_on_startup_for_expert,true)]} {
       set typeCheckMode 1
     } else {set typeCheckMode 0}
    #procUpdateDebuggingMode

    if [file exists "$lib_dir/probcliparser.jar"] {
      # puts "Found Parser"
    } else {
      tkErrorBox "Your installation seems incomplete.\nCannot find probcliparser.jar file in $lib_dir.\nYou will only be able to open machines that have already been parsed."
    }
}


# puts "Starting"
procMainInit
# puts "Treating CommandLineArgs"
#procTreatCommandLineArgs




