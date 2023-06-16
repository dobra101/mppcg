# (c) 2009-2023 Lehrstuhl fuer Softwaretechnik und Programmiersprachen,
# Heinrich Heine Universitaet Duesseldorf
# This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html)

# ------------------------------------------------------------
# ProB TCL file
# main_prob_tcltk_gui.tcl
# ------------------------------------------------------------
# --------
# procedure to initialise menu section
# --------
   # source btracer_pure_procedures.tcl # done in btracer_tcl.pl

   # internationalisation section (should later be put into a separate file):
   global prob_lang_str prob_cur_lang
   set prob_cur_lang "en"
   array set prob_lang_str {}
   # Unicode (see http://unicode-table.com) é -> \u00e9   à -> \u00e0
   set prob_lang_str(wrong_tcltk_version,en) "You have Tcl/Tk Version $tcl_version.\nProB may not work correctly.\nPlease install version 8.4 or newer."
   set prob_lang_str(wrong_tcltk_version,fr) "Vous avez Tcl/Tk en version $tcl_version.\nIl est possible que ProB ne fonctionne pas bien.\nInstallez Tcl/Tk au moins en version 8.4."
   set prob_lang_str(error_occured,en) "An Error Occured"
   set prob_lang_str(error_occured,fr) "Une erreur est apparue"
   set prob_lang_str(show_line_number,en) "Show Line Number:"
   set prob_lang_str(show_line_number,fr) "Sauter \u00e0 la ligne num\u00e9ro:"
   set prob_lang_str(invalid_line_number,en) "Invalid input. Please enter a valid number."
   set prob_lang_str(invalid_line_number,fr) "Donn\u00e9e invalide. Entrez un num\u00e9ro de ligne"

# a procedure to retrieve a language specific string:
proc prob_str {key} {
    global prob_lang_str prob_cur_lang
    return $prob_lang_str($key,$prob_cur_lang)
}

proc prob_read {strFilename {encoding utf-8}} {
    set fid [prob_open $strFilename r $encoding]
    set strInFile [read $fid]
    close $fid
    return $strInFile
}

proc prob_open {strFilename {mode w} {encoding utf-8}} {
    set fid [open $strFilename $mode]
    fconfigure $fid -encoding $encoding
    return $fid
}

proc proc_updateHighlightingPreferencesComments {} {
  global multiline_cmt_color multiline_pragma_color
  prolog preferences:get_preference(sh_comments,Colour)
  set multiline_cmt_color $prolog_variables(Colour)
  prolog preferences:get_preference(sh_pragmas,Colour)
  set multiline_pragma_color $prolog_variables(Colour)
}

global batch_mode
set batch_mode 0
if {$tcl_version<8.4} {
  tk_messageBox -parent . -icon error -message "[prob_str wrong_tcltk_version]"
}
global tcl_patchlevel
set tcl_patchlevel [info patchlevel]
global tcl_table_package_missing_info
if {[catch {package require Tktable} cerr]} {
  puts "### Warning: Cannot use Tktable! Reason: $cerr"
  set tcl_table_package_missing_info " (without tktable, $cerr)"
} elseif {$tcl_patchlevel == "8.6.9" && [prolog tools:host_platform(darwin)]} {
		  # probably Active's buggy Tcl/Tk version installed
		  # TODO: should we do this only on macOS Monterey?
	set tcl_table_package_missing_info " (tktable disabled to avoid crashes)"
} else {
  set tcl_table_package_missing_info ""
}
# puts "Tcl Version $tcl_version"
# see also check for darwing and tcl_patchlevel below
global version shortversion prologversion
prolog version:version(V1,V2,V3,Suffix)
set shortversion "$prolog_variables(V1).$prolog_variables(V2).$prolog_variables(V3)"
set version "$shortversion-$prolog_variables(Suffix)"
prolog current_prolog_flag(version,PV)
set prologversion "$prolog_variables(PV)"
global revision lastchangeddate
prolog version:revision(Rev)
set revision $prolog_variables(Rev)
prolog version:lastchangeddate(LCD)
set lastchangeddate $prolog_variables(LCD)
global normal_user expert_user
set normal_user 0
set expert_user 0
global show_error_if_no_transition
set show_error_if_no_transition 1
global strFilename logFile currentRefSpecFile cspstrFilename curFileDisplayed UsedCspStrFilename
set  strFilename ""
set  logFile ""
set curFileDisplayed "NONE"
set UsedCspStrFilename ""
global forceRecompile
set forceRecompile 1
set  currentRefSpecFile ""
set  cspstrFilename ""
global testing_mode failed_tests detailed_errors
set testing_mode false
set failed_tests ""
set detailed_errors ""
global only_eventb_menues
global multiline_cmt_color
global multiline_pragma_color
proc_updateHighlightingPreferencesComments
global csp_assertions_dialog_arr
array set csp_assertions_dialog_arr {
fontname 			"TkFixedFont"
fontsize 			10
background 			"Gray90"
selectcolor 			"gray60"
spec 				""
impl 				""
ref_model 	        	"Trace"
deterministic_proc   		""
deterministic_model		"Failures"
deadlock_proc 			""
deadlock_model 			"Failures"
livelock_proc 			""
ltl_proc            ""
ltl_entry           ""
ctl_proc            ""
ctl_entry           ""
selected_notebook_type 		"Refinement"
assertion_check_running		false
listbox_size			0
current_new_assertions		{}
all_new_assertions		{}
empty_list_msg "No Assertions have been added."
visualize_process   ""
visualizing_status "No information available."
}
global cbc_default_checks
global cbc_arr
array set cbc_arr {
   fontname 			"TkFixedFont"
   fontsize 			10
   background 			"Gray90"
   selectcolor 			"gray60"
   selected_notebook_type "Ivariant"
 invariant_operation ""
 operations_sequence "Enter sequence of operations, separated by ','"
 b_predicate "Enter GOAL Formula, use B syntax"
 listbox_size 0
 cbc_formulas {}
}

global eval_window_arr
array set eval_window_arr {
    b_hist_index   0
    csp_hist_index 0
    input          ""
    input_index    "2.5"
    b_history      {}
    csp_history    {}
    proCSPInterpreter true
    use_as "evaluator"
}
set only_eventb_menues [list]
global find_menu_arr
array set find_menu_arr {
    case         false
    word         false
    backwards    false
    wraparound   true
    allpatterns  false
    search       ""
    replace      ""
    exact_flag   ""
    found_index  ""
    direction    "-forwards"
    search_hist  {}
    replace_hist {}
}
global text_editor_arr
array set text_editor_arr {
    ctext_package   			false
    text_index      			"1.0"
    file_modified   			false
    file_last_modified			""
    current_timestamp			""
    high_line_index 			0
    reopening_file			        false
    deactivate_asking_for_reloading		false
    font_name				""
    font_size				""
    highlight_brackets			true
    highlight_line				true
    linenumbers				false
    ask_when_content_changed		false
    last_timestamped_file			""
    disable_comments_highlighting		false
    disable_highlighting			false
}
global ltldata
array set ltldata {
fontname    "TkFixedFont"
fontsize    10
background  "Gray90"
selectcolor	"gray60"
formula	""
mode		""
max	""
ok		false
hist		{}
ltl_viewer_formula ""
ctl_viewer_formula ""
ltl_info "Use \{...\} for B predicates, supported LTL operators: G,F,X,U,W,R,true,false..."
ctl_info "Use \{...\} for B predicates, supported CTL operators: ExUy,EXx,AXx,EFx,AGx..."
type "ltl"
checkall_type ""
empty_list_msg "No ASSERT_LTL/CTL in the DEFINITIONS."
ltl_formulas {}
ctl_formulas {}
use_por_for_ltl false
use_safety_ltl_model_checker false
}

global tlc_array
array set tlc_array {
   gstates "-"
   dstates "-"
   qstates "-"
   result "No information produced."
   output ""
   color "black"
   mctime "-"
}

global check_marks_arr

global model_checker_arr
array set model_checker_arr {
  pge off
  use_pge false
  use_por false
  por off
  enable_graph false
  enable_graph_depth -1
  dependency_enable_predicates false
}
global CBCEntry
set CBCEntry ""
global PORTechniqueVar
namespace import ::evaluationView::*
# namespace import ::astInspector::*
# this is now replaced by more generic:
namespace import ::treeInspector::*
namespace import ctext::*

   # patch: to resolve the problem that displaying error messages hangs in Active Tcl/Tk 8.6 or 8.5.9.2
   # question: does it affect Linux/Mac ?? if [prolog tools:host_platform(darwin)]
proc bgerror {msg} {
   tk_messageBox -icon info -message $msg
}

proc procChooseSystemDependentUnicodeSymbols {} {
	global check_marks_arr tcl_version
	if [procIsOSAnOlderVersionOfWindowsMS] {
	    array set check_marks_arr {
	 	   unchecked "?"
	       success   "TRUE"
	 	   failed    "FALSE"
	 	   checking  "checking..."
	 	   aborted   "ABORTED"
           incomplete "INCOMPLETE"
	    }
	} elseif {$tcl_version<8.5 || ![prolog tools:host_platform(darwin)]} {
	    array set check_marks_arr {
	 	   unchecked "?"
	       success    "\u2714"
	 	   failed     "\u2718"
	 	   checking   "\u231A"
	 	   aborted    "\u2762"
           incomplete "\u221E"
	    }
	} else {
	    array set check_marks_arr {
	 	   unchecked  "\u2754"
	       success    "\u2705"
	 	   failed     "\u274C"
	 	   checking   "\u23F3"
	 	   aborted    "\u2757"
           incomplete "\u221E"
	    }
        # positive negation "\u274E"
	}
}

proc procGUI_Menu {} {
    global normal_user
    set normal_user [prolog preferences:get_preference(user_not_beginner,true)]
    global expert_user
    set expert_user [prolog preferences:get_preference(user_is_an_expert_with_accessto_source_distribution,true)]

#    frame .frmMenu
    frame .main -width 860 -height 680
    # top level menu bar
    # option add *tearoff 0
    menu .frmMenu -tearoff 0

    # -tearoff 0 was removed for Leopard
    . config -menu .frmMenu
    # if {[prolog tools:host_platform(darwin)] || [prolog tools:host_platform(windows)]} {
    #        bind .frmMenu <<MenuSelect>> {proc_updateMenuEntryStates}
    # } else {
    #        bind .frmMenu <Any-Motion> {proc_updateMenuEntryStates}
    # }

    foreach m {File Edit Animate Verify Analyse Visualize Preferences Debug Files Help} {
        set $m [menu .frmMenu.mnu$m -tearoff 0]
        .frmMenu add cascade -label $m -menu .frmMenu.mnu$m
    }

    # -------------------- file menu
#    menu .frmMenu.mnuFile -tearoff 0
    set AccKey [procGetAccKey]
    set AccKeyComb "Alt+$AccKey"
    if [prolog tools:host_platform(darwin)] {set CmtCmd "$AccKey+/"} else {set CmtCmd "$AccKey+Shift+T"}
    .frmMenu.mnuFile add command -label "New..." -underline 0 -command procNewFile -accelerator $AccKey+N
    .frmMenu.mnuFile add command -label "Open..." -underline 0 -command procOpenFile -accelerator $AccKey+O
    .frmMenu.mnuFile add command -label "Reopen" -underline 0 -command {procLaunchCmd procReOpenFile} -accelerator $AccKey+R -state disabled
    .frmMenu.mnuFile add command -label "Save" -underline 0 -command procSaveFile -accelerator $AccKey+S -state disabled
    .frmMenu.mnuFile add command -label "Save and Reopen" -command {procLaunchDescCmd "Save and Reopen" procSaveAndReopenFile} -state disabled
    .frmMenu.mnuFile add command -label "Save As..." -command {procLaunchDescCmd "Save As..." procSaveAsFile} -state disabled -accelerator $AccKey+Shift+S
    if {$normal_user} {
      .frmMenu.mnuFile add sep
      .frmMenu.mnuFile add command -label "Save Statespace" -command {procLaunchDescCmd "Save Statespace" procSaveState} -state disabled
      .frmMenu.mnuFile add command -label "Load Saved Statespace" -command {procLaunchDescCmd "Load Statespace" procLoadState} -state disabled
    }
    .frmMenu.mnuFile add sep
		.frmMenu.mnuFile add command -label "Open in External Editor" -command {procLaunchDescCmd "Open in External Editor" procOpenFileInEditor} -state disabled -accelerator $AccKey+E
    if {$normal_user} {
		.frmMenu.mnuFile add cascade -label "Open Special" \
				-menu .frmMenu.mnuFile.mnuOpenSpecial
		.frmMenu.mnuFile add sep
    }
    .frmMenu.mnuFile add cascade -label "Recent Files" \
				-menu .frmMenu.mnuFile.mnuRecentDocuments
    .frmMenu.mnuFile add sep
    .frmMenu.mnuFile add command -label "Quit" -underline 0 -command procDoQuit -accelerator $AccKey+Q

     .frmMenu.mnuEdit add command -label "Undo" -command procundo -accelerator $AccKey+Z -state disabled
     .frmMenu.mnuEdit add command -label "Redo" -command procredo -state disabled -accelerator Shift-$AccKey+Z
    .frmMenu.mnuEdit add sep

    .frmMenu.mnuEdit add command -label "Cut" -command {procCutOrCopySlectedRangeOfText .main.frmSource.text "no"} -accelerator $AccKey+X -state normal
    .frmMenu.mnuEdit add command -label "Copy" -underline 0 -command {procCutOrCopySlectedRangeOfText .main.frmSource.text "yes"} -accelerator $AccKey+C -state normal
    .frmMenu.mnuEdit add command -label "Paste" -command {procPasteTheCopiedRangeOfText} -accelerator $AccKey+V -state normal
    .frmMenu.mnuEdit add command -label "Delete" -command {procLaunchDescCmd "Delete" procDeleteTheSelectedRangeOfText} -state normal
    .frmMenu.mnuEdit add sep
    .frmMenu.mnuEdit add command -label "Select All" -underline 7 -command {procLaunchDescCmd "Select All" {proc_selectAll .main.frmSource.text}} -accelerator $AccKey+A -state normal
    .frmMenu.mnuEdit add sep
    .frmMenu.mnuEdit add command -label "Toggle Comment" -command {procLaunchDescCmd "Toggle Comment" {proc_commmentText .main.frmSource.text}} -accelerator $CmtCmd -state normal
    .frmMenu.mnuEdit add sep

     global find_menu_arr
     prolog preferences:get_history_elements_list(searched_patterns,true,SearchedPat)
     prolog preferences:get_history_elements_list(replaced_patterns,true,ReplacedPat)
     set find_menu_arr(search_hist) $prolog_variables(SearchedPat)
     set find_menu_arr(replace_hist) $prolog_variables(ReplacedPat)

    .frmMenu.mnuEdit add cascade -label "Search" -menu .frmMenu.mnuEdit.mnuSearch -state normal
    menu .frmMenu.mnuEdit.mnuSearch -tearoff 0
    .frmMenu.mnuEdit.mnuSearch add command -label "Find..." -command {procLaunchCmd procLaunchFindMenu} -accelerator $AccKey+F
    .frmMenu.mnuEdit.mnuSearch add command -label "Replace..." -command {procLaunchCmd procLaunchFindAndReplaceMenu} -accelerator Shift-$AccKey+F
    .frmMenu.mnuEdit.mnuSearch add command -label "Goto Line..." -command {procLaunchCmd GotoLineSelect} -accelerator $AccKey+J
    .frmMenu.mnuEdit add sep
     .frmMenu.mnuEdit add command -label "Repeat" -command procReLaunchLastMenuCmd -state disabled -accelerator Shift-$AccKey+R
    .frmMenu.mnuEdit add sep
    .frmMenu.mnuEdit add command -label "Convert B Symbols to Unicode" -command {procLaunchCmd procUnicodeReplacement}

    # -------------------- Open Special menu
    menu .frmMenu.mnuFile.mnuOpenSpecial -tearoff 0
   .frmMenu.mnuFile.mnuOpenSpecial add command -label "Use CSP File to Guide B..." -command {procLaunchCmd procAddCSPFile}
   .frmMenu.mnuFile.mnuOpenSpecial add command -label "Use Default CSP File" -underline 7 -command {procLaunchCmd procAddDefaultCSPFile} -accelerator $AccKey+minus
   #.frmMenu.mnuFile.mnuOpenSpecial add command -label "Add XTL File..." -command procAddXTLFile

    # -------------------- Open Special menu
    menu .frmMenu.mnuFile.mnuRecentDocuments -tearoff 0
    procRebuildRecentDocumentsMenu

    # -------------------- Animate menu
#    menu .frmMenu.mnuAnimate -tearoff 0
    .frmMenu.mnuAnimate add command -label "Reset" -command {procLaunchDescCmd "Reset" procReset}
    .frmMenu.mnuAnimate add sep
    # .frmMenu.mnuAnimate add command -label "Random Animation (10)" -command {procLaunchDescCmd "Random Animation (10)" procRand} -accelerator $AccKey+Shift+X
    .frmMenu.mnuAnimate add command -label "Random Animation Steps..." -command {procLaunchDescCmd "Random Animation..." procRandAny}
    if {$expert_user} {
        .frmMenu.mnuAnimate add command -label "Animate until LTL..." -command {procLaunchDescCmd "Animate until..." procAnimateUntil}
        .frmMenu.mnuAnimate add command -label "Execution Steps..." -command {procLaunchDescCmd "Execute..." procExecute}

    }
    .frmMenu.mnuAnimate add sep
    .frmMenu.mnuAnimate add command -label "Find State Satisfying Predicate..." \
        -command {procLaunchDescCmd "Find State Satisfying Predicate..." procAdvancedFind}  -accelerator $AccKey+Y
    .frmMenu.mnuAnimate add cascade -label "Find Existing State" \
        -menu .frmMenu.mnuAnimate.mnuFind
    if {$normal_user} {
        .frmMenu.mnuAnimate add sep
        .frmMenu.mnuAnimate add command -label "Execute Specific Operation..." -command {procLaunchDescCmd "Execute Specific Operation..." procExecuteOperation} -accelerator $AccKey+Shift+E
        .frmMenu.mnuAnimate add sep
		.frmMenu.mnuAnimate add command -label "Show Shortest Trace to Current State"\
				-command {procLaunchCmd procFindTrace}
		#.frmMenu.mnuAnimate add command -label "Execute Trace to Current State..." -command procExecTrace
		# .frmMenu.mnuAnimate add sep
		.frmMenu.mnuAnimate add command -label "Show Current B State"\
				-command {procLaunchCmd procShowCurrentState}
		.frmMenu.mnuAnimate add sep
		.frmMenu.mnuAnimate add command -label "Refocus On Current State and Discard Rest"\
				-command {procLaunchCmd procRefocusOnCurrentState}
    }



    # -------------------- Verify menu
#    menu .frmMenu.mnuVerify -tearoff 0
    .frmMenu.mnuVerify add command -label "Model Check..." -underline 0 -command {procLaunchDescCmd "Model Check..." procModelCheck} -accelerator $AccKey+M
   # .frmMenu.mnuVerify add cascade -label "Search Settings" \
        -menu .frmMenu.mnuVerify.mnuMCOptions
    .frmMenu.mnuVerify add sep
    #.frmMenu.mnuVerify add command -label "Check LTL Formula..." \
    #    -underline 6 -command procLtl -accelerator $AccKey+L
    .frmMenu.mnuVerify add command -label "Check LTL/CTL Assertions..." \
	    -command {procLaunchCmd OpenLtlAssertionsViewer} -accelerator $AccKey+L

    .frmMenu.mnuVerify add command -label "Check CSP-M Assertions..." \
        -command {procLaunchCmd CheckCspAssertions}
    #.frmMenu.mnuVerify add command -label "Check CTL Formula..." \
    #    -command procCtl
		.frmMenu.mnuVerify add sep
	   .frmMenu.mnuVerify add command -label "Trace Refinement Check..." \
		           -command {procLaunchCmd procRefinementCheck}
	   .frmMenu.mnuVerify add command -label "Save Statespace for Later Refinement Check" \
		           -command {procLaunchCmd procSaveSpecStateForRefinement}
    if {$normal_user} {
		.frmMenu.mnuVerify add sep
        .frmMenu.mnuVerify add cascade -label "Constraint Based Checking" \
            -menu .frmMenu.mnuVerify.mnuCBC
        .frmMenu.mnuVerify add command -label "Constraint Based Checking Viewer" \
	            -command openCBCViewer
        .frmMenu.mnuVerify add sep
        .frmMenu.mnuVerify add cascade -label "Symbolic Model Checking" \
                -menu .frmMenu.mnuVerify.mnuSymbolicMC
		.frmMenu.mnuVerify add sep
        .frmMenu.mnuVerify add cascade -label "External Tools" \
            -menu .frmMenu.mnuVerify.mnuRef
		.frmMenu.mnuVerify add sep
        .frmMenu.mnuVerify add cascade -label "Trace Checking" \
            -menu .frmMenu.mnuVerify.mnuTrace
		.frmMenu.mnuVerify add sep
        .frmMenu.mnuVerify add cascade -label "Static Checking" \
                -menu .frmMenu.mnuVerify.mnuStaticChecks
    }
    .frmMenu.mnuVerify add command -label "Alloy Command" \
        -command {procLaunchDescCmd "Alloy Command" {execInBZMode procVerifyAlloyCommand}}
    if {$expert_user} {
		.frmMenu.mnuVerify add sep
    .frmMenu.mnuVerify add command -label "Find GF GOAL" \
		           -command {procLaunchCmd procDFSGoalCycle}
	    }

         #CBC menu
    menu .frmMenu.mnuVerify.mnuCBC -tearoff 0
	.frmMenu.mnuVerify.mnuCBC add command -label "Check Invariant Preservation for Operations" \
		-command {procLaunchDescCmd "Check Invariant Preservation" {execInBZMode procConstraintBasedCheck}}
	.frmMenu.mnuVerify.mnuCBC add command -label "Check Invariant for Specific Operation..." \
		-command {procLaunchDescCmd "Check Invariant Preservation..." {execInBZMode procOpConstraintBasedCheck}}
    .frmMenu.mnuVerify.mnuCBC add sep
	.frmMenu.mnuVerify.mnuCBC add command -label "Find Sequence..." \
		-command {procLaunchDescCmd "Find Sequence..." {execInBZMode procCBCFindSequenceDialog}}
    .frmMenu.mnuVerify.mnuCBC add sep
	.frmMenu.mnuVerify.mnuCBC add command -label "Find Deadlock" \
		-command {procLaunchDescCmd "Find Deadlock" {execInBZMode procFindDeadlockedState}}
	.frmMenu.mnuVerify.mnuCBC add command -label "Find Deadlock Satisfying Predicate..." \
		-command {procLaunchDescCmd "Find Deadlock..." {execInBZMode procFindDeadlockedStateWithPred}}
    .frmMenu.mnuVerify.mnuCBC add sep
	.frmMenu.mnuVerify.mnuCBC add command -label "Check Static Assertions on Constants" \
		-command {procLaunchDescCmd "Check Static Assertions" {execInBZMode procFindStaticAssertionViolation}}
	.frmMenu.mnuVerify.mnuCBC add command -label "Check Dynamic Assertions on Variables" \
		-command {procLaunchDescCmd "Check Dynamic Assertions" {execInBZMode procConstraintFindAssertionViolation}}
	.frmMenu.mnuVerify.mnuCBC add command -label "Check Dynamic Assertion Preservation for Specific Operation..." \
		-command {procLaunchDescCmd "Check Assertion Preservation..." {execInBZMode procOpConstraintBasedCheckAss}}
	.frmMenu.mnuVerify.mnuCBC add sep
	.frmMenu.mnuVerify.mnuCBC add command -label "Refinement Checking" \
		-command {procLaunchDescCmd "Refinement Checking" {execInBZMode procCBCRefinementCheck}}
    .frmMenu.mnuVerify.mnuCBC add sep
	.frmMenu.mnuVerify.mnuCBC add command -label "Find State Satisfying Invariant and Predicate..." \
		-command {procLaunchDescCmd "Find State Satisfying Invariant..." {execInBZMode procConstraintFindValidStateWithGoalPred}}
	.frmMenu.mnuVerify.mnuCBC add command -label "Find Redundant Invariants" \
		-command {procLaunchDescCmd "Find Redundant Invariants" {execInBZMode procCBCFindRedundantInvariants}}
	if {$expert_user} {
    .frmMenu.mnuVerify.mnuCBC add sep
	.frmMenu.mnuVerify.mnuCBC add command -label "Find Maximal State Satisfying Invariant" \
		-command {procLaunchDescCmd "Find Maximal State Satisfying Invariant" {execInBZMode procConstraintFindMaximalValidState}}
		}
    .frmMenu.mnuVerify.mnuCBC add sep
	.frmMenu.mnuVerify.mnuCBC add command -label "Compute forced CONSTANTS..." \
		-command {procLaunchDescCmd "Compute forced CONSTANTS..." {execInBZMode procForcedConstantsTable}}

    menu .frmMenu.mnuAnalyse.mnuEnabling -tearoff 0
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Enabling Analysis (Table)" \
		-command {procLaunchDescCmd "Enabling Analysis" {execInBZMode procEnablingAnalysisShort}}
	if {$expert_user} {
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Enabling Analysis (Precise, Table)" \
		-command {procLaunchDescCmd "Enabling Analysis" {execInBZMode procEnablingAnalysisLong}}
	}
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Enabling Relations for..." \
		-command {procLaunchDescCmd "Enabling Relations for..." {execInBZMode procEnablingRelationsAfterOperation}}
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Enabling Analysis (Graph)" \
		-command {procLaunchDescCmd "Enabling Analysis" {execInBZMode procEnablingAnalysis_Graph}}
	if {$expert_user} {
  .frmMenu.mnuAnalyse.mnuEnabling add cascade -label "Enabling Analysis (POR)" \
    -menu .frmMenu.mnuAnalyse.mnuEnabling.por
  menu .frmMenu.mnuAnalyse.mnuEnabling.por -tearoff 0
  .frmMenu.mnuAnalyse.mnuEnabling.por add command -label "Table" \
    -command {procLaunchCmd {execInBZMode procEnablingAnalysisShort_POR}}
  .frmMenu.mnuAnalyse.mnuEnabling.por add command -label "CBC Graph" \
    		-command {procLaunchCmd {execInBZMode [list procEnablingAnalysis_GraphForPOR 0 false]}}
  .frmMenu.mnuAnalyse.mnuEnabling.por add command -label "CBC Graph (+Inv)" \
        -command {procLaunchCmd {execInBZMode [list procDisablingAnalysis_GraphForPOR 1]}}
  .frmMenu.mnuAnalyse.mnuEnabling.por add command -label "CBC Disable Graph" \
        -command {procLaunchCmd {execInBZMode [list procDisablingAnalysis_GraphForPOR 0]}}
  .frmMenu.mnuAnalyse.mnuEnabling.por add command -label "CBC Disable Graph (+Inv)" \
        -command {procLaunchCmd {execInBZMode [list procEnablingAnalysis_GraphForPOR 1 false]}}
  .frmMenu.mnuAnalyse.mnuEnabling.por add command -label "Enable Graph" \
        -command {procLaunchCmd {execInBZMode [list procEnablingAnalysis_GraphForPOR 0 true]}}
  .frmMenu.mnuAnalyse.mnuEnabling.por add command -label "Enable Graph (+Inv)" \
        -command {procLaunchCmd {execInBZMode [list procEnablingAnalysis_GraphForPOR 1 true]}}
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Disabling Analysis (Table)" \
		-command {procLaunchCmd {execInBZMode procDisablingAnalysis}}
    .frmMenu.mnuAnalyse.mnuEnabling add sep
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Read/Write Matrix (Table)" \
		-command {procLaunchCmd {execInBZMode procReadWriteMatrix}}
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Variable Read/Write Matrix (Table)" \
		-command {procLaunchCmd {execInBZMode procVarReadWriteMatrix}}
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Read/Write Matrix (Graph)" \
		-command {procLaunchCmd {execInBZMode procReadWriteVarAnalysis_Graph}}
    .frmMenu.mnuAnalyse.mnuEnabling add sep
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Dependence Analysis (Table)" \
		-command {procLaunchCmd {execInBZMode procDependenceAnalysis}}
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Dependence Analysis (Graph)" \
		-command {procLaunchCmd {execInBZMode procDependenceAnalysis_Graph}}
    .frmMenu.mnuAnalyse.mnuEnabling add sep
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "CFG Analysis (Table)" \
		-command {procLaunchCmd {execInBZMode procCFGAnalysis}}
	}
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "CFG Analysis (Graph)" \
		-command {procLaunchCmd {execInBZMode procCFGAnalysis_Graph}}
    .frmMenu.mnuAnalyse.mnuEnabling add sep
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Simultaneous Enabling Analysis (Table)" \
		-command {procLaunchCmd {execInBZMode procSimultEnablingAnalysis}}
	if {$expert_user} {
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Check Feasibility of Operation..." \
		-command {procLaunchCmd {execInBZMode procCheckIfOperationFeasible}}
    .frmMenu.mnuAnalyse.mnuEnabling add sep
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Show Guards of Operations" \
				-command {procLaunchCmd {execInBZMode procShowAllOpGuards}}
	.frmMenu.mnuAnalyse.mnuEnabling add command -label "Show Parameter-Free Guards and Actions of Operations" \
				-command {procLaunchCmd {execInBZMode procShowAllOpParamFreeGuards}}
   }


         #Refinement Check menu
    menu .frmMenu.mnuVerify.mnuRef -tearoff 0
	 .frmMenu.mnuVerify.mnuRef add command -label "Typecheck" \
		-command {procLaunchCmd procTypeCheckFile}  -accelerator $AccKey+B
		if {$normal_user} {
		 .frmMenu.mnuVerify.mnuRef add command -label "Typecheck Internal Representation" \
			-command {procLaunchCmd procTypeCheck_PPF_BFile}
		}
		.frmMenu.mnuVerify.mnuRef add sep
	 .frmMenu.mnuVerify.mnuRef add command -label "Model Check with TLC..." -command {procLaunchCmd procModelCheckWithTLC} -accelerator $AccKey+Shift+M
	 .frmMenu.mnuVerify.mnuRef add command -label "Model Check Invariants with LTSmin" -command {procLaunchCmd {procModelCheckWithLTSmin sequential true false false}}
	 .frmMenu.mnuVerify.mnuRef add command -label "Model Check Invariants with LTSmin (POR)" -command {procLaunchCmd {procModelCheckWithLTSmin sequential true false true}}
	 .frmMenu.mnuVerify.mnuRef add command -label "Model Check Deadlocks with LTSmin (POR)" -command {procLaunchCmd {procModelCheckWithLTSmin sequential false true true}}
	 .frmMenu.mnuVerify.mnuRef add command -label "Symbolic Deadlock Check with LTSmin" -command {procLaunchCmd {procModelCheckWithLTSmin symbolic false true false}}
	 .frmMenu.mnuVerify.mnuRef add command -label "Symbolic Invariant Check with LTSmin" -command {procLaunchCmd {procModelCheckWithLTSmin symbolic true false false}}
   .frmMenu.mnuVerify.mnuRef add sep
	 .frmMenu.mnuVerify.mnuRef add command -label "Save State to FDR/CSP File" \
		-command {procLaunchCmd procSaveStateToFDR}
	 .frmMenu.mnuVerify.mnuRef add command -label "Check Statespace with FDR" \
		-underline 22 -command {procLaunchCmd procCheckStatespaceWithFDR}
	 .frmMenu.mnuVerify.mnuRef add command -label "Open Specification with FDR" -command {procLaunchCmd procOpenFDROnSpec}
     .frmMenu.mnuVerify.mnuRef add sep
     .frmMenu.mnuVerify.mnuRef add command -label "Evaluate with the CSPM-Interpreter" \
               -command {if [prolog animation_mode(cspm)] {
                              set eval_window_arr(proCSPInterpreter) false
			      procEvalConsole
                        } else {
                              tkErrorBoxNoParent "Can only be applied to CSP specifications."
                        } }
     .frmMenu.mnuVerify.mnuRef add sep
	 .frmMenu.mnuVerify.mnuRef add command -label "Save State to Spin/Promela File" \
		-command {procLaunchCmd procSaveStateToSPIN}
	 .frmMenu.mnuVerify.mnuRef add command -label "Save State to Never Claim" \
		-command {procLaunchCmd procSaveStateToNeverClaim}
	 .frmMenu.mnuVerify.mnuRef add command -label "Print Event-B model as classical B" \
		-command {procLaunchCmd procPrintAsEventB} -state disabled
     global only_eventb_menues
    lappend only_eventb_menues [list .frmMenu.mnuVerify.mnuRef [.frmMenu.mnuVerify.mnuRef index end]]

         #Trace Check menu
    menu .frmMenu.mnuVerify.mnuTrace -tearoff 0
     .frmMenu.mnuVerify.mnuTrace add command -label "Save History to Default JSON File" \
        -command {procLaunchCmd {procSaveTraceFile "json" 0}}
     .frmMenu.mnuVerify.mnuTrace add command -label "Replay Default Trace File" \
        -underline 6 -command {procLaunchCmd procCheckTraceFile} -accelerator $AccKey+T
     .frmMenu.mnuVerify.mnuTrace add sep
     .frmMenu.mnuVerify.mnuTrace add command -label "Save History to JSON File..." \
        -command {procLaunchCmd {procSaveTraceFile "json" 1}}
     .frmMenu.mnuVerify.mnuTrace add command -label "Replay Trace from JSON File..." \
			-command {procLaunchCmd {procSelectAndCheckJSONTraceFile}}
     .frmMenu.mnuVerify.mnuTrace add sep
     .frmMenu.mnuVerify.mnuTrace add command -label "Save History to B File..." \
        -command {procLaunchCmd procSaveBTraceFile}
     .frmMenu.mnuVerify.mnuTrace add command -label "Save History to HTML File..." \
        -command {procLaunchCmd {procSaveHTMLTraceFile 0}}
     .frmMenu.mnuVerify.mnuTrace add command -label "Save detailed History to HTML File..." \
        -command {procLaunchCmd {procSaveHTMLTraceFile 3}}
    if {$expert_user} {
     .frmMenu.mnuVerify.mnuTrace add sep
     .frmMenu.mnuVerify.mnuTrace add command -label "Save History to other Prolog Trace File..." \
        -command {procLaunchCmd {procSaveTraceFile "prolog" 1}}
     .frmMenu.mnuVerify.mnuTrace add command -label "Replay Other Prolog Trace File..." \
			-command {procLaunchCmd {procCheckOtherTraceFile "prolog" "default_trace_replay"}}
     .frmMenu.mnuVerify.mnuTrace add command -label "Replay Other Prolog Trace File (deterministically)..." \
			-command {procLaunchCmd {procCheckOtherTraceFile "prolog" "deterministic_trace_replay"}}
		 }
    if {$expert_user} {
     .frmMenu.mnuVerify.mnuTrace add sep
     .frmMenu.mnuVerify.mnuTrace add command -label "Check Trace from B Predicate File..." \
			-command {procLaunchCmd {procCheckOtherTraceFile "state_sequence" "default_trace_replay"}}
		}
    if {$expert_user} {
     .frmMenu.mnuVerify.mnuTrace add command -label "Print FDR Check for History" \
			-command {procLaunchCmd {prologmnf b_trace_checking:print_trace_as_fdr_check}}
     }


    # -------------------- Visualize menu
    .frmMenu.mnuVisualize add command -label "Statespace" \
       -underline 11 -command {procLaunchCmd procDisplayVisitedStates} -accelerator $AccKey+D
     .frmMenu.mnuVisualize add cascade -label "Statespace Projections" \
            -menu .frmMenu.mnuVisualize.mnuDispProj
     .frmMenu.mnuVisualize add cascade -label "Statespace Fast Rendering" \
            -menu .frmMenu.mnuVisualize.mnuDispFast
     .frmMenu.mnuVisualize add sep
     .frmMenu.mnuVisualize add cascade -label "States" \
            -menu .frmMenu.mnuVisualize.mnuDispStates
     .frmMenu.mnuVisualize add cascade -label "Traces" \
            -menu .frmMenu.mnuVisualize.mnuDispTraces
     .frmMenu.mnuVisualize add sep
     .frmMenu.mnuVisualize add cascade -label "Formulas" \
            -menu .frmMenu.mnuVisualize.mnuVisualiseFormulas
     .frmMenu.mnuVisualize add sep
     .frmMenu.mnuVisualize add command -label "Machine Hierarchy" \
        -underline 13 -command {procLaunchCmd {procViewModuleHierarchyType "machines"}}
    if {$normal_user} {
      .frmMenu.mnuVisualize add command -label "Event Refinement Hierarchy..." \
        -underline 13 -command {procLaunchCmd {procViewModuleHierarchyType "events"}}
      .frmMenu.mnuVisualize add command -label "B Definition Hierarchy" \
        -underline 13 -command {procLaunchCmd {procViewModuleHierarchyType "definitions"}}
     .frmMenu.mnuVisualize add command -label "B Operation Call Hierarchy" \
        -underline 13 -command {procLaunchCmd {procViewModuleHierarchyType "operations"}}
      .frmMenu.mnuVisualize add sep
      .frmMenu.mnuVisualize add command -label "CSP-M Process" \
              -command {procLaunchCmd procVisualizeCSPProcessMenu}
      .frmMenu.mnuVisualize add command -label "Statespace as Tree" \
              -command {procLaunchCmd {openTreeInspector state_space}}
    }


         #Display sub menu
    menu .frmMenu.mnuVisualize.mnuDispTraces -tearoff 0
    .frmMenu.mnuVisualize.mnuDispTraces add command -label "Shortest Trace to Current State" \
       -command {procLaunchCmd {procDisplayTraceToCurrentState "yes"}} -state normal
    .frmMenu.mnuVisualize.mnuDispTraces add command -label "History to Current State" \
       -command {procLaunchCmd {procDisplayTraceToCurrentState "no"}} -state normal
    .frmMenu.mnuVisualize.mnuDispTraces add sep
    .frmMenu.mnuVisualize.mnuDispTraces add command -label "Evaluate Expression over History..." -command {procLaunchCmd procEvaluateExpressionOverHistory}
    .frmMenu.mnuVisualize.mnuDispTraces add command -label "History as UML Sequence Chart" -command {procLaunchCmd procHistoryAsUMLSequenceChart}
		.frmMenu.mnuVisualize.mnuDispTraces add sep
		.frmMenu.mnuVisualize.mnuDispTraces add command -label "History using VisB..." \
		   -command {procLaunchCmd {procVisualiseVisB "history_and_vars"}} -state normal
		.frmMenu.mnuVisualize.mnuDispTraces add command -label "History using Previous VisB File" \
		   -command {procLaunchCmd {procReVisualiseLastVisB "history_and_vars"}} -state normal
		.frmMenu.mnuVisualize.mnuDispTraces add command -label "History (wo Vars) using Previous VisB File" \
		   -command {procLaunchCmd {procReVisualiseLastVisB "history"}} -state normal

    menu .frmMenu.mnuVisualize.mnuDispStates -tearoff 0
    .frmMenu.mnuVisualize.mnuDispStates add command -label "Current State" \
       -command {procLaunchCmd {procDisplayCurrentState "no"}} -state normal
    .frmMenu.mnuVisualize.mnuDispStates add command -label "Current State as Graph" \
       -command {procLaunchCmd {procDisplayCurrentState "yes"}} -state normal
    if {$normal_user} {
		.frmMenu.mnuVisualize.mnuDispStates add sep
		.frmMenu.mnuVisualize.mnuDispStates add command -label "Current State as Custom Graph" \
		   -command {procLaunchCmd {procDisplayCurrentState "custom"}} -state normal
		.frmMenu.mnuVisualize.mnuDispStates add sep
		.frmMenu.mnuVisualize.mnuDispStates add command -label "Current State using Latex Template..." \
		   -command {procLaunchCmd procVisualiseLatex} -state normal
		.frmMenu.mnuVisualize.mnuDispStates add command -label "Current State using Previous Latex Template" \
		   -command {procLaunchCmd procReVisualiseLastLatex} -state normal
		.frmMenu.mnuVisualize.mnuDispStates add sep
		.frmMenu.mnuVisualize.mnuDispStates add command -label "Current State using VisB..." \
		   -command {procLaunchCmd {procVisualiseVisB "current_state"}} -state normal
		.frmMenu.mnuVisualize.mnuDispStates add command -label "Current State using Previous VisB File" \
		   -command {procLaunchCmd {procReVisualiseLastVisB "current_state"}} -state normal
		.frmMenu.mnuVisualize.mnuDispStates add command -label "Debug VisB Items" \
		   -command {procLaunchCmd procShowVisBItems} -state normal
		.frmMenu.mnuVisualize.mnuDispStates add command -label "Debug VisB Events" \
		   -command {procLaunchCmd procShowVisBEvents} -state normal
    }


    menu .frmMenu.mnuVisualize.mnuDispFast -tearoff 0
    .frmMenu.mnuVisualize.mnuDispFast add command -label "Force Directed (scale, fast)" \
       -underline 11 -command {procLaunchCmd {procDisplayVisitedStates_sfdp scale fast colour_transitions}}
    .frmMenu.mnuVisualize.mnuDispFast add command -label "Force Directed (prism, fast)" \
       -underline 11 -command {procLaunchCmd {procDisplayVisitedStates_sfdp prism fast colour_transitions}}
    if {$normal_user} {
      .frmMenu.mnuVisualize.mnuDispFast add sep
      .frmMenu.mnuVisualize.mnuDispFast add command -label "Force Directed (scale, fast, no_trans_color)" \
         -underline 11 -command {procLaunchCmd {procDisplayVisitedStates_sfdp scale fast no_colour_for_transitions}}
      .frmMenu.mnuVisualize.mnuDispFast add command -label "Force Directed (prism, fast, no_trans_color)" \
         -underline 11 -command {procLaunchCmd {procDisplayVisitedStates_sfdp prism fast no_colour_for_transitions}}
      .frmMenu.mnuVisualize.mnuDispFast add sep
      .frmMenu.mnuVisualize.mnuDispFast add command -label "Force Directed (scale)" \
         -underline 11 -command {procLaunchCmd {procDisplayVisitedStates_sfdp scale normal colour_transitions}}
      .frmMenu.mnuVisualize.mnuDispFast add command -label "Force Directed (prism)" \
         -underline 11 -command {procLaunchCmd {procDisplayVisitedStates_sfdp prism normal colour_transitions}}
      .frmMenu.mnuVisualize.mnuDispFast add sep
      .frmMenu.mnuVisualize.mnuDispFast add command -label "Force Directed (simple, fast)" \
         -underline 11 -command {procLaunchCmd {procDisplayVisitedStates_sfdp true fast colour_transitions}}
    }
   # Projections:
    menu .frmMenu.mnuVisualize.mnuDispProj -tearoff 0
    .frmMenu.mnuVisualize.mnuDispProj add command -label "Transition Diagram for Custom Expression..." \
       -command {procLaunchCmd procDisplayTransitionDiagram} -state normal
   if {$normal_user} {
    .frmMenu.mnuVisualize.mnuDispProj add sep
    # .frmMenu.mnuVisualize.mnuDispProj add command -label "Signature-Merge Reduced Statespace" \
       -command {procDisplayReducedSigMerge} -state normal -accelerator $AccKey+K
    .frmMenu.mnuVisualize.mnuDispProj add command -label "Signature-Merge Reduced Statespace..." \
       -command {procLaunchCmd procDisplayReducedSigMergeWithOptions} -state normal -accelerator $AccKey+K
    .frmMenu.mnuVisualize.mnuDispProj add command -label "DFA Reduced Statespace" \
       -command {procLaunchCmd procDisplayReducedDFA} -state normal
    .frmMenu.mnuVisualize.mnuDispProj add command -label "Select Operations & Arguments for Reduction" \
        -command {procLaunchCmd procShowArgChooser}; ## Argument chooser for DFA
     if {$expert_user} {
        .frmMenu.mnuVisualize.mnuDispProj add sep
        .frmMenu.mnuVisualize.mnuDispProj add command -label "Subgraph leading to Invariant Violation" \
           -command {procLaunchCmd {procSubgraph "invariant"}} -state normal
        .frmMenu.mnuVisualize.mnuDispProj add command -label "Subgraph for GOAL" \
           -command {procLaunchCmd {procSubgraph "goal"}} -state normal
        .frmMenu.mnuVisualize.mnuDispProj add sep
        .frmMenu.mnuVisualize.mnuDispProj add command -label "Neigbourhood (1)" \
           -command {procLaunchCmd {procDisplayVisitedStatesUpTo 1}} -state normal
        .frmMenu.mnuVisualize.mnuDispProj add command -label "Neigbourhood (2)" \
           -command {procLaunchCmd {procDisplayVisitedStatesUpTo 2}} -state normal
        .frmMenu.mnuVisualize.mnuDispProj add command -label "Neigbourhood (3)" \
           -command {procLaunchCmd {procDisplayVisitedStatesUpTo 3}} -state normal
       }
   }

    # -------------------- Visualise Formulas menu
    menu .frmMenu.mnuVisualize.mnuVisualiseFormulas -tearoff 0
	 .frmMenu.mnuVisualize.mnuVisualiseFormulas add command -label "Invariant" -command {procLaunchCmd procVisualiseInvariant} -accelerator $AccKey+Shift+I
	 .frmMenu.mnuVisualize.mnuVisualiseFormulas add command -label "Properties" -command {procLaunchCmd procVisualiseProperties}
	 .frmMenu.mnuVisualize.mnuVisualiseFormulas add command -label "Assertions" -command {procLaunchCmd procVisualiseAssertions}
	 .frmMenu.mnuVisualize.mnuVisualiseFormulas add command -label "Operation PRE..." -command {procLaunchCmd procVisualiseOperationPre}
	 .frmMenu.mnuVisualize.mnuVisualiseFormulas add command -label "Custom Predicate..." -command {procLaunchCmd procVisualiseCustomPredicate}
	  if {$normal_user} {
	     .frmMenu.mnuVisualize.mnuVisualiseFormulas add command -label "GOAL" -command {provVisualizeDEFGOAL}
	     .frmMenu.mnuVisualize.mnuVisualiseFormulas add command -label "Deadlock PO" -command {procLaunchCmd {procVisualiseInvariantOrOpPRE "_deadlockpo_"}}
	     .frmMenu.mnuVisualize.mnuVisualiseFormulas add sep
	     .frmMenu.mnuVisualize.mnuVisualiseFormulas add command -label "Expression as Table..." -command {procLaunchCmd procShowAnExpressionAsTable}
		.frmMenu.mnuVisualize.mnuVisualiseFormulas add command -label "Expression as Dot Graph..." -command {procLaunchCmd procShowAnExpressionAsDot}
		  if {$expert_user} {
			.frmMenu.mnuVisualize.mnuVisualiseFormulas add command -label "Sets as Venn Diagram..." -command {procLaunchCmd procQuadVennDiagram}
		  }
	  }


    # -------------------- Analyse menu
#    menu .frmMenu.mnuAnalyse -tearoff 0
    .frmMenu.mnuAnalyse add cascade -label "Coverage" \
            -menu .frmMenu.mnuAnalyse.mnuCoverage
    .frmMenu.mnuAnalyse add command -label "Show Typing" -command {procLaunchCmd procShowTyping}
     .frmMenu.mnuAnalyse add sep
     .frmMenu.mnuAnalyse add cascade -label "Analyse Predicate" \
            -menu .frmMenu.mnuAnalyse.mnuDebugPred
     .frmMenu.mnuAnalyse add command -label "Open Evaluation View" \
        -command {procLaunchCmd openEvaluationView} -accelerator $AccKey++
    if {$normal_user} {
    .frmMenu.mnuAnalyse add command -label "Save Values of Current State..." -command {procLaunchCmd procSaveValuesToFile}
    .frmMenu.mnuAnalyse add command -label "Eval Console..." -command {procLaunchCmd procEvalConsole}
    }
    if {$normal_user} {
        .frmMenu.mnuAnalyse add sep
        .frmMenu.mnuAnalyse add cascade -label "Enabling Analysis" \
            -menu .frmMenu.mnuAnalyse.mnuEnabling
        .frmMenu.mnuAnalyse add cascade -label "Testing" \
            -menu .frmMenu.mnuAnalyse.mnuTest
		  if {$expert_user} {
        .frmMenu.mnuAnalyse add sep
        .frmMenu.mnuAnalyse add cascade -label "Experimental" \
            -menu .frmMenu.mnuExperimental
		   }
    }

    # -------------------- Find menu
    menu .frmMenu.mnuAnimate.mnuFind -tearoff 0
    # get_specification_description(operations_lc,R)
    .frmMenu.mnuAnimate.mnuFind add command -label "State Enabling an Operation..." \
        -command {procLaunchCmd procFindEnabledOp}
    .frmMenu.mnuAnimate.mnuFind add sep
    .frmMenu.mnuAnimate.mnuFind add command -label "Non-Deterministic Operation" \
        -command {procLaunchCmd procJumpToNonDetNode}
    .frmMenu.mnuAnimate.mnuFind add command -label "Non-Deterministic Operation Outputs" \
        -command {procLaunchCmd procJumpToNonDetOutputNode}
    .frmMenu.mnuAnimate.mnuFind add command -label "State with Multiple Outgoing Transitions" \
        -command {procLaunchCmd procJumpToBranchingNode}
    .frmMenu.mnuAnimate.mnuFind add command -label "State which Cannot Reach an Initial State" \
        -command {procLaunchCmd procJumpToNonResetableNode}
    .frmMenu.mnuAnimate.mnuFind add sep
    .frmMenu.mnuAnimate.mnuFind add command -label "Relative Deadlock..." \
        -command {procLaunchCmd procFindRelativeDeadlock}
    .frmMenu.mnuAnimate.mnuFind add command -label "Controller State Violation..." \
        -command {procLaunchCmd procFindControllerViolation}
    .frmMenu.mnuAnimate.mnuFind add command -label "Determinism Controller State Violation..." \
        -command {procLaunchCmd procFindDetControllerViolation}
    .frmMenu.mnuAnimate.mnuFind add sep
    .frmMenu.mnuAnimate.mnuFind add command -label "Unprocessed State" -underline 0 \
              -command {procLaunchCmd procJumpToAnOpenNode}
    .frmMenu.mnuAnimate.mnuFind add sep
    .frmMenu.mnuAnimate.mnuFind add command -label "State with ID..." -underline 0 \
              -command {procLaunchCmd procJumpToStateWithID}
     if {$expert_user} {
    .frmMenu.mnuAnimate.mnuFind add command -label "Not fully explored State" \
              -command {procLaunchCmd procJumpToAMaxReachedNode}
		.frmMenu.mnuAnimate.mnuFind add command -label "State with Longest Distance from Root" -command procExecLongestTrace
     }
    # -------------------- Coverage menu
    menu .frmMenu.mnuAnalyse.mnuCoverage -tearoff 0
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "Operation Coverage" -underline 14 -command {procLaunchCmd {procComputeCoverage 0}} -accelerator $AccKey+G
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "Operation Coverage and Degree" -command {procLaunchCmd {procComputeCoverage degree}}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "Operation Coverage and Enabling" -command {procLaunchCmd {procComputeCoverage enabling}}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "Minimum and Maximum Values" -command {procLaunchCmd {procMinMaxCoverageTable}}
    .frmMenu.mnuAnalyse.mnuCoverage add sep
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "Value Coverage for Expression..." -command {procLaunchCmd procEvaluateAnExpressionOverStatespace}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "Number of Values for all Variables" -command {procLaunchCmd procNrVariableValuesOverStatespace}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "Number of Values for all Constants" -command {procLaunchCmd procNrConstantsValuesOverStatespace}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "Analyze Constants" -command {procLaunchCmd procAnalyseConstants}
    .frmMenu.mnuAnalyse.mnuCoverage add sep
	.frmMenu.mnuAnalyse.mnuCoverage add command -label "Determine Vacuous Guards" -command {procLaunchCmd {procComputeCoverage vacuous_guards}}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "MC/DC Coverage Level 0" -command {procLaunchCmd {procMCDC_Op_Coverage 0}}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "MC/DC Coverage Level 1" -command {procLaunchCmd {procMCDC_Op_Coverage 1}}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "MC/DC Coverage Level 2" -command {procLaunchCmd {procMCDC_Op_Coverage 2}}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "MC/DC Coverage Level 10" -command {procLaunchCmd {procMCDC_Op_Coverage 10}}
    .frmMenu.mnuAnalyse.mnuCoverage add sep
	.frmMenu.mnuAnalyse.mnuCoverage add command -label "Determine Vacuous Invariants" -command {procLaunchCmd {procComputeCoverage vacuous_invariants}}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "Invariant Coverage" -command {procLaunchCmd {procInv_Coverage}}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "MC/DC Invariant Coverage Level 2" -command {procLaunchCmd {procMCDC_Inv_Coverage 2}}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "MC/DC Invariant Coverage Level 10" -command {procLaunchCmd {procMCDC_Inv_Coverage 10}}
    .frmMenu.mnuAnalyse.mnuCoverage add sep
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "Quick Operation Coverage" -command {procLaunchCmd {procQuickOrCBCOpCoverage quick}}
    .frmMenu.mnuAnalyse.mnuCoverage add command -label "Precise Operation Coverage" -command {procLaunchCmd {procQuickOrCBCOpCoverage precise}}



    # -------------------- Analyse menu
    menu .frmMenu.mnuAnalyse.mnuDebugPred -tearoff 0
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Invariant" -underline 0 -command {procLaunchCmd {procAnalyseInvariant}} -accelerator $AccKey+I
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Properties" -command {procLaunchCmd {procAnalysePred properties}}
    if {$expert_user} {
        .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Debug Properties" -command {procLaunchCmd {execInBZMode procDebugProperties}}
    }
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Assertions" -command {procLaunchCmd procAnalyseAssertions} -accelerator $AccKey+\]
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Debug Operation PRE..." -command {procLaunchCmd procDebugOperation}
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Custom Predicate..." -command {procLaunchCmd procAnalyseCustomPredicate}
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "GOAL" -command {procLaunchCmd procAnalyseDEFGOAL}  -accelerator $AccKey+\[
    if {$expert_user} {
        .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Debug GOAL" -command {procLaunchCmd {execInBZMode procDebugGOAL}}
    }
    .frmMenu.mnuAnalyse.mnuDebugPred add command -label "Deadlock PO" -command {procLaunchCmd {procAnalysePredCheck2 deadlock -1 -1 -1 0}}





    # -------------------- ProTest menu
    menu .frmMenu.mnuAnalyse.mnuTest -tearoff 0
	   if {$normal_user} {
           .frmMenu.mnuAnalyse.mnuTest add command -label "Constraint-based test case generation..." \
               -command {procLaunchCmd procCBCTestcases}
            .frmMenu.mnuAnalyse.mnuTest add command -label "View CBC Test Cases" -command {openTreeInspector cbc_tests}
	        .frmMenu.mnuAnalyse.mnuTest add sep
	        .frmMenu.mnuAnalyse.mnuTest add command -label "Model-Checking-based test case generation..." \
		         -command {procLaunchCmd procSAP_GlobalMC_Testcases}
	        .frmMenu.mnuAnalyse.mnuTest add command -label "MC local test case generation..." \
		         -command {procLaunchCmd procSAPLocal}
	   }


    # -------------------- Debug menu
#    menu .frmMenu.mnuDebug -tearoff 0
    global debugMode
    .frmMenu.mnuDebug add command -label "Report a Bug..." -command {procLaunchCmd procReportBug}
    .frmMenu.mnuDebug add command -label "Check Java and Parser Version" \
            -command {procLaunchCmd procPerformJavaParserCheck}
    if {$normal_user} {
        .frmMenu.mnuDebug add sep
        if [prolog self_checks_exist] {
            .frmMenu.mnuDebug add command -label "Perform Self Check" \
                -command {procLaunchCmd procPerformSelfCheck}
        }
    }
    if {$expert_user} {
        if [prolog self_checks_exist] {
		.frmMenu.mnuDebug add command -label "Perform Verbose Self Check" \
			-command {procLaunchCmd procPerformVerboseSelfCheck}
		.frmMenu.mnuDebug add command -label "Perform Self Check for Module..." \
			-command {procLaunchCmd {procPerformSelfCheckForModule}}
        }
        .frmMenu.mnuDebug add command -label "Open AST Inspector" -command {procLaunchCmd {openTreeInspector ast}}
    }
	if {$normal_user} {
        .frmMenu.mnuDebug add command -label "Show Internal Representation" -command {procLaunchCmd procShowInternalRep}
        .frmMenu.mnuDebug add command -label "Show Internal Unicode Representation" -command {procLaunchCmd {procShowInternalRep1 1 0}}
		if {$expert_user} {
        .frmMenu.mnuDebug add command -label "Show Internal as EventB" -command {procLaunchCmd {procShowInternalRep1 1 1}}
        .frmMenu.mnuDebug add command -label "Show Internal Prolog Representation" -command {procLaunchCmd procShowInternalPrologRep}
     }
		.frmMenu.mnuDebug add sep
		.frmMenu.mnuDebug add checkbutton -label "Debugging Info" \
			 -variable debugMode -offvalue 0 -onvalue 1 -command procUpdateDebuggingMode
		global typeCheckMode profilingMode
		global traceUponError
		if {$expert_user} {
            .frmMenu.mnuDebug add checkbutton -label "Runtime Type Checking" \
                 -variable typeCheckMode -offvalue 0 -onvalue 1 -command procUpdateDebuggingMode
		     .frmMenu.mnuDebug add checkbutton -label "Trace Upon Error" \
			   -variable traceUponError -offvalue 0 -onvalue 1 -command procUpdateDebuggingMode
			 .frmMenu.mnuDebug add checkbutton -label "Profile (Model Checker)" \
			   -variable profilingMode -offvalue 0 -onvalue 1
			 .frmMenu.mnuDebug add command -command {prolog debug} -label "Turn Prolog Debug Mode on"
			 .frmMenu.mnuDebug add command -command {prolog set_preference(performance_monitoring_on,true)} -label "Show Prolog Performance Messages"
			 .frmMenu.mnuDebug add sep
			 if ![prolog compile_time_flags:compile_time_flag(prob_release)] {
			     # code is in benchmarks.tcl; not loaded in prob_release mode
                   .frmMenu.mnuDebug add command -label "Perform Benchmark Test" \
                    -command {procLaunchCmd ::bench::procBenchmark}
                   .frmMenu.mnuDebug add command -label "Perform Regression Test" \
                    -command {procLaunchCmd ::bench::procRegressionTest}
                   .frmMenu.mnuDebug add command -label "Perform Quick Mathematical Laws Check" \
                    -command {procLaunchCmd {::bench::procLawCheckTest "false"}}
                   .frmMenu.mnuDebug add command -label "Perform Full Mathematical Laws Check" \
                    -command {procLaunchCmd {::bench::procLawCheckTest "true"}}
                   .frmMenu.mnuDebug add command -label "Perform CSP-M Benchmark Test" \
                    -command {procLaunchCmd ::bench::procCSPMBenchmark}
                   .frmMenu.mnuDebug add command -label "Perform Symmetry Benchmark" \
                    -command {procLaunchCmd ::bench::procSymBenchmark}
                   .frmMenu.mnuDebug add sep
			 }
			 .frmMenu.mnuDebug add command -label "Show all specialized invariants" \
				-command {procLaunchCmd {execInBZMode procShowAllSpecializedInvariants}}
			 .frmMenu.mnuDebug add command -label "Show specialized invariants..." \
				-command {procLaunchCmd {execInBZMode procShowSpecializedInvariants}}
			 .frmMenu.mnuDebug add command -label "Show Before-After Predicate..." \
				-command {procLaunchCmd {execInBZMode procShowBAPredicate}}
#			  .frmMenu.mnuDebug add command -label "Print Normal Form" \
				-command {prolog "use_module(probsrc(b_normal_form)),b_normal_form:print_normal_forms"}
		 }
		 .frmMenu.mnuDebug add sep
		 .frmMenu.mnuDebug add command -label "Show ProB Profile Info" \
			-command {procLaunchCmd procShowProBProfileInfo}
		 .frmMenu.mnuDebug add command -label "Print Prolog statistics" \
			-command {procLaunchCmd {prolog (nl,debug:timer_statistics,nl,statistics,nl)}}
		 if {$expert_user} {
             .frmMenu.mnuDebug add command -label "Show ProB Source Code Coverage" \
                -command {procLaunchCmd procShowSourceCodeCoverge}
             .frmMenu.mnuDebug add command -label "Reset Statespace with statistics" \
                -command {procLaunchCmd {prolog state_space_initialise_with_stats; procInsertHistoryOptionsState}}
             .frmMenu.mnuDebug add command -label "Show ProB Operation Reuse Cache Info" \
                -command {procShowOpCacheInfo}
             .frmMenu.mnuDebug add command -label "Show ProB Constants Cache Info" \
                -command {procShowCacheInfo}
             .frmMenu.mnuDebug add command -label "Show Unsat Core" -command {procLaunchCmd procShowUnsatCore}
		 }
     }
    # -------------------- Experimental menu
     menu .frmMenu.mnuExperimental -tearoff 0
	 .frmMenu.mnuExperimental add command -label "Analyze Hash Collisions" -command {procLaunchCmd {prologmnf state_space_exploration_modes:analyze_hash_collisions}}
	 .frmMenu.mnuExperimental add sep
     .frmMenu.mnuExperimental add command -label "BA Symbolic Inductive Invariant Check" -command {procLaunchCmd {execInBZMode procCBCInvariantBACheck}}
     .frmMenu.mnuExperimental add sep
	 .frmMenu.mnuExperimental add command -label "Unsat Core of PROPERTIES" -command {procLaunchCmd procUnsatCore}

     # -------------------- symbolic model checking menu
     menu .frmMenu.mnuVerify.mnuSymbolicMC -tearoff 0
     # BMC*
	 .frmMenu.mnuVerify.mnuSymbolicMC add command -label "Bounded Model Checking (5)" -command {procLaunchCmd {procBoundedModelChecking 5}}
	 .frmMenu.mnuVerify.mnuSymbolicMC add command -label "Bounded Model Checking (50)" -command {procLaunchCmd {procBoundedModelChecking 50}}
	 .frmMenu.mnuVerify.mnuSymbolicMC add command -label "Bounded Deadlock Checking (5)" -command {procLaunchCmd {procBoundedDeadlockChecking 5}}
	 .frmMenu.mnuVerify.mnuSymbolicMC add sep
     .frmMenu.mnuVerify.mnuSymbolicMC add command -label "BMC (monolithic)" -command {procLaunchCmd procSymbolicBMCModelCheck}
     .frmMenu.mnuVerify.mnuSymbolicMC add command -label "k-induction" -command {procLaunchCmd procSymbolicKInductionModelCheck}
     .frmMenu.mnuVerify.mnuSymbolicMC add command -label "IC3" -command {procLaunchCmd procSymbolicIC3ModelCheck}
     .frmMenu.mnuVerify.mnuSymbolicMC add command -label "CTIGAR" -command {procLaunchCmd procSymbolicCTIGARModelCheck}
     .frmMenu.mnuVerify.mnuSymbolicMC add sep

     # -------------------- static checking menu
     menu .frmMenu.mnuVerify.mnuStaticChecks -tearoff 0
	 .frmMenu.mnuVerify.mnuStaticChecks add command -label "Check Well-Definedness POs" -command {procLaunchCmd procStaticWDCheck}
	 .frmMenu.mnuVerify.mnuStaticChecks add command -label "Show Well-Definedness POs" -command {procLaunchCmd {procShowWDPOs "only_goal"} }
	 .frmMenu.mnuVerify.mnuStaticChecks add command -label "Show WD POs with Hypotheses" -command {procLaunchCmd {procShowWDPOs "goal_and_hyps"} }
     .frmMenu.mnuVerify.mnuStaticChecks add sep
	 .frmMenu.mnuVerify.mnuStaticChecks add command -label "Extended Static Checks" -command {procLaunchCmd procExtendedStaticCheck}


    # -------------------- Preferences menu
#       menu .frmMenu.mnuPreferences -tearoff 0
    global tcl_version
    if {$tcl_version >= 8.5} {
       .frmMenu.mnuPreferences add command -label "Text Editor Preferences..." \
              -command {procSetTextEditorPreferences}
    } else {
       .frmMenu.mnuPreferences add cascade -label "Font" \
              -menu .frmMenu.mnuPreferences.mnuFont
       .frmMenu.mnuPreferences add cascade -label "Display Line Numbers" \
              -menu .frmMenu.mnuPreferences.mnuLinenumbers
       .frmMenu.mnuPreferences add cascade -label "Highlight Bracket" \
              -menu .frmMenu.mnuPreferences.mnuHighlightCurrentBracket
       .frmMenu.mnuPreferences add cascade -label "Highlight Current Line" \
              -menu .frmMenu.mnuPreferences.mnuHighlightCurrentLine
    }
    .frmMenu.mnuPreferences add cascade -label "Symmetry" \
            -menu .frmMenu.mnuPreferences.mnuSym
    .frmMenu.mnuPreferences add sep
    .frmMenu.mnuPreferences add command -label "Animation Preferences..." \
          -command {procLaunchCmd {procSetPreferences "animation"}}
    .frmMenu.mnuPreferences add cascade -label "Graphical Viewer" \
            -menu .frmMenu.mnuPreferences.mnuGraphViewer
    .frmMenu.mnuPreferences add command -label "Graphical Viewer Preferences..." \
          -command {procLaunchCmd {procSetPreferences "dot"}}
    .frmMenu.mnuPreferences add command -label "Syntax Highlighting Preferences..." \
          -command {procLaunchCmd {procSyntaxColouringPreferences}}
    .frmMenu.mnuPreferences add command -label "CSP Preferences..." \
          -command {procLaunchCmd {procSetPreferences "csp_prefs"}}
    if {$normal_user} {
    .frmMenu.mnuPreferences add command -label "Advanced Preferences..." \
          -command {procLaunchCmd {procSetPreferences "advanced"}}
    }
	global kodkod_enabled
	set kodkod_enabled [prolog preferences:get_preference(try_kodkod_on_load,true)]
    if {$normal_user || $kodkod_enabled} {
        .frmMenu.mnuPreferences add cascade -label "Enable Kodkod for Properties" \
            -menu .frmMenu.mnuPreferences.mnuKodkod
        menu .frmMenu.mnuPreferences.mnuKodkod -tearoff 0
        .frmMenu.mnuPreferences.mnuKodkod add radiobutton -label "Enabled"  -variable kodkod_enabled -value 1 \
            -command "prologmnf preferences:set_preference(try_kodkod_on_load,true)"
        .frmMenu.mnuPreferences.mnuKodkod add radiobutton -label "Disabled"  -variable kodkod_enabled -value 0 \
            -command "prologmnf preferences:set_preference(try_kodkod_on_load,false)"
    }
    if {$expert_user} {
        global model_checker_arr PORTechniqueVar
        prolog preferences:get_preference(pge,PGE)
        set model_checker_arr(pge) $prolog_variables(PGE)
        if {$model_checker_arr(pge) != off} {set model_checker_arr(use_pge) true} else {set model_checker_arr(use_pge) false}

        prolog preferences:get_preference(por,POR)
        set model_checker_arr(por) $prolog_variables(POR); set PORTechniqueVar $prolog_variables(POR)

	     if {$model_checker_arr(por) != off} {set model_checker_arr(use_por) true} else {set model_checker_arr(use_por) false}

        .frmMenu.mnuPreferences add sep
        .frmMenu.mnuPreferences add command -label "Model Checker Preferences..." -command {procLaunchCmd procModelCheckerPreferencesViewer}
    }
    .frmMenu.mnuPreferences add sep
    .frmMenu.mnuPreferences add cascade -label "User Mode" \
            -menu .frmMenu.mnuPreferences.mnuMode
    .frmMenu.mnuPreferences add cascade -label "Configurations" \
            -menu .frmMenu.mnuPreferences.mnuConfigs
    .frmMenu.mnuPreferences add command -label "All Preferences (alphabetical)..." \
          -command {procLaunchCmd {procSetPreferences "eclipse"}}
    .frmMenu.mnuPreferences add sep
    .frmMenu.mnuPreferences add command -label "Show Non-Default Preferences" \
          -command {procLaunchCmd procShowNonDefaultPreferences}
	if {$expert_user} {
    .frmMenu.mnuPreferences add command -label "Hidden Preferences..." \
          -command {procLaunchCmd {procSetPreferences "hidden"}}
    }

         #line numbers menu
    global text_editor_arr
    prolog preferences:get_preference(show_line_numbers,ShowLines)
    set text_editor_arr(linenumbers) $prolog_variables(ShowLines)

         #highlight current bracket
    prolog preferences:get_preference(highlight_brackets,HighlightBrackets)
    set text_editor_arr(highlight_brackets) $prolog_variables(HighlightBrackets)
         #highlight current line
    prolog preferences:get_preference(highlight_current_line,HighlightCurrentLine)
    set text_editor_arr(highlight_line) $prolog_variables(HighlightCurrentLine)
         #asking when file content changed from external program
    prolog preferences:get_preference(ask_when_content_changed,Ask)
    set text_editor_arr(ask_when_content_changed) $prolog_variables(Ask)
    if {$tcl_version < 8.5} {
         #font menu
        menu .frmMenu.mnuPreferences.mnuFont -tearoff 0
        global chosefontval
        foreach font {"TkFixedFont 9" "TkFixedFont 10" "TkFixedFont 12" "TkFixedFont 14" "TkFixedFont 16" "TkFixedFont 18" "TkFixedFont 20" "TkFixedFont 24"} {
        	.frmMenu.mnuPreferences.mnuFont add radiobutton -label "$font"  -variable chosefontval -value $font \
         	       -command {procLaunchCmd "changeFont {$font}"}
         }
        menu .frmMenu.mnuPreferences.mnuLinenumbers -tearoff 0
        .frmMenu.mnuPreferences.mnuLinenumbers add radiobutton -label "Enabled" -variable text_editor_arr(linenumbers) \
              -value true -command {procSetLinenumbersValueToTrue "true"}
        .frmMenu.mnuPreferences.mnuLinenumbers add radiobutton -label "Disabled" -variable text_editor_arr(linenumbers) \
              -value false -command {procSetLinenumbersValueToTrue "false"}

        menu .frmMenu.mnuPreferences.mnuHighlightCurrentBracket -tearoff 0
        .frmMenu.mnuPreferences.mnuHighlightCurrentBracket add radiobutton -label "Enabled" -variable text_editor_arr(highlight_brackets) \
              -value true -command {procSetPrefOption text_editor_arr highlight_brackets true}
        .frmMenu.mnuPreferences.mnuHighlightCurrentBracket add radiobutton -label "Disabled" -variable text_editor_arr(highlight_brackets) \
              -value false -command {procSetPrefOption text_editor_arr highlight_brackets false}

        menu .frmMenu.mnuPreferences.mnuHighlightCurrentLine -tearoff 0
        .frmMenu.mnuPreferences.mnuHighlightCurrentLine add radiobutton -label "Enabled" -variable text_editor_arr(highlight_line) \
              -value true -command {procSetHighlightCurrentLineValueToTrue "true"}
        .frmMenu.mnuPreferences.mnuHighlightCurrentLine add radiobutton -label "Disabled" -variable text_editor_arr(highlight_line) \
              -value false -command {procSetHighlightCurrentLineValueToTrue "false"}
    }
    menu .frmMenu.mnuPreferences.mnuSym -tearoff 0
    global chosensymmode
    prolog preferences:get_preference(symmetry_mode,SYMMODE)
    set chosensymmode $prolog_variables(SYMMODE)
    #puts "SymmMode $chosensymmode"
    foreach mode {"off" "hash" "flood" "nauty"} {
	.frmMenu.mnuPreferences.mnuSym add radiobutton -label "$mode"  -variable chosensymmode -value $mode \
	       -command "prolog preferences:set_preference(symmetry_mode,$mode); procReOpenFile"
    }
    .frmMenu.mnuPreferences.mnuSym add sep
    .frmMenu.mnuPreferences.mnuSym add command -label "(will reload machine)" -state disabled
     # User mode menu

    menu .frmMenu.mnuPreferences.mnuGraphViewer -tearoff 0
    global chosen_dot_mode
    prologmnf preferences:get_preference(dot_use_ps_viewer,PSV)
    if {$prolog_variables(PSV)==true} {
       set chosen_dot_mode 1
    } else {
       set chosen_dot_mode 2
    }
	.frmMenu.mnuPreferences.mnuGraphViewer add radiobutton -label "PDF"  -variable chosen_dot_mode -value 1 \
	       -command "prologmnf preferences:set_preference(dot_use_ps_viewer,true)"
	.frmMenu.mnuPreferences.mnuGraphViewer add radiobutton -label "Dot Viewer"  -variable chosen_dot_mode -value 2 \
	       -command "prologmnf preferences:set_preference(dot_use_ps_viewer,false)"


    menu .frmMenu.mnuPreferences.mnuConfigs -tearoff 0
	.frmMenu.mnuPreferences.mnuConfigs add command -label "MININT..MAXINT (32bit)" \
	       -command "prologmnf preferences:set_preference_group(integer,int32)"
	.frmMenu.mnuPreferences.mnuConfigs add command -label "MININT..MAXINT = -1..3" \
	       -command "prologmnf preferences:set_preference_group(integer,default)"
	.frmMenu.mnuPreferences.mnuConfigs add sep
	.frmMenu.mnuPreferences.mnuConfigs add command -label "Syntax Colour Scheme (Solarized)" \
	       -command "prologmnf preferences:set_preference_group(sh_colors,solarized)"
	.frmMenu.mnuPreferences.mnuConfigs add command -label "Syntax Colour Scheme (Default)" \
	       -command "prologmnf preferences:set_preference_group(sh_colors,default)"
	.frmMenu.mnuPreferences.mnuConfigs add sep
	.frmMenu.mnuPreferences.mnuConfigs add command -label "Dot Colour Scheme (Winter)" \
	       -command "prologmnf preferences:set_preference_group(dot_colors,winter)"
	.frmMenu.mnuPreferences.mnuConfigs add command -label "Dot Colour Scheme (Dreams)" \
	       -command "prologmnf preferences:set_preference_group(dot_colors,dreams)"
	.frmMenu.mnuPreferences.mnuConfigs add command -label "Dot Colour Scheme (Classic)" \
	       -command "prologmnf preferences:set_preference_group(dot_colors,classic)"
	.frmMenu.mnuPreferences.mnuConfigs add command -label "Dot Colour Scheme (Solarized)" \
	       -command "prologmnf preferences:set_preference_group(dot_colors,solarized)"
	.frmMenu.mnuPreferences.mnuConfigs add command -label "Dot Colour Scheme (Default)" \
	       -command "prologmnf preferences:set_preference_group(dot_colors,default)"

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
    .frmMenu.mnuHelp add command -label "Summary of B Syntax" -command "procLaunchSyntaxInfo B"
    .frmMenu.mnuHelp add command -label "Summary of CSP Syntax" -command "procLaunchSyntaxInfo CSP"
    .frmMenu.mnuHelp add command -label "Summary of TLA+ Syntax" -command "procLaunchSyntaxInfo TLA"
    .frmMenu.mnuHelp add command -label "Summary of Supported Z" -command "procLaunchSyntaxInfo Z"
    .frmMenu.mnuHelp add command -label "Summary of Alloy Syntax" -command "procLaunchSyntaxInfo Alloy"
    .frmMenu.mnuHelp add sep
    .frmMenu.mnuHelp add command -label "Verification Info" -command "procCheckingInfo"
    .frmMenu.mnuHelp add command -label "Summary of LTL Syntax" -command "procLaunchSyntaxInfo LTL"
    .frmMenu.mnuHelp add command -label "Summary of CTL Syntax" -command "procLaunchSyntaxInfo CTL"
    .frmMenu.mnuHelp add command -label "Symmetry Reduction Info" -command "procSymmInfo"
    .frmMenu.mnuHelp add sep
    .frmMenu.mnuHelp add command -label "Install AtelierB 4 Plugin..." -command "installAtelierBPlugin"
    .frmMenu.mnuHelp add command -label "Download and Install LTL2BA Tool" -command "installLTL2BA"


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

proc procErrShowList {Result WinTitle msg} {
   global batch_mode
   global testing_mode strFilename failed_tests detailed_errors
  if {$batch_mode} {
     puts "### An Error Occurred\n$msg"
     prolog assert_cli_error_occurred(procErrShowList)
  } elseif {$testing_mode==true} {
       append failed_tests $strFilename "; 1 Error Message ;;"
       append detailed_errors $strFilename ";  " $msg ";;"
  } else {
       procShowList $Result $WinTitle $msg
  }
}
proc tkErrorBox {msg} {
  global batch_mode
   global testing_mode strFilename failed_tests detailed_errors
  if {$batch_mode} {
     puts "### An Error Occurred\n$msg"
     prolog assert_cli_error_occurred(tkErrorBox)
  } elseif {$testing_mode==true} {
       append failed_tests $strFilename "; 1 Error Message ;;"
       append detailed_errors $strFilename ";  " $msg ";;"
  } elseif [prolog real_error_occurred] {
     procShowErrors2 "" .main.frmSource.text "Error Message" "$msg\n" "subsidiary errors" ErrorIcon
  } else {
     tk_messageBox -parent . -icon error -message $msg
  }
}
proc tkErrorBoxNoParent {msg} {
  global batch_mode
   global testing_mode strFilename failed_tests detailed_errors
  if {$batch_mode} {
     puts "### An Error Occurred\n$msg"
     prolog assert_cli_error_occurred(tkErrorBoxNoParent)
  } elseif {$testing_mode==true} {
       append failed_tests $strFilename "; 1 Error Message ;;"
       append detailed_errors $strFilename ";  " $msg ";;"
  } elseif [prolog real_error_occurred] {
     if [prolog test_error_occurred(_)] {
       procShowErrors2 "" .main.frmSource.text "Error Message" "$msg\n" "subsidiary errors" ErrorIcon
     } else {
        # errors were already removed and transformed into state errors
        tk_messageBox -icon error -message $msg
        prolog reset_errors
     }
  } else {
     tk_messageBox -icon error -message $msg
  }
}
proc tkOptWarningBoxNoParent {msg} {
  global batch_mode
   global testing_mode strFilename failed_tests detailed_errors
  if {$batch_mode} {
     puts "$msg"
  } elseif [prolog real_error_occurred] {
     procShowErrors2 "" .main.frmSource.text "Error Message" "$msg\n" "subsidiary errors" ErrorIcon
  } elseif [prolog error_or_warning_occured] {
     procShowErrors2 "" .main.frmSource.text "Message with Warnings" "$msg\n" "subsidiary warnings" WarningIcon
  } else {
     tk_messageBox -icon warning -message $msg
  }
}
proc tkMessageBox {msg} {
  global batch_mode
  if {$batch_mode} {
     puts "$msg"
  } else {
     tk_messageBox -parent .main -message $msg
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


proc EvaluateSelectedExpressionOverStateSpace {} {
    set sel [selection get]
    if {$sel != ""} {
       regsub -all -- \[\r\n\t] $sel " " sel
       procEvaluateExpressionOverStatespace $sel ""
    }
	selection clear
}

proc VisualizeSelectedExpressionAsDot {} {
    set sel [selection get]
    if {$sel != ""} {
       regsub -all -- \[\r\n\t] $sel " " sel
       procShowExpressionAsDot $sel
    }
	selection clear
}
proc VisualizeSelectedPredicate {} {
    set sel [selection get]
    if {$sel != ""} {
       regsub -all -- \[\r\n\t] $sel " " sel
       procVisualiseCustomPredicate_direct $sel
    }
	selection clear
}
proc AddSelectedFormulaToEvaluationView {} {
    set sel [selection get]
    if {$sel != ""} {
       regsub -all -- \[\r\n\t] $sel " " sel
       ::evaluationView::addUserFormulaToEvalView $sel
    }
	selection clear
}

proc EvaluateSelectedExpression {} {
	set sel [selection get]
	if {$sel != ""} {
		regsub -all -- \[\r\n\t] $sel " " sel
		set EvalSelection [escapeChars $sel]
		prolog "tcltk_eval(\'$EvalSelection\',Res,EnumWarning,Solution)"
		set Result $prolog_variables(Res)
		if [prolog real_error_occurred] {
			procShowErrors
		} else {
			if {$prolog_variables(Solution) != ""} {
				ShowEvaluatedExpressionInDialogWindow "$prolog_variables(Res)\n$prolog_variables(Solution)"
			} else {
				ShowEvaluatedExpressionInDialogWindow "$prolog_variables(Res)"
			}
		}
	}
	selection clear
}

proc procShowAnExpressionAsDot {} {
    if [procCheckBMode "Show Expression Value as Dot Graph"] {
	      global prompt dialog_checkbutton1
	      set f .promptExprDot
        Dialog_Create_Prompt_f $f "Expression as Graph" "Enter Expression to Visualize as Dot Graph\n(use B syntax; you can enter mutliple expressions using pairs and strings within pairs as labels, e.g., (\"lbl1\",rel1,\"lbl2\",rel2,...)):" 90 "Keep Dialog Open" "" ""
        set prompt(ok) 0
	      Dialog_Wait_Prepare $f prompt(ok) $f.entry
        set prompt(ok) 0
	      tkwait variable prompt(ok)
	      while {$prompt(ok)} {
				        procShowExpressionAsDot $prompt(result)
                set prompt(ok) 0
                if {$dialog_checkbutton1} {
				        focus $f.entry
	              tkwait variable prompt(ok)
	      }
		}
		Dialog_Wait_Finish $f
		Dialog_Dismiss $f
	}
}
proc procShowExpressionAsDot {Expr} {
	set EExpr [escapeChars $Expr]
	set dotName [procGetDefaultDotName]
	if [prolog "tcltk_show_expression_as_dot(\'$EExpr\','$dotName')"] {
	  procShowErrors
	  procVisualiseDotFileDefault $dotName
	} else {
	  procShowErrors1 "$Expr" none
	}
}
proc procGetDefaultDotName {} {
  global strFilename
	set rootName [file rootname $strFilename]
	set dotName {}
	append dotName $rootName ".dot"
	return $dotName
}
proc procVisualiseDotFileDefault {dotName} {
  global strFilename
	set rootName [file rootname $strFilename]
	set psName {}
	append psName $rootName ".pdf"
	procVisualiseDotFile $dotName $psName
}
proc procShowAnExpressionAsTable {} {
    global Goal
    if [procCheckBMode "Show Expression Value as Table"] {
		set Goal [Dialog_Promptww "Expression as Table" "Enter Expression to Visualize as Table (use B syntax):" 70 "" ""]
		if {$Goal != ""} {
			procShowExpressionAsTable $Goal
		}
	}
}
proc procShowExpressionAsTable {Expr} {
	if {$Expr != ""} {
		if [prolog "tcltk_eval_as_table(\'$Expr\',Res)"] {
            if [prolog real_error_occurred] {
                procShowErrors1 "$Expr" none
            } else {
                procShowTable $prolog_variables(Res) "Table" "Table for $Expr" "ValueTable" "" ""
            }
		} else {
			tkErrorBox "Evaluation as table failed for: $Expr"
		}
	}
}

proc ShowEvaluatedExpressionInDialogWindow {expr} {
	destroy .evalSelection
	# destroy added for Mac TclTk
    set f .evalSelection
	if [Dialog_Create $f "Evaluated Expression" -borderwidth 10] {

        frame $f.frmSource -borderwidth .1c -relief groove
        ctext $f.frmSource.text -yscrollcommand "$f.frmSource.scrolly set" \
         -setgrid 1 -height 8 -width 50 -state normal -linemap 0
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"

        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.text -expand 1 -fill both

		set b [frame $f.buttons]
		pack $f.frmSource $f.buttons -side top -fill x
		pack $f.frmSource -pady 5 -expand 1 -fill both
		button $b.ok -text Done -command {wm withdraw .evalSelection}
		pack $b.ok -side right

		bind $f.frmSource <Return> {wm withdraw .evalSelection}
		bind $f.frmSource <Control-c> {wm withdraw .evalSelection}
		$f.frmSource.text delete 1.0 end
		$f.frmSource.text insert end $expr
		procDoOnTheFlySyntaxColouring $f.frmSource.text
		$f.frmSource.text configure -state disabled
		focus .evalSelection
	}
}

proc VisualizeSelectedCSPProcess {} {
  set sel [selection get]
  if {$sel != ""} {
    regsub -all -- \[\r\n\t] $sel " " sel
    procVisualizeCSPProcess $sel
  }
  selection clear
}

proc procVisualizeCSPProcess {str} {
  global strFilename csp_assertions_dialog_arr
  # puts "$str"
  if [procSpecFileSuccessfullyLoaded] {
    set rootName [file rootname $strFilename]
    set dotName {}
    set psName {}
    append dotName $rootName ".dot"
    append psName $rootName ".pdf"
    set estr [escapeChars $str]
    set csp_assertions_dialog_arr(visualizing_status) "Exploring state space of process \'$str\'..."
    if [prolog "tcltk_explore_csp_process(\'$estr\',SpecNodeID)"] {
      procShowErrors
      set csp_assertions_dialog_arr(visualizing_status) "Getting staticstics for \'$str\'..."
      prolog get_csp_process_stats(S)
      set NodeID $prolog_variables(SpecNodeID)
      if {$prolog_variables(S) < 250} {
        set csp_assertions_dialog_arr(visualizing_status) "Writting to dot file..."
        prolog "tcltk_visualize_csp_process($NodeID,'$dotName')"
        set csp_assertions_dialog_arr(visualizing_status) "Visualizing dot file..."
        procVisualiseDotFile $dotName $psName
      } else {
        set ans [tk_messageBox -default no -message "The state space is large\
        ($prolog_variables(S) states)!\n\n\
        Are you sure you want to view it?" -title "Warning" -type yesno -icon warning -parent .]
        if {$ans == "yes"} {
        set csp_assertions_dialog_arr(visualizing_status) "Writting to dot file..."
        prolog "tcltk_visualize_csp_process($NodeID,'$dotName')"
        set csp_assertions_dialog_arr(visualizing_status) "Visualizing dot file..."
        procVisualiseDotFile $dotName $psName
        }
      }
      set csp_assertions_dialog_arr(visualizing_status) "Visualization of \'$str\' finished."
    } else {
      procShowErrors1 "$estr" none
    }
  } else {
       tkErrorBox "No specification file loaded. Cannot display statespace of process."
  }
}

proc procVisualizeCSPProcessMenu {} {
  global csp_assertions_dialog_arr
  destroy .cspVizMenu
  eval global viscsp
  set viscsp .cspVizMenu
  if [prolog animation_mode(cspm)] {
    set csp_assertions_dialog_arr(visualize_process) ""
    set csp_assertions_dialog_arr(visualizing_status) "No information available."
    if [Dialog_Create $viscsp "CSP Visualization" -borderwidth 10] {
      wm resizable $viscsp 0 0
      wm transient $viscsp .main.frmSource

      prolog "get_csp_processes(Procs)"
      set ListProcs [lsort -dictionary -unique $prolog_variables(Procs)]

      label $viscsp.menu_name -text "Visualize State Space of CSP Process"

      frame $viscsp.body -borderwidth 3 -relief raised
      label $viscsp.body.lproc -text "Choose a process to visualize: "
      ttk::combobox $viscsp.body.procs -textvariable csp_assertions_dialog_arr(visualize_process) -width 20 -state editable -value $ListProcs

      label $viscsp.result -textvariable csp_assertions_dialog_arr(visualizing_status)

      frame $viscsp.buttons -relief flat
      button $viscsp.buttons.vis   -text "Visualize" -command {procVisualizeCSPProcess $csp_assertions_dialog_arr(visualize_process)} -activebackground lightblue
      button $viscsp.buttons.close -text "Close"     -command {destroy .cspVizMenu} -activebackground lightblue

      pack $viscsp.buttons.close $viscsp.buttons.vis -side right -padx 5
      pack $viscsp.body.procs $viscsp.body.lproc -side right -padx 5
      pack $viscsp.menu_name -side top
      pack $viscsp.body -ipady 10 -ipadx 10 -side top -pady 15
      pack $viscsp.result -side top
      pack $viscsp.buttons -side top
    }
  } else {
    tkErrorBoxNoParent "Can only be applied to CSP specifications."
  }
}

proc GotoLineSelect {} {
   set LineNumber [Dialog_Prompt "[prob_str show_line_number]"]
   while {![string is integer $LineNumber]} {
    tkErrorBox "[prob_str invalid_line_number]"
    set LineNumber [Dialog_Prompt "[prob_str show_line_number]"]
   }
   if {$LineNumber != ""} {
    GotoLine .main.frmSource.text $LineNumber
   }
}

proc GotoLine {textWidget lineNo} {
    set BG   "lightgreen"	;# default background color for text
    set FG   "lightgray"	;# default foreground color for text
	$textWidget tag remove hilite 0.0 end
	$textWidget tag add hilite $lineNo.0 $lineNo.1000
	$textWidget tag configure hilite -background $BG
	# -foreground $FG
	$textWidget yview -pickplace [expr $lineNo-4]
}

proc procGetAccKey {} {
   if [prolog tools:host_platform(windows)] {
	     return "Control"
	} elseif [prolog tools:host_platform(darwin)] {
	     return "Command"
	} else {
	     return "Control"
	     # Meta ?
	}
}

global lastMenuUserCmd
set lastMenuUserCmd ""
proc procReLaunchLastMenuCmd {} {
   global lastMenuUserCmd
   set savedCmd $lastMenuUserCmd
   if {$lastMenuUserCmd != "procReLaunchLastMenuCmd" && $lastMenuUserCmd != ""} {
      procLaunchMenuCmd $lastMenuUserCmd
      set lastMenuUserCmd $savedCmd
   }
}
proc procLaunchMenuCmd {Cmd} {
   # if we start a command from a command key combo: do not remember it as last command
   #global lastMenuUserCmd
   #set lastMenuUserCmd $Cmd
   # wait > 100 ms to circumvent bug in latest Tk versions on Mac (see [ tktoolkit-Bugs-3285355])
   if [prolog tools:host_platform(darwin)] {
     # sometimes more than this needed; when we have a long complex file open (e.g., LandingSysDP_DGT_R3...)
     after 150 $Cmd
   } else { eval $Cmd }
}
proc procLaunchCmd {Cmd} {
   procLaunchDescCmd $Cmd $Cmd
}
proc procLaunchDescCmd {Desc Cmd} {
   global lastMenuUserCmd
   set lastMenuUserCmd $Cmd
   .frmMenu.mnuEdit entryconfigure 14 -label "Repeat ($Desc)"
   .frmMenu.mnuEdit entryconfigure 14 -state normal
   # puts "launch $Cmd"
   eval $Cmd
}
proc procSetCommandKeyShortCuts {} {
   set CMD [procGetAccKey]
   if [prolog tools:host_platform(darwin)] {
      event add <<RIGHTCLICK>> <Button-2>
      event add <<RIGHTCLICK>> <Control-Button-1>
      event add <<Comment>> <$CMD-/>
   } else {
      event add <<RIGHTCLICK>> <Button-3>
      event add <<Comment>> <$CMD-Shift-t>
      event add <<Comment>> <$CMD-Shift-T>
   }
   bind all <$CMD-minus> {procLaunchMenuCmd procAddDefaultCSPFile}

   bind .main.frmSource.text <$CMD-a> {procLaunchMenuCmd proc_selectAllMainText; break}
   bind .main.frmSource.text <$CMD-A> {procLaunchMenuCmd proc_selectAllMainText; break}
   # Ctrl-/ is a standard cammand key short-cut of the Tcl/Tk text widget (selects all text)
   bind .main.frmSource.text <<Comment>> {procLaunchMenuCmd {proc_commmentText .main.frmSource.text};break}

   bind all <$CMD-b> {procLaunchMenuCmd procTypeCheckFile}
   bind all <$CMD-b> {procLaunchMenuCmd procTypeCheckFile}
   bind all <$CMD-c> {procLaunchMenuCmd {procCutOrCopySlectedRangeOfText .main.frmSource.text "yes"}}
   bind all <$CMD-C> {procLaunchMenuCmd {procCutOrCopySlectedRangeOfText .main.frmSource.text "yes"}
                      #event generate .main.frmSource.text <<Copy>>
                      }
   bind all <$CMD-d> {procLaunchMenuCmd procDisplayVisitedStates}
   bind all <$CMD-D> {procLaunchMenuCmd procDisplayVisitedStates}
   bind all <$CMD-e> {procLaunchMenuCmd procOpenFileInEditor}
   bind all <$CMD-E> {procLaunchMenuCmd procOpenFileInEditor}
   bind all <$CMD-f> {procLaunchMenuCmd procLaunchFindMenu}
   bind all <$CMD-F> {procLaunchMenuCmd procLaunchFindMenu}
   bind all <$CMD-g> {procLaunchMenuCmd {procComputeCoverage 0}}
   bind all <$CMD-G> {procLaunchMenuCmd {procComputeCoverage 0}}
   bind all <$CMD-i> {procLaunchMenuCmd procAnalyseInvariant}
   bind all <$CMD-I> {procLaunchMenuCmd procAnalyseInvariant}
   bind all <$CMD-j> {procLaunchMenuCmd GotoLineSelect}
   bind all <$CMD-J> {procLaunchMenuCmd GotoLineSelect}
   # bind all <$CMD-p> {procJumpToAnOpenNode}
   # bind all <$CMD-P> {procJumpToAnOpenNode}

   bind all <$CMD-k> {procLaunchMenuCmd procDisplayReducedSigMergeWithOptions}
   bind all <$CMD-K> {procLaunchMenuCmd procDisplayReducedSigMergeWithOptions}
   bind all <$CMD-l> {procLaunchMenuCmd OpenLtlAssertionsViewer}
   bind all <$CMD-L> {procLaunchMenuCmd OpenLtlAssertionsViewer}
   bind all <$CMD-m> {procLaunchMenuCmd procModelCheck}
   bind all <$CMD-M> {procLaunchMenuCmd procModelCheck}
   bind all <Shift-$CMD-m> {procLaunchMenuCmd procModelCheckWithTLC}
   bind all <Shift-$CMD-M> {procLaunchMenuCmd procModelCheckWithTLC}
   bind all <$CMD-n> {procLaunchMenuCmd procNewFile}
   bind all <$CMD-N> {procLaunchMenuCmd procNewFile}
   bind all <$CMD-o> {procLaunchMenuCmd procOpenFile}
   bind all <$CMD-O> {procLaunchMenuCmd procOpenFile}

   bind all <Shift-$CMD-f> {procLaunchMenuCmd procLaunchFindAndReplaceMenu}
   bind all <Shift-$CMD-F> {procLaunchMenuCmd procLaunchFindAndReplaceMenu}
   bind all <$CMD-q> {procLaunchMenuCmd procDoQuit}
   bind all <$CMD-Q> {procLaunchMenuCmd procDoQuit}
   bind all <$CMD-r> {procLaunchMenuCmd procReOpenFile}
   bind all <$CMD-R> {procLaunchMenuCmd procReOpenFile}
   bind all <$CMD-s> {procLaunchMenuCmd procSaveFile}
   bind all <$CMD-S> {procLaunchMenuCmd procSaveFile}
   bind all <Shift-$CMD-s> {procLaunchMenuCmd procSaveAsFile}
   bind all <Shift-$CMD-S> {procLaunchMenuCmd procSaveAsFile}
   bind all <$CMD-t> {procLaunchMenuCmd procCheckTraceFileNoSuccessMsg}
   bind all <$CMD-T> {procLaunchMenuCmd procCheckTraceFileNoSuccessMsg}
   # bind all <$CMD-u> {procLaunchMenuCmd procCheckStatespaceWithFDR}
   # bind all <$CMD-U> {procLaunchMenuCmd procCheckStatespaceWithFDR}
   bind all <$CMD-u> {procLaunchMenuCmd procSaveReOpenFileAndFastForward}
   bind all <$CMD-U> {procLaunchMenuCmd procSaveReOpenFileAndFastForward}
   bind all <$CMD-v> {.main.frmSource.text highlight 1.0 end}
   bind all <$CMD-V> {procLaunchMenuCmd procPasteTheCopiedRangeOfText}
   bind all <$CMD-X> {procLaunchMenuCmd {procCutOrCopySlectedRangeOfText .main.frmSource.text "no"}
                      #event generate .main.frmSource.text <<Cut>>
                      }
   bind all <$CMD-y> {procLaunchMenuCmd procAdvancedFind}
   bind all <$CMD-Y> {procLaunchMenuCmd procAdvancedFind}

   # available shortcuts U, P

   bind all <$CMD-z> "procundo"
   bind all <$CMD-Z> "procundo"
   bind all <Shift-$CMD-z> "procredo"
   bind all <Shift-$CMD-Z> "procredo"

   bind all <$CMD-bracketleft> {procLaunchMenuCmd procAnalyseDEFGOAL}
   bind all <$CMD-bracketright> {procLaunchMenuCmd procAnalyseAssertions}

   bind all <$CMD-plus> {procLaunchMenuCmd openEvaluationView}


   bind all <Shift-$CMD-e> {procLaunchMenuCmd procExecuteOperation}
   bind all <Shift-$CMD-E> {procLaunchMenuCmd procExecuteOperation}
   bind all <Shift-$CMD-x> {procLaunchMenuCmd procRand}
   bind all <Shift-$CMD-X> {procLaunchMenuCmd procRand}
   bind all <Shift-$CMD-i> {procLaunchMenuCmd procVisualiseInvariant}
   bind all <Shift-$CMD-I> {procLaunchMenuCmd procVisualiseInvariant}
   bind all <Shift-$CMD-r> {procReLaunchLastMenuCmd}
   bind all <Shift-$CMD-R> {procReLaunchLastMenuCmd}
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
   destroy .
}

proc procDoQuitMain {win} {
   global forceQuit
   if [string equal $win ".__ctextTemp"] {
       return
   } else {
       procDoQuit
   }
}

proc procResetFilesMenu {} {
   global CSP_File_Already_Added
   .frmMenu.mnuFiles delete 0 999
   set CSP_File_Already_Added 0
}

proc procAddNewFileToFilesMenu {FileToShow Type} {
   global CSP_File_Already_Added
   if {$Type=="CSP-M"} {
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
     procDoSyntaxColouring .main.frmSource.text
     procEnableSourceCodeEditing
  } elseif {$Type=="CSP-M"} {
     procDoCSPSyntaxColouring  .main.frmSource.text
     procEnableSourceCodeEditing
  }
  procResetCodeModified false false
}

# -------

proc procRebuildRecentDocumentsMenu {} {
    # set lasti [.frmMenu.mnuFile.mnuRecentDocuments cget end]
    # puts "Rebuild: lasti $lasti"

    .frmMenu.mnuFile.mnuRecentDocuments delete 0 999

    prologmnf get_recent_documents(List)
    set ii 0
    set AccKey [procGetAccKey]
    foreach i $prolog_variables(List) {
       if {$ii < 1} {
          .frmMenu.mnuFile.mnuRecentDocuments add command -label "$i" -command "procOpenSpecificFile {$i}" -accelerator "$AccKey+$ii"
          set CMD [procGetAccKey]
          bind all <$CMD-$ii> "procLaunchMenuCmd {procOpenSpecificFile {$i}}"
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
	# enable Save As...
	.frmMenu.mnuFile entryconfigure 5 -state normal

    if {$normal_user} {
      .frmMenu.mnuFile entryconfigure 10 -state normal \
	     -label "Open $fileTail in External Editor"
	 }

    procSetWindowTitle ""

    # also reset State viewer cache:
    ResetStateViewerCache
}

proc procEnableItemsAfterOpeningFile {} {
    global expert_user normal_user

	procEnableReopen

	# enable Save Statespace and Load Saved Statespace
	.frmMenu.mnuFile entryconfigure 7 -state normal
	.frmMenu.mnuFile entryconfigure 8 -state normal

    # enable View Statespace and View
    #.frmMenu.mnuAnimate entryconfigure 8 -state normal
    #.frmMenu.mnuAnimate entryconfigure 9 -state normal

    .frmMenu.mnuEdit entryconfigure 0 -state disabled
	.frmMenu.mnuEdit entryconfigure 1 -state disabled

    procUpdateMenusFromPrologInfo
}
proc procDisableItemsAfterClosingFile {} {
	.frmMenu.mnuFile entryconfigure 5 -state disabled
	.frmMenu.mnuFile entryconfigure 7 -state disabled
	.frmMenu.mnuFile entryconfigure 10 -state disabled


    procResetFilesMenu
}

# -------
# procedure to initialise info section
# -------
proc procGUI_Info {} {
    frame .main.frmInfo -borderwidth .1c  -relief groove
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
    frame .main.frmInfo.frmHisto -relief groove
    label .main.frmInfo.frmHisto.label -text History -relief flat
    scrollbar .main.frmInfo.frmHisto.scrolly -command ".main.frmInfo.frmHisto.list yview"
    scrollbar .main.frmInfo.frmHisto.scrollx -command ".main.frmInfo.frmHisto.list xview" -orient h
    listbox .main.frmInfo.frmHisto.list -yscroll ".main.frmInfo.frmHisto.scrolly set" \
        -xscroll ".main.frmInfo.frmHisto.scrollx set" -setgrid 1 -height $pane_height -width $pane_width  -selectmode extended
    pack .main.frmInfo.frmHisto.label -side top -fill x -ipady 2
    pack .main.frmInfo.frmHisto.scrolly -side right -fill y
    pack .main.frmInfo.frmHisto.scrollx -side bottom -fill x
    pack .main.frmInfo.frmHisto.list -side left -expand 1 -fill both

    bind .main.frmInfo.frmHisto.label <<RIGHTCLICK>> {procPerformHistoryContextMenu %X %Y}
    bind .main.frmInfo.frmHisto.list <<RIGHTCLICK>> {procPerformHistoryContextMenu %X %Y}
    bind .main.frmInfo.frmHisto.list <Double-1> {procPerformHistoryClick}

    # ------ States
    frame .main.frmInfo.frmState -relief groove
    label .main.frmInfo.frmState.label -text StateProperties -relief groove
    # set StatusFrame .main.frmInfo.frmStatus
    global StatusFrame
    set StatusFrame .main.frmInfo.frmState.status
    frame $StatusFrame -relief groove
      # label $StatusFrame.invlbl -text "INVARIANT:" -fg blue -font "TkFixedFont 9"
      global InvKO InvOK InvOKWarn InvUnknown InvRef SwapProcs InvTimeout Timeout TimeoutEmpty TimeoutVirtual Maxreach MaxreachEmpty MaxreachSym tcl_dir ErrorIcon WarningIcon CheckOkIcon
      if [file exists "$tcl_dir/icons/InvUnknown.gif"] {
        image create photo InvUnknown -format gif -fil "$tcl_dir/icons/InvUnknown.gif"
        image create photo InvOK -format gif -fil "$tcl_dir/icons/InvOK.gif"
        image create photo InvKO -format gif -fil "$tcl_dir/icons/InvKO.gif"
        image create photo InvOKWarn -format gif -fil "$tcl_dir/icons/InvOK_Warning.gif"
        image create photo InvTimeout -format gif -fil "$tcl_dir/icons/InvTimeout.gif"
        image create photo InvRef -format gif -fil "$tcl_dir/icons/InvRef.gif"
        image create photo SwapProcs -format gif -fil "$tcl_dir/icons/Change.gif"
        image create photo Timeout -format gif -fil "$tcl_dir/icons/Timeout.gif"
        image create photo TimeoutEmpty -format gif -fil "$tcl_dir/icons/TimeoutEmpty.gif"
        image create photo TimeoutVirtual -format gif -fil "$tcl_dir/icons/TimeoutVirtual.gif"
        image create photo Maxreach -format gif -fil "$tcl_dir/icons/Maxreach.gif"
        image create photo MaxreachEmpty -format gif -fil "$tcl_dir/icons/MaxreachEmpty.gif"
        image create photo MaxreachSym -format gif -fil "$tcl_dir/icons/MaxreachSym.gif"
        image create photo BackEnabled -format gif -fil "$tcl_dir/icons/BackEnabled.gif"
        image create photo BackDisabled -format gif -fil "$tcl_dir/icons/BackDisabled.gif"
        image create photo ForwardEnabled -format gif -fil "$tcl_dir/icons/ForwardEnabled.gif"
        image create photo ForwardDisabled -format gif -fil "$tcl_dir/icons/ForwardDisabled.gif"
        image create photo WindowSubIcon -format gif -fil "$tcl_dir/icons/prob_sub_128.gif"
        image create photo ErrorIcon -format gif -fil "$tcl_dir/icons/error_triangle.gif"
        image create photo WarningIcon -format gif -fil "$tcl_dir/icons/warning_triangle.gif"
        image create photo CheckOkIcon -format gif -fil "$tcl_dir/icons/checked_ok.gif"
      } else {
        image create photo InvUnknown
        image create photo InvOK
        image create photo InvKO
        image create photo InvOKWarn
        image create photo Timeout
        image create photo TimeoutEmpty
        image create photo TimeoutVirtual
        image create photo Maxreach
        image create photo MaxreachEmpty
        image create photo MaxreachSym
        image create photo BackEnabled
        image create photo BackDisabled
        image create photo ForwardEnabled
        image create photo ForwardDisabled
        image create photo WindowSubIcon
        image create photo ErrorIcon
        image create photo WarningIcon
        image create photo CheckOkIcon
      }
      button $StatusFrame.inv -text OK -image InvUnknown -relief flat -command {procStatusFrameClick}
      #button $StatusFrame.maxreached -image MaxreachEmpty\
          -command {tkMessageBox "Possibly not all enabled operations were computed. Increase Max Number of Initialisations/Operations in the Animation Preferences and re-load the machine."}
      # label $StatusFrame.lbl2 -text "STATUS:" -font "TkFixedFont 9"
      label $StatusFrame.lbl2 -text "State Properties"
      bind $StatusFrame.lbl2 <<RIGHTCLICK>> {procPerformStatePropertyContextMenu %X %Y}
      button $StatusFrame.timeout -image TimeoutEmpty\
           -command {procTimeOutButton}
      pack   $StatusFrame.inv -side left
      #pack $StatusFrame.maxreached -side left
      pack $StatusFrame.lbl2 -side left -padx 15 -fill x
      pack $StatusFrame.timeout -side right  -padx 15
    scrollbar .main.frmInfo.frmState.scrolly -command ".main.frmInfo.frmState.list yview"
    scrollbar .main.frmInfo.frmState.scrollx -command ".main.frmInfo.frmState.list xview" -orient h
    listbox .main.frmInfo.frmState.list -yscroll ".main.frmInfo.frmState.scrolly set" \
        -xscroll ".main.frmInfo.frmState.scrollx set" -setgrid 1 -height $pane_height -width $pane_width  -selectmode extended
    bind .main.frmInfo.frmState.list <Button1-ButtonRelease> procShowStateDetail
    # pack .main.frmInfo.frmState.label -side top -fill x
    pack $StatusFrame -side top -fill x
    pack .main.frmInfo.frmState.scrolly -side right -fill y
    pack .main.frmInfo.frmState.scrollx -side bottom -fill x
    pack .main.frmInfo.frmState.list -side left -expand 1 -fill both
    bind .main.frmInfo.frmState.list <Double-1> {procPerformStateList}
    bind .main.frmInfo.frmState.list <<RIGHTCLICK>>  {procPerformStatePropertyContextMenu %X %Y}

    # ------ Available options
    frame .main.frmInfo.frmPerform -relief groove
    global OpStatusFrame
    set OpStatusFrame .main.frmInfo.frmPerform.status
    frame $OpStatusFrame -relief groove
    global spec_desc
    label $OpStatusFrame.label -text "Enabled $spec_desc(operations_lc)"
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

    scrollbar .main.frmInfo.frmPerform.scrolly -command ".main.frmInfo.frmPerform.list yview"
    scrollbar .main.frmInfo.frmPerform.scrollx -command ".main.frmInfo.frmPerform.list xview" -orient h
    listbox .main.frmInfo.frmPerform.list -yscroll ".main.frmInfo.frmPerform.scrolly set" \
         -xscroll ".main.frmInfo.frmPerform.scrollx set" -setgrid 1 -height $pane_height -width $pane_width
    bind .main.frmInfo.frmPerform.list <ButtonRelease> {procPerformOptionSingleClick}
    bind .main.frmInfo.frmPerform.list <Shift-ButtonRelease> {procPerformOptionSingleClickShift}
    bind .main.frmInfo.frmPerform.list <Double-1> {procPerformOption}
    bind .main.frmInfo.frmPerform.list <Return> {procPerformOption}
    pack $OpStatusFrame -side top -fill x
    pack .main.frmInfo.frmPerform.scrolly -side right -fill y
    pack .main.frmInfo.frmPerform.scrollx -side bottom -fill x
    pack .main.frmInfo.frmPerform.list -side left -expand 1 -fill both

    bind $OpStatusFrame.label <<RIGHTCLICK>> {procPerformOpContextMenu_woidx %X %Y}
    bind .main.frmInfo.frmPerform.list <<RIGHTCLICK>> {procPerformContextMenu %X %Y}

    # ------- pack all top frames
    pack .main.frmInfo.frmState .main.frmInfo.frmPerform .main.frmInfo.frmHisto -side left -expand yes -fill both
}

proc proc_updateMenuEntryStates {} {
    if {[.main.frmSource.text cget -state] == "disabled"} {
	set stateCut disabled; set stateCopy normal
	set statePaste disabled; set stateDelete disabled
	set stateundo disabled; set stateredo disabled
    } else {
	set stateCut [procSelectLabelStateOfTheTextContextMenu]
	set stateCopy [procSelectLabelStateOfTheTextContextMenu]
        set statePaste [procSelectLabelStateOfTheTextContextMenuPaste]
        set stateDelete [proc_textEditorExistsAndIsEnabled]
        set stateundo [procSelectLabelStateOfUndo]
        set stateredo [procSelectLabelStateOfRedo]
    }
    .frmMenu.mnuEdit entryconfigure 0  -state $stateundo
    .frmMenu.mnuEdit entryconfigure 1  -state $stateredo
    .frmMenu.mnuEdit entryconfigure 3  -state $stateCut
    .frmMenu.mnuEdit entryconfigure 4  -state $stateCopy
    .frmMenu.mnuEdit entryconfigure 5  -state $statePaste
    .frmMenu.mnuEdit entryconfigure 6  -state $stateDelete
}

proc proc_textEditorExistsAndIsEnabled {} {
    set editor .main.frmSource.text
    if {[winfo exists $editor]  && [$editor cget -state] == "normal"} {
        return normal
    }
    return disable
}

proc truncString {max str} {
   if {[string length $str] < $max} {
      return $str
   } else {
      return "[string range $str 0 $max]..."
   }
}

proc procCreateOperationsPopupMenu {hasTrace} {
    destroy .operationspopup
    menu .operationspopup -tearoff 0
    if {$hasTrace} {
        set state normal
    } else {
        set state disabled
    }
    set iOption [.main.frmInfo.frmPerform.list curselection]
    if {$iOption!=""} {
      set iOption [lindex $iOption 0]
      set SelectedOp [truncString 50 [.main.frmInfo.frmPerform.list get $iOption]]
      # Useful for Tk 8.6 on macOS where double click no longer works
      .operationspopup add command -label "Execute $SelectedOp" -command procPerformOption -state normal
      .operationspopup add sep
    }
    if [prolog tcltk_can_backtrack(Steps)] {
       .operationspopup add command -label "Skip Backward $prolog_variables(Steps) Step(s) to Beginning" -command procReset -state normal
       .operationspopup add sep
    }
    if [prolog tcltk_can_forward(Steps)] {
        .operationspopup add command -label "Skip Forward $prolog_variables(Steps) Step(s" -command procFastForward -state normal
    }
    .operationspopup add command -label "Random Animation (10)" -command procRand -state normal
    .operationspopup add command -label "Random Animation (100)" -command {procRandSteps 100 0 "fully_random"} -state normal
    .operationspopup add command -label "Execute..." -command {procExecute} -state normal
    .operationspopup add command -label "Execute (100)" -command {procExecuteUpTo 100} -state normal
    .operationspopup add sep
    .operationspopup add command -label "Show Event Trace" -command procShowEventTrace -state $state
   if [prolog animation_mode(b)] {
    .operationspopup add sep
    .operationspopup add command -label "Execute an Operation ..." -command procExecuteOperation -state normal
    if {[prolog b_top_level_operation(Name)] && [prolog animation_minor_mode(alloy)]} {
      global GSeqToFind
      set GSeqToFind $prolog_variables(Name)
      .operationspopup add command -label "CBC Execute $prolog_variables(Name)" -command {procCBCFindSequenceG} -state normal
    }
    .operationspopup add sep
    if [prolog tcltk_get_status(_,1,_TIMEOUT)] {
    .operationspopup add command -label "Recompute Operations Randomly" -command procRecomputeCurrentStateWithRandomEnum -state normal
    .operationspopup add sep
    }
    .operationspopup add command -label "Debug Operation PRE..." -command procDebugOperation -state normal
    .operationspopup add sep
    .operationspopup add command -label "Visualize Operation PRE..." -command procVisualiseOperationPre -state normal
    .operationspopup add command -label "Visualize Deadlock PO..." -command {procVisualiseInvariantOrOpPRE "_deadlockpo_"} -state normal
  }
}

proc procPerformLineColumnsContextMenu {X Y} {
    destroy .lincolpopup
    menu .lincolpopup -tearoff 0
    .lincolpopup add command -label "Goto Line..." -command {GotoLineSelect}
    .lincolpopup add sep
    global text_editor_arr
    if {$text_editor_arr(linenumbers)==true} {
        .lincolpopup add command -label "Hide Line Numbers" -command {procSetLinenumbersValueToTrue "false"}
    } else {
        .lincolpopup add command -label "Show Line Numbers" -command {procSetLinenumbersValueToTrue "true"}
    }
    if {$text_editor_arr(highlight_line)==true} {
        .lincolpopup add command -label "Disable Highlighting Current Line" -command {procSetHighlightCurrentLineValueToTrue "false"}
    } else {
        .lincolpopup add command -label "Highlight Current Line" -command {procSetHighlightCurrentLineValueToTrue "true"}
    }
    .lincolpopup add command -label "Text Editor Preferences..." -command {procSetTextEditorPreferences}
    tk_popup .lincolpopup $X $Y
}
proc procPerformHistoryContextMenu {X Y} {
    destroy .historypopup
    menu .historypopup -tearoff 0
    set iOption [.main.frmInfo.frmHisto.list curselection]
    if {$iOption!=""} {
      set iOption [lindex $iOption 0]
      set SelectedOp [truncString 40 [.main.frmInfo.frmHisto.list get $iOption]]
      puts "SelectedOp = $SelectedOp"
      # Useful for Tk 8.6 on macOS where double click no longer works
      .historypopup add command -label "Skip Backward to $SelectedOp" -command procPerformHistoryClick -state normal
    }
    if [prolog tcltk_can_backtrack(Steps)] {
       .historypopup add command -label "Skip Backward $prolog_variables(Steps) Step(s) to Beginning" -command procReset -state normal
    }
    if [prolog tcltk_can_forward(Steps)] {
      .historypopup add command -label "Skip Forward $prolog_variables(Steps) Step(s)" -command procFastForward -state normal
    }
    .historypopup add sep
    .historypopup add command -label "View History as Graph" -command {procDisplayTraceToCurrentState "no"} -state normal
    .historypopup add sep
    .historypopup add command -label "Save History to Trace File..." -command {procSaveTraceFile "prolog" 1}
    .historypopup add command -label "Evaluate Expression over History..." -command {procEvaluateExpressionOverHistory}
    .historypopup add sep
    .historypopup add command -label "Check Trace from Trace File..." -command {procCheckOtherTraceFile "prolog" "backtrack"}
    .historypopup add sep
    .historypopup add command -label "View History in VisB" -command {procReVisualiseLastVisB "history_and_vars"}
    global strFilename
    if {[prolog tcltk_can_backtrack(Steps)] && $strFilename != ""} {
        .historypopup add sep
        set AccKey [procGetAccKey]
        .historypopup add command -label "Update (Save,Reopen,Replay $prolog_variables(Steps) Steps" -command procSaveReOpenFileAndFastForward -state normal -accelerator $AccKey+P
    }
    tk_popup .historypopup $X $Y
}
proc procPerformStatePropertyContextMenu {X Y} {
    global normal_user expert_user
    destroy .statepopup
    menu .statepopup -tearoff 0
    .statepopup add command -label "Eval..." -command {procEvalConsole}
    if [prolog animation_mode(b)] {
      .statepopup add command -label "Open Evaluation View" -command openEvaluationView
      .statepopup add command -label "Analyse Invariant" -command {procAnalyseInvariant}
      .statepopup add command -label "Analyse Properties" -command {procAnalysePred properties}
      .statepopup add command -label "Analyse Assertions" -command procAnalyseAssertions
      .statepopup add sep
      .statepopup add command -label "View Current State as Graph" -command {procDisplayCurrentState "yes"}
      if {$normal_user} {
         .statepopup add command -label "View Current State as Custom Graph" -command {procDisplayCurrentState "custom"}
      }
    }
    if {$normal_user} {
       .statepopup add sep
       .statepopup add command -label "View Current State in VisB" -command {procReVisualiseLastVisB "current_state"}
    }
    if {$expert_user} {
       .statepopup add command -label "Debug VisB Items" \
		   -command {procLaunchCmd procShowVisBItems} -state normal
       .statepopup add command -label "Debug VisB Events" \
		   -command {procLaunchCmd procShowVisBEvents} -state normal
    }
    tk_popup .statepopup $X $Y
}

proc procPerformContextMenu {X Y} {
 # Context Menu for Enabled Operations pane
    set listy [winfo rooty .main.frmInfo.frmPerform.list]
    set ycoord [expr $Y - $listy]
    set index [.main.frmInfo.frmPerform.list nearest $ycoord]
    .main.frmInfo.frmPerform.list selection clear 0 end
    .main.frmInfo.frmPerform.list selection set $index
    .main.frmInfo.frmPerform.list activate $index
    set hasTrace [prolog tcltk_has_eventtrace($index)]
    procCreateOperationsPopupMenu $hasTrace
    tk_popup .operationspopup $X $Y
}
proc procPerformOpContextMenu_woidx {X Y} {
    procCreateOperationsPopupMenu 0
    tk_popup .operationspopup $X $Y
}

# show details about an event/operation
proc procShowEventTrace {} {
    set sel [.main.frmInfo.frmPerform.list curselection]
    if [expr [llength $sel] == 1] {
        puts "selection: $sel"
        if [prolog tcltk_show_eventtrace($sel,Result)] {
            set Result $prolog_variables(Result)
            procShowText6 $Result "Event Trace" none none "EventTrace.txt" ".txt"
        } else {
            tkErrorBox "Internal Error: Could not display event trace."
        }
    }
}

proc procShowStateDetail {} {
    set sel [.main.frmInfo.frmState.list curselection]
    if [expr [llength $sel] == 1] {
		if [prolog tcltk_get_detailed_state_error($sel,ErrorMsg,Line,Col,EL,EC)] {
		    # TO DO: allow returning multiple error lines
		    set line $prolog_variables(Line)
		    if {$line >= 0} {
		        procHighlightErrorSpan .main.frmSource.text $line $prolog_variables(Col) $prolog_variables(EL) $prolog_variables(EC)
		    }
			if {[procSpecFileSuccessfullyLoaded] && [prolog can_generate_dot_from_last_state_error]} {
			      procShowText6 $prolog_variables(ErrorMsg) "Detailed transition error" "Visualize" procVisualiseSpanPredicate "TransitionError.txt" ".txt"
			} else {
			      procShowText6 $prolog_variables(ErrorMsg) "Detailed transition error" "Edit in External Editor..." procEditStateErrorInExternalEditor "TransitionError.txt" ".txt"
			}
		}
    }
}

proc procEditStateErrorInExternalEditor {} {
    set sel [.main.frmInfo.frmState.list curselection]
    if [expr [llength $sel] == 1] {
		if ![prolog tcltk_open_state_error_in_editor($sel)] {
		  tkErrorBox "Could not open state error $sel in editor"
		}
	}
}

proc procTimeOutButton {} {
    prologmnf "preferences:get_preference(time_out,CurTO)"
	set Result $prolog_variables(CurTO)
	prologmnf "tcltk_get_ops_with_virtual_timeout(VOpList)"
	prologmnf "tcltk_get_ops_with_real_timeout(RealOpList)"
	set RList $prolog_variables(RealOpList)
	set VList $prolog_variables(VOpList)
	if {$RList != ""} {
	    if {$VList != ""} {
	      set vmsg " and virtual timeouts for $VList"
	    } else {
	      set vmsg ""
	    }
		set ans [tkYesNoMessageWarningBox "A timeout ($Result ms) has occurred while computing enabled operations ($RList$vmsg).\nRecompute this state without a timeout?" "Timeout!"]
		if { $ans == "yes" } {
			procRecomputeCurrentStateWithoutTimeOut
		}
	} elseif {$VList != ""} {
	    if [prolog preferences:get_preference(use_clpfd_solver,false)] {
	        set msg "\nTry turning on CLPFD in the animation preferences."
	    } else {
	        set msg "\nTry modifying your model to avoid enumeration of infinite sets."
	    }
	    tkErrorBox "A virtual timeout due to enumeration warnings has occurred while computing enabled operations ($VList).$msg"
	} elseif [prolog tcltk_get_ops_user_interrupt_occurred] {
	    set ans [tkYesNoMessageWarningBox "A user interrupt has occurred while computing enabled operations.\nRecompute this state without a timeout?" "User-Interrupt!"]
		if { $ans == "yes" } {
			procRecomputeCurrentStateWithoutTimeOut
		}
	}
}

proc procRecomputeCurrentStateWithoutTimeOut {} {
	prologmnf tcltk_mark_current_node_to_be_recomputed_wo_timeout
	global show_error_if_no_transition
	set show_error_if_no_transition 1
	set sst "Time for recomputing: [time {procInsertHistoryOptionsState}]"
	prolog "tools:print_message('$sst')"
}

proc procRecomputeCurrentStateWithRandomEnum {} {
	prologmnf tcltk_mark_current_node_to_be_recomputed_with_random_enum
	global show_error_if_no_transition
	set show_error_if_no_transition 1
	set sst "Time for recomputing: [time {procInsertHistoryOptionsState}]"
	prologmnf tcltk_finish_current_node_to_be_recomputed_with_random_enum
	prolog "tools:print_message('$sst')"
}


proc procSetSpecDesc {} {
    global spec_desc
    # update animation mode specific descriptions
    prolog get_specification_description(operation,Res)
    set spec_desc(operation) $prolog_variables(Res)
    set spec_desc(operation_lc) [string tolower $prolog_variables(Res)]
    prolog get_specification_description(operations,Res)
    set spec_desc(operations) $prolog_variables(Res)
    set spec_desc(operations_lc) [string tolower $prolog_variables(Res)]
    prolog get_specification_description(machine,Res)
    set spec_desc(machine) $prolog_variables(Res)
    set spec_desc(machine_lc) [string tolower $prolog_variables(Res)]
}

proc procUpdateSpecDesc {} {
    global spec_desc
    global OpStatusFrame
    procSetSpecDesc
    # TO DO: use first char from operations
    # $OpStatusFrame.label configure -text "Enabled $spec_desc(operations_lc)"
}


proc procSetLinenumbersValueToTrue {enable} {
    global text_editor_arr
    set text_editor_arr(linenumbers) $enable
    if {$enable == "true"} {
         prolog preferences:set_preference(show_line_numbers,true)
        .main.frmSource.text configure -linemap 1 -linemapfg grey50 -linemapbg #EDECEB
    } else {
         prolog preferences:set_preference(show_line_numbers,false)
        .main.frmSource.text configure -linemap 0 -linemapfg grey50 -linemapbg #EDECEB
    }
}

proc procSetHighlightCurrentLineValueToTrue {enable} {
    global text_editor_arr
    set text_editor_arr(highlight_line) $enable
    if {$enable == "true"} {
         prolog preferences:set_preference(highlight_current_line,true)
    } else {
         prolog preferences:set_preference(highlight_current_line,false)
         catch {.main.frmSource.text tag delete highlightCurrentLine}
    }
}

proc procSetPrefOption {arrayName pref_opt val} {
    upvar 1 $arrayName arr
    set arr($pref_opt) $val
    prolog preferences:set_preference($pref_opt,$val)
}

proc procSetlinecolumnsValue {} {
        global text_editor_arr find_menu_arr
        procUpdateIndicesAfterLoadingAFile
        set twin .main.frmSource.text
        bind $twin <Button-1> {.main.frmSource.text tag remove patternTag 1.0 end
                               if {$find_menu_arr(allpatterns)} {procHighlightAllPatterns $find_menu_arr(search) $find_menu_arr(exact_flag)}
                               after idle {procUpdatelinecolumns "click"}}
        bind $twin <<RIGHTCLICK>> { after idle {popupContextTextMenu %X %Y}; break}
        bind $twin <Shift-Up> {after idle {procUpdatelinecolumns "up" "shift"}}
        bind $twin <Shift-Down> {after idle {procUpdatelinecolumns "down" "shift"}}
        bind $twin <Key-Up> {after idle {procUpdatelinecolumns "up"}}
        bind $twin <Key-Down> {after idle {procUpdatelinecolumns "down"}}
        bind $twin <Key-Left> {after idle {procUpdatelinecolumns "move"}}
        bind $twin <Key-Right> {after idle {procUpdatelinecolumns "move"}}
        bind $twin <Return> {after idle {procUpdatelinecolumns "move"}}
        bind $twin <BackSpace> {after idle {procUpdatelinecolumns "move"}}
        bind $twin <Key> {after idle {procUpdatelinecolumns "insert" %K}}
        #bind $twin <<Selection>> {after idle {if {$text_editor_arr(highlight_line)} {procHighlightSelectionInAlreadyHighlightedLine}}}
}

proc procUpdateIndicesAfterLoadingAFile {} {
        global currentLinePositionInText currentColumnPositionInText currentMaxColumnPosition
        set position [.main.frmSource.text index end]
        regexp {([0-9]+).([0-9]+)} $position all line column
        incr line -1
        set currentLinePositionInText $line
        set column [string length [.main.frmSource.text get $line.0 $line.end ]]
        set currentColumnPositionInText $column
        set currentMaxColumnPosition $column
        # incr column
        UpdateEditorStatusLine $line $column
}

proc popupContextTextMenu {X Y} {
            CreatePopupMenuForText [.main.frmSource.text cget -state]
            tk_popup .operationsTextpopup $X $Y
}

proc CreatePopupMenuForText {st} {
  global curFileTypeOpened
	destroy .operationsTextpopup
        menu .operationsTextpopup -tearoff 0
	if {$st != "normal"} {
	  set stateCut disabled;set stateCopy normal;set statePaste disabled
		set stateDelete disabled;set stateundo disabled;set stateredo disabled
	} else {
	        set stateCut [procSelectLabelStateOfTheTextContextMenu]
	        set stateCopy [procSelectLabelStateOfTheTextContextMenu]
	        set statePaste [procSelectLabelStateOfTheTextContextMenuPaste]
		set stateDelete normal
       		set stateundo [procSelectLabelStateOfUndo]
        	set stateredo [procSelectLabelStateOfRedo]
	}
        if [prolog animation_mode(b)] {
        .operationsTextpopup add command -label "Visualize Selected Predicate" -command {VisualizeSelectedPredicate} -state normal
	      .operationsTextpopup add command -label "Add Selected Formula to Evaluation View" -command {AddSelectedFormulaToEvaluationView} -state normal
	      .operationsTextpopup add sep
        .operationsTextpopup add command -label "Selected Expression as Dot Graph" -command {VisualizeSelectedExpressionAsDot} -state normal
      	#.operationsTextpopup add command -label "Show Selected Expression As Table" -command {ShowSelectedExpressionAsTable} -state normal
	      # .operationsTextpopup add command -label "Evaluate Expression" -command {EvaluateSelectedExpression} -state normal
	      .operationsTextpopup add command -label "Value Coverage" -command {EvaluateSelectedExpressionOverStateSpace} -state normal
	      .operationsTextpopup add sep
	      .operationsTextpopup add command -label "Check LTL Formula" -command {CheckLTLFormulaFromPopupMenu} -state normal
	      .operationsTextpopup add sep  }
        if [prolog animation_mode(cspm)] {
            .operationsTextpopup add command -label "Visualize Selected Process" -command {VisualizeSelectedCSPProcess} -state normal
        }
        .operationsTextpopup add command -label "Undo" -command {procundo; proc_highlightCurrentLineIfNecessary} -state $stateundo
        .operationsTextpopup add command -label "Redo" -command {procredo; proc_highlightCurrentLineIfNecessary} -state $stateredo
        .operationsTextpopup add sep
        .operationsTextpopup add command -label "Cut" -command {procCutOrCopySlectedRangeOfText .main.frmSource.text "no"; proc_breakCommandExecution} -state $stateCut
        .operationsTextpopup add command -label "Copy" -command {procCutOrCopySlectedRangeOfText .main.frmSource.text "yes"; proc_breakCommandExecution} -state $stateCopy
        .operationsTextpopup add command -label "Paste" -command {procPasteTheCopiedRangeOfText; proc_breakCommandExecution} -state $statePaste
        .operationsTextpopup add command -label "Delete" -command {procDeleteTheSelectedRangeOfText; proc_breakCommandExecution} -state $stateDelete
        .operationsTextpopup add sep
        # does not work properly:
        .operationsTextpopup add command -label "Select All" -command {after idle {proc_selectAllMainText}; if [prolog tools:host_platform(darwin)] {break}} -state normal
        .operationsTextpopup add sep
        if {$curFileTypeOpened == "B" || $curFileTypeOpened == "CSP"} {
        .operationsTextpopup add command -label "Toggle Comment" -command {after idle {proc_commmentText .main.frmSource.text}; if [prolog tools:host_platform(darwin)] {break}} -state $stateCopy
        .operationsTextpopup add sep  }
        .operationsTextpopup add command -label "Find..." -command {procLaunchFindMenu} -state normal
        .operationsTextpopup add command -label "Replace..." -command procLaunchFindAndReplaceMenu -state normal
        .operationsTextpopup add command -label "Goto Line..." -command {GotoLineSelect} -state normal
        .operationsTextpopup add sep
	global text_editor_arr
	if {$text_editor_arr(linenumbers)} {
		.operationsTextpopup add command -label "Hide Line Numbers" -command {procSetLinenumbersValueToTrue "false"}
	} else {
		.operationsTextpopup add command -label "Show Line Numbers" -command {procSetLinenumbersValueToTrue "true"}
	}
	if {$text_editor_arr(highlight_brackets)} {
		.operationsTextpopup add command -label "Disable Bracket Highlighting" -command {procSetPrefOption text_editor_arr highlight_brackets false}
	} else {
		.operationsTextpopup add command -label "Enable Bracket Highlighting" -command {procSetPrefOption text_editor_arr highlight_brackets true}
	}
	if {$text_editor_arr(highlight_line)} {
		.operationsTextpopup add command -label "Disable Highlighting Current Line" -command {procSetHighlightCurrentLineValueToTrue "false"}
	} else {
		.operationsTextpopup add command -label "Highlight Current Line" -command {procSetHighlightCurrentLineValueToTrue "true"}
	}
  .operationsTextpopup add command -label "Text Editor Preferences..." -command {procSetTextEditorPreferences}
}

proc proc_commmentText {twin} {
  global curFileTypeOpened
  set sel_range [$twin tag ranges sel]
  set range_start [lindex [split $sel_range] 0]
  set range_end [lindex [split $sel_range] 1]
  set cmt_line ""
  if {$curFileTypeOpened == "B"} {
    set cmt_start "/*"
    set cmt_end   "*/"
    set cmt_line "//"
  } elseif {$curFileTypeOpened == "CSP"} {
    set cmt_start "{-"
    set cmt_end   "-}"
    set cmt_line "--"
  } else {
    set cmt_start ""
    set cmt_end   ""
    set cmt_line ""
  }
  if {$sel_range == ""} {
    proc_commmentTextLine $twin $cmt_line
  } else {
    if {$sel_range != "" && [.main.frmSource.text cget -state] == "normal"} {
      # puts "range : $sel_range"
      # puts "end_range lineend [$twin index "$range_end lineend"]"
      # puts "end_range linestart [$twin index "$range_end linestart"]"
      $twin insert $range_start $cmt_start
      if {$range_end != [$twin index "$range_end lineend"] && $range_end != [$twin index "$range_end linestart"]} {
        $twin insert [$twin index "$range_end + 2 chars"] $cmt_end
        set add_chars 4
      } else {
        $twin insert $range_end $cmt_end
        set add_chars 2
      }
      proc_highlightMultilineCommentBlocks
      proc_highlightMultilinePragmas
      $twin tag add sel [$twin index $range_start] [$twin index "$range_end + $add_chars chars"]
    }
  }
  proc_highlightCurrentLineIfNecessary
}

proc proc_commmentTextLine {twin cmt_line} {
  if {$cmt_line == ""} {
    #do nothing
  } else {
    regexp {([0-9]+).[0-9]+} [$twin index insert] all line
    $twin insert [$twin index "$line.0"] $cmt_line
  }
}

proc procSelectLabelStateOfTheTextContextMenu {} {
    set twin .main.frmSource.text
    if [winfo exists $twin] {
       if {[.main.frmSource.text tag range sel] == "" || [.main.frmSource.text cget -state] == "disabled"} {
           return disabled
       } else {
           return normal
       }
    } else {
       return disabled
    }
}

proc procSelectLabelStateOfTheTextContextMenuPaste {} {
    set twin .main.frmSource.text
    if [winfo exists $twin] {
        if {[.main.frmSource.text cget -state] == "normal"} {
            if [catch {expr {[selection get -selection CLIPBOARD] == ""}}] {
                return disabled
            } elseif {[selection get -selection CLIPBOARD] == ""} {
                return disabled
            } else {
                return normal
            }
        } else {
            return disabled
        }
    } else {
        return disabled
    }
}

proc procSelectLabelStateOfUndo {} {
    set modified [.main.frmSource.text edit modified]
    if { [catch {.main.frmSource.text edit undo} ] } {
         proc_setFileNotModified
         return disabled
    } else {
         catch {.main.frmSource.text edit redo}
         # Text was modified because of performing undo and then redo
         # by determining the state of the undo menu item.
         # We have to set thus the text widget to not modified, otherwise on every
         # click on the menu bar the "Source file not saved" dialog will be popped up.
         .main.frmSource.text edit modified $modified
         return normal
    }
}

proc procSelectLabelStateOfRedo {} {
    set modified [.main.frmSource.text edit modified]
    if {[catch {.main.frmSource.text edit redo}]} {
         return disabled
    } else {
         catch {.main.frmSource.text edit undo}
         .main.frmSource.text edit modified $modified
         return normal
    }
}

proc procCutOrCopySlectedRangeOfText {w is_copy} {
    global text_editor_arr
    set startIndex [lindex ["$w" tag range sel] 0]
    set endIndex [lindex ["$w" tag range sel] 1]
    if {![catch {set selectedString ["$w" get $startIndex $endIndex]}]} {
        clipboard clear
        clipboard append $selectedString
	if {$is_copy != "yes"} {
	        $w delete $startIndex $endIndex
	} else {
            catch {$w add tag sel $startIndex $endIndex}
        }
        set text_editor_arr(text_index) $startIndex
        proc_highlightCurrentLineIfNecessary
    }
}

proc procPasteTheCopiedRangeOfText {} {
    global text_editor_arr
    set lastSeenIndex $text_editor_arr(text_index)
    set content [selection get -selection CLIPBOARD]
    .main.frmSource.text insert insert $content
    set lines [split $content "\n"]
    set line_num [expr {[llength $lines] -1}]
    set chars [expr {[string length [lindex $lines end]] +1}]
    set text_editor_arr(text_index) [.main.frmSource.text index "$text_editor_arr(text_index) + $line_num lines + $chars chars"]
    .main.frmSource.text see $text_editor_arr(text_index)
    procHighlightTextSpecial .main.frmSource.text $lastSeenIndex $text_editor_arr(text_index)
    proc_highlightCurrentLineIfNecessary
    proc_highlightMultilineCommentBlocks
    proc_highlightMultilinePragmas
}

proc procDeleteTheSelectedRangeOfText {} {
    set editor .main.frmSource.text
    if {[$editor cget -state] == "normal"} {
        set selectionArray [$editor tag range sel]
        if {$selectionArray != ""} {
            set startIndex [lindex $selectionArray 0]
            set endIndex [lindex $selectionArray 1]
            $editor delete $startIndex $endIndex
            regexp {([0-9]+)\.[0-9]+} $startIndex match lineFirst
            regexp {([0-9]+)\.[0-9]+} $endIndex match lineLast
            procHighlightTextSpecial $editor $lineFirst.0 $lineLast.end
            tk::TextSetCursor $editor $startIndex ; # setting the text cursor to the first index of the deleted token
        }
        proc_highlightCurrentLineIfNecessary
        proc_highlightMultilineCommentBlocks
        proc_highlightMultilinePragmas
    }
}

proc proc_selectAllMainText {} {
   proc_selectAll .main.frmSource.text
}

proc proc_selectAll {w {loop false}} {
    $w tag add sel 1.0 end
    if {$loop} {
       if [prolog tools:host_platform(darwin)] {
           return -code break
       } else {
           return -code return
       }
    }
}

proc proc_breakCommandExecution {{end false}} {
    if {!$end} {
        # do nothing
    } else {
       after idle {
           tk::TextSetCursor .main.frmSource.text end
       }
    }
    if [prolog tools:host_platform(darwin)] {
        return -code break
    } else {
        return -code return
    }
}

proc proc_highlightCurrentLineIfNecessary {} {
    global text_editor_arr
    if {$text_editor_arr(highlight_line)} {
         regexp {([0-9]+)\.[0-9]+} [.main.frmSource.text index insert] match line
         procHighlightCurrentLine $line
    }
}

# -------
# procedure to initialise source code section
# -------
proc procGUI_Source {} {
    global tcl_version text_editor_arr
    prolog "get_preference(font_name,FontName)"
    set text_editor_arr(font_name) $prolog_variables(FontName)
    prolog "get_preference(font_size_only,FontSize)"
    set text_editor_arr(font_size) $prolog_variables(FontSize)
    if [prolog get_preference(tk_show_source_pane,true)] {
		if [prolog get_preference(use_small_window,true)] {
			set source_height 17
		} else {
			set source_height 23
		}
		# ------- code source frames
		frame .main.frmSource -borderwidth .1c -relief groove
		scrollbar .main.frmSource.scrolly -command ".main.frmSource.text yview"
		scrollbar .main.frmSource.scrollx -command ".main.frmSource.text xview" -orient h
                set TextFont [string map {" " ""} $text_editor_arr(font_name)]
                if {$text_editor_arr(linenumbers)} {
                     set linemapVar 1
                } else {
                     set linemapVar 0
                }
                ctext .main.frmSource.text  -yscrollcommand ".main.frmSource.scrolly set" \
                       -xscrollcommand ".main.frmSource.scrollx set" -setgrid 1 -height $source_height\
                       -linemap $linemapVar -state disabled -font "$TextFont $text_editor_arr(font_size)" -exportselection true
		frame .main.frmSource.statusframe -bg #EDECEB
		label .main.frmSource.statusframe.messagestatus -relief flat -justify center\
			-width [expr {[.main.frmSource cget -width]-45}] -bg #EDECEB
		label .main.frmSource.statusframe.linecolumns -relief flat -justify left -text "Ln 1, Col 0"\
                          -width [string length [string map {" " ""} "Ln 1, Col 0+"]] -bg #EDECEB
		label .main.frmSource.statusframe.modelstatus -relief flat -justify center -text ""\
                          -width 25 -bg #EDECEB
		#text .main.frmSource.text -yscroll ".main.frmSource.scrolly set" -xscroll ".main.frmSource.scrollx set" \
                #          -setgrid 1 -height $source_height -state disabled
    .main.frmSource.text edit modified 0
    proc_setFileNotModified
    pack .main.frmSource.statusframe.messagestatus -expand 1 -side left -fill both
    pack .main.frmSource.statusframe.linecolumns -expand 0 -side left -fill both
    pack .main.frmSource.statusframe.modelstatus -expand 0 -side left -fill both
		pack .main.frmSource.statusframe -expand 0 -side bottom -fill both
		pack .main.frmSource.scrolly -side right -fill y
		pack .main.frmSource.scrollx -side bottom -fill x
		pack .main.frmSource.text -expand 1 -fill both
                procSetlinecolumnsValue
		bind .main.frmSource.statusframe.linecolumns <<RIGHTCLICK>> {procPerformLineColumnsContextMenu %X %Y}
    } else {
		set source_height 0
		frame .main.frmSource -borderwidth .1c -relief groove
		text .main.frmSource.text -height $source_height -state disabled
		# pack .main.frmSource.text -expand 1 -fill both
    }

    #set font to monospaced font (windows defaults to times else)
    prologmnf "get_preference(font_size,PrefFont)"
    set chosenfont $prolog_variables(PrefFont)
    changeFont $chosenfont

    procSetLinenumbersValueToTrue "$text_editor_arr(linenumbers)"

    #  proxy all insert calls to the filter frame to keep modified state saved
    rename .main.frmSource.text ..main.frmSource.text
    proc .main.frmSource.text args {
        if [regexp {^(ins|del).*} [lindex $args 0]] { procSetCodeModified }
	    uplevel ..main.frmSource.text $args
    }
    focus -force .main.frmSource.text
    .main.frmSource.text configure -undo 1
    bind .main.frmSource.text <<Modified>>  procupdateState
    bind . <Destroy> [list procDoQuitMain %W]
}


proc procUpdatelinecolumns {click args} {
      global strFilename text_editor_arr
      set editor .main.frmSource.text
      set noInsertKeys [list "??" "Next" "Prior" "Home" "End" "KP_Enter"]
      set ix [lsearch -exact $noInsertKeys [lindex $args 0]]
      set line 1; set column 0
      if [winfo exists .main.frmSource.statusframe.linecolumns] {
         if {$click == "click"} {
             regexp {([0-9]+).([0-9]+)} [$editor index current] all line column
	     if {[$editor index $line.end] == "$line.$column"} {
		# this action prevents from not moving the cursor to the begin column of the next line
		# when the end of a line has been clicked
		tk::TextSetCursor $editor $line.$column
	     }
         } elseif {$click == "down"} {
                 regexp {([0-9]+).([0-9]+)} $text_editor_arr(text_index) all line column
                 set lineend [$editor index $line.end]
                 if [$editor compare $line.end == [$editor index "end -1 chars"]] {
                     if {[llength $args] >0 && [lindex $args 0] == "shift"} {
                         if [catch {set start [$editor index sel.first]}] {
                             set start $text_editor_arr(text_index)
                         }
                         tk::TextSetCursor $editor end
                         .main.frmSource.text tag add sel $start [$editor index "end -1 chars"]
                      } else {
                         tk::TextSetCursor $editor end
                      }
                      regexp {([0-9]+).([0-9]+)} [$editor index "end -1 chars"] all line column
                 } else {
                     regexp {([0-9]+).([0-9]+)} [$editor index insert] all line column
                 }
         } elseif {$click == "up"} {
                 if [$editor compare $text_editor_arr(text_index) < 2.0] {
                     if {[llength $args] >0 && [lindex $args 0] == "shift"} {
                         if [catch {set end [$editor index sel.last]}] {
                             set end $text_editor_arr(text_index)
                          }
                          tk::TextSetCursor $editor 1.0
                          .main.frmSource.text tag add sel 1.0 $end
                     } else {
                          tk::TextSetCursor $editor 1.0
                     }
                     set line 1; set column 0
                 } else {
                     regexp {([0-9]+).([0-9]+)} [$editor index insert] all line column
                 }
         } elseif {$click == "move" || $click == "insert"} {
                 regexp {([0-9]+).([0-9]+)} [$editor index insert] all line column
         } else {
             error "Option unknown $click by procedure procUpdatelinecolumns."
         }
         if {($strFilename != "") && [file exists $strFilename]} {
             if {([file mtime $strFilename] != $text_editor_arr(file_last_modified)) && !($text_editor_arr(deactivate_asking_for_reloading))} {
                # File was last modified by another program
                if {!$text_editor_arr(ask_when_content_changed) && ![$editor edit modified]} {
                    puts "file changed editor=$editor"
                  set strInFile [prob_read $strFilename]
                  procRefreshContentInTextEditor $editor $strInFile false
                  procresetundo
                  set text_editor_arr(file_last_modified) [file mtime $strFilename]
                  .main.frmSource.statusframe.modelstatus configure -text "File content was changed." -bg orange
                } else {
                  set ans [tk_messageBox -default yes -message "The file $strFilename changed on disk.\nDo you want\
                         to reload the file content into the text editor?" -title "Warning" -type yesno -icon warning -parent .]
                  if {$ans == "yes"} {
                    set strInFile [prob_read $strFilename]
                    procRefreshContentInTextEditor $editor $strInFile false
                    procresetundo
                    set text_editor_arr(file_last_modified) [file mtime $strFilename]
                    tk_messageBox -parent . -icon warning -message "File content in text editor changed!\
                      \nYou have to reload the current model to apply changes."
                  } else {
                    set text_editor_arr(deactivate_asking_for_reloading) true
                  }
                }
             }
         }
         if {$text_editor_arr(highlight_line)} {
              if {$ix < 0} {
                  procHighlightCurrentLine $line
              } else {
                     # do not update current line highlighting
              }
         }
         set text_editor_arr(text_index) "$line.$column"
         set twin .main.frmSource.text
         proc_setFileModifiedMarker
         $twin tag delete matchBracket
         if {$text_editor_arr(highlight_brackets)} {
             $twin tag delete matchBracket
             set symbol [$twin get $text_editor_arr(text_index)]
             if [procIsBracketSymbol $symbol] {
                 procCallHighlightMatchedPairSymbolProcedure $symbol $text_editor_arr(text_index)
             } else {
                 set symbol [$twin get "$text_editor_arr(text_index) -1 chars"]
                 procCallHighlightMatchedPairSymbolProcedure $symbol [$twin index "$text_editor_arr(text_index) -1 chars"]
            }
         }
         proc_highlightMultilineCommentBlocks ; # need to test efficiency of this procedure
         proc_highlightMultilinePragmas
        # incr column
        UpdateEditorStatusLine $line $column
      }
}

global temporary_status_message
set temporary_status_message ""
proc UpdateEditorStatusLine {line column} {
        global temporary_status_message
        prolog get_state_space_stats(Visited,Tr,Processed,Ignored)
        set statusMsg "States $prolog_variables(Processed)/$prolog_variables(Visited) processed"
        if {$prolog_variables(Ignored) != "0"} {
           append statusMsg ", $prolog_variables(Ignored) ignored"
        }
        append statusMsg ", Ln $line, Col $column"
        if [prolog "animation_minor_mode(X)"] {
           append statusMsg ", Mode $prolog_variables(X)"
        }
        if {$temporary_status_message != ""} {
           append statusMsg " $temporary_status_message"
           set temporary_status_message ""
        }
        .main.frmSource.statusframe.linecolumns configure -text "$statusMsg" \
                           -width [string length [string map {" " ""} "$statusMsg+"]]

}

proc procCallHighlightMatchedPairSymbolProcedure {symbol index} {
      switch -- $symbol {
        "\{" {procHighlightMatchedPairSymbol $index "-forwards" "\\\{" "\\\}" end}
        "\}" {procHighlightMatchedPairSymbol $index "-backwards" "\\\}" "\\\{" 1.0}
        "\[" {procHighlightMatchedPairSymbol $index "-forwards" "\\\[" "\\\]" end}
        "\]" {procHighlightMatchedPairSymbol $index "-backwards" "\\\]" "\\\[" 1.0}
        "\(" {procHighlightMatchedPairSymbol $index "-forwards" "\\(" "\\)" end}
        "\)" {procHighlightMatchedPairSymbol $index "-backwards" "\\)" "\\(" 1.0}
      }
}

proc procIsBracketSymbol {symbol} {
     if {[lsearch -exact [list "\{" "\}" "\[" "\]" "\(" "\)"] $symbol] > -1} {
          return true
     } else {
          return false
     }
}

# add * infornt of the file name when the file were modified
proc proc_setFileModifiedMarker {} {
    global strFilename text_editor_arr
    if {[.main.frmSource.text edit modified] && !$text_editor_arr(file_modified) &&
        $strFilename != ""} {
        set text_editor_arr(file_modified) 1
        procSetWindowTitle " - Edited"
    }
}

proc proc_setFileNotModified {} {
    global strFilename text_editor_arr
    if {![.main.frmSource.text edit modified] && $strFilename != ""} {
        set text_editor_arr(file_modified) 0
        procSetWindowTitle ""
    }
}

proc procSetWindowTitle {Extra} {
      global strFilename version
      set fileTail [file tail $strFilename]
      set ext [file extension $strFilename]
      if {$ext == ".tex"|| $ext == ".zed"} {
           wm title . "ProZ:  $version: \[$fileTail\]$Extra"
      } elseif {$ext == ".cspm" || $ext == ".csp"} {
           wm title . "ProCSP: $version \[$fileTail\]$Extra"
      } elseif {$ext == ".pml" || $ext == ".prom"} {
           wm title . "ProM: $version \[$fileTail\]$Extra"
      } elseif {$ext == ".P"} {
           wm title . "ProXTL $version: \[$fileTail\]$Extra"
      } else {
           wm title . "ProB $version: \[$fileTail\]$Extra"
      }
}

proc procupdateState {args} {

      # Check the modified state and update the label
      if { [.main.frmSource.text edit modified] } {
	     .frmMenu.mnuEdit entryconfigure 0 -state normal
	     .frmMenu.mnuEdit entryconfigure 1 -state normal
      } else {
	     .frmMenu.mnuEdit entryconfigure 0 -state disabled
	     .frmMenu.mnuEdit entryconfigure 1 -state disabled
         proc_setFileNotModified
      }
}

proc procundo {} {
      # edit undo throws an exception when there is nothing to
      # undo. So catch it.
      global undosperformed
      if { [catch {.main.frmSource.text edit undo}] } {
         bell
      } else {
          procHighlightTextSpecial .main.frmSource.text 1.0 end
          incr undosperformed
          .frmMenu.mnuEdit entryconfigure 1 -state normal
      }
}

proc procredo {} {
      # edit redo throws an exception when there is nothing to
      # undo. So catch it.
      if { [catch {.main.frmSource.text edit redo}] } {
         bell
      } else {
        procHighlightTextSpecial .main.frmSource.text 1.0 end
        global undosperformed
        if {$undosperformed >0} {
            set undosperformed [expr $undosperformed-1]
        }
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
     .main.frmSource.text edit modified 0
     proc_setFileNotModified
     # Clear the undo stack
     .main.frmSource.text edit reset
     .main.frmSource.text configure -undo 1
}

proc changeFont {chosenfont} {
    global chosefontval
    # puts "Changing Font: $chosefontval $chosenfont"
    set chosefontval $chosenfont

   #.main.frmSource.text configure -font $chosenfont
    prologmnf "get_preference(use_font_size_for_columns,UseForCols)"
    set useforcolumns $prolog_variables(UseForCols)
   if {$useforcolumns == "true"} {
      .main.frmInfo.frmHisto.label configure -font $chosenfont
      .main.frmInfo.frmHisto.list configure -font $chosenfont
      .main.frmInfo.frmPerform.label configure -font $chosenfont
      .main.frmInfo.frmPerform.list configure -font $chosenfont
      .main.frmInfo.frmState.label configure -font $chosenfont
      .main.frmInfo.frmState.list configure -font $chosenfont
   }
    prolog "set_preference(font_size,'$chosenfont')"
}

proc procLaunchFindMenu {} {
    global find_menu_arr
    set find_menu_arr(found_index) ""
    eval global find
    set find .findMenu
    if [winfo exists .findMenu] {
        raise .findMenu
    } else {
        if [winfo exists .replaceMenu] {
            destroy .replaceMenu
        }
        if [Dialog_Create $find "Find" -borderwidth 10] {
            wm resizable $find 0 0
            wm transient $find .
            set find_menu_arr(search) ""
            frame $find.searchFrame -relief flat
            label $find.searchFrame.searchLabel -text "Search for: "
            ttk::combobox $find.searchFrame.search -textvariable find_menu_arr(search) -state editable -values $find_menu_arr(search_hist)
            frame $find.check -relief flat
            ttk::checkbutton $find.check.matchCase -text "Match Case" -variable find_menu_arr(case) -onvalue true -offvalue false
            ttk::checkbutton $find.check.matchWord -text "Match entire word only" -variable find_menu_arr(word) -onvalue true -offvalue false
            ttk::checkbutton $find.check.searchBackwards -text "Search backwards" -variable find_menu_arr(backwards) -onvalue true -offvalue false
            ttk::checkbutton $find.check.wrapAround -text "Wrap around" -variable find_menu_arr(wraparound) -onvalue true -offvalue false
            frame $find.buttons -relief flat
            button $find.buttons.find -text Find -command {procSearchPatternInText .findMenu} -activebackground lightblue -width 6
            button $find.buttons.close -text Close -command {destroy .findMenu} -activebackground lightblue -width 6
            pack $find.searchFrame $find.check $find.buttons -side top -pady 5
            pack $find.searchFrame.searchLabel $find.searchFrame.search -side left
            pack $find.check.matchCase $find.check.matchWord $find.check.searchBackwards $find.check.wrapAround -side top -pady 1 -fill x
            pack $find.buttons.close $find.buttons.find -side right -padx 25
            bind .findMenu <Return> {$find.buttons.find invoke}
            bind .findMenu <Escape> {destroy .findMenu}
            focus -force $find.searchFrame.search
        }
    }
}

proc procLaunchFindAndReplaceMenu {} {
    global find_menu_arr
    eval global repl
    set repl .replaceMenu
    if [winfo exists .replaceMenu] {
        raise .replaceMenu
    } else {
        if [winfo exists .findMenu] {
            destroy .findMenu
        }
        if [Dialog_Create $repl "Replace" -borderwidth 10] {
            wm resizable $repl 0 0
            wm transient $repl .
            set find_menu_arr(search) ""
            set find_menu_arr(replace) ""
            frame $repl.searchFrame -relief flat
            label $repl.searchFrame.searchLabel -text "Search for: "
            ttk::combobox $repl.searchFrame.search -textvariable find_menu_arr(search) -state editable -values $find_menu_arr(search_hist)
            frame $repl.replaceFrame -relief flat
            label $repl.replaceFrame.replaceLabel -text "Replace with: "
            ttk::combobox $repl.replaceFrame.replace -textvariable find_menu_arr(replace) -state editable -values $find_menu_arr(replace_hist)
            frame $repl.check -relief flat
            ttk::checkbutton $repl.check.matchCase -text "Match Case" -variable find_menu_arr(case) -onvalue true -offvalue false
            ttk::checkbutton $repl.check.matchWord -text "Match entire word only" -variable find_menu_arr(word) -onvalue true -offvalue false
            ttk::checkbutton $repl.check.searchBackwards -text "Search backwards" -variable find_menu_arr(backwards) -onvalue true -offvalue false
            ttk::checkbutton $repl.check.wrapAround -text "Wrap around" -variable find_menu_arr(wraparound) -onvalue true -offvalue false
            frame $repl.buttons -relief flat
            button $repl.buttons.find -text Find -command {procSearchPatternInText .replaceMenu} -activebackground lightblue
            button $repl.buttons.replace -text Replace -command {procReplacePattern .replaceMenu} -activebackground lightblue -state disabled
            button $repl.buttons.replaceAll -text "Replace All" -command {procReplaceAllFoundPatterns .replaceMenu} -activebackground lightblue
            button $repl.buttons.close -text Close -command {destroy .replaceMenu} -activebackground lightblue
            pack $repl.searchFrame $repl.replaceFrame $repl.check $repl.buttons -side top -pady 5
            pack $repl.searchFrame.searchLabel $repl.searchFrame.search -side left -padx 8
            pack $repl.replaceFrame.replaceLabel $repl.replaceFrame.replace -side left
            pack $repl.check.matchCase $repl.check.matchWord $repl.check.searchBackwards $repl.check.wrapAround -side top -pady 1 -fill x
            pack $repl.buttons.close $repl.buttons.replaceAll $repl.buttons.replace $repl.buttons.find -side right -padx 3
            bind .replaceMenu <Return> {procSearchPatternInText .replaceMenu}
            bind .replaceMenu <Escape> {destroy .replaceMenu}
            focus -force $repl.searchFrame.search
        }
    }

}

proc procSearchPatternInText {win} {
     global find_menu_arr text_editor_arr
     set index [procGetFoundedStringIndex $win]
     if {$index != ""} {
        set text_editor_arr(text_index) $index
        set find_menu_arr(found_index) $index
        set NextTextIndex [procReturnEndTextIndex $text_editor_arr(text_index) $find_menu_arr(search)]
        procHighlightAllPatterns $find_menu_arr(search) $find_menu_arr(exact_flag)
        procHighlightPattern $text_editor_arr(text_index) $NextTextIndex
        set text_editor_arr(text_index) $NextTextIndex
        procAppendElementToFindWindowSearchHistory $find_menu_arr(search) $win.searchFrame.search "search"
        if [winfo exists $win.buttons.replace] {
            $win.buttons.replace configure -state normal
        }
    } else {
        .main.frmSource.text tag remove patternTag 1.0 end
        procHighlightAllPatterns $find_menu_arr(search) $find_menu_arr(exact_flag)
        set find_menu_arr(found_index) ""
    }
}

proc procHighlightAllPatterns {pattern flag} {
    global find_menu_arr
        if {$find_menu_arr(word)} {
            procHighlightAllMatchedEntireWordsInText $pattern $flag
        } else {
            procHighlightAllPatternsInText $pattern $flag
        }
}

proc procGetFoundedStringIndex {win} {
     global text_editor_arr find_menu_arr
     set find_menu_arr(search) [$win.searchFrame.search get]
     if {$find_menu_arr(search) != ""} {
         procDefineSearchingFlags
         if {!$find_menu_arr(wraparound)} {
             if {$find_menu_arr(direction) == "-backwards"} {
                 set endIndex 1.0
             } else {
                 set endIndex end
             }
         } else {
             set endIndex ""
         }
         set foundedIndex [procGetPatternIndexDependingOnTheMatchEntireWordOption .main.frmSource.text\
                                                                            $find_menu_arr(search) $find_menu_arr(direction) $text_editor_arr(text_index) $endIndex]
         if [procCheckIndicesAccordingToSearchDirection $text_editor_arr(text_index) $foundedIndex $find_menu_arr(search) $find_menu_arr(direction)] {
             # if we search backwards then we have to change the index parameter
             set text_editor_arr(text_index) [procReturnStartTextIndex $text_editor_arr(text_index) $find_menu_arr(search)]
             set foundedIndex [procGetPatternIndexDependingOnTheMatchEntireWordOption .main.frmSource.text\
                                                                            $find_menu_arr(search) $find_menu_arr(direction) $text_editor_arr(text_index) $endIndex]
          }
          return $foundedIndex
     } else {
          return ""
     }
}

proc procGetPatternIndexDependingOnTheMatchEntireWordOption {twin pattern direction start {end ""}} {
     global find_menu_arr
     if {$end == ""} {
         set endIndexArg {}
     } else {
         set endIndexArg [list $end]
     }
     if {$find_menu_arr(word)} {
         set cmd [concat [list procGetEntireWordMatchIndexOnly $twin $pattern $start $direction] $endIndexArg]
         return [eval $cmd]
     } else {
         set cmd [concat [list $twin search $direction $find_menu_arr(exact_flag) $pattern $start] $endIndexArg]
         return [eval $cmd]
     }
}

proc procGetEntireWordMatchIndexOnly {twin pattern start direction {end "end"}} {
     global find_menu_arr
     while 1 {
          set index [$twin search $direction -count length -regexp -- {([^\s\(\{\[\}\]\)\.\t\n\r;\"'\|,]+)} $start $end]
          if {$index == "" || $pattern == ""} {
              break
          }
          set wordEnd [$twin index "$index + $length chars"]
          set word [$twin get $index $wordEnd]
          if {$pattern == $word} {
              break
          } elseif {$find_menu_arr(exact_flag) == "-nocase"} {
              if {[string tolower $word] == [string tolower $pattern]} {
                  break
              }
          }
          if {$direction == "-backwards"} {
              set start $index
          } else {
              set start $wordEnd
          }
     }
     return $index
}


proc procHighlightAllMatchedEntireWordsInText {pattern flag} {
      global find_menu_arr
      set twin .main.frmSource.text
      if {$pattern != ""} {
          $twin tag remove allPatterns 1.0 end
          set start 1.0
          set highlightedPatterns 0
          while 1 {
              set index [$twin search -count length -regexp -- {([^\s\(\{\[\}\]\)\.\t\n\r;\"'\|,]+)} $start end]
              if {$index == ""} {
                  break
              }
              set wordEnd [$twin index "$index + $length chars"]
              set word [$twin get $index $wordEnd]
              if {$pattern == $word} {
                  $twin tag add allPatterns $index $wordEnd
                  incr highlightedPatterns
              } elseif {$flag == "-nocase"} {
                  if {[string tolower $pattern] == [string tolower $word]} {
                      $twin tag add allPatterns $index $wordEnd
                      incr highlightedPatterns
                  }
              }
              set start $wordEnd
          }
          if {$highlightedPatterns > 0} {
              set find_menu_arr(allpatterns) true
          }
          $twin tag configure allPatterns -background #FFFF00
      }
}

proc procDefineSearchingFlags {} {
     global find_menu_arr
     if {$find_menu_arr(case)} {
         set find_menu_arr(exact_flag) "-exact"
     } else {
         set find_menu_arr(exact_flag) "-nocase"
     }
     if {$find_menu_arr(backwards)} {
        set find_menu_arr(direction) "-backwards"
     } else {
        set find_menu_arr(direction) "-forwards"
     }
}

proc procReplacePattern {win} {
    global find_menu_arr
    if {$find_menu_arr(found_index) != ""} {
        set newPattern [$win.replaceFrame.replace get]
        set endIndex [procReturnEndTextIndex $find_menu_arr(found_index) $find_menu_arr(search)]
        .main.frmSource.text delete $find_menu_arr(found_index) $endIndex
        .main.frmSource.text insert $find_menu_arr(found_index) $newPattern
        procAppendElementToFindWindowSearchHistory $find_menu_arr(search) $win.searchFrame.search "search"
        procAppendElementToFindWindowSearchHistory $newPattern $win.replaceFrame.replace "replace"
        procHighlightTextSpecial .main.frmSource.text $find_menu_arr(found_index) end
        procSearchPatternInText $win
    } else {
        $win.buttons.replace configure -state disabled
    }
}


proc procUnicodeReplacement {} {
   set str2 [string map -nocase {
         "<=>" "\u21D4"
         "=>" "\u21D2"
         "&" "\u2227"
         "#" "\u2203"
         "!" "\u2200"
         " or " " \u2228 "
         " btrue " " \u22A4 "
         " bfalse " " \u22A5 "
         " not " " \uAC " " not(" " \uAC("
         "/=" "\u2260"
         "<=" "\u2264"
         ">=" "\u2265"
         "/<<:" "\u2284" "/<:" "\u2288"
         "<<:" "\u2282" "<:" "\u2286"
         ":=" ":=" "::" ":\u2208" ":\u2208" ":\u2208"
         "/:" "\u2209" ":" "\u2208"
         "\\/" "\u222A"
         "/\\" "\u2229"
         "|->" "\u21A6"
         "-->>" "-->>" "+->>" "+->>"
         "<->" "\u2194"
         "<->>" "<->>" "<<->" "<<->" "<<->>" "<<->>"
         "-->" "\u2192"
         "+->" "\u21F8"
         "{}" "\u2205"
         " INTEGER " " \u2124 " "(INTEGER)" "(\u2114)" "(INTEGER " "(\u2114 " " INTEGER)" " \u2114)"
         " NATURAL " " \u2115 " "(NATURAL)" "(\u2115)" "(NATURAL " "(\u2115 " " NATURAL)" " \u2115)"
         " NATURAL1 " " \u2115\u2081 " "(NATURAL1)" "(\u2115\u2081)" "(NATURAL1 " "(\u2115\u2081 " " NATURAL1)" " \u2115\u2081)"
         " POW " " \u2119 " " POW(" " \u2119(" "(POW(" "(\u2119("
         " POW1 " " \u2119\u2081 " " POW1(" " \u2119\u2081(" "(POW1(" "(\u2119\u2081("
         "*POW(" "\uD7\u2119(" "*POW " "\uD7\u2119 " "* POW " "\uD7 \u2119 " "* POW(" "\uD7 \u2119("
         "*seq(" "\uD7seq(" "*seq1(" "\uD7seq(" "*iseq1(" "\uD7iseq(" "*iseq1(" "\uD7iseq1("
         "*INTEGER " "\uD7\u2124 " "*INTEGER)" "\uD7\u2124)"
         "*NATURAL " "\uD7\u2115 " "*NATURAL)" "\uD7\u2115)"
         "*NATURAL1 " "\uD7\u2115\u2081 " "*NATURAL1)" "\uD7\u2115\u2081)"
         "%" "\u3BB"
         "~w" "~w" "~n" "~n" "~" "\u207B\uB9"
         "<<|" "\u2A64" "<|" "\u25C1" "|>>" "\u2A65" "|>" "\u25B7"
         } [.main.frmSource.text get 1.0 end]]
     # "-->>" "\u21A0" "+->>" "\u2900" and other functions not applied: not very readable; we could provide an option
	  .main.frmSource.text delete 1.0 end
	  .main.frmSource.text insert end $str2
	  procDoOnTheFlySyntaxColouring .main.frmSource.text
		proc_setFileModifiedMarker
}

proc procReplaceAllFoundPatterns {win} {
    procDefineSearchingFlags
    set pattern [$win.searchFrame.search get]
    set newPattern [$win.replaceFrame.replace get]
    if {[expr {$pattern != ""}] && [expr {$newPattern != ""}]} {
        procAppendElementToFindWindowSearchHistory $pattern $win.searchFrame.search "search"
        procAppendElementToFindWindowSearchHistory $newPattern $win.replaceFrame.replace "replace"
        set nextIndex [procGetPatternIndexDependingOnTheMatchEntireWordOption .main.frmSource.text \
                                                                $pattern "-forwards" 1.0 end]
        if {$nextIndex != ""} {
           set veryFirstIndex $nextIndex
            while {$nextIndex != ""} {
                 set nextEndIndex [procReturnEndTextIndex $nextIndex $pattern]
                 set newEndIndex [procReturnEndTextIndex $nextIndex $newPattern]
                 .main.frmSource.text delete $nextIndex $nextEndIndex
                 .main.frmSource.text insert $nextIndex $newPattern
                 set nextIndex [procGetPatternIndexDependingOnTheMatchEntireWordOption .main.frmSource.text \
                                                                $pattern "-forwards" $newEndIndex end]
            }
            procHighlightTextSpecial .main.frmSource.text $veryFirstIndex end
        }
    }
}

proc procAppendElementToFindWindowSearchHistory {element win whichHistory} {
    global find_menu_arr
    if {$element != ""} {
        set eElement [escapeChars $element]
        if {$whichHistory == "search"} {
            prolog preferences:add_element_to_history(searched_patterns,'$eElement',9)
        } else {
            prolog preferences:add_element_to_history(replaced_patterns,'$eElement',9)
        }
        set tmpList [list $eElement]
    } else {
        set tmpList {}
    }
    if {$whichHistory == "search"} {
        set historyList $find_menu_arr(search_hist)
    } else {
        set historyList $find_menu_arr(replace_hist)
    }
    foreach i $historyList {
        if {$eElement != $i} {
            lappend tmpList $i
        }
    }
    if {$whichHistory == "search"} {
        set find_menu_arr(search_hist) $tmpList
    } else {
        set find_menu_arr(replace_hist) $tmpList
    }
    $win configure -values [lrange $tmpList 0 8]
}

proc procHighlightPattern {startIndex endIndex} {
      .main.frmSource.text tag remove patternTag 1.0 end
      .main.frmSource.text tag remove allPatterns $startIndex $endIndex
      .main.frmSource.text tag add patternTag $startIndex $endIndex
      .main.frmSource.text tag configure patternTag -foreground white
      .main.frmSource.text tag configure patternTag -background #86ABD9
      regexp {([0-9]+)\.[0-9]+} $startIndex match line
      .main.frmSource.text see "$line.0"
}

proc procHighlightAllPatternsInText {pattern flag} {
      global find_menu_arr
      if {$pattern != ""} {
          .main.frmSource.text tag remove allPatterns 1.0 end
          set veryFirstIndex [procGetPatternIndexDependingOnTheMatchEntireWordOption .main.frmSource.text \
                                                                $pattern "-forwards" 1.0]
          if {$veryFirstIndex != ""} {
              set find_menu_arr(allpatterns) true
              set endIndex [procReturnEndTextIndex $veryFirstIndex $pattern]
              set nextIndex [procGetPatternIndexDependingOnTheMatchEntireWordOption .main.frmSource.text \
                                                                $pattern "-forwards" $endIndex]
              .main.frmSource.text tag add allPatterns $veryFirstIndex $endIndex
              .main.frmSource.text tag configure allPatterns -background #FFFF00
              while {$nextIndex != $veryFirstIndex} {
                 set nextEndIndex [procReturnEndTextIndex $nextIndex $pattern]
                 .main.frmSource.text tag add allPatterns $nextIndex $nextEndIndex
                 set nextIndex [procGetPatternIndexDependingOnTheMatchEntireWordOption .main.frmSource.text \
                                                                $pattern "-forwards" $nextEndIndex]
              }
          }
      }
}

proc procHighlightSelectionInAlreadyHighlightedLine {} {
     global text_editor_arr
     regexp {([0-9]+)\.[0-9]+} [.main.frmSource.text index insert] all line
     procHighlightCurrentLine $line
         if [procIsCurrentHighlightedLineIndexInSelectionRange $text_editor_arr(high_line_index)] {
             set startIndex [lindex [split [.main.frmSource.text tag range sel]] 0]
             set endIndex [lindex [split [.main.frmSource.text tag range sel]] 1]
             .main.frmSource.text tag remove highlightCurrentLine $startIndex $endIndex
          } else {
             # do nothing
          }
}

proc procIsCurrentHighlightedLineIndexInSelectionRange {line} {
     if [regexp {([0-9]+).[0-9]+\ ([0-9]+).[0-9]+} [.main.frmSource.text tag range sel] all linestart lineend] {
          if {[expr {$line >= $linestart}] && [expr {$line <= $lineend}]} {
                return 1
          } else {
                return 0
          }
     } else {
         return 0
     }
}

proc procHighlightCurrentLine {line} {
     global text_editor_arr
     set twin .main.frmSource.text
     set ltags [list hlite patternTag allPatterns executedTag executedauxTag executedControlFlowTag \
                   errcoltag errtag sel]
     if [$twin compare $line.end != [$twin index end]] {
         $twin tag delete highlightCurrentLine
         set nextline [expr {$line +1}]
         $twin tag add highlightCurrentLine $line.0 $nextline.0
         $twin tag configure highlightCurrentLine -background gray70
         set text_editor_arr(high_line_index) $line
         foreach i $ltags {
              catch {$twin tag raise $i}
         }
     }
}

proc lelement {element list} {
     foreach i $list {
        if {$i == $element} {return true} else {return false}
     }
}

proc procHighlightMatchedPairSymbol {index direction bracket symbol end} {
     global text_editor_arr
     set twin .main.frmSource.text
     set searchRE "[set bracket]|[set symbol]"
     set count 1
     set lastFound ""
     if {$direction == "-forwards"} {
         set index [$twin index "$index + 1 chars"]
     }
     while 1 {
         set found [$twin search $direction -regexp $searchRE $index $end]

         if {$found == ""} {
              return
         }
         if {$lastFound != "" && [$twin compare $found == $lastFound]} {
              return ; # in case the search is wrapping back after no matching symbol found
         }

         set lastFound $found
         set char [$twin get $found]
         set index $found
         if {$direction == "-forwards"} {
             # avoid endless loop by forwards search through the text
             set index [$twin index "$index + 1 chars"]
         }

         if {[string equal $char [subst $symbol]]} {
              incr count -1
              if {$count == 0} {
                  $twin tag add matchBracket $found
                  if {$text_editor_arr(highlight_line)} {
                      $twin tag configure matchBracket -background DarkGrey -foreground white
                  } else {
                      $twin tag configure matchBracket -background grey -foreground white
                  }
                  break
              }
         } elseif {[string equal $char [subst $bracket]]} {
              incr count
         }
     }
}

proc procIsSymbolPaired {str s1 s2} {
	set count 0; set i 0; set len [string length $str]
	while {$i < $len} {
		set ch [string index $str $i]
		if [string equal "$ch" "$s1"] {
			incr count
		} elseif [string equal "$ch" "$s2"] {
			incr count -1
		}
		incr i
	}
	return !$count
}

proc proc_highlightMultilinePragmas {} {
    global multiline_pragma_color
    set searchRE [proc_getMultilinePragmaSearchRE]
    proc_highlightMultilineRE $searchRE multiline_pragma $multiline_pragma_color
}

proc proc_highlightMultilineCommentBlocks {} {
    global multiline_cmt_color
    set searchRE [proc_getMultilineSearchRE]
    proc_highlightMultilineRE $searchRE multiline_comment $multiline_cmt_color
}

proc proc_highlightMultilineRE {searchRE tag_name col} {
     regexp {(.*)\|(.*)} $searchRE match start end
     set twin .main.frmSource.text
     set foundStart [$twin search -forwards -- $start 0.0 end]
     if {$foundStart != "" && [$twin cget -state] != "disabled"} {
         $twin tag delete $tag_name
         while 1 {
             set foundEnd [$twin search -forwards -- $end $foundStart end]
             if {$foundEnd == ""} {
                 proc_highlightCurrentCommentBlock $foundStart end $tag_name $col
                 return
             } else {
                 set nr_chars [string length $end]
                 proc_highlightCurrentCommentBlock $foundStart [$twin index "$foundEnd +$nr_chars chars"] $tag_name $col
             }
             set foundStart [$twin search -forwards -- $start $foundEnd end]
             if {$foundStart == ""} {
                 return
             }
         }
     } elseif {[$twin cget -state] != "disabled"} {
         $twin tag delete $tag_name
     }
}

proc proc_highlightCurrentCommentBlock {start end tag_name col} {
     set twin .main.frmSource.text
     $twin tag add $tag_name $start $end
     $twin tag configure $tag_name -foreground $col
}

proc proc_getMultilineSearchRE {} {
     global strFilename
     set file_ext [file extension $strFilename]
     set commentStartExpression "\/\*"
     set commentEndExpression "\*\/"
     if { $file_ext == ".csp" || $file_ext == ".cspm" } {
          set commentStartExpression "\{\-"
          set commentEndExpression "\-\}"
     } elseif {$file_ext == ".tla"} {
          set commentStartExpression "(\*"
          set commentEndExpression "\*)"
     }
     return "[set commentStartExpression]|[set commentEndExpression]"
}

proc proc_getMultilinePragmaSearchRE {} {
  global strFilename
  set file_ext [file extension $strFilename]
  set pragmaStartExpression "\/\*\@"
  set pragmaEndExpression "\*\/"
  if { $file_ext == ".csp" || $file_ext == ".cspm" } {
    set pragmaStartExpression "\{\-\#"
    set pragmaEndExpression "\#\-\}"
  }
  return "[set pragmaStartExpression]|[set pragmaEndExpression]"
}

proc procReturnEndTextIndex {index pattern} {
    regexp {([0-9]+)\.([0-9]+)} $index match line col
    set endcol [expr {$col + [string length $pattern]}]
    return $line.$endcol
}

proc procReturnStartTextIndex {index pattern} {
    regexp {([0-9]+)\.([0-9]+)} $index match line col
    set startcol [expr {$col - [string length $pattern]}]
    if {$startcol < 0} {
        set startcol 0
    }
    return $line.$startcol
}

proc procCheckIndicesAccordingToSearchDirection {index findex pattern direction} {
    if {[regexp {([0-9]+)\.([0-9]+)} $index match line col] && [regexp {([0-9]+)\.([0-9]+)} $findex match2 line2 col2]} {
        if {$direction == "-backwards"} {
            if {$col2 == [expr {$col - [string length $pattern]}]} {
                return 1
            } else {
                return 0
            }
        } else {
            return 0
        }
     } else {
        return 0
     }
}

# -------
# procedure to initialise main GUI
# -------

proc procInitPreferences {} {
    global mcPerformBreadthFirst mcFindDeadlocks mcFindInvViolations
    global mcSearchForNewErrors mcFindGOAL mcFindAssViolations mcFindStateErrors mcPartialOrderReduction
    set mcPerformBreadthFirst 0
    set mcFindDeadlocks 1
    set mcFindInvViolations 1
    set mcSearchForNewErrors 1
    set mcFindGOAL 0
    set mcFindAssViolations 0
    set mcFindStateErrors 1
    set mcPartialOrderReduction off

    # Setting TLC Model Checker Options
    global mcTLCFindDeadlocks mcTLCFindInvViolations mcTLCFindAssertionViolations mcTLCFindGOAL mcTLCCheckLTLFormulas mcTLCCheckWorkers mcTLCSymm
    set mcTLCFindDeadlocks 1
    set mcTLCFindInvViolations 1
    set mcTLCFindAssertionViolations 1
    set mcTLCCheckLTLFormuls 0
    set mcTLCFindGOAL 0
    set mcTLCSetupConstants 0
    set mcTLCSymm 0

###########################################
    global mc_prompt
    set mc_prompt(result) "100000"
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

# procedures for using the place geometry manager
proc Pane_Create {f1 f2 args} {

   # Map optional arguments into array values
   set t(-orient) vertical
   set t(-percent) 0.5
   set t(-in) [winfo parent $f1]
   array set t $args

   # Keep state in an array associated with the master frame
   set master $t(-in)
   upvar #0 Pane$master pane
   array set pane [array get t]
   # Create the grip and set placement attributes that
   # will not change. A thin divider line is achieved by
   # making the two frames one pixel smaller in the
   # adjustable dimension and making the main frame black.
   set pane(1) $f1
   set pane(2) $f2
   set cursor $t(-cursor)
   set pane(grip) [frame $master.grip -background snow3 \
      -width 10 -height 10 -bd 1 -relief raised \
      -cursor $cursor]
   if {[string match vert* $pane(-orient)]} {
      set pane(D) Y;# Adjust boundary in Y direction
      place $pane(1) -in $master -x 0 -rely 0.0 -anchor nw \
         -relwidth 1.0 -height -1
      place $pane(2) -in $master -x 0 -rely 1.0 -anchor sw \
         -relwidth 1.0 -height -1
      place $pane(grip) -in $master -anchor c -relx 0.8
   } else {
      set pane(D) X ;# Adjust boundary in X direction
      place $pane(1) -in $master -relx 0.0 -y 0 -anchor nw \
         -relheight 1.0 -width -1
      place $pane(2) -in $master -relx 1.0 -y 0 -anchor ne \
         -relheight 1.0 -width -1
      place $pane(grip) -in $master -anchor c -rely 0.8
   }
   $master configure -background grey60
   # Set up bindings for resize, <Configure>, and
   # for dragging the grip.
   bind $master <Configure> [list PaneGeometry $master]
   bind $pane(grip) <ButtonPress-1> \
      [list PaneDrag $master %$pane(D)]
   bind $pane(grip) <B1-Motion> \
      [list PaneDrag $master %$pane(D)]
   bind $pane(grip) <ButtonRelease-1> \
      [list PaneStop $master]
   # Do the initial layout
   PaneGeometry $master
}

proc PaneDrag {master D} {
   upvar #0 Pane$master pane
   if [info exists pane(lastD)] {
      set delta [expr double($pane(lastD) - $D) \
                             / $pane(size)]
      set pane(-percent) [expr $pane(-percent) - $delta]
      if {$pane(-percent) < 0.0} {
         set pane(-percent) 0.2
      } elseif {$pane(-percent) > 1.0} {
         set pane(-percent) 0.8
      }
      PaneGeometry $master
   }
   set pane(lastD) $D
}
proc PaneStop {master} {
   upvar #0 Pane$master pane
   catch {unset pane(lastD)}
}

proc PaneGeometry {master} {
   upvar #0 Pane$master pane
   if {$pane(D) == "X"} {
       place $pane(1) -relwidth $pane(-percent)
       place $pane(2) -relwidth [expr 1.0 - $pane(-percent)]
       place $pane(grip) -relx $pane(-percent)
       set pane(size) [winfo width $master]
   } else {
       place $pane(1) -relheight $pane(-percent)
       place $pane(2) -relheight [expr 1.0 - $pane(-percent)]
       place $pane(grip) -rely $pane(-percent)
       set pane(size) [winfo height $master]
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
    pack .main.frmSource -expand true -fill both
    pack .main.frmInfo -expand true -fill both
    pack .main -expand true -fill both
    pack propagate .main off
    Pane_Create .main.frmSource .main.frmInfo -percent 0.6 -orient vertical -in .main \
           -showhandle 0 -cursor double_arrow

    # ------- arrange all top frames
    #pack .main.frmInfo .main.frmSource -side bottom -expand yes -pady 1 -fill both

#    pack .main.frmInfo -side bottom -expand no -pady 1 -fill both

#    pack .main.frmSource -side bottom -expand yes -pady 1 -fill both


   # wm geometry . 80x50

    procAboutProBInSourceFrame
}


proc putlog {s} {
  global logFile
  if {$logFile != ""} {
     puts $logFile $s
  }
}

proc procCheckInvStatus {iNVVIOLATED} {
   if [prolog tcltk_get_status($iNVVIOLATED,_,_)] {
     puts "Invariant violated = $iNVVIOLATED"
   } else {
     tkErrorBox "Invariant status ($iNVVIOLATED) not as expected !"
   }
}

proc procPerformJavaParserCheck {} {
   if [prolog parsercall:check_java_version(V,Status)] {
      if {$prolog_variables(Status)=="compatible"} {
         if [prolog parsercall:get_version_from_parser(PV)] {
             tkMessageBox "Java and parser version check successful.\n$prolog_variables(V)\nB parser available in version $prolog_variables(PV)."
         } else {
             tkErrorBox "Java version check successful but cannot start B parser!\n$prolog_variables(V)"
         }
      } else {
         tkErrorBox "Java version check *not* successful!\nProB Java parser cannot be used.\n$prolog_variables(V)"
      }
   } else {
     tkErrorBox "Could *not* check Java version ! Try reinstalling Java 1.7 or higher."
   }
}

proc procPerformSelfCheck {} {
  global curFileTypeOpened
  if {$curFileTypeOpened != "None"} {
      tkErrorBox "The Self-Check can only be performed before opening a specification.\nRestart ProB and do the self-check after startup."
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
      tkErrorBox "The Self-Check can only be performed before opening a specification.\nRestart ProB and do the self-check after startup."
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
      tkErrorBox "The Self-Check can only be performed before opening a specification.\nRestart ProB and do the self-check after startup."
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
    	{"Event-B System Files"	{.sys}	}
    	{"B Rules Machine"	    {.rmch}	}
    	{"B Definition Files"	{.def}	}
    	{"All B Files"	{.mch .ref .imp .sys .def}	}
    	{"CSP Files"		{.csp}	}
    	{"TLA Files"		{.tla}	}
    	{"Alloy Files"		{.als}	}
	    {"All files"		*}
    }
    # show the dialog box
    set oldstrFilename $strFilename
    proc_getSaveFile $types
    if {$strFilename != ""} {
        # Now create an empty Machine File
        set MachName [file rootname [file tail $strFilename]]
        set ext [file extension $strFilename]
		set fileid [prob_open "$strFilename"]
		if {$ext == ".cspm" || $ext == ".csp"} {
		  puts $fileid "-- $MachName\nMAIN = SKIP\n"
		} elseif {$ext == ".tex" || $ext == ".zed"} {
		  puts $fileid "\\documentclass[a4paper]{article}\n\\usepackage{fuzz}\n\\begin{document}\n\\end{document}\n"
		} elseif {$ext == ".P"} {
		  puts $fileid "start(a).\ntrans(lock,a,b).\ntrans(unlock,b,a).\nprop(X,X).\nprop(c,unsafe).\n"
		} elseif {$ext == ".tla"} {
		  puts $fileid "----- MODULE $MachName -----\nEXTENDS Naturals\nCONSTANTS lim\nASSUME lim=10\nVARIABLES x\nInit == x = 1\nInc == x < lim /\\ x' = x+1\nDec == x >0 /\\ x'=x-1\nNext == Inc \\/ Dec\n=======================\n"
		} elseif {$ext == ".rmch"} {
		  puts $fileid "RULES_MACHINE $MachName\nEND\n"
		} elseif {$ext == ".als"} {
		  puts $fileid "sig NaturalNumber {succ: one NaturalNumber,predc: lone NaturalNumber}\n"
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
    global strFilename
    # show the dialog box
   if [prolog animation_mode(cspm)] {
      set untitled "Untitled.csp"
      set XT ".csp"
   } elseif [prolog animation_mode(xtl)] {
      set untitled "Untitled.P"
      set XT ".P"
   } else {
      set untitled "Untitled.mch"
      set XT ".mch"
   }
    set strFilename [proc_getSaveFilename $types $untitled $XT]
    if {$strFilename != ""} {
	    # remember the directory for next time
	     setMachinesPath [file dirname $strFilename]
    }

}

proc proc_getSaveFilename { types untitled XT} {
    set machinesPath [getMachinesPath]
    if {$machinesPath != "" && [file isdirectory $machinesPath]} {
         return [tk_getSaveFile -filetypes $types -initialdir $machinesPath -initialfile "$untitled" -parent . -defaultextension "$XT"]
    } else {
         return [tk_getSaveFile -filetypes $types -initialfile "$untitled" -parent .  -defaultextension "$XT"]
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
			{"All B and CSP Files"	{.mch .ref .imp .sys .rmch .csp .eventb .v .cspm .def}	}
			{"B Machine Files"		{.mch}	}
			{"B Refinements"		{.ref}	}
			{"B Implementations"	{.imp}	}
    	    {"Event-B System Files"	{.sys}	}
    	    {"B Rules Machine"	    {.rmch}	}
			{"B Definition Files"	{.def}	}
            {"TLA Files"            {.tla}}
    }
    set otherTypes {
        {"Other Formalisms (CSP,Z,XTL)"	{.csp .tex .zed .P .cspm .pml .prom .eventb .tla .als}	}
    }
    if {$curFileTypeOpened == "B" || $curFileTypeOpened == "CSP" ||
        $curFileTypeOpened == "None" || $curFileTypeOpened == "EVENTB"} {
        set types [concat $btypes $otherTypes]
    } else {
        set types [concat $otherTypes $btypes]
    }
    # append all files
    lappend types {"All files" *}
    # show the dialog box
    set oldstrFilename $strFilename
    proc_getOpenFile $types
    if {$strFilename != ""} {
        after idle {procGenericLoadFile}
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
    global strFilename find_menu_arr tcl_version text_editor_arr fastForwardAfterLoad
    set fastForwardAfterLoad 0
    # now avoid automatic showing of VisB current state when we later re-open VisBLastModelFile again:
    global VisBLastModelFile VisBLastKind
    if {$VisBLastModelFile != $strFilename} {set VisBLastKind "none"}

    if [file exists $strFilename] {
        .main.frmSource.text configure -undo 0
        prolog preferences:add_recent_document('$strFilename')
        # save preferences in case loading fails; we could also save a flag indicating that we are trying to load to detect crashes !
        procSavePrefs
        procRebuildRecentDocumentsMenu
        #procToUnicode
        #proc_updateMenuEntryStates
        procConfigureEventBMenues disabled
        set find_menu_arr(allpatterns) false
		set ext [file extension $strFilename]
        procResetContentInAssertionsCBCFormulaViewers
        set text_editor_arr(text_index) 1.0
        set text_editor_arr(reopening_file) false
        set text_editor_arr(disable_highlighting) false
        set text_editor_arr(file_last_modified) [file mtime $strFilename]
        # prolog "unset_tcltk_cspm_mode"
        if {$ext == ".tex" || $ext == ".zed"} {
           procLoadZFile
        } elseif {$ext == ".cspm" || $ext == ".csp"} {
           procLoadCSPMFile
        } elseif {$ext == ".smv"} {
           # procLoadSmvFile
           tkErrorBox "Loading of SMV files is curently not supported."
        } elseif {$ext == ".P"} {
           procLoadXTLFile
        } elseif {$ext == ".als"} {
           procLoadAlloyFile
        } elseif {$ext == ".eventb"} {
           procLoadEventBPackage
           procConfigureEventBMenues normal
        } elseif {$ext == ".tla"} {
           procLoadTLAFile
        } else {
           procLoadBFile
        }
        procUpdateIndicesAfterLoadingAFile
    } else {
	    tkErrorBox "File does not exist: $strFilename.\nCannot open it."
    }
}


proc procResetContentInAssertionsCBCFormulaViewers {} {
	if [winfo exists .checkCspAsser] {
		procResetAssertionsViewerContents
	}
	if [winfo exists .cbcViewer] {
		procResetCBCViewerContents
	}
}

proc procLoadEventBPackage {} {
   global strFilename curFileTypeOpened
   if {$strFilename != ""} {
        prologmnf tcltk_clear_machine
	      procResetHistory
        procResetState "" LightGray
        procResetOptions "Opening..." DarkGray
        procEnableReopen
        set curFileTypeOpened "EVENTB"
        if [prolog tcltk_load_packaged_eventb_file('$strFilename')] {
           procShowErrors
		   procInitLoadedBMachine
		   procUpdateViewsAfterLoad
           #puts "Showing machine representation...."
           set Unicode "true"
           if [prolog bmachine:b_show_machine_representation(Res,true,false,$Unicode)] {
               procShowSourceCodeFromCodes .main.frmSource.text $prolog_variables(Res)
               #puts "Highlighting source code (begin)...."
               procDoOnTheFlySyntaxColouring .main.frmSource.text
               #puts "Highlighting source code (end)...."
               #procDoSyntaxColouring .main.frmSource.text
               .main.frmSource.statusframe.modelstatus configure -text "" -bg #EDECEB
               procFastForwardIfRequested
           } else {
               procShowSourceCode $strFilename
               procDoOnTheFlyEventBSyntaxColouring .main.frmSource.text
               #procDoEventBSyntaxColouring .main.frmSource.text
               tkErrorBox "Internal Error: Could not display internal representation for EventB model."
           }
        } else {
           procShowErrors
           procClearOptionsStateHistory
           procUpdateViewsAfterLoad
           if [prolog tools_printing:tcltk_nested_read_prolog_file_as_codes('$strFilename',Codes)] {
               procShowSourceCodeFromCodes .main.frmSource.text $prolog_variables(Codes)
           } else {
              procShowSourceCode $strFilename
           }
           procDoEventBSyntaxColouring .main.frmSource.text
        }
        procResetCodeModified false false
        procResetFilesMenu
        global expert_user
        if {$expert_user} {
          .main.frmSource.text configure -state normal
        } else {
          .main.frmSource.text configure -state disabled
        }
  }
}


proc proc_getOpenFile { types } {
    global strFilename
    # show the dialog box
    set machinesPath [getMachinesPath]
    set strFilename [proc_openFile $machinesPath $types .]
    if {$strFilename != ""} {
	      #remember the directory for next time
	      setMachinesPath [file dirname $strFilename]
    }

}

proc proc_openFile {machinesPath types parent} {
    if {$machinesPath != "" && [file isdirectory $machinesPath]} {
         set Filename [tk_getOpenFile -filetypes $types -initialdir $machinesPath -parent $parent ]
    } else {
         set Filename [tk_getOpenFile -filetypes $types -parent $parent ]
    }
    return $Filename
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
      ::bench::procBenchmark
   }
   if [prolog tcltk_get_command_line_option(run_regression_tests)] {
      puts "Running Regression Tests"
      ::bench::procRegressionTest
   }
   if [prolog tcltk_get_command_line_option(run_full_law_tests)] {
      ::bench::procLawCheckTest "true"
   }
   if [prolog tcltk_get_command_line_option(run_quick_law_tests)] {
      ::bench::procLawCheckTest "false"
   }
   if [prolog tcltk_get_command_line_option(run_cspm_tests)] {
      ::bench::procCSPMBenchmark
   }
   if [prolog tcltk_get_command_line_option(tcltk_open_last_file)] {
      prologmnf get_recent_documents(List)
	  set strFilename [lindex $prolog_variables(List) 0]
      if {$strFilename != ""} {
         procDisableItemsAfterClosingFile
         procGenericLoadFile
      }
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
    global strFilename curFileTypeOpened cspstrFilename text_editor_arr
    if {$strFilename != ""} {
        set curFileTypeOpened "XTL"
        set cspstrFilename ""
        global version
        procEnableReopen
        prolog tcltk_open_xtl_file('$strFilename')
            # the file exists, so load it.
        procShowSourceCode $strFilename
        if {!$text_editor_arr(ctext_package)} {
            procDoOnTheFlyXTLSyntaxColouring .main.frmSource.text
        } else {
            procDoXTLSyntaxColouring .main.frmSource.text
        }
        procFinishLoading
        procFastForwardIfRequested
    }
}

proc procLoadAlloyFile {} {
    procCheckIfSaved
    global strFilename curFileTypeOpened cspstrFilename text_editor_arr
    if {$strFilename != ""} {
        set curFileTypeOpened "ALLOY"
        set cspstrFilename ""
        global version
        procEnableReopen
	      procResetHistory
	      procResetOptions "Opening..." DarkGray
        procShowSourceCode $strFilename
        if [prolog tcltk_open_alloy_file('$strFilename')] {
           procFinishLoading
        } else {
           procFinishUnsuccessfulLoading
        }
        if {!$text_editor_arr(ctext_package)} {
            procDoOnTheFlyAlloySyntaxColouring .main.frmSource.text
        } else {
            # procDoXTLSyntaxColouring .main.frmSource.text
        }
    }
}

proc CallFuzz {FUZZ FUZZLIB FuzzName} {
   puts "call $FUZZ -p $FUZZLIB >$FuzzName"
    global strFilename curFileTypeOpened cspstrFilename
	    if {[catch {exec  "$FUZZ" -d -l -p "$FUZZLIB" $strFilename >$FuzzName} errid]} {
           # a .fuzz file will be created even if the specification contains an error
           # We have to delete it. If not, it will be used next time without calling fuzz.
           file delete $FuzzName
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
		 .main.frmSource.text see "$linenr.0"
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
		 .main.frmSource.text see "$linenr.0"
		 return 1
	 }
   }
   return 0
}

proc HighlightFDR3Err {errid} {
   # sample FDR3 message: /Users/.../demo/crossing.csp:249:1-22:
   set idx [string first ":" $errid]
   # puts "errid=$errid"
   if {$idx != -1} {
	 set start [expr $idx+1]
	 set end [string first ":" $errid $start]
	 if {$end != -1 && $end > $start} {
	     set end1 [expr $end+1]
	     set colstartidx [string first "-" $errid $end1]
	     set end [expr $end-1]
		 set linenr [string range $errid $start $end]
		 #  puts "Found line: $linenr ($start - $end) $colstartidx"
		 if {$colstartidx == -1} {
			 procHighlightLine $linenr
			 .main.frmSource.text see "$linenr.0"
		 } else {
		    set colfrom [expr [string range $errid $end1 [expr $colstartidx-1]]-1]
		    # puts "found col=$colfrom"
	        set colendidx [string first ":" $errid [expr $colstartidx+1]]
		    if {$colendidx == -1} {
			  procHighlightLine $linenr
			} else {
		      set colto [expr [string range $errid [expr $colstartidx+1] [expr $colendidx-1]]-1]
		      procUnderlineColumnFromTo $linenr $colfrom $linenr $colto
			}
			.main.frmSource.text see "$linenr.$colfrom"
		 }
		 return 1
	 }
   }
   return 0
}

proc CallFuzzNoLib {FUZZ FuzzName} {
    puts "call fuzz without lib: $FUZZ d -l $strFilename >$FuzzName"
    global strFilename curFileTypeOpened cspstrFilename
	    if {[catch {exec  "$FUZZ" -d -l $strFilename >$FuzzName} errid]} {
           HighlightLineErr $errid ":"
           tkErrorBox "Error while executing fuzz Z parser ($FUZZ).\nError: $errid"
           return 0
        } else {
           return 1
        }
}

proc procLoadZFile {} {
    global strFilename curFileTypeOpened cspstrFilename text_editor_arr
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
            procEnableReopen
            procShowSourceCode $strFilename
            procResetCodeModified false $text_editor_arr(reopening_file)

            if {$recompileRequired} {
              if [file exists $FuzzName] { file delete $FuzzName }
              global lib_dir
              if {[prolog tools:host_platform(windows)]} { set EXT ".exe" } else { set EXT "" }
              if [file exists "$lib_dir/fuzzlib"] {
                   set ok [CallFuzz "$lib_dir/fuzz$EXT" "$lib_dir/fuzzlib" $FuzzName]
              } elseif [file exists "$lib_dir/fuzz$EXT"] {
                   set ok [CallFuzzNoLib "$lib_dir/fuzz$EXT" $FuzzName]
	            } else {
                   set ok [CallFuzzNoLib "fuzz$EXT" $FuzzName]
              }
            } else {
              set ok 1
            }

            if {!$text_editor_arr(ctext_package)} {
                procDoOnTheFlyZedSyntaxColouring .main.frmSource.text
            } else {
                procDoZedSyntaxColouring .main.frmSource.text
            }

            if {$ok && [file exists $FuzzName]} {
                # the file exists, so load it.
                set success [prolog tcltk_open_z_file('$FuzzName')]
                procJumpToTheLastSeenTextIndex
                procShowErrors
                if $success {
                    procInitLoadedBMachine
		            procSetCurrentTimeStampForModel
                } else {
                    tkErrorBox "This is not a valid Z specification.\nLoading fuzz file '$FuzzName' failed."
                }
                procUpdateViewsAfterLoad
                procFastForwardIfRequested
            } elseif {$ok} {
                tkErrorBox "An error occurred while parsing $strFilename. Be sure to have fuzz 2000 installed."
            }
        procResetFilesMenu
        procEnableSourceCodeEditing
    }
}

proc runJavaJarOnFile {JAR FILE} {
    #set MacJava6 "/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Commands/java"
    #if [file exists $MacJava6] {set MyJava $MacJava6} else {set MyJava "java"}
    set MyJava "java"
    # if [prolog get_preference(use_large_jvm_for_parser,true)] {}
    # puts "using large JVM for $JAR and $FILE"
    # exec $MyJava -Xms300m -Xmx1500m -Xss10m -jar $JAR $FILE
    exec $MyJava -jar $JAR $FILE
}

proc procLoadTLAFile {} {
   global strFilename curFileTypeOpened lib_dir cspstrFilename text_editor_arr
   if {$strFilename != ""} {
        prologmnf tcltk_clear_machine
	      procResetHistory
        procResetState "" LightGray
        procResetOptions "Opening..." DarkGray
        set curFileTypeOpened "TLA"
        set cspstrFilename ""
        procEnableReopen
        # puts "File to parse: $strFilename"
        if {[catch {runJavaJarOnFile $lib_dir/TLA2B.jar $strFilename} result]} {
            procShowSourceCode $strFilename
            procLaunchTLASyntaxColouring .main.frmSource.text
            procShowErrorAndHighlightTextTLA "$result"
            procResetCodeModified false $text_editor_arr(reopening_file)
            procClearOptionsStateHistory
        } else {
            set TLAFilename [file rootname $strFilename]
            #set generatedMchFile "$TLAFilename\_tla\.mch"
            set generatedProbFile "$TLAFilename.prob"
            procShowSourceCode $strFilename
            procResetCodeModified false $text_editor_arr(reopening_file)
            procLaunchTLASyntaxColouring .main.frmSource.text
            if [file exists $generatedProbFile] {
               puts $generatedProbFile
               if [prolog eclipse_interface:load_prob_file('$generatedProbFile')] {
               #if [prolog tcltk_open_b_file_for_minor_mode('$generatedMchFile','tla')]
                   procJumpToTheLastSeenTextIndex
                   procShowErrors
                   prolog set_animation_minor_mode(tla)
                   procInitLoadedBMachine
                   procSetCurrentTimeStampForModel
	            } else {
	            # Syntax Error or other problem with main machine
                   procShowErrors
                   procClearOptionsStateHistory
	            }
	            procUpdateViewsAfterLoad
	            procFastForwardIfRequested
            } else {
               tkErrorBox "B translation file $generatedProbFile does not exist.\nCheck that you have the correct version of TLA2B installed."
               procShowErrors
               procClearOptionsStateHistory
            }
       }
       procEnableSourceCodeEditing
   }
}

proc procShowErrorAndHighlightTextTLA {result} {
   global lib_dir
   set lines [split $result \n]
   set outputedError {}
   set positionSeen 0
   foreach i $lines {
        if {[expr {[string first "Encountered" $i] != -1}] && [expr {$positionSeen == 0}]} {
             set tokens [split $i]
             if {[regexp {([0-9]+).*} [lindex $tokens [expr {[lsearch -exact $tokens line] +1}]] match line] &&\
                 [regexp {([0-9]+).*} [lindex $tokens [expr {[lsearch -exact $tokens column] +1}]] match column]} {
                 procHighlightLineIn .main.frmSource.text $line
                 set column [incr column -1]
                 procUnderlineColumn $line $column
		         .main.frmSource.text see $line.$column
                 incr positionSeen
             }
        } elseif {[regexp {\s*line ([0-9]+), col ([0-9]+) to line ([0-9]+), col ([0-9]+).*} \
                  $i match startline startcolumn endline endcolumn] && [expr {$positionSeen == 0}]} {
                 set startcolumn [incr startcolumn -1]
                 procHighlightErrorSpan .main.frmSource.text $startline $startcolumn $endline $endcolumn
                 incr positionSeen
        }
        lappend outputedError $i
   }
   if [file exists $lib_dir/TLA2B.jar] {
     procShowList3 $outputedError "TLA2B Error Messages" "The following errors occurred:" \
    0 0 "Reinstall TLA2B" installTLATools "TLA2B_Output.txt" ErrorIcon
   } else {
     set MSG "\n\nEnsure that you have installed TLA2B.jar into ProB's lib directory!\nMore information can be found in the Help menu."
     set MSG2 "\nYou can also use the 'Install TLA2B' button below."
     procShowList3 $outputedError "TLA2B Error Messages" "The following errors occurred:$MSG\n$MSG2" \
    0 0 "Install TLA2B" installTLATools "TLA2B_Output.txt" ErrorIcon
   }
}



proc installLTL2BA {} {
  global lib_dir
  set LTL2BA_URL "https://stups.hhu-hosting.de/downloads/ltl2ba_pl/sicstus4.7"
  prolog tools:host_platform(Platform)
  if [tools:host_processor(arm)] {
    set platform $prolog_variables(Platform)-arm64
  } else {
    set platform $prolog_variables(Platform)-64
  }
  if {$prolog_variables(Platform) eq "linux"} {
    set archive_lib "ltl2ba.so"
  } elseif {$prolog_variables(Platform) eq "darwin"} {
    set archive_lib "ltl2ba.bundle"
    set platform $prolog_variables(Platform)-64
  } else {
    set archive_lib "ltl2ba.dll"
  }
  puts "archive library $platform/$archive_lib"
  set LTL2BA_URL_Tool "$LTL2BA_URL/$platform/$archive_lib"
  if [regexp {windows-.*} $platform matched] {
    tkMessageBox "Please download LTL2BA from $LTL2BA_URL_Tool and move it to $lib_dir."
  } else {
    set DOWNLOAD_CMD [list exec curl $LTL2BA_URL_Tool -o $lib_dir/$archive_lib]
    puts "$DOWNLOAD_CMD"
    if { [catch {exec curl $LTL2BA_URL_Tool -o $lib_dir/$archive_lib 2> /dev/null} errid] } {
      tkErrorBox "Error while executing '$DOWNLOAD_CMD'.\nError:\n$errid"
    } else {
      puts "LTL2BA has been succesfully installed."
      tkMessageBox "LTL2BA has been succesfully installed."
    }
  }
}

proc procLaunchTLASyntaxColouring {twin} {
    global text_editor_arr
    if {!$text_editor_arr(ctext_package)} {
        procDoOnTheFlyTLASyntaxColouring $twin
    } else {
        procDoTLASyntaxColouring $twin
    }

}



proc procLoadCSPMFile {} {
    global strFilename curFileTypeOpened checkCspAsser text_editor_arr
    if {$strFilename != ""} {
        set curFileTypeOpened "CSP"
        set cspstrFilename ""
        if [file exists $strFilename] {
            askToSaveNewAssertionsFromCheckCspWin "no" .
        }
        procEnableReopen
        procShowSourceCode $strFilename
        if [prolog tcltk_open_cspm_file('$strFilename')] {
            # the file exists, so load it.
            procJumpToTheLastSeenTextIndex
            procFinishLoading
            procFastForwardIfRequested
        } else {
            procErrorBoxWithErrorMessages "Parsing of CSP-M file failed!"
            procFinishUnsuccessfulLoading
        }
        if {!$text_editor_arr(ctext_package)} {
            if [winfo exists .eval] {
                 procDoOnTheFlyCSPSyntaxColouring .eval.frmSource.text
            }
            procDoOnTheFlyCSPSyntaxColouring .main.frmSource.text
        } else {
            procDoCSPSyntaxColouring .main.frmSource.text
        }
        # prolog "set_tcltk_cspm_mode"
        if [winfo exists .checkCspAsser] {
            global UsedCspStrFilename
            set UsedCspStrFilename $strFilename
            procUpdateCheckCspAssertionsWindow .checkCspAsser true
        }
   }
}


proc procErrorBoxWithErrorMessages {Message} {
   if [prolog real_error_occurred] {
          procShowErrors2 "" .main.frmSource.text "$Message" "$Message\n" "errors" ErrorIcon
   } else {
          tkErrorBox "$Message"
   }
}

proc procSetCurrentTimeStampForModel {} {
	# setting timestamp for the currently loaded model needed for updating the status label
	# in case that the current file content changed then the currently loaded model
	# is not anymore consistent with the file content of the script
	global text_editor_arr strFilename
	set text_editor_arr(current_timestamp) [file mtime $strFilename]
	set text_editor_arr(last_timestamped_file) $strFilename
}

proc procFinishLoading {} {
    global show_error_if_no_transition text_editor_arr
    set show_error_if_no_transition 1
	procResetCodeModified false $text_editor_arr(reopening_file)
	procResetFilesMenu
	procShowErrors
	# Show errors that occur during loading
	procSetSpecDesc
	prologmnf tcltk_initialise
	procInsertHistoryOptionsState
	procEnableItemsAfterOpeningFile
	procEnableSourceCodeEditing
	procShowErrors
	procSetCurrentTimeStampForModel
	procJumpToTheLastSeenTextIndex
	procUpdateViewsAfterLoad
}

proc procFinishUnsuccessfulLoading {} {
    global show_error_if_no_transition text_editor_arr
	procResetCodeModified false $text_editor_arr(reopening_file)
	procResetFilesMenu
	procEnableSourceCodeEditing
    procShowErrors
	procClearOptionsStateHistory
	procUpdateViewsAfterLoad
}


proc procAddDefaultCSPFile {} {
    global cspstrFilename strFilename curFileTypeOpened
    if {$curFileTypeOpened!= "B" && $curFileTypeOpened!= "Z" && $curFileTypeOpened!= "EVENTB"} {
          tkErrorBox "You can only use a CSP Process to guide B or Z Machines.\nOpen a B Machine or Z Specification first."
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
          tkErrorBox "You can only use a CSP Process to guide B or Z Machines.\nOpen a B Machine or Z Specification first."
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
		procErrorBoxWithErrorMessages "Parsing of CSP-M file failed!"
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
    global strFilename curFileTypeOpened currentRefSpecFile cspstrFilename version text_editor_arr
    if [file exists $strFilename] {
       set curFileTypeOpened "B"
       set currentRefSpecFile ""
       set cspstrFilename ""
       procEnableReopen
       global version

	   prologmnf tcltk_clear_machine
	   # puts "cleared machine"
	   set ext [file extension $strFilename]
       if {$ext == ".prob"} {
           procShowSourceCodeFromCodes .main.frmSource.text {}
       } else {
  	       procShowSourceCode $strFilename
	   }
	     # puts "showed SourceCode"
       procResetCodeModified false $text_editor_arr(reopening_file)
       procResetFilesMenu
         # puts "procEnableSourceCodeEditing"
       procEnableSourceCodeEditing
         # puts "procResetHistory"
       procResetHistory
         # puts "procResetState"
       procResetState "" LightGray
       procResetOptions "Opening..." DarkGray
       my_after {
           # puts "Loading"
           if [prolog tcltk_open_b_file('$strFilename')] {
             set Unicode "true"
	           set ext [file extension $strFilename]
             if {$ext == ".prob" && [prolog bmachine:b_show_machine_representation(Rep,true,false,$Unicode)]} {
                 procShowSourceCodeFromCodes .main.frmSource.text $prolog_variables(Rep)
                 procResetCodeModified false false
             }
             set rootName [file rootname $strFilename]
             procAddNewFileToFilesMenu $strFilename "B"
             procJumpToTheLastSeenTextIndex
             procShowErrors
             procInitLoadedBMachine
             procSetCurrentTimeStampForModel
           } else {
               # Syntax Error or other problem with main machine
               procShowErrors
               procClearOptionsStateHistory
           }
           procUpdateViewsAfterLoad
           procFastForwardIfRequested
           my_after {
               if {!$text_editor_arr(ctext_package)} {
                    procDoOnTheFlySyntaxColouring .main.frmSource.text
                    if [winfo exists .eval] {
                       procDoOnTheFlySyntaxColouring .eval.frmSource.text
                    }
               } else {
                    procDoSyntaxColouring .main.frmSource.text
               }
               # puts "Colored SourceCode"
           }
       }
    } else {
	     tkErrorBox "File $strFilename does not exist."
    }
}

proc my_after {Query} {
  # Warning: Query may not be executed in correct context and should only refer to global variables
  global batch_mode testing_mode my_after_Query
  if {$batch_mode || $testing_mode==true} {
     # do not execute after command to avoid delaying loading the file and starting to execute test before loading completed
     uplevel 1 $Query
  } else {
  set my_after_Query $Query
   after idle {
     after 1 {
       eval $my_after_Query
     }
   }
  }
}

proc procInitLoadedBMachine {} {
	global show_error_if_no_transition
	set show_error_if_no_transition 1
	procSetSpecDesc
	# the next line does not seem to work (at least on mac)
	procResetState "...Initialising..." LightGray
	prologmnf tcltk_initialise
	procShowErrors
	procInsertHistoryOptionsState
	procEnableItemsAfterOpeningFile
}

proc procUpdateViewsAfterLoad {} {
	resetEvaluationView
	procUpdateCBCViewerContents
	procUpdateLTLViewerContents
    # resetAstInspector
    resetTreeInspector ast
    resetTreeInspector cbc_tests
}


proc procRemoveHighlightExecutedOperation {} {
      .main.frmSource.text tag remove executedTag 1.0 end
      .main.frmSource.text tag remove executedauxTag 1.0 end
      .main.frmSource.text tag remove executedControlFlowTag 1.0 end
	  .main.frmSource.text tag configure executedTag -foreground darkgreen
	  .main.frmSource.text tag configure executedTag -background lightblue
	  .main.frmSource.text tag configure executedauxTag -foreground gray40
	  .main.frmSource.text tag configure executedauxTag -background gold
	  .main.frmSource.text tag configure executedControlFlowTag -foreground black
	  .main.frmSource.text tag configure executedControlFlowTag -background chartreuse3
}
proc procHighlightExecutedOperation {line col endline endcol} {
      if {[.main.frmSource.text compare "$endline.$endcol" >= end]} {
          puts "Highlight location $endline.$endcol out of bounds"
          # happens when wrong file is shown, we need location info to be file aware
      } else {
          # puts "Highlight line:$line, col:$col, endline:$endline, endcol:$endcol."
		  set loffset 0
		  set lChr [.main.frmSource.text get "$line.$col+$loffset chars"]
		  set roffset 1
		  set rChr [.main.frmSource.text get "$endline.$endcol - $roffset chars"]
		  # erase leading and trailing whitespace
		  while {$lChr != "" && [string is space $lChr]} {
			 incr loffset
			 set lChr [.main.frmSource.text get "$line.$col + $loffset chars"]
		  }
		  set start "$line.$col + $loffset chars"
		  while {$rChr != "" && [string is space $rChr]} {
			 incr roffset
			 set rChr [.main.frmSource.text get "$endline.$endcol - $roffset chars"]
		  }
		  set roffset [expr $roffset-1]
		  set endl "$endline.$endcol - $roffset chars"
		   # puts "NewleftChar = '$lChr' $start -> $endl '$rChr'"
		  if {($lChr == "\[") && ($endline > $line || $endcol>$col+2)} {
			# is text describing shared channels
			.main.frmSource.text tag add executedauxTag $start $endl
		  } elseif {$endline==$line && $col==$endcol-1 && ($lChr=="\[" || $lChr=="\]" || $lChr=="\|")} {
			# is a single control flow character from external, internal choice or interleave
			.main.frmSource.text tag add executedControlFlowTag $start $endl
		  } else {
			.main.frmSource.text tag add executedTag $start $endl
		  }
		  .main.frmSource.text see "$line.0"
      }
}


proc procHighlightLine {errlinenr} {
	  procHighlightLineIn .main.frmSource.text $errlinenr
}
proc procHighlightLineIn {w errlinenr} {
	  $w tag add errtag $errlinenr.0 $errlinenr.end
	  $w tag configure errtag -foreground darkred
	  $w tag configure errtag -background yellow
}

proc procUnderlineColumn {errlinenr errcolnr} {
	  .main.frmSource.text tag add errcoltag $errlinenr.[expr $errcolnr-1] $errlinenr.$errcolnr
	  .main.frmSource.text tag configure errcoltag -underline true
	  .main.frmSource.text tag configure errcoltag -foreground darkblue
	  .main.frmSource.text tag configure errcoltag -background yellow
}

proc procDeleteErrorTagsInSourceFile {} {
     foreach tag {errtag errcoltag allPatterns hilite \
                  executedTag executedauxTag executedControlFlowTag} {
          eval ".main.frmSource.text tag delete $tag"
     }
}

proc procUnderlineColumnFromTo {errlinenr errcolnr endline endcol} {
   procUnderlineColumnFromToIn .main.frmSource.text $errlinenr $errcolnr $endline $endcol
}
proc procUnderlineColumnFromToIn {w errlinenr errcolnr endline endcol} {
    if {$errlinenr==$endline && $errcolnr == $endcol} {
	    $w tag add errcoltag $errlinenr.$errcolnr $endline.[expr $endcol+1]
	    # still underline it, even though we have a zero-width span
	  } else {
	    $w tag add errcoltag $errlinenr.$errcolnr $endline.$endcol
	  }
	  $w tag configure errcoltag -underline true
	  $w tag configure errcoltag -foreground darkblue
	  $w tag configure errcoltag -background yellow
}


proc procSyntaxColouringPreferences {} {
   procSetPreferences "syntax_highlighting"
   # Missing: check if changes were made !!
   procDoSyntaxColouring .main.frmSource.text
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
	  set pattern {[0-9]*\s*\.\.\s*[0-9]*|(-->)|(-->>?)|(>->>?)|(\+->>?)|(>\+>>?)|<?<->>?|(?=\m)(POW(1?)|SCOPE|(scope_\w*)|(SET_PREF_\w*)|(FORCE_SYMMETRY_\w*)|FIN(1?)|perm|i?seq1?|b?tree|BOOL|struct|REAL|FLOAT|STRING|INT(EGER)?|NAT(URAL)?1?)(?=\M)|(\`[^\`]*\`)}
	  procMarkRegExpression $textWidget $pattern syntax_type
	  # puts "marking syntax_logical"
	  set pattern {&|(?=\m)(not|or|btrue|bfalse|TRUE|FALSE|bool|GOAL|SCOPE|ANIMATION_IMG\w*|ANIMATION_STR\w*|ANIMATION_FUNCTION_DEFAULT|ANIMATION_FUNCTION[0-9]*|ANIMATION_RIGHT_CLICK|ANIMATION_CLICK|HEURISTIC_FUNCTION|VISB_JSON_FILE|VISB_SVG_BOX|VISB_SVG_CONTENTS\w*|VISB_SVG_OBJECTS\w*|VISB_SVG_HOVERS\w*|VISB_SVG_UPDATES\w*|CUSTOM_GRAPH_EDGES|CUSTOM_GRAPH_NODES|ASSERT_(LTL|CTL)\w*)(?=\M)|<?=>|!|#}
	  procMarkRegExpression $textWidget $pattern syntax_logical

	  # puts "marking syntax_assignment"
	  set pattern {STRING_(APPEND|CONC|LENGTH|SPLIT|JOIN|CHARS|CODES|TO_INT|IS_INT|TO_ENUM)|<--|:=|==|\|\||::|(\"[^\"]*\")}
	  procMarkRegExpression $textWidget $pattern syntax_assignment
	  # puts "marking syntax_operator 1"
	  # note: maplet |-> is not matched to avoid performance problem with large relations in properties,...
	  set pattern {\|->|>=|<=(?!>)|%|\*\*|~|/?<<?:|/(:|=)|<\+|><|^|<-(?!-|>)|<<?\||\|>>?|{}|{|}|\[|\]|\|\[\]|<>|^|(/\|?\\)|(\\\|?/)|\'|(?=\m)(SIGMA|PI|MININT|MAXINT|pred|succ|id|INTER|UNION|card|dom|ran|max|min|union|inter|size|mod|fnc|rel|rev|conc|front|tail|first|last|rec|closure1|closure|iterate|prj(1|2)|top|rank|father|son(s)?|const|left|right|bin|subtree|arity|sizet|prefix|postfix|real|floor|ceiling)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_operator
	  # puts "marking syntax_keyword"
	  set pattern {MACHINE(\s*\w\w*)?|END\s*\Z|(?=\m)(RULES_MACHINE|RULE_FORALL|COMPUTATION|RULE?(ID)|MACHINE|OPERATIONS|LOCAL_OPERATIONS|EVENTS|ASSERTIONS|INITIALI(S|Z)ATION|SEES|PROMOTES|USES|INCLUDES|IMPORTS|REFINES|EXTENDS|REFINEMENT|SYSTEM|MODEL|IMPLEMENTATION|INVARIANT|CONCRETE_VARIABLES|ABSTRACT_VARIABLES|VARIABLES|FREETYPES|PROPERTIES|CONSTANTS|ABSTRACT_CONSTANTS|CONCRETE_CONSTANTS|CONSTRAINTS|SETS|DEFINITIONS|VALUES|ref)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_keyword
	  # puts "marking syntax_control_keyword"
	  set pattern {(?=\m)(skip|LET|BE|VAR|IN|ANY|WHILE|DO|VARIANT|ELSIF|IF|THEN|ELSE|EITHER|CASE|SELECT|ASSERT|WHEN|PRE|BEGIN|END|CHOICE|WHERE|OR|OF)(?=\M)|;}
	  procMarkRegExpression $textWidget $pattern syntax_control_keyword

	  #puts "marking syntax_unsupported"
	  set pattern {invariant_violated|\#\#\#\#|(?=\m)(both_true_false|wd_error(_false|_true)?|timeout(_false|_true)?|infix|mirror)(?=\M)}
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

	  set pattern {(?=\m)(start|trans|prop|heuristic_function_result|heuristic_function_active|animation_(function_result|image|image_right_click_transition|image_click_transition))(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_keyword

	  set pattern {(?=\m)(true|fail|atomic|nonvar|var|functor|op|is|ground|member|append|length)(?=\M)|=}
	  procMarkRegExpression $textWidget $pattern syntax_type

	  set pattern {:-|!|-->|;|\.}
	  procMarkRegExpression $textWidget $pattern syntax_assignment

	  set pattern {%(.*)}
	  procMarkRegExpression $textWidget $pattern syntax_comment
  }
}


proc procDoZedSyntaxColouring {textWidget} {
  procUpdateSyntaxColours $textWidget
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {

	  procMarkRegExpressionPair $textWidget {/\*} {\*/} syntax_comment

	  set pattern {\\(\\|noindent|(sub)?section|begin{document}|end{document}|usepackage{(fuzz|z-eves)}|documentclass)}
	  procMarkRegExpression $textWidget $pattern syntax_comment

	  set pattern {(\\(begin{schema}|end{schema}|begin{(zed|axdef)}|end{(zed|axdef)}))|\\(?=\m)(where|also|Delta)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_keyword

	  set pattern {\\{|\\}|(head|tail|last|front|squash|rev|min|max|first|second|succ|count|items|\\(notin|in|inbag|(big)?cup|(big)?cap|subset|subseteq|subbageq|disjoint|partition|plus|oplus|uplus|uminus|otimes|setminus|times|emptyset|leq|geq|neq|div|mod|dom|(n)?(d|r)res|langle|rangle|lbag|rbag|ran|id|inv|mapsto|succ|cat|dcat|prefix|suffix|inseq|filter|extract|bcount|\#))(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_operator

	  set pattern {\\(power(_1)?|nat(_1)?|num|bag|cross|upto|rel|(p)?fun|(p)?inj|bij|seq(_1)?|iseq(_1)?|(b)?tree)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_type

	  set pattern {\\(land|lor|implies|iff|lnot|forall|exists(_1)?|mu|lambda|true|false)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_logical

	  set pattern {(::)?=|==|\\(IF|THEN|ELSE|LET|defs)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_assignment

	   set pattern  {\\(infix|arithmos)(?=\M)}
	   procMarkRegExpression $textWidget $pattern syntax_unsupported
  }
}

proc procDoOnTheFlyCSPSyntaxColouring {textWidget args} {
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {

          ctext::clearHighlightClasses $textWidget

          prolog preferences:get_preference(sh_top_level_keywords,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_keyword $col {if then else let within}
          ctext::addHighlightClassForRegexp $textWidget syntax_keyword $col {(\{|\}|\{\||\|\}|\[|\]|\[\||\|\]|\\|@@|<-(>)?)}

          prolog preferences:get_preference(sh_operators,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_operator $col {channel datatype subtype nametype MAIN Events}
          prolog preferences:get_preference(sh_type_colour,Colour)

	  set col $prolog_variables(Colour)
          ctext::addHighlightClassForRegexp $textWidget syntax_type_words $col {(?=\m)(STOP|SKIP|CHAOS)(?=\M)}
          ctext::addHighlightClassForRegexp $textWidget syntax_type $col {(\[\]|\|~\||\|\|\||/\\|\[>|\!|\?|->|@)}

          prolog preferences:get_preference(sh_logical_colour,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClassForRegexp $textWidget syntax_logical $col {(?=\m)(true|false|length|null|head|tail|concat|set|Set|seq|Seq|elem|empty|card|member|union|diff|inter|Union|Inter|not|and|or|Int|Bool)(?=\M)}
          ctext::addHighlightClassForRegexp $textWidget syntax_logical1 $col {(\*|\+|-|\%|>|\|\||<|(=|!|<|>)=|=<|&&)}

          prolog preferences:get_preference(sh_assignments_colour,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_assignment $col {assert transparent diamond print include}

          prolog preferences:get_preference(sh_unsupported_background,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_unsupported SlateBlue1 {external Proc}
          ctext::addHighlightClassForRegexp $textWidget syntax_set #f83cff {([0-9]*\s*\.\.\s*[0-9]*)}

          ctext::addHighlightClassForRegexp $textWidget assertion_operators #1e90ff {\[[FDTR]+=}
          ctext::addHighlightClassForRegexp $textWidget assertion_operators1 #1e90ff {\[[FD]+\]}

          prolog preferences:get_preference(sh_comments,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClassForRegexp $textWidget syntax_comments $col {--[^\n\r]*}
          ctext::addHighlightClassForRegexp $textWidget multiline_comment $col {\{-(.|[\n\r]*)*-\}}
          if [regexp {[0-9]+\.[0-9]+} [lindex $args 0] match] {
              procHighlightTextSpecial $textWidget $match end
          } else {
              procHighlightTextSpecial $textWidget 1.0 end
          }
   }
}

proc procDoOnTheFlySyntaxColouring {textWidget} {
   if [prolog preferences:get_preference(do_syntax_highlighting,true)] {

          ctext::clearHighlightClasses $textWidget

          prolog preferences:get_preference(sh_type_colour,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClassForRegexp $textWidget syntax_type $col {([0-9]*\s*\.\.\s*[0-9]*|\u2203|\u2200|\u21A6|\u2192|\u2194|\u21F8|\u2119|\u2081|\u2124|\u2115|-->|-->(>)?|>->(>)?|\+->(>)?|>\+>(>)?|<?<->>?|\
                         scope_\w*|SET_PREF_\w*|FORCE_SYMMETRY_\w*)|((?=\m)(FIN(1?)|POW(1?)|perm|i?seq1?|b?tree|BOOL|struct|REAL|STRING|INT(EGER)?|NAT(URAL)?1?)(?=\M)|(\`[^\`]*\`))}
          #ctext::addHighlightClass $textWidget syntax_type_words $col {POW POW1 FIN FIN1 perm seq iseq seq1 iseq1 BOOL STRING INT INTEGER NAT NAT1 NATURAL NATURAL1 REAL FLOAT}

          prolog preferences:get_preference(sh_logical_colour,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClassForRegexp $textWidget syntax_logical1 $col {(<?=>|((?=\m)(TRUE|FALSE|btrue|bfalse|not|or|bool|GOAL|SCOPE)(?=\M))|&|\u2227|\u2228|\u22A4|\u22A5|\u21D4|\u21D2|HEURISTIC_FUNCTION|VISB_JSON_FILE|VISB_SVG_BOX|VISB_SVG_CONTENTS\w*|VISB_SVG_OBJECTS\w*|VISB_SVG_HOVERS\w*|VISB_SVG_UPDATES\w*|ANIMATION_FUNCTION_DEFAULT|ANIMATION_FUNCTION[0-9]*|ANIMATION_IMG\w*|ANIMATION_STR\w*|ANIMATION_RIGHT_CLICK|ANIMATION_CLICK|CUSTOM_GRAPH_EDGES[0-9]*|CUSTOM_GRAPH_NODES[0-9]*|ASSERT_(LTL|CTL)\w*|!|\#)}

          prolog preferences:get_preference(sh_operators,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_operator1 $col {SIGMA PI MININT MAXINT pred succ \
                         id INTER UNION card dom ran max\
                         min union inter size mod fnc rel rev conc front tail first last rec closure1 closure iterate prj1 prj2 top rank father son const sons left right bin subtree arity sizet prefix postfix real floor ceiling}
          ctext::addHighlightClassForRegexp $textWidget syntax_operator $col {(\u2208|\u2209|\u2260|\u2264|\u2265|\u2282|\u2284|\u2286|\u2287|\u2229|\u222A|\\|\|->|>=|<=|%|\*\*|~|(//)?<(<)?:|//(:|=)|<\+|\^|<-(>)?|<(<)?(\||:)|\
                         \|>(>)?|\{|\}|\[\]|\|\[\]|<>|/(\|)?\\|\\(\|)?/)}

          prolog preferences:get_preference(sh_assignments_colour,Colour)
	  set col $prolog_variables(Colour)
	  ctext::addHighlightClassForRegexp $textWidget syntax_assignment $col {(STRING_(APPEND|CONC|LENGTH|SPLIT|JOIN|CHARS|CODES|TO_INT|IS_INT|TO_ENUM|TO_LOWER|TO_UPPER|IS_DECIMAL|IS_NUMBER|IS_ALPHANUMERIC|PADLEFT|EQUAL_CASE_INSENSITIVE|REV)|CHOOSE|MU|STRINGIFY|DEC_STRING_TO_INT|INT_TO_(HEX|DEC)_STRING|SUB_STRING|STRING_CONTAINS_STRING|STRING_REPLACE|CODES_TO_STRING|SQUASH|SORT|(SHA_)?HASH|READ_XML|WRITE_XML|IS_REGEX|(REGEX_((I)?MATCH|(I)?SEARCH(_ALL|_STR)?|(I)?REPLACE))|(INT_|FORMAT_)?TO_STRING|RANDOM|GCD|MSB|ABS|CDIV|FDIV|SQRT|FACTORIAL|SIN(x)?|COS(x)?|TAN(x)?|LOGx|RADD|RAND|R(A)?SIN(H)?|R(A)?COS(H)?|R(A)?COT(H)?|R(A)?TAN(H)?|RDIV|REXP|RLOGe|RMUL|RSQRT|RSUB|RABS|ROUND|RSIGN|REULER|RFRACTION|RINTEGER|RMIN|RMAX|RONE|RPI|RPOW|RLOG|RPI|RNORMAL|RZERO|(f)?printf|(F)?PRINT(F)?|observe|<--|:=|==|\|\||::|(\"[^\"]*\"))}

          prolog preferences:get_preference(sh_top_level_keywords,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_keyword $col {MACHINE OPERATIONS LOCAL_OPERATIONS EVENTS ASSERTIONS INITIALIZATION INITIALISATION SEES PROMOTES\
                        USES INCLUDES IMPORTS REFINES EXTENDS REFINEMENT SYSTEM MODEL IMPLEMENTATION INVARIANT\
                        CONCRETE_VARIABLES ABSTRACT_VARIABLES VARIABLES PROPERTIES CONSTANTS ABSTRACT_CONSTANTS CONCRETE_CONSTANTS\
                        CONSTRAINTS SETS DEFINITIONS VALUES FREETYPES\
                        RULE_FORALL RULES_MACHINE DEPENDS_ON_COMPUTATION DEPENDS_ON_RULE COUNTEREXAMPLE\
                        COMPUTATION RULEID RULE DEFINE STRING_FORMAT EXPECT BODY TYPE VALUE ref}
          #ctext::addHighlightClassForRegexp $textWidget syntax_keyword_Machine $col {(MACHINE(\s*\w\w*)?|END\s*\Z)}

          prolog preferences:get_preference(sh_control_keywords,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_control_keyword $col {skip LET BE VAR IN ANY WHILE DO VARIANT ELSIF IF THEN ELSE\
                        EITHER CASE SELECT ASSERT WHEN PRE BEGIN END CHOICE WHERE OR OF}
          ###\' throws exception for this char

          prolog preferences:get_preference(sh_unsupported_background,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_unsupported1 red {invariant_violated both_true_false infix \
                         mirror}
          ctext::addHighlightClassForRegexp $textWidget syntax_unsupported red {(wd_error(_false|_true)?|timeout(_false|_true)?|\\(=|:|<:|<<:)|\#\#\#\#)}
          $textWidget tag configure syntax_unsupported -background $col
          $textWidget tag configure syntax_unsupported1 -background $col

          prolog preferences:get_preference(sh_comments,Colour)
	        set col $prolog_variables(Colour)
          ctext::addHighlightClassForRegexp $textWidget syntax_comments $col {//[^\n\r]*}
          ctext::addHighlightClassForRegexp $textWidget multiline_comment $col {/\*(.|[\n\r]*)*\*/}


          prolog preferences:get_preference(sh_pragmas,Colour)
	        set col $prolog_variables(Colour)
          ctext::addHighlightClassForRegexp $textWidget multiline_pragma_color $col {/\*@(label|desc|symbolic)(.|[\n\r]*)*\*/}

          procHighlightTextSpecial $textWidget 1.0 end
   }
}

proc procHighlightTextSpecial {editor start end} {
	  global text_editor_arr
    regexp {([0-9]+)\.[0-9]+} [$editor index "$start"] match firstline
    regexp {([0-9]+)\.[0-9]+} [$editor index "$end"] match lastline
    if {$lastline > 20000} {
       #puts "File very large: stopping syntax highlighting at line 20000 instead of $lastline"
       prolog "tools:print_message('Stopping syntax-highlighting at 20,000 lines')"
       set lastline 20000
    }
    set currentline $firstline
	  if {!$text_editor_arr(disable_highlighting)} {
	          while {$currentline <= $lastline} {
       		       if {[string length [$editor get $currentline.0 $currentline.end]] > 500} {
        	           $editor highlight $currentline.0 $currentline.500
        	      } else {
        	           $editor highlight $currentline.0 $currentline.end
        	      }
        	      incr currentline
        	  }
	  }
}

proc procDoOnTheFlyZedSyntaxColouring {textWidget} {
   if [prolog preferences:get_preference(do_syntax_highlighting,true)] {
        ctext::clearHighlightClasses $textWidget

        prolog preferences:get_preference(sh_operators,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClassForRegexp $textWidget syntax_operator $col {\\{|\\}|(head|tail|last|front|squash|rev|min|max|first|second|succ|count|items|\\(notin|in|inbag|(big)?cup|(big)?cap|subset|subseteq|subbageq|disjoint|partition|plus|oplus|uplus|uminus|otimes|setminus|emptyset|leq|geq|neq|div|mod|dom|(n)?(d|r)res|langle|rangle|lbag|rbag|ran|id|inv|mapsto|succ|cat|dcat|prefix|suffix|inseq|filter|extract|bcount|\#))(?=\M)}

        prolog preferences:get_preference(sh_comments,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClassForRegexp $textWidget syntax_comment $col {\\(noindent|documentclass)}
        ctext::addHighlightClassForRegexp $textWidget syntax_comment1 $col {(\\((begin|end)\{(document)\}|(sub)?section|(usepackage)\{(fuzz|z-eves)\}|\\))}

        prolog preferences:get_preference(sh_top_level_keywords,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClassForRegexp $textWidget syntax_keywords $col {(\\(where|also|Delta))}
        ctext::addHighlightClassForRegexp $textWidget syntax_keywords1 $col {(\\(begin|end)\{(schema|zed|axdef)\})}

        prolog preferences:get_preference(sh_type_colour,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClassForRegexp $textWidget syntax_type $col {(\\(power(_1)?|nat(_1)?|num|bag|cross|upto|rel|(p)?fun|(p)?inj|bij|seq(_1)?|iseq(_1)?|(b)?tree))}

        prolog preferences:get_preference(sh_logical_colour,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClassForRegexp $textWidget syntax_logical $col {(\\(land|lor|implies|iff|lnot|forall|exists(_1)?|mu|lambda|true|false))}

        prolog preferences:get_preference(sh_assignments_colour,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClassForRegexp $textWidget syntax_assignment $col {((::)?=)|==|\\(IF|THEN|ELSE|LET|defs)(?=\M)}

        prolog preferences:get_preference(sh_comments,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClassForRegexp $textWidget syntax_comment1 $col {%[^\n\r]*}
        ctext::addHighlightClass $textWidget multiline_comment $col {/\*(.|[\n\r]*)*?\*/}

        procHighlightTextSpecial $textWidget 1.0 end
   }
}

proc procDoOnTheFlyXTLSyntaxColouring {textWidget} {
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {
        ctext::clearHighlightClasses $textWidget

        prolog preferences:get_preference(sh_type_colour,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClass $textWidget syntax_type $col {true fail atomic atom compound nonvar var functor arg op is ground number copy_term dif member memberchk nonmember keysort term_variables append reverse last delete select selectchk maplist length nth nth1 nth0 perm perm2 permutation same_length add_error print write sort}

        prolog preferences:get_preference(sh_top_level_keywords,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClass $textWidget syntax_keywords $col {start trans prop heuristic_function_result heuristic_function_active animation_image animation_function_result animation_image_right_click_transition animation_image_click_transition prob_pragma_string}

        prolog preferences:get_preference(sh_assignments_colour,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClassForRegexp $textWidget syntax_assignment $col {(:-|!|-->|;|\.)}

        prolog preferences:get_preference(sh_comments,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClassForRegexp $textWidget syntax_comment1 $col {%[^\n\r]*}
        ctext::addHighlightClassForRegexp $textWidget multiline_comment $col {/\*(.|[\n\r]*)*\*/}

        procHighlightTextSpecial $textWidget 1.0 end
  }
}


proc procDoOnTheFlyAlloySyntaxColouring {textWidget} {
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {
        ctext::clearHighlightClasses $textWidget

        prolog preferences:get_preference(sh_type_colour,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClass $textWidget syntax_type $col {not one lone set no all some disjoint disj let in for and or implies iff else none univ iden Int int => && <=> \|\| ! \. \^ \* <: :> ++ \~ -> & + - = \#}

        prolog preferences:get_preference(sh_top_level_keywords,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClass $textWidget syntax_keywords $col {module sig fact extends run abstract open fun pred check assert plus minus mul div rem sum}

        prolog preferences:get_preference(sh_comments,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClassForRegexp $textWidget syntax_comment1 $col {//[^\n\r]*}
        ctext::addHighlightClassForRegexp $textWidget multiline_comment $col {/\*(.|[\n\r]*)*\*/}

        procHighlightTextSpecial $textWidget 1.0 end
  }
}


proc procDoOnTheFlyEventBSyntaxColouring {textWidget} {
   if [prolog preferences:get_preference(do_syntax_highlighting,true)] {
          ctext::clearHighlightClasses $textWidget

          prolog preferences:get_preference(sh_top_level_keywords,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_keywords $col {event_b_model event_b_context sees variables invariant refines \
                          theorems events extends constants axioms sets deferred_set event}

          prolog preferences:get_preference(sh_type_colour,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_type $col {integer natural1_set natural_set couple}

          prolog preferences:get_preference(sh_operators,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_operator $col {subset member not_member set_subtraction set_extension range \
                          domain card add multiplication div interval boolean_true boolean_false}

          prolog preferences:get_preference(sh_logical_colour,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_logical $col {conjunct less_equal greater equal not_equal greater_equal less}

          prolog preferences:get_preference(sh_assignments_colour,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_assignment $col {assign becomes_element_of}

          prolog preferences:get_preference(sh_comments,Colour)
	  set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_comment $col {none identifier}

        procHighlightTextSpecial $textWidget 1.0 end
   }
}

proc procDoOnTheFlyTLASyntaxColouring {textWidget} {
   if [prolog preferences:get_preference(do_syntax_highlighting,true)] {
          ctext::clearHighlightClasses $textWidget

          prolog preferences:get_preference(sh_operators,Colour)
	        set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_operator1 $col {Head SubSeq Append Len Seq Tail IsFiniteSet Cardinality Int Nat}
          ctext::addHighlightClassForRegexp $textWidget syntax_operator $col {(\+|=|-|\*|\^|/|\.\.|\\(o|circ|div|leq|geq)|%|(<|>|/)?=)}

          prolog preferences:get_preference(sh_logical_colour,Colour)
	        set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_logical $col {TRUE FALSE SubSeq Append Len Seq Head Tail Cardinality IsFiniteSet}
          ctext::addHighlightClassForRegexp $textWidget syntax_logical1 $col {((<)?=>|<<|>>|!|\#|/=|\~|(<|-)>|\[\]|\|->|\\|/\\|\\/|~\|\.|\"
                         |\\(land|lor|lnot|neg|equiv|E|A|in|notin|cap|intersect|cup|subseteq|subset|times))}

          prolog preferences:get_preference(sh_top_level_keywords,Colour)
	        set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_keyword $col {MODULE CONSTANTS CONSTANT ASSUME ASSUMPTION VARIABLE \
                         VARIABLES AXIOM THEOREM EXTENDS INSTANCE LOCAL}
          ctext::addHighlightClassForRegexp $textWidget syntax_keyword_Machine $col {(==)}

          prolog preferences:get_preference(sh_pragmas,Colour)
	        set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_type $col {Next Init Spec Inv}

          prolog preferences:get_preference(sh_control_keywords,Colour)
	        set col $prolog_variables(Colour)
          ctext::addHighlightClass $textWidget syntax_control_keyword $col {IF THEN ELSE UNION CHOOSE LET IN UNCHANGED SUBSET\
                         CASE DOMAIN  EXCEPT ENABLED SF_ WF_ WITH OTHER BOOLEAN STRING}

          prolog preferences:get_preference(sh_assignments_colour,Colour)
	        set col $prolog_variables(Colour)
          ctext::addHighlightClassForRegexp $textWidget syntax_assignment $col {SCOPEANIMATION_FUNCTION[0-9]*|SET_PREF_\w*|SCOPE}

          prolog preferences:get_preference(sh_comments,Colour)
	        set col $prolog_variables(Colour)
          ctext::addHighlightClassForRegexp $textWidget multiline_comment $col {\(\*(.|[\n\r]*)*\*\)}
          ctext::addHighlightClassForRegexp $textWidget syntax_comment1 $col {\\\*[^\n\r]*}

        procHighlightTextSpecial $textWidget 1.0 end
   }
}

proc procDoTLASyntaxColouring {textWidget} {
   procUpdateSyntaxColours $textWidget
   if [prolog preferences:get_preference(do_syntax_highlighting,true)] {
          procMarkRegExpressionPair $textWidget {\(\*} {\*\)} syntax_comment

	  set pattern {((?=\m)(Head|SubSeq|Append|Len|Seq|Tail|IsFiniteSet|Cardinality|Int|Nat)(?=\M))\
                                                     |(\+|\*|\^|\.\.|\\(o|circ|div|leq|geq)|%)}
	  procMarkRegExpression $textWidget $pattern syntax_operator

	  set pattern {((<)?=>|<<|>>|!|\#|/=|\~|(<|-)>|\[\]|\|->|\\|/\\|\\/|~\|\.|\"
                         |\\(land|lor|lnot|neg|equiv|E|A|in|notin|cap|intersect|cup|subseteq|subset|times|union)|TRUE|FALSE|SubSeq|Append|Len|Seq|Head|Tail|Cardinality|IsFiniteSet)}
	  procMarkRegExpression $textWidget $pattern syntax_logical

	  set pattern {((?=\m)(MODULE|CONSTANTS|CONSTANT|ASSUME|ASSUMPTION|VARIABLE|VARIABLES|AXIOM|THEOREM|EXTENDS|INSTANCE|LOCAL)(?=\M))|==}
	  procMarkRegExpression $textWidget $pattern syntax_keyword

	  set pattern {((?=\m)(IF|THEN|ELSE|UNION|CHOOSE|LET|IN|UNCHANGED|SUBSET|CASE|DOMAIN|EXCEPT|ENABLED|SF_|WF_|WITH|OTHER|BOOLEAN|STRING)(?=\M))}
	  procMarkRegExpression $textWidget $pattern syntax_control_keyword

	  set pattern {ANIMATION_FUNCTION[0-9]*|SET_PREF_\w*|SCOPE}
	  procMarkRegExpression $textWidget $pattern syntax_assignment
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

	  set pattern {(?=\m)(external|extensions|productions|Proc)(?=\M)}
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
	  $textWidget tag configure syntax_operator1 -foreground $col
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
	  $textWidget tag configure syntax_operator1 -foreground black
	  $textWidget tag configure syntax_keyword -foreground black
	  $textWidget tag configure syntax_control_keyword -foreground black
	  $textWidget tag configure syntax_comment -foreground black
	  $textWidget tag configure syntax_unsupported -background blue
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
        puts "Unclosed comment ($textWidget, $begregpattern, $endregpattern) !"
        # can happen in truncated symbolic values in various views
        $textWidget tag add $tag_to_mark $begstart end
        set begstart ""
      }
    } else {
      set begstart ""
    }
  }
}

proc execInBZMode {Cmd} {
   if ![prolog spec_file_has_been_successfully_loaded] {
    tkErrorBoxNoParent "This command is only possible for successfully loaded B, Z, Alloy or TLA+ specifications ($Cmd)."
   } elseif [prolog b_or_z_mode] {
	   eval $Cmd
   } else {
       tkErrorBoxNoParent "This command is only possible for B, Z, Alloy or TLA+ specifications ($Cmd)."
   }
}
# --------------------------------
# SAVING TO FILE
# --------------------------------

proc procSaveValuesToFile {} {
   global strFilename curFileTypeOpened
   if [prolog b_or_z_mode] {
		set types {
			{"Text Files"	{.txt}	}
			{"All files"		*}
		}
		set machinesPath [getMachinesPath]
	  set rn [file rootname [file tail $strFilename]]
		set valFile [tk_getSaveFile -filetypes $types -initialdir $machinesPath \
		    -initialfile "${rn}_values" -parent . -defaultextension ".txt"]
		if {$valFile != ""} {
	        prolog "b_show_history:write_values_to_file('$valFile')"
	    }
   } else {
       tkErrorBoxNoParent "Values can only be saved for B specifications."
   }
}

proc procSaveAsFile {} {
    global strFilename curFileDisplayed
    if {$curFileDisplayed != $strFilename} {
	      tkErrorBox "Can only save main file $strFilename."
    } else {
		##procCheckIfSaved
		set types {
			{"B Machine Files"		{.mch}	}
			{"B Refinements"		{.ref}	}
			{"B Implementations"	{.imp}	}
    	{"Event-B System Files"	{.sys}	}
    	{"B Rules Machine"	    {.rmch}	}
    	{"B Definition Files"	    {.def}	}
			{"All B Files"	{.mch .ref .imp .sys .rmch .def}	}
      {"TLA Files" {.tla}      }
			{"All files"		*}
		}
		# show the dialog box
		set oldstrFilename $strFilename
		proc_getSaveFile $types
		if {$strFilename != ""} {
            set wasModified [.main.frmSource.text edit modified]
            prolog preferences:add_recent_document('$strFilename')
			set curFileDisplayed $strFilename
			procSaveFile
			procEnableItemsAfterOpeningFile
            procRebuildRecentDocumentsMenu
			if {$wasModified} {
			   procReOpenFile
			}
		} else {
			set strFilename $oldstrFilename
		}
    }
}
proc procSaveFile {} {
     # not yet fully tested !
    global strFilename curFileTypeOpened text_editor_arr
    if {$curFileTypeOpened == "EVENTB"} {
       tkErrorBox "EventB Packaged can not be saved & edited from within ProB."
    } elseif {$strFilename != ""} {
		global curFileDisplayed
		if {$curFileDisplayed != $strFilename} {
		  # puts "Saving $curFileDisplayed (main file: $strFilename)"
		}
                if {$text_editor_arr(deactivate_asking_for_reloading)} {
                    set ans [tk_messageBox -default yes -message "The file $strFilename has been modified since reading it.\
                             \nIf you save it, all external changes will be lost. Save it anyway?" -title "Warning"\
                             -type yesno -icon warning -parent .]
                    if {$ans == "yes"} {
                        procSaveTextContentIntoFile $curFileDisplayed
                        set text_editor_arr(deactivate_asking_for_reloading) false
                    }
                } else {
                    procSaveTextContentIntoFile $curFileDisplayed
                }
    } else {
       tkErrorBox "No file open. Cannot save."
    }
    # insert a separator on the undo stack
    # .main.frmSource.text edit separator
}

proc procSaveTextContentIntoFile {file} {
	global text_editor_arr
	set modified [.main.frmSource.text edit modified]
	set text [.main.frmSource.text get 1.0 "end - 1 chars"]
	#output to file here instead
	set fid [prob_open $file]
	puts -nonewline  $fid $text
	# seems to add extra blank lines at the end ?!
	close $fid
	procResetCodeModified true true
	if {$text_editor_arr(current_timestamp) != "" && $modified &&
            $text_editor_arr(last_timestamped_file) == $file && $text_editor_arr(current_timestamp) != [file mtime $file]} {
		.main.frmSource.statusframe.modelstatus configure -text "File content was changed." -bg Gray80
	}
}

proc procResetCodeModified {saving reopen} {
    global strFilename text_editor_arr
    .frmMenu.mnuFile entryconfigure 3 -state disabled
    .frmMenu.mnuFile entryconfigure 4 -state disabled
    .main.frmSource.text edit modified 0
    if {$saving} {
      proc_setFileNotModified
    } elseif {!$reopen} {
      procresetundo
    }
    if {$strFilename != "" && [file exists $strFilename]} {
        set text_editor_arr(file_last_modified) [file mtime $strFilename]
    }
}

proc procSetCodeModified {} {
    .frmMenu.mnuFile entryconfigure 3 -state normal
    .frmMenu.mnuFile entryconfigure 4 -state normal
    .main.frmSource.text edit modified 1
}
proc procCheckIfSaved {} {
    global curFileDisplayed curFileTypeOpened
    set bModified [.main.frmSource.text edit modified]
    if { $bModified && $curFileTypeOpened != "EVENTB"} {
	   set ans [tk_messageBox -default yes -message "Source File $curFileDisplayed is not saved.  Save changes to disk?" \
	            -title "Warning" -type yesno -icon warning]
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
    procReOpenFile1 0
}
proc procSaveReOpenFileAndFastForward {} {
    procSaveFile
    procReOpenFile1 1
}
proc procReOpenFile1 {FastForward} {
    global strFilename curFileTypeOpened find_menu_arr tcl_version text_editor_arr fastForwardAfterLoad
    set fastForwardAfterLoad $FastForward

    set ext [file extension $strFilename]
    if {$strFilename != ""} {
        procCheckIfSaved
        set find_menu_arr(allpatterns) false
		if [file exists $strFilename] {
                        set text_editor_arr(reopening_file) true
			#.main.frmSource.text configure -undo 0
			prolog tcltk_prepare_for_reload
			if {$curFileTypeOpened == "B"} {
			   procLoadBFile
			} elseif {$curFileTypeOpened == "Z"} {
			   procLoadZFile
			} elseif {$curFileTypeOpened == "XTL"} {
			   procLoadXTLFile
			} elseif {$curFileTypeOpened == "ALLOY"} {
			   procLoadAlloyFile
			} elseif {$curFileTypeOpened == "CSP"} {
			   procLoadCSPMFile
			} elseif {$curFileTypeOpened == "EVENTB"} {
			   procLoadEventBPackage
            } elseif {$curFileTypeOpened == "TLA"} {
               procLoadTLAFile
            } else {
               set FastForward 0
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

proc procJumpToTheLastSeenTextIndex {} {
    global text_editor_arr tcl_version
    if {$tcl_version >= 8.5} {
        tk::TextSetCursor .main.frmSource.text $text_editor_arr(text_index)
    }
    if {[regexp {([0-9]+)\.[0-9]+} $text_editor_arr(text_index) all line] && $text_editor_arr(highlight_line)} {
         procHighlightCurrentLine $line
    }
    .main.frmSource.text see $text_editor_arr(text_index)
}

# -------
# procedure to reset the animation
# -------
proc procReset {} {
	    prologmnf tcltk_reset
      procInsertHistoryOptionsState
}
# fast forward history
proc procFastForward {} {
	  prologmnf tcltk_fast_forward
      procInsertHistoryOptionsState
}
global fastForwardAfterLoad
set fastForwardAfterLoad 0
proc procFastForwardIfRequested {} {
      global fastForwardAfterLoad
      if {$fastForwardAfterLoad} {
        if [prolog spec_file_has_been_successfully_loaded] {
           procFastForward
        } else {
           puts "No fastforward possible, error while reopening file"
        }
   }
}

# ----------------------------------
# procedure to do a random animation
# ----------------------------------
proc procMinimalUpdateHistory {} {
  # minimal update required after random animation step
       procInsertHistory
       prolog tcltk_get_options(Options)
       # faster than tcltk_get_options_or_candidates in case of MAX_OPERATIONS 0
       procResetOptions "Random..." DarkGray
	   procResetState "" LightGray
}
proc procRand {} {
       procRandSteps 10 0 "fully_random"
}
proc procRandSteps {Steps Update Kind} {
    # Kind can be fully_random, explore_open, or no_self_loops
    procResetOptions "Random..." DarkGray
	  procResetState "" LightGray
    for {set i 0} {$i < $Steps} {incr i} {
     if [prolog tcltk_random_perform($Kind)] {
       if {$Update} {
         # also updates history; does not trigger update of Graphical Animation; so not very useful
         procMinimalUpdateHistory
       } else {
         prolog tcltk_get_options_or_candidates(Options)
       }
     } else {set i $Steps}   }
       procInsertHistoryOptionsState
}
# ----------------------------------
global random_step_kind
set random_step_kind "fully_random"

proc procRandAny {} {
	global prompt Depth count done
	destroy .search
	set f .search
	if [Dialog_Create $f "Random Animation:" -borderwidth 10] {
	  procResetPromptToNumber
		message $f.msg -text "Enter maximum nr of animation steps: " -aspect 1000
		entry $f.entry -textvariable prompt(result)
		set b [frame $f.buttons]
		pack $f.msg $f.entry $f.buttons -side top -fill x
		pack $f.entry -pady 5
        global random_step_kind
        set random_step_kind
		# checkbutton $b.checkbutton1 -text "Try Explore New States" -variable random_step_kind -onvalue "explore_open" -offvalue "fully_random"
		radiobutton $b.radiok1 -text "Random Exploration" -variable random_step_kind -value "fully_random"
		radiobutton $b.radiok2 -text "Try Explore New States" -variable random_step_kind -value "explore_open"
		radiobutton $b.radiok3 -text "Minimise Out Degree" -variable random_step_kind -value "min_out_degree"
		if [prolog state_space_exploration_modes:heuristic_function_active] {
            radiobutton $b.radiok4 -text "Minimise HEURISTIC_FUNCTION" -variable random_step_kind -value "heuristic"
        } else {
            radiobutton $b.radiok4 -text "Minimise HEURISTIC_FUNCTION" -variable random_step_kind -value "heuristic" -state disabled
            if {$random_step_kind == "heuristic"} { set random_step_kind "fully_random"}
        }
            pack $b.radiok1 $b.radiok2 $b.radiok3 $b.radiok4 -fill x -fill y
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
    global random_step_kind
	global prompt
	global count Depth done

    if {($prompt(ok)) && ($count < $Depth)} {
      .search.msg2 configure -text "Random... $count / $Depth"
      set count [expr $count+1]
      # puts $count

         if [prolog tcltk_random_perform($random_step_kind)] {
              #procInsertHistoryOptionsState
              procMinimalUpdateHistory
           if [prolog tcltk_current_state_invariant_violated] {
              procInsertHistoryOptionsState
              tkErrorBox "Invariant Violation found!"
              set done 1
           } else {
              after 1 random_step
           }
          } else {
          set count $Depth
          if [prolog tcltk_get_status(INVVIOLATED,MAXREACHED,1)] {
             procInsertHistoryOptionsState
             tkErrorBox "Possible Deadlock found (timeout occurred)."
          } else {
             procInsertHistoryOptionsState
             tkErrorBox "Deadlock found!"
          }
          set done 1
          }

    } else {
      procInsertHistoryOptionsState
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

proc procAnimateUntil {} {
    set Formula [Dialog_Prompt "Animate randomly (at most 1000 steps) until this LTL formula is true (use {...} for B predicates,
  and operators G,F,X,U,W,R,true,false,not,&,or,=>,e(Op)):"]
    if {$Formula != ""} {
      if [prolog tcltk_execute_until('$Formula')] {
              #puts "Executing tcltk_execute_until command with the formula $Formula..."
              procShowErrors
              procInsertHistoryOptionsState
      } else {
              tkErrorBox "Something went wrong during random animation."
      }
    }

}


proc procResetPromptToNumber {} {
    global prompt
    if {$prompt(result)== "" || ![string is integer $prompt(result)]} {
      set prompt(result) "10"
    }
}
proc procExecute {} {
    procResetPromptToNumber
    prolog get_specification_description(operations_lc,R)
    prolog get_specification_description(operation,R2)
    set MaxSteps [Dialog_Prompt "Execute $prolog_variables(R) (choosing first $prolog_variables(R2) found, not storing intermediate states) with this maximum number of steps:"]
    if {$MaxSteps != ""} {
        if [string is integer $MaxSteps] {
          procExecuteUpTo $MaxSteps
        } else {
 		   tkErrorBox "Please enter a number ($MaxSteps is not a number)."
        }
    }
}

proc procExecuteUpTo {maxNrSteps} {
    procResetOptions "Execute..." DarkGray
 		if [prolog "tcltk_execute_model($maxNrSteps,NrSteps,Result)"] {
            # puts "Executed tcltk_execute_model($maxNrSteps) command with $prolog_variables(NrSteps) steps and result $prolog_variables(Result) ..."
            procShowErrors
            procInsertHistoryOptionsState
 		} else {
 		   tkErrorBox "Something went wrong during execution."
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

proc procExecLongestTrace {} {
   prologmnf tcltk_execute_longest_trace_from(root)
   procInsertHistoryOptionsState
}

proc procFindTrace {} {
    global ok
    destroy .trace
    set f .trace
	if [prolog tcltk_exists_an_open_node] {
	   set xtrmsg " (given explored states)"
	} else {
	   set xtrmsg ""
	}
	if [Dialog_Create $f "Find Shortest Trace$xtrmsg" -borderwidth 10] {
		message $f.msg -text "Trace to current node $xtrmsg:" -aspect 1000
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

# ---------------------------------------------------------------
# PREFERENCES
# ---------------------------------------------------------------


proc procSavePrefs {} {
  global app_dir
  if [prolog cli_prefs_file(File)] {
    prolog preferences:save_preferences('$prolog_variables(File)')
  } elseif [file writable "$app_dir/ProB_Preferences.pl"] {
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
  # procShowErrors
}

proc procSetPreferences {prefcategory} {
   procSetPreferences3 $prefcategory "Finished" ""
}
proc procSetPreferences3 {prefcategory OKBUTTON ForMsg} {
    global curprefcategory
    set curprefcategory $prefcategory
	  prologmnf preferences:backup_preferences
    global ok
    destroy .trace
    set f .trace
	  if [Dialog_Create $f "View/Edit Preferences$ForMsg" -borderwidth 10] {
		    message $f.msg -text "List of Preferences$ForMsg" -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        listbox $f.frmSource.text -yscroll "$f.frmSource.scrolly set" \
         -setgrid 1 -height 10 -width 60
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
        button $b.ok -text $OKBUTTON -command {set ok 1; procPerformSetPrefOption}
        button $b.cancel -text Cancel -command {set ok 2}
        button $b.resetall -text "Reset all to defaults" -command {procResetPrefToDefaults}
        pack $b.ok $b.cancel $b.resetall -side right

        bind $f.frmSource <Return> {set ok 1 ; procPerformSetPrefOption; break}
        bind $f.frmSource <Control-c> {set ok 1 ; break}
      }

    prologmnf get_preferences_list($prefcategory,Res)
  	# set Result [split [string trimright $prolog_variables(Res) {;}] \;]
	  set Result $prolog_variables(Res)
    foreach i $Result {
       $f.frmSource.text insert end [string trimleft $i]
    }

    # $f.frmSource.text configure -state normal

    set ok 0
	Dialog_Wait $f ok $f.frmSource
	Dialog_Dismiss $f
	if {$ok==1} {
	  global app_dir
	  procSavePrefs
	  return true
	} else {
	  prologmnf preferences:revert_preferences
	  return false
	}
	# procInsertHistoryOptionsState
}
proc procResetPrefToDefaults {} {
    global curprefcategory
    procPerformSetPrefOption
    prologmnf reset_to_defaults($curprefcategory)
    procPerformGetPrefOption
    proc_updateHighlightingPreferencesComments
}
proc procPerformPrefOption {} {
    global curprefcategory
    procPerformSetPrefOption
    procPerformGetPrefOption
    proc_updateHighlightingPreferencesComments
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
              set textPrefVal $colres
            }
        } elseif {$curtype == "path"} {
            set pathres [tk_getOpenFile -parent .trace]
            if {$pathres != ""} {
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
        } elseif {$preftype=="string" || $preftype=="rgb_color" || $preftype=="path" || $preftype=="file_path" || $preftype=="text_encoding"} {
           # preference value could start with UpperCase character...
           prolog set_ith_preference_value($curprefcategory,$lastiOption,'$textPrefVal')
        } else {
           # preference is lower-case atom or number (for the latter we should not add quotes)
           if {![prolog set_ith_preference_value($curprefcategory,$lastiOption,$textPrefVal)]} {
               tkErrorBoxNoParent "Could not set $curprefcategory preference to $textPrefVal"
           }
        }
   }
}

proc procSetTextEditorPreferences {} {
   global text_editor_arr
   destroy .textEditorPref
   eval global epref
   set epref .textEditorPref
   if [Dialog_Create $epref "Text Editor Preferences" -borderwidth 10] {
       wm resizable $epref 0 0
       wm transient $epref .main.frmSource
       frame $epref.body -borderwidth 3 -relief raised
       set eb $epref.body
       frame $eb.font
       # Sorted list with all system relevant fonts
       set allfonts [lsort -dictionary -unique [font families]]
       message $eb.fontmsg -text "Font" -font {-family times -size 14 -weight bold} -aspect 1000 -justify left
       label $eb.lfont -text "Editor font: "
       label $eb.lsize -text "Editor size: "
       ttk::combobox $eb.fonts -textvariable text_editor_arr(font_name) -width 20 -state readonly -value $allfonts
       ttk::combobox $eb.size -textvariable text_editor_arr(font_size) -state normal -width 4 -value {8 9 10 12 14 16 18 20 22 24 36 72}
       scale $eb.scale -from 8 -to 36 -orient horizontal -length 120 -variable text_editor_arr(font_size) -tickinterval 6 -showvalue false
       message $eb.title1 -text "Line Numbers" -font {-family times -size 14 -weight bold} -aspect 1000 -justify left
       message $eb.title2 -text "Current Line" -font {-family times -size 14 -weight bold} -aspect 1000 -justify left
       message $eb.title3 -text "Bracket Matching" -font {-family times -size 14 -weight bold} -aspect 1000 -justify left
       message $eb.title4 -text "File Content Changed" -font {-family times -size 14 -weight bold} -aspect 1000 -justify left
       if {$text_editor_arr(ctext_package)} {
           set st disabled
       } else {
           set st normal
       }
       ttk::checkbutton $eb.check1 -text "Display Line Numbers                " -variable text_editor_arr(linenumbers) \
             -onvalue true -offvalue false \
                         -command {procSetLinenumbersValueToTrue $text_editor_arr(linenumbers)} -state $st
       ttk::checkbutton $eb.check2 -text "Highlight Current Line               " -variable text_editor_arr(highlight_line)\
             -onvalue true -offvalue false \
                         -command {procSetHighlightCurrentLineValueToTrue $text_editor_arr(highlight_line)}
       ttk::checkbutton $eb.check3 -text "Highlight Bracket                       " -variable text_editor_arr(highlight_brackets)\
             -onvalue true -offvalue false \
                         -command {procSetPrefOption text_editor_arr highlight_brackets $text_editor_arr(highlight_brackets)}
       ttk::checkbutton $eb.check4 -text "Ask When File Content Changed" -variable text_editor_arr(ask_when_content_changed)\
             -onvalue true -offvalue false \
                         -command {procSetPrefOption text_editor_arr ask_when_content_changed $text_editor_arr(ask_when_content_changed)}
       frame $epref.buttons -relief flat
       frame $epref.buttons.right
       button $epref.buttons.reset -text "Reset all to defaults" -command "procResetToDefaultTextEditorPreferences" -activebackground lightblue
       button $epref.buttons.right.close -text "Close" -command {destroy .textEditorPref} -activebackground lightblue
       button $epref.buttons.right.apply -text "Apply" -command "procApplyTextEditorPreferences" -activebackground lightblue

       grid $eb.fontmsg -row 0 -column 0 -sticky nw -pady 5
       grid $eb.lfont -row 1 -column 0 -sticky e
       grid $eb.fonts -row 1 -column 1 -columnspan 2 -sticky sew
       grid $eb.lsize -row 2 -column 0 -sticky ne -pady 3
       grid $eb.size -row 2 -column 1 -sticky new -pady 3
       grid $eb.scale -row 2 -column 2 -sticky sew -pady 6
       foreach t {title1 title2 title3 title4} c {check1 check2 check3 check4} i {3 5 7 9} j {4 6 8 10} {
           grid $eb.$t -row $i -column 0 -sticky nw -pady 5
               grid $eb.$c -row $j -column 0 -columnspan 3 -sticky nw -pady 3 -padx 35
       }
       pack $epref.buttons.right.close $epref.buttons.right.apply -side right -padx 5
       pack $epref.buttons.reset $epref.buttons.right -side left -padx 20
       pack $eb -ipady 10 -ipadx 10 -side top -pady 15
       pack $epref.buttons -side top
   }
}

proc getCurrentFontName {} {
   global text_editor_arr
   set FontName [string map {" " ""} $text_editor_arr(font_name)]
   return "$FontName"
}
proc getCurrentFont {} {
   global text_editor_arr
   set FontName [string map {" " ""} $text_editor_arr(font_name)]
   return "$FontName $text_editor_arr(font_size)"
}
proc getCurrentStateViewerFont {} {
   global text_editor_arr scaleStateViewerUp scaleStateViewerDown stateViewerFontSize
   prolog get_preference(tk_custom_state_viewer_font_name,FN)
   if {$prolog_variables(FN) != ""} {
       set FontName $prolog_variables(FN)
   } else {
       set FontName [string map {" " ""} $text_editor_arr(font_name)]
   }
   prolog get_preference(tk_custom_state_viewer_font_size,FS)
   set stateViewerFontSize $prolog_variables(FS)
   if {$stateViewerFontSize != 0} {
       set FontSize [expr ($scaleStateViewerUp*$stateViewerFontSize)/$scaleStateViewerDown]
   } else {
       set FontSize [expr ($scaleStateViewerUp*$text_editor_arr(font_size))/$scaleStateViewerDown]
   }
   return "$FontName $FontSize"
}
proc procApplyTextEditorPreferences {} {
   global text_editor_arr
   set FontName [string map {" " ""} $text_editor_arr(font_name)]
   if [catch {.main.frmSource.text configure -font "$FontName $text_editor_arr(font_size)"}] {
       puts "Error by setting the new font \'$text_editor_arr(font_name) $text_editor_arr(font_size)\'."
   } else {
       prolog "set_preference(font_name,'$text_editor_arr(font_name)')"
       prolog "set_preference(font_size_only,$text_editor_arr(font_size))"
   }
}

proc procResetToDefaultTextEditorPreferences {} {
   global text_editor_arr
   prolog preference_default_value(font_name,Value)
   set curvalue $prolog_variables(Value)
   prolog "set_preference(font_name,'$curvalue')"
   set text_editor_arr(font_name) $curvalue
   set prolog_preferences [list font_size_only show_line_numbers highlight_current_line highlight_brackets ask_when_content_changed]
   set tcltk_preferences  [list font_size linenumbers highlight_line highlight_brackets ask_when_content_changed]
   foreach pref_name $prolog_preferences editor_pref $tcltk_preferences  {
     prolog preference_default_value($pref_name,Value)
     set curvalue $prolog_variables(Value)
     prolog set_preference($pref_name,$curvalue)
     set text_editor_arr($editor_pref) $curvalue
   }
   .main.frmSource.text configure -font "$text_editor_arr(font_name) $text_editor_arr(font_size)"
}

proc procResetToDefaultModelCheckerPreferences {} {
   global model_checker_arr
   set model_checker_arr(pge) off; set model_checker_arr(por) off
   set model_checker_arr(use_pge) false
   set model_checker_arr(use_por) false
   prolog "set_preference(pge,off)" ; prolog "set_preference(por,off)"
   prolog "set_preference(dependency_enable_predicates,false)"
}

proc update_mc_array {} {
  global model_checker_arr
  foreach id [array names model_checker_arr] {
    if {$id eq "use_por"} {
      prolog get_preference(por,Value)
      set Val $prolog_variables(Value)
      if {$Val eq "ample_sets" || $Val eq "ample_sets2"} {
        set model_checker_arr($id) true
      } else {
        set model_checker_arr($id) false
      }
    } else {
      if {$id eq "use_pge"} {
        prolog get_preference(pge,Value)
        set Val $prolog_variables(Value)
        if {$Val eq "off"} {
          set model_checker_arr($id) false
        } else {
          set model_checker_arr($id) true
        }
      } else {
        prolog get_preference($id,Value)
        set Val $prolog_variables(Value)
        set model_checker_arr($id) $Val
      }
    }
  }
}


proc procModelCheckerPreferencesViewer {} {
   global model_checker_arr
   destroy .mcPref
   eval global mcpref
   set mcpref .mcPref
   if [Dialog_Create $mcpref "Model Checker Preferences" -borderwidth 10] {
       update_mc_array
       wm resizable $mcpref 0 0
       wm transient $mcpref .main.frmSource
       frame $mcpref.body -borderwidth 3 -relief raised
       set mcb $mcpref.body
       global PORTechniqueVar

       if {$model_checker_arr(pge) != "off"} {
          set st_pge normal
       } else {
          set st_pge disabled
       }

       ttk::checkbutton $mcb.use_pge -text "+ Partial Guard Evaluation" -variable model_checker_arr(use_pge) \
             -onvalue true -offvalue false -command {setSubOptionStates .mcPref.body $model_checker_arr(use_pge) [list pge_options] normal; procSetWidgetToNormalState .mcPref.buttons.right.apply}
       ttk::combobox $mcb.pge_options -textvariable model_checker_arr(pge) -width 15 -state $st_pge -value {"off" "disabled" "enabled" "full" "disabled2" "enabled2" "full2"}

      if {$model_checker_arr(enable_graph)} {
        set st_enable_graph normal
      } else {
        set st_enable_graph disabled
      }

#       ttk::checkbutton $mcb.use_pge_dis  -text "disable"     -onvalue true -offvalue false -command {procSetWidgetToNormalState .mcPref.buttons.right.apply}\
#		-variable model_checker_arr(use_pge_dis)  -state $st_pge
#       ttk::checkbutton $mcb.use_pge_ind  -text "independent" -onvalue true -offvalue false -command {procSetPrefOption $model_checker_arr(use_pge_ind) "use_pge_ind"}\
#		-variable model_checker_arr(use_pge_ind)  -state $st_pge
#       ttk::checkbutton $mcb.use_pge_keep -text "keep"        -onvalue true -offvalue false -command {procSetWidgetToNormalState .mcPref.buttons.right.apply}\
#		-variable model_checker_arr(use_pge_keep)  -state $st_pge

       ttk::checkbutton $mcb.use_por -text "+ Partial Order Reduction: " -variable model_checker_arr(use_por) -offvalue false -onvalue true \
		 -command {setSubOptionStates .mcPref.body $model_checker_arr(use_por) [list reductions por_enable_graph enable_graph_depth dep_enabling] normal;\
			   procSetWidgetToNormalState .mcPref.buttons.right.apply}

       if {$model_checker_arr(use_por)} {
	   set st_por readonly
       } else {
	   set st_por disabled
       }

       ttk::combobox $mcb.reductions -textvariable PORTechniqueVar -width 15 -state $st_por -value {"off" "ample sets" "sleep sets" "ample_sets2"}
       ttk::combobox $mcb.enable_graph_depth -textvariable model_checker_arr(enable_graph_depth) -width 5 -state $st_enable_graph -value {0 1 2 3 4 5 -1} -postcommand {procSetWidgetToNormalState .mcPref.buttons.right.apply}

       bind $mcb.reductions <Enter> { after idle {if {$model_checker_arr(por) != $PORTechniqueVar} \
					{set model_checker_arr(por) $PORTechniqueVar;procSetWidgetToNormalState .mcPref.buttons.right.apply}}}

      ttk::checkbutton $mcb.por_enable_graph  -text "Enable Graph" -onvalue true -offvalue false -command {setSubOptionStates .mcPref.body $model_checker_arr(enable_graph)\
         [list enable_graph_depth] normal; procSetWidgetToNormalState .mcPref.buttons.right.apply} -variable model_checker_arr(enable_graph) -state $st_por
      ttk::checkbutton $mcb.dep_enabling  -text "Dependency Predicates" -onvalue true -offvalue false -variable model_checker_arr(dependency_enable_predicates)\
         -command {procSetWidgetToNormalState .mcPref.buttons.right.apply; update} -state $st_por

       frame $mcpref.buttons -relief flat
       frame $mcpref.buttons.right
       button $mcpref.buttons.reset -text "Reset all to defaults" -command "procResetToDefaultModelCheckerPreferences" -activebackground lightblue
       button $mcpref.buttons.right.close -text "Close" -command {destroy .mcPref} -activebackground lightblue
       button $mcpref.buttons.right.apply -text "Apply Changes" -command {procSetModelCheckerPreferences; .mcPref.buttons.right.apply configure -state disabled} -activebackground lightblue -state disabled

       grid $mcb.use_pge -row 0 -column 0 -sticky w -columnspan 3 -pady 5 -padx 10
       grid $mcb.pge_options -row 0 -column 2 -sticky ws -pady 5
       # grid $mcb.use_pge_dis  -row 1 -column 0 -sticky ws -padx 30
#       grid $mcb.use_pge_ind  -row 1 -column 1 -sticky ws -padx 10
       # grid $mcb.use_pge_keep -row 1 -column 1 -sticky ws -padx 7
       grid $mcb.use_por -row 2 -column 0 -sticky w -columnspan 2 -pady 5 -padx 10
       grid $mcb.reductions -row 2 -column 2 -sticky ws -pady 5

       grid $mcb.por_enable_graph   -row 4 -column 0 -sticky ws -padx 30 -pady 3
       grid $mcb.enable_graph_depth -row 4 -column 1 -sticky ws -padx 7  -pady 3
       grid $mcb.dep_enabling -row 5 -column 0 -sticky ws -padx 30 -pady 3

       pack $mcpref.buttons.right.close $mcpref.buttons.right.apply -side right -padx 5
       pack $mcpref.buttons.reset $mcpref.buttons.right -side left -padx 20
       pack $mcb -ipady 10 -ipadx 10 -side top -pady 15
       pack $mcpref.buttons -side top
       }
}

proc procSetWidgetToNormalState {widget} {
	$widget configure -state normal
}

proc procSetModelCheckerPreferences {} {
	global model_checker_arr PORTechniqueVar
	foreach id [array names model_checker_arr] {
		if {$id != "use_por" && $id != "use_pge"} {
			if {$id == "por" && !$model_checker_arr(use_por)} {
				set model_checker_arr(por) off
				set PORTechniqueVar off
				prolog preferences:set_preference($id,off)
			} else {
        if {$id == "pge" && !$model_checker_arr(use_pge)} {
          set model_checker_arr(pge) off
          prolog preferences:set_preference($id,off)
        } else {
  				set val [string map {" " "_"} $model_checker_arr($id)]
          # puts "id,val = $id,$val"
  				prolog preferences:set_preference($id,$val)
        }
			}
		}
	}
}

proc setSubOptionStates {mcb val opts st} {
	if {$val} {
		set state $st
	} else {
		set state disabled
	}
	foreach w $opts {
		$mcb.$w configure -state $state
	}
}

# -------
# procedure to display coverage of operation information
# -------

proc procComputeCoverage0 {} {
  procComputeCoverage 0
}
proc procComputeCoverage {type} {
  if [procSpecFileSuccessfullyLoaded] {
     procComputeCoverage2 $type
  } else {
       tkErrorBox "No specification file loaded. Cannot compute coverage."
  }
}
proc procComputeCoverage2 {type} {
    global spec_desc
    set Title "Analysing Coverage"
    set File "Coverage.txt"
    set AdditionalTxt ""
    prolog get_state_space_stats(Visited,T,Processed)
    if [prolog tcltk_exists_an_open_node] {
         set st "$prolog_variables(Visited) visited states"
         set pst "$prolog_variables(Processed) visited states"
    } else {
        if [prolog model_checking_is_incomplete(0,1,0,0,Msg,Term)] {
			set AdditionalTxt "\n$prolog_variables(Msg)$prolog_variables(Term)\n"
			set st "all states (***)"
			set pst "all states (***)"
        } else {
			set st "all states"
			set pst "all states"
        }
    }
    if {$type == "enabling"} {
        set no_interrupt [prologmnfi tcltk_compute_coverage_and_enabling(Res)]
        set Text "Coverage for $spec_desc(operations_lc) (with enabling info) in $pst"
    } elseif {$type == "degree"} {
        set no_interrupt [prologmnfi tcltk_compute_coverage_and_degree(Res)]
        set Text "Coverage for $spec_desc(operations_lc) (with degree info) in $pst"
    } elseif {$type == "minmax"} {
        set no_interrupt [prologmnfi tcltk_compute_min_max(Res)]
        set Text "Coverage for constants and variables (minumum and maximum values) in $st"
    } elseif {$type == "vacuous_invariants"} {
        set no_interrupt [prologmnfi tcltk_get_vacuous_invariants(Res)]
        set Title "List of vacuous invariants"
        set File "VacuousInvariants.txt"
        set Text "Vacuous implications inside invariants (implication or disjunctions which never trigger) in $st"
    } elseif {$type == "vacuous_guards"} {
        set no_interrupt [prologmnfi tcltk_get_vacuous_guards(Res)]
        set Title "List of vacuous guards"
        set File "VacuousGuards.txt"
        set Text "Vacuous guards inside $spec_desc(operations_lc) (guards which cannot be set independently to false) for $st"
    } else {
        set no_interrupt [prologmnfi tcltk_compute_coverage(Res)]
        set Text "Coverage for $spec_desc(operations_lc) in $pst"
    }
    # obtain a list of conjuncts with their status
    procShowErrors
    if {$no_interrupt} {
		set Result "$prolog_variables(Res)"
		global batch_mode
		if {$batch_mode} {
		   puts "Coverage Statistics:\n$Result"
		} else {
		  # procDisplayCoverage $Result $Text
			procShowList2 $Result "$Title" "$Text:$AdditionalTxt" 1 0 "$File" none
		}
	}
}

# -------
# procedure to analyse the invariant in the current state
# -------
proc procDebugOperation {} {
   global OpName
   set OpName [procChooseOperationOrINITcurState]
   if {$OpName != ""} {
	  procDebugProperties1 $OpName 0
   }
}

proc procChooseOperationOrINITcurState {} {
  procChooseOperationOrINITcurState2 1
}
proc procChooseOperationcurState {} {
  procChooseOperationOrINITcurState2 0
}
proc procChooseOperationOrINITcurState2 {showINITPROP} {
   # let user choose operation or INIT/SETUP_CONST for current state
   global spec_desc
   if {[prolog current_state_corresponds_to_initialised_b_machine]} {
      if {[prolog tcltk_unique_top_level_operation(Name)]} {
         return $prolog_variables(Name)
      } else {
         return [procChooseOperation]
      }
   } elseif {[prolog current_state_corresponds_to_setup_constants_b_machine]} {
      if {$showINITPROP} {
         return [procChooseElementFromList {@INITIALISATION} "$spec_desc(operations)" "no" "" "var" "true"]
      } else {
         return {@INITIALISATION}
      }
   } else {
      if {$showINITPROP} {
         return [procChooseElementFromList {@PROPERTIES} "$spec_desc(operations)" "no" "" "var" "true"]
      } else {
         return {@PROPERTIES}
      }
   }
}

proc procChooseOperationOrINITIALISATION {} {
    global spec_desc
    prologmnf tcltk_top_level_operations(LN)
    set Lst {@INITIALISATION}
    append Lst $prolog_variables(LN)
    return [procChooseElementFromList $Lst "$spec_desc(operations)" "no" "" "var" "true"]
}


proc procVisualiseOperationPre {} {
   global OpName strFilename
    if [procSpecFileSuccessfullyLoaded] {
	   # set OpName [Dialog_Prompt "Enter operation name"]
	   set OpName [procChooseOperationOrINITcurState]
	   if {$OpName != ""} {
		   procVisualiseInvariantOrOpPRE $OpName
		   procShowErrors
	   }
    } else {
       tkErrorBox "No specification file loaded. Cannot visualise operation enabling condition."
    }
}

proc procChooseOperation {} {
    return [procChooseOperation2 "no" "" "var" "true"]
}
proc procChooseOperation2 {ChkBox ChkBoxMsg ChkBoxVar FilterPred} {
    prologmnf tcltk_top_level_operations(LN)
    global spec_desc
    return [procChooseElementFromList $prolog_variables(LN) "$spec_desc(operations)" $ChkBox $ChkBoxMsg $ChkBoxVar $FilterPred]
}
proc procChooseElementFromList {List ElName ChkBox ChkBoxMsg ChkBoxVar FilterPred} {
    global ok
    destroy .trace
    set f .trace

	if [Dialog_Create $f "Choose $ElName" -borderwidth 10] {
		message $f.msg -text "List of $ElName" -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        listbox $f.frmSource.text -yscroll "$f.frmSource.scrolly set" \
         -setgrid 1 -height 9 -width 40
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
	if {$ok==1} {
      set sel [.trace.frmSource.text curselection]
	  set res [lindex $List $sel]
	  return $res
	} else {
	  return ""
	}
}


proc procExecuteOperation {} {
   global OpName
   # set OpName [Dialog_Prompt "Enter operation name"]

   if {[prolog b_machine_has_constants_or_properties] && [prolog current_expression(root,_)]} {
       set OpName {$setup_constants}
   } else {
       set OpName [procChooseOperationcurState]
   }
   procExecuteOperation2
}
proc procExecuteOperationWithName {Name} {
   global OpName
   set OpName $Name
   procExecuteOperation2
}
proc procExecuteOperation2 {} {
   global OpName
   if {$OpName != ""} {
		prolog b_get_machine_operation_parameter_names('$OpName',LP)
        # puts "paras=$prolog_variables(LP)"
        if {$prolog_variables(LP) == ""} {
             prolog b_get_operation_non_det_modifies('$OpName',NP)
             #puts "non_det_modifies=$prolog_variables(NP)"
             if {$prolog_variables(NP) == ""} {
                set msg "Postcondition for $OpName"
             } else {
                set msg "Non-deterministically Modified Variables for $OpName"
             }
             procChooseValuesForParametersWithCheckBoxExtra $msg $prolog_variables(NP) "Execute" "" "Visualise"
        } else {
		     procChooseValuesForParametersWithCheckBoxExtra "Parameters for $OpName" $prolog_variables(LP) "Execute" "" "Visualise"
		}
         global ParameterValues AdditionalPredicate ok
         if {$ok} {
           global debugOpName
           if {$ok==2} {
		       set debugOpName $OpName
               VisualiseOpGuardWithParasAndAdditionalPredicate
           } else {
               if [prolog "tcltk_add_user_executed_operation('$OpName',\[$ParameterValues\],'$AdditionalPredicate')"] {
                  procShowErrors
                  procInsertHistoryOptionsState
               } else {
                  procShowErrors
                  global spec_desc
                  # tkErrorBox "Could not execute $spec_desc(operation_lc) $OpName\nExtra Predicate: $AdditionalPredicate\nParameters: $ParameterValues."
                  set Msg "Could not execute $spec_desc(operation_lc) $OpName"
                  set Result "Extra Predicate: $AdditionalPredicate\nParameters: $ParameterValues."
                  set ExtraBtn "Visualise..."
                  set Cmd "VisualiseOpGuardWithParasAndAdditionalPredicate"
                  set debugOpName $OpName
                  procShowList3 $Result "$spec_desc(operation_lc) not executable with additional guard" $Msg 1 0 $ExtraBtn $Cmd "OpNotExecutable.txt" WarningIcon
                }
            }
          }
   }
}

proc VisualiseOpGuardWithParasAndAdditionalPredicate {} {
      global debugOpName
      global ParameterValues AdditionalPredicate
      global strFilename
      set rootName [file rootname $strFilename]
      set dotName {}
      set psName {}
      append dotName $rootName ".dot"
      append psName $rootName ".pdf"
      if [prolog "generate_dot_from_operation_with_params('$debugOpName',\[$ParameterValues\],'$AdditionalPredicate','$dotName')"] {
             procShowErrors
             procVisualiseDotFile $dotName $psName
      } else {
          tkErrorBox "Generating visualisation of precondition/guard for $debugOpName failed."
      }
}

proc procChooseValuesForParameters {ParKind Parameters BtnName} {
   procChooseValuesForParametersWithCheckBox $ParKind $Parameters $BtnName ""
}
proc procChooseValuesForParametersWithCheckBox {ParKind Parameters BtnName CheckBoxTxt} {
   procChooseValuesForParametersWithCheckBoxExtra $ParKind $Parameters $BtnName $CheckBoxTxt ""
}
proc procChooseValuesForParametersWithCheckBoxExtra {ParKind Parameters BtnName CheckBoxTxt ExtraBtnName} {
    global ok choose_para_checkbutton1
    global ParameterValues AdditionalPredicate
		destroy .execOp
		set f .execOp
		if [Dialog_Create $f "Enter $ParKind" -borderwidth 10] {
			message $f.msg -text "$ParKind" -aspect 1000
			# ------- code source frames
			frame $f.frmVals -borderwidth .1c -relief groove

			set b [frame $f.buttons]
			pack $f.msg $f.frmVals $b -side top -fill x

			if {$CheckBoxTxt != ""} {
			    checkbutton $b.checkbutton1 -text "$CheckBoxTxt" -variable choose_para_checkbutton1 -onvalue 1 -offvalue 0
                 pack $b.checkbutton1  -fill x -fill y
			}

			button $b.ok -text $BtnName -command {set ok 1}
			button $b.cancel -text Cancel -command {set ok 3}
			if {$ExtraBtnName != ""} {
			    button $b.extra -text $ExtraBtnName -command {set ok 2}
			    pack $b.ok $b.extra $b.cancel -side right
			} else {
			    pack $b.ok $b.cancel -side right
			}

			foreach i $Parameters {
			   global exopvars
			   # i may contain dots which confuses Tk below
			   regsub -all -- {\.} $i "___" itkname
				 message $f.frmVals.valmsg$itkname -text "$i:" -aspect 1000
				 entry $f.frmVals.val$itkname -textvariable exopvars(textVal$i) -width 40 -state normal
				 pack $f.frmVals.valmsg$itkname $f.frmVals.val$itkname -side top -fill x
			}
			global precondval
			message $f.frmVals.premsg -text "Additional Predicate (over parameters and post-state):" -aspect 1000
			entry $f.frmVals.preval -textvariable precondval -width 60 -state normal
			pack $f.frmVals.premsg $f.frmVals.preval -side top

			bind $f.frmVals <Return> {set ok 1 ; break}
			bind $f.frmVals <Control-c> {set ok 3 ; break}
		}

		# $f.frmVals.text configure -state normal

		set ok 0
		Dialog_Wait $f ok $f.frmVals
		Dialog_Dismiss $f
		if { $ok==1 || $ok==2 } {
		    puts "Executing/Visualising ($ok)"
		  if {$precondval != ""} {
		      set AdditionalPredicate [escapeChars $precondval]
		  } else {
		      set AdditionalPredicate "(TRUE=TRUE)"
		  }
		  set ParameterValues ""
		  foreach i $Parameters {
			     set val [escapeChars $exopvars(textVal$i)]
			     # puts "Parameter $i = $val"
			     if {$val != ""} {
			        if {$ParameterValues == ""} {
			           set ParameterValues "=('$i','$val')"
			        } else {
			           append ParameterValues ", =('$i','$val')"
			        }
			     }
		  }
		  puts "Parameters $Parameters and values $ParameterValues and Predicate $AdditionalPredicate"
		} else {
		   set ok 0
		   set ParameterValues ""
		   set AdditionalPredicate ""
		}
}

proc prologmnfi {Query} {
   # checks that a prolog query does not fail (mnf) but allows interrupts
   set PQ "prolog interruptable_call($Query)"
   if [uplevel 1 $PQ] {
      return 1
   } else {
      tkErrorBox "Prolog Query unexpectedly failed or interrupted: $Query"
      procShowErrors
      return 0
   }
}

proc prologmnf {Query} {
   # checks that a prolog query does not fail (mnf)
   set PQ "prolog $Query"
   # puts "exec prolog $Query"
   if [uplevel 1 $PQ] {
      # puts "done prolog $Query"
      return 1
   } else {
      tkErrorBox "Prolog Query unexpectedly failed: $Query"
      procShowErrors
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
proc procDebugGOAL {} {
   procDebugProperties1 "@GOAL" 0
}
proc procDebugProperties1 {OpName UnsatCore} {
    global spec_desc
    puts "procDebugProperties $OpName"
    set ExtraBtn "Minimize..."
    set UnsatCoreFile "Debug"
    if {$UnsatCore} {
       global expert_user
	   if {$expert_user} {
	      set ans [tk_messageBox -default no \
	            -message "Precise minimisation of unsatisfiable predicates (may take a long time)?" \
	            -title "Minmise Unsatisfiable Predicates" -type yesnocancel -icon question -parent .]
	   } else {
	      set ans "yes"
	   }
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
    if [prolog interruptable_call(tcltk_debug_properties_or_op('$OpName',$Unsat,Res,Satisfiable))] {
        if {$prolog_variables(Satisfiable)==false} {
           assertBatchError "Property Unsatisfiable"
        } else {
           set ExtraBtn "none"
        }
		# set Result [split [string trimright $prolog_variables(Res) {;}] \;]
		set Result   $prolog_variables(Res)
		procShowErrors
		if {$OpName == "@PROPERTIES"} {
		   set Msg1 "Constraints and Properties"
		   set Msg2 "the CONSTRAINTS and PROPERTIES clauses"
		} else {
		   set Msg1 "$OpName Enabling Condition"
		   set Msg2 "the $spec_desc(operation_lc) $OpName"
		}
		set Cmd "procDebugProperties1 $OpName 1"
		procShowList3 $Result "Debugging $Msg1" "Debugging $Msg2:" 1 0 $ExtraBtn $Cmd "$UnsatCoreFile.txt" none
    } else {
	   tkErrorBox "Debugging of $OpName interrupted or failed."
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
   procAnalysePredCheck2 $TYPE $ExpTot $ExpFal $ExpUnk 1
}

proc procAnalysePredCheck2 {TYPE ExpTot ExpFal ExpUnk ErrorIfFalse} {
    prolog tcltk_analyse_option($TYPE,Desc)
    # We do not use unicode but ascii: text widget becomes quite slow with unicode
    prolog "register_conjunct_register_error_span"
    # error spans registered and then shown by procShowErrors
    if [prologmnf tcltk_analyse(ascii,$TYPE,Res,Total,False,Unknown)] {
		# set Result [split [string trimright $prolog_variables(Res) {;}] \;]
		set Result $prolog_variables(Res)
		# puts "Showing results"
		procShowErrors
		set Tot $prolog_variables(Total)
		procCheckNumber "conjuncts" $ExpTot $Tot
		set Fal $prolog_variables(False)
		procCheckNumber "false conjuncts" $ExpFal $Fal
		set Unk $prolog_variables(Unknown)
		procCheckNumber "unknown conjuncts" $ExpUnk $Unk
		if {$Fal==0 && $Unk==0} {
		   set Failed ""
		   if {$ErrorIfFalse==1} {
		      set TopLevelRes "SUCCESSFUL"
		      if {$TYPE=="invariant" &&  [prolog tcltk_state_exists_with_invariant_violated]} {
		         append TopLevelRes ", but other state with INVARIANT violation exists"
		      }
		    } else {
		      set TopLevelRes "TRUE"
		    }
		   set ErrIcn "CheckOkIcon"
		} else {
		   set Failed " ($Fal false, $Unk unknown)"
		   if {$ErrorIfFalse==0 && $Unk==0} {
		      set TopLevelRes "FALSE"
		      set ErrIcn "WarningIcon"
		    } else {
		      set TopLevelRes "FAILED"
		      set ErrIcn "ErrorIcon"
		    }
		}
		procShowList2 "$Result ------ SUMMARY ------ Analysed:$Tot False:$Fal Unknown:$Unk" "Analysis of $prolog_variables(Desc) $TopLevelRes" "$Tot (Non-Typing) Conjuncts of $prolog_variables(Desc)$Failed:" 1 0 "PredicateAnalysis.txt" $ErrIcn
    }
    prolog "reset_conjunct_error_hook"
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
		# set Result [split [string trimright $prolog_variables(Res) {;}] \;]
		procShowList2 $prolog_variables(Res) "Analysing GOAL" "Conjunct of the GOAL:" 1 0 "Goal_Analysis.txt" none
    }
}



proc procShowTyping {} {
    if [prologmnf tcltk_show_typing_of_variables_and_constants(Res)] {
        procShowErrors
		procShowList2 $prolog_variables(Res) "Analysing Typing" "Typing of Constants and Variables:" 1 0 "Typing.txt" none
    }
}

proc procShowCurrentState {} {
  if [prolog tcltk_show_current_state(TB)] {
     puts "current: $prolog_variables(TB)"
	 set Result [split $prolog_variables(TB) \n]
     procShowList "$Result" "Current B State" "Textual representation of current B State"
  } else {
         tkErrorBox "Please set up constants first"
  }
}

proc procRefocusOnCurrentState {} {
  ## will discard state space and re-focus animator on current state
  set Msg "Refocus animator on current state and discard rest of state space?\nThis will allow you to run targeted model checking from this state."
  if [tk_messageBox -default yes\
                  -message "$Msg" -title "Refocus Animator" -type yesno -parent .] {
    prolog tcltk_clear_state_space_and_refocus_to_current_state
    procShowErrors
    procInsertHistoryOptionsState
  }
}

proc procShowNonDefaultPreferences {} {
  prolog preferences:get_non_default_preferences(TB)
  procShowList "$prolog_variables(TB)" "Non-Default Preferences" "Preferences which are not set to their default value:"
}

proc procShowList {Result WindowTitleMsg1 Msg2} {
    procShowList2 $Result $WindowTitleMsg1 $Msg2 0 0 "ProB_Output.txt" none
}
proc procShowListOK {Result WindowTitleMsg1 Msg2} {
    procShowList2 $Result $WindowTitleMsg1 $Msg2 0 0 "ProB_Output.txt" "CheckOkIcon"
}
proc procShowListError {Result WindowTitleMsg1 Msg2} {
    procShowList2 $Result $WindowTitleMsg1 $Msg2 0 0 "ProB_Output.txt" "ErrorIcon"
}

proc procShowList2 {Result WindowTitleMsg1 Msg2 SyntaxHighlight BugReportButton DefaultSaveName MessageIcon} {
  procShowList3 $Result $WindowTitleMsg1 $Msg2 $SyntaxHighlight $BugReportButton none none $DefaultSaveName $MessageIcon
}
proc procShowList3 {Result WindowTitleMsg1 Msg2 SyntaxHighlight BugReportButton ExtraButtonName ExtraCmd DefaultSaveName MessageIcon} {
  procShowList4 $Result $WindowTitleMsg1 $Msg2 $SyntaxHighlight $BugReportButton $ExtraButtonName $ExtraCmd $DefaultSaveName none $MessageIcon
}
proc procShowList4 {Result WindowTitleMsg1 Msg2 SyntaxHighlight BugReportButton ExtraButtonName ExtraCmd DefaultSaveName ListClickCommand MessageIcon} {
  global batch_mode testing_mode
  if {$batch_mode || $testing_mode==true} {
     puts "$WindowTitleMsg1\n$Msg2\n\n$Result"
  } else {
     global strFilename
	 set rn [file rootname [file tail $strFilename]]
	 set us "_"
     procShowList5 $Result $WindowTitleMsg1 $Msg2 $SyntaxHighlight $BugReportButton $ExtraButtonName $ExtraCmd "$rn$us$DefaultSaveName" $ListClickCommand $MessageIcon
  }
}


proc procShowList5 {Result WindowTitleMsg1 Msg2 SyntaxHighlight BugReportButton ExtraButtonName ExtraCmd DefaultSaveName ListClickCommand MessageIcon} {
    global ok
	destroy .invariant
	# destroy added for Mac TclTk
    set f .invariant
	if [Dialog_Create $f $WindowTitleMsg1 -borderwidth 10] {
	    if {$MessageIcon != "none"} {
	      set im [frame $f.msg]
	      # label $im.icon -bitmap $MessageIcon
	      label $im.icon -image $MessageIcon
		    message $im.msg -text $Msg2 -aspect 1000
		    pack $im.icon $im.msg -side left
	    } else {
		    message $f.msg -text $Msg2 -aspect 1000
		}
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
		bind $f.frmSource <Escape> {set ok 1 ; break}
	}

    $f.frmSource.text configure -state normal
    $f.frmSource.text delete 0.0 end
    foreach i $Result {
       $f.frmSource.text insert end $i
       $f.frmSource.text insert end "\n"
    }
    if {$SyntaxHighlight} {
        # puts "Analyse Invariant"
        procDoSyntaxColouringForAnalyseInvariant $f.frmSource.text
        # highlight errors in the main source window
        HighlightSyntaxErrors .main.frmSource.text
    } else {
        HighlightSyntaxErrors $f.frmSource.text
    }

    # $f.frmSource.text configure -state disabled
    # keep normal instead of disabled: this allows users to select and copy the list

    global procShowListClick_textwidget
    set procShowListClick_textwidget $f.frmSource.text
    if {$ListClickCommand != "none"} {
         bind $f.frmSource.text <Double-1> {procShowListClick}
    }

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
	    set DEBUGFILE [tk_getSaveFile -filetypes $types -initialfile $DefaultSaveName -parent . -defaultextension ".txt"]
		if {$DEBUGFILE != ""} {
		   # puts "Opening File: $DEBUGFILE"
		   set channel [prob_open "$DEBUGFILE" "w+"]
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


# old style Eval Window
proc procEval {} {
    set f .eval
    global eval_window_arr text_editor_arr
    if [Dialog_Create $f "Eval Console" -borderwidth 10] {
        entry $f.entry -textvariable eval_window_arr(input) -width 80
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        scrollbar $f.frmSource.scrollx -command "$f.frmSource.text xview" -orient h
        if {!$text_editor_arr(ctext_package)} {
             ctext $f.frmSource.text -yscroll "$f.frmSource.scrolly set" -xscroll "$f.frmSource.scrollx set" \
               -setgrid 1 -height 18 -state disabled -linemap 0
        } else {
             text $f.frmSource.text -yscroll "$f.frmSource.scrolly set" -xscroll "$f.frmSource.scrollx set" \
               -setgrid 1 -height 18 -state disabled
        }
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.scrollx -side bottom -fill x
        pack $f.frmSource.text -expand 1 -fill both
        set b [frame $f.buttons]
        pack $f.frmSource $f.buttons -side top -fill x
        pack $f.frmSource -pady 5 -expand 1 -fill both
        pack $f.entry -side bottom
        button $b.eval -text Eval -command {procEvalInput; procUpdateHistoryListAndHistoryIndex; set eval_window_arr(input) ""}
        button $b.ok -text Finished -command {destroy .eval; set eval_window_arr(input) ""; procSetHistoryToLastEvaluatedString}
        pack $b.ok $b.eval -side right
        bind $f.entry <Return> {procEvalInput;procUpdateHistoryListAndHistoryIndex;set eval_window_arr(input) "";break}
        bind $f.entry <Up> {if [prolog animation_mode(cspm)] {
                              procRecallHistoryEval -1 $eval_window_arr(csp_history) true
                            } else {
                              procRecallHistoryEval -1 $eval_window_arr(b_history) false}}
        bind $f.entry <Down> {if [prolog animation_mode(cspm)] {
                              procRecallHistoryEval 1 $eval_window_arr(csp_history) true
                            } else {
                              procRecallHistoryEval 1 $eval_window_arr(b_history) false}}
        bind $f.entry <Control-c> {destroy .eval ; break; set eval_window_arr(input) ""; procSetHistoryToLastEvaluatedString}
        bind $f <Destroy> {set eval_window_arr(input) "1+1"; procSetHistoryToLastEvaluatedString}
        bind $f <<RIGHTCLICK>> {after idle {break}}
	}
        catch {tkwait visibility .eval}
        if [prolog get_all_errors(Res)] {
        }
}

proc procEvalConsole {} {
    global eval_window_arr text_editor_arr
    destroy .eval
    set e .eval
    prolog preferences:get_history_elements_list(eval_history_elements,false,Hist)
    prolog preferences:get_history_elements_list(eval_csp_history_elements,false,CSPHist)
    set eval_window_arr(b_history) $prolog_variables(Hist)
    set eval_window_arr(csp_history) $prolog_variables(CSPHist)
    set eval_window_arr(b_hist_index) [llength $eval_window_arr(b_history)]
    set eval_window_arr(csp_hist_index) [llength $eval_window_arr(csp_history)]
    if {!$text_editor_arr(ctext_package)} {
        if [Dialog_Create $e "Eval - 80x18" -borderwidth 10] {
            set CMD [procGetAccKey]
            if [prolog tools:host_platform(darwin)] {
                  set CopyAccKey $CMD
            } else {
                  set CopyAccKey "Control-Shift"
            }
            menu $e.menubar -tearoff 0
            $e config -menu $e.menubar

             foreach m {Console Edit Preferences} {
                 set mb [string tolower $m]
                 set $m [menu $e.menubar.$mb -tearoff 0]
                 $e.menubar add cascade -label $m -menu $e.menubar.$mb
             }

            # Console menu bar
            #    -> Clear Console
            #    -> Exit

            $e.menubar.console add command -label "Clear Console" -command {.eval.frmSource.text delete 1.0 end; procInitConsole .eval.frmSource.text}
            $e.menubar.console add command -label Exit -command {destroy .eval}

            # Edit menu bar
            #    -> Copy
            #    -> Paste
            #    -> Select All

            $e.menubar.edit add command -label Copy -command {after idle {procCutOrCopySlectedRangeOfText .eval.frmSource.text yes}} -accelerator $CopyAccKey+C
            $e.menubar.edit add command -label Paste -command {procPasteInEvalConsole .eval.frmSource.text} -accelerator $CopyAccKey+V
            $e.menubar.edit add command -label "Select All" -command {proc_selectAll .eval.frmSource.text} -accelerator $CopyAccKey+a

            # Preferences menu bar
            #    -> Use As
            #       -> Evaluator
            #       -> Simplifier

            $e.menubar.preferences add cascade -label "Use As" -menu $e.menubar.preferences.useas

            menu $e.menubar.preferences.useas -tearoff 0
            $e.menubar.preferences.useas add radiobutton -label "Evaluator"  -variable eval_window_arr(use_as) -value "evaluator"  -command {set eval_window_arr(use_as) "evaluator"}
            $e.menubar.preferences.useas add radiobutton -label "Simplifier" -variable eval_window_arr(use_as) -value "simplifier" -command {set eval_window_arr(use_as) "simplifier"}

            set eval_window_arr(input) ""
            frame $e.frmSource -borderwidth .1c -width 80
            prolog "get_preference(font_name,FontName)"
            set text_editor_arr(font_name) $prolog_variables(FontName)
            prolog "get_preference(font_size_only,FontSize)"
            set TextFont [string map {" " ""} $text_editor_arr(font_name)]
            ctext $e.frmSource.text -yscrollcommand "$e.frmSource.scrolly set"\
               -setgrid 1 -height 18 -state normal -linemap 0  -font "$TextFont $text_editor_arr(font_size)"
            scrollbar $e.frmSource.scrolly -command "$e.frmSource.text yview"
            if [prolog animation_mode(cspm)] {
                procDoOnTheFlyCSPSyntaxColouring $e.frmSource.text
            } else {
                procDoOnTheFlySyntaxColouring $e.frmSource.text
            }
            set eval_window_arr(input_index) 2.5
            set t $e.frmSource.text

            procInitConsole $t
            pack $e.frmSource.scrolly -side right -fill y
            pack $e.frmSource.text -expand 1 -fill both
            pack $e.frmSource -pady 5 -expand 1 -fill both
            focus -force .eval.frmSource.text
            bind $t <Configure> {procChangeEvalWidnowsTitle .eval .eval.frmSource.text}
            bind $t <Return> {procClearSelelction .eval.frmSource.text; procEvalInputConsole .eval.frmSource.text; procUpdateHistoryListAndHistoryIndex}
            bind $t <Up> {procClearSelelction .eval.frmSource.text
                          if [prolog animation_mode(cspm)] {
                              procRecallHistoryOfEvalConsole -1 $eval_window_arr(csp_history)
                          } else {
                              procRecallHistoryOfEvalConsole -1 $eval_window_arr(b_history)}}
            bind $t <Down> {procClearSelelction .eval.frmSource.text
                            if [prolog animation_mode(cspm)] {
                              procRecallHistoryOfEvalConsole 1 $eval_window_arr(csp_history)
                            } else {
                              procRecallHistoryOfEvalConsole 1 $eval_window_arr(b_history)}}
            bind $t <Left>  {procClearSelelction .eval.frmSource.text; procMoveCursorLeft "no"}
            bind $t <Right> {procClearSelelction .eval.frmSource.text; procMoveCursorRight}
            bind $t <Button-1> {after idle {procUpdateCursorAfterLeftMouseClick .eval.frmSource.text}}
            # bind $t <BackSpace> {procMoveCursorLeft "yes"}
            bind $t <Tab> {procClearSelelction .eval.frmSource.text; break}
            bind $t <$CopyAccKey-c> {after idle {procCutOrCopySlectedRangeOfText .eval.frmSource.text "yes"}; break}
            bind $t <$CopyAccKey-C> {after idle {procCutOrCopySlectedRangeOfText .eval.frmSource.text "yes"}; break}
            bind $t <$CMD-BackSpace> {procClearWindow .eval.frmSource.text; break}
            bind $t <$CMD-k> {procClearWindow .eval.frmSource.text; break}
            bind $t <Control-c> {after idle {tk::TextSetCursor .eval.frmSource.text $eval_window_arr(input_index); procAddNewEnterLine ; procSetHistoryToLastEvaluatedString}}
            bind $t <Control-C> {after idle {tk::TextSetCursor .eval.frmSource.text $eval_window_arr(input_index); procAddNewEnterLine ; procSetHistoryToLastEvaluatedString}}
	          bind $t <$CMD-v> {procClearSelelction .eval.frmSource.text; procPasteInEvalConsole .eval.frmSource.text; break}
      	    bind $t <$CMD-V> {procClearSelelction .eval.frmSource.text; procPasteInEvalConsole .eval.frmSource.text; break}
            bind $t <Control-a> {regexp {([0-9]+)\.[0-9]+} $eval_window_arr(input_index) match curline; after idle {tk::TextSetCursor .eval.frmSource.text $curline.5}; break}
            bind $t <Control-A> {regexp {([0-9]+)\.[0-9]+} $eval_window_arr(input_index) match curline; after idle {tk::TextSetCursor .eval.frmSource.text $curline.5}; break}
            bind $t <Home> {procClearSelelction .eval.frmSource.text; procMoveCursorLeft "no"}
            # does not work:
            #bind $t <<RIGHTCLICK>> {procPerformEvalContextMenus $t %X %Y}
            bind $t <Button-2> {selection clear; clipboard clear
                                after 1000 {catch {procDisableRightclickInEvalWindow $eval_window_arr(input_index)}}}
            bind $t <Delete>    {procClearSelelction .eval.frmSource.text; if [procDisableDeleteBinding .eval.frmSource.text 1] {break}}
            bind $t <BackSpace> {procClearSelelction .eval.frmSource.text; if [procDisableDeleteBinding .eval.frmSource.text 0] {break}}
            bind $t <Key> {if [procDisableKeyIfSelectionNotEmpty .eval.frmSource.text] {break}}
            wm protocol .eval WM_DELETE_WINDOW {set eval_window_arr(proCSPInterpreter) true; destroy .eval}
            if ![prolog animation_mode(cspm)] {
                update
                # ensure Java parser launched and is ready when user types in return
                prolog parsercall:ensure_console_parser_launched
            }
        }
    } else {
             procEval
    }
}

proc procDisableDeleteBinding {text_widget is_del} {
  global eval_window_arr
  regexp {([0-9]+)\.([0-9]+)} [.eval.frmSource.text index insert] match curline curcolumn
  regexp {([0-9]+)\.([0-9]+)} $eval_window_arr(input_index) match evalline evalcolumn
  if {$curline < $evalline} {
    return 1
  } elseif {$curline == $evalline} {
    if {$curcolumn > (5 - $is_del)} {
      incr curcolumn -1
      set eval_window_arr(input_index) "$curline.$curcolumn"
      return 0
    }
    return 1
  } else {
    return 0
  }
}

proc procClearSelelction {text_widget} {
  global eval_window_arr
  if {[$text_widget tag ranges sel] != ""} {
      selection clear
      tk::TextSetCursor .eval.frmSource.text $eval_window_arr(input_index)
  }
}

proc procDisableKeyIfSelectionNotEmpty {text_widget} {
  if {[$text_widget tag ranges sel] != ""} {
     return 1
  }
  return 0
}

proc procUpdateCursorAfterLeftMouseClick {text_widget} {
  global eval_window_arr
  set curindex [.eval.frmSource.text index insert]
  regexp {([0-9]+)\.([0-9]+)} $curindex match curline curcolumn
  regexp {([0-9]+)\.([0-9]+)} $eval_window_arr(input_index) match evalline evalcolumn
  if {($curline < $evalline) || ($curcolumn < 5)} {
    tk::TextSetCursor .eval.frmSource.text $eval_window_arr(input_index)
  } else {
    set eval_window_arr(input_index) $curindex
  }
}

proc procInitConsole {t} {
   set seconds [clock seconds]
   $t insert 1.0 "Logged on [clock format $seconds]\n"
   $t insert 2.0 ">>>> "
}

proc procPerformEvalContextMenus {t X Y} {
   destroy .eval_popup
   menu .eval_popup -tearoff 0
   .eval_popup add command -label "Clear" -command {procClearWindow}
   .eval_popup add command -label "Evaluate" -command {procEvalInputConsole $t; procUpdateHistoryListAndHistoryIndex}
   tk_popup .eval_popup $X $Y
}


proc procPasteInEvalConsole {w} {
   global eval_window_arr
   if {[$w tag ranges sel] != ""} {
        $w tag delete sel
	tk::TextSetCursor $w $eval_window_arr(input_index)
   }
   regexp {([0-9]+)\.[0-9]+} [$w index insert] match line
   regexp {([0-9]+)\.[0-9]+} $eval_window_arr(input_index) match curline
   if {$line != $curline} {
	tk::TextSetCursor $w $eval_window_arr(input_index)
   }
   tk_textPaste $w
}

proc tk_textPaste {w} {
   global tcl_platform
   array set range {}
   if { [catch {$w tag ranges sel} ranges] } { return }
   foreach {first last} $ranges {
     set range($first) $last
     # puts "set range($first) \[$last\]"
   }

   if { [catch {::tk::GetSelection $w CLIPBOARD} sel] } { return }

   set oldSeparator [$w cget -autoseparators]
   if { $oldSeparator } {
     $w configure -autoseparators 0
     $w edit separator
   }
   if { ![string equal [tk windowingsystem] "x11"] } {
     foreach first [lsort -decreasing [array names range]] {
       $w delete $first $range($first)
     }
   }
   $w insert insert $sel
   if { $oldSeparator } {
     $w edit separator
     $w configure -autoseparators 1
   }
 }

proc tk_textCopy {w} {
   set list [list]
   if { [catch {$w tag ranges sel} ranges] } { return }
   foreach {first last} $ranges {
     lappend list [$w get $first $last]
   }
   clipboard clear -displayof $w
   clipboard append -displayof $w [join $list \n]
 }

proc procDisableRightclickInEvalWindow {index} {
     after idle {
     set ind [.eval.frmSource.text index current]
#        puts "ind: $ind"
     if [catch {selection get} sel] {
          if [catch {selection get -selection CLIPBOARD} sel] {
              # no text were pasted
          } else {
#                 puts "Selection: $sel"
              set len [string length $sel]
              .eval.frmSource.text delete $ind "$ind +$len chars"
          }
     } else {
#                 puts "Selection: $sel"
              set len [string length $sel]
              .eval.frmSource.text delete $ind "$ind +$len chars"
     } }
     tk::TextSetCursor .eval.frmSource.text $index
}

proc procRecallHistoryOfEvalConsole {num listHistory} {
    global eval_window_arr
    set ev .eval.frmSource.text
    if [prolog animation_mode(cspm)] {
        procRecallHistoryEval $num $listHistory true
    } else {
        procRecallHistoryEval $num $listHistory false
    }
    regexp {([0-9]+)\.([0-9]+)} [$ev index insert] match line col
    if {$col > 5} {
         $ev delete $line.5 $line.end
    }
    $ev insert $line.6 $eval_window_arr(input)
    $ev highlight $line.5 $line.end
    if {$eval_window_arr(input) != ""} {
        set chs [string length $eval_window_arr(input)]
        set col [expr {$chs + 6}]
        set eval_window_arr(input_index) $line.$col
    }
    after idle {tk::TextSetCursor .eval.frmSource.text $eval_window_arr(input_index)}
}

proc procCatchTabulatorEvent {} {
    after idle {
        regexp {([0-9]+)\.([0-9]+)} [.eval.frmSource.text index insert] match line col
        .eval.frmSource.text delete $line.5 $line.$col
        tk::TextSetCursor .eval.frmSource.text $line.6
    }
}

proc procMoveCursorLeft {is_backspace} {
    regexp {([0-9]+)\.([0-9]+)} [.eval.frmSource.text index insert] match line col
    if {$col < 6} {
        if {$is_backspace == "yes"} {
            after idle {}
            .eval.frmSource.text insert $line.5 " "
            tk::TextSetCursor .eval.frmSource.text $line.6
        } else {
            after idle {
                    regexp {([0-9]+)\.[0-9]+} [.eval.frmSource.text index insert] match line
                    tk::TextSetCursor .eval.frmSource.text $line.5
            }
        }
    }
}

proc procMoveCursorRight {} {
    after idle {
           regexp {([0-9]+)\.([0-9]+)} [.eval.frmSource.text index insert] match line col
           if {$col == 0} {
               # new line
               incr line -1
               tk::TextSetCursor .eval.frmSource.text $line.end
           }
    }
}

proc procClearWindow {twin} {
  global eval_window_arr
  # now get line and column number from input_index:
  regexp {([0-9]+)\.([0-9]+)} $eval_window_arr(input_index) match line col
  $twin delete 1.0 $line.0
  set eval_window_arr(input_index) "1.5"
}
proc procEvalInputConsole {twin} {
    global eval_window_arr
    regexp {([0-9]+)\.([0-9]+)} $eval_window_arr(input_index) match line col
    set eval_window_arr(input) [$twin get $line.5 $line.end]
    set eval_args [split $eval_window_arr(input) " "]
    if {$eval_window_arr(input) == ":x" || $eval_window_arr(input) == ":q"|| $eval_window_arr(input) == ":exit"} {
        set eval_window_arr(input) ""
        set eval_window_arr(input_index) "2.5"
	      set eval_window_arr(proCSPInterpreter) true
        wm withdraw .eval
    } elseif {$eval_window_arr(input) == ":help" || $eval_window_arr(input) == ":h"} {
        if [prolog animation_mode(cspm)] {
             procLaunchSyntaxInfo "CSP"
        } else {
             procLaunchSyntaxInfo "B"
        }
	    procAddNewEnterLine
    } elseif {$eval_window_arr(input) == ":show"} {
        if {[prolog last_expression_type(pred)]} {
            procVisualiseCustomPredicate_direct $eval_window_arr(last_input)
        } else {
            procShowExpressionAsDot $eval_window_arr(last_input)
        }
	    procAddNewEnterLine
    } else {
        set eval_window_arr(last_input) $eval_window_arr(input)
		if [prolog animation_mode(cspm)] {
			procRunCspmEvaluator
		} else {
		    procEvalConsoleCurrentInput
		}
    }
}

proc procAddNewEnterLine {} {
    global eval_window_arr
    set twin .eval.frmSource.text
    regexp {([0-9]+)\.([0-9]+)} [$twin index insert] match line col
    append EnterInput "\n" ">>>> "
    $twin insert $line.$col $EnterInput
    incr line
    set col 5
    set eval_window_arr(input_index) $line.$col
    $twin delete $eval_window_arr(input_index) end
    after idle {tk::TextSetCursor .eval.frmSource.text $eval_window_arr(input_index)}
}

proc procEvalInput {} {
    if [prolog animation_mode(cspm)] {
        procRunCspmEvaluator
    } else {
        procEvalCurrentInput
    }
}

proc procSetHistoryToLastEvaluatedString {} {
     global eval_window_arr
     if [prolog animation_mode(cspm)] {
         set eval_window_arr(csp_hist_index) [llength $eval_window_arr(csp_history)]
     } else {
         set eval_window_arr(b_hist_index) [llength $eval_window_arr(b_history)]
     }
}

proc procUpdateHistoryListAndHistoryIndex {} {
     global eval_window_arr
     if {[prolog animation_mode(cspm)]} {
       set history csp_history
       set hist_index csp_hist_index
       set eval_elements eval_csp_history_elements
     } else {
       set history b_history
       set hist_index b_hist_index
       set eval_elements eval_history_elements
     }

     if {$eval_window_arr(input) != ""} {
      set EevalInput [escapeChars $eval_window_arr(input)]
      #puts "EevalInput: $EevalInput"
      lappend eval_window_arr($history) $eval_window_arr(input)
      set history $eval_window_arr($history)
      set eval_window_arr($hist_index) [llength $history]
      set eval_window_arr(input) $EevalInput
      if [procIsSymbolPaired $EevalInput "\{" "\}"] { ;# saving the history of commands only with equal number of {'s and }'s
                     # there is a problem by getting strings with no paired braces of type { }
                     # later from ProB_Preferences.pl file.
        prolog preferences:add_element_to_history($eval_elements,'$EevalInput',50)
      }
    }
}

proc procRecallHistoryEval {num listHistory is_csp} {
   global eval_window_arr
   set nextIndex 0
   set len [llength $listHistory]
   if {[llength $listHistory] == 0} {
       ##there is no history saved
   } elseif {[llength $listHistory] >0} {
       if {$num == 1} {
           set nextIndex [procEvalConsoleHistoryGetNextIndex $is_csp 1]
           if {$nextIndex < [llength $listHistory]} {
                set eval_window_arr(input) [procGetEvalStringFromProlog [lindex $listHistory $nextIndex]]
                if {$is_csp} {
                    set eval_window_arr(csp_hist_index) $nextIndex
                } else {
                   set eval_window_arr(b_hist_index) $nextIndex
                }
           } else {
                set eval_window_arr(input) ""
                if {$is_csp} {
                    set eval_window_arr(csp_hist_index) $len
                } else {
                   set eval_window_arr(b_hist_index) $len
                }
           }
       } elseif {$num == -1} {
           set nextIndex [procEvalConsoleHistoryGetNextIndex $is_csp -1]
           if {$nextIndex >= 0} {
                set eval_window_arr(input) [procGetEvalStringFromProlog [lindex $listHistory $nextIndex]]
                if {$is_csp} {
                    set eval_window_arr(csp_hist_index) $nextIndex
                } else {
                   set eval_window_arr(b_hist_index) $nextIndex
                }
           } elseif {[llength $listHistory]>0} {
                set eval_window_arr(input) [procGetEvalStringFromProlog [lindex $listHistory 0]]
                if {$is_csp} {
                    set eval_window_arr(csp_hist_index) 0
                } else {
                   set eval_window_arr(b_hist_index) 0
                }
           } else {
                set eval_window_arr(input) ""
           }
       }
   }
}

proc procGetEvalStringFromProlog {input} {
   if {[string length $input] == 0} {
      # an empty list [] was the result
      return "\[\]"
   } else {
      return $input
   }
}

proc procEvalConsoleHistoryGetNextIndex {is_csp upd} {
  global eval_window_arr
  if {$is_csp} {
      return [expr {$eval_window_arr(csp_hist_index) + $upd}]
 } else {
      return [expr {$eval_window_arr(b_hist_index) + $upd}]
 }
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

proc procEvalCurrentInput {} {
    global eval_window_arr
    set widget .eval.frmSource.text
    $widget configure -state normal
    $widget insert end ">>>> $eval_window_arr(input)\n"
    set EevalInput [escapeChars $eval_window_arr(input)]
    prolog tcltk_eval('$EevalInput',Res,EnumWarning)
    set output [escapeChars $prolog_variables(Res)]
    $widget insert end "  $output\n"
    if {$prolog_variables(EnumWarning) == "true"} {
        $widget insert end "#### Warning: infinite types were enumerated\n"
    }
    if [prolog get_all_errors(ERes)] {
        set PResult [split [string trimright $prolog_variables(ERes) {;}] \;]
        $widget insert end "ERRORS:\n$PResult\n"
        prolog reset_errors
    }
    procDoSyntaxColouring $widget
    $widget see end
    $widget configure -state disabled
}

proc procEvalConsoleCurrentInput {} {
    global eval_window_arr
    if {$eval_window_arr(input) != ""} {
       set widget .eval.frmSource.text
       $widget configure -state normal
       regexp {([0-9]+)\.([0-9]+)} [$widget index insert] match line col
       $widget delete $line.5 $line.end
       $widget insert $line.5 $eval_window_arr(input)
       set eval_window_arr(input_index) [$widget index end]
       regexp {([0-9]+)\.[0-9]+} $eval_window_arr(input_index) match line
       set EevalInput [escapeChars $eval_window_arr(input)]
       if {$eval_window_arr(use_as) == "evaluator"} {
         prolog "interruptable_tcltk_eval(\'$EevalInput\',Res,EnumWarning,Solution)"
         set EvalResult [procGetEvalStringFromProlog $prolog_variables(Res)]
         procWrapEvalOutput $widget $EvalResult
         if {$prolog_variables(EnumWarning) == "true"} {
             $widget insert [$widget index insert] "#### Warning: infinite types were enumerated\n"
         }
         if [prolog get_all_errors(ERes)] {
             # set PResult [split [string trimright $prolog_variables(ERes) {;}] \;]
             set PResult [string map {";" "\n"} $prolog_variables(ERes)]
             procPrintOutEvalConsoleErrorsAndReset $widget $PResult
         }
         if {$prolog_variables(Solution) != ""} {
             procWrapEvalOutput $widget $prolog_variables(Solution)
         }
       } elseif {$eval_window_arr(use_as) == "simplifier"} {
         prolog "b_simplifier: tcltk_simplify_b_predicate_error(\'$EevalInput\',Res)"
         set EvalResult [procGetEvalStringFromProlog $prolog_variables(Res)]
         procWrapEvalOutput $widget $EvalResult
         if [prolog get_all_errors(ERes)] {
            # set PResult [split [string trimright $prolog_variables(ERes) {;}] \;]
            set PResult [string map {";" "\n"} $prolog_variables(ERes)]
            procPrintOutEvalConsoleErrorsAndReset $widget $PResult
         }
       }
       $widget see end
       incr line -2
       procAddNewEnterLine
       $widget highlight $line.0 end
    } else {
       procAddNewEnterLine
    }
}

proc procPrintOutEvalConsoleErrorsAndReset {widget errors} {
  global eval_window_arr
  $widget insert $eval_window_arr(input_index) "\nERRORS:\n$errors\n"
  if [prolog get_error_positions(EPos)] {
    set ErrorPosition [string map {";" "\n"} $prolog_variables(EPos)]
    #set eval_window_arr(input_index) [$widget index end]
    #puts "eval_window_arr(input_index) = $eval_window_arr(input_index)"
    $widget insert [$widget index insert] "\n$ErrorPosition"
    #puts "ErrorPosition = $ErrorPosition"
  }
  prolog reset_errors
}

proc procGetCurrentTextWidgetWidth {widget} {
    return [expr {([winfo width $widget] -2*[$widget cget -borderwidth] -4)\
                                  /[font measure [$widget cget -font] 0]}]
}

proc procGetCurrentTextWidgetHeight {widget} {
    return [expr {(([winfo height $widget] -2*[$widget cget -borderwidth] -4)\
                                  /[font measure [$widget cget -font] 0] -2)/2}]
}

proc procChangeEvalWidnowsTitle {widget twidget} {
    set width [procGetCurrentTextWidgetWidth $twidget]
    set height [procGetCurrentTextWidgetHeight $twidget]
    set title [join [list "Eval - $width" "$height"] x]
    wm title $widget $title
}


proc procWrapEvalOutput {widget output} {
    set strlen [string length $output]
    set FirstInsertIndex [$widget index insert]
    regexp {([0-9]+)\.[0-9]+} [$widget index insert] index line
    set firstn [string first "\n" [string trim $output "\n"]]
    # prolog "tools:print_message('check firstn=$firstn, strlen=$strlen')"
    if {$firstn < 1 || $strlen > 1000 } {
	# we want to wrap only long outputs consisting of one line only
		set width [expr {[procGetCurrentTextWidgetWidth $widget] -2}]
		set height [procGetCurrentTextWidgetHeight $widget]
		set k [expr { $width -1 }]
		set lines [procReturnFloatBase [expr {$strlen/$k}]]
		set remstr [expr {$strlen % $k}]
		if {$remstr > 0} {incr lines}
		set i 0; set j 0
		incr line
		while {$i < $lines} {
			  $widget insert $index "\n  "
			  $widget insert $line.2 [string range $output $j $k]
			  set j $k ; set k [expr {$k + $width -1}] ;# step interval
			  set index $line.end
			  incr i; incr j; incr line
		}
		$widget see $index
		set LastInsertIndex [$widget index insert]
		$widget insert $LastInsertIndex "\n  "
		$widget highlight $FirstInsertIndex $LastInsertIndex
    } else {
		$widget insert $index "\n\n$output\n"
		$widget highlight $FirstInsertIndex [$widget index insert]
    }
}

proc procReturnFloatBase {float} {
    return [lindex [split $float \.] 0]
}

proc procShowText {Result Msg1} {
    procShowText6 $Result $Msg1 none none "ProB_Output.txt" ".txt"
}

proc procShowText6 {Result Msg1 ExtraButtonName ExtraButtonCmd DefaultSaveName DefaultExt} {
   procShowText7 $Result $Msg1 "Done" $ExtraButtonName $ExtraButtonCmd $DefaultSaveName $DefaultExt
}
proc procShowText7 {Result Msg1 MainButtonName ExtraButtonName ExtraButtonCmd DefaultSaveName DefaultExt} {
  global ok
	destroy .showtext
	# destroy added for Mac TclTk
  set f .showtext
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
		if {$DefaultSaveName != ""} {
      button $b.save -text "Save..." -command {set ok 3}
      pack $b.save -side left
		}
		if {$ExtraButtonName!="none"} {
		  button $b.extra -text "$ExtraButtonName" -command {set ok 4}
		  pack $b.extra -side left
		}
		button $b.ok -text "$MainButtonName" -command {set ok 1}
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
	    set DEBUGFILE [tk_getSaveFile -filetypes $types -initialfile "$DefaultSaveName" -parent . -defaultextension "$DefaultExt"]
      if {$DEBUGFILE != ""} {
        puts "Opening File: $DEBUGFILE"
        set channel [prob_open "$DEBUGFILE" "w+"]
        foreach i $Result {
           puts -nonewline $channel [format %c $i]
        }
        close $channel
      }
	} elseif {$ok==4} {
	   eval $ExtraButtonCmd
	   # Note: this command can access the text using [.showtext.frmSource.text get 1.0 end]
	   procShowErrors
	}

	Dialog_Dismiss $f
}

proc procShowTLCLogWindow {Msg1 Msg2} {
	destroy .tlcConsoleWindow
	# destroy added for Mac TclTk
  set f .tlcConsoleWindow
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
		button $b.ok -text Done -command {destroy .tlcConsoleWindow}
		pack $b.ok -side right

		bind $f.frmSource <Return> {destroy .tlcConsoleWindow}
		bind $f.frmSource <Control-c> {destroy .tlcConsoleWindow}
	}


    $f.frmSource.text configure -state normal
    $f.frmSource.text delete 0.0 end

    # $f.frmSource.text configure -state disabled

 #    set ok 0
	# Dialog_Wait $f ok $f.frmSource
	# Dialog_Dismiss $f
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
proc procShowAllSpecializedInvariants {} {
	   if [prolog bmachine:tcltk_get_specialized_invariants_for_ops(X)] {
		   procShowList2 $prolog_variables(X) "Specialized Invariants" "Specialized Invariants" 1 0 "SpecializedInvariants.mch" none
	   }
	   procShowErrors
}
proc procShowAllOpGuards {} {
	   if [prolog b_read_write_info:tcltk_get_guards_for_ops(X)] {
		   procShowList2 $prolog_variables(X) "Operation Guards" "Operation Guards used for Enabling Analysis" 1 0 "OperationGuards.mch" none
	   }
	   procShowErrors
}
proc procShowAllOpParamFreeGuards {} {
	   if [prolog b_read_write_info:tcltk_get_parameter_free_guards_for_ops(X)] {
		   procShowList2 $prolog_variables(X) "Operation Splitting into Parameter-Free Guard and Action" "Operation Parameter-Free Guards (used for LTSMin) and Actions (actions used for Enabling Analysis)" 1 0 "OperationParameterFreeGuards.mch" none
	   }
	   procShowErrors
}
proc procShowSpecializedInvariants {} {
	   global OpName
	   set OpName [procChooseOperation]
	   if {$OpName != ""} {
		   if [prolog bmachine:tcltk_get_specialized_invariant_for_op('$OpName',X)] {
		     puts "res = $prolog_variables(X)"
			   procShowList2 $prolog_variables(X) "Specialized Invariant for $OpName" "Specialized Invariant for $OpName" 1 0 "SpecializedInvariant_$OpName.mch" none
		   }
		   procShowErrors
       }
}

proc procCBCInvariantBACheck {} {
	  if [prolog tcltk_cbc_symbolic_invariant_violation(X)] {
			   procShowTable $prolog_variables(X) "Inductive Invariant Check" "Inductive Invariant Check" "InductiveInvariantCheck" "" ""
	  }
	  procShowErrors
}

proc procShowBAPredicate {} {
	   global OpName
	   set OpName [procChooseOperation]
	   if {$OpName != ""} {
		   if [prolog before_after_predicates:tcltk_before_after_predicate_for_operation('$OpName',X)] {
		       procShowErrors
			   procShowList2 $prolog_variables(X) "Before-After Predicate for $OpName" "Before-After Predicate for $OpName" 1 0 "BeforeAfterPred_$OpName.mch" none
		   }
		   procShowErrors
       }
}

proc procShowOpCacheInfo {} {
  global spec_desc
  if [prolog get_preference(try_operation_reuse,false)] {
       tkErrorBoxNoParent "Operation caching disabled. Turn it on using the OPERATION_REUSE preference."
  } else {
       prolog b_operation_cache:tcltk_op_cache_stats(T)
		   set res $prolog_variables(T)
		   procShowTable $res "ProB Operation Cache Info" "Cached $spec_desc(operation) Calls" "OpCacheInfo.txt" "" ""
  }
}

proc procShowCacheInfo {} {
  if [prolog value_persistance:tcltk_load_constants(ConstantStores)] {
		   set res $prolog_variables(ConstantStores)
		   procShowList2 $res "Cached Constant Values" "Cached Constant Values" 1 0 "CachedValues.txt" none
  } else {
       tkErrorBoxNoParent "Caching disabled or no cache info available. Enable caching by providing -cache DIRECTORY to ProB."
  }
}

proc procShowProBProfileInfo {} {
  global spec_desc
  prolog runtime_profiler:tcltk_get_profile_info(Info)
  set res $prolog_variables(Info)
  procShowTable $res "ProB Profile Info" "$spec_desc(operation) Computation Runtimes" "ProBProfile" "" ""
}



# -------
# procedure to search for invariant violations using open search
# -------
global last_queue_size last_ms
proc initInfos {f} {
	 global model_check_start_time_sec
	 set model_check_start_time_sec [clock seconds]
   global last_queue_size last_ms
   set last_ms -1
   set last_queue_size -1
   updateInfos $f
}


proc updateInfos {f} {
	global mcFindDeadlocks mcFindInvViolations mcSearchForNewErrors doInspectExistingNodes
	global mcFindGOAL mcFindAssViolations mcFindStateErrors
  prolog get_state_space_stats(DistinctStates,T,Processed,Ignored)
	set ds [expr ($prolog_variables(DistinctStates)-1)-$prolog_variables(Ignored)]
	set ps [expr $prolog_variables(Processed)-1]
	if {$ps > 0} {
	    if {$ps == $ds} {
	      if [prolog model_checking_is_incomplete($mcFindInvViolations,$mcFindDeadlocks,$mcFindGOAL,$mcFindAssViolations,Msg,Term)] {
             $f.infos.f0.cstates configure -text "$prolog_variables(Processed) (100 % **)"
	      } else {
              $f.infos.f0.cstates configure -text "$prolog_variables(Processed) (100 %)"
	      }
	    } else {
	      set perc [expr (100*$ps)/$ds]
        $f.infos.f0.cstates configure -text "$prolog_variables(Processed) ($perc %)"
        }
    } else {
        $f.infos.f0.cstates configure -text "$prolog_variables(Processed)"
    }
    $f.infos.f1.states configure -text "$prolog_variables(DistinctStates)"
    $f.infos.f2.transitions configure -text "$prolog_variables(T)"
	  global expert_user tcl_version
	  if {$tcl_version >= 8.5} {
      # $f.pb1 configure -maximum $ds
      # $f.pb1 configure -value $ps
      # we now use a heuristic formula which seems to work better
      $f.pb1 configure -maximum 200
      $f.pb1 configure -value [expr 200*((($ps+0.0)/$ds)**6)]

      global last_queue_size last_ms model_check_start_time_sec
      set delta [expr [clock seconds] - $model_check_start_time_sec]
      prolog statistics(memory_used,Mem)
      set mb [expr $prolog_variables(Mem) / 1000000]
      # use instead of the deprecated 1048576
      $f.msg2 configure -text "Checking ($delta sec, $mb MB)... "
      # comment in to observe model checking queue statistics in CSV format:
      # puts "$ps,$ds,$delta,$prolog_variables(T),$mb,[expr 200*((($ps+0.0)/$ds)**6)]"
      update idletasks
    }
}

proc procModelCheckWithTLCCommand {} {
	global strFilename lib_dir
	global mcTLCFindDeadlocks mcTLCFindInvViolations mcTLCFindAssertionViolations mcTLCFindGoal mcTLCCheckLTLFormulas mcTLCCheckWorkers mcTLCSetupConstants mcTLCPartialInvEvaluation
    global TLCGeneratedStates mcTLCFindDeadlocks mcTLCSymm
    global tlc_worker TLCPipeID TLCChanID
    global tlc_array
    array set tlc_array {
        gstates "-"
        dstates "-"
        qstates "-"
        result "No information produced (yet)."
        output ""
        color "black"
        mctime "-"
    }

	if ![string is integer -strict $mcTLCCheckWorkers] {
			 tkErrorBox "Invalid number of workers: $mcTLCCheckWorkers. Please enter a positive number."
	} elseif {$mcTLCCheckWorkers < 1} {
			 tkErrorBox "Invalid number of workers: $mcTLCCheckWorkers. Please enter a positive number."
	} elseif {$strFilename != "" && [prolog animation_mode(b)]} {
    .searchTLC.infos.consoleOutput configure -state normal
		.searchTLC.infos.result configure -foreground black
		.searchTLC.infos.result configure -text "Model Checking started ..."
		.searchTLC.infos.time configure -text "-"
    .searchTLC.buttons.ok configure -state disabled
		update
		# set FileName [file rootname $strFilename]
		# set resultFile "$FileName.tla.temp"
		#set MacJava6 "/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Commands/java"
		#if [file exists $MacJava6] {set MyJava $MacJava6} else {set MyJava "java"}
    set MyJava "java"
    # TO DO: parsercall:get_jvm_options(L) or get_preference(jvm_parser_heap_size_mb,Heap)
	  set TLC4BCommand [list $MyJava -Xmx3G -Xss20m -jar $lib_dir/TLC4B.jar]
		foreach option [list $mcTLCFindDeadlocks $mcTLCFindInvViolations $mcTLCFindAssertionViolations $mcTLCFindGoal $mcTLCCheckLTLFormulas] \
			arg {-nodead -noinv -noass -nogoal -noltl} {
				if {$option == 0} {
					lappend TLC4BCommand $arg
				}
		}

    if $mcTLCPartialInvEvaluation {lappend TLC4BCommand -parinveval}
    if $mcTLCSymm {lappend TLC4BCommand -symmetry}
    if $mcTLCSetupConstants {
      prolog get_preference(maxNrOfEnablingsPerOperation,MO)
      prolog tcltk_get_constants_predicate(P,$prolog_variables(MO))
      set Result [join [split $prolog_variables(P) "\n"]]
      lappend TLC4BCommand [join "-constantssetup "]
      lappend TLC4BCommand $Result
    }
		if {$mcTLCCheckWorkers != ""} {
		    puts "Parallel Execution: Number of Workers = $mcTLCCheckWorkers"
        lappend TLC4BCommand -workers $mcTLCCheckWorkers
		    prolog preferences:set_preference(tlc_number_of_workers,$mcTLCCheckWorkers)
		}
    prolog preferences:get_preference(minint,MININT)
    prolog preferences:get_preference(maxint,MAXINT)
    lappend TLC4BCommand -minint $prolog_variables(MININT) -maxint $prolog_variables(MAXINT)

    # if the input file is an Event-B machine translate it to a B machine first and
    # then start the TLC model checker with the translated machine
    set TLCInputFilename ""
    if { [prolog "specfile:animation_minor_mode(eventb)"] } {
      if [regexp {(.*)\.eventb} $strFilename match FName] {
        append TLCInputFilename $FName "_eventb" ".mch"
        set tlc_array(result) "Translating Event-B model to Classical B"
        prolog bmachine:b_write_eventb_machine_to_classicalb_to_file('$TLCInputFilename')
        } else {
          tkErrorBox "File $strFilename does not have the file extention .eventb."
        }
    } else {
      set TLCInputFilename $strFilename
    }
    set FileName [file rootname $TLCInputFilename]
    set traceFile "$FileName.tla.trace"
    if [file exists $traceFile] { file delete $traceFile }

		lappend TLC4BCommand $TLCInputFilename;# > $resultFile
		puts "******** Executing TLC4BCommand: $TLC4BCommand ********"

        package require Thread

        set tlc_worker [thread::create {

            proc openpipe {COMMAND} {
                if { [catch {open "| $COMMAND 2>@stdout" w+} FILEHANDLE] } {
                   return "Can't open pipe for '$COMMAND'"
                } else {
                    puts "FILEHANDLE $FILEHANDLE"
                    set PID [pid $FILEHANDLE]
                    puts "PID $PID"
                    return [list $FILEHANDLE $PID]
                }
            }

            proc execpipe {PIPE ID} {
               puts "PIPE $PIPE"

               fconfigure $PIPE -buffering none

               set OUTPUT ""

               while { [gets $PIPE DATA] >= 0 } {
                  puts $DATA
                  thread::send -async $ID [list procUpdateTLCModelCheckerDialogOnTheFly $DATA]
                  append OUTPUT $DATA "\n"
               }

               if { [catch {close $PIPE} ERRORMSG] } {
                  if { [string compare "$ERRORMSG" "child process exited abnormally"] == [equal] } {
                     # this error means there was nothing on stderr (which makes sense) and
                     # there was a non-zero exit code - this is OK as we intentionally send
                     # stderr to stdout, so we just do nothing here (and return the output)
                  } else {
                     return "Error '$ERRORMSG' on closing pipe for '$COMMAND'"
                  }
               }

               regsub -all -- "\n$" $OUTPUT "" STRIPPED_STRING
               return "$STRIPPED_STRING"
            }

            proc procGetTLCResult {content} {
                set TLCModelCheckResult "No Result Produced."
                set color black
                set resultFound false
                set statesResult false
                foreach var {states_generated distinct_states states_on_queue} {
            	    eval {set $var "-"}
                }
                set lines [split $content \n]
                foreach line $lines {
            	if {!$resultFound} {
                    if [regexp {^Result:\s*(.*)$} $line all result] {
               		    set TLCModelCheckResult $result
            		    set color black
            		    set resultFound true
            			} elseif [regexp {^Model\schecking\stime:\s*(.*)$} $line all result] {
            				set ModelCheckingTime $result
            		}
            	}
            	if {!$statesResult && ![regexp {^Progress.*$} $line all] && [regexp {^>\s*([0-9]+)\s*(states|state)\s*generated.*$}\
            		$line all states_generated]} {
            		# get states information
            		regexp {.*\s([0-9]+)\s*distinct\s*(states|state)\s*found.*} $line all distinct_states
            		regexp {.*\s([0-9]+)\s*(states|state)\s*left\s*on\s*queue.*} $line all states_on_queue
            		set statesResult true
            	}
                }
                return [list gstates $states_generated dstates $distinct_states qstates $states_on_queue\
                             result  "$TLCModelCheckResult" output $content color $color mctime $ModelCheckingTime]
                #return [list $TLCModelCheckResult $color $states_generated $distinct_states $states_on_queue $ModelCheckingTime]
            }

            proc procExecuteCmdGetResult {COMMAND MainThreadID traceFile} {
                set result ""
                if {[catch {set output [execpipe $COMMAND $MainThreadID]} err]} {
                    set result "Aborted"
                    set output "TLC Check has been aborted and no console output could be produced."
                } else {
                    set result [procGetTLCResult $output]
                }
                thread::send -async $MainThreadID [list procUpdateTLCModelCheckerDialog $result $output $traceFile]
                thread::release
            }

            thread::wait
            }]

            set commandOutput ""
            thread::send $tlc_worker [list openpipe $TLC4BCommand] ChanPIPEIDs
            set TLCChanID [lindex $ChanPIPEIDs 0]
            set TLCPipeID [lindex $ChanPIPEIDs 1]

            set MainID [thread::id]
            puts "MainID: $MainID"
            puts "tlc_worker: $tlc_worker"
            thread::send -async $tlc_worker [list procExecuteCmdGetResult $TLCChanID $MainID $traceFile]

            .searchTLC.buttons.cancel configure -text "Stop"
            .searchTLC.buttons.cancel configure -command {procTLCTerminateProcess $TLCPipeID}

	} else {
		tkErrorBox "No B machine open. Cannot Start TLC Model Checker."
	}
}

proc procUpdateTLCModelCheckerDialogOnTheFly {DATA} {
  global tlc_array
  if [regexp {^>\s*Progress\(([0-9]+)\).*:\s([0-9]+).*,\s([0-9]+).*,\s([0-9]+).*} $DATA match prgr gst dst qst] {
       array set tlc_array [list gstates $gst dstates $dst qstates $qst result "Progress($prgr)..."]
  } elseif [regexp {^>\s*(Parsing|Semantic\sprocessing|Computing\sinitial\sstates).*} $DATA match status] {
       array set tlc_array [list result "$status..."]
  }

  array set tlc_array [list output "$tlc_array(output)$DATA\n"]

  if [winfo exists .tlcConsoleWindow] {
     .tlcConsoleWindow.frmSource.text configure -state normal
     .tlcConsoleWindow.frmSource.text insert end "$DATA\n"
     .tlcConsoleWindow.frmSource.text see end
     .tlcConsoleWindow.frmSource.text configure -state disabled
  }
}

proc procUpdateTLCModelCheckerDialog {result commandOutput traceFile} {
    global tlc_array
    if {$result == "Aborted"} {
        array set tlc_array [list result "Aborted"]
        .searchTLC.infos.result configure -foreground red
        set tlc_array(output) "$tlc_array(output)Aborted\n"

        #.searchTLC.infos.consoleOutput configure -state normal
        #.searchTLC.infos.consoleOutput configure -command [list procShowTLCOutput $commandOutput]
    } else {
        array set tlc_array $result
        #tkMessageBox "TLC Model Checking has been finished."
        .searchTLC.buttons.ok configure -state normal
        .searchTLC.buttons.cancel configure -text "Cancel"
        .searchTLC.buttons.cancel configure -command {destroy .searchTLC}

        if [file exists $traceFile] {
            .searchTLC.infos.result configure -foreground $tlc_array(color)
            update
            prologmnf tcltk_reset
            # replay from beginning
            procCheckTraceFileInternal $traceFile "state_sequence" "backtracking_trace_replay" 0 0
        }
    }
    if [winfo exists .tlcConsoleWindow] {
     .tlcConsoleWindow.frmSource.text configure -state normal
   }
}

proc procTLCTerminateProcess {TLCPipeID} {
    if {[.searchTLC.buttons.cancel cget -text] == "Stop"} {
        puts "TLCPipeID $TLCPipeID"
        catch {exec kill $TLCPipeID} res
        #puts "res: $res"
        #thread::release $tlc_worker
        .searchTLC.buttons.cancel configure -text "Cancel"
        .searchTLC.buttons.ok configure -state normal
        if [winfo exists .tlcConsoleWindow] {
           .tlcConsoleWindow.frmSource.text configure -state normal
        }
    } else {
        destroy .searchTLC
    }
}

proc procShowTLCOutput {} {
   global tlc_array
   procShowTLCLogWindow "TLC Model Checker\'s Output" "TLC Model Checker\'s Output"
   .tlcConsoleWindow.frmSource.text insert 0.0 $tlc_array(output)
   .tlcConsoleWindow.frmSource.text see end
}

proc procInitTLC {} {
    global mcTLCCheckWorkers
		prolog get_preference(tlc_number_of_workers,WRK)
		set mcTLCCheckWorkers $prolog_variables(WRK)
}

proc procModelCheckWithTLC {} {
    if [procCheckBMode "Model Check with TLC"] {
      procModelCheckWithTLC2
    }
}
proc procModelCheckWithTLC2 {} {
	destroy .searchTLC
	set tlc .searchTLC
	global mcTLCFindDeadlocks mcTLCFindInvViolations mcTLCFindAssertionViolations mcTLCFindGoal mcTLCCheckWorkers mcTLCSetupConstants TLCGeneratedStates mcTLCPartialInvEvaluation
	global mcTLCSymm
  global tlc_wait tlc_array
	if [Dialog_Create $tlc "Model Check with TLC:" -borderwidth 22] {
		wm resizable $tlc 0 0
		set b [frame $tlc.buttons]
		set op [frame $tlc.options]
		set infos [frame $tlc.infos]
		pack $b $op $infos -side top -fill x
        label $infos.result -textvariable tlc_array(result)
        label $infos.time -textvariable tlc_array(mctime)
		button $infos.consoleOutput -text "Console Output" -pady 2 -state disabled -command {procShowTLCOutput}

		button $b.ok -text "Model Check" -pady 3 -command {procModelCheckWithTLCCommand}
		button $b.cancel -text Cancel    -pady 3 -command {destroy .searchTLC}
		pack $b.ok -side left -padx 5
		pack $b.cancel -side right -padx 5

		checkbutton $op.deadlocks -text "Find Deadlocks" -pady 2 -variable mcTLCFindDeadlocks
		if {[prolog tcltk_machine_has_assertions]} {
		   checkbutton $op.assertions -text "Find Assertion Violations" -pady 2 -variable mcTLCFindAssertionViolations
		} else {
		   checkbutton $op.assertions -text "Find Assertion Violations" -pady 2 -variable mcTLCFindAssertionViolations -state disabled
		}
		checkbutton $op.invariants -text "Find Invariant Violations" -pady 2 -variable mcTLCFindInvViolations \
              -command {setSubOptionStates .searchTLC.options.inv_lbl $mcTLCFindInvViolations [list partial_inv] normal}
        label $op.inv_lbl
		checkbutton $op.inv_lbl.partial_inv -text "+ Proof Guided MC" -pady 2 -variable mcTLCPartialInvEvaluation
		if {[prolog bmachine:b_get_machine_goal(_)]} {
		   checkbutton $op.findGOAL -text "Find GOAL" -pady 2 -variable mcTLCFindGoal
		} else {
		   checkbutton $op.findGOAL -text "Find GOAL (from DEFINITIONS)" -pady 2 -variable mcTLCFindGoal -state disabled
		}
    checkbutton $op.useSymm -text "Use Symmetry" -pady 2 -variable mcTLCSymm -state normal
		if [procMchSetupConstantsTLC] {
            checkbutton $op.setupconstants -text "Setup Constants using ProB" -pady 2 -variable mcTLCSetupConstants -onvalue 1 -offvalue 0
		} else {
            checkbutton $op.setupconstants -text "Setup Constants using ProB" -pady 2 -variable mcTLCSetupConstants -onvalue 1 -offvalue 0 -state disabled
		}
		checkbutton $op.ltlformulas -text "Check LTL Formulas" -pady 2 -variable mcTLCCheckLTLFormulas -onvalue 1 -offvalue 0

		pack $op.deadlocks $op.assertions $op.invariants $op.inv_lbl $op.findGOAL $op.useSymm $op.setupconstants $op.ltlformulas -side top -fill x
        pack $op.inv_lbl.partial_inv -padx 10

		frame $op.workers
		label $op.workers.workerlabel -text "Number of Workers: "
		entry $op.workers.numberworkers -textvariable mcTLCCheckWorkers -width 4
		pack  $op.workers.workerlabel $op.workers.numberworkers -side left
		pack  $op.workers -side top
        bind $op.workers.numberworkers <Return> {procModelCheckWithTLCCommand}


		set infos0 [frame $infos.f0]
		set infos1 [frame $infos.f1]
		set infos2 [frame $infos.f2]

        label $infos0.l0 -text "Generated States: "
        label $infos0.gstates -textvariable tlc_array(gstates)
        label $infos1.l1 -text "Distinct States: "
        label $infos1.dstates -textvariable tlc_array(dstates)
	    label $infos2.l2 -text "States On Queue: "
		label $infos2.qstates -textvariable tlc_array(qstates)

	    pack $infos0 $infos1 $infos2 -side top

	    pack $infos0.l0 $infos0.gstates -side left
	    pack $infos1.l1 $infos1.dstates -side left
	    pack $infos2.l2 $infos2.qstates -side left

		pack $infos.time $infos.result -side bottom -fill x
		pack $infos.consoleOutput -side bottom -fill x


        # set tlc_wait 0
        # Dialog_Wait_Prepare $tlc tlc_wait $tlc.buttons
        # tkwait variable tlc_wait
	}
    # Dialog_Wait_Finish $tlc
    # Dialog_Dismiss $tlc
    # procShowErrors
}

proc procMchSetupConstantsTLC {} {
    prolog get_preference(maxNrOfEnablingsPerOperation,MO)
    prolog tcltk_get_constants_predicate(P,$prolog_variables(MO))
    if {"1=1" == $prolog_variables(P)} {
        # empty state, no constants to be set up
        return 0
    } else {
        return 1
    }
}

proc procModelCheck {} {
    if [procSpecFileSuccessfullyLoaded] {
        procModelCheck2
    } else {
       tkErrorBox "No specification file loaded. Cannot run model checker."
    }
}
proc procModelCheck2 {} {
	global mc_prompt Depth count done spec_desc
	destroy .search
	global expert_user normal_user
	global model_checker_arr PORTechniqueVar
	# destroy re-added for Mac TclTk
	set f .search
    global mcPerformBreadthFirst mcFindDeadlocks mcFindInvViolations mcSearchForNewErrors
	global mcFindAssViolations mcFindStateErrors mcFindGOAL mcStopWhenFullCoverage mcPartialOrderReduction
	if [Dialog_Create $f "Model Check:" -borderwidth 10] {
		wm resizable $f 0 0
		# message $f.msg -text "Enter max. nr of nodes to check:" -aspect 1000
		# entry $f.entry -textvariable mc_prompt(result)
		label $f.msg2 -text "Waiting"
		set b [frame $f.buttons]
		set op [frame $f.options]
		set infos [frame $f.infos]
		# pack $f.msg $f.entry $f.buttons $f.options $f.msg2 $f.progress -side top -fill x
		pack $b $op $infos $f.msg2 -side top -fill x
        global expert_user
		# pack $f.entry -pady 5
		set infos0 [frame $infos.f0]
		set infos1 [frame $infos.f1]
		set infos2 [frame $infos.f2]
        label $infos0.l0 -text "Processed States: "
        label $infos0.cstates -text "-"
        label $infos1.l1 -text "Total Distinct States: "
        label $infos1.states -text "-"
        label $infos2.l2 -text "Total Transitions: "
        label $infos2.transitions -text "-"
        pack $infos0 $infos1 $infos2 -side top
        pack $infos0.l0 $infos0.cstates -side left
        pack $infos1.l1 $infos1.states -side left
        pack $infos2.l2 $infos2.transitions -side left
        global tcl_version
        if {$tcl_version >= 8.5} {
          ttk::progressbar $f.pb1 -orient horizontal -mode determinate -maximum 100 -length 200
          pack $f.pb1 -side top
        }
        initInfos $f

		button $b.ok -text "Model Check" -command {set mc_prompt(ok) 1}
		button $b.cancel -text Cancel -command {set mc_prompt(ok) 0}
		pack $b.ok -side left
		pack $b.cancel -side right
		bind $f <Return> {set mc_prompt(ok) 1 ; break}
		bind $f <Control-c> {set mc_prompt(ok) 0 ; break}

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

		#checkbutton $op.bf -text "Breadth First" -variable mcPerformBreadthFirst -offvalue 0 -onvalue 1
		checkbutton $op.deadlocks -text "Find Deadlocks" -variable mcFindDeadlocks
		if {[prolog preferences:get_preference(do_invariant_checking,false)]} {
		     set mcFindInvViolations 0
		}
		checkbutton $op.invariants -text "Find Invariant Violations" -variable mcFindInvViolations

		if {$normal_user == 1} {
			checkbutton $op.reduction -text "Partial Order Reduction" -variable model_checker_arr(use_por) -offvalue false -onvalue true -state normal\
            -command {if {$model_checker_arr(use_por)} {set model_checker_arr(por) "ample_sets"; } {set model_checker_arr(por) "off"}; procSetModelCheckerPreferences }
		}
	   if {$expert_user == 1} {
			checkbutton $op.partial_ge -text "Partial Guards Evaluation" -variable model_checker_arr(use_pge) -onvalue true -offvalue false -state normal
			}

        global curFileTypeOpened cspstrFilename
      if {[prolog bmachine:b_get_machine_goal(_)]} {
         checkbutton $op.findGOAL -text "Find Defined GOAL" -variable mcFindGOAL
      } elseif {$curFileTypeOpened == "CSP" || $cspstrFilename != ""} {
         checkbutton $op.findGOAL -text "Find Event on \"goal\" CSP Channel" -variable mcFindGOAL
      } else {
         checkbutton $op.findGOAL -text "Find GOAL (from DEFINITIONS)" -variable mcFindGOAL -state disabled
      }
      set ASS [AssertionName]
	     if [prolog tcltk_machine_has_assertions] {
            set assertionState normal
        } else {
            set assertionState disabled
        }
        checkbutton $op.assertions -text "Find B $ASS Violations" -variable mcFindAssViolations -state $assertionState
        checkbutton $op.stateerrors -text "Find Other Errors" -variable mcFindStateErrors -state normal

	    if {[prolog operation_name_not_yet_covered(_)]} {
		     checkbutton $op.stopcoverage -text "Stop when All $spec_desc(operations) Covered" -variable mcStopWhenFullCoverage
		} else {
		     set mcStopWhenFullCoverage 0
		     checkbutton $op.stopcoverage -text "Stop when All $spec_desc(operations) Covered" -variable mcStopWhenFullCoverage -state disabled
		}
		if {![prolog tcltk_exists_an_open_node]} {
		   set mcSearchForNewErrors 0
		}
		checkbutton $op.already -text "Search for New Errors" -variable mcSearchForNewErrors
		if {$expert_user == 1} {
			pack $op.deadlocks $op.invariants $op.assertions $op.stateerrors $op.findGOAL $op.stopcoverage $op.stateerrors $op.already $op.reduction $op.partial_ge -side top -fill x
		} else {
			pack $op.deadlocks $op.invariants $op.assertions $op.stateerrors $op.findGOAL $op.stopcoverage $op.already -side top -fill x
		}
	} else {
	    wm title $f "Model Check:"
	    # $f.msg configure -text "Enter max. nr of nodes to check:" -aspect 1000
	    set b $f.buttons
	}
    $f.msg2 configure -text ""
    global batch_mode
    set done 0
    set mc_prompt(ok) 0
    $b.ok configure -text "Model Check"
    $b.ok configure -state normal

    Dialog_Wait_Prepare $f mc_prompt(ok) $f.buttons

    while {$done != 1} {
	  $b.ok configure -state normal

      if {$batch_mode} {
        set mc_prompt(ok) 1
      } else {
        set mc_prompt(ok) 0
        tkwait variable mc_prompt(ok)
      }

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
	  if {$mc_prompt(ok) && $doInspectExistingNodes} {
		    # puts "Searching existing nodes"
		    if [prolog tcltk_search_among_existing_nodes(ErrRes,$mcFindDeadlocks,$mcFindInvViolations,$mcFindGOAL,$mcFindAssViolations,$mcFindStateErrors)] {
				# puts "finished"
				set Result $prolog_variables(ErrRes)
				if {$Result == "all"} {
				  $f.msg2 configure -text "No error among existing nodes"
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
			  $f.msg2 configure -text "Internal error occurred"
			  set done 0
	          tkErrorBox "error occurred !!"
	        }
	    }
	   # .search.progress stop
	    if {$mc_prompt(ok) && ($done != 1)} {
		  # puts "Searching new nodes"
		  # set Depth $mc_prompt(result)
		  prolog tcltk_set_dbf_mode($mcPerformBreadthFirst)
		  $f.msg2 configure -text "Checking... "
		  global model_check_start_time_sec
		  set model_check_start_time_sec [clock seconds]
		  set mc_prompt(ok) 1
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
		  global profilingMode
		  if {$profilingMode} { prolog "set_prolog_flag(profiling,on)" }
		  model_checking_search_loop
		  if {$profilingMode} { prolog "nl,print('PROFILE'),set_prolog_flag(profiling,off), nl,print_profile,nl" }
		  if {$profilingMode} { prolog "use_module(library(gauge)),view" }

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
	procUpdateStatusFrame
}



proc model_checking_search_loop {} {
	global mc_prompt spec_desc
	global count done stepval found_or_all_visited
	global mcFindDeadlocks mcFindInvViolations mcSearchForNewErrors doInspectExistingNodes mcFindGOAL mcFindAssViolations mcFindStateErrors
	global searchResult

    set sst "Total time for model checking step: [time {model_checking_search_step} 1]"
    prolog "statistics(memory_used,MEM)"
    set MB [expr $prolog_variables(MEM)/1048576]
	  prolog "tools:print_message('$sst (used memory: $MB MBytes)')"
    updateInfos .search
     if {$found_or_all_visited} {
	    update
        if {$searchResult == "all"} {
            if [prolog tcltk_find_max_reached_node(_)] {
              prolog get_preference(maxNrOfInitialisations,MI)
              prolog get_preference(maxNrOfEnablingsPerOperation,MO)
              set msg "\nNote: not all transitions were computed (increase MAX_INITIALISATIONS and MAX_OPERATIONS in Animation Preferences, current values $prolog_variables(MI) and $prolog_variables(MO))."
            } else {
              set msg ""
            }
            if [prolog state_space:operation_name_not_yet_covered(R)] {
                set msg "\nNote: some $spec_desc(operations_lc) are not covered (e.g., $prolog_variables(R)).$msg"
            }
            if [prolog model_checker:proof_info_model_checking_incomplete($mcFindInvViolations,$mcFindDeadlocks,$mcFindGOAL,$mcFindAssViolations,Msg)] {
                set msg "\nNote: Using PROOF information is potentially incomplete when [AssertionName] violation checking is turned off.$msg"
            }
            if {$mcFindInvViolations && [prolog time_out_for_invariant(SomeID)]} {
                set msg "\nBut TIMEOUT occured during invariant checking (e.g., state id $prolog_variables(SomeID)) !!$msg"
            }
            # we could use more simply model_checking_is_incomplete to get all messages
            if [prolog tcltk_hash_model_checking_imprecise] {
                set msg "\nNote: Hash markers could be imprecise for this $spec_desc(machine_lc).$msg"
            }
            prolog get_state_space_stats(States,Trans,ProcessedNodes,IgnoredNodes)
            if {$prolog_variables(IgnoredNodes) != 0} {
                set msg "\nNote: $prolog_variables(IgnoredNodes) states were ignored (not satisfying SCOPE).$msg"
            }
            append msg [getUnboundedWarnings]
            update
            if {$doInspectExistingNodes} {
                  if [prolog state_error_exists] {
                     # mcFindStateErrors is off
                     set msg "\nNote: some states do contain errors.\nTurn on 'Find Other Errors' and re-run the model checker to find them.$msg"
                  }
                  tkMessageBoxNoParent "No error state found, ALL $prolog_variables(States) states visited.$msg"
            } else {
                  if [prolog state_error_exists] {
                     set msg "\nNote: some previously explored states do contain errors.\nTurn off 'Search for New Errors' and re-run the model checker to find them.$msg"
                  }
                  tkMessageBoxNoParent "No error state found, all NEW states visited during this model checking run.$msg"
            }
        } else {
          # searchResult not equal to all
              if {$searchResult == "goal_found"} {
                 tkMessageBoxNoParent "State satisfying GOAL predicate was found."
               } elseif {$searchResult == "full_coverage"} {
                 tkMessageBoxNoParent "All $spec_desc(operations_lc) are now covered by at least one transition.\nUse Analyse -> Coverage menu to obtain more details."
              } else {
                 tkErrorBoxNoParent "Error state was found for new state:  $searchResult"
              }
              procInsertHistoryOptionsState
	        }
     } else {
      # puts "finished model_checking_search_loop"
        if {!($done)} {
         set done 2
         if {$mc_prompt(ok)} {
         .search.msg2 configure -text "No error so far, $count states visited"
         } else {
        .search.msg2 configure -text "User stopped after $count"
         }
      }}
      return

}


proc model_checking_search_step {} {
	global mc_prompt
	global count done stepval found_or_all_visited
	global mcFindDeadlocks mcFindInvViolations mcSearchForNewErrors mcFindGOAL
	global mcFindAssViolations mcFindStateErrors mcStopWhenFullCoverage mcPartialOrderReduction
	global searchResult
        global model_checker_arr

	set found_or_all_visited 0
	.search.buttons.cancel configure -text "Stop"
    while {($mc_prompt(ok)) && (!$found_or_all_visited)} {
      #.search.msg2 configure -text "Checking... $count"
      updateInfos .search
      # .search.progress step $stepval
      update
      set count [expr $count+$stepval]
      # puts $count

      set mcPartialOrderReduction [string map {" " "_"} $model_checker_arr(por)]

      prolog tcltk_model_check($stepval,ErrRes,$mcFindDeadlocks,$mcFindInvViolations,$mcFindGOAL,$mcFindAssViolations,$mcFindStateErrors,$mcStopWhenFullCoverage,$mcPartialOrderReduction,$model_checker_arr(pge),500,_)

       set searchResult $prolog_variables(ErrRes)
       set ll [llength $searchResult]

       if {$ll==2} {
          # timeout occurred
          set count [expr $count-$stepval+[lindex $searchResult end]]
       } elseif {$searchResult != "no"} {
	        # force exit of loop
	        set found_or_all_visited 1
	        set done 1
            updateInfos .search
            .search.msg2 configure -text "Checking Finished"
	   } else {
	      set stepval [expr $stepval+50]
	       # increase stepval; reduced overhead of GUI
	   }
    }
	.search.buttons.cancel configure -text "Cancel"
}



proc Dialog_Wait_Prepare {top varName {focus {}}} {
	upvar $varName var

	# Poke the variable if the user nukes the window
	bind $top <Destroy> [list set $varName $var]

	# Grab focus for the dialog
	if {[string length $focus] == 0} {
		set focus $top
	}
    global dw_old
	set dw_old [focus -displayof $top]
	focus $focus
	catch {tkwait visibility $top}
	catch {grab $top}
}

proc Dialog_Wait_Finish {top} {
    global dw_old
	catch {grab release $top}
	focus $dw_old
}

# -------
global last_type
set last_type none
proc set_prompt_max_depth_default {new_depth new_type} {
  global prompt last_type
  if {$last_type != $new_type} {
    set prompt(sap_max_depth) $new_depth
    set last_type $new_type
  }
  # otherwise: keep the value the user has typed last time
}
global sap_checkbutton1
set sap_checkbutton1 0
proc procCBCTestcases {} {
    global prompt
    global mc_prompt sap_checkbutton1
    set finished 0
    prologmnf sap:tcl_get_events_preselected(Events,Presel)
    set_prompt_max_depth_default 5 "procCBCTestcases"
    set prompt(sap_eventsel) $prolog_variables(Presel)
    set default_xml_name ""
    if { [catch {set $prompt(sap_filename)}] } {set prompt(sap_filename) "$default_xml_name"}
    while {!$finished} {
        set chosen [SAPDialog "Constraint-Based Testcase Generation" $prolog_variables(Events) "covered" 0 1 1 1 0 "Target events must be final only"]
        if {[llength $chosen] > 0} {
            set clist [join $chosen ","]
            set target [escapeChars $prompt(sap_target_predicate)]
            set maxdepth $prompt(sap_max_depth)
            set filename [escapeChars $prompt(sap_filename)]
            if [string is integer -strict $maxdepth] {
                procRunCBCTests "CBC Main Test Cases" "\[$clist\]" $target $maxdepth $filename $sap_checkbutton1
				set finished 1
            } else {
                tkMessageBox "maximum depth is not a number"
            }
        } else {
            # tkMessageBox "No events selected."
            set finished 1
        }
    }
}

proc procBoundedModelChecking {maxdepth} {
   procRunCBCTests "Bounded Model Checking" "all" "#not_invariant" $maxdepth "" 0
   # TO DO: check if model checking complete + automatically execute first test case + stop after that
}
proc procBoundedDeadlockChecking {maxdepth} {
   procRunCBCTests "Bounded Model Checking" "all" "#deadlock" $maxdepth "" 0
   # TO DO: check if model checking complete + automatically execute first test case + stop after that
}

proc procModelCheckWithLTSmin {symorseq nodead noinv usepor} {
   puts "Starting LTSMin Server"
   if [prolog tcltk_run_ltsmin($symorseq,$nodead,$noinv,$usepor,Res)] {
     puts "Stopped LTSMin Server $prolog_variables(Res)"
     procInsertHistoryOptionsState
     procShowErrors
     if {$prolog_variables(Res) == "no_counter_example_found"} {
        tkMessageBox "LTSMin found no counter examples"
     } else {
        tkErrorBox "LTSMin error: $prolog_variables(Res)"
     }
	 } else {
      tkErrorBox "Starting LTSMin failed"
	 }
}

proc procRunCBCTests {Msg clist target maxdepth filename targets_are_final} {
    if [procRunPrologTask cbc_tests($clist,'$target',$maxdepth,'$filename',$targets_are_final)] {set abmsg ""} {set abmsg ", Aborted"}
    procInsertHistoryOptionsState
	procShowErrors
	prolog cbc_get_uncovered_events(Uncovered)
	set uncovered $prolog_variables(Uncovered)
	set XtraButton "View CBC Test Tree"
	set XtraCmd {openTreeInspector cbc_tests}
	prolog sap:tcl_get_stored_test_cases(TC)
	set nrtests [llength $prolog_variables(TC)]
	#tkMessageBox "Generated $nrtests test cases, uncovered events: $uncovered"
  puts "targ=$target"
	if {$target == "\#not_invariant" || $target == "\#deadlock"} {
	  if {$target == "\#not_invariant"} { set ERR "INVARIANT"} else { set ERR "DEADLOCK"}
	  if {$nrtests > 0} {
		procShowList4 $prolog_variables(TC) "$Msg" "$ERR violation found! Generated $nrtests counter examples:" 0 0 $XtraButton $XtraCmd "MainBMC_Counterexamples.txt" procShowListClick none
		} else {
		tkMessageBox "No $ERR counter examples found by bounded model checking until depth $maxdepth"
		}
	} elseif {[llength $uncovered] == 0} {
		procShowList4 $prolog_variables(TC) "$Msg" "Generated $nrtests test cases for all events:" 0 0 $XtraButton $XtraCmd "MainTestCases.txt" procShowListClick none
	} else {
		procShowList4 $prolog_variables(TC) "$Msg (Depth $maxdepth$abmsg)" "Generated $nrtests test cases, uncovered events: $uncovered" 0 0 $XtraButton $XtraCmd "MainTestCases.txt" procShowListClick none
	}
}

# get selected line of text widget:
proc getSelectedLine {f} {
	set position [$f index insert]
	regexp {([0-9]+).([0-9]+)} $position all line column
	return $line
}
# double click a testcase to execute its associated trace:
proc procShowListClick {} {
    global procShowListClick_textwidget
    set line [getSelectedLine $procShowListClick_textwidget]
    puts "ins = $line"
    GotoLine $procShowListClick_textwidget $line
    prolog tcl_execute_stored_test_case($line)
    procInsertHistoryOptionsState
}
proc procSAP_GlobalMC_Testcases {} {
    global prompt mc_prompt
    set finished 0
    prologmnf sap:tcl_get_events_preselected(Events,Presel)
    set_prompt_max_depth_default 10 "procSAP_GlobalMC_Testcases"
    set prompt(sap_eventsel) $prolog_variables(Presel)
    if { [catch {set $prompt(sap_filename)}] } {set prompt(sap_filename) "mcm_testcases.xml"}
    while {!$finished} {
	set chosen [SAPDialog "Model Checking Testcase Generation" $prolog_variables(Events) "covered" 1 1 1 1 0 ""]
	if {[llength $chosen] > 0} {
	    set clist [join $chosen ","]
	    set target [escapeChars $prompt(sap_target_predicate)]
	    set maxdepth $prompt(sap_max_depth)
	    set maxnodes $mc_prompt(result)
	    set filename [escapeChars $prompt(sap_filename)]
	    if [string is integer -strict $maxdepth] {
		if [string is integer -strict $maxnodes] {
		    if [prolog sap:tcl_generate_testcases(\[$clist\],'$target',$maxdepth,$maxnodes,'$filename',NrTestcases,Evs)] {
			procShowErrors
			set numberTC $prolog_variables(NrTestcases)
			tkMessageBox "Generated $numberTC test cases covering $prolog_variables(Evs) events."
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
proc SAPDialog {title events verb prompt_for_new_states prompt_for_search_depth prompt_for_file prompt_for_pred allow_empty_sel CheckBox1} {
    global prompt
    global mc_prompt spec_desc sap_checkbutton1
    set result [list]
    destroy .sap
    if [Dialog_Create .sap "$title" -borderwidth 10] {
        if $prompt_for_file {
			message .sap.filemsg -aspect 1000 \
				-text "file for saving test cases:"
			frame .sap.file
			entry .sap.file.filename -textvariable prompt(sap_filename)
			button .sap.file.browse -text Browse -command {SAPFile}
			pack .sap.file.filename .sap.file.browse -side left
			pack .sap.file -side top
        }

        message .sap.predmsg -aspect 1000 \
            -text "predicate to identify target states:"
        entry .sap.pred -textvariable prompt(sap_target_predicate)
        message .sap.eventmsg -aspect 1000 \
            -text "Select $spec_desc(operations_lc) that should be $verb:"
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

        frame .sap.selbuttons
        button .sap.selbuttons.selall -text "Select all" -command {.sap.events.list selection set 0 end}
        button .sap.selbuttons.selnone -text "Deselect all" -command {.sap.events.list selection clear 0 end}
        pack .sap.selbuttons.selnone .sap.selbuttons.selall -side left

		if $prompt_for_pred {
           pack .sap.predmsg .sap.pred .sap.eventmsg .sap.events .sap.selbuttons -expand 1 -fill both
        } else {
           pack .sap.eventmsg .sap.events .sap.selbuttons -expand 1 -fill both
        }

        if $prompt_for_search_depth {
			message .sap.maxmsg -aspect 1000 \
				-text "maximum search depth:"
			entry .sap.max -textvariable prompt(sap_max_depth)
			pack .sap.maxmsg .sap.max -fill x -fill y
        }
        if $prompt_for_new_states {
            message .sap.maxnodemsg -aspect 1000 \
                -text "maximum number of new states:"
            entry .sap.maxnodes -textvariable mc_prompt(result)
			pack .sap.maxnodemsg .sap.maxnodes -fill x -fill y
        }
        if {$CheckBox1 != ""} {
          checkbutton .sap.checkbutton1 -text "$CheckBox1" -variable sap_checkbutton1 -onvalue 1 -offvalue 0
          pack .sap.checkbutton1  -fill x -fill y
		    }

        frame .sap.buttons
        if {$allow_empty_sel} {
           button .sap.buttons.ok -text OK -command {set prompt(ok) 1}
        } else {
           button .sap.buttons.ok -text OK -command {if {[llength [.sap.events.list curselection]] < 1} {tk_messageBox -message "No $spec_desc(operations_lc) selected." -parent .sap -icon warning} else {set prompt(ok) 1}}
        }
        button .sap.buttons.cancel -text Cancel -command {set prompt(ok) 0}
        pack .sap.buttons.ok -side right
        pack .sap.buttons.cancel -side right

        pack .sap.buttons -fill x -fill y

        set prompt(ok) 0
        bind .sap <Return> {set prompt(ok) 1 ; break}
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
    set prompt(sap_filename) [tk_getSaveFile -filetypes $types -initialfile $prompt(sap_filename) -parent . -defaultextension ".xml"]
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
    set prompt(sap_local) [tk_getSaveFile -filetypes $types -initialfile $prompt(sap_local) -parent . -defaultextension ".xml"]
}

# -------
proc procShowInternalRep {} {
  procShowInternalRep1 0 0
}
proc procShowInternalRep1 {unicode forceEventB} {
   global strFilename temporary_unicode_mode
   if {$unicode} {
      prolog set_unicode_mode
      set temporary_unicode_mode 1
   }
   if {$forceEventB} {
      prolog set_force_eventb_mode
   }
   if [prolog specfile:get_internal_representation(Res,true)] {
        set Result $prolog_variables(Res)
        # set SpecName [file tail $strFilename]
        procShowText6 $Result "Internal Representation" none none "Internal.mch" ".mch"
   } else {
       tkErrorBox "Internal Error: Could not display internal representation."
   }
   if {$unicode} {
      prolog unset_unicode_mode
      set temporary_unicode_mode 0
   }
   if {$forceEventB} {
      prolog unset_force_eventb_mode
   }
}

proc procShowInternalPrologRep {} {
   global strFilename
   if [prolog bmachine:b_get_internal_prolog_representation_as_codes(Codes)] {
        # procShowSourceCodeFromCodes .main.frmSource.text $prolog_variables(Codes)
        procShowText6 $prolog_variables(Codes) "Internal Prolog Representation" none none "Internal.pl" ".pl"
   }
}

proc procShowUnsatCore {} {
    prolog predicate_debugger:tcltk_get_unsat_core_with_types(UC)
    set Result $prolog_variables(UC)
    procShowText6 "$Result" "UnsatCore with types" none none "UnsatCore.mch" ".mch"
}

proc procPrintAsEventB {} {
   global strFilename
   if [prolog bmachine:b_show_eventb_as_classicalb(Res,true)] {
        set Result $prolog_variables(Res)
        # set SpecName [file tail $strFilename]
        procShowText6 $Result "Event-B as Classical B" none none "Classical.mch" ".mch"
   } else {
       tkErrorBox "Internal Error: Could not display B representation."
   }
}



proc procParseGoal {Goal} {
       set EGoal [escapeChars $Goal]
       set success [prolog b_set_machine_goal('$EGoal',with_deferred)]
       procShowErrors2 $Goal .main.frmSource.text "Parsing Error Messages" "" "parse errors" ErrorIcon
       return $success
}

proc HighlightSyntaxErrors {w} {
    # puts "widget $w"
    # TO DO: provide a better way to detect which file is shown
    if {$w == ".main.frmSource.text"} {
      # only highlights errors in main file
      while {[prolog tk_get_error_span_linecol_for_main_file(Line,Col,EL,EC)]} {
          procHighlightErrorSpan $w $prolog_variables(Line) $prolog_variables(Col) $prolog_variables(EL) $prolog_variables(EC)
      }
    } else {
      while {[prolog get_error_span_linecol(Line,Col,EL,EC)]} {
          procHighlightErrorSpan $w $prolog_variables(Line) $prolog_variables(Col) $prolog_variables(EL) $prolog_variables(EC)
      }
	}
	prolog reset_errors
}

proc procShowSourceCodeCoverge {} {
  if [prolog source_profiler:source_profiler_enabled] {
		while {[prolog source_profiler:tcltk_get_source_hit_location(Nr,Line,Col,EL,EC)]} {
			  procHighlightErrorSpan ".main.frmSource.text" $prolog_variables(Line) $prolog_variables(Col) $prolog_variables(EL) $prolog_variables(EC)
		}
	} else {
	 tkErrorBox "No source profiling information available. Recompile or launch ProB with -Dprob_src_profile=true"
	}
}

proc procHighlightErrorSpan {w sline scol eline ecol} {
          #puts "Highlight $w $sline"
		  procHighlightLineIn $w $sline
		  procUnderlineColumnFromToIn $w $sline $scol $eline $ecol
		  $w see $eline.0
}
proc procShowErrorsWithBBResults {} {
   puts "show errors"
   prolog "tools_commands:show_errors_with_bb_results"
   puts "done"
}
proc procShowErrors {} {
   procShowErrors1 "" .main.frmSource.text
}
proc procShowErrors1 {ParseContext TextWidget} {
   if [prolog real_error_occurred] {
      procShowErrors2 $ParseContext $TextWidget "Error Messages" "" "errors" ErrorIcon
   } else {
      procShowErrors2 $ParseContext $TextWidget "Warnings" "" "warnings" WarningIcon
   }
}
proc procShowErrors2 {ParseContext TextWidget ErrorTitleMessage ErrorBodyMessage ErrorType ErrWarnIcon} {
   global testing_mode strFilename failed_tests detailed_errors
   if [prolog real_error_occurred] {
      set ERRWARN "error(s)"
   } else {
      set ERRWARN "warning(s)"
   }
   if [prolog get_preference(view_probcli_errors_using_bbresults,true)] {
      prolog backup_errors
      set ExtraButtonName "Show in BBedit..."
      set ExtraCommand "procShowErrorsWithBBResults"
   } else {
      set ExtraButtonName none
      set ExtraCommand none
   }
   if [prolog tcltk_get_all_errors(200,Res,NrErrsShown,NrErrs)] {
    if {$prolog_variables(NrErrs) > $prolog_variables(NrErrsShown)} {
       set NrMsg "(showing first $prolog_variables(NrErrsShown) out of $prolog_variables(NrErrs))"
    } else {
       set NrMsg "($prolog_variables(NrErrs))"
    }
    if {$ParseContext==""} {
        HighlightSyntaxErrors $TextWidget
        set Result $prolog_variables(Res)
    } else {
        # Syntax errors do not relate to main machine but to a subsidiary goal: ParseContext
        set Result [concat [list $ParseContext] $prolog_variables(Res)]
    }
	prolog reset_errors
    if {$testing_mode==true} {
       set errs [llength $Result]
       append failed_tests $strFilename ";  " $errs " $ERRWARN ;;"
       append detailed_errors $strFilename ";  " $prolog_variables(Res) ";;"
       # procShowList2 $detailed_errors "Error Messages" "The following errors occurred:" 0 1 error
    } else {
        global batch_mode
        if {$batch_mode} {
             puts "The following errors occurred:\n$Result\n"
	         prolog assert_cli_error_occurred(procShowErrors)
        } else {
             procShowList3 $Result "$ErrorTitleMessage ($prolog_variables(NrErrs))" "${ErrorBodyMessage}The following $ErrorType occurred $NrMsg:" 0 1 $ExtraButtonName $ExtraCommand "Errors.txt" $ErrWarnIcon
             # errors
        }
    }
   }
}

proc procExpectErrors {n} {
   prologmnf count_errors_occurred(_,NrErrs)
   if {$n==0} {
       procShowErrors
   } else {
       if [prologmnf get_all_errors(Res)] {
           set err_len $prolog_variables(NrErrs)
           if {$err_len != $n} {
                set Result [split [string trimright $prolog_variables(Res) {;}] \;]
                procShowList $Result "Error Messages" "Expected $n errors but got $err_len :"
           }
       } else {
           tkErrorBox "Expecting $n errors: none happened!"
       }
   }
}

proc procJumpToAnOpenNode {} {
    if [prolog tcltk_goto_an_open_node] {
       procInsertHistoryOptionsState
    } else {
       tkMessageBox "No open node, ALL nodes have been processed."
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
       procAllNodesMsg "resetable (can reach an initial state)"
    }
}

proc procJumpToNonDetNode {} {
    global spec_desc
    if [prolog tcltk_goto_a_non_deterministic_node(Msg)] {
       procInsertHistoryOptionsState
	   tkMessageBox "$prolog_variables(Msg)"
    } else {
       procAllNodesMsg "deterministic  (i.e., the same $spec_desc(operation_lc) with same arguments always leads to the same resulting state)"
    }
}
proc procJumpToNonDetOutputNode {} {
    global spec_desc
    if { [prolog "specfile:animation_minor_mode(eventb)"] } {
	   tkMessageBox "This command does not make sense for Event-B models as events have no outputs"
    } elseif [prolog tcltk_goto_a_non_deterministic_output_node(Msg)] {
       procInsertHistoryOptionsState
	   tkMessageBox "$prolog_variables(Msg)"
    } else {
       procAllNodesMsg "have deterministic outputs  (i.e., the same $spec_desc(operation_lc) with same arguments always has the same outputs)"
    }
}
proc procJumpToStateWithID {} {
   # prolog get_state_space_stats(DistinctStates,T,Processed)
   set ID [Dialog_Prompt "Find state with number (ID):"]
   if {$ID != ""} {
		if [prolog tcltk_goto_node_with_id($ID)] {
		   procInsertHistoryOptionsState
		} else {
		   tkMessageBox "Cannot find a state with id '$ID'."
		}
   }
}

proc procAllNodesMsg {NodeFeature} {
      if [prolog tcltk_exists_an_open_node] {
         tkMessageBox "All states visited so far are $NodeFeature.\nUse the model checking command to explore more states."
      } else {
         tkMessageBox "All states are $NodeFeature."
      }
}

proc procJumpToBranchingNode {} {
    global spec_desc
    if [prolog tcltk_goto_a_branching_node] {
       procInsertHistoryOptionsState
    } else {
       procAllNodesMsg "non-branching (i.e., only one $spec_desc(operation_lc) is enabled)"
    }
}

# -------
# procedure to search for custom predicate
# -------

proc procAdvancedFind {} {
   if ![procSpecFileSuccessfullyLoaded] {
       tkErrorBox "No specification file loaded. Cannot find in statespace."
   } elseif [EnterGOALFormulaWithCheckBtn "Enter GOAL predicate for search within existing states (use B syntax):" ""] {
	   prolog tcltk_search_among_existing_nodes(ErrRes,0,0,1,0,0)
	   procShowErrors
	   if {$prolog_variables(ErrRes) == "goal_found"} {
		     tkSuccessMessageWithUpdate "State FOUND" "State satisfying your GOAL predicate was found."
	   } elseif [prolog tcltk_exists_an_open_node] {
		     tkMessageBox "No node visited so far satisfies your GOAL predicate.
You can explore more (unvisited) nodes by using Temporal Model Check and ticking the 'Find defined GOAL' box."
		 } else {
		     tkMessageBox "No node satisfies your GOAL predicate."
	   }
   }
}

proc EnterGOALFormula {} {
   EnterGOALFormulaWithCheckBtn "Enter GOAL predicate (use B syntax):" ""
}
proc EnterGOALFormulaWithCheckBtn {MSG CheckBtn1} {
   EnterGOALFormulaWithCheckBtn2 $MSG $CheckBtn1 ""
}
proc EnterGOALFormulaWithCheckBtn2 {MSG CheckBtn1 DefaultResult} {
   global Goal
   if ![procSpecFileSuccessfullyLoaded] {
       return 0
   }
   set Goal [Dialog_Promptww "Enter Formula" "$MSG" 70 $CheckBtn1 $DefaultResult]
   if {$Goal != ""} {
       return [procParseGoal $Goal]
   } else {
      return 0
   }
}
# TO DO: merge these three into one ? and provide check boxes for DLK / DET check !?
# Maybe we should also keep the dialog box open
proc procFindRelativeDeadlock {} {
   procFindEventListPropertyViolation "relative_deadlock_freedom" "Relative Deadlock" "disabled in a Deadlock" "at least"
}
proc procFindControllerViolation {} {
   procFindEventListPropertyViolation "valid_controller" "Controller State Violation" "enabled by the controller (exactly one in every state)" "exactly"
}
proc procFindDetControllerViolation {} {
   procFindEventListPropertyViolation "det_controller" "Deterministic Controller State Violation" "enabled by the controller (at most one in every state)" "at most"
}
set DLK_chosen ""
proc procFindEventListPropertyViolation {Property Msg Verb Qualifier} {
    global DLK_chosen
    global prompt mc_prompt spec_desc
	prologmnf sap:tcl_get_events_preselected(Events,Presel)
    set_prompt_max_depth_default 10000 "procFindEventListPropertyViolation"
    set prompt(sap_eventsel) $DLK_chosen
	set DLK_chosen [SAPDialog "$Msg Search" $prolog_variables(Events) $Verb 0 0 0 0 0 ""]
	if {[llength $DLK_chosen] > 0} {
          set clist [join $DLK_chosen ","]
		  if [prolog tcltk_goto_event_list_property_violation($Property,\[$clist\],OpList)] {
			   procInsertHistoryOptionsState
			   tkMessageBox "$Msg was found for $spec_desc(operations_lc):\n $prolog_variables(OpList)."
		  } else {
		      prolog sap:get_selected_events(\[$clist\],_,OpList)
			  if [prolog tcltk_exists_an_open_node] {
					tkMessageBox "Every node visited so far enables $Qualifier one $spec_desc(operation_lc) from:\n $prolog_variables(OpList)."
			 } else {
					tkMessageBox "Every node enables $Qualifier one $spec_desc(operation_lc) from:\n $prolog_variables(OpList)."
			 }

		 }
	}
	procShowErrors
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

# -------
# procedure to enter and check an ltl formula
# -------

proc procInitLtldata {} {
    global ltldata
    set ltldata(max) "10000"
    set ltldata(mode) "init"
    #prolog get_preference(use_por_for_ltl,USEPOR)
    #set ltldata(use_por_for_ltl) $prolog_variables(USEPOR)
    prolog get_preference(use_safety_ltl_model_checker,USESAFETY)
    set ltldata(use_safety_ltl_model_checker) $prolog_variables(USESAFETY)
    # puts "inital pref value for use_safety_ltl_model_checker: $prolog_variables(USESAFETY)"
}

proc procInitCtldata {} {
    global ctldata
    set ctldata(max) "10000"
    # set ctldata(mode) "starthere"
    set ctldata(mode) "init"
}

proc procLtl {} {
    global ltldata spec_desc
    set ok [LtlDialog "LTL model checking"\
	"Enter an LTL formula
   use {...} for B predicates,
   G,F,X,U,W,R,true,false,not,&,or and => are part of the supported LTL syntax,
   use e(op) to check if an $spec_desc(operation_lc) op is enabled,
   use sink to check if no $spec_desc(operation_lc) is enabled that leads to another state,
   use brackets to check what is the next $spec_desc(operation_lc), e.g. \[reset\] => X{db={}},
   Past-LTL is supported: Y,H,O,S,T are the duals to X,G,F,U,R"]

   if {$ok} {
       if {[string is integer $ltldata(max)]} {
	   set formula [escapeChars $ltldata(formula)]
	   # ltldata(max) is the maximum number of new nodes processed
	   if [prolog ltl_model_check(\'$formula\',$ltldata(max),$ltldata(mode),Res)] {
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
	      tkErrorBox "Invalid input: $ltldata(max). Please enter a number."
       }
   }
}

proc procCtl {} {
    global ctldata spec_desc
    set ok [CtlDialog "CTL model checking"\
                "Enter a CTL formula
   use {...} for B predicates,
   ExUy,EXx,AXx,EFx,AGx are supported CTL syntax,
   use e(op) to check if an $spec_desc(operation_lc) op is enabled,
   use EX\[Op\]x to check what is the next $spec_desc(operation_lc), e.g. EX\[reset\]{db={}}"]

    if {$ok} {
        if {[string is integer $ctldata(max)]} {
            set formula [escapeChars $ctldata(formula)]

            if [prolog ctl_model_check('$formula',$ctldata(max),$ctldata(mode),Res)] {
                procShowErrors
                set res $prolog_variables(Res)
                if {$res != "syntax_error"} {
                    procInsertHistoryOptionsState
                    if {$res == true} {
                        if [prolog state_space:max_nr_of_new_nodes_limit_not_reached] {
                            tkMessageBox "Formula TRUE.\nWitness found."
                        } else {
                            tkMessageBox "Potential witness found.\nNot all nodes explored!"
                        }
                    } else {
                        if [prolog state_space:max_nr_of_new_nodes_limit_not_reached] {
                            tkErrorBox "Formula FALSE.\nCounterexample found."
                        } else {
                            tkErrorBox "Potential counterexample found.\nNot all nodes explored!"
                        }
                    }
                } else {
                    tkErrorBox "Syntax error."
                }
            } else {
                tkErrorBox "Internal error: CTL model checking failed."
            }
        } else {
            tkErrorBox "Invalid input: $ctldata(max). Please enter a number."
        }
    }
    procShowErrors
}

# procedure to code forward slashes and quotes for transmission to Prolog (encode)
# avoid layout_inside_quotes error message
proc escapeChars {str} {return [string map {"\\" "\\\\" "'" "\\'" "\t" "  " "\n" " " "\r" " "} $str]}

proc AssertionName {} {
     global curFileTypeOpened
     if {$curFileTypeOpened == "EVENTB"} { return "THEOREM" } else { return "ASSERTION" }
}

proc procCBCFindSequenceDialog {} {
    global SeqToFind spec_desc prompt
    if {[prolog b_top_level_operation(Name)]} {
      if {$prompt(result) == ""} {set prompt(result) $prolog_variables(Name)}
      set SeqToFind [Dialog_Promptww "Constraint-Based Find Sequence" "Enter sequence of $spec_desc(operations_lc) (e.g., $prolog_variables(Name)), separated by ',':" 70 "" ""]
      if {$SeqToFind != ""} {
           procCBCFindSequence $SeqToFind
       }
   } else {
       tkErrorBox "No operations available (you may want to use the find valid state command instead)."
   }
}

proc procVerifyAlloyCommand {} {
    global prompt strFilename prolog_variables
    set ext [file extension $strFilename]
    if {$ext != ".als"} {
        tkErrorBox "No Alloy model (.als) loaded."
    } elseif {[prolog b_top_level_operation(Name)]} {
        set AlloyCmdToVerify [procChooseAlloyCommand]
        if {[llength $AlloyCmdToVerify] == 2} {
            set cmdName [lindex $AlloyCmdToVerify 0]
            set solverName [lindex $AlloyCmdToVerify 1]
            prolog tcltk_load_alloy_cmd_in_current_translation('$cmdName')
            prolog tcltk_verify_alloy_cmd('$cmdName','$solverName',CmdIsValid,IsCheckCmd)
            procInsertHistoryOptionsState
            if {$prolog_variables(CmdIsValid) == "true" && $prolog_variables(IsCheckCmd) == "true"} {
                tkMessageBox "The Alloy command is valid. No counterexample found."
            } elseif {$prolog_variables(CmdIsValid) == "false" && $prolog_variables(IsCheckCmd) == "true"} {
                tkMessageBox "The Alloy command is invalid. A counterexample has been found."
            } elseif {$prolog_variables(CmdIsValid) == "true" && $prolog_variables(IsCheckCmd) == "false"} {
                tkMessageBox "The Alloy command is valid."
            } elseif {$prolog_variables(CmdIsValid) == "false" && $prolog_variables(IsCheckCmd) == "false"} {
                tkMessageBox "The Alloy command is invalid. No valid state found."
            } else {
                tkErrorBox "The Alloy command couldn't be verified (unknown)."
            }
        }
    } else {
        tkErrorBox "No Alloy commands available."
   }
}

proc procChooseAlloyCommand {} {
    prologmnf tcltk_get_alloy_cmd_names(CmdNames)
    return [procChooseAlloyCommandFromList $prolog_variables(CmdNames)]
}

proc procChooseAlloyCommandFromList {List} {
    global ok
    destroy .trace
    set f .trace

    if [Dialog_Create $f "Choose Alloy Command" -borderwidth 10] {
        message $f.msg -text "List of Alloy Commands" -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        listbox $f.frmSource.text -yscroll "$f.frmSource.scrolly set" \
         -setgrid 1 -height 9 -width 40
        bind $f.frmSource.text <Double-1> {set ok 1}
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.text -expand 1 -fill both

        set b [frame $f.buttons]
        pack $f.msg $f.frmSource $f.buttons -side top -fill x
        pack $f.frmSource -pady 5 -expand 1 -fill both
        message $f.solvermsg -text "Constraint Solver:" -aspect 1000
        pack $f.solvermsg -side left
        # radio buttons for solver selection
        global solver
        radiobutton $f.radioBtProB -text "ProB" -variable solver -value "prob"
        radiobutton $f.radioBtProBSMT -text "ProB-SMT" -variable solver -value "probsmt"
        radiobutton $f.radioBtZ3 -text "Z3" -variable solver -value "z3"
        pack $f.radioBtProB $f.radioBtProBSMT $f.radioBtZ3 -side left
        $f.radioBtProB select
        button $b.ok -text Select -command {set ok 1}
        button $b.cancel -text Cancel -command {set ok 2}
        pack $b.ok $b.cancel -side right

        bind $f.frmSource <Return> {set ok 2 ; break}
        bind $f.frmSource <Control-c> {set ok 2 ; break}
    }

    foreach i $List {
        $f.frmSource.text insert end $i
    }

    set ok 0
    Dialog_Wait $f ok $f.frmSource
    Dialog_Dismiss $f
    if {$ok==1} {
      set sel [.trace.frmSource.text curselection]
      set res [lindex $List $sel]
      return [list $res $solver]
    } else {
      return [list]
    }
}

proc procCBCFindSequenceG {} {
   global GSeqToFind
   procCBCFindSequence $GSeqToFind
}
proc procCBCFindSequence {SeqToFind} {
    prolog temporary_set_preference(use_smt_mode,true)
    if [prolog tcltk_cbc_find_trace('$SeqToFind',Res)] {
         procInsertHistoryOptionsState
         procShowErrors
         # puts "$prolog_variables(Res)"
         if {$prolog_variables(Res)=="no_solution_found"} {
               tkErrorBox "No solution for trace from initial states found."
         } elseif {$prolog_variables(Res)=="time_out"} {
               tkErrorBox "No solution for trace from initial states found due to TIME-OUT."
         }
    } else {
           procShowErrors
           tkErrorBox "Internal error: command failed."
    }
    prolog reset_temporary_preference(use_smt_mode)
}


proc procConstraintFindValidState {} {
  prolog tcltk_constraint_find_valid_state
  procInsertHistoryOptionsState
}
proc procConstraintFindMaximalValidState {} {
  prolog tcltk_constraint_find_maximal_valid_state
  procInsertHistoryOptionsState
}
proc procConstraintFindAssertionViolation {} {
  prolog tcltk_constraint_find_dynamic_assertion_violation
  procInsertHistoryOptionsState
}



proc procConstraintFindValidStateWithGoalPred {} {
   global ParameterValues AdditionalPredicate ok choose_para_checkbutton1
   prolog b_get_machine_operation_parameter_names('@INITIALISATION',Vars)
   # TODO: also add constants or at least use constants when Vars empty
    prolog current_state_id(CurStateID)
    if {$prolog_variables(CurStateID) == "root"} {
      set chckboxTxt ""
      set choose_para_checkbutton1 0
    } else {
      set chckboxTxt "Use constants from current state"
    }
   procChooseValuesForParametersWithCheckBox "Values for Variables for State satisfying INVARIANT" $prolog_variables(Vars) "Construct" "$chckboxTxt"
   if {$ok} {
    if {$choose_para_checkbutton1} {
       set Use $prolog_variables(CurStateID)
    } else {
       set Use "none"
    }
	  if [prolog "tcltk_constraint_find_valid_state_with_pred(\[$ParameterValues\],'$AdditionalPredicate',$Use)"] {
	       tkSuccessMessageWithUpdate "State CONSTRUCTED" "State satisfying INVARIANT and goal predicate was constructed."
	  } elseif [prolog real_error_occurred] {
	       tkErrorBoxNoParent "Error occured while looking for state satisfying goal predicate and INVARIANT"
	  } else {
	       tkOptWarningBoxNoParent "No state satisfying goal predicate and INVARIANT could be constructed."
	  }
  }
}

proc procCBCFindRedundantInvariants {} {
  procPerformCBCCheck "'INV_NO_RED'"
}
proc procPerformCBCCheck {CBCID} {
   puts "CBC Check $CBCID"
   if [prolog tcltk_perform_cbc_check($CBCID,Text,Result,Ok)] {
       procInsertHistoryOptionsState
       if {$prolog_variables(Ok)=="true"} {
          set Msg "CBC Validation Successful"
       } elseif {$prolog_variables(Ok)=="false"} {
          set Msg "CBC Validation Found Errors"
       } else {
          set Msg "CBC Validation Incomplete"
       }
       procShowList $prolog_variables(Result) $prolog_variables(Text) $Msg
   } else {
       tkErrorBoxNoParent "CBC check failed $CBCID."
   }
}

proc procCBCRefinementCheck {} {
    prolog temporary_set_preference(use_smt_mode,true)

    if [prologmnf tcltk_cbc_refinement_check(ErrRes,_)] {
        procShowErrors
        procShowList2 $prolog_variables(ErrRes) "Constraint Based Refinement Checking" "Constraint Based Refinement Check:" 1 0 "CBC_out.txt" none
        procInsertHistoryOptionsState
    }

    prolog reset_temporary_preference(use_smt_mode)
}

proc procConstraintBasedCheck {} {
    global spec_desc
       prolog temporary_set_preference(use_smt_mode,true)
	   if [prologmnf tcltk_constraint_based_check(ErrRes)] {
			procShowList2 $prolog_variables(ErrRes) "Constraint Based Checking" "Constraint Based Check for the $spec_desc(operations):" 1 0 "CBC_out.txt" none
			 procInsertHistoryOptionsState
       }
       prolog reset_temporary_preference(use_smt_mode)
}

proc procOpConstraintBasedCheck {} {
  procOpConstraintBasedCheck2 "invariant"
}
proc procOpConstraintBasedCheckAss {} {
  procOpConstraintBasedCheck2 "assertions"
}

proc procOpConstraintBasedCheck2 {InvOrAssertions} {
   global OpName
   set OpName [procChooseOperationOrINITIALISATION]
   if {$OpName != ""} {
           prolog temporary_set_preference(use_smt_mode,true)
	       if [prolog tcltk_constraint_based_check_op('$OpName','$InvOrAssertions',ErrRes)] {
			   procInsertHistoryOptionsState
			   procShowErrors
			   set Result $prolog_variables(ErrRes)
			   if {$Result == "ok"} {
			       set msg [getUnboundedWarnings]
				   tkMessageBox "No error state found for $OpName.$msg"
			   } else {
				   tkErrorBox "Error state was found for $OpName:  $Result"
			   }
            } else {
               tkErrorBox "Internal error."
			   procShowErrors
            }
            prolog reset_temporary_preference(use_smt_mode)
	}
}
proc tkCheckOkMessageBox {msg} {
    set f .okmsgbox
	if [Dialog_Create $f "Check Successful" -borderwidth 10] {
	    label $f.icon -image CheckOkIcon
	    message $f.msg -text "$msg" -aspect 1000
	    button $f.ok -text "  OK  " -command {destroy .okmsgbox}
	    pack $f.icon $f.msg -side top
	    pack $f.ok -side right
      bind $f <Return> {destroy .okmsgbox}
      focus -force $f.ok
	} else {
	   tkMessageBox $msg
	}
}
proc procFindDeadlockedState {} {
         prolog temporary_set_preference(use_smt_mode,true)
	       if [prolog tcltk_constraint_find_deadlock_state(Res)] {
                procInsertHistoryOptionsState
                procShowErrors
                if {$prolog_variables(Res)=="time_out"} {
                           tkMessageBox "Time-out occurred during Constraint-Based Deadlock checking\n (using 10 times time-out specified in Animation Preferences)."
                } else {
                           tkMessageBox "Deadlocking state satisfying invariant found!"
                }
         } elseif [prolog error_or_warning_occured] {
			         set msg [getUnboundedWarnings]
               procShowErrors2 "" .main.frmSource.text "Message with Warnings" \
                            "No deadlock counterexample (satisfying invariant) found.$msg\n" "subsidiary warnings" WarningIcon
          } else {
			         procShowErrors
			         set msg [getUnboundedWarnings]
               tkMessageBox "No deadlock counterexample (satisfying invariant) found.$msg"
          }
          prolog reset_temporary_preference(use_smt_mode)
}
proc procFindDeadlockedStateWithPred {} {
   global dialog_checkbutton1
   if [EnterGOALFormulaWithCheckBtn "Enter additional predicate to hold in deadlocked state (use B syntax):" "Filter Unsatisfiable Guards"] {
           prolog temporary_set_preference(use_smt_mode,true)
	       if [prolog tcltk_constraint_find_deadlock_state_with_goal($dialog_checkbutton1,Res)] {
				procInsertHistoryOptionsState
				procShowErrors
				if {$prolog_variables(Res)=="time_out"} {
                   tkMessageBox "Time-out occurred during Deadlock checking\n (using 10 times the time-out specified in Animation Preferences)."
				} else {
                   tkMessageBox "Deadlocking State Satisfying Goal Predicate and Invariant Found!"
				}
            } else {
			   procShowErrors
			   set msg [getUnboundedWarnings]
               tkMessageBox "No Deadlock Counterexample (Satisfying Goal Predicate and Invariant) Found.$msg"
            }
            prolog reset_temporary_preference(use_smt_mode)
   }
}

proc procFindStaticAssertionViolation {} {
           prolog temporary_set_preference(use_smt_mode,true)
	       if [prolog tcltk_constraint_find_static_assertion_violation(Res)] {
				procInsertHistoryOptionsState
				procShowErrors
				if {$prolog_variables(Res)=="time_out"} {
                   tkMessageBox "Time-out occurred during static assertion checking\n (using 10 times time-out specified in Animation Preferences)."
				} elseif {$prolog_variables(Res)=="no_counterexample_found"} {
				   set msg [getUnboundedWarnings]
				   tkMessageBox "No static assertion counterexample found (*enumeration warning occurred*).$msg"
				} elseif {$prolog_variables(Res)=="no_counterexample_exists"} {
				   set msg [getUnboundedWarnings]
				   # TO DO: if disprover_mode on then show PROOF was found
				   tkMessageBox "No static assertion counterexample exists.$msg"
			    } else {
                   tkMessageBox "Counterexample for assertions found!"
				}
            } else {
			   procShowErrors
			   set msg [getUnboundedWarnings]
               tkMessageBox "No static assertion counterexample found (Search failed !).$msg"
            }
            prolog reset_temporary_preference(use_smt_mode)
}

proc getUnboundedWarnings {} {
	if {[prolog b_or_z_mode] && [prolog unfixed_deferred_set(DS)]} {
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
    global curFileDisplayed text_editor_arr
    set twin .main.frmSource.text
    $twin configure -state normal
    # open file
    set strInFile [prob_read $sFileName]
    procDeleteErrorTagsInSourceFile
    .main.frmSource.statusframe.modelstatus configure -text "" -bg #EDECEB
    # On re-opening the File we don't have to delete and insert everytime the text in the text editor.
    # When there are no differences between the text in the file and the text in the ProB editor we
    # just keep the old text in source code.
    if {$text_editor_arr(reopening_file) && $text_editor_arr(file_last_modified) != [file mtime $sFileName] \
                   && !$text_editor_arr(deactivate_asking_for_reloading)} {
        if {$text_editor_arr(ask_when_content_changed) || \
                  (!$text_editor_arr(ask_when_content_changed) && [$twin edit modified])} {
	        # save re-opening, for the case that file is opened with another editor and content in file is changed
	        set ans [tk_messageBox -default no -message "The file $sFileName changed on disk.\nDo you want\
        	         to reload the file content into the text editor?" -title "Warning" -type yesno -icon warning -parent .]
	        if {$ans == "yes"} {
	             procRefreshContentInTextEditor $twin $strInFile false
	             procresetundo
	             set text_editor_arr(file_last_modified) [file mtime $sFileName]
	             tk_messageBox -parent . -icon warning -message "File content in text editor changed!\
                          \nYou have to reload the current model to apply changes."
	        } else {
	             set text_editor_arr(deactivate_asking_for_reloading) true
	        }
	 } elseif {![$twin edit modified]} {
		procRefreshContentInTextEditor $twin $strInFile false
		procresetundo
		set text_editor_arr(file_last_modified) [file mtime $sFileName]
		#.main.frmSource.statusframe.modelstatus configure -text "File content was changed." -bg orange
         }
    }
    if {!$text_editor_arr(reopening_file)} {
        procRefreshContentInTextEditor $twin $strInFile true
        set text_editor_arr(deactivate_asking_for_reloading) false
    }
    set curFileDisplayed $sFileName
}

proc procRefreshContentInTextEditor {editor str disable} {
    global text_editor_arr
    ctext::getAr $editor config ar
    # on deleting and inserting the file content in the text editor
    # we don't have to highlight the text. That's why we have
    # to set ar(highlightAfterId) to value different from ""
    set ar(highlightAfterId) "disable_highlighting"
    $editor delete 1.0 end
    $editor insert 1.0 $str
    set ar(highlightAfterId) ""
    if {$disable} {$editor configure -state disabled}
}

# maybe we have to delete this procedure
proc procBadPerfomanceMayOccurBecauseOfLongLines {str} {
    set countLongLines 0
    set lines [split $str "\n"]
    foreach line $lines {
	set strlen [string length $line]
	if {$strlen > 500 } {
	     set countLongLines [expr {($strlen / 500) + $countLongLines}]
	}
	if {$countLongLines > 5} {
	     return true
 	}
    }
    return false
}

proc procShowSourceCodeFromString {sString} {
    .main.frmSource.text configure -state normal
    procRefreshContentInTextEditor .main.frmSource.text $sString true
}
proc procShowSourceCodeFromCodes {widget codes} {
    # temporary_unicode_mode no longer used: we now always use unicode mode
    global temporary_unicode_mode
    $widget configure -state normal
    $widget delete 0.0 end
    set src ""
    set charcount 0
    set insidetoken 0
    #prolog get_preference(repl_unicode,FULLUNICODE)
    #set allowunicode $prolog_variables(FULLUNICODE)
    set allowunicode true
    foreach i $codes {
        append src [format %c $i]

        if {$i==34} {
          # we enter or leave a string "...", TO DO: perform better token detection, e.g., ==, -->, ...
          set insidetoken [expr 1-$insidetoken]
        }
        if {$i==10 || $i==13} {
            set charcount 0
        } elseif {$charcount > 90 && $i <= 47 && !$insidetoken} {
            # to do: also detect other separators "= 61 "
            # lines which are too long cause performance problems !
            set charcount 2
            append src "\n  "
        } elseif {$charcount > 150 && !$insidetoken} {
            # lines which are too long cause performance problems !
            set charcount 0
            append src "\n"
        } elseif {$charcount > 500} {
            # lines which are too long cause performance problems !
            set charcount 0
            append src "\n"
        } else {
            incr charcount
        }
    }
    $widget insert end $src
    $widget configure -state disabled
}

proc procEnableSourceCodeEditing {} {
    if [prolog preferences:get_preference(allow_source_code_editing,true)] {
      .main.frmSource.text configure -state normal
      #.frmMenu.mnuFile entryconfigure 3 -state normal
    }
}

# -------
# procedure get new options from prolog and insert them to the listbox
# -------
proc procInsertHistoryOptionsState {} {
#      puts "procInsertHistoryOptionsState"
    procInsertHistory
#      puts "Inserting Options"
    procInsertOptions
#      puts "Inserting State"
    procInsertState
#      puts "Inserting State Errors"
    procInsertStateErrors
#      puts "Done procInsertHistoryOptionsState"
    updateEvaluationTree
#      puts "Done updateEvaluationTree"
    procAutomaticExternalToolUpdates
}
proc procClearOptionsStateHistory {} {
        .main.frmInfo.frmPerform.list delete 0 end
        .main.frmInfo.frmState.list delete 0 end
        .main.frmInfo.frmState.list delete 0 end
	    # show parse and type errors attached to root
	    procInsertStateErrors
        procSetSpecDesc
        HighlightSyntaxErrors .main.frmSource.text
}

proc procResetOptions {arg argcolor} {
       global StatusFrame OpStatusFrame
       .main.frmInfo.frmPerform.list delete 0 end
       .main.frmInfo.frmPerform.list insert 0 $arg
       .main.frmInfo.frmPerform.list itemconfigure 0 -foreground $argcolor
       $StatusFrame.inv configure -state disabled
       $StatusFrame.inv configure -image InvUnknown
       $StatusFrame.timeout configure -image TimeoutEmpty
       $StatusFrame.timeout configure -state disabled
       $OpStatusFrame.maxreached configure -image MaxreachEmpty
       $OpStatusFrame.maxreached configure -state disabled
       update idletasks
}
proc procInsertOptions {} {
    # Insert Options into Enabled Operations pane
    global spec_desc
    procResetOptions "Computing..." DarkGray
    if [prolog tcltk_get_options_or_candidates(Options)] {
         procUpdateStatusFrame
         procShowErrors
         .main.frmInfo.frmPerform.list delete 0 end
         # obligatory to split into list because listbox fail to detect complex structure from prolog
         set listOptions $prolog_variables(Options)
         set iNbElem [llength $listOptions]

         # puts "inserting $iNbElem elements from $listOptions"
         #prolog get_specification_description(operations,Ops)
         #set OpDesc [string tolower $prolog_variables(Ops)]
        if {$iNbElem == 0} {
			     .main.frmInfo.frmPerform.status.label configure -text "No enabled operations"
        } else {
           for {set i 0} {$i < $iNbElem} {incr i} {
            .main.frmInfo.frmPerform.list insert $i [lindex $listOptions $i]
           }
          .main.frmInfo.frmPerform.status.label configure -text "Enabled $spec_desc(operations_lc) ($iNbElem)"
         }
	     prologmnf tcltk_get_options_dest_info(DestInfoOptions)
       set listOptions $prolog_variables(DestInfoOptions)
       set iDestNbElem [llength $listOptions]
	     for {set i 0} {$i < $iDestNbElem} {incr i} {
            set destinfo [lindex $listOptions $i]
            if {$destinfo == "skip"} {
              .main.frmInfo.frmPerform.list itemconfigure $i -foreground "midnight blue"
            } elseif {$destinfo == "query"} {
              .main.frmInfo.frmPerform.list itemconfigure $i -foreground "slate blue"
            } elseif {$destinfo == "open"} {
              .main.frmInfo.frmPerform.list itemconfigure $i -foreground DarkOliveGreen4
            } elseif {$destinfo == "invariant_violated"} {
              .main.frmInfo.frmPerform.list itemconfigure $i -foreground IndianRed4
            } elseif {$destinfo == "deadlock"} {
              .main.frmInfo.frmPerform.list itemconfigure $i -foreground DarkOrange3
            } elseif {$destinfo == "goal"} {
              .main.frmInfo.frmPerform.list itemconfigure $i -foreground "dark green"
            } elseif {$destinfo == "time_out"} {
              .main.frmInfo.frmPerform.list itemconfigure $i -foreground DarkOrange1
            } elseif {$destinfo == "candidate"} {
              .main.frmInfo.frmPerform.list itemconfigure $i -foreground gold3
            }
        }
        #color destinations not in list, now this should no longer happen
	     for {set i $iDestNbElem} {$i < $iNbElem} {incr i} {
              .main.frmInfo.frmPerform.list itemconfigure $i -foreground red
	     }
	      if {$iNbElem == 0 && [prolog bmachine:bmachine_is_precompiled] && [prolog tcltk_no_constants_or_no_inititalisation_found]} {
          if {[prolog current_state_id(root)]} {
              if {[prolog b_machine_has_constants_or_properties]} {
                .main.frmInfo.frmPerform.list insert 0 "NO-CONSTANTS-FOUND"
              } else {
                .main.frmInfo.frmPerform.list insert 0 "NO-INITIALISATION-FOUND"
              }
          } else {
              .main.frmInfo.frmPerform.list insert 0 "NO-INITIALISATION-FOUND"
          }
		      .main.frmInfo.frmPerform.list itemconfigure 0 -foreground IndianRed4
       }
       procCheckIfUnsatisfiableProperties 0
    } else {
         procResetOptions "InternalError" DarkOrange3
         procUpdateStatusFrame
         procShowErrors
         tkErrorBox "Internal error occurred while computing enabled $spec_desc(operations_lc)."
    }
}


proc procCheckIfUnsatisfiableProperties {force_feedback} {
	  prologmnf tcltk_get_status(INVVIOLATED,MAXREACHED,TIMEOUT)
    global show_error_if_no_transition
    if {$force_feedback} {
        set show_error_if_no_transition 1
        set not_computed 0
    } else {
        set not_computed $prolog_variables(MAXREACHED)
    }
	if {$show_error_if_no_transition && $not_computed==0 && [prolog tcltk_current_node_has_no_real_transition]} {
		set show_error_if_no_transition 0
		if {[prolog b_or_z_mode] && [prolog bmachine:bmachine_is_precompiled] && [prolog b_machine_has_constants_or_properties] && [prolog current_expression(root,_)]} {
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
		   } elseif  {$prolog_variables(TIMEOUT)==2} {
		      set cmsg "Virtual timeout due to enumeration of infinite types occurred while searching for CONSTANTS which satisfy the PROPERTIES!$ptxt"
		   } elseif  {$prolog_variables(TIMEOUT)==3} {
		      set cmsg "User interrupt while searching for CONSTANTS which satisfy the PROPERTIES!$ptxt"
		   } elseif {$prolog_variables(MAXREACHED)==1} {
		      set cmsg "No CONSTANTS were computed (MAX_INITIALISATIONS=0)!.$ptxt"
		   } else {
		      # TO DO: check status of tcltk_quick_describe_unsat_properties
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
			  # procDebugProperties
			  procUnsatCore
		   }
		} elseif {$force_feedback && ([prolog current_expression(_,root)] || [prolog current_expression(_,concrete_constants(_))])} {
		   global spec_desc
		   if {$prolog_variables(TIMEOUT)==1} {
			  tkErrorBox "Timeout occurred while initialising $spec_desc(machine)!\nClick on the timeout button to recompute without a timeout."
		   } else {
			  if {[prolog b_or_z_mode]} {
				 set ans [tkYesNoMessageWarningBox "Cannot initialise $spec_desc(machine)!\nDo you want to debug the INITIALISATION?" \
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
	if {[prolog b_interpreter:tcltk_quick_describe_unsat_properties(Descr,Status)]} {
		 # set Cmd "procDebugProperties"
		 # set ExtraBtn "Full Debug..."
		 set Cmd "procUnsatCore"
		 set ExtraBtn "Full Debug (UNSAT CORE)..."
		 set Result $prolog_variables(Descr)
		 puts "Status = $prolog_variables(Status)"
		 if {$prolog_variables(Status)=="UNKNOWN"} {
		     procShowList3 $Result "Unknown PROPERTIES Info" $Msg 1 0 $ExtraBtn $Cmd "UNKNOWN_PROPERTIES.txt" WarningIcon
		} elseif {$prolog_variables(Status)!="prob-ignore"} {
		     procShowList3 $Result "Unsatisfiable PROPERTIES Info" $Msg 1 0 $ExtraBtn $Cmd "UNSAT_PROPERTIES.txt" ErrorIcon
		}
	}
}

proc procMaxreachedButtonAction {} {
    global spec_desc
    if [SymmetryOn] {
     prolog preferences:get_preference(symmetry_mode,SYMMODE)
       set Msg "Symmetry is turned on ($prolog_variables(SYMMODE)). This may permute the destination state of an $spec_desc(operation_lc)."
    } else {
       set Msg "Note: Symmetry is off."
    }
    if [prolog tcltk_get_status(INVVIOLATED,MAXREACHED,TIMEOUT)] {
      if {$prolog_variables(MAXREACHED)==1} {
              prolog get_preference(maxNrOfInitialisations,MI)
              prolog get_preference(maxNrOfEnablingsPerOperation,MO)
              if [prolog current_state_corresponds_to_initialised_b_machine] {
                 set Msg "Possibly not all enabled $spec_desc(operations_lc) were computed.\nIncrease MAX_OPERATIONS (current value $prolog_variables(MO)) in the Animation Preferences and reopen the machine."
              } else {
                 set Msg "Possibly not all initialisations for variables and constants were computed.\nIncrease MAX_INITIALISATIONS (current value $prolog_variables(MI)) in the Animation Preferences and reopen the machine."
              }
       } elseif {$prolog_variables(MAXREACHED)==2} {
              set Msg "This state has been ignored as it does not satisfy the SCOPE predicate."
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

proc procUpdateCheckCspAssertionsWindow {win is_new_file} {
    global tcl_version csp_assertions_dialog_arr ltldata
    set csp_assertions_dialog_arr(all_new_assertions) {}
    procUpdateListBoxIfExists $win false true
    prolog "get_csp_processes(Procs)"
    set ListProcs [lsort -dictionary -unique [split [string trimright $prolog_variables(Procs) {$}] \$]]
    set f $win.refMenu
    if {$tcl_version < 8.5} {
         foreach w [list $f.col1.fr.spec $f.col3.fr.impl $f.col2.fr.spec] {
	     if [winfo exists $w] {
        	$w delete 0 end
		foreach i [split $ListProcs] {
			$w insert end $i
		}
	     }
             foreach m [list $f.col1.entry $f.col3.entry $f.col2.entry] {
		if [winfo exists $m] {
             	    $m delete 0 end
	        }
             }
         }
    } else {
        foreach w [list $f.refinement.col1.spec1 $f.refinement.col3.spec2 $f.determinism.col1.box1 \
                      $f.livelock.col2.box $f.deadlock.col1.box1 $f.ltl.line1.col1.box $f.ctl.line1.col1.box] {
             $w configure -values $ListProcs
             $w set ""
        }

        set csp_assertions_dialog_arr(ltl_entry) $ltldata(ltl_info)
        set csp_assertions_dialog_arr(ctl_entry) $ltldata(ctl_info)
        $f.ltl.line1.col2.entry configure -foreground Gray70
        $f.ctl.line1.col2.entry configure -foreground Gray70

        foreach m [list "determinism" "deadlock"] {
             $f.$m.col2.box2 set "Failures"
        }
        $f.refinement.col2.model set "Trace"
    }
}

proc procStatusFrameClick {} {
   if [prolog animation_mode(cspm)] {
        set win .checkCspAsser
        if [winfo exists .checkCspAsser] {
            procUpdateCheckCspAssertionsWindow .checkCspAsser true
            switch -- [wm state $win] {
               normal {
                   raise $win
               }
               withdraw -
               iconic {
                   wm deiconify $win
                   catch {wm geometry $win $dialog(geo,.checkCspAsser)}
               }
            }
            raise .checkCspAsser
            focus $win
        } else {
            CheckCspAssertions
        }
   } else {
        procAnalyseInvariant
   }
}
proc procUpdateStatusFrame {} {
    if [prolog tcltk_get_status(INVVIOLATED,MAXREACHED,TIMEOUT)] {
       global StatusFrame OpStatusFrame
       if {$prolog_variables(INVVIOLATED)==1} {
           # $StatusFrame.inv configure -text KO
           # $StatusFrame.inv configure -background red
           $StatusFrame.inv configure -image InvKO
           $StatusFrame.inv configure -state normal
           $StatusFrame.inv configure -cursor question_arrow
       } elseif {$prolog_variables(INVVIOLATED)==2} {
           $StatusFrame.inv configure -image InvTimeout
           $StatusFrame.inv configure -state normal
           $StatusFrame.inv configure -cursor question_arrow
       } elseif {$prolog_variables(INVVIOLATED)==3} {
           $StatusFrame.inv configure -image InvUnknown
           $StatusFrame.inv configure -state normal
           $StatusFrame.inv configure -cursor question_arrow
       } elseif [prolog current_state_corresponds_to_initialised_b_machine] {
           if [prolog tcltk_state_exists_with_invariant_violated] {
                $StatusFrame.inv configure -image InvOKWarn
           } else {
                $StatusFrame.inv configure -image InvOK
           }
           $StatusFrame.inv configure -state normal
           $StatusFrame.inv configure -cursor question_arrow
       } elseif [prolog animation_mode(cspm)] {
           $StatusFrame.inv configure -image InvRef
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
       } elseif {$prolog_variables(TIMEOUT)==2} {
           $StatusFrame.timeout configure -image TimeoutVirtual
           $StatusFrame.timeout configure -state normal
       } elseif {$prolog_variables(TIMEOUT)==3} {
           $StatusFrame.timeout configure -image Timeout
           $StatusFrame.timeout configure -state normal
       } else {
           $StatusFrame.timeout configure -image TimeoutEmpty
           $StatusFrame.timeout configure -state disabled
       }
       if {$prolog_variables(MAXREACHED)!=0} {
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
           $OpStatusFrame.backtrack configure -image BackEnabled
           $OpStatusFrame.backtrack configure -state normal
       } else {
           $OpStatusFrame.backtrack configure -image BackDisabled
           $OpStatusFrame.backtrack configure -state disabled
       }
       if [prolog tcltk_can_forward] {
           $OpStatusFrame.forward configure -image ForwardEnabled
           $OpStatusFrame.forward configure -state normal
       } else {
           $OpStatusFrame.forward configure -image ForwardDisabled
           $OpStatusFrame.forward configure -state disabled
       }

    }
    # update States Info in Editor status line
    procUpdateIndicesAfterLoadingAFile
}


# -------
# procedure get new state from prolog and insert them to the listbox
# -------
proc procResetState {arg argcolor} {
    global StatusFrame OpStatusFrame
    $StatusFrame.inv configure -state disabled
    $StatusFrame.inv configure -image InvUnknown
    .main.frmInfo.frmState.list delete 0 end
    .main.frmInfo.frmState.list insert 0 $arg
	  .main.frmInfo.frmState.list itemconfigure 0 -foreground $argcolor
    update idletasks
}
proc procInsertState {} {
    .main.frmInfo.frmState.list delete 0 end
    if [prolog tcltk_get_state(State)] {
        # puts "state: $prolog_variables(State)"
        # obligatory to split into list because listbox fail to detect complex structure from prolog
        set listState $prolog_variables(State)
        set iNbElem [llength $listState]
        # we have to also insert 1 by 1
        for {set i 0} {$i < $iNbElem} {incr i} {
            .main.frmInfo.frmState.list insert $i [lindex $listState $i]
        }
			.main.frmInfo.frmState.status.lbl2  configure -text "State Properties ($iNbElem)"
    }
    procShowErrors
    procUpdateStateViewer
}

# -------
# procedure to get the state's errors from prolog and insert them
# to the listbox
# This is not done in procInsertState, because the operations
# (a source for errors) may not yet have been computed
# -------
proc procInsertStateErrors {} {
    if [prolog tcltk_get_state_errors(State)] {
        # obligatory to split into list because listbox fail to detect complex structure from prolog
        set listState $prolog_variables(State)
        set iNbElem [llength $listState]
        # we have to also insert 1 by 1
        for {set i 0} {$i < $iNbElem} {incr i} {
            .main.frmInfo.frmState.list insert $i [lindex $listState $i]
	        .main.frmInfo.frmState.list itemconfigure $i -foreground red
        }
    }
}


# -------
# procedure get new history from prolog and insert them to the listbox
# -------
proc procResetHistory {} {
        .main.frmInfo.frmHisto.list delete 0 end
}
proc procInsertHistory {} {
    global tempHistoryForegroundcol
    if [prolog tcltk_get_history(History)] {
        .main.frmInfo.frmHisto.list delete 0 end
        # obligatory to split into list because listbox fail to detect complex structure from prolog
        set listHistory $prolog_variables(History)
        set iNbElem [llength $listHistory]
        # we have to also insert 1 by 1
        if {$iNbElem == 0} {
			      .main.frmInfo.frmHisto.label configure -text "History"
        } else {
            for {set i 0} {$i < $iNbElem} {incr i} {
              .main.frmInfo.frmHisto.list insert $i [lindex $listHistory $i]
            }
            .main.frmInfo.frmHisto.label configure -text "History ($iNbElem)" -foreground $tempHistoryForegroundcol -highlightbackground IndianRed4
        }
        set tempHistoryForegroundcol black
    }
}

proc tkSuccessMessageWithUpdate {shortmsg longmsg} {
   global temporary_status_message tempHistoryForegroundcol
   # temporarily mark history header as green; to highlight something was added to history
   set tempHistoryForegroundcol DarkOliveGreen4
	 set temporary_status_message " :: $shortmsg"
	 procInsertHistoryOptionsState
   #tkMessageBoxNoParent $longmsg
}
# -------
# procedure perform an option
# -------
global lastClicki lastSeeItem
set lastClicki -1
set lastSeeItem 1

proc procPerformOptionSingleClickShift {} {
  # always highlight, also operations in B mode
  procPerformOptionSingleClickHighlight
}
proc procPerformOptionSingleClick {} {
  if [prolog csp_mode] {
     # only highlight actions in CSP mode
     procPerformOptionSingleClickHighlight
  }
}

proc procPerformOptionSingleClickHighlight {} {
    global lastClicki lastSeeItem
    procRemoveHighlightExecutedOperation
    set iOption [.main.frmInfo.frmPerform.list curselection]
    if {$iOption != ""} {
        set iOption [lindex $iOption 0]
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
	           .main.frmSource.text tag configure executedTag -background gold2
	        }
	       # Make sure that by repeatedly clicking we cycle through the various positions
		   if {$lastClicki==$iOption} {
			   incr lastSeeItem
			   if {$lastSeeItem>=$iNbElem} {
				   set lastSeeItem 0
			   }
			   set SeeLine [lindex $Line $lastSeeItem]
			   # puts "SeeLine: $SeeLine,  lastSeeItem: $lastSeeItem"
			   .main.frmSource.text see "$SeeLine.0"
		   } else {
			   set lastSeeItem $iNbElem
			   incr lastSeeItem -1
		   }
        }
        set lastClicki $iOption
    }
}
proc procPerformHistoryClick {} {
   # React to double-click on item in History
    set iOption [.main.frmInfo.frmHisto.list curselection]
    if {$iOption != ""} {
        set iOption [lindex $iOption 0]
        if [prolog tcltk_backtrack($iOption)] {
            procInsertHistoryOptionsState
        }
    }
}

proc procPerformOption {} {
   # React to double-click on Enabled Operations List
    set iOption [.main.frmInfo.frmPerform.list curselection]
    procPerformOptionByIOption $iOption
}

proc procPerformOptionByIOption {iOption} {
    if {$iOption != ""} {
        set iOption [lindex $iOption 0]
        if {[prolog tcltk_no_constants_or_no_inititalisation_found]} {
            # we have a NO-CONSTANTS-FOUND or NO-INITIALISATION-FOUND transition
            procCheckIfUnsatisfiableProperties 1
        } else {
			incr iOption
			if [prolog tcltk_get_options_candidate_nr_name($iOption,CandidateOpName)] {
			   # the option list was empty and Prolog inserted a list of candidate operations
               # We propose the user to execute this operation by predicate
               procExecuteOperationWithName $prolog_variables(CandidateOpName)
			} else {
			 if [prolog tcltk_perform_nr($iOption)] {
				procInsertHistoryOptionsState
			 }
			}
        }
    }
}

proc procPerformStateList {} {
    global eval_window_arr
    # React to double-click on State Properties List
    if [catch {set itemNrs [.main.frmInfo.frmState.list curselection]}] {
         set ii [lindex $itemNrs 0]
         if {![prolog animation_mode(cspm)]} {
               set eval_window_arr(input) [.main.frmInfo.frmState.list get $ii]
         } else {
               set eval_window_arr(input) ""
         }
    } else {set eval_window_arr(input) ""}
    procEvalConsole
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
	if [winfo exists $top] {
		switch -- [wm state $top] {
			normal {
				# Raise a buried window
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
		eval {toplevel $top} $args
		wm title $top $title
        if {[prolog tools:host_platform(darwin)]} {
            # Hiding the main menu bar
            menu $top.m -tearoff 0
            $top config -menu $top.m
        }
        if {$tcl_version>=8.5} {
            wm iconphoto $top WindowSubIcon
        }
		return 1
	}
}
proc Dialog_WaitVisible {top {focus {}}} {
	# Grab focus for the dialog
	if {[string length $focus] == 0} {
		set focus $top
	}
	set old [focus -displayof $top]
	focus $focus
	catch {tkwait visibility $top}
	catch {grab $top}
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
   Dialog_Promptwww "Prompt" $string 0 "" "" ""
}


proc Dialog_Create_Prompt { Title msgstring width CheckBox1 msgstring2 args} {
   Dialog_Create_Prompt_f .prompt $Title $msgstring $width $CheckBox1 $msgstring2 "" $args
}

proc Dialog_Create_Prompt_f { f Title msgstring width CheckBox1 msgstring2 Button3PrefCategory args} {
	global prompt dialog_checkbutton1
	destroy $f
	if [Dialog_Create $f "$Title" -borderwidth 10] {
		message $f.msg -text $msgstring -aspect 1000
		if {$width > 0} {
		   entry $f.entry -textvariable prompt(result) -width $width
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
		if {$msgstring2 != ""} {
		    message $f.msg2 -text $msgstring2 -aspect 1000
		    entry $f.entry2 -textvariable prompt(result2) -width $width
		    pack $f.msg2 $f.entry2 -pady 5 -side top
			  bind $f.entry2 <Return> {set prompt(ok) 1 ; break}
			  bind $f.entry2 <Control-c> {set prompt(ok) 0 ; break}
		}
		button $b.ok -text OK -command {set prompt(ok) 1}
		button $b.cancel -text Cancel \
			-command {set prompt(ok) 0}
		pack $b.ok -side left
		if {$Button3PrefCategory != ""} {
		  button $b.btn3 -text "Preferences..." -command "procSetPreferences $Button3PrefCategory"
		  # TO DO: Preference dialog does not fully work in this context: text fields cannot be edited
		  pack $b.btn3 -side left
		}
		pack $b.cancel -side right
		bind $f.entry <Return> {set prompt(ok) 1 ; break}
		bind $f.entry <Control-c> {set prompt(ok) 0 ; break}
		if {[llength $args] == 1 && [lindex $args 0] != ""} {
		puts "args = $args"
			set strInEntry [$f.entry get]
			$f.entry delete 0 [string length $strInEntry]
			$f.entry insert 0 [lindex $args 0]
		}
	}
}

proc Dialog_Promptww { Title msgstring width CheckBox1 DefaultResult} {
   Dialog_Promptwww $Title $msgstring $width $CheckBox1 $DefaultResult ""
}

proc Dialog_Promptwww { Title msgstring width CheckBox1 DefaultResult msgstring2} {
	global prompt dialog_checkbutton1
	set f .prompt
	Dialog_Create_Prompt $Title $msgstring $width $CheckBox1 $msgstring2
	set prompt(ok) 0
	Dialog_Wait $f prompt(ok) $f.entry
	Dialog_Dismiss $f
	if {$prompt(ok)} {
	    if {$prompt(result)!=""} {
		   return $prompt(result)
		} else {
		   return $DefaultResult
		}
	} else {
		return {}
	}
}

#########################
###### CBC Viewer #######
#########################
proc procResetCBCViewerContents {} {
    global cbc_arr spec_desc
    .cbcViewer.frmSource.text delete 0 end
    procInsertDefaultCBCCheckFormulas .cbcViewer.frmSource.text "No CBC Formula has been added yet."
    .cbcViewer.menu.invariant.col2.box configure -values {}
	.cbcViewer.menu.invariant.col2.box set ""

	set cbc_arr(operations_sequence) ""
	procUpdateEntryContentDel .cbcViewer.menu.sequence.line1.entry "dummy_key" "Enter sequence of $spec_desc(operations_lc), separated by ','" cbc_arr operations_sequence

	set cbc_arr(b_predicate) ""
	procUpdateEntryContentDel .cbcViewer.menu.deadlock.line1.entry "dummy_key" "Enter GOAL Formula, use B syntax" cbc_arr b_predicate

    update
}

proc procInsertDefaultCBCCheckFormulas {w empty_list_msg} {
	global cbc_default_checks check_marks_arr
    if {[prolog get_cbc_data_base_id_checks(IDs)] && [prolog get_cbc_data_base_text_checks(Strings)]} {
  	    set Result1 [split [string trimright $prolog_variables(IDs)]]
  	    set Result2 [procGetPrologTextResultAsList $prolog_variables(Strings) ";"]
		#puts "Result1: $Result1, Result2: $Result2"
        foreach i $Result2 {
				 $w insert end "$check_marks_arr(unchecked)   $i"
        }
        if {$Result2==""} {
      		$w insert end $empty_list_msg
        }
		set def_list {}
		foreach i $Result1 j $Result2 {
            lappend def_list $j $i
		}
		# cbc_default_checks is an array from arrays
		# the array names in cbc_default_checks match
		# to the cbc_id in Result1
		array set cbc_default_checks $def_list
    } else {
        procShowErrors
        $w insert end $empty_list_msg
    }
}

proc procGetPrologTextResultAsList {Text sep} {
	set Result [split [string trimright $Text "$sep"] "$sep"]
	set NewResult {}
	foreach str $Result {
	    lappend NewResult [string trim $str]
	    # removes trailing and leading spaces; leading spaces cause problems
	}
    return $NewResult
}

proc procGetMachineOperations {} {
    prologmnf findall(Name,b_top_level_operation(Name),LN)
    return [lsort -dictionary -unique $prolog_variables(LN)]
}

proc procUpdateCBCViewerContents {} {
    if [winfo exists .cbcViewer] {
        .cbcViewer.menu.invariant.col2.box configure -values [procGetMachineOperations]
        .cbcViewer.menu.invariant.col2.box set ""
		.cbcViewer.frmSource.text delete 0 end
		procInsertDefaultCBCCheckFormulas .cbcViewer.frmSource.text "No CBC Formula has been added yet."
	}
}

proc updateCBCListBoxInvariant {arrayName name ListBox must_be_checked} {
   global spec_desc
   upvar 1 $arrayName arr
   set Op $arr($name);#[lindex [array get $array $name] 1]
   if {$Op != "" } {
      deleteFirstRowIfThereAreNoAssertionsInTheListbox $ListBox "No CBC Formula has been added yet."
      set len [$ListBox index end]
      set str "Op($Op) :\[ invariant preserving \]"
      updateListBoxWithTheAppropriateCBCFormula $ListBox $str $must_be_checked
   } else {
       tk_messageBox -parent .cbcViewer -icon warning -message "No $spec_desc(operation_lc) has been selected."
   }
}

proc updateCBCListBoxSequence {arrayName name ListBox must_be_checked} {
   global spec_desc
   upvar 1 $arrayName arr
   set OpSequence $arr($name);#[lindex [array get $array $name] 1]
   if {$OpSequence != "" && $OpSequence != "Enter sequence of $spec_desc(operations_lc), separated by ','"} {
      deleteFirstRowIfThereAreNoAssertionsInTheListbox $ListBox "No CBC Formula has been added yet."
	  set len [$ListBox index end]
      set str "\u226A $OpSequence \u226B"
      updateListBoxWithTheAppropriateCBCFormula $ListBox $str $must_be_checked
   } else {
       tk_messageBox -parent .cbcViewer -icon warning -message "No Sequence has been entered."
   }
}

proc updateCBCListBoxDeadlockPredicate {arrayName name ListBox must_be_checked} {
    upvar 1 $arrayName arr
    set BPredicate $arr($name);# [lindex [array get $array $name] 1]
    if {$BPredicate != "" && $BPredicate != "Enter GOAL Formula, use B syntax"} {
       deleteFirstRowIfThereAreNoAssertionsInTheListbox $ListBox "No CBC Formula has been added yet."
       set len [$ListBox index end]
       set str "pred($BPredicate) :\[ deadlock state \]"
       updateListBoxWithTheAppropriateCBCFormula $ListBox $str $must_be_checked
    } else {
        tk_messageBox -parent .cbcViewer -icon warning -message "No Sequence has been entered."
    }
}

proc updateListBoxWithTheAppropriateCBCFormula {ListBox str must_be_checked} {
	AddTheNewDefinedStringInListBox $ListBox $str\
	       "HandleCBCFormulaLine" "getCBCDebuggerTrace" "AppendStringToTheCBCFormulasList" .cbcViewer\
		   "Formula has been already checked." "Formula is already in the List Box."\
		   "No CBC Formula has been added yet." $must_be_checked
}

proc AppendStringToTheCBCFormulasList {str} {
   global cbc_arr
   lappend cbc_arr(cbc_formulas) $str
}

proc cbcContextMenu {name box x y empty_list_msg} {

}

proc openCBCViewer {} {
	global CBCEntry CBCTimer cbc_arr
	destroy .cbcViewer
	eval global cbcViewer
	set cbcViewer .cbcViewer
	if [Dialog_Create $cbcViewer "CBC Formula Viewer" -borderwidth 10] {

		############ Menu Bar ############

		menu $cbcViewer.menubar -tearoff 0
		$cbcViewer config -menu $cbcViewer.menubar

		foreach m {File Font Formulas} {
            set mb [string tolower $m]
            set $m [menu $cbcViewer.menubar.$mb -tearoff 0]
            $cbcViewer.menubar add cascade -label $m -menu $cbcViewer.menubar.$mb
		}
		set lb $cbcViewer.frmSource.text
		array set arr_file_cbc_commands {"AReopen File" procGenericLoadFile
									 "BSave CBC Formulas to External File..." "saveAssertionsListToFile .cbcViewer .cbcViewer.frmSource.text"
								     "CExit" "destroy .cbcViewerViewer"}

		procAddSimpleMenuBar $cbcViewer.menubar.file arr_file_cbc_commands
	    procAddFontMenuBar $lb $cbcViewer.menubar.font cbc_arr

		array set arr_check_cbc_commands {"ACheck All CBC Formulas" "CheckAllAssertionsInTheListBox .cbcViewer.frmSource.text HandleCBCFormulaLine"}

		procAddSimpleMenuBar $cbcViewer.menubar.formulas arr_check_cbc_commands


		############ List Box ############
		createListBoxViewer $cbcViewer "List of CBC Formulas" "Gray90" "TkFixedFont"\
		 "HandleCBCFormulaLine" "getCBCDebuggerInfo" "cbcContextMenu" "No CBC Formula has been added yet."

        prologmnf findall(Name,b_top_level_operation(Name),LN)
        set Operations [lsort -dictionary -unique $prolog_variables(LN)]

        ttk::notebook $cbcViewer.menu
        foreach fr {invariant sequence deadlock} name {Invariant Sequence Deadlock} {
              ttk::frame $cbcViewer.menu.$fr
			  $cbcViewer.menu add $cbcViewer.menu.$fr -text $name
        }

		set inv $cbcViewer.menu.invariant
		set seq $cbcViewer.menu.sequence
		set dlock $cbcViewer.menu.deadlock
		global spec_desc
		procCreateChooseAddCheckMenu $inv "$spec_desc(operation):"\
		[list updateCBCListBoxInvariant cbc_arr invariant_operation $cbcViewer.frmSource.text false]\
		[list updateCBCListBoxInvariant cbc_arr invariant_operation $cbcViewer.frmSource.text true]\
		{destroy .cbcViewer} cbc_arr(invariant_operation) [procGetMachineOperations]}

		procCreateEntryAddCheckMenu $seq "Sequence of $spec_desc(operations):"\
		[list updateCBCListBoxSequence cbc_arr operations_sequence $cbcViewer.frmSource.text false]\
		[list updateCBCListBoxSequence cbc_arr operations_sequence $cbcViewer.frmSource.text true]\
		{destroy .cbcViewer} cbc_arr(operations_sequence)\
		"Enter Sequence of $spec_desc(operations_lc), separated by ','" cbc_arr operations_sequence

		procCreateEntryAddCheckMenu $dlock "Formula:"\
		[list updateCBCListBoxDeadlockPredicate cbc_arr b_predicate $cbcViewer.frmSource.text false]\
		[list updateCBCListBoxDeadlockPredicate cbc_arr b_predicate $cbcViewer.frmSource.text true]\
		{destroy .cbcViewer} cbc_arr(b_predicate)\
		"Enter GOAL Formula, use B syntax" cbc_arr b_predicate

		pack $cbcViewer.menu -side top -fill both

		procInsertDefaultCBCCheckFormulas $cbcViewer.frmSource.text "No CBC Formula has been added yet."
}

proc HandleCBCFormulaLine {box str i} {
	global cbc_default_checks check_marks_arr
    $box delete $i
	$box insert $i "$check_marks_arr(checking)   $str"
	update
	$box see $i
	$box activate $i
    set Result ""
	update
	set cbc_id_arr [lindex [array get cbc_default_checks $str] 1]
	if {$cbc_id_arr != ""} {
		global $cbc_id_arr
        procPerformCBCCheckList $box $cbc_id_arr $str $i
	} else {
        if [regexp {^Op\((.*)\) :\[ invariant preserving \]$} $str match operation] {
	        runCBCCommandCheck $box $operation $str $i "invariant_preserving"
        } elseif [regexp {^\u226A (.*) \u226B$} $str match sequence] {
	        runCBCCommandCheck $box $sequence $str $i "find_trace"
	    } elseif [regexp {^pred\((.*)\) :\[ deadlock state \]$} $str match predicate] {
	        if [procParseGoal $predicate] {
		        runCBCCommandCheck $box $predicate $str $i "cbc_find_deadlock"
			}
	    }
	}
}

proc runCBCCommandCheck {box arg str i cbc_type} {
	global check_marks_arr
	set pl_cmd ""; set res ""; set if_res ""; set else_res ""
    switch -exact $cbc_type {
		"invariant_preserving" {
			set pl_cmd  [list prolog tcltk_constraint_based_check_op('$arg',Res)]
		    set res "ok"; set if_res "$check_marks_arr(success)"; set else_res "$check_marks_arr(failed)"
		}
		"find_trace" {
			set pl_cmd [list prolog tcltk_cbc_find_trace('$arg',Res)]
		    set res "no_solution_found"; set if_res "$check_marks_arr(failed)"; set else_res "$check_marks_arr(success)"
		}
		"cbc_find_deadlock" {
			set pl_cmd  [list prolog tcltk_constraint_find_deadlock_state_with_goal(0,Res)]
		    set res "time_out"; set if_res "$check_marks_arr(aborted)"; set else_res "$check_marks_arr(success)"
		}
		default {set pl_cmd "prolog assert_real_error_occurred(tcltk_cbc)"}
	}
	runCBCCommandCheck1 $box $str $i $pl_cmd $res $if_res $else_res
}

proc runCBCCommandCheck1 {box str i pl_cmd res if_res else_res} {
   prolog temporary_set_preference(use_smt_mode,true)
   if [eval $pl_cmd] {
	   if [prolog real_error_occurred] {
           procShowErrors
	   } else {
	       set Result $prolog_variables(Res)
	       $box delete $i
	       if {$Result == "$res"} {
	           $box insert $i "$if_res   $str"
	       } else {
	           $box insert $i "$else_res   $str"
	       }
	   }
	   procInsertHistoryOptionsState
   } else {
       tkErrorBox "Internal error."
	   procShowErrors
   }
   prolog reset_temporary_preference(use_smt_mode)
}

proc procPerformCBCCheckList {box cbc_id_arr str i} {
	global cbc_default_checks check_marks_arr
	set CBCID "\'$cbc_id_arr\'"
    if [prolog tcltk_perform_cbc_check($CBCID,Text,Result,Ok)] {
        procInsertHistoryOptionsState
 	    $box delete $i
		global $cbc_id_arr
		foreach idx {ok text result} val {Ok Text Result} {
			eval [list set [list $cbc_id_arr]($idx) $prolog_variables($val)]
		}
		#puts "$cbc_id_arr: [array get $cbc_id_arr]"
        if {$prolog_variables(Ok)=="true"} {
           puts "CBC Validation Successful"
	       $box insert $i "$check_marks_arr(success)   $str"
        } elseif {$prolog_variables(Ok)=="false"} {
           puts "CBC Validation Found Errors"
	       $box insert $i "$check_marks_arr(failed)   $str"
        } else {
           puts "CBC Validation Incomplete"
	       $box insert $i "$check_marks_arr(aborted)   $str"
        }
    } else {
        tkErrorBoxNoParent "CBC check failed $cbc_id."
    }
}

proc getCBCDebuggerInfo {box} {
	global cbc_default_checks check_marks_arr
    foreach i [$box curselection] {
		regexp {([^a-zA-Z_])   (.*)} [$box get $i] match res str
        if {$res == "$check_marks_arr(success)"} {
			set Msg "CBC Validation Successful"
		} elseif {$res == "$check_marks_arr(failed)"} {
			set Msg "CBC Validation Found Errors"
		} else {
			set Msg "CBC Validation Incomplete"
		}
		set arr [lindex [array get cbc_default_checks $str] 1]
		global $arr
		set result [lindex [array get $arr result] 1]
		set txt [lindex [array get cbc_default_checks($str) text] 1]
	    procShowList $result $txt $Msg
	}
    $box selection clear 0 end
}

proc createListBoxViewer {wviewer title background fontname checkLineProc runDebuggerProc openContextMenuProc empty_list_msg} {
    frame $wviewer.frmSource -borderwidth .1c -relief groove
    message $wviewer.msg -text $title -aspect 1000
    set ctx_name ""
    append ctx_name $wviewer "_ctx"
    if [prolog tools:host_platform(darwin)] {
         set listboxheight 11
    } else {
         set listboxheight 10
    }
    # the scrollbars for the text window
    scrollbar $wviewer.frmSource.scrolly -command "$wviewer.frmSource.text yview"
    scrollbar $wviewer.frmSource.scrollx -orient horizontal -command "$wviewer.frmSource.text xview"
    listbox $wviewer.frmSource.text -yscroll "$wviewer.frmSource.scrolly set" \
        -xscroll "$wviewer.frmSource.scrollx set" \
        -setgrid 1 -height $listboxheight -width 70 -bg [string tolower $background] -selectborderwidth 3 -selectbackground LightSkyBlue1 \
        -selectborderwidth 0 -selectforeground black -fg black -highlightcolor black -font {$fontname 11} \
        -activestyle dotbox -selectmode extended
    pack $wviewer.frmSource.scrolly -side right -fill y
    pack $wviewer.frmSource.scrollx -side bottom -fill x
    pack $wviewer.frmSource.text -expand true -fill both
    bind $wviewer.frmSource.text <Double-ButtonPress-1> [list ActivateLineInListbox $wviewer.frmSource.text $checkLineProc $runDebuggerProc $empty_list_msg]
    bind $wviewer.frmSource.text <Return> [list ActivateLineInListbox $wviewer.frmSource.text $checkLineProc $runDebuggerProc $empty_list_msg]
    bind $wviewer.frmSource.text <<RIGHTCLICK>> [list $openContextMenuProc $ctx_name $wviewer.frmSource.text %X %Y $empty_list_msg]
	set CMD [procGetAccKey]
	bind $wviewer.frmSource.text <$CMD-a> [list $wviewer.frmSource.text selection set 0 end]
	bind $wviewer.frmSource.text <$CMD-A> [list $wviewer.frmSource.text selection set 0 end]
    if [prolog tools:host_platform(darwin)] {
          bind $wviewer.frmSource.text <Control-Button-1> [list $openContextMenuProc $ctx_name $wviewer.frmSource.text %X %Y "$empty_list_msg"]
    }
    pack $wviewer.msg $wviewer.frmSource -side top -fill x
    pack $wviewer.frmSource -expand true -fill both

}

# precedure defines a simple choose-add-check menu with 4 elements:
#     3 buttons -> Add, Check and Cancel
#     1 combobox : for choosing an element to be selcted
# Positioning of the elements:
#
#                <-combobox_name->
#                      box
#
#      <Add>         <Check>        <Cancel>

proc procCreateChooseAddCheckMenu {frame_name combobox_name add_cmd check_cmd cancel_cmd txt_var content} {
	 set fr $frame_name
     foreach col {col1 col2 col3} {
         procCreateFrameOrLabelPlatformDepended frame "$fr.$col"
     }
     procCreateFrameOrLabelPlatformDepended label "$fr.col2.label" $combobox_name
     procCreateFrameOrLabelPlatformDepended label "$fr.col1.empty"
     procCreateFrameOrLabelPlatformDepended label "$fr.col3.empty"
     ttk::combobox $fr.col2.box -textvariable $txt_var -state editable -value $content
     button $fr.col1.add -text Add -command "$add_cmd" -state normal -width 6
     button $fr.col2.check -text Check -command "$check_cmd" -state normal -width 6
     button $fr.col3.cancel -text Cancel -command "$cancel_cmd" -state normal -width 6
     foreach m1 {empty empty add} m2 {label box check} m3 {empty empty cancel} n {15 18 20} n2 {5 5 20} {
           pack "$fr.col1.$m1" -pady "$n" -side top
           pack "$fr.col2.$m2" -pady "$n2" -side top
           pack "$fr.col3.$m3" -pady "$n" -side top
     }
     pack $fr.col1 -side left -expand true -padx 35
     pack $fr.col2 -side left -expand true
     pack $fr.col3 -side left -expand true -padx 40
     if [prolog tools:host_platform(darwin)] {
         foreach b "col1.add col2.check col3.cancel" {
             $fr.$b configure -highlightbackground systemSheetBackground
         }
     }
}

# precedure defines a simple choose-add-check menu with 4 elements:
#     3 buttons -> Add, Check and Cancel
#     2 comboboxes : for choosing an element to be selcted
# Positioning of the elements:
#
#    <-combobox_name1->    <-combobox_name2->
#          box1                  box2
#
#         <Add>                 <Check>            <Cancel>

proc procCreateChooseAddCheckMenu2 {frame_name combobox_name1 combobox_name2 add_cmd check_cmd cancel_cmd txt_var1 txt_var2 content1 content2} {
	set fr $frame_name
    foreach col {col1 col2 col3} {
        procCreateFrameOrLabelPlatformDepended frame "$fr.$col"
    }
    procCreateFrameOrLabelPlatformDepended label "$fr.col1.label1" $combobox_name1
    procCreateFrameOrLabelPlatformDepended label "$fr.col2.label2" $combobox_name2
    procCreateFrameOrLabelPlatformDepended label "$fr.col3.empty"

	ttk::combobox $fr.col1.box1 -textvariable $txt_var1 -state editable -value $content1
	ttk::combobox $fr.col2.box2 -textvariable $txt_var2 -state readonly -value $content2

	button $fr.col1.add -text Add -command "$add_cmd" -state normal -width 6
	button $fr.col2.check -text Check -command "$check_cmd" -state normal -width 6
  button $fr.col3.cancel -text Cancel -command "$cancel_cmd" -state normal

    foreach m1 {label1 box1 add} m2 {label2 box2 check} m3 {empty empty cancel} n {5 5 20} n3 {15 18 20} {
         pack "$fr.col1.$m1" -pady "$n" -side top
         pack "$fr.col2.$m2" -pady "$n" -side top
         pack "$fr.col3.$m3" -pady "$n3" -side top
    }
    pack $fr.col1 $fr.col2 -side left -expand true
    pack $fr.col3 -side left -expand true -padx 40

    if [prolog tools:host_platform(darwin)] {
        foreach b "col1.add col2.check col3.cancel" {
            $fr.$b configure -highlightbackground systemSheetBackground
        }
    }
}

# precedure defines a simple choose-add-check menu with 4 elements:
#     3 buttons -> Add, Check and Cancel
#     1 entry
# Positioning of the elements:
#
#                            <Entry name:>
#          <--------------------Entry--------------------->
#
#         <Add>                 <Check>            <Cancel>

proc procCreateEntryAddCheckMenu {frame_name entry_name add_cmd check_cmd cancel_cmd txt_var sub_txt array arr_elt} {
	set fr $frame_name
    procCreateFrameOrLabelPlatformDepended frame "$fr.line1"
    procCreateFrameOrLabelPlatformDepended frame "$fr.line2"

    foreach col {col1 col2 col3} {
        procCreateFrameOrLabelPlatformDepended frame "$fr.line2.$col"
    }

    pack $fr.line1 $fr.line2 -pady 5 -side top -fill x

    procCreateFrameOrLabelPlatformDepended label "$fr.line1.label" $entry_name
	ttk::entry $fr.line1.entry -textvariable $txt_var -state normal -foreground Gray70

    pack $fr.line1.label -side top
	pack $fr.line1.entry -side top -pady 5 -expand true -padx 50 -fill x

    bind $fr.line1.entry <FocusIn> [list after idle [list procClearSelectionSetCursor $fr.line1.entry $sub_txt]]
    bind $fr.line1.entry <Button-1> [list after idle [list procUpdateEntryContent $fr.line1.entry $sub_txt $array $arr_elt]]
    bind $fr.line1.entry <BackSpace> [list after idle [list procUpdateEntryContentDel $fr.line1.entry "dummy_key" $sub_txt $array $arr_elt]]
    bind $fr.line1.entry <Delete> [list after idle [list procUpdateEntryContentDel $fr.line1.entry "Delete" $sub_txt $array $arr_elt]]
	  bind $fr.line1.entry <Key> [list after idle [list procUpdateEntryContent $fr.line1.entry $sub_txt $array $arr_elt]]

	button $fr.line2.col1.add -text Add -command "$add_cmd" -state normal -width 6
	button $fr.line2.col2.check -text Check -command "$check_cmd" -state normal -width 6
    button $fr.line2.col3.cancel -text Cancel -command "$cancel_cmd" -state normal

    foreach b {add check cancel} col {col1 col2 col3} {
          pack "$fr.line2.$col.$b" -pady 20 -side top
    }

    pack $fr.line2.col1 -side left -expand true -padx 35
    pack $fr.line2.col2 -side left -expand true
    pack $fr.line2.col3 -side left -expand true -padx 40

    if [prolog tools:host_platform(darwin)] {
        foreach b "line2.col1.add line2.col2.check line2.col3.cancel" {
            $fr.$b configure -highlightbackground systemSheetBackground
        }
    }
}

# precedure defines a simple choose-add-check menu with 4 elements:
#     3 buttons -> Add, Check and Cancel
#     1 combobox, 1 entry
# Positioning of the elements:
#
#    <-combobox_name->    <Entry name:>
#          box            <----------Entry---------->
#
#         <Add>           <Check>            <Cancel>
proc procCreateChooseEntryAddCheckMenu {frame_name combobox_name entry_name add_cmd check_cmd cancel_cmd txt_var_combo txt_var_entry content_combo sub_txt_entry array arr_elt} {
  set fr $frame_name
    procCreateFrameOrLabelPlatformDepended frame "$fr.line1"
    procCreateFrameOrLabelPlatformDepended frame "$fr.line2"

    foreach col {col1 col2 col3} {
        procCreateFrameOrLabelPlatformDepended frame "$fr.line2.$col"
    }

    procCreateFrameOrLabelPlatformDepended frame "$fr.line1.col1"
    procCreateFrameOrLabelPlatformDepended frame "$fr.line1.col2"

    pack $fr.line1 $fr.line2 -pady 5 -side top -fill x
    pack $fr.line1.col1 -side left -expand true
    pack $fr.line1.col2 -side left -expand true -fill x

    procCreateFrameOrLabelPlatformDepended label "$fr.line1.col1.label" $combobox_name
    ttk::combobox $fr.line1.col1.box -textvariable $txt_var_combo -state editable -value $content_combo

    pack $fr.line1.col1.label -side top
    pack $fr.line1.col1.box   -side top -pady 5 -expand true

    procCreateFrameOrLabelPlatformDepended label "$fr.line1.col2.label" $entry_name
    ttk::entry $fr.line1.col2.entry -textvariable $txt_var_entry -state normal -foreground Gray70

    pack $fr.line1.col2.label -side top
    pack $fr.line1.col2.entry -side top -pady 5 -padx 20 -expand true -fill x

    bind $fr.line1.col2.entry <FocusIn> [list after idle [list procClearSelectionSetCursor $fr.line1.col2.entry $sub_txt_entry]]
    bind $fr.line1.col2.entry <Button-1> [list after idle [list procUpdateEntryContent $fr.line1.col2.entry $sub_txt_entry $array $arr_elt]]
    bind $fr.line1.col2.entry <BackSpace> [list after idle [list procUpdateEntryContentDel $fr.line1.col2.entry "dummy_key" $sub_txt_entry $array $arr_elt]]
    bind $fr.line1.col2.entry <Delete> [list after idle [list procUpdateEntryContentDel $fr.line1.col2.entry "Delete" $sub_txt_entry $array $arr_elt]]
    bind $fr.line1.col2.entry <Key> [list after idle [list procUpdateEntryContent $fr.line1.col2.entry $sub_txt_entry $array $arr_elt]]

    button $fr.line2.col1.add -text Add -command "$add_cmd" -state normal -width 6
    button $fr.line2.col2.check -text Check -command "$check_cmd" -state normal -width 6
    button $fr.line2.col3.cancel -text Cancel -command "$cancel_cmd" -state normal

    foreach b {add check cancel} col {col1 col2 col3} {
          pack "$fr.line2.$col.$b" -pady 20 -side top
    }

    pack $fr.line2.col1 -side left -expand true -padx 35
    pack $fr.line2.col2 -side left -expand true
    pack $fr.line2.col3 -side left -expand true -padx 40

    if [prolog tools:host_platform(darwin)] {
        foreach b "line2.col1.add line2.col2.check line2.col3.cancel" {
            $fr.$b configure -highlightbackground systemSheetBackground
        }
    }
}

proc procClearSelectionIfNecessary {w sub_txt} {
	puts "[$w get] == $sub_txt"
    if {[$w get] == $sub_txt} {
	    selection clear
		$w icursor 0
	}
}

proc procClearSelectionSetCursor {w sub_txt} {
    selection clear
	if {[$w get] == $sub_txt} {
	    $w icursor 0
	} else {
	    $w icursor end
	}
}

proc procUpdateEntryContentDel {w key sub_txt arrayName arr_elt} {
    upvar 1 $arrayName arr
	if {($arr($arr_elt) == "" || $arr($arr_elt) == [string range $sub_txt 1 end]) && $key == "Delete"} {
	    set arr($arr_elt) $sub_txt
	} elseif {$arr($arr_elt) == ""} {
	    $w configure -foreground Gray70
		set arr($arr_elt) $sub_txt;#array set $array [list $arr_elt $sub_txt];#set cbc_arr($arr_elt) $sub_txt
	} else {
	    # do nothing
	}
}

proc procUpdateEntryContent {w sub_txt arrayName arr_elt} {
 	upvar 1 $arrayName arr
	#puts [list regexp "\(.*\)$sub_txt\(.*\)" $cbc_arr($arr_elt) match head tail]
	#set elt $arr($arr_elt);#[lindex [array get $array $arr_elt] 1]
	set val [eval [list regexp "\(.*\)$sub_txt\(.*\)" $arr($arr_elt) match head tail]]
	if $val {
		if {"$head$tail" != ""} {
		    $w configure -foreground black
			set arr($arr_elt) "$head$tail";#array set $array [list $arr_elt $head$tail];#set cbc_arr($arr_elt) "$head$tail"
	    } else {
		    $w icursor 0
		}
	}
}

## This procedure defines a new popup window for testing assertions in csp-files.
proc CheckCspAssertions {} {
   global strFilename checkCspAsser cspstrFilename UsedCspStrFilename curFileTypeOpened
   global tcl_dir tcl_version csp_assertions_dialog_arr check_marks_arr
   global ltldata
   destroy .checkCspAsser
   set csp_ext [file extension $strFilename]
   if { $csp_ext == ".csp" || $csp_ext == ".cspm" || ($curFileTypeOpened == "B" && $cspstrFilename != "")} {
       if {!($curFileTypeOpened == "B" && $cspstrFilename != "")} {
            set UsedCspStrFilename $strFilename
       } else {
            set UsedCspStrFilename $cspstrFilename
       }
       eval global csp
       set csp .checkCspAsser
       if [Dialog_Create $csp "Check CSP Assertions" -borderwidth 10] {
             #wm transient $csp .
			 ###################################### Menu Bar Part #######################################################################
             menu $csp.menubar -tearoff 0
             $csp config -menu $csp.menubar
             foreach m {File Font Assertions} {
                 set mb [string tolower $m]
                 set $m [menu $csp.menubar.$mb -tearoff 0]
                 $csp.menubar add cascade -label $m -menu $csp.menubar.$mb
             }

             set Filename [file tail $UsedCspStrFilename]
			 # Extra character (in the array bellow this comment) is placed infront of the manu bar label name
			 # to define in which order the labels in the array to be placed in the particular menu bar
			 array set arr_file_commands {"AReopen File" procReOpenCSPFile
		 							      "BCopy new Assertions to File" procAddAllNewCspAssertionsIntoFile
								          "CSave Assertions to External File..." "saveAssertionsListToFile .checkCspAsser  $csp.frmSource.text"
								          "DExit" "destroy .checkCspAsser"}
		     # File menu
			 #		-> Reopen File
			 #		-> Copy new Assertions to File
			 #		-> Save Assertions to External File...
			 #		-> Exit

             procAddSimpleMenuBar $csp.menubar.file arr_file_commands
			 procAddFontMenuBar $csp.frmSource.text $csp.menubar.font csp_assertions_dialog_arr

			 # Assertions menu
			 #          -> Check All Assertions
			 #          -> Check All Refinement...
			 #					-> Traces
			 #					-> Failures
			 #					-> Failures-divergence
			 #					-> Refusals
			 #					-> Refusals-divergence
			 #			-> Check Processes for...
			 #					-> Determinism
			 #					-> Deadlock
			 #					-> Livelock
             set fassb $csp.menubar.assertions

             set box $csp.frmSource.text

             $fassb add command -label "Uncheck All Assertions" -command [list procUncheckACheckedAssertionAll $box]
             $fassb add command -label "Delete All Assertions"  -command [list procDeleteAssertionAll $box $csp_assertions_dialog_arr(empty_list_msg)]
             $fassb add sep

             $fassb add cascade -label "Check All Refinement..." -menu $fassb.refs
             set assRefs [menu $fassb.refs -tearoff 0]
             foreach csc {Traces Failures Failures-divergence Refusals Refusals-divergence} i {"T=" "F=" "FD=" "R=" "RD="} {
                   $assRefs add command -label $csc -command [list procCheckAllAssertionsFromType $box $i]
             }
             $fassb add cascade -label "Check Processes for..." -menu $fassb.procs
             set assProc [menu $fassb.procs -tearoff 0]
             foreach csc {Deadlock Determinism Livelock} i {deadlock deterministic livelock} {
                   $assProc add command -label $csc -command "procCheckAllAssertionsFromType $csp.frmSource.text $i"
             }
             $fassb add sep
             $fassb add command -label "Check All LTL Assertions" -command [list procCheckAllAssertionsFromType $box LTL]
             $fassb add command -label "Check All CTL Assertions" -command [list procCheckAllAssertionsFromType $box CTL]

             $fassb add sep
             $fassb add command -label "Check All Assertions" -command [list CheckAllAssertionsInTheListBox $box HandleRefinementAssertion]

             set extTools [menu $csp.menubar.extTools -tearoff 0]
             $csp.menubar add cascade -label "External Tools" -menu $csp.menubar.extTools
		     # File External Tools bar
			 #		-> Open Specification with FDR
			 #		-> Evaluate with CSPM-Interpreter
			 array set arr_extTools_commands {"AOpen Specification with FDR" procOpenFDROnSpec
                                              "BEvaluate with CSPM-Interpreter" procOpenCSPMEvalConsoleInCSPMMode}
			 procAddSimpleMenuBar $csp.menubar.extTools arr_extTools_commands

             if {$tcl_version < 8.5} {
                 set view [menu $csp.menubar.view -tearoff 0]
                 $csp.menubar add cascade -label "View" -menu $csp.menubar.view
             }

			 ###################################### List Box Part #######################################################################
             createListBoxViewer $csp "List of CSP Assertions" $csp_assertions_dialog_arr(background) $csp_assertions_dialog_arr(fontname)\
			                        "HandleRefinementAssertion" "procShowCSPDebugTrace" "procPerformAssertionsContextMenus"\
									$csp_assertions_dialog_arr(empty_list_msg)

			 ###################################### Refinement Menu #####################################################################
             if {$tcl_version >= 8.5} {
                  ttk::notebook $csp.refMenu
                  # four pages for the refinement menu
                  foreach fr {refinement determinism deadlock livelock ltl ctl} {
                        ttk::frame $csp.refMenu.$fr
                  }
             } else {
                  set csp_assertions_dialog_arr(selected_notebook_type) Refinement
                  frame $csp.refMenu
                  set vview $csp.menubar.view
                  foreach m {Refinement Determinism Deadlock Livelock LTL CTL} {
                        $vview add radio -label $m -variable $csp_assertions_dialog_arr(selected_notebook_type) -command "procChangeViewInCheckCspAssertionsWindow $csp.refMenu $m"
                  }
             }
		        pack $csp.refMenu -pady 5 -fill both
             if {$tcl_version >= 8.5} {
                  foreach nl {Refinement Determinism Deadlock Livelock LTL CTL} {
                       set n [string tolower $nl]
                       $csp.refMenu add $csp.refMenu.$n -text $nl
                  }
                  set ref    $csp.refMenu.refinement
                  ##Define widgets content in refinement subwindows
                  procCreateFrameOrLabelPlatformDepended frame "$ref.space"
                  pack  $ref.space -fill x
                  foreach col {col1 col2 col3} lbl {lspec lmod limp} t {"Specification:" "Model:" "Implementation:"} {
                      procCreateFrameOrLabelPlatformDepended frame "$ref.$col"
                      procCreateFrameOrLabelPlatformDepended label "$ref.$col.$lbl" "$t"
                  }
                  ## Get all CSP processes for the combobox widgets.
                  prolog "get_csp_processes(Procs)"
                  set ListProcs [lsort -dictionary -unique [split [string trimright $prolog_variables(Procs) {$}] \$]]
                  set ListModls {"Failures-divergence" "Failures" "Trace" "Refusals" "Refusals-divergence"}
                  ttk::combobox $ref.col1.spec1 -textvariable csp_assertions_dialog_arr(spec) -state editable -value $ListProcs
                  ttk::combobox $ref.col2.model -textvariable csp_assertions_dialog_arr(ref_model) -state readonly -value $ListModls
                  ttk::combobox $ref.col3.spec2 -textvariable csp_assertions_dialog_arr(impl) -state editable -value $ListProcs
                  ## Arrange widgets content in refinement subwindow
                  button $ref.col1.add -text Add -command {updateListSpec $csp_assertions_dialog_arr(spec) $csp_assertions_dialog_arr(impl) \
                                      $csp_assertions_dialog_arr(ref_model) $csp.frmSource.text false} -state normal \
                                      -width 6
                  button $ref.col2.check -text Check -command \
                                     {updateListSpec $csp_assertions_dialog_arr(spec) $csp_assertions_dialog_arr(impl) $csp_assertions_dialog_arr(ref_model) $csp.frmSource.text true} \
                                     -state normal -width 6
                  button $ref.col2.change -image SwapProcs -relief raised -command {changeTextvariablesInComboboxes} -state normal
                  button $ref.col3.cancel -text Cancel -command {procCancelAction csp_assertions_dialog_arr assertion_check_running $csp.frmSource.text .checkCspAsser} -state normal -width 6
                  foreach m1 {lspec spec1 add} m3 {limp spec2 cancel} num {5 5 20} {
                       pack "$ref.col1.$m1" -pady $num -side top
                       pack "$ref.col3.$m3" -pady $num -side top
                  }
                  foreach m {lmod model change check} num {5 5 5 5} {
                       pack "$ref.col2.$m" -pady $num -side top
                  }
                  pack $ref.col1 $ref.col2 $ref.col3 -side left -expand true

                  set determ $csp.refMenu.determinism
                  set ddlock $csp.refMenu.deadlock
                  set lvlock $csp.refMenu.livelock
                  set ltl    $csp.refMenu.ltl
                  set ctl    $csp.refMenu.ctl

                  ## Define widgets for the Determinism pane
				  procCreateChooseAddCheckMenu2 $determ "Specification:" "Model:"\
				       [list updateListDetermDeadlock csp_assertions_dialog_arr deterministic_proc deterministic_model $csp.frmSource.text "deterministic" false]\
					   [list updateListDetermDeadlock csp_assertions_dialog_arr deterministic_proc deterministic_model $csp.frmSource.text "deterministic" true]\
					   [list procCancelAction csp_assertions_dialog_arr assertion_check_running $csp.frmSource.text .checkCspAsser]\
					   csp_assertions_dialog_arr(deterministic_proc) csp_assertions_dialog_arr(deterministic_model) $ListProcs {"Failures" "Failures-divergence"}

                   ## Define widgets for the Deadlock pane
	 			  procCreateChooseAddCheckMenu2 $ddlock "Specification:" "Model:"\
	 				       [list updateListDetermDeadlock csp_assertions_dialog_arr deadlock_proc deadlock_model $csp.frmSource.text "deadlock free" false]\
	 					   [list updateListDetermDeadlock csp_assertions_dialog_arr deadlock_proc deadlock_model $csp.frmSource.text "deadlock free" true]\
	 					   [list procCancelAction csp_assertions_dialog_arr assertion_check_running $csp.frmSource.text .checkCspAsser]\
	 					   csp_assertions_dialog_arr(deadlock_proc) csp_assertions_dialog_arr(deadlock_model) $ListProcs {"Failures" "Failures-divergence"}

                  ## Define widgets for the Livelock pane
				  procCreateChooseAddCheckMenu $lvlock "Specification:" \
				        [list updateListLivelock csp_assertions_dialog_arr livelock_proc $csp.frmSource.text false]\
				        [list updateListLivelock csp_assertions_dialog_arr livelock_proc $csp.frmSource.text true]\
						[list procCancelAction csp_assertions_dialog_arr assertion_check_running $csp.frmSource.text .checkCspAsser]\
				        "csp_assertions_dialog_arr(livelock_proc)"  $ListProcs

          set ltl_info $ltldata(ltl_info)
          set ctl_info $ltldata(ctl_info)
          set csp_assertions_dialog_arr(ltl_entry) $ltl_info
          set csp_assertions_dialog_arr(ctl_entry) $ctl_info

          ## Define widgets fot the LTL formulae pane
          procCreateChooseEntryAddCheckMenu $ltl "Specification:" "LTL Formula:"\
               [list updateListLTLCTL csp_assertions_dialog_arr ltl_proc ltl_entry LTL $ltl_info $csp.frmSource.text false]\
               [list updateListLTLCTL csp_assertions_dialog_arr ltl_proc ltl_entry LTL $ltl_info $csp.frmSource.text true]\
               [list procCancelAction csp_assertions_dialog_arr assertion_check_running $csp.frmSource.text .checkCspAsser]\
               csp_assertions_dialog_arr(ltl_proc) csp_assertions_dialog_arr(ltl_entry) $ListProcs\
               $ltl_info csp_assertions_dialog_arr ltl_entry
           #{frame_name combobox_name entry_name add_cmd check_cmd cancel_cmd txt_var_combo txt_var_entry content_combo sub_txt_entry array arr_elt}
           # puts "Created LTL subwindow"
          ## Define widgets fot the CTL formulae pane
          procCreateChooseEntryAddCheckMenu $ctl "Specification:" "CTL Formula:"\
               [list updateListLTLCTL csp_assertions_dialog_arr ctl_proc ctl_entry CTL $ctl_info $csp.frmSource.text false]\
               [list updateListLTLCTL csp_assertions_dialog_arr ctl_proc ctl_entry CTL $ctl_info $csp.frmSource.text true]\
               [list procCancelAction csp_assertions_dialog_arr assertion_check_running $csp.frmSource.text .checkCspAsser]\
               csp_assertions_dialog_arr(ctl_proc) csp_assertions_dialog_arr(ctl_entry) $ListProcs\
               $ctl_info csp_assertions_dialog_arr ctl_entry

                  if [prolog tools:host_platform(darwin)] {
                           $ref.col1.add configure -highlightbackground systemSheetBackground
                           $ref.col2.check configure -highlightbackground systemSheetBackground
                           $ref.col2.change configure -highlightbackground systemSheetBackground
                           $ref.col3.cancel configure -highlightbackground systemSheetBackground
                  }
                  wm protocol .checkCspAsser WM_DELETE_WINDOW {askToSaveNewAssertionsFromCheckCspWin "yes" $csp.frmSource}
             } else {
                  procChangeViewInCheckCspAssertionsWindow $csp.refMenu "Refinement"
             }
      }
      if [prolog "get_csp_assertions_as_string(\'$UsedCspStrFilename\',Res)"] {
		set Result [split [string map {"CSP: " ""} [string trimright $prolog_variables(Res) {$}]] \$]
		set csp_assertions_dialog_arr(listbox_size) [llength $Result]
		foreach i $Result {
			$csp.frmSource.text insert end "$check_marks_arr(unchecked)   $i"
		}
		if {"$Result"==""} {
        		$csp.frmSource.text insert end $csp_assertions_dialog_arr(empty_list_msg)
		}
      } else {
			procShowErrors
        		$csp.frmSource.text insert end $csp_assertions_dialog_arr(empty_list_msg)
      }
    } else {
       tkErrorBoxNoParent "Can only be applied to CSP specifications."
    }
}


# Arguments: 1. The List Box widget (w)
#			 2. The Font Menu Bar (fmb)
#            3. The array for the Viewer configuration (arrayName).
###############################################################
# Font menu bar
#     -> Family-Name
#           -> Courier
#           -> Times
#           -> Arial
#           -> Helvetica TkFixedFont
#     -> Size
#           -> 9
#           -> ...
#           -> 24
#     -> Background
#           -> Gray60
#			 -> ...
#			 -> White
proc procAddFontMenuBar {w fmb arrayName} {
	upvar 1 $arrayName arr
    foreach ms {Family-Name Size Background} mi {name size bg} {
         $fmb add cascade -label $ms -menu $fmb.$mi
    }
    set fnt [menu $fmb.name -tearoff 0]
    foreach name {Arial Courier Helvetica Times TkFixedFont} {
         set n [string tolower $name]
         $fnt add radiobutton -label $name -variable arr(fontname) -value $name -command "procChangeFontName $w $name $arr(fontsize)"
    }
    set fsz [menu $fmb.size -tearoff 0]
    foreach num {9 10 12 14 16 18 20 24} {
         $fsz add radiobutton -label $num -variable arr(fontsize) -value $num -command "procChangeFontSize $w $arr(fontname) $num"
    }
    set fcol [menu $fmb.bg -tearoff 0]
    foreach cl {Gray60 Gray70 Gray80 Gray90 Snow1 Snow2 Snow3 Snow4 White} {
         set c [string tolower $cl]
         $fcol add radiobutton -label $cl -background $c -variable arr(background) -value $cl -command "procChangeBackground $w $c"
    }
    set scol [menu $fmb.color -tearoff 0]
    foreach sc {gray60 LightSkyBlue1 LightSkyBlue2} {
          $scol add radiobutton -label $sc -background $sc -variable arr(selectcolor) -value $sc -command "procChangeSelectColor $w $c"
    }
}

# Arguments: 1. The Menu Bar identifier (fimb)
#            2. The commands array for the menu labels (arrayCommands).
proc procAddSimpleMenuBar {fimb arrayCommands} {
	upvar 1 $arrayCommands arr_cmds
	foreach lbl [lsort -dictionary [array names arr_cmds]] {
        $fimb add command -label [string range $lbl 1 end] -command $arr_cmds($lbl)
    }
}

proc procOpenCSPMEvalConsoleInCSPMMode {} {
	global eval_window_arr
    set eval_window_arr(proCSPInterpreter) false
	procEvalConsole
}

proc procResetAssertionsViewerContents {} {
    # clear Csp Assertions section
	global tcl_version csp_assertions_dialog_arr ltldata
    .checkCspAsser.frmSource.text delete 0 end
    .checkCspAsser.frmSource.text insert end $csp_assertions_dialog_arr(empty_list_msg)
	set f .checkCspAsser.refMenu
    if {$tcl_version >= 8.5} {
         foreach w [list $f.refinement.col1.spec1 $f.refinement.col3.spec2 $f.determinism.col1.box1 \
                    $f.livelock.col2.box $f.deadlock.col1.box1 $f.ltl.line1.col1.box $f.ctl.line1.col1.box] {
              $w configure -values {}
              $w set ""
         }
        set csp_assertions_dialog_arr(ltl_entry) $ltldata(ltl_info)
        set csp_assertions_dialog_arr(ctl_entry) $ltldata(ctl_info)
        $f.ltl.line1.col2.entry configure -foreground Gray70
        $f.ctl.line1.col2.entry configure -foreground Gray70
        update
	} else {
         foreach w [list $f.col1.fr.spec $f.col3.fr.impl] {
			  if [winfo exists $w] {
			       $w delete 0 end
              }
		  }
		  foreach e [list $f.col1.entry $f.col2.entry $f.col3.entry] {
              if [winfo exists $e] {
			       $e delete 0 end
			   }
		   }
		   update
	}
}

proc procCreateFrameOrLabelPlatformDepended {command name args} {
    if [prolog tools:host_platform(darwin)] {
       if {[llength $args] == 1} {
           eval [list $command $name -text [lindex $args 0] -bg #[pickUpPlatformDependendColor]]
       } else {
           eval [list $command $name -bg #[pickUpPlatformDependendColor]]
       }
    } else {
       if {[llength $args] == 1} {
           eval [list $command $name -text [lindex $args 0]]
       } else {
           eval [list $command $name]
       }
    }
}

proc pickUpPlatformDependendColor {} {
    if [prolog tools:host_platform(darwin)] {
        return "EDEDED"
    } elseif [prolog tools:host_platform(windows)] {
        return "F1F1F1"
    } else {
        return "D9D9D9"
    }
}

proc procCancelAction {arrayName name w main_w} {
  upvar 1 $arrayName arr
	set is_running [expr {$arr($name) || [procAssertionMarkedAsChecked $w]}]
  if {!$is_running} {
		askToSaveNewAssertionsFromCheckCspWin yes $w
	} else {
		procInterruptAssertionCheck $main_w
	}
}

proc procChangeViewInCheckCspAssertionsWindow {menu model} {
    global csp_assertions_dialog_arr
    if [winfo exists .checkCspAsser] {
        if [winfo exists $menu.col2.entry] {
            $menu.col2.entry delete 0 end
            if {$model != "Livelock" && [winfo exists $menu.col1.entry]} {
                $menu.col1.entry delete 0 end
                if {$model == "Refinement" && [winfo exists $menu.col3.entry]} {
                    $menu.col3.entry delete 0 end
                }
            }
        }
        destroy $menu
    }
    prolog "get_csp_processes(Procs)"
    set ListProcs [lsort -dictionary -unique [split [string trimright $prolog_variables(Procs) {$}] \$]]
    frame .checkCspAsser.refMenu
    pack .checkCspAsser.refMenu -pady 5 -expand true -fill both
    set menu .checkCspAsser.refMenu
    frame $menu.space
    pack $menu.space -fill x
    frame $menu.col1; frame $menu.col2; frame $menu.col3
    button $menu.col1.add -text Add -command "updateListBoxAfterModelView $model false" -state normal -height 1 -width 7
    button $menu.col2.check -text Check -command "updateListBoxAfterModelView $model true" -state normal -height 1 -width 7
    button $menu.col3.cancel -text Cancel -command \
                  { if {!$csp_assertions_dialog_arr(assertion_check_running)} {askToSaveNewAssertionsFromCheckCspWin "yes" $csp.frmSource
                    } else {procInterruptAssertionCheck .checkCspAsser} } -state normal -height 1 -width 7
    if {$model != "Livelock"} {
        label $menu.col1.lspec -text Specification: -justify left
        label $menu.col2.lmod  -text Model:
        frame $menu.col1.fr
        scrollbar $menu.col1.fr.scrolly -orient vertical -command "$menu.col1.fr.spec yview"
        listbox $menu.col1.fr.spec -yscrollcommand "$menu.col1.fr.scrolly set" \
          -setgrid 1 -height 5 -width 17  -selectbackground gray80 \
          -selectborderwidth 0 -selectforeground black -fg black -highlightcolor black -font {$csp_assertions_dialog_arr(fontname) 10} \
          -activestyle dotbox -selectmode single
        bind $menu.col1.fr.spec <MouseWheel-1> [list $menu.col1.fr.scrolly activate]
        listbox $menu.col2.model \
          -setgrid 1 -height 3 -width 17  -selectbackground gray80 \
          -selectborderwidth 0 -selectforeground black -fg black -highlightcolor black -font {$csp_assertions_dialog_arr(fontname) 10} \
          -activestyle dotbox -selectmode single
        foreach p $ListProcs {
           $menu.col1.fr.spec insert end $p
        }
        foreach m {"Failures-divergence" "Failures" "Trace" "Refusals" "Refusals-divergence"} {
             if {$model != "Refinement" && $m == "Trace"} {break}
                 $menu.col2.model insert end $m
        }
        pack $menu.col1.fr.scrolly -side right -fill y
        pack $menu.col1.fr.spec -fill both
        if {$model == "Refinement"} {
            entry $menu.col1.entry -textvariable csp_assertions_dialog_arr(spec)
            entry $menu.col2.entry -textvariable csp_assertions_dialog_arr(ref_model) -state disabled
            bind $menu.col1.fr.spec <Double-ButtonPress-1> \
                  [list setEntryVariable $menu.col1.fr.spec $menu.col1.entry "Proc1"]
            bind $menu.col2.model   <Double-ButtonPress-1> \
                  [list setEntryVariable $menu.col2.model $menu.col2.entry "Modl"]
            button $menu.col2.change -image SwapProcs -relief raised \
	          -command {changeTextvariablesInComboboxes} -state normal
        } elseif {$model == "Determinism"} {
                           entry $menu.col1.entry -textvariable csp_assertions_dialog_arr(deterministic_proc)
            entry $menu.col2.entry -textvariable csp_assertions_dialog_arr(deterministic_model) -state disabled
            bind $menu.col1.fr.spec <Double-ButtonPress-1> \
                  [list setEntryVariable $menu.col1.fr.spec $menu.col1.entry "ProcDet"]
            bind $menu.col2.model   <Double-ButtonPress-1> \
                  [list setEntryVariable $menu.col2.model $menu.col2.entry "ProcModl"]
            bind $menu.col1.fr.spec <Return> \
                  [list setEntryVariable $menu.col1.fr.spec $menu.col1.entry "ProcDet"]
            bind $menu.col2.model   <Return> \
                  [list setEntryVariable $menu.col2.model $menu.col2.entry "ProcModl"]
         } elseif {$model == "Deadlock"} {
            entry $menu.col1.entry -textvariable csp_assertions_dialog_arr(deadlock_proc)
            entry $menu.col2.entry -textvariable csp_assertions_dialog_arr(deadlock_model) -state disabled
            bind $menu.col1.fr.spec <Double-ButtonPress-1> \
                  [list setEntryVariable $menu.col1.fr.spec $menu.col1.entry "ProcDeadlock"]
            bind $menu.col2.model   <Double-ButtonPress-1> \
                  [list setEntryVariable $menu.col2.model $menu.col2.entry "ProcModlDeadlock"]
         } elseif {$model == "LTL"} {
            entry $menu.col1.entry -textvariable csp_assertions_dialog_arr(ltl_proc)
            entry $menu.col2.entry -textvariable csp_assertions_dialog_arr(ltl_entry)
            bind  $menu.col1.fr.spec <Double-ButtonPress-1> \
                  [list setEntryVariable $menu.col1.fr.spec $menu.col1.entry "ProcLTL"]
         } elseif {$model == "CTL"} {
            entry $menu.col1.entry -textvariable csp_assertions_dialog_arr(ctl_proc)
            entry $menu.col2.entry -textvariable csp_assertions_dialog_arr(ctl_entry)
            bind  $menu.col1.fr.spec <Double-ButtonPress-1> \
                  [list setEntryVariable $menu.col1.fr.spec $menu.col1.entry "ProcCTL"]
         }

         foreach w {lspec fr entry} {
              pack "$menu.col1.$w" -pady 5 -side top
         }
         if {$model == "Refinement"} {
              foreach w {lmod model entry change check} num {5 5 5 2 8} {
                  pack "$menu.col2.$w" -pady "$num" -side top
              }
         } else {
              foreach w {lmod model entry check} num {5 5 5 20} {
                  pack "$menu.col2.$w" -pady "$num" -side top
              }
         }
         pack $menu.col1.add   -pady 20 -side top
         pack $menu.col1 $menu.col2 -side left -expand true
         if {$model == "Refinement"} {
             label $menu.col3.limp  -text Implementation:
             frame $menu.col3.fr
             scrollbar $menu.col3.fr.scrolly -orient vertical -command "$menu.col3.fr.impl yview"
             listbox $menu.col3.fr.impl -yscroll "$menu.col3.fr.scrolly set" \
                 -setgrid 1 -height 5 -width 17  -selectbackground gray80 \
                 -selectborderwidth 0 -selectforeground black -fg black -highlightcolor black -font {$csp_assertions_dialog_arr(fontname) 10} \
                 -activestyle dotbox -selectmode single
             foreach p $ListProcs {$menu.col3.fr.impl insert end $p}
             entry $menu.col3.entry -textvariable csp_assertions_dialog_arr(impl)
             pack $menu.col3.fr.scrolly -side right -fill y
             pack $menu.col3.fr.impl -fill both
             bind $menu.col3.fr.impl <Double-ButtonPress-1> [list setEntryVariable $menu.col3.fr.impl $menu.col3.entry "Proc2"]
             bind $menu.col3.fr.impl <Return> [list setEntryVariable $menu.col3.fr.impl $menu.col3.entry "Proc2"]
             foreach w {limp fr entry cancel} n {5 5 5 20} {
                pack "$menu.col3.$w" -pady "$n" -side top
             }
             pack $menu.col3 -side left -expand true
        }
    } else {
           label $menu.col2.lspec -text Specification:
           frame $menu.col2.fr
           scrollbar $menu.col2.fr.scrolly -orient vertical -command "$menu.col2.fr.spec yview"
           listbox $menu.col2.fr.spec -yscroll "$menu.col2.fr.scrolly set" \
               -setgrid 1 -height 5 -width 17  -selectbackground gray80 \
               -selectborderwidth 0 -selectforeground black -fg black -highlightcolor black -font {$csp_assertions_dialog_arr(fontname) 10} \
               -activestyle dotbox -selectmode single
           foreach p $ListProcs {$menu.col2.fr.spec insert end $p}
           entry $menu.col2.entry -textvariable csp_assertions_dialog_arr(livelock_proc)
           pack $menu.col2.fr.scrolly -side right -fill y
           pack $menu.col2.fr.spec -fill both
           bind $menu.col2.fr.spec <Double-ButtonPress-1> [list setEntryVariable $menu.col2.fr.spec $menu.col2.entry "ProcLivelock"]
           bind $menu.col2.fr.spec <Return> [list setEntryVariable $menu.col2.fr.spec $menu.col2.entry "ProcLivelock"]
           frame $menu.col1.empty
           foreach w {empty empty add} n {55 61 20} {
               pack "$menu.col1.$w" -pady "$n" -side top
           }
           pack $menu.col1 -side left -expand true -padx 40
           foreach w {lspec fr entry check} n {5 5 5 20} {
               pack "$menu.col2.$w" -pady "$n" -side top
           }
           pack $menu.col2 -side left -expand true
    }
    if {$model != "Refinement"} {
        frame $menu.col3.empty
        foreach w {empty empty cancel} n {55 61 20} {
           pack "$menu.col3.$w" -pady "$n" -side top
        }
        pack $menu.col3 -side left -expand true -padx 40
    }
}

proc updateListBoxAfterModelView {model m} {
    global csp_assertions_dialog_arr
    set csp .checkCspAsser
    if {$model == "Refinement"} {
        updateListSpec $csp_assertions_dialog_arr(spec) $csp_assertions_dialog_arr(impl) $csp_assertions_dialog_arr(ref_model) $csp.frmSource.text $m
    } elseif {$model == "Determinism"} {
        updateListDetermDeadlock csp_assertions_dialog_arr deterministic_proc deterministic_model $csp.frmSource.text "deterministic" $m
    } elseif {$model == "Deadlock"} {
        updateListDetermDeadlock csp_assertions_dialog_arr deadlock_proc deadlock_model $csp.frmSource.text "deadlock free" $m
    } elseif {$model == "Livelock"} {
        updateListLivelock csp_assertions_dialog_arr livelock_proc $csp.frmSource.text $m
    }
}

proc setEntryVariable {box entry pr} {
    global csp_assertions_dialog_arr
    foreach i [$box curselection] {
         $entry delete 0 end
         set choosen [$box get $i]
         switch $pr {
             "Proc1" {set csp_assertions_dialog_arr(spec) $choosen}
             "Modl"  {set csp_assertions_dialog_arr(ref_model)  $choosen}
             "Proc2" {set csp_assertions_dialog_arr(impl) $choosen}
             "ProcDet" {set csp_assertions_dialog_arr(deterministic_proc) $choosen}
             "ProcModl" {set csp_assertions_dialog_arr(deterministic_model) $choosen}
             "ProcDeadlock" {set csp_assertions_dialog_arr(deadlock_proc) $choosen}
             "ProcModlDeadlock" {set csp_assertions_dialog_arr(deadlock_model) $choosen}
             "ProcLivelock" {set csp_assertions_dialog_arr(livelock_proc) $choosen}
             "ProcLTL" {set csp_assertions_dialog_arr(ltl_proc) $choosen}
             "ProcCTL" {set csp_assertions_dialog_arr(ctl_proc) $choosen}
         }
    }
}

proc saveAssertionsListToFile {parent ListBox} {
    set types { { "LTL File" {.ltl} } {"Text File" {.txt} } }
    set machinesPath [getMachinesPath]
    set textFileName ""
    if {$machinesPath != "" && [file isdirectory $machinesPath]} {
         set textFileName [tk_getSaveFile -filetypes $types -initialdir $machinesPath -initialfile "Untitled.ltl" -parent $parent -defaultextension ".ltl"]
    } else {
         set textFileName [tk_getSaveFile -filetypes $types -initialfile "Untitled.ltl" -parent $parent -defaultextension ".ltl"]
    }
    set TextToBeWritten ""
    set i 0
    foreach s [$ListBox get 0 end] {
        if [regexp {[^a-zA-Z_] \s* (.*)} $s match str] {
            if {[file extension $textFileName] == ".ltl"} {
                append TextToBeWritten "\[ LTL-Formula $i \]\n"
            }
            append TextToBeWritten $str "\n"
            if {[file extension $textFileName] == ".ltl"} {
                append TextToBeWritten "\n"
            }
        }
        incr i;
    }
    if {![catch {set fid [prob_open $textFileName]}]} {
         puts -nonewline $fid $TextToBeWritten
         close $fid
	 }
}

proc addAssertionsToViewer {parent ListBox} {
    global ltldata
    set types {
            { "LTL File" {.ltl} }
            { "TXT File" {.txt} }
              }
    # open file
    set ltl_filename [proc_openFile [getMachinesPath] $types $parent]
    if {$ltl_filename != ""} {
        # read file
        set strInFile [prob_read $ltl_filename]
        if {[file extension $ltl_filename] == ".ltl"} {
            set formulas [parseLTLFile $ltl_filename]
        } else {
            set formulas [split $strInFile "\n"]
        }
        foreach f $formulas {
            if {$f != ""} {
                if {[$ListBox get 0] == $ltldata(empty_list_msg)} {
                    $ListBox delete 0
                }
                updateListBoxWithTheAppropriateLTLFormula $ListBox $f false $ltldata(empty_list_msg)
            }
        }
    }
}

proc parseLTLFile {strInFile} {
    if [prolog "get_ltl_formulas_from_file(\'$strInFile\',Text)"] {
        return [split [string trimright $prolog_variables(Text) {$}] \$]
    } else {
        tkErrorBoxNoParent "Could not read in LTL File: $strInFile."
        return ""
    }
}

proc askToSaveNewAssertionsFromCheckCspWin {d par} {
   global strFilename UsedCspStrFilename\
          csp_assertions_dialog_arr checkCspAsser
   if {[llength $csp_assertions_dialog_arr(all_new_assertions)] > 0 && false} {
        eval global f
        set f .checkCspAsser
        set ans [tk_messageBox -default yes -message "Do you want to save the generated new assertions in the current source file:\
                                    $UsedCspStrFilename.\nSave this assertions to disk?" \
                    	               -title "Warning" -type yesno -icon warning -parent $par]
        if { $ans == "yes" } {
            set csp_assertions_dialog_arr(listbox_size) 0
            procAddAllNewCspAssertionsIntoFile
            destroyCheckCspAssertionsWindowAndDeleteTmpFiles $d
			      prolog parse_and_load_cspm_file('$UsedCspStrFilename')
        } else {
            set csp_assertions_dialog_arr(listbox_size) 0
            set csp_assertions_dialog_arr(all_new_assertions) {}
            destroyCheckCspAssertionsWindowAndDeleteTmpFiles $d
        }
   } else {
       set csp_assertions_dialog_arr(listbox_size) 0
       destroyCheckCspAssertionsWindowAndDeleteTmpFiles $d
   }
}

proc destroyCheckCspAssertionsWindowAndDeleteTmpFiles {d} {
       if {[expr {$d == "yes"}] && [winfo exists .checkCspAsser]} {
           destroy .checkCspAsser
       } else {
           ## do not close .checkCspAsser
       }
}

proc procReOpenCSPFile {} {
  global curFileTypeOpened
  if {$curFileTypeOpened == "CSP"} {
    procLoadCSPMFile
  } else {
    tkMessageBox "The currently loaded file is not a CSP-M specification."
  }
}

proc procAddAllNewCspAssertionsIntoFile {} {
   global strFilename UsedCspStrFilename csp_assertions_dialog_arr
   if { [llength $csp_assertions_dialog_arr(all_new_assertions)] > 0} {
       set twin .main.frmSource.text
       set lastEndIndex [$twin index end]
       $twin insert end "\n\n--These assertions were added automatically by the ProCSP tool on [clock format [clock seconds]].--\n"
       foreach i $csp_assertions_dialog_arr(all_new_assertions) {
           $twin insert end "\nassert $i"
       }
       $twin insert end "\n\n--End of the automatically added assertions.--\n"
       procHighlightTextSpecial $twin $lastEndIndex end
       $twin see end
       prolog "get_writable_compiled_filename(\'$UsedCspStrFilename\', \'.pl\', PrologFile)"
       set strPrologFilename $prolog_variables(PrologFile)
       prolog "parse_and_load_cspm_file_into_specific_pl_file(\'$UsedCspStrFilename\', \'$strPrologFilename\')"
       set csp_assertions_dialog_arr(all_new_assertions) {}
   }
}

proc procCopyFileContentToTemporaryFile {CspFile tmpCspFile} {
   if {[file exists $CspFile] && [file exists $tmpCspFile]} {
       set fileId [open $CspFile r]
       set fileTempId [open $tmpCspFile a]
       fconfigure $fileId -encoding utf-8
       fconfigure $fileTempId -encoding utf-8

       foreach line [split [read $fileId] \n] {
          puts $fileTempId $line
       }
       close $fileId
       close $fileTempId
   } elseif {![file exists $CspFile]} {
       tkErrorBoxNoParent "The source file does not exist."
   } elseif {![file exists $tmpCspFile]} {
       tkErrorBoxNoParent "The temporary file does not exist."
   } else {
       tkErrorBoxNoParent "Both files do not exist."
   }
}

proc procDeleteTmpFileContent {tmp} {
     prolog "generate_temporary_cspm_file(\'$tmp\', FileNameTemp)"
     set temp $prolog_variables(FileNameTemp)
     file rename -force $temp $tmp
}

proc procRunCspmEvaluator {} {
   global eval_window_arr strFilename
   set widget .eval.frmSource.text
   $widget configure -state normal
   regexp {([0-9]+)\.([0-9]+)} [$widget index insert] match line col
   $widget delete $line.5 $line.end
   $widget insert $line.5 $eval_window_arr(input)
   set evalArg "--src="
   set EvalString [escapeChars $eval_window_arr(input)]
   if {$EvalString != ""} {
       set consoleCommand [lindex [split $EvalString] 0]
       if {$consoleCommand == "time"} {
		set EvalString [join [lreplace [split $EvalString] 0 0 ""]]
       }
       set eval_window_arr(input_index) [$widget index end]
       regexp {([0-9]+)\.[0-9]+} $eval_window_arr(input_index) match line
       if {$eval_window_arr(proCSPInterpreter)} {
		set csp_eval_command "evaluate_csp_expression(\'$EvalString\', \'$strFilename\',Result)"
	}
    if {$consoleCommand == "time"} {
		set command "tcltk_time_call($csp_eval_command)"
		set now [clock clicks -milliseconds]
		procExecuteAndPrintOutCSPResult $widget $command
		set after [clock clicks -milliseconds]
		set RealTime [makePrettyTimeFormat [expr {$after-$now}]]
		prolog "eval_elapsed_time(Time)"
		set KernelTime [makePrettyTimeFormat $prolog_variables(Time)]
		$widget insert [$widget index insert] "\nreal time: $RealTime\nkernel time: $KernelTime\n"
        } else {
		procExecuteAndPrintOutCSPResult $widget $csp_eval_command
	}
        $widget see end
	incr line -1
	procAddNewEnterLine
	$widget highlight $line.0 end

   } else {
       procAddNewEnterLine
   }
}

proc procExecuteAndPrintOutCSPResult {widget command} {
       if {[prolog $command] && ![prolog get_all_errors(ERes)]} {
	       procWrapEvalOutput $widget $prolog_variables(Result)
	} else {
	       prolog get_all_errors(ERes)
	       set PResult [string map {";" "\n"} $prolog_variables(ERes)]
	       $widget insert [$widget index insert] "\n\nERRORS:\n $PResult"
	       prolog reset_errors
	}
}

proc changeTextvariablesInComboboxes {} {
   global csp_assertions_dialog_arr
   if {$csp_assertions_dialog_arr(spec) != "" && $csp_assertions_dialog_arr(impl) != ""} {
       set temp $csp_assertions_dialog_arr(spec)
       set csp_assertions_dialog_arr(spec) $csp_assertions_dialog_arr(impl)
       set csp_assertions_dialog_arr(impl) $temp
   } else {
       tk_messageBox -parent .checkCspAsser -icon warning\
                         -message "One of the string values in the combboxes is empty."
  }
}

proc SwapProcessesRefExpression {box} {
  global check_marks_arr
  foreach i [$box curselection] {
         array set val [procGetAssertionResult [$box get $i]]
         set last [$box index end]
         if $val(negated) {
             set fileString [SwapProcesses $val(pos_expr)]
             if {[isInTheListBox $box $last "$val(mark)   not $fileString"] != -1} {
                  tk_messageBox -parent .checkCspAsser -icon error\
                           -message "Cannot swap processes at this assertion or assertion is already in the list."
             } else {
                  $box insert $last "$check_marks_arr(unchecked)   not $fileString"
                  AppendStringToTheAssertionLists "not $fileString"
             }
         } else {
             set fileString [SwapProcesses $val(expression)]
             if {[isInTheListBox $box $last "$val(mark)   $fileString"] != -1} {
                  tk_messageBox -parent .checkCspAsser -icon error\
                           -message "Cannot swap processes at this assertion or assertion is already in the list."
             } else {
                  $box insert $last "$check_marks_arr(unchecked)   $fileString"
                  AppendStringToTheAssertionLists $fileString
             }
         }
  }
}

# procedure pops-up the context menu by performing a right click on a assertion declaration from the list box
proc procPerformAssertionsContextMenus {ctx_name box X Y empty_list_msg} {
     if {[$box get 0] != $empty_list_msg} {
        set listy [winfo rooty $box]
        set ycoord [expr $Y -$listy]
        set index [$box nearest $ycoord]
        $box selection clear 0 end
        $box selection set $index
        $box activate $index
        CreatePopupMenuForTraces $ctx_name $box $empty_list_msg
        tk_popup $ctx_name $X $Y
     }
}

proc CreatePopupMenuForTraces {ctx_name llbox empty_list_msg} {
  global csp_assertions_dialog_arr
  destroy $ctx_name
  menu $ctx_name -tearoff 0
  set debugState [procSeeIfAssertionExploredAndHolds $llbox yes]
  set checkState [procSeeIfTheSelectedAssertionIsChecked $llbox true]
  set uncheckState [procSeeIfTheSelectedAssertionIsChecked $llbox false]
  set runningModeState [procGetStatusForInterruptinAssertion $llbox]
  set deleteState [procSeeIfAssertionCheckIsRunningDelete $llbox]
  set ref_state [procSeeIfAssertionIsRefinement $llbox]
  procAddCSPAssertionDebugLabels $ctx_name $llbox $debugState
  $ctx_name add command -label "Check Assertion" \
            -command [list ActivateLineInListbox $llbox "HandleRefinementAssertion" "procShowCSPDebugTrace" $csp_assertions_dialog_arr(empty_list_msg)] \
            -state $checkState
  $ctx_name add command -label "Interrupt Assertion Check" -command {procInterruptAssertionCheck .checkCspAsser} \
                        -state $runningModeState
  $ctx_name add command -label "Uncheck Assertion" -command [list procUncheckACheckedAssertion $llbox] \
                        -state $uncheckState
  $ctx_name add command -label "Delete Assertion" -command [list procDeleteAssertion $llbox $empty_list_msg] -state normal
  $ctx_name add command -label "Negate Assertion" -command [list NegateLineInListBox $llbox] -state normal
  if {$ref_state == "normal"} {
    $ctx_name add command -label "Swap Processes" -command [list SwapProcessesRefExpression $llbox] -state $ref_state
  }
  $ctx_name add sep
  $ctx_name add command -label "Check All Assertions" -command   [list CheckAllAssertionsInTheListBox $llbox HandleRefinementAssertion] -state normal
  $ctx_name add command -label "Uncheck All Assertions" -command [list procUncheckACheckedAssertionAll $llbox] -state normal
  $ctx_name add command -label "Delete All Assertions" -command  [list procDeleteAssertionAll $llbox $csp_assertions_dialog_arr(empty_list_msg)] -state normal
  $ctx_name add sep
  $ctx_name add command -label "Summary of the CSP Syntax" -command {procLaunchSyntaxInfo "CSP"} -state normal
  $ctx_name add command -label "Evaluate CSP Expressions" -command {procEvalConsole} -state normal
  $ctx_name add sep
  $ctx_name add command -label "Open Specification with FDR" -command {procOpenFDROnSpec} -state normal
}

proc procAddCSPAssertionDebugLabels {ctx_name llbox debugState} {
  foreach i [$llbox curselection] {
    set AssType [procGetCSPAssertionType [$llbox get $i]]
    if {$AssType == "LTL" || $AssType == "CTL"} {
      $ctx_name add command -label "Show $AssType Counterexample" -command [list procShowCSPDebugTrace $llbox] -state $debugState
      $ctx_name add command -label "Show $AssType Counterexample in State Space" -command [list procDisplayVisitedStates] -state $debugState
    } else {
      $ctx_name add command -label "Debug" -command [list procShowCSPDebugTrace $llbox] -state $debugState
    }
  }
}

proc procGetCSPAssertionType {str} {
  if [regexp {.*(LTL|CTL):.*} $str match type] {
    return $type
  } else {
    return "other"
  }
}

proc procSeeIfAssertionIsRefinement {box} {
  foreach i [$box curselection] {
      if [isRefinementAssertion [$box get $i]] {
        return normal
      }
  }
  return disabled
}

proc procDeleteAssertionAll {box empty_list_msg} {
  set nr_elements [$box index end]
  $box delete 0 $nr_elements
  $box insert 0 $empty_list_msg
}

proc procDeleteAssertion {box empty_list_msg} {
    foreach i [$box curselection] {
        $box delete $i
    }
    if {[$box size] == 0} {
        $box insert 0 $empty_list_msg
    }
}

proc procCopyAssertion {box} {
    foreach i [$box curselection] {
        regexp {[^a-zA-Z_] \s* (.*)} [$box get $i] match str
        clipboard clear
        clipboard append $str
    }
}

proc procGetAssertionResult {str} {
  if [regexp {([^a-zA-Z_]) \s* (.*)} $str match mark expression] {
    set negated 0
    set pos_expr $expression
    if [regexp {^not[\ ](.*)$} $expression match1 pos] {
      set negated 1
      set pos_expr $pos
    }
    return [list match $match mark $mark expression $expression negated $negated pos_expr $pos_expr]
  } else {
    tkErrorBox "Assertion \"$str\" could not be matched."
    procShowErrors
  }
}

proc procUncheckACheckedAssertionAll {box} {
  global check_marks_arr csp_assertions_dialog_arr
  if {[$box get 0] != $csp_assertions_dialog_arr(empty_list_msg)} {
    set nr_elements [$box index end]
    for {set i 0} {$i < $nr_elements} {incr i} {
      array set val [procGetAssertionResult [$box get $i]]
      if {$val(mark) != "$check_marks_arr(unchecked)"} {
        $box delete $i
        $box insert $i "$check_marks_arr(unchecked)   $val(expression)"
      }
    }
  }
}

proc procAssertionCurrentlyNotChecked {box} {
  global check_marks_arr csp_assertions_dialog_arr
  if {[$box get 0] != $csp_assertions_dialog_arr(empty_list_msg)} {
    for {set i 0} {$i < [$box size]} {incr i} {
      array set val [procGetAssertionResult [$box get $i]]
      if {$val(mark) == "$check_marks_arr(checking)"} {
         return 0
      }
    }
  }
  return 1
}

proc procUncheckACheckedAssertion {box} {
  global check_marks_arr
  foreach i [$box curselection] {
     array set val [procGetAssertionResult [$box get $i]]
     if {$val(mark) != "$check_marks_arr(unchecked)"} {
         $box delete $i
         $box insert $i "$check_marks_arr(unchecked)   $val(expression)"
         $box see $i
         $box activate $i
     } else {
         # do nothing
     }
  }
}

proc procSeeIfAssertionExploredAndHolds {box check_not} {
  global check_marks_arr
  foreach i [$box curselection] {
     array set val [procGetAssertionResult [$box get $i]]
     if {$check_not == "yes" && $val(negated)} {
         if {$val(mark) == "$check_marks_arr(failed)"} {
             return "disabled"
         }
     } else {
         if {$val(mark) == "$check_marks_arr(success)"} {
               return "disabled"
         }
     }
    foreach m {unchecked checking aborted incomplete} {
      if {$val(mark) == "$check_marks_arr($m)"} {
        return disabled;
      }
    }
  }
  return "normal"
}

proc procSeeIfLTLAssertionExploredAndHolds {box} {
  global check_marks_arr
  foreach i [$box curselection] {
     array set val [procGetAssertionResult [$box get $i]]
     if {$val(mark) == "$check_marks_arr(failed)"} {
        return "normal"
     }
  }
  return "disabled"
}

proc procSeeIfTheSelectedAssertionIsChecked {box check} {
  global check_marks_arr
    for {set i 0} {$i < [$box size]} {incr i} {
     array set val [procGetAssertionResult [$box get $i]]
     if {$val(mark) == "$check_marks_arr(checking)"} {
        return disabled
     }
  }
  foreach i [$box curselection] {
     array set val [procGetAssertionResult [$box get $i]]
     if {$val(mark) == "$check_marks_arr(unchecked)" && $check == false} {
        return disabled
     } elseif {$val(mark) != "$check_marks_arr(unchecked)" && $check == false} {
        return normal
     } elseif {$val(mark) == "$check_marks_arr(unchecked)" && $check == true} {
        return normal
     } else {
        return disabled
     }
  }
}

proc procGetStatusForInterruptinAssertion {box} {
	global check_marks_arr
  for {set i 0} {$i < [$box size]} {incr i} {
     array set val [procGetAssertionResult [$box get $i]]
     if {$val(mark) == "$check_marks_arr(checking)"} {
         return normal
     }
  }
  return disabled
}

proc procSeeIfAssertionCheckIsRunningDelete {box} {
  global check_marks_arr
  for {set i 0} {$i < [$box size]} {incr i} {
     array set val [procGetAssertionResult [$box get $i]]
     if {$val(mark) == "$check_marks_arr(checking)"} {
         return disabled
     }
  }
  return normal
}

proc procAssertionMarkedAsChecked {box} {
  global check_marks_arr
  for {set i 0} {$i < [$box size]} {incr i}  {
     array set val [procGetAssertionResult [$box get $i]]
     if {$val(mark) == "$check_marks_arr(checking)"} {
         return true
     }
  }
  return false
}

proc procInterruptAssertionCheck {widget} {
    global lib_dir app_dir
    # puts "interrupting signal"
    prolog "user: get_user_signal_ref(ID)"
    set ProcId $prolog_variables(ID)
    if [prolog tools:host_platform(windows)] {
        set cmd [list exec $app_dir/send_user_interrupt.exe $ProcId]
        set cmdlib [list exec $lib_dir/send_user_interrupt.exe $ProcId]
    } else {
        set cmd [list exec $app_dir/send_user_interrupt $ProcId]
        set cmdlib [list exec $lib_dir/send_user_interrupt.exe $ProcId]
    }
    if [catch {eval $cmd}] {
      if [catch {eval $cmdlib}] {
        puts "Error by interrupting process.\n Please check if the send_user_interrupt programm is in $app_dir folder."
      }
    }
    after idle {}
    update
    catch {focus $widget}
}

proc procShowCSPDebugTrace {box} {
  global strFilename UsedCspStrFilename check_marks_arr
  destroy .cspDebugWindow
  set w .cspDebugWindow
  foreach i [$box curselection] {
     set actualString ""
     set Negated ""
     array set val [procGetAssertionResult [$box get $i]]
     if $val(negated) {
         set Negated "\'True\'"
         set actualString $val(pos_expr)
     } else {
         set Negated "\'False\'"
         set actualString $val(expression)
     }
     set ModelString [procGetModelStringForTheTraceDebuggerTitle $actualString]
     if {$val(mark) == "$check_marks_arr(aborted)"} {
              set outStr "Assertion check interrupted by user. No debugging info can be produced."
              tk_messageBox -parent .checkCspAsser -icon info -message $outStr
     } elseif {($val(mark) == "$check_marks_arr(success)" && $Negated == "\'False\'") || ($val(mark) == "$check_marks_arr(failed)" && $Negated == "\'True\'")} {
              if {$ModelString == "LTL"} {
                     set outStr "LTL assertion check succeeded."
              } elseif {$ModelString == "CTL"} {
                     set outStr "CTL assertion check succeeded."
              } elseif {$ModelString == "Deadlock Free"} {
                     regexp (\[^:\]*): $actualString match Proc
                     set outStr "The Process $Proc has no deadlocks."
              } elseif {$ModelString == "Livelock Free"} {
                     regexp (\[^:\]*): $actualString match Proc
                     set outStr "The Process $Proc is divergence free."
              } elseif {$ModelString == "Deterministic"} {
                     regexp (\[^:\]*): $actualString match Proc
                     set outStr "The Process $Proc is deterministic."
              } else {
                     set l [split [string map {"\[T=" "$" "\[F=" "$" "\[FD=" "$" "\[R=" "$"} $actualString] \$]
                     set LeftProc [lindex $l 0]
                     set RightProc [lindex $l 1]
                     set outStr "$RightProc is a $ModelString of $LeftProc."
              }
              tk_messageBox -parent .checkCspAsser -icon info -message $outStr
     } else {
        if {$ModelString == "LTL"} {
          getLTLDebuggerInfo_CE $box $val(expression) ltl
        } elseif {$ModelString == "CTL"} {
          getLTLDebuggerInfo_CE $box $val(expression) ctl
        } else {
		    set assertion "assert [string map {"\\" "\\\\" "'" "\\'"} $actualString]"
        if {[catch { prolog "tcltk_check_csp_assertion(\'$assertion\',\'$UsedCspStrFilename\',Negated,Res)"
             set Result $prolog_variables(Res)}]} {
             puts "Unexpected interruption."
             ##procShowErrors
             ##break
        } else {
             if {$Result == "no_counter_example"} {
                 $box delete $i
                 if {$Negated == "\'True\'"} {
                     $box insert $i "$check_marks_arr(failed)   not $actualString"
                 } else {
                     $box insert $i "$check_marks_arr(success)   $actualString"
                 }
             } elseif {$ModelString != "Unknown"} {
                 $box delete $i
                 if {$Negated == "\'True\'"} {
                     $box insert $i "$check_marks_arr(success)   not $actualString"
                 } else {
                     $box insert $i "$check_marks_arr(failed)   $actualString"
                 }
                 if [Dialog_Create $w "Trace Debugger" -borderwidth 10] {
                     if {$ModelString == "Deadlock Free" || $ModelString == "Livelock Free"} {
                     ## matching just the process
                     regexp (\[^:\]*): $actualString match Proc
                     Create_Pane_Single_Trace $w $ModelString $Proc [procPrettyPrintCSPTrace [string map {"GO:" "go:"} $Result]]
                     } elseif { [lsearch -regexp $actualString {(\[T=|\[F=|\[FD=|\[R=|\[RD=)}] > -1 || $ModelString == "Deterministic"} {
                           if {$ModelString == "Deterministic"} {
                               regexp (\[^:\]*): $actualString match Proc
                               set LeftProc $Proc; set RightProc $Proc
                               set isTraceRefinement 0; set isFailureRefinement 1
                            } else {
                               set l [split [string map {"\[T=" "$" "\[F=" "$" "\[FD=" "$" "\[R=" "$" "\[RD=" "$"} $actualString] \$]
                               set LeftProc [lindex $l 0]; set RightProc [lindex $l 1]
                               set isTraceRefinement [expr {[string first "\[T=" $actualString] > 0}]
                               set isFailureRefinement [expr {[string first "\[T=" $actualString] == -1}]
                            }
                            set outStr [split [string map {"Found counter example: " "" \
                                      "At_last_step_specification_can_do_one_of:" ";"\
                                      "Trace_of_the_left_specification:" ";"\
                                      "CANNOT_REFUSE_COMPL:" ";" "CANNOT_REFUSE:" ";"\
				      "REFUSED_SET:" "refuse:" \
				      "GO:" "go:"
				      } $Result] \;]
			    #puts "outStr: $outStr"
                            set len [llength $outStr]
                            if {$len == 3} {
                                set spec_trace [string trim [lindex $outStr 2]]
                                set impl_trace [string trim [lindex $outStr 0]]
                                set spec_accepts [string trim [lindex $outStr 1]]
                            } elseif {$len == 5} {
                                set spec_trace [string trim [lindex $outStr 4]]
                                set spec_accepts [string trim [lindex $outStr 3]]
                                set impl_trace [string trim [lindex $outStr 0]]
                                set impl_accepts [string trim [lindex $outStr 1]]
                                set impl_refuses [string trim [lindex $outStr 2]]
                                # need to be ample tested
                                set spec_accepts [procFindCommonElement $spec_accepts $impl_accepts $impl_refuses]
                                lappend spec_trace $spec_accepts
                            } else {
                                # Unknown handling of the list outStr
                            }
                            if {($isTraceRefinement || $isFailureRefinement) && $len == 3} {
                                if {[lsearch $impl_trace DIVERGES] > -1 && $ModelString == "Deterministic" } {
                                    Create_Pane_Single_Trace $w $ModelString $Proc $impl_trace
                                } else {
                                    Create_Pane_TraceRefinement .cspDebugWindow $ModelString \
                                        $LeftProc $RightProc $spec_trace $spec_accepts $impl_trace horizontal
                                }
                            } elseif {$isFailureRefinement && $len == 5} {
                                Create_Pane_FailureRefinement .cspDebugWindow $ModelString \
                                        $LeftProc $RightProc $spec_trace $impl_trace $impl_accepts $impl_refuses horizontal
                            } else {
                            }
                           }
                         }
                     } else {
                          tk_messageBox -parent .checkCspAsser -icon error\
                                 -message "Assertion declaration is not a valid refinement."
                     }
             }
       }
     }
    }
}

proc Create_Pane_Single_Trace {pane model Proc trace} {
     global csp_assertions_dialog_arr
     message $pane.model -justify center -text "Model: $model" -aspect 1000
     frame $pane.win
     frame $pane.win.w -borderwidth .1c -relief groove
     message $pane.win.w.spec -text "Specification:" -aspect 1000
     message $pane.win.w.msg  -text [procLimitStringLength $Proc 27] -aspect 1000
     scrollbar $pane.win.w.scrolly -command "$pane.win.w.text yview"
     scrollbar $pane.win.w.scrollx -orient horizontal -command "$pane.win.w.text xview"
     listbox $pane.win.w.text -yscroll "$pane.win.w.scrolly set" -xscroll "$pane.win.w.scrollx set"\
               -setgrid 1 -height 11 -width 30  -selectborderwidth 3 -selectbackground LightSkyBlue1 \
               -selectborderwidth 0 -selectforeground black -fg black -highlightcolor black -font {$csp_assertions_dialog_arr(fontname) 10} \
               -activestyle dotbox -selectmode browse
     procFillTraceDebuggerListBox $pane.win.w.text $trace
     pack $pane.model -fill x -expand false
     pack $pane.win -fill both -expand true
     if {[$pane.win.w.text get end] == "DEADLOCKS"} {
          message $pane.resultMsg -justify center -fg blue \
              -text "This process is not deadlock free." \
              -aspect 1000 -pady 10 -width 400
     } else {
          message $pane.resultMsg -justify center -fg blue \
              -text "This process diverges." \
              -aspect 1000 -pady 10 -width 400
     }
     pack $pane.win.w.spec $pane.win.w.msg -side top -fill x
     pack $pane.win.w.scrollx -side bottom -fill x
     pack $pane.win.w.scrolly -side right -fill y
     pack $pane.win.w.text -expand true -fill both
     pack $pane.win.w -side top -expand true -fill both
     pack $pane.resultMsg -expand false -fill x -side top
}

proc Create_Pane_TraceRefinement {pane model Proc Impl proc_trace proc_accepts impl_trace {orient horizontal}} {
    global csp_assertions_dialog_arr
    message $pane.model -justify center -text "Model: $model" -aspect 1000
    frame $pane.fr -width 450 -height 250

    foreach f {leftFrame rightFrame} name {"Specification:" "Implementation:"} bg {LightBlue LightGreen} pr [list $Proc $Impl] {
        frame $pane.fr.$f -borderwidth .1c -relief groove
        message $pane.fr.$f.spec -text $name -aspec 1000
        message $pane.fr.$f.proc  -text [procLimitStringLength $pr 27] -background $bg -aspect 1000
    }

    frame $pane.fr.leftFrame.fr
    foreach side {left right} titl {Accepts Trace} {
        frame $pane.fr.leftFrame.fr.$side -borderwidth .1c -relief groove
        message $pane.fr.leftFrame.fr.$side.title -text $titl -aspect 1000
        scrollbar $pane.fr.leftFrame.fr.$side.scrolly -orient vertical -command "$pane.fr.leftFrame.fr.$side.text yview"
        scrollbar $pane.fr.leftFrame.fr.$side.scrollx -orient horizontal -command "$pane.fr.leftFrame.fr.$side.text xview"
        listbox $pane.fr.leftFrame.fr.$side.text -yscroll "$pane.fr.leftFrame.fr.$side.scrolly set" \
          -xscroll "$pane.fr.leftFrame.fr.$side.scrollx set" \
          -setgrid 1 -height 11 -width 30  -selectborderwidth 3 -selectbackground LightSkyBlue1 \
          -selectborderwidth 0 -selectforeground black -fg black -highlightcolor black -font {$csp_assertions_dialog_arr(fontname) 10} \
          -activestyle dotbox -selectmode browse
        pack $pane.fr.leftFrame.fr.$side.title -side top
        pack $pane.fr.leftFrame.fr.$side.scrollx -side bottom -fill x
        pack $pane.fr.leftFrame.fr.$side.scrolly -side right -fill y
        pack $pane.fr.leftFrame.fr.$side.text -expand true -side top -fill both
    }
    pack $pane.fr.leftFrame.spec $pane.fr.leftFrame.proc -side top
    pack $pane.fr.leftFrame.fr -expand true -fill both
    pack propagate $pane.fr.leftFrame.fr off
    Pane_Create $pane.fr.leftFrame.fr.left $pane.fr.leftFrame.fr.right -in $pane.fr.leftFrame.fr\
                                                     -orient $orient -percent 0.5 -cursor sb_h_double_arrow

    frame $pane.fr.rightFrame.fr -borderwidth .1c -relief groove
    message $pane.fr.rightFrame.fr.title -text Trace -aspect 1000
    scrollbar $pane.fr.rightFrame.fr.scrolly -orient vertical -command "$pane.fr.rightFrame.fr.text yview"
    scrollbar $pane.fr.rightFrame.fr.scrollx -orient horizontal -command "$pane.fr.rightFrame.fr.text xview"
    listbox $pane.fr.rightFrame.fr.text -yscroll "$pane.fr.rightFrame.fr.scrolly set" \
       -xscroll "$pane.fr.rightFrame.fr.scrollx set" \
       -setgrid 1 -height 11 -width 30  -selectborderwidth 3 -selectbackground LightSkyBlue1 \
       -selectborderwidth 0 -selectforeground black -fg black -highlightcolor black -font {$csp_assertions_dialog_arr(fontname) 10} \
       -activestyle dotbox -selectmode browse
    pack $pane.fr.rightFrame.spec $pane.fr.rightFrame.proc -side top
    pack $pane.fr.rightFrame.fr.title -side top
    pack $pane.fr.rightFrame.fr.scrollx -side bottom -fill x
    pack $pane.fr.rightFrame.fr.scrolly -side right -fill y
    pack $pane.fr.rightFrame.fr.text -expand true -side top -fill both
    pack $pane.fr.rightFrame.fr -expand true -fill both

    pack $pane.model -side top
    pack $pane.fr -expand true -fill both
    pack propagate $pane.fr off
    Pane_Create $pane.fr.leftFrame $pane.fr.rightFrame -in $pane.fr -orient $orient -percent 0.66 -cursor sb_h_double_arrow

    procFillTraceDebuggerListBox $pane.fr.leftFrame.fr.right.text $proc_trace
    procFillTraceDebuggerListBox $pane.fr.rightFrame.fr.text $impl_trace
    procInsertEventListIntoAppropriateListBox $pane.fr.leftFrame.fr.left.text $proc_accepts
}

proc procInsertEventListIntoAppropriateListBox {box list} {
    if {[llength $list] == 0} {
        $box insert end "\{ \}"
    } elseif {[llength $list] == 1} {
        $box insert end "\{ $list \}"
    } else {
        for {set i 0} {$i < [llength $list]} {incr i} {
            if {$i == 0} {
                $box insert end "\{ [lindex $list $i],"
            } elseif {$i == [expr {[llength $list] -1}] } {
                $box insert end "  [lindex $list $i] \}"
            } else {
                $box insert end "  [lindex $list $i],"
            }
        }
    }
}

proc Create_Pane_FailureRefinement {pane model Proc Impl proc_trace impl_trace accepts refuses {orient horizontal}} {
    global csp_assertions_dialog_arr
    message $pane.model -justify center -text "Model: $model" -aspect 1000
    frame $pane.fr -width 600 -height 250
    foreach f {leftFrame rightFrame} name {"Specification:" "Implementation:"} bg {LightBlue LightGreen} pr [list $Proc $Impl] {
        frame $pane.fr.$f -borderwidth .1c -relief groove
        message $pane.fr.$f.spec -text $name -aspec 1000
        message $pane.fr.$f.proc  -text [procLimitStringLength $pr 27] -background $bg -aspect 1000
    }
    frame $pane.fr.leftFrame.fr -borderwidth .1c -relief groove
    message $pane.fr.leftFrame.fr.title -text Trace -aspect 1000
    scrollbar $pane.fr.leftFrame.fr.scrolly -orient vertical -command "$pane.fr.leftFrame.fr.text yview"
    scrollbar $pane.fr.leftFrame.fr.scrollx -orient horizontal -command "$pane.fr.leftFrame.fr.text xview"
    listbox $pane.fr.leftFrame.fr.text -yscroll "$pane.fr.leftFrame.fr.scrolly set" \
       -xscroll "$pane.fr.leftFrame.fr.scrollx set" \
       -setgrid 1 -height 11 -width 30  -selectborderwidth 3 -selectbackground LightSkyBlue1 \
       -selectborderwidth 0 -selectforeground black -fg black -highlightcolor black -font {$csp_assertions_dialog_arr(fontname) 10} \
       -activestyle dotbox -selectmode browse
    pack $pane.fr.leftFrame.spec $pane.fr.leftFrame.proc -side top
    pack $pane.fr.leftFrame.fr.title -side top
    pack $pane.fr.leftFrame.fr.scrollx -side bottom -fill x
    pack $pane.fr.leftFrame.fr.scrolly -side right -fill y
    pack $pane.fr.leftFrame.fr.text -expand true -side top -fill both
    pack $pane.fr.leftFrame.fr -expand true -fill both

    frame $pane.fr.rightFrame.fr -borderwidth .1c -relief groove
    frame $pane.fr.rightFrame.fr.leftFr -relief flat

    pack $pane.fr.rightFrame.spec $pane.fr.rightFrame.proc -side top

    set leftFr $pane.fr.rightFrame.fr.leftFr
    foreach side {left right} titl {Trace Accepts} {
        frame $leftFr.$side -borderwidth .1c -relief groove
        message $leftFr.$side.title -text $titl -aspect 1000
        scrollbar $leftFr.$side.scrolly -orient vertical -command "$leftFr.$side.text yview"
        scrollbar $leftFr.$side.scrollx -orient horizontal -command "$leftFr.$side.text xview"
        listbox $leftFr.$side.text -yscroll "$leftFr.$side.scrolly set" \
          -xscroll "$leftFr.$side.scrollx set" \
          -setgrid 1 -height 11 -width 30  -selectborderwidth 3 -selectbackground LightSkyBlue1 \
          -selectborderwidth 0 -selectforeground black -fg black -highlightcolor black -font {$csp_assertions_dialog_arr(fontname) 10} \
          -activestyle dotbox -selectmode browse
        pack $leftFr.$side.title -side top
        pack $leftFr.$side.scrollx -side bottom -fill x
        pack $leftFr.$side.scrolly -side right -fill y
        pack $leftFr.$side.text -expand true -side top -fill both
    }
    pack $leftFr -expand true -fill both
    pack propagate $leftFr off
    Pane_Create $leftFr.left $leftFr.right -in $leftFr -orient $orient -percent 0.5 -cursor sb_h_double_arrow

    frame $pane.fr.rightFrame.fr.rightFr -borderwidth .1c -relief groove
    set rightFr $pane.fr.rightFrame.fr.rightFr
    message $rightFr.title -text Refuses -aspect 1000
    scrollbar $rightFr.scrolly -orient vertical -command "$rightFr.text yview"
    scrollbar $rightFr.scrollx -orient horizontal -command "$rightFr.text xview"
    listbox $rightFr.text -yscroll "$rightFr.scrolly set" \
       -xscroll "$rightFr.scrollx set" \
       -setgrid 1 -height 11 -width 30  -selectborderwidth 3 -selectbackground LightSkyBlue1 \
       -selectborderwidth 0 -selectforeground black -fg black -highlightcolor black -font {$csp_assertions_dialog_arr(fontname) 10} \
       -activestyle dotbox -selectmode browse
    pack $rightFr.title -side top
    pack $rightFr.scrollx -side bottom -fill x
    pack $rightFr.scrolly -side right -fill y
    pack $rightFr.text -expand true -side top -fill both
    pack $rightFr -expand true -fill both

    pack $pane.fr.rightFrame.fr -expand true -fill both
    pack propagate $pane.fr.rightFrame.fr off
    Pane_Create $leftFr $rightFr -in $pane.fr.rightFrame.fr -orient $orient -percent 0.66 -cursor sb_h_double_arrow

    pack $pane.model -side top
    pack $pane.fr -expand true -fill both
    pack propagate $pane.fr off
    Pane_Create $pane.fr.leftFrame $pane.fr.rightFrame -in $pane.fr -orient $orient -percent 0.25 -cursor sb_h_double_arrow

    procFillTraceDebuggerListBox $pane.fr.leftFrame.fr.text $proc_trace
    procFillTraceDebuggerListBox $pane.fr.rightFrame.fr.leftFr.left.text $impl_trace
    procInsertEventListIntoAppropriateListBox $leftFr.right.text $accepts
    procInsertEventListIntoAppropriateListBox $rightFr.text $refuses
}

proc procFillTraceDebuggerListBox {box list} {
    if {[llength $list] == 0} {
          $box insert end "Empty Trace"
    } else {
          foreach el [procPrettyPrintCSPTrace $list] {
              $box insert end [string map {"!" "\." "?" "\."} $el]
          }
    }
}

proc NegateLineInListBox {box} {
  global check_marks_arr
  foreach i [$box curselection] {
     regexp {([^a-zA-Z_]) [\ ]* (.*)} [$box get $i] match sym str
     set last [$box index end]
     if [regexp {^(not) (.*)$} $str m n str1] {
            if {[isInTheListBox $box $last "$sym   $str1"] != -1} {
                  tk_messageBox -parent .checkCspAsser -icon error\
                           -message "The negated version of this assertion is already in the list."
            } else {
                  $box insert $last "$check_marks_arr(unchecked)   $str1"
                  AppendStringToTheAssertionLists $str1
            }
     } else {
            if {[isInTheListBox $box $last "$sym   not $str"] != -1} {
                  tk_messageBox -parent .checkCspAsser -icon error\
                           -message "The negated version of this assertion is already in the list."
            } else {
                  $box insert $last "$check_marks_arr(unchecked)   not $str"
                  AppendStringToTheAssertionLists "not $str"
            }
     }
  }
}

proc procCheckAllAssertionsFromType {box ty} {
	global check_marks_arr
    for {set i 0} {$i < [$box index end]} {incr i} {
         regexp {([^a-zA-Z0-9_])[\ ]*(.*)} [$box get $i] match mark str
         if {$mark != "$check_marks_arr(success)" && $mark != "$check_marks_arr(failed)"} {
             set m [string first "\[$ty" $str]
             set d [string first "$ty" $str]
             if {$m != -1} {
                 HandleRefinementAssertion $box $str $i
             } elseif {$d !=-1 && $ty == "deterministic"} {
                 HandleRefinementAssertion $box $str $i
             } elseif {$d !=-1 && $ty == "livelock"} {
                 HandleRefinementAssertion $box $str $i
             } elseif {$d !=-1 && $ty == "deadlock"} {
                 HandleRefinementAssertion $box $str $i
             } elseif {$d !=-1 && $ty == "LTL"} {
                 HandleRefinementAssertion $box $str $i
             } elseif {$d !=-1 && $ty == "CTL"} {
                 HandleRefinementAssertion $box $str $i
             }
         }
    }
}

proc CheckAllAssertionsInTheListBox {box HandleCmd} {
	global check_marks_arr csp_assertions_dialog_arr
    set i 0
    if {[$box get 0] != "$csp_assertions_dialog_arr(empty_list_msg)"} {
       foreach ln [$box get 0 end] {
            regexp {([^a-zA-Z_])[\ ]*(.*)} [$box get $i] match mark str
            if {$mark != "$check_marks_arr(success)" && $mark != "$check_marks_arr(failed)"} {
                eval [list $HandleCmd $box $str $i]
            }
            incr i
       }
    }
}

# run a command in a list box, e.g., LTL or CTL formula check
# calls for example HandleLTLFormulaLine
proc ActivateLineInListbox {box CheckLineProc ShowDebugTraceProc empty_list_msg} {
	global check_marks_arr ltldata
    if {[$box get 0] != "$empty_list_msg"} {
       foreach i [$box curselection] {
           regexp {([^a-zA-Z_])[\ ]*(.*)} [$box get $i] match result str
           #puts "The Result of this assertion is $result"
           if {$result == "$check_marks_arr(unchecked)" || \
                   $result == "$check_marks_arr(checking)" || $result == "$check_marks_arr(incomplete)"} {
                eval [list $CheckLineProc $box $str $i]
          } else {
                eval [list $ShowDebugTraceProc $box]
          }
       }
    }
}

proc HandleRefinementAssertion {box str i} {
  global csp_assertions_dialog_arr
  if [procAssertionCurrentlyNotChecked $box] {
    ParseStringAssertions $box $str $i false
  }
}

proc ParseStringAssertions {box str i is_new} {
## possible input arguments for this procedure:
## Refinement Assertions: "P [m= Q" or "not P [m= Q" where P and Q are Processes and m can be T, F, FD or R
## Determinism Assertions: "P :[ deterministic [m]]" or "not P :[determinism [m]]" where m F or FD is and P is Process
## Deadlock Assertions: "P :[ deadlock free [m]]" or "not P :[deadlock free [m]]" m and P are the same as above
## Livelock Assertions: "P :[ livelock free [m]]" or "not P :[ livelock free [m]]" m and P are the same as above
## LTL Assertions: "P |= LTL: "ltl-formula"
## CTL Assertions: "P |= CTL: "ctl-formula"
## We use here regexp command
   global strFilename UsedCspStrFilename check_marks_arr
   if {![procIsAnyOfTheSubstringsInTheString $str\
	       $check_marks_arr(success) $check_marks_arr(failed) $check_marks_arr(checking)]} {
       if {!$is_new} {
           $box delete $i
       }
       $box insert $i "$check_marks_arr(checking)   $str"
       update
       $box see $i
       $box activate $i
       set Result ""
       catch {focus .checkCspAsser.frmSource.text}
       catch {grab .checkCspAsser}
       set csp_assertions_dialog_arr(assertion_check_running) 1
       procChangeCancelButtonsTextInCheckCspAsser "Interrupt"
       procChangeStateStatusOfCheckButtonsInCheckCspAsser "disabled"
       update
	   # need to replace every single escape character (using of the hiding operator in assertions) by double escape character,
	   # otherwise syntax_error(incorrect_escape_char) will occure on prolog side
	   set assertion "assert [string map {"\\" "\\\\" "'" "\\'"} $str]"
       if {[catch { prolog "tcltk_check_csp_assertion(\'$assertion\',\'$UsedCspStrFilename\',Negated,RR)"
                    set Result $prolog_variables(RR);update} result]} {
           catch {grab release .checkCspAsser}
           puts "An error occurred..."
           puts $result
           procShowErrors
           $box delete $i
           $box insert $i "$check_marks_arr(aborted)   $str"
           $box itemconfigure $i -foreground "red" -selectforeground "red"
           # error occurred by Assertion Check
       } else {
           catch {grab release .checkCspAsser}
           $box delete $i
           if [prolog real_error_occurred] {
               $box insert $i "$check_marks_arr(aborted)   $str"
               $box itemconfigure $i -foreground "red" -selectforeground "red"
           } else {
               if {$prolog_variables(Negated) == "True" } {
                   if {$Result == "no_counter_example"} {
                       $box insert $i "$check_marks_arr(failed)   $str"
                   } elseif {$Result == "interrupted"} {
                       $box insert $i "$check_marks_arr(aborted)   $str"
                   } else {
                       $box insert $i "$check_marks_arr(success)   $str"
                   }
               } else {
                   if {$Result == "no_counter_example"} {
                       $box insert $i "$check_marks_arr(success)   $str"
                   } elseif {$Result == "interrupted"} {
                       $box insert $i "$check_marks_arr(aborted)   $str"
                   } else {
                       #puts "Found a counter example: $Result."
                       $box insert $i "$check_marks_arr(failed)   $str"
                   }
               }
           }
           $box see $i
           procInsertHistoryOptionsState
           procShowErrors
       }
       set csp_assertions_dialog_arr(assertion_check_running) 0
       procChangeCancelButtonsTextInCheckCspAsser "Cancel"
       procChangeStateStatusOfCheckButtonsInCheckCspAsser "normal"
       update
  }
}

proc procIsOSAnOlderVersionOfWindowsMS {} {
  global tcl_platform
  if [prolog tools:host_platform(windows)] {
       if {$tcl_platform(osVersion) < 6.0 } {
           # WINDOWS XP
           return true
       }
  }
  return false
}

proc procChangeCancelButtonsTextInCheckCspAsser {text} {
       global tcl_version
       set m .checkCspAsser.refMenu
       if {$tcl_version >= 8.5} {
           foreach b [list $m.refinement $m.determinism $m.deadlock $m.livelock $m.ltl.line2 $m.ctl.line2] {
               $b.col3.cancel configure -text $text
           }
       } else {
           $m.col3.cancel configure -text $text
       }
}

proc procChangeStateStatusOfCheckButtonsInCheckCspAsser {status} {
       global tcl_version
       set m .checkCspAsser.refMenu
       if {$tcl_version >= 8.5} {
           foreach b [list $m.refinement $m.determinism $m.deadlock $m.livelock $m.ltl.line2 $m.ctl.line2] {
                $b.col2.check configure -state $status
           }
       } else {
           $m.col2.check configure -state $status
       }
}

proc procChangeFontSize {box fname num} {
    $box configure -font "$fname $num"
}

proc procChangeFontName {box fname fsize} {
    $box configure -font "$fname $fsize"
}

proc procChangeBackground {box bgr} {
    $box configure -bg  $bgr
}

proc procChangeSelectColor {box col} {
    $box configure -selectbackground $col
}

proc procUpdateListBoxIfExists {box must_be_checked is_reopen_process} {
   global csp_assertions_dialog_arr strFilename check_marks_arr
   global UsedCspStrFilename
   if [winfo exists $box] {
        set listStr [$box.frmSource.text get 0 end]
        $box.frmSource.text delete 0 end
        prolog "get_csp_assertions_as_string(\'$UsedCspStrFilename\',Res)"
        set Result [split [string map {"CSP: " ""} [string trimright $prolog_variables(Res) {$}]] \$]
        set csp_assertions_dialog_arr(listbox_size) [llength $Result]
        if {"$Result"==""} {
             $box.frmSource.text insert end $csp_assertions_dialog_arr(empty_list_msg)
             update
        } else {
             set currentIndex 0
             foreach i $Result {
                 if {$is_reopen_process} {
                     $box.frmSource.text insert end "$check_marks_arr(unchecked)   $i"
                 } else {
                     foreach j $listStr {
                         if [regexp {(TRUE|FALSE) \s* (.*)} $j match symbol str] {
                             # do nothing
                         } else {
                            regexp {^([^a-zA-Z_]) \s* (.*)} $j match symbol str
                         }
                         if {$str == $i} {
                             $box.frmSource.text insert end "$symbol   $i"
                             break
                             # there could be more than one assertion from this type in the listStr list
                         }
                     }
                 }
             }
             update
        }
   }
}

proc updateListBoxWithTheAppropriateAssertion {ListBox str must_be_checked} {
  global csp_assertions_dialog_arr
	AddTheNewDefinedStringInListBox $ListBox $str\
	     "HandleRefinementAssertion" "procShowCSPDebugTrace" "AppendStringToTheAssertionLists" .checkCspAsser\
		   "Assertion has been already checked." "Assertion is already in the List."\
		   $csp_assertions_dialog_arr(empty_list_msg) $must_be_checked
}

proc updateListSpec {Proc1 Proc2 Model ListBox must_be_checked} {
  global csp_assertions_dialog_arr
# test if options and proc are selected
    if {$Proc1 != "" && $Proc2 != "" && $Model != ""} {
       deleteFirstRowIfThereAreNoAssertionsInTheListbox $ListBox $csp_assertions_dialog_arr(empty_list_msg)
       set str ""
       set len [$ListBox index end]
       switch $Model {
          "Trace"   {set str "$Proc1 \[T= $Proc2"}
          "Failures" {set str "$Proc1 \[F= $Proc2"}
          "Failures-divergence" {set str "$Proc1 \[FD= $Proc2"}
          "Refusals" {set str "$Proc1 \[R= $Proc2"}
          "Refusals-divergence" {set str "$Proc1 \[RD= $Proc2"}
          default {}
       }
       updateListBoxWithTheAppropriateAssertion $ListBox $str $must_be_checked
    } else {
       tk_messageBox -parent .checkCspAsser -icon warning -message "No Process or Model has been selected."
    }
}

proc updateListDetermDeadlock {arrayName name1 name2 ListBox type must_be_checked} {
    global csp_assertions_dialog_arr
	  upvar 1 $arrayName arr
    set Proc $arr($name1);#[lindex [array get $array $name1] 1]
    set Model $arr($name2);#[lindex [array get $array $name2] 1]
    if {$Proc != "" && $Model != ""} {
      deleteFirstRowIfThereAreNoAssertionsInTheListbox $ListBox $csp_assertions_dialog_arr(empty_list_msg)
      set str ""
      set len [$ListBox index end]
      switch $Model {
           "Failures" {set str "$Proc :\[ $type \[F\] \]"}
           "Failures-divergence" {set str "$Proc :\[ $type \[FD\] \]"}
           default {}
      }
      updateListBoxWithTheAppropriateAssertion $ListBox $str $must_be_checked
    } else {
      tk_messageBox -parent .checkCspAsser -icon warning -message "No Process or Model has been selected."
    }
}

proc updateListLivelock {arrayName name ListBox must_be_checked} {
  global csp_assertions_dialog_arr
   upvar 1 $arrayName arr
   set Proc $arr($name);#[lindex [array get $array $name] 1]
   if {$Proc != "" } {
      deleteFirstRowIfThereAreNoAssertionsInTheListbox $ListBox $csp_assertions_dialog_arr(empty_list_msg)
      set len [$ListBox index end]
      set str "$Proc :\[ livelock free \]"
      updateListBoxWithTheAppropriateAssertion $ListBox $str $must_be_checked
   } else {
       tk_messageBox -parent .checkCspAsser -icon warning -message "No Process or Model has been selected."
   }
}

proc updateListLTLCTL {arrayName name formula type info ListBox must_be_checked} {
  global csp_assertions_dialog_arr
   upvar 1 $arrayName arr
   set Proc $arr($name);#[lindex [array get $array $name] 1]
   set Formula $arr($formula)
   if {$Proc != "" && $Formula != "" && $Formula != $info} {
      deleteFirstRowIfThereAreNoAssertionsInTheListbox $ListBox $csp_assertions_dialog_arr(empty_list_msg)
      set len [$ListBox index end]
      set str "$Proc |= $type: \"$Formula\""
      updateListBoxWithTheAppropriateAssertion $ListBox $str $must_be_checked
   } else {
       tk_messageBox -parent .checkCspAsser -icon warning -message "No Process or $type formula has been selected/entered."
   }
}

proc deleteFirstRowIfThereAreNoAssertionsInTheListbox {ListBox empty_msg_line} {
   if {[$ListBox get 0] == "$empty_msg_line"} {
        $ListBox delete 0
   }
}

proc AddTheNewDefinedStringInListBox {ListBox str checkLineProc runDebuggerProc appendLineToArrayListProc parent msg1 msg2 empty_list_msg must_be_checked} {
    global check_marks_arr
    set len [$ListBox index end]
    set ind [isInTheListBox $ListBox $len "$check_marks_arr(unchecked)   $str"]
    if {$ind == -1} {
        eval [list $appendLineToArrayListProc $str]
		$ListBox insert end "$check_marks_arr(unchecked)   $str"
        if {$must_be_checked} {
            $ListBox selection clear 0 end
            $ListBox selection set end
            ActivateLineInListbox $ListBox "$checkLineProc" "$runDebuggerProc" "$empty_list_msg"
        }
        } else {
           if [procIsOSAnOlderVersionOfWindowsMS] {
               regexp {^(TRUE|FALSE) \s* (.*)} [$ListBox get $ind] match symbol str
           } else {
               regexp {^([^a-zA-Z_]) \s* (.*)} [$ListBox get $ind] match symbol str
           }
           if {$symbol == "$check_marks_arr(success)" || $symbol == "$check_marks_arr(failed)" || $symbol == "FALSE" || $symbol == "TRUE"} {
               tk_messageBox -parent $parent -icon warning -message $msg1
           } else {
               tk_messageBox -parent $parent -icon warning -message $msg2
               $ListBox selection clear 0 end
               $ListBox selection set $ind
               if {$must_be_checked} {
                   ActivateLineInListbox $ListBox "$checkLineProc" "$runDebuggerProc" "$empty_list_msg"
               }
           }
        }
}

proc AppendStringToTheAssertionLists {str} {
   global csp_assertions_dialog_arr
   lappend csp_assertions_dialog_arr(current_new_assertions) $str
   lappend csp_assertions_dialog_arr(all_new_assertions) $str
}

proc isInTheListBox {lbox len str} {
    for {set i 0} {$i < $len} {incr i 1} {
		set nstr [escapeSpecialRegExpChars $str]
         if [procIsOSAnOlderVersionOfWindowsMS] {
             if {[regexp {^(TRUE|FALSE|\?) \s* (.*)} $nstr match ignore new_str] &&
                 [regexp {^(TRUE|FALSE|\?) \s* (.*)} [$lbox get $i] match ignore lstr]} {
			         if {$lstr == [revertEscapeSpecialRegExpChars $new_str]} {
			             return $i
			         }
				 }
         } else {
             if {[regexp {^([^a-zA-Z_]) \s* (.*)} $nstr match ignore new_str] &&
                 [regexp {^([^a-zA-Z_]) \s* (.*)} [$lbox get $i] match ignore lstr]} {
			         if {$lstr == [revertEscapeSpecialRegExpChars $new_str]} {
			             return $i
			         }
				 }
         }
    }
    return -1
}

proc escapeSpecialRegExpChars {str} {
    return [string map {
		       "\[" "\\\[" "\]" "\\\]" "\{" "\\\{" "\}" "\\\}"} $str]
}

proc revertEscapeSpecialRegExpChars {str} {
    return [string map {
		       "\\\[" "\[" "\\\]" "\]" "\\\{" "\{" "\\\}" "\}"} $str]
}


######### The List Box Dialog #########

proc procOpenDialog {} {
  set l [list bli bla blupp]; # also added for testing (will be removed later)
  procDialogListBox "testcases" "Test Cases" "List of Test Cases" "$l" \
                    procDlgAction procDlgDebug DlgContextMenu "No Test Cases generated"
}

#### Creating a dialog in which a number of entries will be listed and can be later checked
## Arguments:
##### Dialogname - name of the widget
##### Title - title of the dialog
##### Msg - a message which will be placed on the top of the list box
##### List - the list of entries
##### Action - the name of the procedure that should handle the entries,
#####         procedure should take 3 parameters: 1. name of the list box, 2. the entry, 3. the index of the entry in the list box
##### Debug - the name of the procedure for debugging entries which checks has failed
##### CtxMenu - the name of the procedure creating the popup menu,
#####           procedure should take 5 parameters:
#####                1. the name of the context menu's widget
#####                2. the name of the list box widget
#####                3 + 4. the coordinates (X Y)
#####                5. the empty list message (needed in case there are no entries in the list box)
##### EmptyListMsg - the empty list message (needed in case there are no entries in the list box),
#####                e.g. 'No Assertions in this File found.' when no assertions in the CSP file have been found
proc procDialogListBox {Dialogname Title Msg List Action Debug CtxMenu EmptyListMsg} {
  set dlg .$Dialogname
  destroy $dlg

  if [Dialog_Create $dlg $Title -borderwidth 10] {
      set lb $dlg.frmSource.text

      createListBoxViewer $dlg $Msg "Gray90" "TkFixedFont" $Action $Debug $CtxMenu $EmptyListMsg

      procInsertEntries $lb $List $EmptyListMsg

  }
}

proc procDlgAction {box str i} {
  global check_marks_arr
  $box delete $i
  $box insert $i "$check_marks_arr(checking)   $str"
  update
  $box see $i
  $box activate $i
  update
  #### Here comes the prolog call and the implementation for reading the result from prolog
  set Result [getRandomResult] ; # just for testing (getRandomResult will be removed later)
  set Success 1
  set Failure 0
  ###

  $box delete $i
  if {$Result==$Success} {
      $box insert $i "$check_marks_arr(success)   $str"
  } elseif {$Result==$Failure} {
      $box insert $i "$check_marks_arr(failed)   $str"
  } else {
      $box insert $i "$check_marks_arr(aborted)   $str"
  }
}

proc procDlgDebug {box} {
  puts "Do some debugging"
}

# created just to test procOpenDialog
proc getRandomResult {} {
  if {[expr rand()] >= 0.5} {
    return 1
  } else {
    return 0
  }
}

## the context menu
proc DlgContextMenu {ctx_name box X Y empty_list_msg} {
     if {[$box get 0] != $empty_list_msg} {
        set listy [winfo rooty $box]
        set ycoord [expr $Y -$listy]
        set index [$box nearest $ycoord]
        $box selection clear 0 end
        $box selection set $index
        $box activate $index
        CreatePopupDlgMenu $ctx_name $box $empty_list_msg
        tk_popup $ctx_name $X $Y
     }
}

proc CreatePopupDlgMenu {ctx ListBox EmptyListMsg} {
  destroy $ctx
  menu $ctx -tearoff 0
  set checkState [procSeeIfTheSelectedAssertionIsChecked $ListBox true]
  set uncheckState [procSeeIfTheSelectedAssertionIsChecked $ListBox false]
  set debugState [procSeeIfAssertionExploredAndHolds $ListBox yes]
  $ctx add command -label "Check Formula" \
            -command [list ActivateLineInListbox $ListBox "procDlgAction" "procDlgDebug" $EmptyListMsg] \
            -state $checkState
  $ctx add command -label "Uncheck Formula" -command [list procUncheckACheckedAssertion $ListBox] -state $uncheckState
  $ctx add command -label "Delete Formula" -command [list procDeleteAssertion $ListBox $EmptyListMsg] -state normal
  $ctx add sep
  $ctx add command -label "Check All" -command [list CheckAllAssertionsInTheListBox $ListBox "procDlgAction"] -state normal
}

proc procInsertEntries {ListBox List EmptyListMsg} {
    global check_marks_arr
    if {$List == ""} {
      $ListBox insert end $EmptyListMsg
    }
    foreach i $List {
      $ListBox insert end "$check_marks_arr(unchecked)   $i"
    }
}

###############################################################################

proc OpenLtlAssertionsViewer {} {
	global ltldata
    destroy .ltlViewer
	set ltl .ltlViewer
	if [Dialog_Create $ltl "Check LTL/CTL Assertions" -borderwidth 10] {

		############## Menu Bar #################
		menu $ltl.menubar -tearoff 0
		$ltl config -menu $ltl.menubar

		foreach m {File Font Assertions Preferences Info OldLtlViewers} {
            set mb [string tolower $m]
            set $m [menu $ltl.menubar.$mb -tearoff 0]
            $ltl.menubar add cascade -label $m -menu $ltl.menubar.$mb
		}
		set lb $ltl.frmSource.text
		array set arr_file_ltl_commands {"AReopen File" procGenericLoadFile
						 "BSave LTL Assertions to External File..." "saveAssertionsListToFile .ltlViewer .ltlViewer.frmSource.text"
                                                 "CImport LTL Assertions from File..." "addAssertionsToViewer .ltlViewer .ltlViewer.frmSource.text"
						 "DExit" "destroy .ltlViewer"}

		procAddSimpleMenuBar $ltl.menubar.file arr_file_ltl_commands
	    procAddFontMenuBar $lb $ltl.menubar.font ltldata

		array set arr_ass_ltl_commands {"ACheck All Assertions" "CheckAllAssertionsInTheListBox .ltlViewer.frmSource.text HandleLTLFormulaLine"
						"BCheck All LTL Assertions" "CheckAllLTLAssertionsInTheListBox .ltlViewer.frmSource.text"
						"CCheck All CTL Assertions" "CheckAllCTLAssertionsInTheListBox .ltlViewer.frmSource.text"}

		procAddSimpleMenuBar $ltl.menubar.assertions arr_ass_ltl_commands

		array set arr_info_commands {"ASupported LTL Syntax" "procLaunchSyntaxInfo LTL"
					     "BSupported CTL Syntax" "procLaunchSyntaxInfo CTL"}
 		procAddSimpleMenuBar $ltl.menubar.info arr_info_commands

		array set arr_pref_commands {"ALTL Preferences" "procLaunchLTL_CTLPreferences ltldata LTL"
					     "BCTL Preferences" "procLaunchLTL_CTLPreferences ctldata CTL"}

  		procAddSimpleMenuBar $ltl.menubar.preferences arr_pref_commands

        array set arr_old_ltl_viewers {"ACheck LTL Formula..." procLtl
				       "BCheck CTL Formula..." procCtl}

   		procAddSimpleMenuBar $ltl.menubar.oldltlviewers arr_old_ltl_viewers

	   ############# List Box Part #################
        if [prolog animation_mode(b)] {
          set ltldata(empty_list_msg)  "No ASSERT_LTL/CTL in the DEFINITIONS."
        } elseif [prolog animation_mode(cspm)] {
          set ltldata(empty_list_msg)  "No ASSERT_LTL_ inside of pragmas in the CSP model."
        } else {
          set ltldata(empty_list_msg) "LTL Assertions are only supported for B and CSP models at the moment."
        }

	    createListBoxViewer $ltl "List of LTL/CTL Formulas" "Gray90" "TkFixedFont" \
		     "HandleLTLFormulaLine" "getLTLDebuggerInfo" "ltlContextMenu" $ltldata(empty_list_msg)

		############# Bottom Menu ###################
		ttk::notebook $ltl.menu
		foreach fr {ltlformula ctlformula} name {"Add LTL Formula" "Add CTL Formula"} {
		     ttk::frame $ltl.menu.$fr
			 $ltl.menu add $ltl.menu.$fr -text $name
		}
		set add1 $ltl.menu.ltlformula
		set add2 $ltl.menu.ctlformula

		set ltldata(ltl_viewer_formula) $ltldata(ltl_info)
    set ltldata(ctl_viewer_formula) $ltldata(ctl_info)

		procCreateEntryAddCheckMenu $add1 "LTL Formula:"\
		[list updateLTLListBox ltldata ltl_viewer_formula $lb false empty_list_msg]\
		[list updateLTLListBox ltldata ltl_viewer_formula $lb true  empty_list_msg]\
		{destroy .ltlViewer} ltldata(ltl_viewer_formula)\
		$ltldata(ltl_info) ltldata ltl_viewer_formula

		procCreateEntryAddCheckMenu $add2 "CTL Formula:"\
		[list updateLTLListBox ltldata ctl_viewer_formula $lb false empty_list_msg]\
		[list updateLTLListBox ltldata ctl_viewer_formula $lb true  empty_list_msg]\
		{destroy .ltlViewer} ltldata(ctl_viewer_formula)\
		$ltldata(ctl_info) ltldata ctl_viewer_formula

		pack $ltl.menu -side top -fill both

		procInsertLTLFormulasFromModel $lb $ltldata(empty_list_msg)

		wm geometry .ltlViewer 88x12
	}
}

proc ltlContextMenu {ctx_name box X Y empty_list_msg} {
     if {[$box get 0] != $empty_list_msg} {
        set listy [winfo rooty $box]
        set ycoord [expr $Y -$listy]
        set index [$box nearest $ycoord]
        $box selection clear 0 end
        $box selection set $index
        $box activate $index
        CreatePopupLTLCTLMenu $ctx_name $box $empty_list_msg
        tk_popup $ctx_name $X $Y
     }
}

proc CreatePopupLTLCTLMenu {ctx_name box empty_list_msg} {
  destroy $ctx_name
  menu $ctx_name -tearoff 0
  set checkState [procSeeIfTheSelectedAssertionIsChecked $box true]
  set uncheckState [procSeeIfTheSelectedAssertionIsChecked $box false]
  set debugState [procSeeIfLTLAssertionExploredAndHolds $box]
  $ctx_name add command -label "Check Formula" \
            -command [list ActivateLineInListbox $box "HandleLTLFormulaLine" "getLTLDebuggerInfo" $empty_list_msg] \
            -state $checkState
  $ctx_name add command -label "History to Current State" -command {procDisplayTraceToCurrentState "no"} -state $debugState
  $ctx_name add command -label "Uncheck Formula" -command [list procUncheckACheckedAssertion $box] \
                        -state $uncheckState
  $ctx_name add command -label "Copy Formula" -command [list procCopyAssertion $box] -state normal
  $ctx_name add command -label "Delete Formula" -command [list procDeleteAssertion $box $empty_list_msg] -state normal
  $ctx_name add sep
  $ctx_name add command -label "Check All Assertions" -command [list CheckAllAssertionsInTheListBox $box "HandleLTLFormulaLine"] -state normal
  $ctx_name add command -label "Check All LTL Assertions" -command [list CheckAllLTLAssertionsInTheListBox $box] -state normal
  $ctx_name add command -label "Check All CTL Assertions" -command [list CheckAllCTLAssertionsInTheListBox $box] -state normal
}


proc procLaunchLTL_CTLPreferences {arrayName title} {
    global ltldata
    destroy .ltlPref
    if [Dialog_Create .ltlPref "$title Preferences" -borderwidth 10] {
        wm resizable .ltlPref 0 0
        wm transient .ltlPref .ltlViewer

		frame .ltlPref.body -borderwidth 3 -relief raised
        set mpref .ltlPref.body

		frame $mpref.states
		message $mpref.states.msg -text "Max no. of new states:" -borderwidth 3 -aspect 1000
		if {$title == "LTL"} {entry $mpref.states.max -textvariable ltldata(max) -width 9
		} else {entry $mpref.states.max -textvariable ctldata(max) -width 9}
		pack $mpref.states.msg $mpref.states.max -side left

		frame $mpref.modi
		message $mpref.modi.msg -text "Start search in:" -aspect 10000

		frame $mpref.modi.start
		if {$title == "LTL"} {
			radiobutton $mpref.modi.start.init -variable ltldata(mode) -value "init" \
			    -text "initialisation"
			radiobutton $mpref.modi.start.starthere -variable ltldata(mode) -value "starthere" \
			    -text "current state"
    		radiobutton $mpref.modi.checkhere -variable ltldata(mode) -value "checkhere" \
    			-text "initialisation, but check formula in current state"
		} else {
			radiobutton $mpref.modi.start.init -variable ctldata(mode) -value "init" \
			    -text "initialisation"
			radiobutton $mpref.modi.start.starthere -variable ctldata(mode) -value "starthere" \
			    -text "current state"
		}
        #ttk::checkbutton $mpref.por -text "+ Partial Order Reduction"\
        #    -variable ltldata(use_por_for_ltl) -offvalue false -onvalue true\
        #    -command {prolog set_preference(use_por_for_ltl,$ltldata(use_por_for_ltl))}

        # ttk::checkbutton $mpref.safety -text "With Safety Check"\
        #     -variable ltldata(use_safety_ltl_model_checker) -offvalue false -onvalue true\
        #     -command {prolog set_preference(use_safety_ltl_model_checker,$ltldata(use_safety_ltl_model_checker))}

        ttk::checkbutton $mpref.safety -text "With Safety Check"\
            -variable ltldata(use_safety_ltl_model_checker) -offvalue false -onvalue true\
            -command {prolog preferences:set_preference(use_safety_ltl_model_checker,$ltldata(use_safety_ltl_model_checker))}

		pack $mpref.modi.start.init $mpref.modi.start.starthere -side left -padx 20

    pack $mpref.modi.msg $mpref.modi.start -side top -pady 3

    if {$title == "LTL"} {
      pack $mpref.modi.checkhere -pady 3
    }

		pack $mpref.states -side top -pady 5

#     if {$title == "LTL"} {
#       pack $mpref.por -side top -pady 5
#       pack $mpref.safety -side top -pady 5
#     }

    pack $mpref.modi -side top -pady 5

		pack $mpref
    }
}

proc CheckAllLTLAssertionsInTheListBox {ListBox} {
    global ltldata
	set ltldata(checkall_type) ltl
	CheckAllAssertionsInTheListBox $ListBox HandleLTLFormulaLine
	set ltldata(checkall_type) no
}

proc CheckAllCTLAssertionsInTheListBox {ListBox} {
    global ltldata
	set ltldata(checkall_type) ctl
	CheckAllAssertionsInTheListBox $ListBox HandleLTLFormulaLine
	set ltldata(checkall_type) no
}

proc procUpdateLTLViewerContents {} {
    global ltldata
	set empty_list_msg ""
    if [prolog animation_mode(b)] {
      set ltldata(empty_list_msg)  "No ASSERT_LTL/CTL in the DEFINITIONS."
      #$f.frmSource.text insert end "Add DEFINITIONS of the form ASSERT_LTLnr == \"LTL FORMULA\" to your B model."
    } elseif [prolog animation_mode(cspm)] {
      set ltldata(empty_list_msg)  "No ASSERT_LTL_ inside of pragmas in the Csp model."
      #$f.frmSource.text insert end "Add pragmas of the form {-# ASSERT_LTL_nr == \"LTL FORMULA\" #-} to your Csp model."
    } else {
      set ltldata(empty_list_msg) "LTL Assertions are only supported for B and Csp models at the moment."
    }
    if [winfo exists .ltlViewer] {
		.ltlViewer.frmSource.text delete 0 end
		procInsertLTLFormulasFromModel .ltlViewer.frmSource.text $ltldata(empty_list_msg)
	}
}

proc getLTLDebuggerInfo {box} {
    if {[procSeeIfAssertionExploredAndHolds $box no] == "normal"} {
        procDisplayTraceToCurrentState "no"
    } else {
        tk_messageBox -parent .ltlViewer -icon info -message "Formula TRUE.\nNo counterexample found."
    }
}

proc getLTLDebuggerInfo_CE {box str type} {
  global UsedCspStrFilename
  if {[procSeeIfAssertionExploredAndHolds $box no] == "normal"} {
      set assertion "assert [string map {"\\" "\\\\" "'" "\\'"} $str]"
      prolog "tcltk_play_ltl_ctl_counterexample_trace(\'$assertion\',\'$UsedCspStrFilename\',$type,FairnessChecked)"
      set fairness_checked $prolog_variables(FairnessChecked)
      if {$fairness_checked == yes} {
        procDisplayTraceToCurrentStateWithNeighbors
      } else {
        procDisplayTraceToCurrentState "no"
      }
      procInsertHistoryOptionsState
  } else {
      tk_messageBox -parent .checkCspAsser -icon info -message "Assertion declaration is TRUE."
  }
}

proc procInsertLTLFormulasFromModel {box empty_list_msg} {
	global ltldata check_marks_arr
    set Cmd ""
    if [prolog animation_mode(cspm)] {
  	    set Cmd "haskell_csp:get_formulas_from_cspm_file(ltl,CSPRes1),\
                 haskell_csp:get_formulas_from_cspm_file(ctl,CSPRes2)"
    } elseif [prolog animation_mode(csp_and_b)] {
        set Cmd "haskell_csp:get_formulas_from_cspm_file(ltl,CSPRes1),\
                 haskell_csp:get_formulas_from_cspm_file(ctl,CSPRes2),\
                 b_get_definition_string_list_from_spec('ASSERT_LTL',BRes1),\
                 b_get_definition_string_list_from_spec('ASSERT_CTL',BRes2)"
  	} else {
        set Cmd "b_get_definition_string_list_from_spec('ASSERT_LTL',BRes1),\
                 b_get_definition_string_list_from_spec('ASSERT_CTL',BRes2)"
    }
	set ltldata(ltl_formulas) {}
	set ltldata(ctl_formulas) {}
	# comments for Infolog:
  # prolog haskell_csp:get_formulas_from_cspm_file(A,B)
  # prolog b_get_definition_string_list_from_spec(A,B)
    if [prolog "$Cmd"] {
	    if [prolog animation_mode(cspm)] {
  	        set Result [procGetPrologTextResultAsList $prolog_variables(CSPRes1) ";"]
            set ltldata(ltl_formulas) $Result
            set Result2 [procGetPrologTextResultAsList $prolog_variables(CSPRes2) ";"]
            set ltldata(ctl_formulas) $Result2
            foreach str $Result2 {
				lappend Result $str
			}
		} elseif [prolog animation_mode(csp_and_b)] {
            set Result  [procGetPrologTextResultAsList $prolog_variables(CSPRes1) ";"]
            set BResult [procGetPrologTextResultAsList $prolog_variables(BRes1) ";"]
		    foreach str $BResult {
				lappend Result $str
			}
			set ltldata(ltl_formulas) $Result
            set Result2 [procGetPrologTextResultAsList $prolog_variables(CSPRes2) ";"]
			set BResult2 [procGetPrologTextResultAsList $prolog_variables(BRes2) ";"]
		    foreach str $BResult2 {
				lappend  Result2 $str
			}
			set ltldata(ctl_formulas) $Result2
		    foreach str $Result2 {
				lappend  Result $str
			}
        } else {
            set Result [procGetPrologTextResultAsList $prolog_variables(BRes1) ";"]
			set ltldata(ltl_formulas) $Result
			set Result2 [procGetPrologTextResultAsList $prolog_variables(BRes2) ";"]
			set ltldata(ctl_formulas) $Result2
		    foreach str $Result2 {
				lappend Result $str
			}
		}
		#puts "Result1: $Result1, Result2: $Result2"
		if {$Result == ""} {
			$box insert end $empty_list_msg
		}
        foreach i $Result {
				 $box insert end "$check_marks_arr(unchecked)   $i"
        }
    } else {
        procShowErrors
        $box insert end $empty_list_msg
    }
}

proc updateLTLListBox {arrayName name ListBox must_be_checked empty_list_var} {
    upvar 1 $arrayName arr
	if {$name == "ctl_viewer_formula"} {
	    set arr(type) ctl;#array set $array [list type ctl]
	} else {
	    set arr(type) ltl;#array set $array [list type ltl]
	}
    set Formula $arr($name)
    if {$Formula != "" && $Formula != $arr(ltl_info)\
	                     && $Formula != $arr(ctl_info)} {
       set pp_formula [procParseAndPrettyPrintFormula $ListBox $Formula]
       if {$pp_formula != "syntax error"} {
             deleteFirstRowIfThereAreNoAssertionsInTheListbox $ListBox $arr($empty_list_var)
             set len [$ListBox index end]
             updateListBoxWithTheAppropriateLTLFormula $ListBox $pp_formula $must_be_checked $arr($empty_list_var)
       }
    } else {
        tk_messageBox -parent .ltlViewer -icon warning -message "No Fomula has been typed."
    }
}

proc updateListBoxWithTheAppropriateLTLFormula {ListBox formula must_be_checked empty_list_msg} {
        AddTheNewDefinedStringInListBox $ListBox $formula\
	       "HandleLTLFormulaLine" "getLTLDebuggerInfo" "AppendStringToTheLTLFormulasList" .ltlViewer\
		   "Formula has been already checked." "Formula is already in the List Box."\
		   $empty_list_msg $must_be_checked
}

proc procParseAndPrettyPrintFormula {ListBox formula} {
    global ltldata
    set eformula [escapeChars $formula]
    if {$ltldata(type) == "ltl"} {
        puts "Parsing LTL"
        if [prolog "ltl: parse_and_pp_ltl_formula(\'$eformula\',Text)"] {
            puts "Parsed LTL Formula"
            return $prolog_variables(Text)
        } else {
            tk_messageBox -parent $ListBox -icon error -message "Invalid LTL Formula!"
            procShowErrors
            return "syntax error"
        }
    } elseif {$ltldata(type) == "ctl"} {
        if [prolog "ctl: parse_and_pp_ctl_formula(\'$eformula\',Text)"] {
            return $prolog_variables(Text)
        } else {
            tk_messageBox -parent $ListBox -icon error -message "Invalid CTL Formula!"
            procShowErrors
            return "syntax error"
        }
    }
}

proc AppendStringToTheLTLFormulasList {str} {
	global ltldata
	if {$ltldata(type) == "ltl"} {
	    lappend ltldata(ltl_formulas) $str
	} elseif {$ltldata(type) == "ctl"} {
	    lappend ltldata(ctl_formulas) $str
	} else {
	    # no formula need to be added
	}
}

# for Infolog
# prolog ctl_model_check(A,B,C,D)
# prolog ltl_model_check(A,B,C,D)
proc proc_getLTLCTLCmd {formula} {
    global ltldata ctldata
    set continue true; set pos_result ""; set cmd ""
    set eformula [escapeChars $formula]
    # puts "CTL: $ltldata(ctl_formulas)"
    # puts "LTL: $ltldata(ltl_formulas)"
    # puts "$ltldata(checkall_type)"
    if {$ltldata(checkall_type) == "ctl"} {
		if {[lsearch -exact $ltldata(ctl_formulas) $formula] >-1} {
	        set cmd "ctl_model_check(\'$eformula\',$ctldata(max),$ctldata(mode),Res)"
			set pos_result "true"
		} else {
			set continue false
		}
	} elseif {$ltldata(checkall_type) == "ltl"} {
		if {[lsearch -exact $ltldata(ltl_formulas) $formula] >-1} {
	       set cmd "ltl_model_check(\'$eformula\',$ltldata(max),$ltldata(mode),Res)"
			set pos_result "ok"
		} else {
			set continue false
		}
	} else {
		# ltldata(type) keeps track from which tab menu (Add LTL Formula/Add CTL Formula) the formula is comming
	    if {[lsearch -exact $ltldata(ctl_formulas) $formula] >-1} {
	       set cmd "ctl_model_check(\'$eformula\',$ctldata(max),$ctldata(mode),Res)"
		   set pos_result "true"
	    } elseif {[lsearch -exact $ltldata(ltl_formulas) $formula] >-1} {
	       set cmd "ltl_model_check(\'$eformula\',$ltldata(max),$ltldata(mode),Res)"
		   set pos_result "ok"
	    } else {
	       set continue false
	    }
    }
    return [list $continue $cmd $pos_result]
}

proc HandleLTLFormulaLine {box formula i} {
	global ltldata ctldata check_marks_arr
	if {$formula != ""} {
        set cmdlist [proc_getLTLCTLCmd $formula]
        set continue [lindex $cmdlist 0]; set cmd [lindex $cmdlist 1];
        set pos_result [lindex $cmdlist 2]
		if {$continue} {
		    $box delete $i
			$box insert $i "$check_marks_arr(checking)  $formula"
			update
			$box see $i
			$box activate $i
		    set Result ""
			update
			puts "LTL/CTL Predicate call: $cmd"
		    if [prolog $cmd] {
			   set res $prolog_variables(Res)
			   #puts "Result: $res"
		       if {$res == "syntax_error"} {
	 			   $box delete $i
	         $box insert $i "$check_marks_arr(aborted)   $formula"
	 			   $box itemconfigure $i -foreground red
			       tkErrorBox "Internal error: LTL model checking failed."
				   procShowErrors
			   } else {
				   $box delete $i
				   if {$res == $pos_result} {
	 	               $box insert $i "$check_marks_arr(success)   $formula"
	 	           } elseif {$res == "incomplete"} {
                       $box insert $i "$check_marks_arr(incomplete)   $formula"
                   } else {
	 	               $box insert $i "$check_marks_arr(failed)   $formula"
	 	           }
				       procInsertHistoryOptionsState
			   }
			} else {
 			   $box delete $i
        $box insert $i "$check_marks_arr(aborted)   $formula"
 			   $box itemconfigure $i -foreground red
			   tkErrorBoxNoParent "Model Check ($ltldata(type)) failed for : $formula"
			}
			procShowErrors
	}
   }
}

proc CheckLTLFormulaFromPopupMenu {} {
	global ltldata
	set sel [selection get]
	if {$sel != ""} {
		regsub -all -- \[\r\n\t] $sel " " sel
		set CheckSelection [escapeChars $sel]
		if [prolog ltl_model_check('$CheckSelection',$ltldata(max),$ltldata(mode),Res)] {
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
	}
	selection clear
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

proc procUpdateLtlFormulaHistory {} {
	global ltldata
	#puts "updating ltl history $ltldata(formula)"
	if {$ltldata(formula) != ""} {
    set eFormula [escapeChars $ltldata(formula)]
		prolog preferences:add_element_to_history(checked_ltl_formulas,'$eFormula',10)
		set tmpList [list $ltldata(formula)]
	} else {
		set tmpList {}
	}
	foreach i $ltldata(hist) {
		if {$ltldata(formula) != $i} {
			lappend tmpList $i
		}
	}
	set ltldata(hist) $tmpList
	.ltl.formula configure -values [lrange $tmpList 0 9]
}

proc LtlDialog {title msg} {
    global ltldata strFilename
    destroy .ltl
    prolog preferences:get_history_elements_list(checked_ltl_formulas,true,Formulas)
    set ltldata(hist) $prolog_variables(Formulas)
    if [Dialog_Create .ltl $title -borderwidth 10] {
	message .ltl.msg -text $msg -aspect 1000
	ttk::combobox .ltl.formula -textvariable ltldata(formula) -width 40 -state editable -value $ltldata(hist)
	set CMD [procGetAccKey]
	if {$ltldata(formula) == ""} {
	 	    if [prolog b_get_definition_string_from_spec('ASSERT_LTL',_,Res)] {
	 	        set ltldata(formula) $prolog_variables(Res)
	 	    } elseif {[prolog animation_mode(cspm)] && [prolog haskell_csp:get_formulas_from_cspm_file(ltl,Res)]} {
			#set ltldata(formula) $prolog_variables(Res)
		    }
  	}

	frame .ltl.states
	message .ltl.states.msg -text "Max no. of new states:" -aspect 1000
	entry .ltl.states.max -textvariable ltldata(max) -width 9
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
	button .ltl.buttons.ok -text OK -command {procUpdateLtlFormulaHistory; set ltldata(ok) 1}
	button .ltl.buttons.cancel -text Cancel -command {set ltldata(ok) 0}
	pack .ltl.buttons.cancel -side left
	pack .ltl.buttons.ok -side right
	set CMD [procGetAccKey]
    bind .ltl.formula <$CMD-a> {after idle {.ltl.formula selection range 0 end}}
    bind .ltl.formula <$CMD-A> {after idle {.ltl.formula selection range 0 end}}
	bind .ltl <Return> {procUpdateLtlFormulaHistory; set ltldata(ok) 1 ; break}
	bind .ltl <Control-c> {set ltldata(ok) 0 ; break}
    bind .ltl <Escape> {set ltldata(ok) 0;break}
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

proc CtlDialog {title msg} {
    global ctldata strFilename
    destroy .ctl
    if [Dialog_Create .ctl $title -borderwidth 10] {
        message .ctl.msg -text $msg -aspect 1000
        entry .ctl.formula -textvariable ctldata(formula) -width 40

        if {$ctldata(formula) == ""} {
            if [prolog b_get_definition_string_from_spec('ASSERT_CTL',_,Res)] {
                set ltldata(formula) $prolog_variables(Res)
            }
        }

        frame .ctl.states
        message .ctl.states.msg -text "Max no. of new states:" -aspect 1000
        entry .ctl.states.max -textvariable ctldata(max) -width 9
        pack .ctl.states.msg .ctl.states.max -side left

        frame .ctl.modi
        radiobutton .ctl.modi.init -variable ctldata(mode) -value "init" \
            -text "Start search in initialisation"
        radiobutton .ctl.modi.starthere -variable ctldata(mode) -value "starthere" \
            -text "Start search in current state"
        pack .ctl.modi.init .ctl.modi.starthere -anchor w

        frame .ctl.buttons
        button .ctl.buttons.ok -text OK -command {set ctldata(ok) 1}
        button .ctl.buttons.cancel -text Cancel -command {set ctldata(ok) 0}
        pack .ctl.buttons.cancel -side left
        pack .ctl.buttons.ok -side right
        bind .ctl <Return> {set ctldata(ok) 1 ; break}
        bind .ctl <Control-c> {set ctldata(ok) 0 ; break}

        pack .ctl.msg .ctl.formula .ctl.states .ctl.modi .ctl.buttons -fill x -side top -anchor w
    }
    set ctldata(ok) 0
    Dialog_Wait .ctl ctldata(ok) .ctl.formula
    Dialog_Dismiss .ctl
    if $ctldata(ok) {
        if {[string trim $ctldata(formula)] != ""} {
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
	  } elseif [file isdirectory "$pathres/Contents/Resources/bbin"] {
	      # on Mac for 4.7.1: go inside this
	      set apathres "$pathres/Contents/Resources/"
	  } else {
	       set apathres [file dirname $pathres]
	  puts "set3 $apathres"
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
            set fid [prob_open "$extdir/$etoolfile"]
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
   # set text [.main.frmSource.text get 1.0 "end - 1 chars"]
   set shortened_details [string range $details 0 5000]
   set DESC "Bug report for ProB $version-$plat.\\nUser=($normal_user/$expert_user).Mode=$curMode.\\n\\n$shortened_details\\n\\n--PASTE YOUR SPECIFICATION HERE OR ATTACH BELOW--"
   # proc_open_url "http://prob:prob@cobra.cs.uni-duesseldorf.de/trac/login/"
   # set shorturl "/prob.hhu.de/w/index.php/Bugs"
   set shorturl "probjira.atlassian.net/secure/Dashboard.jspa"
   if {[prolog tools:host_platform(_windows)]} {
      set bug_report_url "https://$shorturl"
      # Windows seems to have a problem with long urls
       tkMessageBox "I will try and open $shorturl.\nPlease enter Bug details. Then press 'Submit'.\n\nThanks for providing feedback!"
   } else {
      set bug_report_url "https://$shorturl?version=$TKVERS&component=prolog_core&description=$DESC&summary=$SUMM&keywords=$KEYW"
       tkMessageBox "I will try and open $shorturl.\nPlease complete Bug details. Then press 'Submit'.\n\nThanks for providing feedback!"
   }
   proc_open_url $bug_report_url
}

proc procCheckForUpdates {} {
    global version
    set prob_update_url "https://stups.hhu-hosting.de/downloads/prob/tcltk/releases/"
    package require http
    package require tls
    prolog "tools:print_message('Opening https connection')"
    http::register https 443 tls::socket
    set token [::http::geturl "$prob_update_url/current_version.txt"]
    set data [::http::data $token]
    # puts "data $data"
    ::http::cleanup $token
    http::unregister https
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
            -height 18 -width 80
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
		$f.frmSource.text insert 0.0 $prolog_variables(Lic)
		global revision lastchangeddate prologversion tcl_version tcl_patchlevel tcl_table_package_missing_info
    if [prolog "parsercall:get_java_fullversion(JVCA),atom_codes(JV,JVCA)"] {
       set JavaVersion $prolog_variables(JV)
    } else {
       set JavaVersion "Java not properly installed
    }
    prolog compile_time_flags(Flags)
    $f.frmSource.text insert 0.0 "ProB $version\n Revision: $revision\n $lastchangeddate\n Compile time flags: $prolog_variables(Flags)\nTcl/Tk $tcl_patchlevel$tcl_table_package_missing_info\n$prologversion\n $JavaVersion\n"

		set pattern {ProB|\((C|c)\)|Michael Leuschel|-+}
		procMarkRegExpression $f.frmSource.text $pattern about_highlight_tag
		$f.frmSource.text tag configure about_highlight_tag -foreground darkslateblue
		set pattern {https?://[a-zA-Z0-9/~_\-\.]*}
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
		global revision lastchangeddate prologversion tcl_version tcl_patchlevel tcl_table_package_missing_info
		if {$tcl_patchlevel == "8.5.9" && [prolog tools:host_platform(darwin)]} {
		  # probably Apple's buggy Tcl/Tk version installed
	    set warnmsg "\n\n!!!!!! WARNING !!!!!!!\n\n Install a newer version of Tcl/Tk 8.5 or 8.6\n using Homebrew, MacPorts or from http://www.activestate.com/activetcl/downloads/\n\n!!!!!!!!!!!!!!!!!!!!!!\n"
    } elseif {$tcl_patchlevel == "8.6.9" && [prolog tools:host_platform(darwin)]} {
		  # probably Active's buggy Tcl/Tk version installed
	    set warnmsg "\nWarning: Double-Clicking does not work with Active Tcl/Tk 8.6.9!\nTktable can crash on macOS Monterey and is thus disabled!"
    } else {set warnmsg ""}
    if {$tcl_table_package_missing_info != ""} {
       set warnmsg2 "\n\nInstall the Tcl/Tk Tktable extension for better table support!"
    } else {set warnmsg2 ""}

		procShowSourceCodeFromString "ProB $version\n Revision: $revision\n $lastchangeddate\nTcl/Tk $tcl_patchlevel$tcl_table_package_missing_info\n$prologversion$warnmsg$warnmsg2\n$prolog_variables(Lic)"

        global tcl_dir
	    if [file exists "$tcl_dir/icons/prob_logo.gif"] {
			image create photo prob_img -format gif -fil "$tcl_dir/icons/prob_logo.gif"
			#image create photo prob_img -format gif -fil "$tcl_dir/icons/ProBNewLogo.gif"
			.main.frmSource.text configure -state normal
			.main.frmSource.text insert 1.0 "\n    \n\n"
			.main.frmSource.text image create 2.3 -image prob_img
			.main.frmSource.text configure -state disable
		}

		set pattern {ProB|\((C|c)\)|Michael Leuschel|-+}
		procMarkRegExpression .main.frmSource.text $pattern about_highlight_tag
		.main.frmSource.text tag configure about_highlight_tag -foreground darkslateblue
		set pattern {https?://[a-zA-Z0-9/~_\-\.]*}
		procMarkRegExpression .main.frmSource.text $pattern about_highlight_tag2
		.main.frmSource.text tag configure about_highlight_tag2 -foreground tomato
		set pattern {WARNING}
		procMarkRegExpression .main.frmSource.text $pattern about_highlight_tag3
		.main.frmSource.text tag configure about_highlight_tag3 -foreground red
		procResetCodeModified false false
}

proc procSyntaxInfo {name title msg highlightProc state prolog_command} {
     destroy $name
     CreateTextDialog $name $title $msg 0 0

     set txt $name.frmSource.text
     $txt configure -state normal
     $txt delete 1.0 end

     eval [list prologmnf $prolog_command]
     $txt insert 1.0 $prolog_variables(Summary)

     eval [list $highlightProc $txt]
     $txt configure -state $state
}

proc procLaunchSyntaxInfo {syntax} {
     if {$syntax == "B"} {
         procSyntaxInfo .bsinfo "B Syntax" "Summary of supported B notation" \
                                     "procDoSyntaxColouring" "normal" "prob_summary(Summary)"
     } elseif {$syntax == "CSP"} {
         procSyntaxInfo .cspinfo "CSP Syntax" "Summary of supported CSP notation" \
                                     "procDoCSPSyntaxColouring" "disabled" "procsp_summary(Summary)"
     } elseif {$syntax == "Z"} {
         procSyntaxInfo .zinfo "ProZ" "Summary of supported Z notation" \
                                     "procDoZedSyntaxColouring" "disabled" "proz_summary(Summary)"
     } elseif {$syntax == "TLA"} {
         procSyntaxInfo .zinfo "TLA+" "Summary of supported TLA+ notation" \
                                     "procDoTLASyntaxColouring" "normal" "protla_summary(Summary)"
     } elseif {$syntax == "Alloy"} {
         procSyntaxInfo .zinfo "Alloy" "Summary of supported Alloy notation" \
                                     "procDoZedSyntaxColouring" "normal" "alloy2b_summary(Summary)"
     } elseif {$syntax == "LTL"} {
         procSyntaxInfo .zinfo "LTL" "Summary of supported LTL notation" \
                                     "procDoSyntaxColouring" "normal" "ltl_summary(Summary)"
     } elseif {$syntax == "CTL"} {
         procSyntaxInfo .zinfo "LTL" "Summary of supported CTL notation" \
                                     "procDoSyntaxColouring" "normal" "ctl_summary(Summary)"
	 }
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

proc procSymmInfo {} {
		destroy .syminfo
		set f .syminfo
		CreateTextDialog $f "ProB Symmetry" "Summary of Symmetry Reduction Techniques" 0 0
		$f.frmSource.text configure -state normal
		$f.frmSource.text delete 0.0 end
		prologmnf symmetry_summary(Summary)
		$f.frmSource.text insert 0.0 $prolog_variables(Summary)
		$f.frmSource.text configure -state disabled
		set ok 0
}
# ------------------------------------------------------------


proc procOpenFileInEditor {} {
    global strFilename
    if {$strFilename != ""} {
		if [file exists $strFilename] {
           procExecutePathPrefOnFile path_to_text_editor "" {External Text Editor} Advanced $strFilename
        } else {
           tkErrorBox "Source file was moved or no longer exists: $strFilename"
        }
    } else {
       tkErrorBox "No File open. Cannot Open External Editor."
    }
}


proc procSaveState {} {
    global strFilename curFileTypeOpened
    if [procSpecFileSuccessfullyLoaded] {
		set rootName [file rootname $strFilename]
		set saveName {}
		append saveName $rootName ".saved.P"
		# use P extension so that we can use XTL

		prolog "tcltk_save_state('$saveName')"
    } else {
       tkErrorBox "No specification file loaded. Cannot Save State."
    }
}
proc procLoadState {} {
    global strFilename curFileTypeOpened
    if [procSpecFileSuccessfullyLoaded] {
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
       tkErrorBox "No specification file loaded. Cannot Load State."
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
		   tkMessageBox "Warning: not all transitions have been computed.\nLater refinement check may miss counterexamples.\nIncrease MAX_INITIALISATIONS and MAX_OPERATIONS in Animation Preferences and reopen machine."
    }
}



proc procSetDefaultRefSpecFile {} {
    global currentRefSpecFile strFilename
    if {$currentRefSpecFile == "" && [prolog bmachine:b_get_refined_machine_name(Machine)]} {
        global strFilename
		set dirName [file dirname $strFilename]
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
proc profRefinementShowNoCounterExample {FailureCheckingMode} {
  global currentRefSpecFile
  set curT [file tail $currentRefSpecFile]
  if [prolog "tcltk_exists_an_open_node"] {
   tkMessageBox "No counter example found.\nAll traces explored so far are matched by $curT.\nNote: Explore more nodes using model check to check all traces."
  } else {
      if [prolog "tcltk_find_max_reached_node"] {
       tkMessageBox "No counter example found.\nAll traces explored so far are matched by $curT.\nNote: not all transitions have been computed.
    Increase MAX_INITIALISATIONS and MAX_OPERATIONS in Animation Preferences and reopen machine."
      } elseif {$FailureCheckingMode != "trace"} {
       tkMessageBox "No counter example exists.\nAll traces (and $FailureCheckingMode) of the implementation are matched by $curT.\nNote: ProB does not check the particular gluing invariant you may have provided."
      } else {
       tkMessageBox "No counter example exists.\nAll traces of the implementation are matched by $curT.\nNote: ProB does not check the particular gluing invariant you may have provided."
      }
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

global mcFailureCheckingMode
set mcFailureCheckingMode "trace"

proc procDFSGoalCycle {} {
   if [prolog tcltk_nested_dfs_goal_cycle_check(MSG)] {
	    #procShowList2 $prolog_variables(MSG) "Checking GF {GOAL}" "Result of liveness check:" 1 0 "GFGOAL.txt" none
			procInsertHistoryOptionsState
			tkMessageBox $prolog_variables(MSG)
	 } else {
       tkErrorBox "tcltk_nested_dfs_goal_cycle_check failed!"
	 }
}

proc procRefinementCheck {} {
  global strFilename
  if ![procSpecFileSuccessfullyLoaded] {
       tkErrorBox "No specification file loaded."
  } else {
	global mc_prompt Depth count done
	destroy .refcheck
	set f .refcheck
  global currentRefSpecFile
	procSetDefaultRefSpecFile

  global mcFailureCheckingMode
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
	    if [prolog b_get_refined_ancestors_names(AncList)] {
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

		checkbutton $op.failures -text "Check Failures" -variable mcFailureCheckingMode \
		    -offvalue "trace" -onvalue "failures"
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
			  refinement_search $currentRefSpecFile $Depth $mcFailureCheckingMode
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

proc refinement_search {currentRefSpecFile Depth mcFailureCheckingMode} {
	global mc_prompt
	global  done
    global batch_mode

	prolog tcltk_load_refine_spec_file('$currentRefSpecFile')
    if [prolog tcltk_refinement_search(ErrRes,$mcFailureCheckingMode,$Depth)] {
		set Result $prolog_variables(ErrRes)
		set done 1
		if {$Result == "no_counter_example"} {
	        if {!$batch_mode} {
			   .refcheck.msg2 configure -text "No counter example found so far"
			}
			if [prolog "tcltk_exists_an_open_node"] { set done 2 } else { profRefinementShowNoCounterExample $mcFailureCheckingMode }
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
proc procSaveBTraceFile {} {
    global strFilename
    if [procSpecFileSuccessfullyLoaded] {
      set types {
        {"Text Files"	{.txt}	}
        {"All files"		*}
      }
      set Name [file rootname [file tail $strFilename]]
      set TraceFile [proc_getSaveFilename $types "${Name}_history" "txt"]
      if {$TraceFile != ""} {
            prolog "b_show_history:write_full_history_to_file('$TraceFile')"
      }
    } else {
       tkErrorBox "No specification file loaded. Cannot save current history to B file."
    }
}
proc procSaveHTMLTraceFile {level} {
    global strFilename
    if [procSpecFileSuccessfullyLoaded] {
      set types {
        {"Text Files"	{.html}	}
        {"All files"		*}
      }
      set Name [file rootname [file tail $strFilename]]
      set TraceFile [proc_getSaveFilename $types "${Name}_history" "html"]
      if {$TraceFile != ""} {
            prolog "graphical_state_viewer_images:html_print_history('$TraceFile',$level)"
      }
    } else {
       tkErrorBox "No specification file loaded. Cannot save current history to HTML Graphical Visualisation file."
    }
}
proc procSaveTraceFile {ExportStyle saveDialog} {
    global strFilename curFileTypeOpened
    if [procSpecFileSuccessfullyLoaded] {
      if {$saveDialog==1} {
        set types {
          {"Text Files"	{.txt}	}
          {"All files"		*}
        }
          set Name [file rootname [file tail $strFilename]]
          if {$ExportStyle == "json"} {
             set extension ".prob2trace"
          } else {
             set extension ".trace"
          }
          set TraceFile [proc_getSaveFilename $types "${Name}_history" "$extension"]
          if {$TraceFile != ""} {
            prolog "tcltk_save_history_as_trace_file($ExportStyle,'${TraceFile}')"
        }
      } elseif {$ExportStyle == "json"} {
          set rootName [file rootname $strFilename]
          prolog "tcltk_save_history_as_trace_file($ExportStyle,'${rootName}.prob2trace')"
      } else {
          set rootName [file rootname $strFilename]
          prolog "tcltk_save_history_as_trace_file($ExportStyle,'${rootName}.trace')"
      }
    } else {
       tkErrorBox "No specification file loaded. Cannot save current history to trace file."
    }
}
proc procCheckTraceFile {} {
   procCheckTraceFile2 1 0
}
proc procCheckTraceFileNoSuccessMsg {} {
   procCheckTraceFile2 0 0
}
proc procCheckTraceFile2 {display_success_message silent} {
    global strFilename curFileTypeOpened
    if {$strFilename != ""} {
        set rootName [file rootname $strFilename]
        set saveName {}
        append saveName $rootName ".prob2trace"
        if [file exists $saveName] {
                # we have a new style JSON ProB2-UI trace file
                procCheckJSONTraceFile $saveName $display_success_message
        } else {
            set saveName {}
            append saveName $rootName ".trace"
            if [file exists $saveName] {
                   # we have an old style Prolog trace file
                    procCheckTraceFileInternal $saveName "prolog" "default_trace_replay" $silent $display_success_message
            } else {
                   tkErrorBox "No trace has been saved for this machine.\nUnable to open $saveName."
            }
        }
    } else {
        tkErrorBox "No file currently open. Cannot check trace."
    }
}

proc procSelectTraceFile {fileStyle} {
    global strTraceFilename
    set strTraceFilename ""
    if {$fileStyle == "json"} {
      set types {
        {"JSON Trace Files"		{.json .trace .prob2trace}	}
        {"Prolog Trace Files"		{.trace}	}
      }
    } else {
      set types {
        {"Trace Files"		{.trace}	}
        {"Prolog Files"		{.pl .pro}	}
        {"All files"		*}
      }
    }
    # show the dialog box
    set machinesPath [getMachinesPath]
    if {$machinesPath != "" && [file isdirectory $machinesPath]} {
         set strTraceFilename [tk_getOpenFile -filetypes $types -initialdir $machinesPath -parent . ]
    } else {
         set strTraceFilename [tk_getOpenFile -filetypes $types -parent . ]
    }
}

proc procCheckOtherTraceFile {fileStyle deterministic} {
    global strTraceFilename
    procSelectTraceFile $fileStyle
    if {$strTraceFilename != ""} {
        procCheckTraceFileInternal $strTraceFilename $fileStyle $deterministic 0 1
    }
}

# Check ProB2-UI JSON Trace file
proc procSelectAndCheckJSONTraceFile {} {
    global strTraceFilename
    procSelectTraceFile "json"
    procCheckJSONTraceFile $strTraceFilename 1
}

proc procCheckJSONTraceFile {strTraceFilename showTable} {
    if {$strTraceFilename != ""} {
         if [prolog "b_intelligent_trace_replay:tcltk_replay_json_trace_file('$strTraceFilename',ReplayStatus,Table)"] {set success 1} else {set success 0}
         set warns ""
         if [prolog real_error_occurred] {
             procShowErrors2 "$strTraceFilename" none "Trace Replay Error" "" "errors (while replaying JSON trace file $strTraceFilename)" ErrorIcon
             set showTable 1
         } elseif {$success && [prolog tcltk_get_all_errors(200,Warnings,NrErrsShown,NrErrs)]} {
             set warns [join $prolog_variables(Warnings) ","]
             set warns "\n$warns"
             # extract warnings manually and show as part of table
             # typically Imperfect replay, steps replay
             set showTable 1
             prolog reset_errors
         }
         procInsertHistoryOptionsState
		 if {$success && $showTable} {
             set res $prolog_variables(Table)
             set fileTail [file tail $strTraceFilename]
			 procShowTable $res "JSON Trace Replay ($fileTail)" "Trace Replay Status: $prolog_variables(ReplayStatus) $warns" "TraceReplayResult" "" ""
		} else {
		     procShowErrors2 "$strTraceFilename" none "Trace Replay Warnings" "" "warnings (while replaying JSON trace file $strTraceFilename)" WarningIcon
		}
    }
}

proc procCheckTraceFileInternal {tracefile fileStyle deterministic silent display_success_message} {
    # puts "procCheckTraceFileInternal $tracefile"
    if {$fileStyle == "state_sequence"} {
        # this is a file of B predicates describing states; not of events to be executed
        prolog "tcltk_check_state_sequence_from_file('$tracefile')"
        set TLC "TLC"
    } else {
        prolog "tcltk_check_sequence_from_file('$fileStyle','$tracefile',$deterministic)"
        set TLC ""
    }
    # puts "finished replay"
    if [prolog test_error_occurred(_)] {
        # errors will be displayed by procInsertHistoryOptionsState
        # tkMessageBox "Trace checking failed."

         # procShowText6 $prolog_result(Summary) "Errors occurred while replaying TLC trace file $tracefile:" none none "TLC4B.trace" "trace"
         procShowErrors2 "$tracefile" none "Trace Replay Error" "" "errors (while replaying $TLC trace file $tracefile)" ErrorIcon
         procFastForward
    } elseif {$display_success_message} {
        # tkMessageBox "Trace checking successful."
        # "We could insert a status message before Ln in .main.frmSource.statusframe.linecolumns"
    }
    if {$silent==0} { procInsertHistoryOptionsState }
}

# ------------------------------------------------------------
# Visualization Section
# ------------------------------------------------------------

proc procViewModuleHierarchy {} {
   procViewModuleHierarchyType "machines"
}
proc procViewModuleHierarchyType {type} {
    global curFileTypeOpened strFilename
    puts "FileType: $curFileTypeOpened"
    if {$strFilename=="" || ($curFileTypeOpened != "B" && $curFileTypeOpened != "EVENTB")} {
       tkErrorBox "Please open a B or Event-B Machine first."
    } else {
       set rootName [file rootname $strFilename]
       set dotName {}
       set psName {}
       append dotName $rootName ".dot"
       append psName $rootName ".pdf"
       if {$type == "machines"} {
            #prolog "b_machine_hierarchy:write_dot_hierarchy_to_file('$dotName')"
            procCallDotCommand "machine_hierarchy" $dotName $psName
       } elseif {$type == "definitions"} {
            procCallDotCommand "definitions" $dotName $psName
       } elseif {$type == "operations"} {
            procCallDotCommand "operations" $dotName $psName
        } else {
            procCallDotCommand "event_hierarchy" $dotName $psName
        }
    }
}

proc procSetPreferencesForMetaCmd {Cmd OKButton Msg} {
    global batch_mode
    if [prolog "meta_interface:command_preferences_category($Cmd,Category)"] {
       if {!$batch_mode && ![procSetPreferences3 "$prolog_variables(Category)" $OKButton $Msg]} {
         return
       }
    }
}

proc procCallDotCommand {Cmd dotName psName} {
    prologmnf "command_description($Cmd,Name,Desc)"
    procSetPreferencesForMetaCmd $Cmd "View $prolog_variables(Name)" " for viewing $prolog_variables(Name)"
	  if [prolog "meta_interface:call_dot_command($Cmd,'$dotName')"] {
		   procShowErrors1 "$prolog_variables(Name)" none
		   procVisualiseDotFile $dotName $psName
    } else {
		   procShowErrors1 "$prolog_variables(Name)" none
    }
}

proc procCallDotExprCommand {Cmd Expr dotName psName} {
    global batch_mode
    prologmnf "command_description($Cmd,Name,Desc)"
    procSetPreferencesForMetaCmd $Cmd "View $prolog_variables(Name)" " for viewing $prolog_variables(Name)"
    set EObsVar [escapeChars $Expr]
	  if [prolog "meta_interface:call_dot_command_for_expr($Cmd,\'$EObsVar\','$dotName')"] {
		  procShowErrors1 "$prolog_variables(Name)" none
		  procVisualiseDotFile $dotName $psName
    } else {
		  procShowErrors1 "$prolog_variables(Name)" none
    }
}
proc procCallDotExprCommandWoPrefsDialog {Cmd Expr dotName psName} {
    global batch_mode
    prologmnf "command_description($Cmd,Name,Desc)"
    set EObsVar [escapeChars $Expr]
	  if [prolog "meta_interface:call_dot_command_for_expr($Cmd,\'$EObsVar\','$dotName')"] {
		  procShowErrors1 "$prolog_variables(Name)" none
		  procVisualiseDotFile $dotName $psName
    } else {
		  procShowErrors1 "$prolog_variables(Name)" none
    }
}



proc procDisplayVisitedStates {} {
  procDisplayVisitedStates_sfdp "" "normal" "no_colour_for_transitions"
}

proc procDisplayVisitedStates_sfdp {overlap fast colourtrans} {
    global strFilename curFileTypeOpened
    if [procSpecFileSuccessfullyLoaded] {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append pdfName $rootName ".pdf"
        prolog get_state_space_stats(S,T,PrNodes)
        if {$overlap != ""} {
                if {$fast == "fast"} {
                   prolog "tcltk_print_states_for_dot_sfdp('$dotName',$colourtrans)"
                } else {
		           prolog "tcltk_print_states_for_dot('$dotName',$colourtrans)"
		        }
		   		procVisualiseLargeDotFile_with_sfdp $dotName $pdfName $overlap
        } elseif {$prolog_variables(S) < 250} {
		   procCallDotCommand "state_space" $dotName $pdfName
		} else {
		   set ans [tk_messageBox -default yes -message "The state space is large\
		    ($prolog_variables(S) states and $prolog_variables(T) transitions)!\n\n\
		    Do you want to use the fast display algorithm?" -title "Warning" -type yesnocancel -icon warning -parent .]
	        if {$ans == "no"} {
		        prolog "tcltk_print_states_for_dot('$dotName',$colourtrans)"
		   		procVisualiseDotFile $dotName $pdfName
		   		# we could use procVisualiseLargeDotFile
		    } elseif {$ans == "yes"} {
		        prolog "tcltk_print_states_for_dot_sfdp('$dotName',$colourtrans)"
		   		procVisualiseLargeDotFile_with_sfdp $dotName $pdfName scale
		    }
		}
    } else {
       tkErrorBox "No specification file loaded. Cannot display statespace."
    }
}
proc procDisplayVisitedStatesUpTo {dist} {
    global strFilename curFileTypeOpened
    if [procSpecFileSuccessfullyLoaded] {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".pdf"
    prolog "tcltk_print_neighbourhood_states_for_dot($dist,'$dotName')"
		procVisualiseDotFile $dotName $psName
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
   set Goal [Dialog_Promptww "Visualize Predicate" "Enter Predicate to Visualize (use B syntax):" 90 "" ""]
   procVisualiseCustomPredicate_direct $Goal
}
proc procVisualiseCustomPredicate_direct { Predicate } {
   if {$Predicate != ""} {
       set success [procParseGoal $Predicate]
       if {$success} {
            procVisualiseInvariantOrOpPRE "_goal_"
       }
   }
}
proc procAnalyseCustomPredicate {} {
   global Goal
   set Goal [Dialog_Promptww "Analyse Predicate" "Enter Predicate to Analyse (use B syntax):" 90 "" ""]
   if {$Goal != ""} {
       set success [procParseGoal $Goal]
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
proc procVisualiseSpanPredicate {} {
	procVisualiseInvariantOrOpPRE "_span_predicate_"
}
proc procVisualiseInvariantOrOpPRE {opName} {
    global strFilename curFileTypeOpened
    if [procSpecFileSuccessfullyLoaded] {
        if { $opName == "@PROPERTIES" || $opName == "_assertions_" || $opName == "_goal_" || $opName == "@INITIALISATION" || $opName == "_span_predicate_" || [prolog current_state_corresponds_to_initialised_b_machine] } {
			set rootName [file rootname $strFilename]
			set dotName {}
			set psName {}
			append dotName $rootName ".dot"
			append psName $rootName ".pdf"
			if {$opName == "@PROPERTIES"} {
			   procCallDotCommand "properties" $dotName $psName
			} elseif {$opName == "_assertions_"} {
			   procCallDotCommand "assertions" $dotName $psName
			} elseif {$opName == "_goal_"} {
			   procCallDotCommand "goal" $dotName $psName
			} elseif {$opName == "_deadlockpo_"} {
			   procCallDotCommand "deadlock" $dotName $psName
			} elseif {$opName == "_span_predicate_"} {
			   procCallDotCommand "last_error" $dotName $psName
			} elseif {$opName != ""} {
			    if [prolog generate_dot_from_operation('$opName','$dotName')] {
				     procShowErrors
				     procVisualiseDotFile $dotName $psName
				  } else {
		              tkErrorBox "Generating visualisation of precondition/guard for $opName failed."
				  }
			} else {
			   procCallDotCommand "invariant" $dotName $psName
			}
		 } else {
		   tkErrorBox "Please intialise Machine first."
		 }
    } else {
       tkErrorBox "No specification file loaded. Cannot visualise predicate."
    }
}


proc procVisualiseLargeDotFile_with_sfdp {dotName pdfName overlap} {
  # overlap can be scale, prism
  if [file exists $dotName] {
        prolog preferences:get_preference_with_warnings(path_to_dot,PDotCmd)
        # page="8.5, 11",ratio=fill,size="7.5,10"
        puts "$prolog_variables(PDotCmd) -Ksfdp -x -Goverlap=$overlap -Tpdf $dotName -o $pdfName"
	    if {[catch {exec $prolog_variables(PDotCmd) -Ksfdp -x -Goverlap=$overlap -Tpdf $dotName -o $pdfName} errid]} {
           tkErrorBox "Could not execute sfdp dot conversion.\n Be sure that sfdp is installed (e.g., from http://www.graphviz.org) and that path '$prolog_variables(PDotCmd)' in the Graphical Viewer Preferences is correct.\nError: $errid"
         } else {
            procOpenPSFile $pdfName
        }
   } else {
       tkErrorBox "Dot file does not exists: '$dotName'."
   }

}
proc procVisualiseDotFile {dotName pdfName} {
  if [file exists $dotName] {
    if [prolog preferences:get_preference(dot_use_ps_viewer,true)] {
      if [prolog preferences:get_preference_with_warnings(path_to_dot,PDotCmd)] {
	        set DotCmd $prolog_variables(PDotCmd)
	    } else {set DotCmd dot}
	    # puts "exec $DotCmd -Tpdf $dotName -o $pdfName"
	    # use eval in case DotCmd contains arguments
	    if {[catch {exec $DotCmd -Tpdf $dotName -o $pdfName} errid]} {
           tkErrorBox "Could not execute dot.\nBe sure that dot is installed (e.g., from http://www.graphviz.org) and that path '$DotCmd' in the Graphical Viewer Preferences is correct.\nError: $errid"
      } else {
            procOpenPSFile $pdfName
      }
    } else {
        procExecutePathPrefOnFileForApp path_to_dotty dotty dotty Viewer $dotName "GraphViz (http://www.graphviz.org)"
    }
   } else {
       tkErrorBox "Dot file does not exists: '$dotName'."
   }
}

# -------------------------

global VisBJSONFile VisBLastKind VisBLastModelFile
set VisBJSONFile ""
set VisBLastKind ""
set VisBmanuallySelectedFile 0
set VisBLastModelFile ""
proc procVisualiseVisB {Kind} {
    global VisBJSONFile
    set types {
      {"VisB JSON Files"		{.json}	}
      {"All files"		*}
    }
    set machinesPath [getMachinesPath]
    set VisBJSONFile [tk_getOpenFile -filetypes $types -initialdir $machinesPath -parent . ]
    if {$VisBJSONFile != ""} {
        procVisualiseVisBFile $VisBJSONFile $Kind
        set VisBmanuallySelectedFile 1
    }
}
proc procReVisualiseLastVisB {Kind} {
    global VisBJSONFile VisBLastModelFile VisBmanuallySelectedFile strFilename
    if {$VisBJSONFile != "" && ($VisBLastModelFile == $strFilename) && $VisBmanuallySelectedFile} {
        procVisualiseVisBFile $VisBJSONFile $Kind
    } elseif {[prolog get_default_visb_file(Res,_)]} {
        if {$prolog_variables(Res) == ""} {
           set VisBJSONFile  ""
        } else {
           set VisBJSONFile [file join [file dirname $strFilename] $prolog_variables(Res)]
        }
        set VisBmanuallySelectedFile 0
        procVisualiseVisBFile $VisBJSONFile $Kind
    } elseif {$VisBJSONFile != "" && ($VisBLastModelFile == $strFilename)} {
        procVisualiseVisBFile $VisBJSONFile $Kind
    } else {
        procVisualiseVisB $Kind
    }
}

proc procShowVisBItems {} {
  if [prolog visb_file_is_loaded(JsonFile)] {
      prolog visb_visualiser:tcltk_get_visb_items(Info)
      set res $prolog_variables(Info)
      procShowErrors
      procShowTable $res "VisB items in current state" "VisB items ($prolog_variables(JsonFile))" "VisB_Items" "" ""
  } else {
      tkErrorBox "No VisB JSON file loaded."
  }
}
proc procShowVisBEvents {} {
  if [prolog visb_file_is_loaded(JsonFile)] {
     prolog visb_visualiser:tcltk_get_visb_events(Info)
     set res $prolog_variables(Info)
     procShowErrors
     procShowTable $res "VisB events in current state" "VisB events ($prolog_variables(JsonFile))" "VisB_Events" "" ""
  } else {
      tkErrorBox "No VisB JSON file loaded."
  }
}


# called by procInsertHistoryOptionsState, i.e., can react to state changes
proc procAutomaticExternalToolUpdates {} {
    global VisBJSONFile VisBLastKind VisBLastModelFile strFilename
    if {($VisBJSONFile != "" || [prolog get_default_visb_file('',_)]) &&
        ($VisBLastKind == "current_state") && ($VisBLastModelFile == $strFilename)} {
        if [prolog visb_current_state_can_be_visualised] {
            procReVisualiseLastVisB "current_state"
        }
    }
}

proc procVisualiseVisBFile {JSONFile Kind} {
    global VisBLastKind VisBLastModelFile strFilename
    set VisBLastKind $Kind
    set VisBLastModelFile $strFilename
       # to do: avoid re-loading VisB file if already loaded for same machine
       if [prolog "load_visb_file_if_necessary(\'$JSONFile\')"] {
          global strFilename
          set rootName [file rootname $strFilename]
          if {$Kind == "history_and_vars"} {
            prolog "generate_visb_html_for_history_with_vars(\'${rootName}.html\')"
          } elseif {$Kind == "history"} {
            prolog "generate_visb_html_for_history(\'${rootName}.html\')"
          } else {
            prolog "generate_visb_html_for_current_state(\'${rootName}.html\')"
          }
          proc_open_url "file://${rootName}.html"
       } else {
          tkErrorBox "Loading VisB file $JSONFile failed."
       }
       procShowErrors
}
# -------------------------

global rawLatexFile
set rawLatexFile ""
proc procVisualiseLatex {} {
    global rawLatexFile
    set types {
      {"Latex Files"		{.tex}	}
      {"All files"		*}
    }
    set machinesPath [getMachinesPath]
    set rawLatexFile [tk_getOpenFile -filetypes $types -initialdir $machinesPath -parent . ]
    if {$rawLatexFile != ""} {
        procVisualiseLatexFile $rawLatexFile
    }
}
proc procReVisualiseLastLatex {} {
    global rawLatexFile
    if {$rawLatexFile != ""} {
        procVisualiseLatexFile $rawLatexFile
    } else {
        procVisualiseLatex
    }
}

proc procVisualiseLatexFile {rawLatexFile} {
    if [procCheckBModeInitialisedOrConstantsSetup "Visualize Current State using Latex Template."] {
		    set rootName [file rootname $rawLatexFile]
		    append convertedLatex $rootName "_conv.tex"
		    append pdfName $rootName "_conv.pdf"
	      set dir [file dirname $rawLatexFile]
		    puts "Processing file $rawLatexFile"
        if [prolog process_latex_file('$rawLatexFile','$convertedLatex')] {
            procShowErrors
            puts "Generated $convertedLatex"
            prolog preferences:get_preference(path_to_latex,LatexCmd)
            puts "exec $prolog_variables(LatexCmd) -output-directory $dir $convertedLatex"
            if {[catch {exec $prolog_variables(LatexCmd) -output-directory $dir $convertedLatex &} errid]} {
               tkErrorBox "Could not execute Latex command '$prolog_variables(LatexCmd)'.\nBe sure Latex is installed and the ProB preference path_to_latex is correctly set.\n$ExtraErrMsg\nError: $errid"
            }
            puts "opening PDF $pdfName"
            procOpenPSFile $pdfName
        } else {
            tkErrorBox "Converting Latex file $rawLatexFile to $convertedLatex failed."
        }
    }
}

proc procExecutePathPrefOnFile {pathpreference DefaultCmd PathName PrefCategory FileName} {
      if [prolog preferences:get_preference_with_warnings($pathpreference,PDottyCmd)] {
	        set Cmd $prolog_variables(PDottyCmd)
	    } else {set Cmd $DefaultCmd}
      procExecuteCommandOnFile $Cmd $FileName \
           "Be sure that the path for $PathName in the $PrefCategory Preferences is correct."
	  procShowErrors
}
proc procExecutePathPrefOnFileForApp {pathpreference DefaultCmd PathName PrefCategory FileName AppName} {
      if [prolog preferences:get_preference_with_warnings($pathpreference,PDottyCmd)] {
	        set Cmd $prolog_variables(PDottyCmd)
	    } else {set Cmd $DefaultCmd}
      procExecuteCommandOnFile $Cmd $FileName \
           "Be sure that $AppName is installed and that the path for $PathName in the $PrefCategory Preferences is correct."
	  procShowErrors
}

proc procExecuteCommandOnFile {Cmd FileName ExtraErrMsg} {
  set dcmdtail [file extension $Cmd]
  set firstcmd [lindex $Cmd 0]
  if {$Cmd == ""} {
         tkErrorBox "No program found for opening '$FileName'.\n$ExtraErrMsg"
  } elseif {[prolog tools:host_platform(darwin)] && ($firstcmd == "open" || $firstcmd == "wish") } {
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
     procExecutePathPrefOnFileForApp path_to_ps_viewer gv {Postscript Viewer} Viewer $psName "a PostScript viewer"
}

proc procDisplayTransitionDiagram {} {
    global strFilename spec_desc
    if [procCheckBMode "Compute Transition Diagram"] {
        prolog get_state_space_stats(DistinctStates,T,Processed)

		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".pdf"

	    global prompt dialog_checkbutton1
	    set f .promptExprTD
        Dialog_Create_Prompt_f $f "Transition Diagram for Expression" "Enter Variable or Expression to observe\n(ProB will show which $spec_desc(operations_lc) can change the value of the expression, examining $prolog_variables(DistinctStates) visited states and $prolog_variables(T) transitions):" 90 "Keep Dialog Open" "" "dot_projection"
        set prompt(ok) 0
	    Dialog_Wait_Prepare $f prompt(ok) $f.entry
        set prompt(ok) 0
	    tkwait variable prompt(ok)

	    while {$prompt(ok)} {
			procCallDotExprCommandWoPrefsDialog "transition_diagram" $prompt(result) $dotName $psName
			set prompt(ok) 0
			if {$dialog_checkbutton1} {
			   focus $f.entry
			   tkwait variable prompt(ok)
			}
		}
		Dialog_Wait_Finish $f
		Dialog_Dismiss $f
    }
}


proc procSpecFileSuccessfullyLoaded {} {
  global strFilename
    if {$strFilename != "" && [prolog spec_file_has_been_successfully_loaded]} {
       return true
    } else {
       return false
    }
}

proc procCheckBMode {command} {
    global strFilename
    if {$strFilename != "" && [prolog b_or_z_mode] && [prolog spec_file_has_been_successfully_loaded]} {
       return 1
       # we allow b and csp_and_b
    } else {
       procBModeErrorBox $command
       return 0
    }
}
proc procBModeErrorBox {command} {
    global strFilename
    if {$strFilename != ""} {
        tkErrorBox "The command '$command' is only available for B, Event-B, Z and TLA specifications."
    } else {
        tkErrorBox "To use the command '$command' you first have to open a  B, Event-B, Z or TLA specification."
    }
}
proc procCheckBModeInitialisedOrConstantsSetup {command} {
    if [procCheckBMode $command] {
       if [prolog current_state_corresponds_to_setup_constants_b_machine] {
         return 1
       } else {
        tkErrorBox "The command '$command' requires the current state to be initialised. Please animate further."
       }
    }
    return 0
}
proc procCheckBModeWithIntialisedStates {command} {
    if [procCheckBMode $command] {
       if [prolog initialised_b_state_exists] {
         return 1
       } else {
        tkErrorBox "The command '$command' requires at least one initialised state. Please animate or model check further."
       }
    }
    return 0
}
proc procEvaluateAnExpressionOverStatespace {} {
    global strFilename prompt
    if [procCheckBModeWithIntialisedStates "Compute Covered Values"] {
		set rootName [file rootname $strFilename]
		set csvName {}
		append csvName $rootName ".csv"
        prolog get_state_space_stats(S,T,PrNodes)
        set LTLMsg "Optional LTL Guard Property (syntax: e(OP), {PRED}, not, &, or, =>)\nOnly states satisying guard will be examined."
        set ObsVar [Dialog_Promptwww "Evaluate Expression over Statespace" "Enter Variable or Expression for which you want to compute the coverage (possible values in all $prolog_variables(S) visited states):" 80 "" "" $LTLMsg]
        if {$ObsVar != ""} {
           procEvaluateExpressionOverStatespace $ObsVar $prompt(result2)
		}
    }
}
proc procEvaluateExpressionOverStatespace {Expr LTL} {
        set E [escapeChars $Expr]
        set LG [escapeChars $LTL]
		if [prolog "compute_covered_values_for_expression(\'$E\',\'$LG\',MaxTypeCard,Total,Res)"] {
			procShowErrors
			if {$LTL == ""} {
			   set ltlmsg ""
			} else {
			   set ltlmsg " satisfying LTL guard $LTL"
		    }
            prolog get_state_space_stats(S,T,PrNodes)
			procShowTable $prolog_variables(Res) "Coverage Table" "$prolog_variables(Total) Covered Values for $Expr (Max.Card=$prolog_variables(MaxTypeCard)) in initialised states (of $prolog_variables(S) total states)$ltlmsg" "CoverageValueTable" "" ""
		} else {
		    procShowErrors
		}
}
proc procNrVariableValuesOverStatespace {} {
		if [prolog "tcltk_compute_nr_covered_values_for_all_variables(Res)"] {
			procShowErrors
			procShowTable $prolog_variables(Res) "Coverage Table" "Number of Covered Values for Variables" "CoverageVariablesTable" "" ""
		} else {
		    procShowErrors
		}
}
proc procNrConstantsValuesOverStatespace {} {
		if [prolog "tcltk_compute_nr_covered_values_for_all_constants(Res)"] {
			procShowErrors
			procShowTable $prolog_variables(Res) "Coverage Table" "Number of Covered Values for Constants" "CoverageConstantsTable" "" ""
		} else {
		    procShowErrors
		}
}
proc procAnalyseConstants {} {
		if [prolog "coverage_statistics:tcltk_analyse_constants(Res)"] {
			procShowErrors
			procShowTable $prolog_variables(Res) "Infos about Constants" "Information about Constants" "ConstantsInfos" "" ""
		} else {
		    procShowErrors
		}
}

proc procMinMaxCoverageTable {} {
		if [prolog "tcltk_compute_min_max(Res)"] {
			procShowErrors
			procShowTable $prolog_variables(Res) "Min/Max Values" "Minimum and Maximum Values of Variables and Constants" "MinMaxCoverage" "" ""
		} else {
		    procShowErrors
		}
}



proc procEvaluateExpressionOverHistory {} {
    global strFilename
    if [procCheckBMode "Evaluate expression over history"] {
        set ObsVar [Dialog_Prompt "Enter Variable or Expression to observe over History:"]
        if {$ObsVar != ""} {
            set EObsVar [escapeChars $ObsVar]
			if [prolog "tcltk_evaluate_expression_over_history(\'$EObsVar\',Res)"] {
			    procShowErrors1 "$ObsVar" none
			    procShowTable $prolog_variables(Res) "History Value Table" "History Values for $ObsVar" "HistoryValueTable" "" ""
			} else {
			    procShowErrors1 "$ObsVar" none
			}
		}
    }
}

proc procShowPNGFile {Path Msg} {
  global tcl_version
  puts "Showing PNG File $Path ($tcl_version)"
  if {$tcl_version<8.6} {
      # try open at least for macOS; png not supported in Tk 8.5
      if {[catch {exec open $Path &} errid]} {
		        tkErrorBox "Could not open '$Path'.\nError: $errid"
	    }
   } else {
      set f .pngviewer
      destroy $f
      if [Dialog_Create $f "UML Sequence Diagram" -borderwidth 10] {
        image create photo png_image -format png -file $Path
        button $f.png -text "" -image png_image -relief flat
        pack $f.png -side top
        label $f.msg -text "$Msg"
        pack $f.msg -side top
	    }
	 }
}

proc procHistoryAsUMLSequenceChart {} {
    global strFilename lib_dir
	  set rootName [file rootname $strFilename]
	  set umlName {}
	  append umlName $rootName ".uml"
	  puts "Generating plantUML file: $umlName"
    if [prolog "write_uml_sequence_chart('$umlName')"] {
        puts "Calling plantuml.jar in ProB's lib folder"
        if {[catch {runJavaJarOnFile $lib_dir/plantuml.jar $umlName} ErrorOrWarning]} {
           puts "Error or Warning: $ErrorOrWarning"
        } else {
           set ErrorOrWarning ""
        }
        # PDF generation requires a custom jar or additional jars: https://plantuml.com/pdf and adding -Tpdf option
	      set pngName {}
	      append pngName $rootName ".png"
        procShowPNGFile $pngName $ErrorOrWarning
    }
}


proc procDisplayCurrentState {asgraph} {
    global strFilename
    if ![procSpecFileSuccessfullyLoaded] {
       tkErrorBox "No specification file loaded. Cannot display current state."
    } elseif {$asgraph!="no" && ![prolog current_state_corresponds_to_setup_constants_b_machine]} {
       tkErrorBox "Please initialise your machine first."
    } else {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".pdf"
		if {$asgraph=="no"} {
		    if [prolog "tcltk_print_current_state_for_dot('$dotName')"] {
				procVisualiseDotFile $dotName $psName
		    }
		} elseif {$asgraph=="custom"} {
		    if [prolog "state_custom_dot_graph:tcltk_generate_state_custom_dot_graph('$dotName')"] {
				procVisualiseDotFile $dotName $psName
		    }
		} else {
		    procCallDotCommand "state_as_graph" $dotName $psName
		}
		procShowErrors
    }
}

proc procDisplayTraceToCurrentStateWithNeighbors {} {
    global strFilename
    if [procSpecFileSuccessfullyLoaded] {
      set rootName [file rootname $strFilename]
      set dotName {}
      set psName {}
      append dotName $rootName ".dot"
      append psName $rootName ".pdf"
      prolog "tcltk_print_history_to_current_state_with_neighbors_for_dot('$dotName')"
      procVisualiseDotFile $dotName $psName
    } else {
       tkErrorBox "No specification file loaded. Cannot display trace to current state."
    }
}

proc procDisplayTraceToCurrentState {shortest} {
    global strFilename
    if [procSpecFileSuccessfullyLoaded] {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".pdf"
		if {$shortest=="yes"} {
		  prolog "tcltk_print_shortest_trace_to_current_state_for_dot('$dotName')"
		} else {
		  prolog "tcltk_print_history_to_current_state_for_dot('$dotName')"
		}
		procVisualiseDotFile $dotName $psName
    } else {
       tkErrorBox "No specification file loaded. Cannot display trace to current state."
    }
}
proc procDisplayReducedSigMergeWithOptions {} {
    global prompt
	set title "Signature-Merge Reduction"
	prologmnf sap:tcl_get_events_preselected(Events,Presel)
    set_prompt_max_depth_default 5 "procDisplayReducedSigMergeWithOptions"
    set prompt(sap_eventsel) {}
	set chosen [SAPDialog $title $prolog_variables(Events) "ignored" 0 0 0 0 1 ""]
	   set clist [join $chosen ","]
	   procDisplayReducedState "no" $clist
}
proc procDisplayReducedSigMerge {} {
  procDisplayReducedState "no" ""
}
proc procDisplayReducedDFA {} {
  procDisplayReducedState "yes" ""
}
proc procDisplayReducedState {dfa IgnoredEvents} {
    global strFilename
    if [procSpecFileSuccessfullyLoaded] {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".pdf"
		if {$dfa=="yes"} {
		  procCallDotCommand "dfa_merge" $dotName $psName
		} else {
		  prolog "state_space_reduction:tcltk_write_signature_merge_to_dotfile(\[$IgnoredEvents\],'$dotName')"
		  procVisualiseDotFile $dotName $psName
		}
    } else {
       tkErrorBox "No specification file loaded. Cannot display reduced state space."
    }
}

proc procSubgraph {type} {
    global strFilename
    if [procSpecFileSuccessfullyLoaded] {
		set rootName [file rootname $strFilename]
		set dotName {}
		set psName {}
		append dotName $rootName ".dot"
		append psName $rootName ".pdf"
		if {$type=="goal"} {
		    prolog "reduce_graph_state_space:print_subgraph_of_goal_nodes('$dotName')"
		} else {
		    prolog "reduce_graph_state_space:print_subgraph_associated_with_invariant_violations('$dotName')"
		}
		procVisualiseDotFile $dotName $psName
    } else {
       tkErrorBox "No specification file loaded. Cannot display subgraph."
    }
}





proc procUpdateMenusFromPrologInfo {} {
  global debugMode typeCheckMode traceUponError
   if {[prolog "debug_mode(on)"]} {set debugMode 1} else {set debugMode 0}
   if {[prolog "run_time_type_check_mode(on)"]} {set typeCheckMode 1} else {set typeCheckMode 0}
   if {[prolog "get_preference(trace_upon_error,true)"]} {set traceUponError 1} else {set traceUponError 0}

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
      prologmnf "set_preference(trace_upon_error,true)"
      prologmnf "debug"
      # will only work when ProB run from source
   } else {
      prologmnf "set_preference(trace_upon_error,false)"
      # prologmnf "nodebug"
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
proc procOpenFDROnSpec {} {
	global strFilename
    if [prolog animation_mode(cspm)] {
    	procExecutePathPrefOnFileForApp path_to_fdr fdr2 {FDR} Advanced $strFilename "FDR (http://www.cs.ox.ac.uk/projects/concurrency-tools/)"
    } else {
    	tkErrorBox "This command can only be used in CSPM mode."
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
        if [string match {*false*} $errfdrout] {
            puts "FDR detected mismatch between FDR and ProB"
            # TO DO : check if we have Checking MAIN [FD= Nroot\nfalse or xfalse or otherway around
            # puts "FDR stderr:"
            # puts "$errfdrout"
            # tkErrorBox "Refinement checks failed!\nLog: $errfdrout"
		    set Result [split $errfdrout \n]
            procErrShowList $Result "FDR - ProB Refinement Check Failed" "FDR - ProB Refinement Check Failed"
            procExecutePathPrefOnFileForApp path_to_fdr fdr2 {FDR} Advanced $cspName "FDR (http://www.cs.ox.ac.uk/projects/concurrency-tools/)"
        } else {
           puts "\nFDR Compliance Check OK: No error occurred.\n"
        }
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
	prologmnf preferences:get_preference_with_warnings(path_to_csp_typechecker,PCmd)
	set Cmd $prolog_variables(PCmd)
	puts "Calling $Cmd $strFilename"

	if {[catch {set tc_res [exec $Cmd "--typecheck" $strFilename]} errid]} {
			 set Result [split $errid \n]
			 # HighlightLineErr $errid " "
             foreach i $Result {
			       HighlightFDR3Err $i
			 }
			 procShowListError $Result "CSP Type Check of $strFilename" "CSP Type Check FAILED !!"
	} else {
		 set Result [split $tc_res  \n]
		 set errs 0
		 foreach i $Result {
			   if [HighlightFDR3Err $i] {
			      incr i
			    }
		 }
		 if {$errs>0} {
			 procShowListError $Result "CSP Type Check of $strFilename" "CSP Type Check found $errs ERRORS"
		 } else {
			 procShowListOK $Result "CSP Type Check of $strFilename" "CSP Type Check SUCCESSFUL"
		 }
	}
}
proc procTypeCheckB {BFilename highlighterr} {
	prologmnf preferences:get_preference_with_warnings(path_to_bcomp,PCmd)
	set Cmd $prolog_variables(PCmd)
	set pwd [pwd]
	set newd [file dirname $BFilename]
	puts "cd $newd"
	cd $newd
	puts "Calling $Cmd -v -a -i $BFilename"

	if {[catch {set tc_res [exec $Cmd -v -a -i $BFilename]} errid]} {
			 set Result [split $errid \n]
			 if {$highlighterr==1} { HighlightBCompErr $errid }
			 append Result "\n{Ensure that the bcomp path advanced preference is correctly set.}"
			 procShowList "$Result" \
			              "B Type Check of $BFilename" "B Type Check FAILED !!"
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

proc procGetTempDir {} {
  package require fileutil
    return [::fileutil::tempdir]
}

proc procTypeCheck_PPF_BFile {} {
    global strFilename
    if [prolog animation_mode(b)] {
            set IRName "ProB_Typed_Internal_Representation"
	set TMP [procGetTempDir]
	    set tmp_PPF_File "$TMP$IRName.mch"
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


proc procStaticWDCheck {} {
		if [prolog "well_def_analyser:analyse_wd_for_machine(DisNr,TotNr,Discharged)"] {
       if [prolog error_or_warning_occured] {
          procShowErrors2 "" .main.frmSource.text "WD Proof Obligations Warnings ($prolog_variables(DisNr)/$prolog_variables(TotNr) discharged)" "" "WD warnings" WarningIcon
       } else {
		     tkMessageBox "WD Proof Obligations Discharged: ($prolog_variables(DisNr)/$prolog_variables(TotNr))"
       }
		}
		procShowErrors
}
proc procExtendedStaticCheck {} {
		if [prolog "bmachine_static_checks:extended_static_check_machine"] {
		# we could also call extended_static_check_default_visb_file
       if [prolog error_or_warning_occured] {
          procShowErrors2 "" .main.frmSource.text "Extended Static Checking Warnings" "" "warnings" WarningIcon
       } else {
		     tkMessageBox "No (additional) issues found by extended static checking."
       }
		}
		procShowErrors
}

proc procShowWDPOs {OnlyGoal} {
		prolog "well_def_analyser:tcltk_get_machine_wd_pos($OnlyGoal,ResList,ResStr,Discharged)"
    procShowTable $prolog_variables(ResList) "WD PO List" "WD Proof Obligations List ($prolog_variables(ResStr))" "WD_PO_Stats" "" ""
		procShowErrors
}


# ------------------------------------------------------------

global stateViewerContentsAvailable stateViewerContentsStateID last_imagelist
set stateViewerContentsAvailable false
set stateViewerContentsStateID -1
set last_imagelist ""

proc procCreateStateViewer {} {
  global startAtColumn laststartAtColumn scaleVar
  global stateViewerContentsAvailable stateViewerContentsStateID last_imagelist
	# destroy .stateviewer
	set f .stateviewer
	# puts "Creating State Viewer"
	if [Dialog_Create $f "Graphical Visualisation of Current State" -borderwidth 10] {
	   set startAtColumn 1
	   set laststartAtColumn 0
	   set scaleVar 0
	   set stateViewerContentsAvailable false
	   set stateViewerContentsStateID -1
	   set last_imagelist ""
	   procCreateStateViewFrame
	}
}
proc procCreateStateViewFrame {} {
	set f .stateviewer
	#message $f.msg -text "Graphical Visualisation" -aspect 1000
	frame $f.sviewFrame -borderwidth .1c -relief groove
	#label $f.sviewFrame.label1 -text empty
	#pack $f.sviewFrame.label1 -side right
	pack $f.sviewFrame -side top
	pack $f.sviewFrame -pady 5 -expand yes -fill both
}

proc disableShrink {f} {
  # puts "disable shrink $f [winfo width $f] [winfo height $f]"
  # avoid flickering of e.g. state viewer
  wm minsize $f [winfo width $f] [winfo height $f]
}
proc enableShrink {f} {
  wm minsize $f 100 10
}

proc ResetStateViewerCache {} {
   global stateViewerContentsStateID last_imagelist
   set stateViewerContentsStateID -1
   set last_imagelist ""
}

proc procGetStateViewerImageListAndContents {} {
   global stateViewerContentsAvailable stateViewerContentsStateID contents_list
   prolog "state_space:current_state_id(CurrentID)"

   if {$stateViewerContentsStateID == $prolog_variables(CurrentID)} {
      # puts "Caching images and contents for state: $stateViewerContentsStateID"
   } else {
       if [prolog tcltk_get_image_list(Images,MinRow,MaxRow,MinCol,MaxCol,Contents)] {
             # puts "MinRow=$prolog_variables(MinRow),MaxRow=$prolog_variables(MaxRow),MinCol=$prolog_variables(MinCol),MaxCol=$prolog_variables(MaxCol)"

            set stateViewerContentsAvailable true
            set stateViewerContentsStateID $prolog_variables(CurrentID)
            global last_imagelist
            set imagelist $prolog_variables(Images)
            if {$last_imagelist == $imagelist} {
              # puts "Caching image list $last_imagelist"
            } else {
              global strFilename tcl_dir
              set path [file dirname $strFilename]
              set notfound ""
              set path2 ""
              #puts "app_dir: '$app_dir'"
              if [prolog get_animation_image_source_file(_,File)] {
                 set path2 [file dirname $prolog_variables(File)]
              }

              foreach {label img} $imagelist {
                if [file exists $path/$img] {
                  procCreate_StateViewer_Image $label $path/$img
                } elseif [file exists $img] {
                  procCreate_StateViewer_Image $label $img
                } elseif [file exists $tcl_dir/$img] {
                  procCreate_StateViewer_Image $label $tcl_dir/$img
                } elseif {$path2 != "" && [file exists $path2/$img]} {
                  procCreate_StateViewer_Image $label $path2/$img
                } else {
                  append notfound " $img"
                }
              }
              if {$notfound != ""} {
                 tkErrorBox "Cannot find gif image(s):$notfound.\nPaths: $path ; $tcl_dir"
                 #set stateViewerContentsAvailable true
              } else {
                 set last_imagelist $imagelist
              }
            }
            set contents_list  $prolog_variables(Contents)
            # puts "contents $contents_list"

       } else {
            set stateViewerContentsAvailable false
              set stateViewerContentsStateID -1
              set last_imagelist ""
             if [winfo exists .stateviewer] { destroy .stateviewer}
       }
   }
}

proc procUpdateStateViewer {} {
   global strFilename tcl_dir
   global lastWToPath lastWToImage
   set lastWToPath ""
   set f .stateviewer
   if [prolog preferences:get_preference(use_tk_custom_state_viewer,true)] {
     global stateViewerContentsAvailable contents_list
	   procGetStateViewerImageListAndContents
	   if {$stateViewerContentsAvailable} {
         prolog get_preference(tk_custom_state_viewer_padding,PADDING)
         set imgpad $prolog_variables(PADDING)
         prolog get_preference(tk_custom_state_viewer_str_padding,STRPADDING)
         set strpad $prolog_variables(STRPADDING)

         if [winfo exists $f] {
             # destroy $f.sviewFrame
           # procCreateStateViewFrame
           disableShrink $f
           foreach i [winfo children $f.sviewFrame] {
              destroy $i
           }
         } else {
           procCreateStateViewer
         }

          set fviz $f.sviewFrame.viz
          frame $fviz -relief groove
          # scrollbar $f.scrollx -command "$fviz xview" -orient h

          # with startAtColumn we can perform horizontal scrolling; the column numbers start at 1 irrespective of what the ANIMATION_FUNCTION does
          set currentColumnInFrame 0
          set maxColumnInFrame 0
          set currentWidth 0
          set maxWidth 0
          set currentFrame "no__FRAME"
          global startAtColumn laststartAtColumn stateViewerFontSize
          set laststartAtColumn $startAtColumn
          set FONT [getCurrentStateViewerFont]
               # TkFixedFont

          foreach {type framename label} $contents_list {
           # puts "Generating content $type $framename $label"
           if {$type=="frame"} {
              # frame $fviz.$framename -borderwidth .1c -relief flat
              frame $fviz.$framename -relief flat
              #pack $fviz.$framename -side $label
              pack $fviz.$framename -side $label
           } else {
                if {$framename == $currentFrame} {
                   incr currentColumnInFrame
                   if {$currentColumnInFrame > $maxColumnInFrame} { set maxColumnInFrame $currentColumnInFrame }
                } else {
                   set currentColumnInFrame 1
                   set currentFrame $framename
                   set currentWidth 0
                }
               if {[string range $type 0 3]=="text"} {
                 set currentWidth [expr $currentWidth+[string length $label]*$stateViewerFontSize]
                 if {$currentWidth > $maxWidth} {set maxWidth $currentWidth}
                 if {$currentColumnInFrame >= $startAtColumn} {
                     # puts "label = '$label'"
                    label $fviz.$framename.$type -text "$label" -font $FONT -relief groove -padx $strpad
                    #pack $fviz.$framename.$type -side left
                    bind $fviz.$framename.$type <<RIGHTCLICK>> {procReactToStateViewerRightClick "%W" "%X" "%Y"}
                    pack $fviz.$framename.$type -side left
                }
             } else {
                 set imgwidth [image width $label]
                 set currentWidth [expr $currentWidth+$imgwidth]
                 if {$currentWidth > $maxWidth} {set maxWidth $currentWidth}
                 # puts "cur $currentWidth, max: $maxWidth at col $currentColumnInFrame"
                 if {$currentColumnInFrame >= $startAtColumn} {
                     # something like item1_1 frame1 IMG0
                    label $fviz.$framename.$type -image $label -padx $imgpad -pady $imgpad -borderwidth 0
                    bind $fviz.$framename.$type <<RIGHTCLICK>> {procReactToStateViewerRightClick "%W" "%X" "%Y"}
                             # TO DO: maybe change image
                    bind $fviz.$framename.$type <ButtonPress-1> {procInitDragImageInfo}
                    bind $fviz.$framename.$type <ButtonRelease-1> {procReactToStateViewerRelease "%W" "%X" "%Y"}
                    bind $fviz.$framename.$type <Motion> {procReactToStateViewerMotion "%W"  "%X" "%Y"}
        #click: .stateviewer.sviewFrame.frame8.item8_8
                   # label $fviz.$framename.$type -image $label -padx 0 -pady 0 -borderwidth 0 -relief flat
                   #pack $fviz.$framename.$type -side left
                    pack $fviz.$framename.$type -side left
                }
             }
           }
          }
          if {$maxWidth > 1280} {
              global scaleVar
              if {![winfo exists $f.scale]} {
                scale $f.scale -from 0 -to [expr ($maxColumnInFrame/5)-4] -length 400 -variable scaleVar\
                    -orient h -command procUpdateStateViewerScale -showvalue false
                pack $f.scale -side bottom -expand yes -fill x
              }
          } elseif {[winfo exists $f.scale]} {
              destroy $f.scale
          }
          pack $fviz
          # pack $f.scrollx -side bottom -fill x

          update idletasks
          enableShrink $f

	   } else { destroy $f }
   } else { destroy $f }
	 procShowErrors
}

proc procUpdateStateViewerScale {args} {
  global startAtColumn laststartAtColumn scaleVar
  set startAtColumn [expr 1+($scaleVar*5)]
  # puts "procUpdateStateViewerScale $args ($startAtColumn, $laststartAtColumn)"
  if {$startAtColumn != $laststartAtColumn} {
      procUpdateStateViewer
  }
}

global scaleStateViewerUp scaleStateViewerDown stateViewerFontSize
set scaleStateViewerUp 1
set scaleStateViewerDown 1
set stateViewerFontSize 12

proc procCreate_StateViewer_Image {Lbl Path} {
	  global tcl_version
	  if {$tcl_version<8.6 && [file extension $Path] != ".gif"} {
       package require Img
    }
    global scaleStateViewerUp scaleStateViewerDown
    if {$scaleStateViewerUp == 1 && $scaleStateViewerDown == 1} {
      image create photo $Lbl -file $Path
    } elseif {$scaleStateViewerUp == 1} {
      image create photo tmp_img -file $Path
      image create photo $Lbl
      $Lbl copy tmp_img -subsample $scaleStateViewerDown
      image delete tmp_img
    } elseif {$scaleStateViewerDown == 1} {
      image create photo tmp_img -file $Path
      image create photo $Lbl
      $Lbl copy tmp_img -zoom [expr $scaleStateViewerUp]
      image delete tmp_img
    } else {
      # does not work yet
      image create photo tmp_img
      image create photo $Lbl -file $Path
      tmp_img copy $Lbl -zoom $scaleStateViewerUp
      $Lbl blank
      $Lbl copy tmp_img -subsample $scaleStateViewerDown
      image delete tmp_img
		}
}

global lastWToPath
set lastWToPath ""
proc procReactToStateViewerMotion {WFromPath X Y} {
    global lastWToPath lastWToImage
    set WToPath [winfo containing $X $Y]
    if {$WToPath != $lastWToPath} {
      # puts "motion: $WFromPath at $X and $Y to $WToPath"
      procResetDragImage
      if [prolog "tcltk_can_react_to_item_drag('$WFromPath','$WToPath',Image)"] {
          if {$prolog_variables(Image) != ""} {
            set lastWToPath $WToPath
            set lastWToImage [$WToPath cget -image]
            $WToPath configure -image "$prolog_variables(Image)"
          }
      }
      procShowErrors
    }
}

proc procInitDragImageInfo {} {
    global lastWToPath lastWToImage
    set lastWToPath ""
}
proc procResetDragImage {} {
    global lastWToPath lastWToImage
    # reset image when mouse leaves a target field
    if {$lastWToPath != ""} {
		$lastWToPath configure -image "$lastWToImage"
        set lastWToImage ""
        set lastWToPath ""
    }
}

proc procReactToStateViewerRightClick {WPath X Y} {
    global svcolPath
    # puts "right click command: $WPath"
    if [prolog "tcltk_can_react_to_item_right_click('$WPath',Reactions)"] {
      destroy .svcolpopup
      menu .svcolpopup -tearoff 0
      set svcolPath $WPath
      if {$prolog_variables(Reactions) == ""} {
          if [prolog real_error_occurred] {
           .svcolpopup add command -label "ERROR COMPUTING REACT TO RIGHT CLICK"
          } else {
           .svcolpopup add command -label "No Operation Available"
          }
      } else {
        foreach {reaction} $prolog_variables(Reactions) {
           set cmd "procReactToStateViewerRightClick_Menu {$reaction}"
           .svcolpopup add command -label "$reaction" -command $cmd
        }
      }
      .svcolpopup add sep
      .svcolpopup add command -label "200 %" -command {set scaleStateViewerUp 2; set scaleStateViewerDown 1; ResetStateViewerCache; procUpdateStateViewer}
      .svcolpopup add command -label "Default" -command {set scaleStateViewerUp 1; set scaleStateViewerDown 1; ResetStateViewerCache; procUpdateStateViewer}
      # .svcolpopup add command -label "75 %" -command {set scaleStateViewerUp 3; set scaleStateViewerDown 4; procUpdateStateViewer}
      .svcolpopup add command -label "50 %" -command {set scaleStateViewerUp 1; set scaleStateViewerDown 2; ResetStateViewerCache; procUpdateStateViewer}
      .svcolpopup add command -label "33 %" -command {set scaleStateViewerUp 1; set scaleStateViewerDown 3; ResetStateViewerCache; procUpdateStateViewer}
      .svcolpopup add sep
      if [prolog state_custom_dot_graph:state_custom_dot_graph_available] {
          .svcolpopup add command -label "Current State as Custom Graph" -command {procLaunchCmd {procDisplayCurrentState "custom"}}
      }
      tk_popup .svcolpopup $X $Y
      procShowErrors
    }
}
proc procReactToStateViewerRightClick_Menu {TransitionName} {
    # puts "Trans: $TransitionName"
    global svcolPath
    if [prolog "tcltk_react_to_item_right_click('$svcolPath','$TransitionName')"] {
		procInsertHistoryOptionsState
		procShowErrors
    }
}

proc procReactToStateViewerRelease {WFromPath X Y} {
    # puts "release: $WFromPath at $X and $Y"
    procResetDragImage
    set WToPath [winfo containing $X $Y]
    # puts "winfo containing: $WToPath"
    if [prolog "tcltk_react_to_item_drag('$WFromPath','$WToPath')"] {
		procInsertHistoryOptionsState
		procShowErrors
    }
}
#=============================

proc procQuadVennDiagram {} {
    global Goal
    if [procCheckBMode "Show Sets as Venn Diagram"] {

		destroy .venn
		set f .venn
	    if [Dialog_Create $f "Venn Diagram for Sets:" -borderwidth 10] {
	        global prompt
			message $f.help -text "Enter 2-4 sets" -aspect 1000
			pack $f.help -side top -fill x
			message $f.msg1 -text "Set 1: " -aspect 1000
			entry $f.entry1 -textvariable prompt(set1)
			pack $f.msg1 $f.entry1 -side top -fill x
			message $f.msg2 -text "Set 2: " -aspect 1000
			entry $f.entry2 -textvariable prompt(set2)
			pack $f.msg2 $f.entry2 -side top -fill x
			message $f.msg3 -text "Set 3: " -aspect 1000
			entry $f.entry3 -textvariable prompt(set3)
			pack $f.msg3 $f.entry3 -side top -fill x
			message $f.msg4 -text "Set 4: " -aspect 1000
			entry $f.entry4 -textvariable prompt(set4)
			pack $f.msg4 $f.entry4 -side top -fill x
			set b [frame $f.buttons]
			button $b.ok -text "Venn Diagram" -command {set prompt(ok) 1}
			button $b.cancel -text Finished -command {set prompt(ok) 0}
			pack $b.ok -side left
			pack $b.cancel -side right
			pack $b -side bottom -fill x

            set prompt(ok) 1
			Dialog_Wait_Prepare $f prompt(ok) $f.entry1
			while {$prompt(ok)!=0} {
				set prompt(ok) 0
				tkwait variable prompt(ok)

				if {$prompt(ok) != 0} {
					puts "ls = $prompt(set1) $prompt(set2) $prompt(set3) $prompt(set4)"

					if {$prompt(set2) == "" && $prompt(set3) == "" && $prompt(set4) == ""} {
					    # try and split set 1 into multiple sets
					    puts "Only one expression provided; trying to split expression using spaces"
				        set ls [split $prompt(set1)]
			            procVennDiagramForExprs [lindex $ls 0] [lindex $ls 1] [lindex $ls 2] [lindex $ls 3]
					} else {
						procVennDiagramForExprs $prompt(set1) $prompt(set2) $prompt(set3) $prompt(set4)
					}
				}
			}
			Dialog_Wait_Finish $f
			Dialog_Dismiss $f
		}
	}
}

proc procVennDiagramForExprs {E1 E2 E3 E4} {
   set a(1) ""; set a(2) ""; set a(3) ""; set a(4) ""
   set i 1
   if {$E1 != ""} {set a($i) $E1; incr i}
   if {$E2 != ""} {set a($i) $E2; incr i}
   if {$E3 != ""} {set a($i) $E3; incr i}
   if {$E4 != ""} {set a($i) $E4; incr i}
   procVennDiagramForExprs_aux $a(1) $a(2) $a(3) $a(4)
}
proc procVennDiagramForExprs_aux {E1 E2 E3 E4} {
   global strFilename
   puts "Quad:\n E1=$E1\n E2=$E2\n E3=$E3\n E4=$E4\n"
   if {$E4 != ""} {
      set venntype "quad"
    } elseif {$E3 != ""} {
      set venntype "triple"
    } else {
      set venntype "pairwise"
    }
   set rootName [file rootname $strFilename]
   set rName {}; set pdfName {}
   append rName $rootName "_venn.R"
   append pdfName $rootName "_venn.pdf"
   set ch [prob_open "$rName" "w+"]
   puts $ch "library(\"VennDiagram\")"
   puts $ch "pdf(\"$pdfName\",width=10)"
   puts $ch "venn.plot <- draw.$venntype.venn("
   prolog "tcltk_eval_string_in_cur_state(\'card($E1)\',R)"
   set a1 $prolog_variables(R)[]
   prolog "tcltk_eval_string_in_cur_state(\'card($E2)\',R)"
   set a2 $prolog_variables(R)
   puts "a1,a2 = $a1,$a2"
   puts $ch "area1 = $a1,"
   puts $ch "area2 = $a2,"
   if {$E3 == ""} {
      # pairwise
	   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E1),($E2)}))\',R)"
	   puts $ch "cross.area = $prolog_variables(R),"
	   puts "cross.area = $prolog_variables(R)"
       puts $ch "euler.d=TRUE,"
       puts $ch "scaled=TRUE,"
	   puts $ch "category = c(\"$E1 ($a1)\", \"$E2 ($a2)\"),"
	   puts $ch "fill = c(\"red\", \"green\"),"
   } else {
	   prolog "tcltk_eval_string_in_cur_state(\'card($E3)\',R)"
	   set a3 $prolog_variables(R)
	   puts $ch "area3 = $a3,"
	   if {$E4 != ""} {
		   prolog "tcltk_eval_string_in_cur_state(\'card($E4)\',R)"
		   set a4 $prolog_variables(R)
		   puts $ch "area4 = $a4,"
		   puts "a1,a2,a3,a4 = $a1,$a2,$a3,$a4"
		}
	   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E1),($E2)}))\',R)"
	   puts $ch "n12 = $prolog_variables(R),"
	   puts "n12 = $prolog_variables(R)"
	   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E1),($E3)}))\',R)"
	   puts $ch "n13 = $prolog_variables(R),"
	   puts "n13 = $prolog_variables(R)"
	   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E2),($E3)}))\',R)"
	   puts $ch "n23 = $prolog_variables(R),"
	   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E1),($E2),($E3)}))\',R)"
	   puts $ch "n123 = $prolog_variables(R),"
	   puts "n123 = $prolog_variables(R)"

	   if {$E4 != ""} {
		   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E1),($E4)}))\',R)"
		   puts $ch "n14 = $prolog_variables(R),"
		   puts "n14 = $prolog_variables(R)"
		   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E2),($E4)}))\',R)"
		   puts $ch "n24 = $prolog_variables(R),"
		   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E3),($E4)}))\',R)"
		   puts $ch "n34 = $prolog_variables(R),"
		   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E1),($E2),($E4)}))\',R)"
		   puts $ch "n124 = $prolog_variables(R),"
		   puts "n124 = $prolog_variables(R)"
		   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E1),($E3),($E4)}))\',R)"
		   puts $ch "n134 = $prolog_variables(R),"
		   puts "n134 = $prolog_variables(R)"
		   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E2),($E3),($E4)}))\',R)"
		   puts $ch "n234 = $prolog_variables(R),"
		   prolog "tcltk_eval_string_in_cur_state(\'card(inter({($E1),($E2),($E3),($E4)}))\',R)"
		   puts $ch "n1234 = $prolog_variables(R),"
		   puts $ch "category = c(\"$E1 ($a1)\", \"$E2 ($a2)\", \"$E3 ($a3)\", \"$E4 ($a4)\"),"
		   puts $ch "fill = c(\"red\", \"green\", \"blue\", \"orange\"),"
	   } else {
	       # triple Venn
           puts $ch "euler.d=TRUE,"
           puts $ch "scaled=TRUE,"
		   puts $ch "category = c(\"$E1 ($a1)\", \"$E2 ($a2)\", \"$E3 ($a3)\"),"
		   puts $ch "fill = c(\"red\", \"green\", \"blue\"),"
	   }
   }
   puts $ch "lty = \"dashed\","
   puts $ch "cat.dist=0.1,"
   puts $ch "cat.cex=2,"
   puts $ch "cex=2,"
   puts $ch "margin=0.1);"
   close $ch
   puts "Running R program: $rName"

	if {[catch {exec R --slave --vanilla < $rName} errid]} {
	   puts "Error occured while executing R.\n Be sure that R with VennDiagram library is installed (using install.packages(\"VennDiagram\") from within R).\nError: $errid"
	   #tkErrorBox "Could not execute R.\n Be sure that R is installed.\nError: $errid"
	}
   puts "Viewing generated PDF: $pdfName"
    procOpenPSFile $pdfName
	procShowErrors
}

proc procSymbolicBMCModelCheck {} {
    if [procCheckBMode "Symbolic Model Checking"] {
	   #prolog "use_module(symbolic_model_checker(bmc))"
	   if [prologmnf "bmc_symbolic_model_check(KIndRes)"] {
		   if {$prolog_variables(KIndRes) == "property_holds"} {
			   tkMessageBoxNoParent "No error state found."
		   }
		   if {$prolog_variables(KIndRes) == "counterexample_found"} {
			   tkErrorBoxNoParent "Error state was found. Trying to reconstruct trace to the erroneous state."
			   procInsertHistoryOptionsState
		   }
		   if {$prolog_variables(KIndRes) == "limit_reached"} {
			   tkMessageBoxNoParent "Iteration limit k=25 reached. k-Induction terminated without result."
		   }
		   if {$prolog_variables(KIndRes) == "solver_and_provers_too_weak"} {
			   tkMessageBoxNoParent "Symbolic Model Checking could not be performed, because the solver and provers are too weak for the occurring predicates."
		   }
       }
	   procShowErrors
   }
}

proc procSymbolicKInductionModelCheck {} {
    if [procCheckBMode "Symbolic Model Checking"] {
	   # prolog "use_module(symbolic_model_checker(kinduction))"
	   if [prologmnf "kinduction_symbolic_model_check(KIndRes)"] {
		   if {$prolog_variables(KIndRes) == "property_holds"} {
			   tkMessageBoxNoParent "No error state found."
		   }
		   if {$prolog_variables(KIndRes) == "counterexample_found"} {
			   tkErrorBoxNoParent "Error state was found. Trying to reconstruct trace to the erroneous state."
			   procInsertHistoryOptionsState
		   }
		   if {$prolog_variables(KIndRes) == "limit_reached"} {
			   tkMessageBoxNoParent "Iteration limit k=25 reached. k-Induction terminated without result."
		   }
		   if {$prolog_variables(KIndRes) == "solver_and_provers_too_weak"} {
			   tkMessageBoxNoParent "Symbolic Model Checking could not be performed, because the solver and provers are too weak for the occurring predicates."
		   }
       }
	   procShowErrors
   }
}

proc procSymbolicIC3ModelCheck {} {
    if [procCheckBMode "Symbolic Model Checking"] {
	   # prolog "use_module(symbolic_model_checker(ic3))"
	   if [prolog "ic3_symbolic_model_check(IC3Res)"] {
       if {$prolog_variables(IC3Res) == "property_holds"} {
           tkMessageBoxNoParent "No error state found."
       }
       if {$prolog_variables(IC3Res) == "counterexample_found"} {
           if [prolog ic3:counter_example_can_be_replayed] {
               tkErrorBoxNoParent "Error state was found. Trying to reconstruct trace to the erroneous state."
               procInsertHistoryOptionsState
           } else {
               tkErrorBoxNoParent "Error state was found. However, the trace can not be replayed due to state space abstraction."
           }
       }
       if {$prolog_variables(IC3Res) == "solver_and_provers_too_weak"} {
           tkMessageBoxNoParent "Symbolic Model Checking could not be performed, because the solver and provers are too weak for the occurring predicates."
       }
	     procShowErrors
	   } else {
        procShowErrors2 "" .main.frmSource.text "Error Message" "IC3 failed unexpectedly\n" "errors" ErrorIcon
	   }
   }
}

proc procSymbolicCTIGARModelCheck {} {
    if [procCheckBMode "Symbolic Model Checking"] {
	   # prolog "use_module(symbolic_model_checker(ctigar))"
	   prologmnf "ctigar_symbolic_model_check(IC3Res)"
       if {$prolog_variables(IC3Res) == "property_holds"} {
           tkMessageBoxNoParent "No error state found."
       }
       if {$prolog_variables(IC3Res) == "counterexample_found"} {
           tkErrorBoxNoParent "Error state was found. Trying to reconstruct trace to the erroneous state."
           procInsertHistoryOptionsState
       }
       if {$prolog_variables(IC3Res) == "solver_and_provers_too_weak"} {
           tkMessageBoxNoParent "Symbolic Model Checking could not be performed, because the solver and provers are too weak for the occurring predicates."
       }
	   procShowErrors
   }
}

proc procUnsatCore {} {
   procUnsatCoreMsg "PROPERTIES"
}
proc procUnsatCoreMsg {Clause} {
    if [procCheckBMode "Unsat Core"] {
	   # prolog "use_module(probsrc(unsat_cores))"
	   if [prolog "tcltk_unsat_core_properties(CoreList,HasLabels,Res)"] {
		     HighlightSyntaxErrors .main.frmSource.text
		     if [prolog real_error_occurred] {
		        procShowErrors
		      } else {
		        prolog reset_errors
		      }
		     # procHighlightErrorSpan ".main.frmSource.text" $prolog_variables(Line) $prolog_variables(Col) $prolog_variables(EL) $prolog_variables(EC)
		     set Legend ""
		     if {$prolog_variables(Res) == "FALSE"} {
		        set Msg "Unsatisfiable Core of $Clause\n (a subset of [llength $prolog_variables(CoreList)] predicate(s) which are still unsatisfiable):"
		        if {$prolog_variables(HasLabels) == "true"} {
                set Legend "\n  @TO means conjunct was kept to prevent a time-out\n  @VTO means conjunct was kept to prevent a virtual time-out\n  @WD means conjunct was kept to prevent a WD error"
		        }
		     } else {
		        set Msg "Result of solving $Clause: $prolog_variables(Res)\nUnsatisfiable/Time-Out Core of $Clause\n (a subset of [llength $prolog_variables(CoreList)] predicate(s) which are unsatisfiable or lead to a time-out):"
		        if {$prolog_variables(HasLabels) == "true"} {
		           set Legend "\n  @TO means conjunct was kept due to a time-out\n  @VTO means conjunct was kept due to a virtual time-out\n  @WD means conjunct was kept to due to a WD error"
		        }
		     }
		     procShowList2 $prolog_variables(CoreList) "UNSAT CORE $Clause ($prolog_variables(Res))" "$Msg$Legend" 1 0 "UNSAT_CORE_$Clause.txt" none
	   } else {
			   procShowErrors
	   }
   }
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
		# prolog reduce_graph_state_space:get_alphabet_signatures(A)
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

    # prolog reduce_graph_state_space:assert_all_transition_arg_dont_cares
    # prolog reduce_graph_state_space:get_transition_arguments_from_source(A,B)
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


    global spec_desc
		destroy .argChooser;
		if [Dialog_Create .argChooser "Operation/Argument Chooser"] {
			wm geometry .argChooser +300+300;
			# top label
			label .argChooser.msg -padx 5 -pady 10 -wraplength 4i -justify left -text "Select which $spec_desc(operations_lc) & arguments to use in Reduced Visted States / Reduced DFA graph. To select, click on an item in the list."
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

proc procEnablingAnalysisShort_POR {} {
    prolog tcltk_enabling_analysis(Result,POR)
    procShowErrors
    set LegendList {" * guaranteed: op2 (column) guaranteed to be executable after op1 (row)" " * impossible: op2 impossible to execute after op1"  " * independent: op1 and op2 syntactically independent; op2 stays enabled or disabled after op1" " * activation_indepdendent: op1 does not influence guard of op2, but (op1;op2) could have a different effect than (op2;op1)" " * keep: op2 always stays enabled resp. disabled after op1" " * possible_disable: op2 possible after op1, but can not be enabled only disabled by op1" " * possible_enable: op2 possible after op1, but op2 can not be disabled only enabled by op1"}
    procShowTable $prolog_variables(Result) "Enabling Analysis" "Enabling Information after Operation" "EnablingInfo" $LegendList "";#$prolog_variables(POR)
}

proc procEnablingAnalysisShort {} {
    procEnablingAnalysis 0
}
proc procEnablingAnalysisLong {} {
    procEnablingAnalysis 2500
}
proc procEnablingAnalysis {ExtraTimeout} {
    global spec_desc
    if [procRunPrologTask enabling($ExtraTimeout)] {
		prolog tcltk_cbc_enabling_analysis($ExtraTimeout,Result)
		procShowErrors
		set LegendList {" * guaranteed: op2 (column) guaranteed to be executable after op1 (row)" " * impossible: op2 impossible to execute after op1"  " * independent: op1 and op2 syntactically independent; op2 stays enabled or disabled after op1" " * activation_indepdendent: op1 does not influence guard of op2, but (op1;op2) could have a different effect than (op2;op1)" " * keep: op2 always stays enabled resp. disabled after op1" " * possible_disable: op2 possible after op1, but can not be enabled only disabled by op1" " * possible_enable: op2 possible after op1, but op2 can not be disabled only enabled by op1"}
		procShowTable $prolog_variables(Result) "Enabling Analysis" "Enabling Information after $spec_desc(operation)" "EnablingInfo" $LegendList ""
    }
}

proc procEnablingRelationsAfterOperation {} {
   global spec_desc
   global OpName AfterOp
   set OpName [procChooseOperation2 "yes" "Compute effect on other $spec_desc(operations)" "AfterOp" "true"]
   if {$OpName != ""} {
        if {$AfterOp} {
		     prolog "tcltk_cbc_enabling_relations_after_operation('$OpName',0,Result)"
		     set Mtxt "Enabling Information after $spec_desc(operation) $OpName for other events"
		} else {
		     prolog "tcltk_cbc_enabling_relations_for_operation('$OpName',0,Result)"
		     set Mtxt "Influence of events on Enabling of $spec_desc(operation) $OpName"
		}
		procShowErrors
		set LegendList {" * fail: this transition is not possible" " * timeout: solver timed out" " * ok: at least one state satisfying invariant was found for this transition"}
		procShowTable $prolog_variables(Result) "Enabling Relations" $Mtxt "EnablingRelations" $LegendList ""
	}
}

proc procSimultEnablingAnalysis {} {
    global spec_desc
    prolog tcltk_cbc_simultaneous_enabling_analysis(Result)
    procShowErrors
    set LegendList {" * impossible: impossible to have both enabled in same state"  " * possible: both $spec_desc(operations_lc) can be enabled in same state" " * time_out: unkown due to time-out" " * -: see other row/column"}
    procShowTable $prolog_variables(Result) "Simultaneous Enabling Analysis" "Simultaneous Enabling Information for $spec_desc(operations)" "SimulEnablingInfo" $LegendList ""
}
proc procDependenceAnalysis {} {
    global spec_desc
    prolog tcltk_cbc_dependence_analysis(Result)
    procShowErrors
    procShowTable $prolog_variables(Result) "Dependence Analysis" "Dependence Information for $spec_desc(operations)" "DependenceInfo" "" ""
}
proc GetStateSpaceInfo {} {
    prolog get_state_space_stats(Visited,T,Processed,Ignored)
    if [prolog tcltk_exists_an_open_node] {
         return "$prolog_variables(Visited) currently seen distinct states"
    } elseif {$prolog_variables(Ignored) == 0} {
         return "all $prolog_variables(Visited) states"
    } else {
         return "all $prolog_variables(Visited) states satisfying SCOPE predicate"
    }
}
proc GetProcessedStateSpaceInfo {} {
    prolog get_state_space_stats(Visited,T,Processed)
    if [prolog tcltk_exists_an_open_node] {
         return "$prolog_variables(Processed) currently processed states (out of $prolog_variables(Visited))"
    } else {
         return "all $prolog_variables(Processed) states"
    }
}
proc procQuickOrCBCOpCoverage {PreciseOrQuick} {
    # start Prolog Task for computing feasibility
    if {$PreciseOrQuick == "precise" && ![procRunPrologTask feasibility]} {
        return
        # user cancelled
    }
    prolog tcltk_operations_covered_info(Result,Occs,$PreciseOrQuick)
    procShowErrors
    set Info [GetStateSpaceInfo]
    set LegendList {" * uncovered: not covered in current state space"  " * number: identifier of state covering MCDC criterion" " * impossible: guard cannot be true in state satisfying invariant" " * possible: not covered in current state space, but feasible according to invariant"}
    procShowTable $prolog_variables(Result) "Operation Coverage ($PreciseOrQuick)" "Operation Coverage in $Info: $prolog_variables(Occs)" "Operation_Coverage" $LegendList ""
}
proc procCheckIfOperationFeasible {} {
	   global OpName
	   set OpName [procChooseOperation]
	   if {$OpName != ""} {
	       set UseInvariant 1
	       set TimeOutFactor 5
	       if [prolog "tcltk_check_if_feasible_operation('$OpName',$UseInvariant,$TimeOutFactor,Result)"] {
	         procInsertHistoryOptionsState
				   tkMessageBox "Feasibility Checking Result for $OpName:  $prolog_variables(Result)"
		   } else {
				   tkMessageBox "Feasibility Checking Failed for $OpName"
		  }
		  procShowErrors
	   }
}
proc procMCDC_Op_Coverage {level} {
    global spec_desc
    prolog tcltk_compute_mcdc_operation_coverage($level,Result)
    procShowErrors
    set Info [GetStateSpaceInfo]
    set LegendList {" * uncovered: not covered in current state space"  " * number: identifier of state covering MCDC criterion"}
    procShowTable $prolog_variables(Result) "MCDC Coverage" "MCDC (level $level) Coverage for guards of $spec_desc(operations) in $Info" "MCDC_Op_Coverage_$level" $LegendList ""
}
proc procMCDC_Inv_Coverage {level} {
    global spec_desc
    prolog tcltk_compute_mcdc_invariant_coverage($level,Result)
    procShowErrors
    set Info [GetStateSpaceInfo]
    set LegendList {" * uncovered: not covered in current state space"  " * number: identifier of state covering MCDC criterion"}
    procShowTable $prolog_variables(Result) "MCDC Coverage" "MCDC (level $level) Coverage for Invariants in $Info" "MCDC_Inv_Coverage_$level" $LegendList ""
}
proc procInv_Coverage {} {
    global spec_desc
    prolog tcltk_get_invariant_coverage(Result)
    procShowErrors
    set Info [GetProcessedStateSpaceInfo]
    set LegendList {" * number: identifier of state covering  criterion"}
    procShowTable $prolog_variables(Result) "Individual Invariant Coverage" "Coverage for Individual Invariants in $Info" "Inv_Coverage" $LegendList ""
}
proc procCFGAnalysis {} {
    global spec_desc
    prolog tcltk_cbc_cfg_analysis(Result)
    procShowErrors
    procShowTable $prolog_variables(Result) "CFG Analysis" "Quick CFG for $spec_desc(operations)" "CFGInfo" "" ""
}
proc procReadWriteMatrix {} {
    global spec_desc
    prolog b_read_write_info:tcltk_read_write_matrix(Result)
    procShowErrors
    procShowTable $prolog_variables(Result) "Read/Write Matrix" "Read/Write Constants/Variables Matrix for $spec_desc(operations)" "ReadWriteMatrix" "" ""
}
proc procVarReadWriteMatrix {} {
    global spec_desc
    prolog b_read_write_info:tcltk_variable_read_write_matrix(check,Result)
    procShowErrors
    procShowTable $prolog_variables(Result) "Variable Read/Write Matrix" "Variable Matrix showing $spec_desc(operations) which read and write them" "VariableReadWriteMatrix" "" ""
}

proc procForcedConstantsTable {} {
    procSetPreferencesForMetaCmd "det_check_constants" "Compute Forced Constants" "for computing forced constants"
    prolog b_state_model_check:tcltk_cbc_constants_det_check(Result)
    procShowErrors
    procShowTable $prolog_variables(Result) "Forced CONSTANTS" "Table showing CONSTANTS whose value is forced with explanations" "ForcedConstantsTable" "" ""
}



proc procReadWriteVarAnalysis_Graph {} {
    # prolog tcltk_create_dependence_graph
    procDependenceEnablingAnalysis_Graph "tcltk_dot_variable_modification_analysis" "rwmatrix"
}

proc procDependenceAnalysis_Graph {} {
    # prolog tcltk_create_dependence_graph
    procDependenceEnablingAnalysis_Graph "tcltk_create_dependence_graph" "dependent"
}

proc procEnablingAnalysis_Graph {} {
    # prolog tcltk_create_enable_graph
    procDependenceEnablingAnalysis_Graph "tcltk_create_enable_graph" "enable"
}

proc procEnablingAnalysis_GraphForPOR {inv en_graph} {
    # prolog tcltk_create_enable_graph_por
        procDependenceEnablingAnalysis_Graph "tcltk_create_enable_graph_por" "enable" $inv $en_graph
}

proc procDisablingAnalysis_GraphForPOR {inv} {
    # prolog tcltk_create_disable_graph_por
  procDependenceEnablingAnalysis_Graph "tcltk_create_disable_graph_por" "disable" $inv
}

proc procCFGAnalysis_Graph {} {
    # prolog tcltk_dot_cfg_analysis
    procDependenceEnablingAnalysis_Graph "tcltk_dot_cfg_analysis" "cfg"
}

proc procDisablingAnalysis {} {
    global spec_desc
    prolog tcltk_disabling_analysis(Result)
    procShowErrors
    procShowTable $prolog_variables(Result) "Disabling Analysis" "Disabling Information for $spec_desc(operations)" "DisablingInfo" "" ""
}

proc procDependenceEnablingAnalysis_Graph {predicate_call suffix args} {
   global strFilename
   set rootName [file rootname $strFilename]
   set dotName {}; set psName {}
   append dotName $rootName "_$suffix.dot"
   append psName $rootName "_$suffix.pdf"
   set CMD {}
   append CMD $predicate_call ('$dotName'
   set i 0
   while {$i < [llength $args]} {
      append CMD , [lindex $args $i]
      incr i
   }
   append CMD )
   # puts "predicate: $CMD"
   prolog $CMD
   procShowErrors
   procVisualiseDotFile $dotName $psName
}

proc procShowLegend {} {
   global LegendTextList
   # puts "Legend: $LegendTextList"
   procSyntaxInfo .enInfo "Legend" "Explanation of table entries:" \
                                     "procDoSyntaxColouring" "normal" "enabling_legend(Summary)"
   # procShowList "$LegendTextList" "Legend" "Explanation of table entries:"
}
proc procShowTable {Result WindowTitleMsg1 Msg2 DefaultSaveName LegendList POR} {
    global tcl_table_package_missing_info
    if {$tcl_table_package_missing_info != ""} {
       puts "### Warning: Cannot use Tktable $tcl_table_package_missing_info !"
       procShowList2 $Result $WindowTitleMsg1 $Msg2 0 0 $DefaultSaveName none
    } else {
       procShowTable2 $Result $WindowTitleMsg1 $Msg2 $DefaultSaveName $LegendList $POR
    }
}

proc procShowTable2 {Result WindowTitleMsg1 Msg2 DefaultSaveName LegendList POR} {
  # Result is a list of lists;
  global cells
	destroy .table
  set f .table
  array set cells {}
	if [Dialog_Create $f $WindowTitleMsg1 -borderwidth 20] {
		message $f.msg -text $Msg2 -aspect 1500
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        package require Tktable
        set listlen [llength $Result]
        set rows $listlen
        set cols [llength [lindex $Result 0]]

        array set cells [setTableCellsArray $Result]
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.table yview" -orient v
        scrollbar $f.frmSource.scrollx -command "$f.frmSource.table xview" -orient h
        # maxheight and maxwidth are set by default to 800 and 600 respectively
        table $f.frmSource.table -rows $rows -cols $cols -variable cells -roworigin 0 -colorigin 0 \
        -titlecols 1 -titlerows 1 -colstretchmode all -maxheight 500\
        -rowstretchmode all -state disabled -xscroll "$f.frmSource.scrollx set" -yscroll "$f.frmSource.scrolly set"
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.scrollx -side bottom -fill x
        pack $f.frmSource.table -expand true -fill both
        set b [frame $f.buttons]
        if {$POR != ""} {
            if {$POR == "false"} {
                set color salmon
                set txt "Model not suitable for Model Checking with Partial Order Reduction"
            } else {
                set color "sea green"
                set txt "Model suitable for Model Checking with Partial Order Reduction"
            }
            message $f.suitableForPOR -text $txt -aspect 2500 -fg $color
            pack $f.msg $f.frmSource $f.suitableForPOR $f.buttons -side top -fill x
        } else {
            pack $f.msg $f.frmSource $f.buttons -side top -fill x
        }

        pack $f.frmSource -pady 5 -expand 1 -fill both
        button $b.save -text "Save..." -command [list procSaveTableToFile $DefaultSaveName $Result]
        pack $b.save -side left
        if {$LegendList != ""} {
            global LegendTextList
            set LegendTextList "$LegendList"
            button $b.legend -text "Legend..." -command {procShowLegend}
            pack $b.legend -side left
        }
        button $b.ok -text Done -command {destroy .table}
        pack $b.ok -side right

        set t $f.frmSource.table
      	$t tag celltag guaranteed
      	$t tag configure guaranteed -background lightgreen

      	$t tag celltag impossible
      	$t tag configure impossible -background pink

      	$t tag celltag independent
      	$t tag configure independent -background lightyellow

      	$t tag celltag neutral
      	$t tag configure neutral -background white

      	$t tag celltag timeout
      	$t tag configure timeout -background RosyBrown

      	$t tag celltag dependent
      	$t tag configure dependent -background lightgreen

      	$t tag celltag syntactic_independent
      	$t tag configure syntactic_independent -background lightyellow

      	$t tag celltag race_dependent
      	$t tag configure race_dependent -background lightgreen

      	$t tag celltag yes
      	$t tag configure yes -background lightgreen

      	$t tag celltag no
      	$t tag configure no -background pink

        for {set i 0} {$i <$rows} {incr i} {
            for {set j 0} {$j < $cols} {incr j} {
                switch -regexp $cells($i,$j) {
                    ^timeout.* {
                        $t tag celltag timeout "$i,$j"
                    }
                    guaranteed {
                        $t tag celltag guaranteed "$i,$j"
                    }
                    impossible {
                        $t tag celltag impossible "$i,$j"
                    }
                    uncovered {
                        $t tag celltag impossible "$i,$j"
                    }
                    cannot_enable {
                        $t tag celltag impossible "$i,$j"
                    }
                    independent {
                        $t tag celltag independent "$i,$j"
                    }
                    keep {
                        $t tag celltag independent "$i,$j"
                    }
                    activation_indepdendent {
                        $t tag celltag independent "$i,$j"
                    }
                    possible.* {
                        $t tag celltag neutral "$i,$j"
                    }
                    syntactic_independent {
                        $t tag celltag syntactic_independent "$i,$j"
                    }
                    dependent {
                        $t tag celltag dependent "$i,$j"
                    }
                    race_dependent {
                        $t tag celltag race_dependent "$i,$j"
                    }
                    (yes|ok|TRUE) {
                        $t tag celltag yes "$i,$j"
                    }
                    (no|FALSE|invariant_violat.*) {
                        $t tag celltag no "$i,$j"
                    }
                }
            }
        }

        # adjusting table columns' width
        for {set c 1} {$c < $cols} {incr c} {
            set maxw [$f.frmSource.table width $c]
            for {set r 0} {$r < $rows} {incr r} {
                set w [string length [$f.frmSource.table get $r,$c]]
                if {$maxw<$w} {
                    set maxw $w
                }
            }
            $f.frmSource.table width $c $maxw
        }

        bind $f.frmSource.table <Double-1> {
            set index [%W index active]
            lassign [split $index ,] row col
            if {[%W get "0,$col"] == "Source"} {
                set content [%W get $index]
                procHightlightSourceCode $content
            }
        }
        #bind $f.frmSource <Return> {destroy $f ; break}
        #bind $f.frmSource <Control-c> {destroy $f ; break}
        #bind $f.frmSource <Escape> {set ok 1 ; break}
	}
  #(session,session_card,session_state)
	#Dialog_Wait $f ok $f.frmSource
	#Dialog_Dismiss $f
}

proc procHightlightSourceCode {content} {
    if [regexp {at line ([0-9]+):([0-9]+) - ([0-9]+):([0-9]+)} $content match l1 c1 l2 c2] {
        # puts "matched: $match"
        # puts "range is $l1.$c1 $l2.$c2"
        focus .main.frmSource.text
        procHighlightPattern $l1.$c1 $l2.$c2
    }
}

proc procSaveTableToFile {DefaultSaveName Result} {
    set types {
        {"Text File" {.txt} }
	    {"CSV File" {.csv}}
	    {"All files"		*}
	}
    set DEBUGFILE [tk_getSaveFile -filetypes $types -initialfile $DefaultSaveName -parent .table -defaultextension ".csv"]
    if {$DEBUGFILE != ""} {
	#puts "Opening File: $DEBUGFILE"
	set csv false
	if {[file extension $DEBUGFILE] == ".csv"} {
		set csv true
	}
	set channel [prob_open "$DEBUGFILE" "w+"]
	foreach i $Result {
	   if {$csv} {
	   set first 1
	    foreach col_item $i {
	        if {$first} {
	           set first 0
	        } else {
	           puts -nonewline $channel ","
	        }
	        if [string is integer $col_item] {
	            puts -nonewline $channel $col_item
	        } else {
	            puts -nonewline $channel "\"$col_item\""
	        }
	    }
	    puts $channel ""
	   } else {
	   	puts $channel $i
	   }
	}
	close $channel
   	    }
}

proc setTableCellsArray {content} {
    set list {}; set row 0
    foreach line $content {
        set col 0
        foreach el $line {
            if [regexp {timeout_(.*)} $el match arg] {
                set new_el "timeout($arg)"
            } else {
                set new_el $el
            }
            lappend list "$row,$col" $new_el
            incr col
        }
        incr row
    }
    return $list
}

#-----------------

# Running Prolog Tasks with Progress Bar

proc procRunPrologTask {task} {
  # prolog "use_module(probsrc(prologTasks))"
  global cancelTask
  set f .runPrologTask
  if [prolog prologTaskStart($task,TaskMsg,Steps)] {
    set cancelTask 0
    set taskCompleted 1
	if {$prolog_variables(Steps) > 0 && [Dialog_Create $f "Run Task $prolog_variables(TaskMsg):" -borderwidth 10]} {
          ttk::progressbar $f.pb1 -orient horizontal -mode determinate -maximum $prolog_variables(Steps) -length 450
          pack $f.pb1 -side top
          label $f.msg0 -text "$prolog_variables(TaskMsg)" -justify center
          label $f.msg1 -text "" -justify left
          label $f.msg2 -text "" -justify left
          label $f.msg3 -text "" -justify left
          pack $f.msg0 $f.msg1 $f.msg2 $f.msg3 -side top
		  button $f.cancel -text Cancel -command {puts "Cancel"; set cancelTask 1}
		  pack $f.cancel -side right
		  bind $f <Control-c> {set cancelTask 1 ; break}
		  bind $f <Return> {set cancelTask 1 ; break}

		  Dialog_Wait_Prepare $f cancelTask $f
		  # TO DO: cancelling by closing dialog window does not work yet
		  set last1Msg ""
		  set last2Msg ""
		  prolog "tools:walltime(WTS)"
		  set wts $prolog_variables(WTS)
			for {set i 0} {$i < $prolog_variables(Steps)} {incr i 1} {
			        #puts "Step = $i"
			        $f.pb1 configure -value $i
			        # update idletasks
			        update
					prolog prologTaskStep($task,$i,Msg)
					$f.msg1 configure -text "$last1Msg"
			        set last1Msg $last2Msg
					$f.msg2 configure -text "$last2Msg"
			        set last2Msg "$prolog_variables(Msg)"
					puts "\nTask message : $prolog_variables(Msg)"
					$f.msg3 configure -text "$prolog_variables(Msg)"
			        update
					if {$cancelTask==1} {
					   prolog prologTaskAbort($task)
					   set i $prolog_variables(Steps)
					   # puts "cancel $i ($cancelTask)"
					   set taskCompleted 0
					}
				}
		prolog prologTaskFinish($task)
		prolog "tools:walltime(WTE)"
		set wt $prolog_variables(WTE)
		set wt [expr $wt-$wts]
		puts "Total runtime for task $task : $wt ms"
	    Dialog_Dismiss $f
	    destroy $f
		procShowErrors
	}
	# puts "Finished $cancelTask"
	return $taskCompleted
  } else {
		tkErrorBox "Cannot start prolog Task $task."
		return 0
  }
}

# ----------------

### TIP #171: Change Default Bindings Behavior
### MouseWheel event is also possible on other widget like tables
proc ::tk::MouseWheel {wFired X Y D {shifted 0}} {
    # Set event to check based on call
    set evt "<[expr {$shifted?{Shift-}:{}}]MouseWheel>"
    # do not double-fire in case the class already has a binding
    if {[bind [winfo class $wFired] $evt] ne ""} { return }
    # obtain the window the mouse is over
    set w [winfo containing $X $Y]
    # if we are outside the app, try and scroll the focus widget
    if {![winfo exists $w]} { catch {set w [focus]} }
    if {[winfo exists $w]} {
  if {[bind $w $evt] ne ""} {
      # Awkward ... this widget has a MouseWheel binding, but to
      # trigger successfully in it, we must give it focus.
      catch {focus} old
      if {$w ne $old} { focus $w }
      event generate $w $evt -rootx $X -rooty $Y -delta $D
      if {$w ne $old} { focus $old }
      return
  }
  # aqua and x11/win32 have different delta handling
  if {[tk windowingsystem] ne "aqua"} {
      set delta [expr {- ($D / 30)}]
  } else {
      set delta [expr {- ($D)}]
  }
  # scrollbars have different call conventions
  if {[string match "*Scrollbar" [winfo class $w]]} {
      catch {tk::ScrollByUnits $w \
           [string index [$w cget -orient] 0] $delta}
  } else {
      set cmd [list $w [expr {$shifted ? "xview" : "yview"}] \
       scroll $delta units]
      # Walking up to find the proper widget handles cases like
      # embedded widgets in a canvas
      while {[catch $cmd] && [winfo toplevel $w] ne $w} {
    set w [winfo parent $w]
      }
  }
    }
}

# -------
# procedure initialise everythings
# -------
proc procMainInit {} {
    global lib_dir
    procComputeApplicationDirectory
    procSetSpecDesc

    global prompt
    set prompt(result) ""
    set prompt(result2) ""
    set prompt(ok) 0

    global curFileTypeOpened
    set curFileTypeOpened "None"

    global strFilename cspstrFilename temporary_unicode_mode
    set strFilename ""
    set cspstrFilename ""
    set temporary_unicode_mode 0

    # set the first machine (a phonebook)
    prologmnf tcltk_set_initial_machine

    procInitPreferences
    procLoadPreferences
    procInitTLC
    procInitLtldata
    procInitCtldata

    # initialise GUI
    procInitGUI
    # initialise prolog
    prologmnf tcltk_initialise

    # get options by default
    #procInsertHistoryOptionsState
    procUpdateSpecDesc
    procUpdateMenusFromPrologInfo
    global typeCheckMode traceUponError
    global expert_user
    set traceUponError 0
    global profilingMode
    set profilingMode 0
    set typeCheckMode 0
    procUpdateDebuggingMode

    procChooseSystemDependentUnicodeSymbols
    # Evaluation View was very slow in unicode mode, but not anymore it Tcl/Tk 8.5.18?
    prolog bvisual2:set_bvisual2_translation_mode(unicode)
    if [file exists "$lib_dir/probcliparser.jar"] {
      # puts "Found Parser"
    } else {
      tkErrorBox "Your installation seems incomplete.\nCannot find probcliparser.jar file in $lib_dir.\nYou will only be able to open machines that have already been parsed."
    }
    procShowErrors

    bind all <MouseWheel> [list ::tk::MouseWheel %W %X %Y %D 0]
    bind all <Shift-MouseWheel> [list ::tk::MouseWheel %W %X %Y %D 1]
    if {[tk windowingsystem] eq "x11"} {
      # Support for mousewheels on Linux/Unix commonly comes through
      # mapping the wheel to the extended buttons.
      bind all <4> [list ::tk::MouseWheel %W %X %Y 120]
      bind all <5> [list ::tk::MouseWheel %W %X %Y -120]
    }
}

# puts "Starting"
procMainInit
# puts "Treating CommandLineArgs"
procTreatCommandLineArgs


#if {$tcl_patchlevel == "8.5.9" && [prolog tools:host_platform(darwin)]} {
#	    tk_messageBox -parent . -icon warning -message "You have version $tcl_patchlevel installed. Please consider installing a newer version of Active Tcl/Tk 8.5 from http://www.activestate.com/activetcl/downloads/."
#}
