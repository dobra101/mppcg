
namespace eval ::evaluationView:: {
    variable allNodes
    array set allNodes {}

    variable evaluation_view_filter_input ""

    variable number_of_marked 0
    variable show_only_marked false

    namespace export openEvaluationView
    namespace export resetEvaluationView
    namespace export updateEvaluationTree
}


proc ::evaluationView::openEvaluationView {} {
    set f .evaluationview

    global tcl_version
    if {$tcl_version<8.5} {
        if [prolog tools:host_platform(windows)] {
            tkErrorBox "This command is not yet available on the Windows platform.\n(You have Tcl/Tk Version $tcl_version.\nThe evaluation view requires 8.5 or newer.)"
        } else {
            tkErrorBox "You have Tcl/Tk Version $tcl_version.\nThe evaluation view requires 8.5 or newer."
        }
    } elseif [winfo exists $f] {
        raise $f
    } else {
        if [Dialog_Create $f "Evaluation View" -borderwidth 10 -width 750] {
            set CMD [procGetAccKey]
            bind $f <$CMD-w> "destroy $f"
            bind $f <$CMD-d> [namespace code "showData; break;"]
            fillEvaluationView $f
        }
    }
}

proc ::evaluationView::updateEvaluationTree {} {
    if [winfo exists .evaluationview] {
        updateEvalNodes2 .evaluationview.tree {} 1
    }
}

proc ::evaluationView::fillEvaluationView {f} {
    foreach {c} [winfo children $f] {
        destroy $c
    }
    # search box on top
    set srch $f.search
    set t $f.tree

    frame $srch
    message $srch.msg -text "Search:" -aspect 1000
    set filterValidateCmd [namespace code "updateFilterinput $t %P"]
    entry $srch.entry -validatecommand $filterValidateCmd -validate key
    button $srch.clr -text "Clear" -command "$srch.entry delete 0 end"
    #button $srch.print -text "Print..." -command [namespace code "printMarked"]
    pack $srch.clr $srch.entry $srch.msg -side right
    #pack $srch.print -side right
    pack $srch -side top -fill x

    scrollbar $f.scrolly -orient vertical   -command "$t yview"
    scrollbar $f.scrollx -orient horizontal -command "$t xview"
    ttk::treeview $t -columns value -yscrollcommand "$f.scrolly set" -xscrollcommand "$f.scrollx set" -height 15
    $t column value -minwidth 100
    $t column value -width 320
    $t column #0 -minwidth 120
    $t column #0 -width 380
    $t heading #0 -text "Expression"
    $t heading #0 -command {tkMessageBoxNoParent "Here you can inspect predicates and (sub-)expressions."}
    $t heading value -text "Value"
    $t heading value -command {tkMessageBoxNoParent "This column contains the value of the corresponding expression or predicate."}
    $t tag configure inac   -foreground grey
    $t tag configure error  -foreground red
    $t tag configure ptrue  -foreground darkgreen
    $t tag configure pfalse -foreground darkred
    $t tag configure unseen -foreground yellow -background black
    $t tag configure marked -foreground gray30
    $t tag configure marked -background snow2
    $t tag configure explaintrue  -foreground green4
    $t tag configure explainfalse -foreground sienna3
    global tcl_dir
	# image create photo MarkedImg -format gif -fil "$tcl_dir/icons/Marked.gif"
    # $t tag configure marked -image MarkedImg
    if [prolog tcltk_bv_get_tops(Tops)] {
        set tops $prolog_variables(Tops)
        createItemInfo {}
        # The top node is always opened
        setNodeStatus {} true
        setNodeChildren {} $tops
        foreach topvalue $tops {
            evalNodeStructure $t {} $tops
            evalNodeValues $t {}
        }
    } {
        puts "tcltk_bv_get_tops/1 failed"
    }
    bind $t <<TreeviewOpen>> [namespace code "openEvalNode $t"]
    bind $t <<TreeviewClose>> [namespace code "closeEvalNode $t"]
    bind $t <Double-Button-1> [namespace code "if \[showEvaluationElement $t %x %y\] {break}"]
    bind $t <Return> [namespace code "toggleMark $t"]
    set rightclickcode [namespace code "createContextMenu $t %X %Y %x %y"]
    bind $t <<RIGHTCLICK>> $rightclickcode
    pack $f.scrolly -side right -fill y
    pack $t -fill both -expand yes
    pack $f.scrollx -side bottom -fill x
}

proc ::evaluationView::showEvaluationElement {t x y} {
    set col [$t identify column $x $y]
    # remove *** so that double-click only has effect on value column
    if {$col == "***#0"} {
        set stop_event_loop 0
    } else {
        set item [$t identify row $x $y]
        showEvaluationElementDetail $item
        set stop_event_loop 1
    }
    return $stop_event_loop;
}

proc ::evaluationView::showEvaluationElementDetailAsTable {itemid} {
  if {$itemid != ""} {
    if [prolog tcltk_bv_get_pp_formula_string($itemid,Label)] {
        # TODO: it would be better to retrieve formula in Prolog
        procShowExpressionAsTable $prolog_variables(Label)
    } else {
        puts "Cannot retrive item $itemid as string"
    }
  }
}

# allow modifying variable values by the user
proc ::evaluationView::modifyEvaluationElementValue {itemid} {
  if {$itemid != ""} {
    if [prolog tcltk_bv_get_structure($itemid,Label,_)] {
        puts "Modify Variable: $prolog_variables(Label)"
        prolog tcltk_bv_get_unlimited_value($itemid,TextC,Tag)
        global IdToModify
        set IdToModify $prolog_variables(Label)
        procShowText7 "$prolog_variables(TextC)" "Enter new value for $prolog_variables(Label):" "Cancel" "Modify $prolog_variables(Label)" "::evaluationView::modifyEvaluationElementValueAction" "" ""
    }
  }
}
proc ::evaluationView::modifyEvaluationElementValueAction {} {
   global IdToModify
   puts "Modifying $IdToModify"
   set NewVal [.showtext.frmSource.text get 1.0 end]
   # remove newlines, not allowed by Prolog:
   regsub -all -- \[\r\n\t] $NewVal " " NewVal
   puts "NewVal = $NewVal"
   prolog "tcltk_add_user_modify_variable_transition('$IdToModify','$NewVal')"
   procShowErrors
   procInsertHistoryOptionsState
}

proc ::evaluationView::procShowBVExpressionAsDotGraph {itemid} {
  set dotName [procGetDefaultDotName]
	if [prolog "tcltk_bv_show_formula_as_dot_graph($itemid,'$dotName')"] {
	  procShowErrors
	  procVisualiseDotFileDefault $dotName
	} else {
	  procShowErrors1 "$itemid" none
	}
}
proc ::evaluationView::procShowBVExprValueAsDotTree {itemid} {
  set dotName [procGetDefaultDotName]
	if [prolog "tcltk_bv_show_value_as_dot_tree($itemid,'$dotName')"] {
	  procShowErrors
	  procVisualiseDotFileDefault $dotName
	} else {
	  procShowErrors1 "$itemid" none
	}
}
proc ::evaluationView::procShowBVExpressionAsDotTree {itemid} {
  set dotName [procGetDefaultDotName]
	if [prolog "tcltk_bv_show_formula_as_dot_tree($itemid,'$dotName')"] {
	  procShowErrors
	  procVisualiseDotFileDefault $dotName
	} else {
	  procShowErrors1 "$itemid" none
	}
}

proc ::evaluationView::showEvaluationElementDetailCoverage {itemid} {
  if {$itemid != ""} {
    if [prolog tcltk_bv_get_pp_formula_string($itemid,Label)] {
        procEvaluateExpressionOverStatespace $prolog_variables(Label) ""
    }
  }
}

proc ::evaluationView::showEvaluationElementDetail {itemid} {
  if {$itemid != ""} {
    if [prolog tcltk_bv_get_structure($itemid,Label,_)] {
        set label $prolog_variables(Label)
        if [prolog tcltk_bv_get_unlimited_value($itemid,Text,Tag)] {
            # puts "show  id='$itemid' label='$label' text='$prolog_variables(Text)' tag='$prolog_variables(Tag)'"
            procShowText "$prolog_variables(Text)" $label
        }
    }
  }
}
proc ::evaluationView::showEvaluationElementOrigin {itemid} {
  if {$itemid != ""} {
    if [prolog bv_show_formula_origin($itemid,Desc)] {
        puts "Showing $prolog_variables(Desc)"
    }
  }
}

proc ::evaluationView::resetEvaluationView {} {
    variable allNodes
    variable evaluation_view_filter_input
    variable number_of_marked
    variable show_only_marked
    array set allNodes {}
    set evaluation_view_filter_input ""
    set number_of_marked 0
    set show_only_marked false
    set f .evaluationview
    if [winfo exists $f] {
        fillEvaluationView $f
    }
}

proc ::evaluationView::openEvalNode {tree} {
    # expand a node
    variable evaluation_view_filter_input
    set item [$tree focus]
    set children [$tree children $item]
    evalNodeStructure $tree $item $children
    setNodeStatus $item true
    if {[prolog bv_get_formula_functor_symbol($item,Symbol)] && [prolog "bv_is_child_formula($item)"]} {
         # show a more compact form with just functor/operator, TODO: disable for explanation nodes?
         $tree item $item -text "$prolog_variables(Symbol)"
    }
    #procEvalNodeValues $tree $item
    updateEvalNodes2 $tree $item 1
    # filter the nodes if a filter is set
    if [filterIsSet] {
        filterSubstrings $tree $item $evaluation_view_filter_input
    }
}

proc ::evaluationView::closeEvalNode {tree} {
    set item [$tree focus]
    setNodeStatus $item false
    if {[prolog bv_get_formula_functor_symbol($item,_)] && [prolog "bv_is_child_formula($item)"]} {
         # reset text to full formula again
         prolog tcltk_bv_get_structure($item,Label,_)
         $tree item $item -text "$prolog_variables(Label)"
    }
}

proc ::evaluationView::updateEvalNodes2 {tree parent parentopen} {
    #puts "update2: parent=$parent parentopen=$parentopen"
    if $parentopen {
        evalNodeValues $tree $parent
    } else {
        foreach child [$tree children $parent] {
            $tree item $child -values [list "should be hidden"] -tags unseen
        }
    }
    set children [$tree children $parent]
    foreach child $children {
        set childopen [$tree item $child -open]
        set isopen [expr $parentopen && $childopen]
        updateEvalNodes2 $tree $child $isopen
    }
}

proc ::evaluationView::evalNodeValues {tree parent} {
    set children [$tree children $parent]
    set ids [join $children ","]
    set lc [list $children]
    prolog "tcltk_bv_get_values(\[$ids\],Values,Tags)"
    foreach child $children Value $prolog_variables(Values) tag $prolog_variables(Tags) {
            set value "  $Value"
            if [isNodeMarked $child] {
                set tags [list $tag marked]
            } else {
                set tags [list $tag]
            }
            $tree item $child -values [list $value] -tags $tags
    }
}

proc ::evaluationView::evalNodeStructure {tree parent nodes} {
    foreach id $nodes {
        evalNodeStructure2 $tree $parent $id
    }
}

proc ::evaluationView::evalNodeStructure2 {tree parent id} {
    if [prolog tcltk_bv_get_structure($id,Label,Subs)] {
        set label $prolog_variables(Label)
        set subs  $prolog_variables(Subs)
        if [$tree exists $id] {
            $tree item $id -text $label
        } {
            $tree insert $parent end -id $id -text $label
            createItemInfo $id
        }
        foreach sub $subs {
            if [$tree exists $sub] {
                #puts "  sub: $sub exists, child of $id"
            } {
                #puts "  sub: $sub created, child of $id"
                $tree insert $id end -id $sub
                createItemInfo $sub
            }
        }
        setNodeChildren $id $subs
    } {
        puts "tcltk_bv_get_structure($id,Label,Subs) failed"
    }
}

proc ::evaluationView::hideNodes {tree items} {
    #puts "evalview_hideNodes: items=$items"
    foreach item $items {
        set parent [$tree parent $item]
        setHasNodeHiddenChildren $parent true
    }
    $tree detach $items
}

proc ::evaluationView::showAll {tree item} {
    #puts "evalview_showAll: item=$item"
    set all [getNodeChildren $item]
    if [hasNodeHiddenChildren $item] {
        #puts "evalview_showAll: item has hidden subitems"
        # detach all items first to ensure that we get the old order back
        $tree detach [$tree children $item]
        foreach child $all {
            $tree move $child $item end
        }
        setHasNodeHiddenChildren $item false
    }
    foreach child $all {
        showAll $tree $child
    }
}

proc ::evaluationView::filterSubstrings {tree item filter} {
    set removelist [list]
    filterSubstrings2 $tree $item $filter removelist
    hideNodes $tree $removelist
}

proc ::evaluationView::filterSubstrings2 {tree parent filter removelistvar} {
    #puts "evalview_filterSubstrings2: parent=$parent filter=$filter"
    upvar $removelistvar removelist
    set children [$tree children $parent]
    set count 0
    foreach child $children {
        if [isNodeOpen $child] {
            set pcount [filterSubstrings2 $tree $child $filter removelist]
        } else {
            set pcount 0
        }
        if {$pcount > 0} {
            #puts "item $child not filtered because it has non-filtered subitems"
            set count [expr $count + $pcount]
        } else {
            set filterPassed [passFilter $tree $child $filter]
            if {$filterPassed} {
                #puts "filter keep: $child"
                incr count
            } else {
                #puts "filter remove: $child"
                lappend removelist $child
            }
        }
    }
    return $count
}

proc ::evaluationView::passFilter {tree item filter} {
    variable show_only_marked
    if {$show_only_marked} {
        set markok [isNodeMarked $item]
    } else {
        set markok true
    }
    if {$markok} {
        if {$filter == ""} {
            set result true
        } else {
            set text [$tree item $item -text]
            set first [string first $filter $text]
            set result [expr $first >= 0]
        }
    } else {
        set result false
    }
    return $result
}

proc ::evaluationView::filterIsSet {} {
    variable evaluation_view_filter_input
    variable show_only_marked
    set searchEmpty [expr {$evaluation_view_filter_input} == {""}]
    return [expr $show_only_marked || ! $searchEmpty]
}

proc ::evaluationView::updateFilterinput {tree input} {
    variable evaluation_view_filter_input
    set old $evaluation_view_filter_input
    # we have to react only if the filter has changed
    if {$old != $input} {
        set filterActive [filterIsSet]
        if {!$filterActive} {
            # just reset the view without filtering if the filter string is empty
            showAll $tree {}
        } else {
            # check if the old filter text is a substring of the new filter text
            # if yes, we can just apply the stronger filter on the already filtered
            # elements
            set old_length [string length $old]
            set index [string first $old $input]
            # we have to check if the old string is empty because "string first" does not
            # report an empty string as a substring
            if {$index < 0 && $old_length>0 } {
                # reset the view if the filter has been changed more than just getting
                # "stricter"
                #puts "evalview_updateFilterinput: reset, filter=$input"
                showAll $tree {}
            } else {
                #puts "evalview_updateFilterinput: incremental, filter=$input"
            }
            filterSubstrings $tree {} $input
        }
        set evaluation_view_filter_input $input
    }
    return 1
}

proc ::evaluationView::showData {} {
    variable allNodes
    puts "all nodes:"
    foreach item [array names allNodes] {
        puts "  '$item' -> $allNodes($item)"
    }
    puts "----"
}

proc ::evaluationView::isNodeOpen {item} {
    return [getItemInfo $item 0]
}
proc ::evaluationView::setNodeStatus {item isopen} {
    setItemInfo $item 0 $isopen
}

proc ::evaluationView::isNodeExplored {item} {
    return [getItemInfo $item 1]
}

proc ::evaluationView::hasNodeHiddenChildren {item} {
    return [getItemInfo $item 2]
}
proc ::evaluationView::setHasNodeHiddenChildren {item hasHiddenChildren} {
    setItemInfo $item 2 $hasHiddenChildren
}

proc ::evaluationView::getNodeChildren {item} {
    return [getItemInfo $item 3]
}
proc ::evaluationView::setNodeChildren {item children} {
    setItemInfo $item 1 true
    setItemInfo $item 3 $children
}

proc ::evaluationView::isNodeMarked {item} {
    return [getItemInfo $item 4]
}
proc ::evaluationView::setNodeMarked {item ismarked} {
    variable number_of_marked
    set old [isNodeMarked $item]
    if {$ismarked && ! $old} {
        setItemInfo $item 4 $ismarked
        incr number_of_marked
    }
    if {(! $ismarked) && $old} {
        setItemInfo $item 4 $ismarked
        set number_of_marked [expr $number_of_marked - 1]
    }
}

proc ::evaluationView::getItemInfo {item pos} {
    variable allNodes
    #puts "getItemInfo '$item' $pos"
    return [lindex $allNodes($item) $pos]
}

proc ::evaluationView::setItemInfo {item pos value} {
    variable allNodes
    #puts "setItemInfo '$item' $pos: '$value'"
    set oldlist $allNodes($item)
    set newlist [lreplace $oldlist $pos $pos $value]
    set allNodes($item) $newlist
}

proc ::evaluationView::createItemInfo {item} {
    #puts "createItemInfo '$item'"
    variable allNodes
    set allNodes($item) [list false false false [list] false]
}

proc ::evaluationView::toggleMark {tree} {
    set sel [$tree selection]
    foreach item $sel {
        set tags [$tree item $item -tags]
        set pos [lsearch -exact $tags marked]
        if {$pos < 0} {
            lappend tags marked
            setNodeMarked $item true
        } else {
            set tags [lreplace $tags $pos $pos [list]]
            setNodeMarked $item false
        }
        $tree item $item -tags $tags
    }
}

proc ::evaluationView::printMarked {} {
    set items [list]
    findMarkedItems {} items
    set types [list {"All files" *}]
    set machinesPath [getMachinesPath]
    set valFile [tk_getSaveFile -filetypes $types -initialdir $machinesPath]
    if {$valFile != ""} {
        prolog "current_expression(CurID,_)"
        set curId $prolog_variables(CurID)
        set ids [join $items ","]
        prolog "bvisual2:bv_print_to_file(\[$ids\],$curId,'$valFile')"
    }
}

proc ::evaluationView::saveAllValues {} {
    set types [list {"All files" *}]
    set machinesPath [getMachinesPath]
    set valFile [tk_getSaveFile -filetypes $types -initialdir $machinesPath]
    if {$valFile != ""} {
        prolog "current_expression(CurID,_)"
        set curId $prolog_variables(CurID)
        prolog "bvisual2:bv_write_all_variables_and_constants($curId,'$valFile')"
    }
}

proc ::evaluationView::findMarkedItems {item markedvar} {
    upvar $markedvar marked
    if [isNodeMarked $item] {
        lappend marked $item
    }
    set children [getNodeChildren $item]
    foreach child $children {
        findMarkedItems $child marked
    }
}

proc ::evaluationView::createContextMenu {t X Y x y} {
    variable number_of_marked

    set sel [$t selection]
    set sell [llength $sel]
    if {$sell > 0} {
        set markmenu normal
    } else {
        set markmenu disabled
    }
    if {$number_of_marked > 0} {
        set printmenu normal
    } else {
        set printmenu disabled
    }

    set e .evalviewpopup
    destroy $e
    menu $e -tearoff 0
    set item [$t identify row $x $y]
      if {$item != "" && [prolog bv_formula_extended_description($item,Desc)]} {
       #  $e add command -label "Description (@desc pragma):" -state disabled
        set lines [split $prolog_variables(Desc) "\n"]
        foreach i $lines {
            $e add command -label [string trimleft "$i" " "] -state disabled
        }
        $e add separator
      }
      $e add command -label "Full Value..." -command [namespace code "showEvaluationElementDetail $item"] -state normal
      if {$item != "" && [prolog bv_is_typed_formula($item)]} {
          if [prolog bv_is_typed_predicate($item)] {
             $e add separator
             $e add command -label "Predicate As Dot Tree..." -command [namespace code "procShowBVExpressionAsDotTree $item"] -state normal
          } else {
              $e add command -label "Value As Dot Graph..." -command [namespace code "procShowBVExpressionAsDotGraph $item"] -state normal
              $e add command -label "Value As Dot Tree..." -command [namespace code "procShowBVExprValueAsDotTree $item"] -state normal
              global expert_user
              if {$expert_user} {
                 $e add command -label "Value As Table..." -command [namespace code "showEvaluationElementDetailAsTable $item"] -state normal
              }
              $e add separator
              if ![prolog bv_is_typed_identifier($item,_)] {
                 # for simple ids it makes no sense to show the expression tree
                 $e add command -label "Expression As Dot Tree..." -command [namespace code "procShowBVExpressionAsDotTree $item"] -state normal
             } else {
                 $e add command -label "Modify Value..." -command [namespace code "modifyEvaluationElementValue $item"] -state normal
             }
          }
      }
      $e add separator
      if { ![prolog "specfile:animation_minor_mode(eventb)"] } {
          $e add command -label "Show Origin..." -command [namespace code "showEvaluationElementOrigin $item"] -state normal
          $e add separator
      }
      $e add command -label "Mark all selected rows" -command [namespace code "setMark $t $x $y"] -state $markmenu
      $e add command -label "Unmark all selected rows" -command [namespace code "unsetMark $t $x $y"] -state $markmenu
      $e add command -label "Clear Selection" -command [namespace code "clearSelection $t"] -state $markmenu
      $e add separator
      $e add command -label "Add User Formula..." -command [namespace code "addUserFormula"] -state normal
      $e add separator
      $e add command -label "Coverage for full Statespace..." -command [namespace code "showEvaluationElementDetailCoverage $item"] -state normal
      $e add separator
    if {1==2} {
		if {[isNodeMarked $item]} {
		   $e add command -label "Unmark" -command [namespace code "toggleMarkIndividualItem $t $item"] -state normal
		} else {
		   $e add command -label "Mark" -command [namespace code "toggleMarkIndividualItem $t $item"] -state normal
		}
    }
    $e add checkbutton -label "Show only marked rows" -variable ::evaluationView::show_only_marked -onvalue true -offvalue false -command [namespace code "limitToMarked $t"]
    $e add separator
    $e add command -label "Save values of marked rows..." -command [namespace code "printMarked"] -state $printmenu
    $e add command -label "Save all values..." -command [namespace code "saveAllValues"]
    tk_popup $e $X $Y
}

proc ::evaluationView::clearSelection {t} {
   $t selection set {}
}

proc ::evaluationView::toggleMarkIndividualItem {t item} {
        if {! [isNodeMarked $item]} {
            set tags [$t item $item -tags]
            lappend tags marked
            $t item $item -tags $tags
            setNodeMarked $item true
        }  else {
            set tags [$t item $item -tags]
            set pos [lsearch -exact $tags marked]
            set tags [lreplace $tags $pos $pos [list]]
            $t item $item -tags $tags
            setNodeMarked $item false
        }
       ::evaluationView::clearSelection $t
}

proc ::evaluationView::setMark {t X Y} {
    set sel [$t selection]
    foreach item $sel {
        if {! [isNodeMarked $item]} {
            set tags [$t item $item -tags]
            lappend tags marked
            $t item $item -tags $tags
            setNodeMarked $item true
        }
    }
    # ::evaluationView::clearSelection $t
}

proc ::evaluationView::unsetMark {t X Y} {
    set sel [$t selection]
    foreach item $sel {
        if [isNodeMarked $item] {
            set tags [$t item $item -tags]
            set pos [lsearch -exact $tags marked]
            set tags [lreplace $tags $pos $pos [list]]
            $t item $item -tags $tags
            setNodeMarked $item false
        }
    }
    # ::evaluationView::clearSelection $t
}

proc ::evaluationView::limitToMarked {tree} {
    variable show_only_marked
    variable evaluation_view_filter_input
    if {! $show_only_marked} {
        showAll $tree {}
    }
    if [filterIsSet] {
        filterSubstrings $tree {} $evaluation_view_filter_input
    }
}

proc ::evaluationView::addUserFormula {} {
    set Pred [Dialog_Promptww "Enter User Formula" "Enter Formula (Predicate or Expression) to Analyse (use B syntax):" 90 "" ""]
    if {$Pred != ""} {
        addUserFormulaToEvalView $Pred
    }
}

proc ::evaluationView::addUserFormulaToEvalView {Pred} {
        set P [escapeChars $Pred]
		if [prolog "bvisual2:tcltk_register_new_user_formula('$P')"] {
			#  TO DO: ideally we should only force re-evaluation of the user_predicates node
			destroy .evaluationview
			openEvaluationView
		}
		procShowErrors
}
