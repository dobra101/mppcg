
namespace eval ::astInspector:: {
    namespace export openAstInspector
    namespace export resetAstInspector
}

proc ::astInspector::openAstInspector {} {
    set f .astInspectorView

    global tcl_version
    if {$tcl_version<8.5} {
        if [prolog tools:host_platform(windows)] {
            tkErrorBox "This command is not yet available on the Windows platform.\n(You have Tcl/Tk Version $tcl_version.\nThe AST inspector view requires 8.5 or newer.)"
        } else {
            tkErrorBox "You have Tcl/Tk Version $tcl_version.\nThe AST inspector view requires 8.5 or newer."
        }
    } elseif [winfo exists $f] {
        raise $f
    } else {
        if [Dialog_Create $f "AST Inspector" -borderwidth 10 -width 400] {
            set CMD [procGetAccKey]
            bind $f <$CMD-w> "destroy $f"
            fillAstInspector $f
        }
    }
}

proc ::astInspector::resetAstInspector {} {
    set f .astInspectorView
    if [winfo exists $f] {
        fillAstInspector $f
    }
}

proc ::astInspector::fillAstInspector {f} {
    foreach {c} [winfo children $f] {
        destroy $c
    }

    set t $f.tree

    scrollbar $f.scrolly -orient vertical   -command "$t yview"
    scrollbar $f.scrollx -orient horizontal -command "$t xview"
    ttk::treeview $t -columns {type infos ids index} -displaycolumns {type infos ids} -yscrollcommand "$f.scrolly set" -xscrollcommand "$f.scrollx set"
    $t column type  -minwidth 100
    $t column infos -minwidth 100
    $t column ids   -minwidth 100
    $t column #0 -minwidth 100
    $t heading #0 -text "Node"
    $t heading type  -text "Type"
    $t heading infos -text "Infos"
    $t heading ids   -text "Quantified Ids"

    # The index of the root element is []
    $t set {} index ""
    initItem $t {}
    initChildren $t {}

    bind $t <<TreeviewOpen>> [namespace code "openASTnode $t"]

    pack $f.scrolly -side right -fill y
    pack $t -fill both -expand yes
    pack $f.scrollx -side bottom -fill x
}

proc ::astInspector::openASTnode {t} {
    initChildren $t [$t focus]
}

proc ::astInspector::initChildren {t elem} {
    foreach child [$t children $elem] {
        initItem $t $child
    }
}

proc ::astInspector::initItem {t elem} {
    if [$t tag has opened $elem] {
        return
    }
    # The index has a form x,y,z
    set index [$t set $elem index]
    # As Prolog list it is [x,y,z]
    set pindex "\[$index\]"
    # Prepare to add an item: Add a comma if the list is not empty
    if {$index == ""} {
        set cindex $index
    } {
        set cindex "$index,"
    }
    if [prolog ast_inspector:tcltk_get_node_info($pindex,Txt,Type,Infos,QuantIDs,NrSubs)] {
        $t item $elem -text "$prolog_variables(Txt)"
        $t set $elem type  "$prolog_variables(Type)"
        $t set $elem infos "$prolog_variables(Infos)"
        $t set $elem ids   "$prolog_variables(QuantIDs)"
        set n $prolog_variables(NrSubs)
        for {set si 0} {$si < $n} {incr si} {
            set f [$t insert $elem end]
            $t set $f index "$cindex$si"
        }
        $t tag add opened $elem
    }
}
