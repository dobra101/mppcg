
###########   JENS

proc procFlowEnableGraph {} {
    global strFilename
    if { [prolog "specfile:animation_minor_mode(eventb)"] } {
       if [procSpecFileSuccessfullyLoaded] {
		  set rootName [file rootname $strFilename]
		  set dotName {}
		  set psName {}
		  append dotName $rootName "_flow_eg.dot"
		  append psName $rootName ".ps"
		  prolog "flow:create_enable_graph('$dotName')"
		  procShowErrors
		  procVisualiseDotFile $dotName $psName
       } else {
          tkErrorBox "No specification file loaded. Cannot display FlowEnableGraph."
       }
    } else {
      tkErrorBox "Flow Graphs are only available for Event-B."
    }
}
proc procFlowEnableGraphs {} {
    global strFilename
    if { [prolog "specfile:animation_minor_mode(eventb)"] } {
       if [procSpecFileSuccessfullyLoaded] {
		  set rootName [file rootname $strFilename]
		  set dotName {}
		  set psName {}
		  append dotName $rootName "_flow_eg.dot"
		  append psName $rootName ".ps"
		  prolog "flow:create_enable_graphs('$dotName')"
	      procShowErrors
		  procVisualiseDotFile $dotName $psName
       } else {
          tkErrorBox "No specification file loaded. Cannot display FlowEnableGraph."
       }
    } else {
      tkErrorBox "Flow Graphs are only available for Event-B."
    }
}

proc procBaseGuide {} {
    global strFilename
    if { [prolog "specfile:animation_minor_mode(eventb)"] } {
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
global strFilename
if { [prolog "specfile:animation_minor_mode(eventb)"] } {
   if {$strFilename != ""} {

   set Event [Dialog_Promptww "Enter Event Name:" 70 "" ""]
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
