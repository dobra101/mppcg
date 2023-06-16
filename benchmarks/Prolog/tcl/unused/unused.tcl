
    #.frmMenu.mnuEdit add cascade -label "Unicode Options" -menu .frmMenu.mnuEdit.mnuUnicode -state disabled
   # menu .frmMenu.mnuEdit.mnuUnicode -tearoff 0
   # .frmMenu.mnuEdit.mnuUnicode add command -label "To Unicode Symbols" -command {procLaunchCmd toUnicodeCommand}
   # .frmMenu.mnuEdit.mnuUnicode add command -label "Remove Unicode Symbols" -command {procLaunchCmd fromUnicodeCommand}

proc toUnicodeCommand {} {
    global strFilename
    set addUnicodeOption "--addUnicode="
    prolog "cspm_file_add_remove_unicode('$strFilename', '$strFilename', '$addUnicodeOption')"
    procReOpenFile
}

proc fromUnicodeCommand {} {
    global strFilename
    set removeUnicodeOption "--removeUnicode="
    prolog "cspm_file_add_remove_unicode('$strFilename', '$strFilename', '$removeUnicodeOption')"
    procReOpenFile
}

proc procToUnicode {} {
    global strFilename
    set csp_ext [file extension $strFilename]
    if { $csp_ext == ".csp" || $csp_ext == ".cspm" } {
    # Enable "Unicode Options" Menu when the loaded File one of the Extensions .csp or .cspm has
          .frmMenu.mnuEdit entryconfigure 14 -state normal
    } else {
          .frmMenu.mnuEdit entryconfigure 14 -state disabled
    }
}


#			 .frmMenu.mnuAnalyse add sep
#             .frmMenu.mnuAnalyse add cascade -label "Slicing" -menu .frmMenu.mnuAnalyse.mnuSlicer
#			 .frmMenu.mnuAnalyse add command -label "AbsInt (experimental)" -command procAbsInt
#			 .frmMenu.mnuAnalyse add command -label "Show Enable Graph (experimental)" -command procFlowEnableGraph
#			 .frmMenu.mnuAnalyse add command -label "Show Enable Graphs (experimental)" -command procFlowEnableGraphs
#			 .frmMenu.mnuAnalyse add command -label "Show Base Guide Automaton (experimental)" -command procBaseGuide
#			 .frmMenu.mnuAnalyse add command -label "Show Event Guide Automaton (experimental)" -command procEventGuide


    # -------------------- Flow menu
#  menu .frmMenu.mnuAnalyse.mnuFlow -tearoff 0
#		.frmMenu.mnuAnalyse.mnuFlow add command -label "Calculate Enable Graph" \
#		   -command {procFlowEnableGraph} -state normal
