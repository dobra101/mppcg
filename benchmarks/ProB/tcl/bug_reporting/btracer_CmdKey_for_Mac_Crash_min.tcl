# Simply start the file using wish and then choose the File -> Open menu
# Should report an error but on 8.6 it simply hangs
# is one of a variety of similar problems with 8.6 and maybe also 8.5.9.2

bind all <Command-m> {procTestMsg}
bind all <Command-d> {procTestDlg}

proc procGUI_Menu {} {
    menu .frmMenu -tearoff 0
    . config -menu .frmMenu
    foreach m {File} {
        set $m [menu .frmMenu.mnu$m -tearoff 0]
        .frmMenu add cascade -label $m -menu .frmMenu.mnu$m
    }
    set AccKey "Command"
    .frmMenu.mnuFile add command -label "Test MsgBox..." -underline 0 -command procTestMsg -accelerator $AccKey+M
    .frmMenu.mnuFile add command -label "Test Dialog..." -underline 0 -command procTestDlg -accelerator $AccKey+D
    .frmMenu.mnuFile add sep
    .frmMenu.mnuFile add command -label "Quit" -underline 0 -command procDoQuit -accelerator $AccKey+Q
}

proc procTestMsg {} {
    puts "procTest"
    tk_messageBox -parent . -message "This is a message box. This Works."
}

proc procTestDlg {} {
    global mc_prompt
    set f .evaluationview
    puts "setting up dialog"
    if [winfo exists $f] {
        raise $f
    } else {
        puts "calling Dialog_Create"
        if [Dialog_Create $f "Evaluation View" -borderwidth 10] {
            puts "binding destroy"
            bind $f <Command-w> "destroy $f"
		    label $f.msg -text "TestText"
		    set b [frame $f.buttons]
			set op [frame $f.options]
			# pack $f.msg $f.entry $f.buttons $f.options $f.msg2 $f.progress -side top -fill x
			pack $b $op $f.msg -side top -fill x
			# pack $f.entry -pady 5

			button $b.ok -text "Model Check" -command {set mc_prompt(ok) 1}
			button $b.cancel -text Cancel -command {set mc_prompt(ok) 0}
			pack $b.ok -side left
		bind $f <Return> {set mc_prompt(ok) 1 ; break}
		bind $f <Control-c> {set mc_prompt(ok) 0 ; break}

            set mc_prompt(ok) 0

            puts "Dialog_Wait_Prepare"
	        Dialog_Wait_Prepare $f mc_prompt(ok) $f.buttons
            puts "Prepared"

        tkwait variable mc_prompt(ok)
        puts "Dialog_Wait_Finish"

		Dialog_Wait_Finish $f
        puts "Dialog_Dismiss"
        # Dialog_Dismiss $f
        destroy $f

        }
    }
}

proc procDoQuit {} {
   destroy .
}

# ---------------------------------------------

proc Dialog_Create {top title args} {
	global dialog
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
		return 1
	}
}
proc Dialog_Wait_Prepare {top varName {focus {}}} {
    puts "Dialog_Wait_Prepare $top $varName"
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

puts "Setting up menu"
procGUI_Menu
puts "Done setting up menu; now choose File -> Test... using Command-D"
puts "If you start doing Command-D then Tcl/Tk 8.6 hangs immediately"
puts "If you use the procTestDlg from the menu it works; afterwards "
puts "you need to do open the dialog twice with Command-D for the bug to appear"
puts "The Bug appears on Mac with the current Active Tcl/Tk 8.6b1.2 wish"
