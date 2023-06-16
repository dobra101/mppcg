# Simply start the file using wish and then choose the File -> Open menu
# Should report an error but on 8.6 it simply hangs
# is one of a variety of similar problems with 8.6 and maybe also 8.5.9.2

proc procGUI_Menu {} {
    menu .frmMenu -tearoff 0
    . config -menu .frmMenu
    foreach m {File} {
        set $m [menu .frmMenu.mnu$m -tearoff 0]
        .frmMenu add cascade -label $m -menu .frmMenu.mnu$m
    }
    set AccKey "Command"
    .frmMenu.mnuFile add command -label "Open..." -underline 0 -command procOpenFile -accelerator $AccKey+O
    .frmMenu.mnuFile add sep
    .frmMenu.mnuFile add command -label "Quit" -underline 0 -command procDoQuit -accelerator $AccKey+Q
}

proc procOpenFile {} {
    puts "procOpenFile"
    global strFilename curFileTypeOpened
    procWhichDoesNotExist
}

puts "Setting up menu"
procGUI_Menu
puts "Done setting up menu; now choose File -> Open"
