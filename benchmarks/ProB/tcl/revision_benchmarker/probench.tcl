#!/usr/bin/env tclsh

package require Tk

#  Probench Revision Benchmark tool for ProB



proc main {} {
    eval global {tcl_platform argv argv0}
    #puts $tcl_platform(os)

    global putsDest
    global putsistxtbox
    set putsDest puts
    set putsistxtbox 0

    global activetests
    set activetests 0

    global silentrun
    set silentrun 0

    global notempdelete
    set notempdelete 0; #do not delete temporary files while building if true

    #save results to codespeed server
    global savetocodespeed
    set savetocodespeed 1

    #directory at server
    global sourcedir
    set sourcedir ""
    global sourcedirSVN
    set sourcedirSVN "trunk/prolog/"
    global sourcedirGIT
    set sourcedirGIT ""; #no other directory

    #use local probcli
    global uselocal
    set uselocal 0

    #predefined local path to probcli is used only if GUI is started with --local <PATH> and no other options
    global predefinedLocal
    set predefinedLocal ""



    global homedir
    set homedir [pwd]
    cd [file dirname $argv0]

    set arglist [split $argv " "]
    if {[lsearch [string tolower $arglist] -?]>-1||[lsearch [string tolower $arglist] --help]>-1||[lsearch [string tolower $arglist] -help]>-1} {
        #puts "possible parameters:\n -autobench (or -autorun) : gets and builds current revision, performs benchmarks (from autobench.input) and saves the result to a running codespeed server.\n -env <NAME> : define the environment name used for codespeed (must be defined in codespeed first!) (if -autobench is used without -env, environment Cobra will be used)\n"
        puts "ProBench GUI-version\n\npossible parameters:\n\
         -local <PATH>  : use local probcli in <PATH> \n\
         -input <FILE>  : use the given input file\n\
         -iterations <NUMBER> : number of iterations for benchmarks (default: 2)\n\
         -timeout <MS>  : timeout for probcli (ms), 0=no timeout (default: 0)\n\n\
         Do not use parameters to see GUI. Parameters can be used to run tests with local probcli. For autobench, use probench_nongui.tcl only.\n"

        exit
    }



    wm title . "Probench"
    wm geometry . 1x1+[expr {([winfo screenwidth .] / 2)}]+[expr {([winfo screenheight .] / 2)}]

    setvars1

    if {[string length $arglist] >0} {
     #parameters given

     if {[lsearch [string tolower $arglist] -autorun]>-1||[lsearch [string tolower $arglist] --autorun]>-1||
         [lsearch [string tolower $arglist] -autobench]>-1||[lsearch [string tolower $arglist] --autobench]>-1} {

        puts "***Please use probench_nongui.tcl for autobench.***\n"
        exit -1

        if {[tk windowingsystem] eq "win32"} {
            console show
            console eval {wm geometry . 150x50+00+0}
        }
        puts "autobench modus active...\n"
        set silentrun 1

        global globEnvironment
        set globEnvironment ""
        set foundautobenchenv 0
        foreach {xarg} [split $arglist " "] {
          if {$foundautobenchenv} {
             set globEnvironment $xarg
             set foundautobenchenv 0
             puts "used environment: $globEnvironment \n"
          }
          if {[string tolower $xarg] eq "-env"} {
            set foundautobenchenv 1
          }
          if {[string tolower $xarg] eq "-notempdelete"} {
            set notempdelete 1
          }
        }

        if {[readINI 1]} {
            performautorun
        }
     }

     #other parameters than autorun
     set helpargs [string tolower $arglist]
     if {[lsearch $helpargs -local] >-1 || [lsearch $helpargs --local] >-1} {
        #local probench
        if {[lsearch $helpargs -input] >-1 || [lsearch $helpargs --input] >-1} {
          # --local and --input given
          if {[readINI 1]} {
            runCommandlineTests $arglist
          }

        } else {
          # --local found, but not --input: Start GUI with predefined local path

          set predefinedLocal ""
          set foundlocal 0
          foreach {xarg} [split $arglist " "] {
            if {$foundlocal} {
               set predefinedLocal [file normalize $xarg]
               set foundlocal 0
               puts "predefined local probcli path: $predefinedLocal \n"
               if {![file exists $xarg/probcli]} {puts "Warning: Path $xarg does not contain probcli"}
            }
            if {[string tolower $xarg] eq "-local" || [string tolower $xarg] eq "--local"} {
              set foundlocal 1
            }
          }
          if {[readINI 1]} {
            showstart
          }
        }
     } else {
       if {[lsearch $helpargs -input] >-1 || [lsearch $helpargs --input] >-1} {
         # --input given, but --local missing
         puts "***Error: missing parameter: --local <PATH>***"
         exit -1
       } else {
          if {[string tolower $helpargs] eq "-notempdelete"} {
            set notempdelete 1
            if {[readINI 1]} {
              # start
              showstart
            }
          } else {
            puts "Unknown parameter(s)."
            exit -1
          }
       }
     }

    } elseif {[readINI 1]} {
        # no parameters
        showstart
    }
}



proc runCommandlineTests {commandArgs} {

    #set thisdir [pwd]
    #cd $homedir; cd [file dirname $argv0]
    #if {![file exists ./savecodespeed_probench.py]} {
    #  putsTxt "***error: savecodespeed_probench.py not found. It must be in the same directory as this file.***\n"
    #  exit
    #}
    #cd $thisdir
    eval global {probrev probrev2 useeveryXrevisionINPUT}


    global probpath
    set probpath ""
    global uselocal
    set uselocal 0
    global xinput
    global xrepeatings
    global xtimeout
    global xspecialopts
    set xspecialopts ""
    global xusedrev
    set xusedrev 0
    global xusedrevB
    set xusedrevB 0
    global xuseresfile


        set predefinedLocal ""; #global variable, first defined in main
        set xinput ""
        set xrepeatings 2
        set xtimeout 0
        set foundlocal        0; #predefinedLocal contains used path to local probcli
        set foundinputfilearg 0; #xinput contains used input file for autobench
        set foundrepeatings   0; #xrepeatings contains the number of iterations
        set foundtimeout      0; #xtimeout contains timeout for probcli (0=no timeout)
        foreach {xarg} [split $commandArgs " "] {
          if {$foundinputfilearg} {
             set xinput [file normalize $xarg]
             set foundinputfilearg 0
             puts "used input file: $xinput \n"
          }
          if {$foundrepeatings} {
             if {[string is integer -strict $xarg]} {
                if {$xarg>0} {
                  set xrepeatings $xarg
                } else {
                  puts "invalid argument: -iterations can only be a positive integer"
                  exit -1
                }
             } else {
                  puts "invalid argument: $xarg is not a valid argument for -iterations (must be a positive integer)"
                  exit -1
             }
             set foundrepeatings 0
             puts "iterations: $xrepeatings \n"
          }
          if {$foundtimeout} {
             if {[string is integer -strict $xarg]} {
                if {$xarg>=0} {
                  set xtimeout $xarg
                } else {
                  puts "invalid argument: -timeout can only be a positive integer (or 0=no timeout)"
                  exit -1
                }
             } else {
                  puts "invalid argument: $xarg is not a valid argument for -timeout (must be a positive integer or 0=no timeout)"
                  exit -1
             }
             set foundtimeout 0
             puts "timeout: $xtimeout \n"
          }
          if {$foundlocal} {
               set predefinedLocal [file normalize $xarg]
               set foundlocal 0
               puts "local probcli path: $predefinedLocal \n"
               #if {![file exists $xarg/probcli]} {puts "Warning: Path $xarg does not contain probcli"}
               if {(![file exists $predefinedLocal/probcli])&&(![file exists $predefinedLocal/probcli.sh])&&(![file exists $predefinedLocal/probcli.exe])} {
                  puts "error: probcli not found in given path $predefinedLocal"
                  if {[file exists $predefinedLocal] && ![file isdirectory $predefinedLocal]} {
                    puts "$predefinedLocal is not a valid directory.\nPlease specify only the directory of probcli (not the full path including file name)"
                  }
                  exit -1
               }
          }
          if {[string tolower $xarg] eq "-local" || [string tolower $xarg] eq "--local"} {
            set foundlocal 1
          }
          if {([string tolower $xarg] eq "-input" || [string tolower $xarg] eq "--input")} {
            set foundinputfilearg 1
          }
          if {([string tolower $xarg] eq "-iterations")||([string tolower $xarg] eq "-repeatings")} {
            set foundrepeatings 1
          }
          if {([string tolower $xarg] eq "-timeout")} {
            set foundtimeout 1
          }
          #if {([string tolower $xarg] eq "-showterminal")} {
          #  puts "using xterm...\n"
          #  set showterminal 1
          #}
          #if {([string tolower $xarg] eq "-noterminal")} {
          #  puts "...\n"
          #  set showterminal 0
          #}
          #if {([string tolower $xarg] eq "-currentdate")} {
          #  puts "using current date instead of revision date...\n"
          #  set overwritedate 1
          #}
        }









        if {[string length $xinput] == 0 || ![file exists $xinput]} {
            puts "Input file \"$xinput\" not found.\nAborted."
            exit -1
        }



        #perform tests
            if {[string length $xtimeout] == 0} {set xtimeout 0}
            set xuseresfile "[string range $xinput 0 [string last "." $xinput]]res"
            set resfile $xuseresfile

            # use probcli from local path
            set probrev $predefinedLocal
            set uselocal 1


            set probrev2 0
            #if {[string length $xusedrevB] > 0} {
            #if {[string first " " [string trim $xusedrevB " "]]>-1} {
            #        set probrev2 [string trim "[string range $xusedrevB 0 [string first " " $xusedrevB]]" " ()"]
            #    } else {
            #        set probrev2 [string trim $xusedrevB " ()"]
            #    }
            #}
            destroy .startwnd

            wm geometry . 1x1+[expr {([winfo screenwidth .] / 2)}]+[expr {([winfo screenheight .] / 2)}]
            toplevel .testwindow
            wm geometry .testwindow 800x600+[expr {[winfo screenwidth .]/2 - 400}]+[expr {[winfo screenheight .]/2 - 300}]
            wm transient .testwindow .
            set_runtests_window .testwindow

            if {$uselocal} {
              set probpath $probrev
            #} else {
            #  set probpath $workspace/rev$probrev
            }
            puts "PATH=$probpath"
            puts "REV=$probrev"
            puts "REV2=$probrev2"
            eval global {argv0}
            cd [file dirname $argv0]
            prepare_runtests .testwindow $probrev $probrev2 0 0 $useeveryXrevisionINPUT

}



proc performautorun {} {
    set autoruntesting 1

    eval global {os resfile vers argv0 workspace putsDest putsistxtbox}
    eval global {tcl_platform argv inputfileslist revslist resfileslist probrev probrev2}
    eval global {workspace homedir}

    set thisdir [pwd]
    cd $homedir; cd [file dirname $argv0]
    if {![file exists ./savecodespeed_probench.py]} {
      putsTxt "***error: savecodespeed_probench.py not found. It must be in the same directory as this file.***\n"
      exit -1
    }
    cd $thisdir


    global probpath
    set probpath ""
    global xinput
    global xrepeatings
    global xtimeout
    global xspecialopts
    global xusedrev
    global xusedrevB
    global xuseresfile

    global codespeedfailed
    set codespeedfailed 0

    global autorunerror
    set autorunerror "begin autobench "

    set xinput "autobench.input"
    set xuseresfile "[string range $xinput 0 [string last "." $xinput]]res"

    set resfile $xuseresfile

    set thisdir [pwd]
    cd $homedir; cd [file dirname $argv0]; cd $workspace
    #putsTxt "$homedir\n$argv0\n$thisdir\n[pwd]\n$xinput"
    if {![file exists $xinput]} {
      putsTxt "***error: you have to create an input file \"autobench.input\" first.\n
                  Try to run probench without parameters and create the input file\n
                  (all tests in autobench.input will be performed during autorun)***\n"
      exit -1
    }
    cd $thisdir



    ##################################################################
    putsTxt "step 1: getting and building current revision..."
    ##################################################################
    global xsource
    global xmake
    global xterm
    global xsvn
    global xrev
    set xsource "https://cobra.cs.uni-duesseldorf.de/prob/"
    if [string match $os win] {
        set xmake "Make_probcli_Win.bat"
        set xterm "cmd /k"
    } else {
        set xmake "make"
        set xterm "xterm -e"
    }
    set xsvn "svn"
    set xrev "HEAD"

    if [string match $os win] { console show }

    #get and build current revision
    instant_checkout

    #frame .checkoutwnd.runinfo
    #label .checkoutwnd.runinfo.lrun -text "Getting and building revision.\nThis may take several minutes..."
    #pack .checkoutwnd.runinfo -fill both -expand yes
    #pack .checkoutwnd.runinfo.lrun -expand yes -fill both -padx 10 -pady 10
    update

    #if {$autoruntesting} {
    #wm geometry . 1x1+[expr {[winfo screenwidth .]/2}]+[expr {[winfo screenheight .]/2}]
    #toplevel .checkoutwnd
    #wm geometry .checkoutwnd 1x1+[expr {[winfo screenwidth .]/2 - 1}]+[expr {[winfo screenheight .]/2 - 1}]
    #wm transient .checkoutwnd .

    set currentrev [checkout_runall $xrev $xsource $xterm $xsvn $xmake]
    update

    #}

    eval global {homedir}
    cd $homedir; cd [file dirname $argv0]; # change to script directory
    readINI 0
    if {$currentrev eq ""} {
        putsTxt "*error: Could not get and build current revision.*"
        exit -1
    } else {
        putsTxt "Finished building."
    }




    ##################################################################
    putsTxt "step 2: performing benchmarks..."
    ##################################################################

    set xrepeatings 1
    set xtimeout 0
    set xspecialopts ""
    set xusedrev $currentrev
    set xusedrevB ""


    if {[string first " " [string trim $xusedrev " "]]>-1} {
        set probrev [string trim "[string range $xusedrev 0 [string first " " $xusedrev]]" " ()"]
    } else {
        set probrev [string trim $xusedrev " ()"]
    }
    set probrev2 0
    if {[string length $xusedrevB] > 0} {
        if {[string first " " [string trim $xusedrevB " "]]>-1} {
            set probrev2 [string trim "[string range $xusedrevB 0 [string first " " $xusedrevB]]" " ()"]
        } else {
            set probrev2 [string trim $xusedrevB " ()"]
        }
    }

    wm geometry . 1x1+[expr {([winfo screenwidth .] / 2)}]+[expr {([winfo screenheight .] / 2)}]
    #toplevel .testwindow
    #wm geometry .testwindow 800x600+[expr {[winfo screenwidth .]/2 - 400}]+[expr {[winfo screenheight .]/2 - 300}]
    #wm transient .testwindow .; #all .testwindow lines can be disabled
    #set_runtests_window .testwindow; #can be disabled

    set probpath $workspace/rev$probrev
    prepare_runtests . $probrev $probrev2 1 1 1; #    prepare_runtests .testwindow $probrev $probrev2 1 1 $useeveryXrevisionINPUT(=1)


    set putsDest puts
    set putsistxtbox 0

    if {$codespeedfailed} {
      set xusecsvfile "[string range $resfile 0 [string last "." $resfile]]csv"
      putsTxt "\nwarning: could not save results to codespeed. Is the server running? Does the environment exist (case-sensitive)?\nResults were saved to $xusecsvfile only."
    } else {
      puts "Results saved to codespeed."
    }
    putsTxt "Finished."

    exit 0
}


proc autobench_save_result {resvalue} {

    set resvalue [string trim $resvalue "res_probcli( );"]
    #putsTxt "RESULT: $resvalue"
    set resvalue [split $resvalue ","]
    #putsTxt "RESULT: $resvalue"

    set abURL "ignored"; #ignored yet, set in python-file (http://localhost:8000/ or http://cobra.cs.uni-duesseldorf.de:8000)
    set abProj "ProBench"
    set abExec "probcli"
    eval global {getrevdate overwritedate}
    if {($overwritedate) || ($getrevdate eq "")} {
       set abRevDate "0"
       set abRevTime "0"
    } else {
       set abRevDate "[string range $getrevdate 0 9]"
       set abRevTime "[string range $getrevdate 11 end-1]"
    }
    set abEnvironment "Cobra"; #environment must always exist in codespeed
    eval global {homedir argv0 globEnvironment}
    if {! ($globEnvironment eq "")} {
      set abEnvironment $globEnvironment
    }
    set thisdir [pwd]
    cd $homedir; cd [file dirname $argv0]; # change to script directory
    if {![file exists ./savecodespeed_probench.py]} {
      putsTxt "***error while trying to store data to codespeed: savecodespeed_probench.py not found***"
    } else {
      foreach {abRev abBenchFile abOpts abBenchName abRes abTime abErr} $resvalue {
          #putsTxt "RESULT VALUE: $abRes"
          if {$abBenchName eq ""} {
              set abBenchName [file tail $abBenchFile]
          }
          #codespeed server URL, project, revision(id), benchmark name, result value, interpreter(executable), environment(host)
          #puts "python savecodespeed_probench.py $abURL $abProj $abRev $abBenchName $abRes $abExec $abEnvironment"
          time "catch {exec python savecodespeed_probench.py $abURL $abProj $abRev $abBenchName $abRes $abExec $abEnvironment $abRevDate $abRevTime} xxres xxopts" 1
          if [containsPart $xxopts "-errorinfo"] {
              putsTxt "error while trying to store data to codespeed ($abProj $abRev $abBenchName $abRes $abExec $abEnvironment $abRevDate  $abRevTime):\n[dict get $xxopts -errorinfo]"
              eval global {codespeedfailed}
              set codespeedfailed 1
          } else {
              puts "Results saved to Codespeed ($abProj $abRev $abBenchName $abRes $abExec $abEnvironment $abRevDate  $abRevTime)."
          }
      }
    }
    cd $thisdir
}



proc showresults {testswnd revlst} {
  eval global {os resfile vers argv0 workspace putsDest putsistxtbox};

    set putsDest puts
    set putsistxtbox 0


    frame .menuframe
    frame .felder
    canvas .felder.rtab
    canvas .felder.rw

    pack .menuframe -side top -fill none
    pack [frame .menuframe2] -side top -fill none
    pack [label .menuframe2.info1 -text "Work in progress..."]
    pack .felder -side left -fill both -expand 1

    #pack [button .menuframe.cancel -text " Exit " -command {exit}] -side left -anchor w -fill x
    global isdiag
    set isdiag 1
    pack [button .menuframe.tomainmenu -text " Back to main menu " -command {
         readINI 0
         showstart
         set isdiag 0
         if $showdiaginfobtn {
                destroy .diaginfo
                set showdiaginfobtn 0
         }
         pack forget .menuframe .menuframe2 .felder
         destroy .menuframe .felder .menuframe2
         #wm geometry . 1x1+0+0
        }] -side left -anchor w -fill x
    button .menuframe.switchbtn -text " Tab view " -command {
        if $isdiag {
            .menuframe.switchbtn config -text "Chart view"
            set isdiag 0
            pack forget .felder.rw
            .menuframe.btnInfo config -state disabled
            .menuframe.btnShowSek config -state disabled
            destroy .diaginfo
            pack .felder.rtab -side left -fill both -expand 1
            #drawtable .felder.rtab $allres
        } else {
            .menuframe.switchbtn config -text " Tab view "
            .menuframe.btnInfo config -state normal
            .menuframe.btnShowSek config -state normal
            set isdiag 1
            pack forget .felder.rtab
            pack .felder.rw -side left -fill both -expand 1
            if $showdiaginfobtn {
                eval global {usedTests testClrs}
                showdiaginfo $usedTests $testClrs
            }
        }
    }
    button .menuframe.csvbtn -text " Export CSV " -command {
      exportCSV
    }
    pack .menuframe.switchbtn -side left -anchor w -fill x
    pack .menuframe.csvbtn -side left -anchor w -fill x

    checkbutton .menuframe.btnInfo -text "Show information" -variable showdiaginfobtn -relief flat -command {
            if $showdiaginfobtn {
                eval global {usedTests testClrs}
                showdiaginfo $usedTests $testClrs
            } else {
                destroy .diaginfo
            }
    }
    pack .menuframe.btnInfo -side left -anchor w -fill x

    checkbutton .menuframe.btnShowSek -text "Show time in microsec." -variable showtime -relief flat -command {
            calldrawdiag .felder.rw $allres
    }
    pack .menuframe.btnShowSek -side left -anchor w -fill x



    update idletasks
  ######################################################################################################


    eval global {homedir}
    cd $homedir; cd [file dirname $argv0]; # change to script directory

    set thisdir [pwd]
    cd $workspace
    if {[file exists $resfile]} {
        putsTxt "$resfile found..."
        set f [open $resfile]
        set reslist [split [read $f] ";"]; #read results file
        close $f
    } else {
        errorproc 1 warning "File not found" "Error: Result file (\"$resfile\") not found. Try to run the tests first."
    }

    set foundpos 0
    foreach {zeile} $reslist {
        set pos [string first "res_probcli(" $zeile]
        if $pos>-1 {
            set foundpos 1
            set eintrag [string trim "[string range $zeile [expr {$pos+11}] [string first ")" $zeile]]" "\n ()"]
            lappend probclireslist [split $eintrag ","]
        }
    }
    if !{$foundpos} {
        errorproc 1 warning "File not valid" "Error: Result file (\"$resfile\") does not contain any results. Try to run the tests first."
    }
    cd $thisdir

    update
    if {$revlst eq {}} {
        preparediag $probclireslist 0 {} 0 {} {lightgreen lightblue blue green orange yellow red black} 1 $testswnd
    } else {
        preparediag $probclireslist 1 $revlst 0 {} {lightgreen lightblue blue green orange yellow red black} 1 $testswnd
    }

}


proc preparediag {dreslist drevtag drev dtesttag dtests clrlst dwidth testswnd} {
    # dreslist=list of all results; drevtag=0:showallentries,drevtag<>0:show only revisions contained in drev;
    # dtesttag=0:showalltests,dtesttag<>0:show only tests contained in dtests;
    # clrlist=list of usable colors; dwidth=width of line
      global usedRevs; #global?
    set usedRevs {}
    global usedTests
    set usedTests {}
    global testClrs
    set testClrs {}
    global allres
    set allres {}
    set maxval 1
    set nextclrnr 0
    foreach {xeintrag} $dreslist {
        foreach {xrev xfile xparams xname xruntime xtimestamp xiserror} $xeintrag {
            if [string match $xname ""] { set xname $xfile }
            if {(($drevtag==0) || [contains $drev $xrev]) && (($dtesttag==0) || [containsNCS $dtests $xname])} {
                if {$xruntime > $maxval} { set maxval $xruntime }
                if ![contains $usedRevs $xrev] { lappend usedRevs $xrev }
                if ![contains $usedTests $xname] {
                    lappend usedTests $xname
                    lappend testClrs [lindex $clrlst $nextclrnr]
                    incr nextclrnr 1
                    if [expr {$nextclrnr >= [llength $clrlst]}] { set nextclrnr 0 }
                }
                lappend allres [set dummy {}; lappend dummy $xrev $xruntime $xname $xiserror [lindex $testClrs [lsearch -exact $usedTests $xname]] $dwidth]
            }
        }
    }


    calldrawdiag .felder.rw $allres
    bind all <Expose> {
    # window size changes
      if $isdiag {
        calldrawdiag .felder.rw $allres
      }
    }

    pack .felder.rw -side left -fill both -expand 1; #resultwindow
    update
    update idletasks


    if [string length $testswnd]>0 { destroy $testswnd }
    pack forget .menuframe2.info1
    update
    update idletasks
    drawtable .felder.rtab $allres $usedTests $usedRevs

}

proc showdiaginfo {tsts clrs} {
    eval global {diagbgclr os}
    toplevel .diaginfo
    if {[tk windowingsystem] eq "win32"} {
        wm attributes .diaginfo -toolwindow 1
    }
    wm transient .diaginfo .
    wm geometry .diaginfo 155x80+[expr {[winfo screenwidth .]-160}]+20
    set clrcnt 0
    foreach {clrEntry} $clrs {
        #msgbox "[lindex $tsts $clrcnt] $clrEntry"
        frame .diaginfo.frame${clrcnt}
        label .diaginfo.frame${clrcnt}.clr${clrcnt} -text "   " \
                -bg $clrEntry -fg $diagbgclr -font {Helvetica 8} -justify left
        label .diaginfo.frame${clrcnt}.label${clrcnt} -text [lindex $tsts $clrcnt] -font {Helvetica 8}
        pack .diaginfo.frame${clrcnt}.clr${clrcnt} .diaginfo.frame${clrcnt}.label${clrcnt} -side left
        pack .diaginfo.frame${clrcnt} -fill both
        incr clrcnt 1
    }
    set thisheight [expr {$clrcnt*20}]
    if $thisheight>[winfo screenheight .] {
        set thisheight [winfo screenheight .]
    }
    wm geometry .diaginfo 155x${thisheight}+[expr {[winfo screenwidth .]-160}]+20
}


proc calldrawdiag {canv xres} {
      eval global {diagbgclr}
      $canv create rect 0 0 [winfo width $canv] [winfo height $canv] -fill $diagbgclr
      drawdiag  $canv 10 10 [expr {[winfo width $canv]-10}] [expr {[winfo height $canv]-10}] $xres
}



proc contains {list el} {expr {[lsearch -exact $list $el] >= 0}}

proc containsNCS {list el} {expr {[lsearch -exact -nocase $list $el] >= 0}}

proc containsPart {list el} {expr {[string first $el $list] > -1}}

proc containsPartNCS {list el} {expr {[string first [string tolower $el] [string tolower $list]] > -1}}


proc exportCSV {} {

    #package require tablelist

   # eval global {diagbgclr}

    set csvtxt "\""; #csv-text-sign
    set csvsep ";"; #csv-separator


    eval global {allres usedTests usedRevs}
    set xres $allres
    set xtests $usedTests
    set xrevs $usedRevs


    #create as many columns as revisions shown (sorted)
    set xrevs [lsort -dictionary $xrevs]
    set collist {}
    set colnr 1; #column 0 not used (description column)

    set csvtopline "${csvtxt}test name / revision${csvtxt}"
    set csvlines {}
    set addlist {}
    set csvdummyline {}
    foreach revcol $xrevs {
        lappend addlist 0 $revcol center
        lappend collist $revcol $colnr
        incr colnr
        set csvtopline "${csvtopline}${csvsep}${csvtxt}${revcol}${csvtxt}"
        set csvdummyline "${csvdummyline} 0"
    }

    #create as many rows as tests (sorted)
    set xtests [lsort -dictionary $xtests]
    set rowlist {}
    set rownr 0
    set addlist {}
    foreach row $xtests {
        #${canv}.restab insert end $row
        lappend addlist $row
        lappend rowlist $row $rownr
        lappend csvlines "$row$csvdummyline"
        incr rownr
    }



    set lasttxt {}
    set xres [lsort -dictionary -index 2 $xres]; #sort by text
    set xres [lsort -dictionary -index 0 $xres]; #sort by revision number

    foreach point $xres {

        set pointrev   [lindex $point 0]
        set pointval   [lindex $point 1]
        set pointtxt   [lindex $point 2]
        set pointiserr [lindex $point 3]
        set pointclr   [lindex $point 4]
        set pointwdth  [lindex $point 5]

        set nrow [lindex $rowlist [expr {[lsearch -exact $rowlist $pointtxt]+1}]]
        set ncol [lindex $collist [expr {[lsearch -exact $collist $pointrev]+1}]]
        if ![string match "$pointtxt $pointrev" $lasttxt] {
            # 0=no error, 1=invariant violation, 2=unknown error, 3=deadlock,
            # 4=known error other than invariant violation or deadlock (includes description),
            # 5=found_goal
            lset csvlines $nrow $ncol $pointval
        } else {
            lset csvlines $nrow $ncol $pointval
        }
        set lasttxt "$pointtxt $pointrev"

    }

    eval global {resfile workspace}
    set xusecsvfile "[string range $resfile 0 [string last "." $resfile]]csv"
    set thisdir [pwd]
    cd $workspace
    if { [catch {
        set fwritecsv [open $xusecsvfile w]
        puts $fwritecsv $csvtopline
        foreach linecsv $csvlines {
            set thiscsvline ""
            foreach csvele $linecsv {
                if {$thiscsvline eq ""} {
                    set thiscsvline "${csvtxt}${csvele}${csvtxt}"
                } else {
                    set thiscsvline "${thiscsvline}${csvsep}${csvele}"
                }
            }
            puts $fwritecsv $thiscsvline
        }
        close $fwritecsv
        tk_messageBox -title "CSV written to workspace" -message "CSV file written to [pwd]/$xusecsvfile. CSV file can be opened with spreadsheet applications." -type ok -parent .
    } ] } {
        tk_messageBox -icon warning -title "Permission denied" -message "Could not write CSV file to [pwd]/$xusecsvfile. Please check permissions." -type ok -parent .
    }
    cd $thisdir

}


proc drawtable {canv xres xtests xrevs} {

    package require tablelist

    eval global {diagbgclr}

    set csvtxt "\""; #csv-text-sign
    set csvsep ";"; #csv-separator

    global xscrollname
    global yscrollname
    global resulttabname
    set xscrollname ${canv}.scrollx
    set yscrollname ${canv}.scrolly
    set resulttabname ${canv}.restab

    tablelist::tablelist ${canv}.restab -columns {0 "Test Name / revision >" center} -stretch all -background white \
                -xscrollcommand "$xscrollname set" -yscrollcommand "$yscrollname set" \
                -titlecolumns 1 -showseparators 1 -stripebackground lightgrey -foreground black

    if {[tk windowingsystem] ne "aqua"} {
        # x11 (X11-based), win32 (Windows), aqua (Mac OS X Aqua)
        ttk::scrollbar ${canv}.scrolly -command {
            $resulttabname yview
        } -orient vertical

        ttk::scrollbar ${canv}.scrollx -command {
            $resulttabname xview
        } -orient horizontal
    } else {
        scrollbar ${canv}.scrolly -command {
            $resulttabname yview
        } -orient vertical

        scrollbar ${canv}.scrollx -command {
            $resulttabname xview
        } -orient horizontal
    }



    pack ${canv}.scrolly -side right -fill y
    pack ${canv}.scrollx -side bottom -fill x

    pack ${canv}.restab -fill both -expand 1 -side top



    #create as many columns as revisions shown (sorted)
    set xrevs [lsort -dictionary $xrevs]
    set collist {}
    set colnr 1; #column 0 not used (description column)

    set csvtopline "${csvtxt}test name / revision${csvtxt}"
    set csvlines {}
    set addlist {}
    set csvdummyline {}
    foreach revcol $xrevs {
        lappend addlist 0 $revcol center
        lappend collist $revcol $colnr
        incr colnr
        set csvtopline "${csvtopline}${csvsep}${csvtxt}${revcol}${csvtxt}"
        set csvdummyline "${csvdummyline} 0"
    }
    ${canv}.restab insertcolumnlist end $addlist

    #create as many rows as tests (sorted)
    set xtests [lsort -dictionary $xtests]
    set rowlist {}
    set rownr 0
    set addlist {}
    foreach row $xtests {
        #${canv}.restab insert end $row
        lappend addlist $row
        lappend rowlist $row $rownr
        lappend csvlines "$row$csvdummyline"
        incr rownr
    }
    ${canv}.restab insertlist end $addlist



    set lasttxt {}
    set xres [lsort -dictionary -index 2 $xres]; #sort by text
    set xres [lsort -dictionary -index 0 $xres]; #sort by revision number

    foreach point $xres {

        set pointrev   [lindex $point 0]
        set pointval   [lindex $point 1]
        set pointtxt   [lindex $point 2]
        set pointiserr [lindex $point 3]
        set pointclr   [lindex $point 4]
        set pointwdth  [lindex $point 5]

        set nrow [lindex $rowlist [expr {[lsearch -exact $rowlist $pointtxt]+1}]]
        set ncol [lindex $collist [expr {[lsearch -exact $collist $pointrev]+1}]]
        if ![string match "$pointtxt $pointrev" $lasttxt] {
            # 0=no error, 1=invariant violation, 2=unknown error, 3=deadlock,
            # 4=known error other than invariant violation or deadlock (includes description),
            # 5=found_goal
            if [string match $pointiserr "1"] {
                ${canv}.restab cellconfigure $nrow,$ncol -foreground red -text $pointval
            } elseif {[string match $pointiserr "2"]||[string match $pointiserr "3"]||[string match $pointiserr "4"]||[string match $pointiserr "5"]} {
                ${canv}.restab cellconfigure $nrow,$ncol -foreground red -text "error"
            } else {
                ${canv}.restab cellconfigure $nrow,$ncol -text $pointval
            }
            lset csvlines $nrow $ncol $pointval
        } else {
            ${canv}.restab cellconfigure $nrow,$ncol -text "[${canv}.restab getcells $nrow,$ncol]\n$pointval"
            lset csvlines $nrow $ncol $pointval
        }
        set lasttxt "$pointtxt $pointrev"

    }

    eval global {resfile workspace}
    set xusecsvfile "[string range $resfile 0 [string last "." $resfile]]csv"
    set thisdir [pwd]
    cd $workspace
    if { [catch {
        set fwritecsv [open $xusecsvfile w]
        puts $fwritecsv $csvtopline
        foreach linecsv $csvlines {
            set thiscsvline ""
            foreach csvele $linecsv {
                if {$thiscsvline eq ""} {
                    set thiscsvline "${csvtxt}${csvele}${csvtxt}"
                } else {
                    set thiscsvline "${thiscsvline}${csvsep}${csvele}"
                }
            }
            puts $fwritecsv $thiscsvline
        }
        close $fwritecsv
    } ] } {
        tk_messageBox -icon warning -title "Permission denied" -message "Could not write CSV file to [pwd]/$xusecsvfile. Please check permissions." -type ok -parent .
    }
    cd $thisdir

}




proc drawdiag {canv x0 y0 x1 y1 dat} {

    eval global {diagbgclr showtime}

    set xfix 60; #free border space (left)
    set yfix 30; #free border space (bottom)
    set pointsize 3
    set yfixtop [expr {$pointsize+12}]; #free border space (top)
    set xfix2 [expr {$pointsize+10}]; #free border space (right)

    if {$x0>$x1} {swap x0 x1}
    if {$y0>$y1} {swap y0 y1}
    set vals 0
    set entries 0
    set tmpRevs {}
    foreach pointc $dat {
        lappend vals [lindex $pointc 1]
        incr entries 1
        if ![contains $tmpRevs [lindex $pointc 0]] {
            lappend tmpRevs [lindex $pointc 0]
        }
    }
    set revEntries [llength $tmpRevs]
    set maxval [max $vals]
    if [expr {$maxval <= 0}] {set maxval 1}
    set height [expr {$y1-$y0-$yfix}]
    set width  [expr {$x1-$x0-$xfix}]
    if {$height<=0} {return}
    if {$width<=0} {return}
    if {$entries<2} {set entries 2}
    if {$revEntries<2} {set revEntries 2}
    set scalex  [expr {int(${width}-$xfix2)/int(${revEntries}-1)}]; #an entry every (scalex) px, scalex rounded down (int-div)
    set scaley  [expr {double($maxval)/double(${height}-$yfixtop)}]; #valuepos(y)=(y1-yfix)-(value/scaley) = ybase-(value/scaley)
    if [expr {$scaley <= 0}] {set scaley 1}


    set ybase [expr {$y1-$yfix}]
    #draw coords...
    $canv create rect $x0 $y0 $x1 $y1 -outline navy -fill $diagbgclr
    $canv create line [expr {$x0+$xfix}] [expr {$y1-$yfix}] $x1 [expr {$y1-$yfix}] -fill black
    $canv create line [expr {$x0+$xfix}] [expr {$y1-$yfix}] [expr {$x0+$xfix}] [expr {$y0}] -fill black

    drawYlabels $canv [expr {$x0+$xfix-2}] [expr {$ybase-($maxval/$scaley)}] $ybase $scaley $maxval




    set thisx [expr {$x0+$xfix+1}]
    set oldx -1
    set oldy -1

    set dat [lsort -dictionary -index 0 $dat]; #sort by revision number

    set lastrev -1
    set lastpos {}
    incr thisx -$scalex; #thisx will be increased before drawing
    foreach point $dat {
        set pointrev   [lindex $point 0]
        set pointval   [lindex $point 1]
        set pointtxt   [lindex $point 2]
        set pointiserr [lindex $point 3]
        set pointclr   [lindex $point 4]
        set pointwdth  [lindex $point 5]

        if ![string match $pointrev $lastrev] {
            incr thisx $scalex
            set lastrev $pointrev
        }

        set thisy [expr {($ybase-int(double($pointval)/double($scaley)))}]
        $canv create oval [expr {$thisx-$pointsize}] [expr {$thisy-$pointsize}] [expr {$thisx+$pointsize}] [expr {$thisy+$pointsize}] -fill $pointclr -width 1
        if $oldx>-1 {
            if [contains $lastpos $pointtxt] {
                set tmpx [lindex $lastpos [expr {[lsearch -exact $lastpos $pointtxt]+1}]]
                set tmpy [lindex $lastpos [expr {[lsearch -exact $lastpos $pointtxt]+2}]]
                $canv create line $tmpx $tmpy $thisx $thisy -fill $pointclr -width $pointwdth
            }
        }
        ####################
        if (($showtime)) {
            if [string match $pointiserr "1"] {
                $canv create text $thisx [expr {$thisy-$pointsize+1}] -anchor s -font {Helvetica 8} -fill red -text $pointval
            } elseif {[string match $pointiserr "2"]||[string match $pointiserr "3"]||[string match $pointiserr "4"]||[string match $pointiserr "5"]} {
                $canv create text $thisx [expr {$thisy-$pointsize+1}] -anchor s -font {Helvetica 8} -fill red -text "err"
            } else {
                $canv create text $thisx [expr {$thisy-$pointsize+1}] -anchor s -font {Helvetica 8} -text $pointval
            }
        }
        $canv create text $thisx $ybase -anchor n -font {Helvetica 8} -text $pointrev
        set oldx $thisx
        set oldy $thisy
        if [contains $lastpos $pointtxt] {
            set lastpos [lreplace $lastpos [lsearch -exact $lastpos $pointtxt] [expr {[lsearch -exact $lastpos $pointtxt]+2}] $pointtxt $thisx $thisy]
        } else {
            lappend lastpos $pointtxt $thisx $thisy
        }
    }

}

proc drawYlabels {canv x0 ybase y1 scaley maxval} {
    set pixelstep 50; #print a label every "pixelstep" pixels
    set entries [expr {int(($y1-$ybase)/$pixelstep)}]
    set valstep [expr {int(($pixelstep)*$scaley)}]
    set ypos $y1
    set ylabel 0
    while {$ypos>$ybase} {
        $canv create text $x0 $ypos -anchor e -font {Helvetica 8} -text $ylabel
        set ypos [expr {$ypos-$pixelstep}]
        set ylabel [expr {$ylabel+$valstep}]
    }
}



proc swap {_a _b} {
    upvar 1 $_a a $_b b
    foreach {a b} [list $b $a] break
}


proc max {l} {
    set res [lindex $l 0]
    foreach ele [lrange $l 1 end] {
        if {$ele > $res} {
            set res $ele
        }
    }
    return $res
}

proc min {l} {
    set res [lindex $l 0]
    foreach ele [lrange $l 1 end] {
        if {$ele < $res} {
            set res $ele
        }
    }
    return $l
}


proc setvars1 {} {
  #set path variables and remember OS

  eval global {tcl_platform argv0 argv argc}
  global os
  global probrev
  global probrev2
  global useeveryXrevisionINPUT
  global resfile
  global vers
  global timestamp

  set vers 1; #internal version number

  set probrev 0; #will be overwritten by showstart-procedure
  set probrev2 0; #will be overwritten by showstart-procedure

  set useeveryXrevisionINPUT 1

  set arglist [split $argv " "]
  set resfile results.dat; #will be overwritten by showstart-procedure

  set timestamp [clock seconds]; #timestamp fuer diesen Durchlauf

  if [containsPartNCS $tcl_platform(os) "windows"] {
   set os win
  } elseif [containsPartNCS $tcl_platform(os) "linux"] {
   set os linux
  } elseif [containsPartNCS $tcl_platform(os) "mac"] {
   set os mac
  } elseif [containsPartNCS $tcl_platform(os) "unix"] {
   set os unix
  } else {
   set os other
  }

}


proc setvars2 {} {
  #variables for test runs

  eval global {tcl_platform argv0 argv argc os probrev resfile vers xinput probpath}
  global params
  global mchpath
  global testlist

  set params {-mc 1000}; # default parameter
  set mchpath ${probpath}/test; #default path for mch files

  set arglist [split $argv " "]

  set returnval [setvars3]

  set testlist $xinput


  if [lsearch [string tolower $arglist] -mchdir]>-1 {
     set mchpath [lindex $arglist [expr {[lsearch [string tolower $arglist] -mchdir]+1}]]
     if ![file isdirectory $mchpath] {
          errorproc 1 warning "path not valid" "Error: \"$mchpath\" is not a valid path."
     }
  } else {
     #putsTxt "uses default mch path"
  }

  return $returnval
}


proc setvars3 {} {
  eval global {probrev probpath}

  if [file isdirectory $probpath] {
     if {[file exists $probpath/probcli]||[file exists $probpath/probcli.exe]} {
        #putsTxt "probcli found in $probpath..."
        return 1
     } else {
        errorproc -1 error "probcli not found" "Error: probcli not found in path \"$probpath\". Maybe you have to check out the revision $probrev again (use revbench_co.tcl)"
        return 0
     }
  } else {
      errorproc -1 error "probcli not found" "Error: \"$probpath\" is not a valid directory. Maybe you have to check out the revision $probrev again (use revbench_co.tcl)"
      return 0
  }
}




proc readmchfiledat {} {
   eval global {testlist workspace}
   global mchfilelist
   # ********read list of mch-files from tstfiles.dat***********************
   set thisdir [pwd]
   cd $workspace
   if {[file exists $testlist]} {
        #putsTxt "$testlist found..."
        set f [open $testlist]
        set mchfilelist [split [read $f] "\n"]; #read list of mch-files from testlist (tstfiles.dat)
        close $f
   } else {
        errorproc 1 warning "Test file list not found" "Error: Test file list \"$testlist\" not found."
   }
   cd $thisdir

}








proc errorproc {shouldexit msgicon msgtitle msg} {
    if {$msg ne ""} {
        putsTxt $msg
        eval global {activetests silentrun}
        if {!$silentrun} {
          if $activetests {
            set activewnd .testwindow
          } else {
            set activewnd .
          }
          if {($msgicon eq "info") || ($msgicon eq "question") || ($msgicon eq "warning") || ($msgicon eq "error")} {
                tk_messageBox -icon $msgicon -title $msgtitle -message $msg -type ok -parent $activewnd
          } else {
                tk_messageBox -icon error -title $msgtitle -message $msg -type ok -parent $activewnd
          }
        }
    }

    set returnval 0

    if $shouldexit>-1 {
        eval global {aftererror}
        set aftererror 1
    }

    if $shouldexit>0 {
        putsTxt "Aborted."
        exit -1
    }

    return $returnval
}


proc putsTxt {txt} {
    eval global {putsDest putsistxtbox}
    if $putsistxtbox {
        $putsDest insert end "${txt}\n"
        update
        update idletasks
    } else {
        $putsDest $txt
    }
}


proc prepare_showres {testwnd revlst} {
  #show results (if revlst <> {} show only revisions in revlst)
  global diagbgclr
  set diagbgclr white
  global showtime
  set showtime 1
  wm geometry . [expr {([winfo screenwidth .] - 40)}]x[expr {([winfo screenheight .] - 80)}]+20+10
  showresults $testwnd $revlst
}



proc canceltests {showmsg} {
    #go back to main menu
    set activetests 0
    wm forget .testwindow
    destroy .testwindow
    showstart
    if {$showmsg} {
        eval global {silentrun}
        if {!$silentrun} {
          tk_messageBox -icon error -title "Workspace corrupt" -message "No tests were performed." -type ok -parent .
        } else {
          putsTxt "error: workspace corrupt: no tests were performed."
        }
    }
    return
}


proc prepare_runtests {testwnd fromrev torev silent autobench useeveryXrevision} {
    # perform tests (silent=used for autorun only, otherwise false)
    eval global {activetests os probpath probrev revslist workspace resfile silentrun silenterrs uselocal}
    global aftererror
    set aftererror 0
    global filenotfoundlist
    set filenotfoundlist {}
    global succmachines
    set succmachines ""
    global errmachines1
    set errmachines1 ""
    global errmachines1detail
    set errmachines1detail ""
    global errmachines2
    set errmachines2 ""
    global errmachines2detail
    set errmachines2detail ""
    global errmachines3
    set errmachines3 ""
    global errmachines3detail
    set errmachines3detail ""
    global errmachines4
    set errmachines4 ""
    global errmachines4detail
    set errmachines4detail ""
    global errmachines5
    set errmachines5 ""
    global errmachines5detail
    set errmachines5detail ""


    set activetests 1
    if {![setvars2]} {
                canceltests 1
                return
    }
    #tests
    if !{$aftererror} {
        readmchfiledat
    }

    #if {($torev ne 0)&&(!$uselocal)} {
    #    if {$fromrev > $torev} {
    #        set dummy $fromrev
    #        set fromrev $torev
    #        set torev $dummy
    #    }
    #}

    set jumpoverrevlistCounter $useeveryXrevision
    if {[expr {$torev ne 0}] && [expr {$fromrev ne $torev}]} {
        set foundrevstart 0
        set justrevstarted 0
        foreach {thisentry} $revslist {
            if {[string first " " [string trim $thisentry " "]]>-1} {
                set thisrev1 [string trim "[string range $thisentry 0 [string first " " $thisentry]]" " ()"]
            } else {
                set thisrev1 [string trim $thisentry " ()"]
            }
            #puts "found: $thisrev1"
            if {$foundrevstart==0 && ([expr {$thisrev1 == $fromrev}] || [expr {$thisrev1 == $torev}])} {
              set foundrevstart 1
              set justrevstarted 1
              #puts "FOUNDREVSTART ($thisrev1)"
            }
            if {$foundrevstart} {
                incr jumpoverrevlistCounter
                #puts "counter: $jumpoverrevlistCounter ($thisrev1)"
                if {$jumpoverrevlistCounter>=$useeveryXrevision || [expr {$thisrev1 == $torev}] || [expr {$thisrev1 == $fromrev}]} {
                   #if only every X (second/third/...) revision within the range should be used, jump over the other except if it is the last revision within the range
                   lappend probrevlist $thisrev1
                   set jumpoverrevlistCounter 0
                   #puts "USE: $thisrev1"
                }
            }
            if {$foundrevstart && !$justrevstarted && ([expr {$thisrev1 == $fromrev}] || [expr {$thisrev1 == $torev}])} {
              set foundrevstart 0
              #puts "FOUNDREVEND ($thisrev1)"
            }
            set justrevstarted 0
        }
        #lappend probrevlist $torev
    } else {
        set probrevlist $fromrev
    }

      #msgbox $probrevlist

    # first, check if workspace is not corrupt
    set thisdir [pwd]
    foreach {thisrev} $probrevlist {
        set probrev $thisrev
        if {$uselocal} {set probpath $fromrev} else {set probpath $workspace/rev$probrev}
        cd $thisdir
        if {![setvars3]} {
                #if setvars3 returns false, probcli could not be found in the specific workspace directory
                cd $thisdir
                canceltests 1
                return
        }
    }

    # now do a single run without measuring
    set probrev $fromrev
    if {$uselocal} {set probpath $probrev} else {set probpath $workspace/rev$probrev}
    cd $thisdir
    if !{$aftererror} {
        if {[setvars3]} {
                #setvars3 returns true only if probcli executable was found
                runtests 0
        } else {
                cd $thisdir
                canceltests 1
                return
        }
    }
    if {[llength $filenotfoundlist]>0} {
      if {!$silentrun} {
        set errmsg "The following files could not be found: \n\n$filenotfoundlist\n\n\nDo you want to continue?"
        set clicked [tk_messageBox -icon warning -title "Could not find all machine files" -message $errmsg -type yesno -parent .]
            switch -- $clicked {
                yes {  }
                no  {
                        cd $thisdir
                        canceltests 0
                        return
                    }
            }
      } else {
          lappend silenterrs "The following files could not be found: $filenotfoundlist"
          putsTxt "warning: The following benchmark files could not be found: $filenotfoundlist"
      }
    }
    # now perform tests
    foreach {thisrev} $probrevlist {
        set probrev $thisrev
        if {$uselocal} {set probpath $probrev} else {set probpath $workspace/rev$probrev}
        cd $thisdir
        if !{$aftererror} {
            if {[setvars3]} {
                #setvars3 returns true only if probcli executable was found
                if {$autobench} {
                    runtests 2
                } else {
                    runtests 1
                }
            }
        } else {
            break
        }
    }
    set errmsg ""
    set errtype info
    if {[llength $filenotfoundlist]>0} {
        set errmsg "The following files could not be found (other tests were performed as well): \n$filenotfoundlist\n\n\n"
        set errtype warning
    }

    set errmsg "${errmsg}[llength $succmachines] successfull tests.\n\n"

    set errmachines1 [lsort -dictionary $errmachines1]
    set errmachines2 [lsort -dictionary $errmachines2]
    set errmachines3 [lsort -dictionary $errmachines3]
    set errmachines4 [lsort -dictionary $errmachines4]
    set succmachines [lsort -dictionary $succmachines]
    if {[llength $errmachines1]>0} {
        if {[llength $errmachines1]==1} {
            set errmsg "${errmsg}[llength $errmachines1] invariant violation occurred: \n$errmachines1\n\n"
        } else {
            set errmsg "${errmsg}[llength $errmachines1] invariant violations occurred: \n$errmachines1\n\n"
        }
        set errtype warning
    } else {
        set errmsg "${errmsg}No invariant violations occurred.\n\n"
    }
    if {[llength $errmachines3]>0} {
        if {[llength $errmachines3]==1} {
            set errmsg "${errmsg}[llength $errmachines3] deadlock error occurred: \n$errmachines3\n\n"
        } else {
            set errmsg "${errmsg}[llength $errmachines3] deadlock errors occurred: \n$errmachines3\n\n"
        }
        set errtype warning
    } else {
        set errmsg "${errmsg}No deadlocks occurred.\n\n"
    }
    if {[llength $errmachines5]>0} {
        if {[llength $errmachines5]==1} {
            set errmsg "${errmsg}[llength $errmachines5] machine with found_goal: \n$errmachines3\n\n\n"
        } else {
            set errmsg "${errmsg}[llength $errmachines5] machines with found_goal: \n$errmachines3\n\n\n"
        }
        #set errtype warning
    } else {
        #set errmsg "${errmsg}No found_goal.\n\n"
    }
    if {[llength $errmachines2]>0 || [llength $errmachines4]>0} {
        if {[expr {[llength $errmachines2]+[llength $errmachines4]}]==1} {
            set errmsg "${errmsg}[expr {[llength $errmachines2]+[llength $errmachines4]}] other error occurred: \n$errmachines4\n$errmachines2\n\n\n"
        } else {
            set errmsg "${errmsg}[expr {[llength $errmachines2]+[llength $errmachines4]}] other errors occurred: \n$errmachines4\n$errmachines2\n\n\n"
        }
        set errtype warning
    } else {
        set errmsg "${errmsg}No other errors occurred.\n\n"
    }
    set xuselogfile "[string range $resfile 0 [string last "." $resfile]]log"
    set errmsg "${errmsg}You can review detailed informations in logfile \"$xuselogfile\" (saved in workspace directory \"$workspace\")."
    #message will be displayed at the end of this procedure


    #write logfile
    set xuselogfile "[string range $resfile 0 [string last "." $resfile]]log"
    #msgbox $xuselogfile
    set thisdir [pwd]
    cd $workspace
    set fwrite [open $xuselogfile w]
    puts $fwrite "Log File for input file [string range $resfile 0 [string last "." $resfile]]input, created at [clock format [clock seconds] -format {%Y-%m-%d %H:%M:%S}]."

    puts $fwrite "\nused revisions:\n"
    foreach {entry} $probrevlist {
        puts $fwrite $entry
    }
    puts $fwrite "\n"
    if {[llength $filenotfoundlist]>0} {
        puts $fwrite "The following files could not be found:"
        foreach {entry} $filenotfoundlist {
            puts $fwrite $entry
        }
        puts $fwrite ""
    }
    if {[llength $errmachines1]>0} {
        puts $fwrite "Short summary of invariant violations ([llength $errmachines1]) (see below for details):"
        foreach {entry} $errmachines1 {
            puts $fwrite $entry
        }
    } else {
        puts $fwrite "No invariant violations occurred."
    }
    puts $fwrite ""

    if {[llength $errmachines3]>0} {
        puts $fwrite "Short summary of deadlock errors ([llength $errmachines3]) (see below for details):"
        foreach {entry} $errmachines3 {
            puts $fwrite $entry
        }
    } else {
        puts $fwrite "No deadlocks occurred."
    }
    puts $fwrite ""

    if {[llength $errmachines5]>0} {
        puts $fwrite "Short summary of found_goal ([llength $errmachines5]) (see below for details):"
        foreach {entry} $errmachines5 {
            puts $fwrite $entry
        }
        puts $fwrite ""
    } else {
        #puts $fwrite "No found_goal."
        #puts $fwrite "\n"
    }

    if {[llength $errmachines2detail]>0 || [llength $errmachines4]>0} {
        if {[llength $errmachines4]>0} {
            puts $fwrite "Short summary of ([llength $errmachines4]) other errors:"
            foreach {entry} $errmachines4 {
                puts $fwrite $entry
            }
        }
        if {[llength $errmachines2detail]>0} {
            puts $fwrite "There are [llength $errmachines2detail] more errors, see detailed information below."
            #foreach {entry} $errmachines2 {
            #    puts $fwrite $entry
            #}
        }
    } else {
        puts $fwrite "No other errors occurred."
    }

    puts $fwrite "*****************************\n"
    puts $fwrite "Detailed error informations:\n"
    if {[llength $errmachines2]>0} {
        foreach {entry} $errmachines2detail {
            puts $fwrite $entry
        }
        puts $fwrite "###\n"
    }
    if {[llength $errmachines1]>0} {
        foreach {entry} $errmachines1detail {
            puts $fwrite $entry
        }
        puts $fwrite "###\n"
    }
    if {[llength $errmachines3]>0} {
        foreach {entry} $errmachines3detail {
            puts $fwrite $entry
        }
        puts $fwrite "###\n"
    }
    if {[llength $errmachines4]>0} {
        foreach {entry} $errmachines4detail {
            puts $fwrite $entry
        }
        puts $fwrite "###\n"
    }
    if {[llength $errmachines5]>0} {
        foreach {entry} $errmachines5detail {
            puts $fwrite $entry
        }
    }

    puts $fwrite "\n"
    puts $fwrite "*****************************\n"
    puts $fwrite "Runtime results of successfully tested machines:"
    foreach {entry} $succmachines {
        puts $fwrite $entry
    }
    puts $fwrite "\n"

    close $fwrite
    cd $thisdir
    #end of writing logfile


    set activetests 0
    if !{$aftererror} {
        putsTxt "Benchmarks finished."
        #if ![string match $os win] { exit }
        if {!$silent} {
            prepare_showres $testwnd $probrevlist
        }
    }
    if {[string length $errmsg]>0} {
        if {$silent} {
            set putsDest puts
            set putsistxtbox 0
            puts "Benchmark summary:\n$errmsg"
        } else {
            errorproc -1 $errtype "Finished." $errmsg
        }
    }
}



proc runtests {performtests} {
  eval global {mchpath probpath params mchfilelist resfile vers timestamp workspace \
               xrepeatings filenotfoundlist xtimeout xspecialopts silentrun \
               savetocodespeed};

    putsTxt "Please wait..."

    set thisdir [pwd]
    cd $workspace
    if {$performtests} {
        set fwrite [open $resfile a]
        puts $fwrite "Begin_tests(${vers},${timestamp});"
    }
    if [file isdirectory $mchpath] {
        cd ${mchpath}
        #putsTxt "Switch to mch-path = [pwd]"
    }
    cd $thisdir

    foreach {x} $mchfilelist {
        set mchname ""
        if ([string first "cd " $x]==0) {
            set todir [string range $x 3 end]
            if [file isdirectory $todir] {
                cd $todir
            } else {
                if {$performtests} {
                    if {[lsearch $filenotfoundlist "directory:$todir"] < 0} {
                        putsTxt "ERROR: Directory $todir does not exists. Could not change directory."
                        lappend filenotfoundlist "directory:$todir"
                    }
                } else {
                    lappend filenotfoundlist "directory:$todir"
                }
            }
        } elseif !([string first "#" $x]==0) {
            if [containsPart $x "@"] {
                # there is at least one @ seperator in string
                set mchfile "[string range $x 0 [expr {[string first "@" $x]-1}]]"
                set y "[string range $x [expr {[string first "@" $x]+1}] [string length $x]]"
                if [containsPart $y "@"] {
                    # there are at least two @ seperator in string
                    set mchparams "[string range $y 0 [expr {[string first "@" $y]-1}]]"
                    set mchname "[string range $y [expr {[string first "@" $y]+1}] [string length $y]]"
                } else {
                    set mchparams "$y"
                }
                if [string match [string trim $mchparams " "] ""] {
                    set mchparams "$params"; #use global parameters
                }
            } else {
                # no @ seperator in string
                set mchfile "$x"
                set mchparams $params; #use global parameters
            }
            if [file exists $mchfile] {
                set xspecialopts [string trim $xspecialopts " "]
                if {$xspecialopts ne ""} {
                    set usedparams $xspecialopts
                } else {
                    set usedparams $mchparams
                }
                if {$xtimeout > 0} {
                    set usedparams "$usedparams --timeout $xtimeout"
                }
                if {$performtests} {
                    putsTxt "running probcli with ${mchfile} (${usedparams})..."
                    set mchname [string trim $mchname " "]
                    set benchresvalue [runprob $mchfile $usedparams $mchname $xrepeatings $performtests]
                    puts $fwrite $benchresvalue
                    if {$performtests==2} {
                        #autobench
                        if {$savetocodespeed} {
                          autobench_save_result $benchresvalue
                        } else {
                           puts "-nocodespeed\n"
                        }
                    }
                } else {
                    #putsTxt "dummy test run with ${mchfile} (${usedparams})..."
                    putsTxt "preparing ${mchfile} (this may take a while)..."
                    set mchname [string trim $mchname " "]
                    runprob $mchfile $usedparams $mchname 1 $performtests
                }
            } else {
                if ![string match [string trim $mchfile " "] ""] {
                    if {$performtests} {
                        if {[lsearch $filenotfoundlist "${mchfile}"] < 0} {
                            #add only if not yet in list
                            putsTxt "ERROR: the testmachine $mchfile does not exist. (PARAM:${mchparams}) \=\> skipped, continue with next file..."
                            puts $fwrite "error_filenotfound(${mchfile},${timestamp});"
                            lappend filenotfoundlist "${mchfile}"
                        }
                    } else {
                        lappend filenotfoundlist "${mchfile}"
                    }
                }
            }
        }
    }
    if {$performtests} {
        puts $fwrite "End_tests(${vers},${timestamp});"
        close $fwrite
    }

    if {!$silentrun} {
      wm transient .testwindow {}
    }
}




proc runprob {mchfile1 mchparams1 mchname1 mrepeatings performtests} {
    eval global {probpath probrev timestamp argv0 errmachines1 errmachines1detail errmachines2 errmachines2detail \
                errmachines3 errmachines3detail errmachines4 errmachines4detail succmachines}
    #performtests=0 : initial dummy run without measuring

    set tmpfile tester.tmp
    file delete $tmpfile

    set ausgabe "0 0"
    set iserror 0

    #OLD: first run without timing (now performed before test run)
    #catch {exec ${probpath}/probcli ${mchfile1} ${mchparams1}}

    #now run for "mrepeatings" times and time...
    set ausgabe [time "catch {exec ${probpath}/probcli ${mchfile1} ${mchparams1}} results options" $mrepeatings]

    if {$performtests} {
        # check for errors only if this is not a "dummy run"
        if [containsPart $options "-errorinfo"] {
            set thiserrinfo "[dict get $options -errorinfo]"
            if [containsPart $thiserrinfo "found_error(invariant_violation"] {
                set iserror 1
                if {$mchname1 ne ""} {
                    lappend errmachines1 "[file tail $mchname1] @ rev. $probrev"
                    lappend errmachines1detail "#### begin detailed error info: [file tail $mchname1] @ rev. $probrev (machine file: $mchfile1), details:\n$thiserrinfo\
                            \n##### end of detailed error info for [file tail $mchname1] @ rev #####\n\n"
                } else {
                    lappend errmachines1 "[file tail $mchfile1] @ rev. $probrev"
                    lappend errmachines1detail "##### begin detailed error info: [file tail $mchfile1] @ rev. $probrev, details:\n$thiserrinfo\
                            \n##### end of detailed error info for ([file tail $mchfile1] @ rev) #####\n\n"
                }
                #msgbox "[file tail $mchfile1] with rev. $probrev: *INVARIANT*:\n$thiserrinfo"
                putsTxt "machine causes invariant violation"
            } elseif [containsPart $thiserrinfo "found_error(deadlock"] {
                set iserror 3; #"deadlock"
                if {$mchname1 ne ""} {
                    lappend errmachines3 "[file tail $mchname1] @ rev. $probrev"
                    lappend errmachines3detail "#### begin detailed error info: [file tail $mchname1] @ rev. $probrev (machine file: $mchfile1), details:\n$thiserrinfo\
                            \n##### end of detailed error info for [file tail $mchname1] @ rev #####\n\n"
                } else {
                    lappend errmachines3 "[file tail $mchfile1] @ rev. $probrev"
                    lappend errmachines3detail "##### begin detailed error info: [file tail $mchfile1] @ rev. $probrev, details:\n$thiserrinfo\
                            \n##### end of detailed error info for ([file tail $mchfile1] @ rev) #####\n\n"
                }
                #msgbox "[file tail $mchfile1] with rev. $probrev: *DEADLOCK*: \n$thiserrinfo"
                #incr errmachinescount 1
                putsTxt "machine causes deadlock error"
            } elseif [containsPart $thiserrinfo "found_error("] {
                set iserror 4; #"error"
                set thiserr "[string range $thiserrinfo [expr {[string first "found_error(" $thiserrinfo]+11}] [string length $thiserrinfo]]"
                set thiserr "[string range $thiserr 0 [string first ")" $thiserr]]"
                if {$mchname1 ne ""} {
                    lappend errmachines4 "${thiserr}: [file tail $mchname1] @ rev. $probrev"
                    lappend errmachines4detail "#### begin detailed error info: [file tail $mchname1] @ rev. $probrev (machine file: $mchfile1), details:\n$thiserrinfo\
                            \n##### end of detailed error info for [file tail $mchname1] @ rev #####\n\n"
                } else {
                    lappend errmachines4 "${thiserr}: [file tail $mchfile1] @ rev. $probrev"
                    lappend errmachines4detail "##### begin detailed error info: [file tail $mchfile1] @ rev. $probrev, details:\n$thiserrinfo\
                            \n##### end of detailed error info for ([file tail $mchfile1] @ rev) #####\n\n"
                }
                #msgbox "[file tail $mchfile1] with rev. $probrev: *OTHER-KNOWN-ERROR*: *$thiserr*\n$thiserrinfo"
                putsTxt "error while executing probcli"
            } elseif [containsPart $thiserrinfo "! goal_found"] {
                set iserror 5; #"goal_found"
                if {$mchname1 ne ""} {
                    lappend errmachines5 "[file tail $mchname1] @ rev. $probrev"
                    lappend errmachines5detail "#### begin detailed error info: [file tail $mchname1] @ rev. $probrev (machine file: $mchfile1), details:\n$thiserrinfo\
                            \n##### end of detailed error info for [file tail $mchname1] @ rev #####\n\n"
                } else {
                    lappend errmachines5 "[file tail $mchfile1] @ rev. $probrev"
                    lappend errmachines5detail "##### begin detailed error info: [file tail $mchfile1] @ rev. $probrev, details:\n$thiserrinfo\
                            \n##### end of detailed error info for ([file tail $mchfile1] @ rev) #####\n\n"
                }
                putsTxt "goal_found while executing probcli"
            } else {
                set iserror 2; #"other error"
                if {$mchname1 ne ""} {
                    lappend errmachines2 "other error (see logfile for details): [file tail $mchname1] @ rev. $probrev"
                    lappend errmachines2detail "#### begin detailed error info: [file tail $mchname1] @ rev. $probrev (machine file: $mchfile1), details:\n$thiserrinfo\
                            \n##### end of detailed error info for [file tail $mchname1] @ rev #####\n\n"
                } else {
                    lappend errmachines2 "other error (see logfile for details): [file tail $mchfile1] @ rev. $probrev"
                    lappend errmachines2detail "##### begin detailed error info: [file tail $mchfile1] @ rev. $probrev, details:\n$thiserrinfo\
                            \n##### end of detailed error info for ([file tail $mchfile1] @ rev) #####\n\n"
                }
                #msgbox "[file tail $mchfile1] with rev. $probrev: *OTHER*:\n$thiserrinfo"
                putsTxt "other error while executing probcli"
            }
        }

        putsTxt $ausgabe
    }

    set ausgabe [string trimleft ${ausgabe} " "]
    # Vom String "ausgabe" nur die Zeichen 0 bis vor dem ersten Leerzeichen nehmen und in "runtime" speichern
    set runtime [string range ${ausgabe} 0 [expr {[string first " " ${ausgabe}]-1}]]

    if {$performtests} {
        putsTxt "runtime ${runtime} microseconds"
        if {$mchname1 ne ""} {
            lappend succmachines "[file tail $mchname1] @ rev. $probrev: ${runtime}ms"
        } else {
            lappend succmachines "[file tail $mchfile1] @ rev. $probrev: ${runtime}ms"
        }
    }

    set mchres "res_probcli(${probrev},${mchfile1},${mchparams1},${mchname1},${runtime},${timestamp},${iserror});"
    return $mchres
}







proc msgbox {msg} { tk_messageBox -icon question -title "Debug" -message $msg }

proc instant_checkout {} {
            #wm geometry . 1x1+[expr {[winfo screenwidth .]/2}]+[expr {[winfo screenheight .]/2}]
            #toplevel .checkoutwnd
            #wm geometry .checkoutwnd 700x300+[expr {[winfo screenwidth .]/2 - 350}]+[expr {[winfo screenheight .]/2 - 175}]
            #wm transient .checkoutwnd .
            wm withdraw .
}

proc readINI {firstrun} {
    eval global {homedir argv0}
    #check if ini file exists
    set inifile revbench.ini
    set stddir probtst_workspace
    global workspace
    set workspace ""
    global workspaceini
    set workspaceini "workspce.dat"
    set returnval 1


    # change to script directory
    #cd $homedir
    cd [file dirname $argv0]

    #check if ini file exists
    set inifile revbench.ini

    if {[file exists $inifile]} {
        #puts "$inifile found..."
        if {[catch {
            set f [open $inifile]
            set inilist [split [read $f] "\n"]; #read ini file
            close $f
        }]} {
                    tk_messageBox -icon error -title "error" -message "Could not read $inifile.\nPlease check permissions." -type ok -parent .
                    exit -1
        }

        set foundpos 0
        set workspace ""
        foreach {zeile} $inilist {
            set pos [string first "workspace=" $zeile]
            if $pos>-1 {
                set foundpos 1
                set workspace [string trim "[string range $zeile [expr {[string first "=" $zeile]+1}] end]" "\n ()"]
            }
        }
        if !{$foundpos} {
            if {[file exists [pwd]/$stddir]} {
                if {[file isdirectory [pwd]/$stddir]} {
                    set thismsg "Workspace found at \"[pwd]/$stddir\".\nDo you want to continue?"
                } else {
                    tk_messageBox -icon error -title "workspace error" -message "Error: workspace \"[pwd]/$stddir\" could not be accessed. There is a file with the same name." -type ok -parent .
                    exit -1
                }
            } else {
                set thismsg "\"$inifile\" does not contain a workspace. If you proceed, a new workspace will be created at \"[pwd]/$stddir\".\nDo you want to continue?"
            }
            set clicked [tk_messageBox -icon warning -title "Could not read ini-file" -message $thismsg -type yesno -parent .]
            switch -- $clicked {
                yes { set workspace [pwd]/$stddir }
                no  { exit -1 }
            }
        } else {
            if {[file exists $workspace] && ![file isdirectory $workspace]} {
                tk_messageBox -icon error -title "workspace error" -message "Error: workspace \"$workspace\" could not be accessed. There is a file with the same name." -type ok -parent .
                exit -1
            } elseif {![file exists $workspace]} {
                set clicked [tk_messageBox -icon warning -title "create workspace" -message "Last used workspace \"$workspace\" does not exist. Do you want to create it?\n(Select no if you want to use the default workspace instead)" -type yesnocancel -parent .]
                switch -- $clicked {
                    yes {   createWorkspaceDir $workspace }
                    no  {   set clicked2 [tk_messageBox -icon question -title "Create default workspace?" -message "Do you want to create a new workspace at \"[pwd]/$stddir\" (default workspace directory)?" -type yesno -parent .]
                            switch -- $clicked2 {
                                yes {   set workspace [pwd]/$stddir
                                        createWorkspaceDir $workspace
                                    }
                                no  { exit -1 }
                            }
                        }
                    cancel { exit -1 }
                }
            }
        }
    } else {
        puts "Checking workspace..."
        if !{[file exists [pwd]/$stddir]} {
            set clicked [tk_messageBox -icon question -title "No workspace" -message "No workspace found. Do you want to create a new workspace at \"[pwd]/$stddir\"?" -type yesno -parent .]
            switch -- $clicked {
                yes { set workspace [pwd]/$stddir  }
                no  { exit -1 }
            }
        } else {
            if {[file isdirectory [pwd]/$stddir]} {
                set workspace [pwd]/$stddir
            } else {
                tk_messageBox -icon error -title "workspace error" -message "Error: workspace [pwd]/$stddir could not be accessed. There is a file with the same name." -type ok -parent .
                exit -1
            }
        }
        set inilist ""
    }


    if {![file exists $workspace]} {
        createWorkspaceDir $workspace;
    }

    if {![file exists $workspace]} {
        tk_messageBox -icon error -title "workspace error" -message "Error: Workspace \"$workspace\" could not be accessed/created. Please check permissions." -type ok -parent .
        exit -1
    }
    puts "used workspace: $workspace"

    if { [catch {
        set f [open $inifile w]; #write ini file
        puts $f "#probtester_ini_file"
        puts $f "workspace=$workspace"
        close $f
    } ] } {
        tk_messageBox -icon error -title "Permission denied" -message "Could not write to [pwd]/$inifile. Please check permissions." -type ok -parent .
        exit -1
    }



    cd $workspace
    global inputfileslist
    set inputfileslist ""
    if {[catch {set inputfileslist [split [glob *.input] " "]}]} {
        set inputfileslist ""
    }
    global resfileslist
    set resfileslist ""
    if {[catch {set resfileslist [split [glob *.res] " "]}]} {
        set resfileslist ""
    }
    global revslist
    set revslist ""
    global revslistDateFirst
    set revslistDateFirst ""
    if {[file exists $workspaceini]} {
        #puts "$workspaceini found..."
        set f [open $workspaceini]
        set revslist [lsort -dictionary [split [read $f] "\n"]]; #read workspace ini file
        foreach {revslistEntry} $revslist {
          set revslistEntry [string trim $revslistEntry " "]
          lappend revslistDateFirst "[string range $revslistEntry [expr {[string first " " $revslistEntry]+1}] 9999] [string range $revslistEntry 0 [expr {[string first " " $revslistEntry]-1}]]"
        }
        set revslistDateFirst [lreverse [lsort -dictionary $revslistDateFirst]]
        #puts "DATEFRST: $revslistDateFirst"
        set revslist ""
        foreach {XXrevslistEntry} $revslistDateFirst {
          set XXrevslistEntry [string trim $XXrevslistEntry " "]
          lappend revslist "[string range $XXrevslistEntry [expr {[string first ")" $XXrevslistEntry]+2}] 9999] [string range $XXrevslistEntry 0 [string first ")" $XXrevslistEntry]]"
        }
        set revslist [lreverse $revslist]
        #revslist: revisionnumber (date), oldest entry first. revslistDateFirst: (date) revisionnumber, newest entry first
        #puts "REVSLIST: $revslist"
        close $f
    } else {
        puts "No workspace index file found."
        #tk_messageBox -icon warning -title "No workspace index" -message "The workspace does not contain an index file. You will have to check out at least one revision before you can run tests." -type ok -parent .
        #exit
    }

    cd $homedir; cd [file dirname $argv0]; # change to script directory

    return $returnval
}



proc showstart {} {
  #eval global {uselocal}

  #if $uselocal {
  #  showstartLocal
  #} else {
    showstartDefault
  #}
}




proc showstartDefault {} {
    eval global {tcl_platform argv argv0 inputfileslist revslist revslistDateFirst resfileslist probrev probrev2 uselocal useeveryXrevisionINPUT}

    global probpath
    set probpath ""


    pack [frame .startwnd] -fill both -expand yes -anchor nw -side top


    eval global {workspace}
    global xinput
    global xrepeatings
    global xtimeout
    global xspecialopts
    global xusedrev
    global xusedrevB
    global xuseresfile
    global xlocalpath
    set xinput ""
    set xrepeatings 1
    set xtimeout 0
    set xspecialopts ""
    set xusedrev ""
    set xusedrevB ""
    set xuseresfile ""
    set xlocalpath ""; #use probcli from local path instead of checked out revision
      eval global {predefinedLocal}; set xlocalpath $predefinedLocal
    set uselocal 0

    frame .startwnd.work
    frame .startwnd.probframe
    frame .startwnd.middle
    frame .startwnd.middle.work1
    frame .startwnd.middle.work2
    frame .startwnd.menu1
    frame .startwnd.menu1.top
    frame .startwnd.menu1.bottom
    frame .startwnd.infospecialframe
    frame .startwnd.resframe
    frame .startwnd.menu2

    pack .startwnd.work -side top -fill both
    pack .startwnd.probframe -side top -fill both
    pack .startwnd.middle -side top -fill both
    pack .startwnd.middle.work1 -side left -fill y
    pack .startwnd.middle.work2 -side right -expand yes -fill both
    pack .startwnd.infospecialframe -side top -fill both
    pack .startwnd.menu1 -side top -fill both
    pack .startwnd.menu1.top -side left -expand yes -fill both
    pack .startwnd.menu1.bottom -side right -expand yes -fill both
    pack .startwnd.resframe -side top -fill both
    pack .startwnd.menu2 -side bottom -fill both

    button .startwnd.menu1.top.weiter -text "Run tests" -command {
        if {([string length $xusedrev] == 0) && ([string length $xlocalpath] == 0)} {
            tk_messageBox -icon warning -title "error" -message "Please specify the revision(s) or a local path to probcli" -type ok -parent .startwnd
        } elseif {(([string length $xusedrev] > 0)||([string length $xusedrevB] > 0)) && ([string length $xlocalpath] > 0)} {
            tk_messageBox -icon warning -title "error" -message "Please specify EITHER the revision(s) you want to use OR a local path to probcli, not both." -type ok -parent .startwnd
        } elseif {(!([file exists [string trim $xlocalpath/probcli]])) && ([string length $xlocalpath] > 0)} {
            tk_messageBox -icon warning -title "error" -message "The directory you specified does not contain probcli: \"[string trim $xlocalpath]\"\n\nPlease specify only the path without file name." -type ok -parent .startwnd
        } elseif {[string length $xinput] == 0} {
            tk_messageBox -icon warning -title "error" -message "Please specify the input file.\n\nTo create new input files please click on \"Create new input file...\"" -type ok -parent .startwnd
        } elseif {[string length $xrepeatings] == 0} {
            tk_messageBox -icon warning -title "error" -message "Please specify the number of repeatings." -type ok -parent .startwnd
        } elseif {[containsPartNCS $xspecialopts "--timeout"]} {
            tk_messageBox -icon warning -title "error" -message "Please set a value for \"timeout for probcli (ms)\" instead of using --timeout option." -type ok -parent .startwnd
        } else {
            #perform tests
            if {[string length $xtimeout] == 0} {set xtimeout 0}
            set xuseresfile "[string range $xinput 0 [string last "." $xinput]]res"
            set resfile $xuseresfile

            if {[string length $xusedrev] > 0} {
              set xusedrev "[string range $xusedrev [expr {[string first ")" $xusedrev]+2}] 9999] [string range $xusedrev 0 [string first ")" $xusedrev]]"; #change to inverse format (revision number first)
              if {[string length $xusedrevB] > 0} {set xusedrevB "[string range $xusedrevB [expr {[string first ")" $xusedrevB]+2}] 9999] [string range $xusedrevB 0 [string first ")" $xusedrevB]]"}
              # use probcli from workspace (checked out revisions)
              set uselocal 0
              if {[string first " " [string trim $xusedrev " "]]>-1} {
                set probrev [string trim "[string range $xusedrev 0 [string first " " $xusedrev]]" " ()"]
              } else {
                set probrev [string trim $xusedrev " ()"]
              }
            } else {
              # use probcli from local path
              set probrev $xlocalpath
              set uselocal 1
            }
            set probrev2 0
            if {[string length $xusedrevB] > 0} {
                if {[string first " " [string trim $xusedrevB " "]]>-1} {
                    set probrev2 [string trim "[string range $xusedrevB 0 [string first " " $xusedrevB]]" " ()"]
                } else {
                    set probrev2 [string trim $xusedrevB " ()"]
                }
            }
            destroy .startwnd

            wm geometry . 1x1+[expr {([winfo screenwidth .] / 2)}]+[expr {([winfo screenheight .] / 2)}]
            toplevel .testwindow
            wm geometry .testwindow 800x600+[expr {[winfo screenwidth .]/2 - 400}]+[expr {[winfo screenheight .]/2 - 300}]
            wm transient .testwindow .
            set_runtests_window .testwindow

            if {$uselocal} {
              set probpath $probrev
            } else {
              set probpath $workspace/rev$probrev
            }
            prepare_runtests .testwindow $probrev $probrev2 0 0 $useeveryXrevisionINPUT
        }
    }
    button .startwnd.menu1.bottom.ende -text "Exit" -command exit

    button .startwnd.menu1.top.createinput -text "   Create new input file...   " -command {
            eval global {homedir}
            cd $homedir; cd [file dirname $argv0]; # change to script directory
            #destroy .startwnd
            #wm geometry . 1x1+[expr {[winfo screenwidth .]/2}]+[expr {[winfo screenheight .]/2}]
            toplevel .inputwnd
            wm geometry .inputwnd 700x350+[expr {[winfo screenwidth .]/2 - 350}]+[expr {[winfo screenheight .]/2 - 175}]
            wm transient .inputwnd .
            input_main
    }
    button .startwnd.probframe.checkoutrev -text "Check out more revisions..." -command {
            eval global {homedir}
            cd $homedir; cd [file dirname $argv0]; # change to script directory
            #destroy .startwnd
            #wm geometry . 1x1+[expr {[winfo screenwidth .]/2}]+[expr {[winfo screenheight .]/2}]
            toplevel .checkoutwnd
            wm geometry .checkoutwnd 700x300+[expr {[winfo screenwidth .]/2 - 350}]+[expr {[winfo screenheight .]/2 - 175}]
            wm transient .checkoutwnd .
            checkout_main
    }
    #underconstruction
    button .startwnd.menu1.bottom.specialtests -text "Find performance differences..." -command {
            eval global {homedir}
            cd $homedir; cd [file dirname $argv0]; # change to script directory
            #destroy .startwnd
            #wm geometry . 1x1+[expr {[winfo screenwidth .]/2}]+[expr {[winfo screenheight .]/2}]
            toplevel .specialtestswnd
            wm geometry .specialtestswnd 700x600+[expr {[winfo screenwidth .]/2 - 350}]+[expr {[winfo screenheight .]/2 - 300}]
            wm transient .specialtestswnd .
            specialCheckoutTest
    }


    label .startwnd.work.ltop -text "To run tests, please choose input file, revision and number of repeatings.\n \
         To show the results of previous tests, please choose the result file.\n \
         You can also check out more revisions or create a new input file."

    pack [label .startwnd.infospecialframe.labelspec -text "*all valid probcli options (i.e. -mc 1000) may be used here.\n \
         *Warning: Machine specific options for probcli defined in input file will be ignored!"] -padx 5 -pady 2 -expand yes -side top


    ttk::labelframe .startwnd.probframe.probS -text "use revisions in workspace"
    label .startwnd.probframe.probS.lbl1 -text "from:" -pady 3
    ttk::combobox .startwnd.probframe.probS.erev -textvariable xusedrev -state readonly -values $revslistDateFirst
    label .startwnd.probframe.probS.lbl2 -text "to:" -pady 3
    ttk::combobox .startwnd.probframe.probS.erev2 -textvariable xusedrevB -state readonly -values $revslistDateFirst

    label .startwnd.probframe.probS.lbl3 -text "step:" -pady 3
    set useeveryXrevisionSelection {1 2 3 4 5 6 7 8 9 10}
    ttk::combobox .startwnd.probframe.probS.erev3 -textvariable useeveryXrevisionINPUT -state readonly -values $useeveryXrevisionSelection


    pack .startwnd.probframe.probS -side top -pady 10 -padx 10 -expand yes -fill x
    pack .startwnd.probframe.probS.lbl1 -pady 10 -padx 10 -side left
    pack .startwnd.probframe.probS.erev -pady 10 -padx 10 -expand yes -side left -fill x
    pack .startwnd.probframe.probS.lbl2 -pady 10 -padx 10 -side left
    pack .startwnd.probframe.probS.erev2 -pady 10 -padx 10 -expand yes -side left -fill x
    pack .startwnd.probframe.probS.lbl3 -pady 10 -padx 1 -side left
    pack .startwnd.probframe.probS.erev3 -pady 10 -padx 6 -expand yes -side left -fill x
    pack .startwnd.probframe.checkoutrev

    label .startwnd.middle.work1.lsource -text "Input file:" -pady 3
    ttk::combobox .startwnd.middle.work2.esource -textvariable xinput -state readonly -values $inputfileslist

    #label .startwnd.middle.work1.lrev -text "from Revision:" -pady 3
    #ttk::combobox .startwnd.middle.work2.erev -textvariable xusedrev -state readonly -values $revslist

    #label .startwnd.middle.work1.lrev2 -text "to Revision:" -pady 3
    #ttk::combobox .startwnd.middle.work2.erev2 -textvariable xusedrevB -state readonly -values $revslist


    #LOCAL probcli
    ttk::labelframe .startwnd.probframe.probL -text "OR use local probcli"
      label .startwnd.probframe.probL.lbl1 -text "Local probcli:" -pady 3
    entry .startwnd.probframe.probL.edt1 -textvariable xlocalpath
    button .startwnd.probframe.probL.btn1 -text "Choose path..." -command {
        set xlocalpath [input_openprobcli]
    }
    pack .startwnd.probframe.probL -side top -pady 10 -padx 10 -expand yes -fill x
    #pack .startwnd.probframe.probL.lbl1 -pady 10 -padx 10 -side left
    pack .startwnd.probframe.probL.edt1 -pady 10 -padx 10 -expand yes -side left -fill x
    pack .startwnd.probframe.probL.btn1 -pady 10 -padx 10 -side right



    label .startwnd.middle.work1.lterm -text "Repeatings:" -pady 3
    spinbox .startwnd.middle.work2.eterm -from 1 -to 100 -validate key -vcmd {string is integer %P} -textvariable xrepeatings

    label .startwnd.middle.work1.ltimeout -text "Timeout for probcli (ms):"
    spinbox .startwnd.middle.work2.etimeout -from 0 -to 100000000 -validate key -vcmd {string is integer %P} -textvariable xtimeout

    label .startwnd.middle.work1.lspezial -text "Options for probcli*:"
    entry .startwnd.middle.work2.espezial -textvariable xspecialopts

    pack .startwnd.work.ltop -padx 5 -pady 10 -expand yes -side top

    pack .startwnd.middle.work1.lsource .startwnd.middle.work1.lterm .startwnd.middle.work1.ltimeout .startwnd.middle.work1.lspezial -side top -padx 3 -pady 3 -fill y
    pack .startwnd.middle.work2.esource .startwnd.middle.work2.eterm .startwnd.middle.work2.etimeout .startwnd.middle.work2.espezial -side top -expand yes -padx 3 -pady 3 -fill x

    pack .startwnd.menu1.top.weiter .startwnd.menu1.top.createinput -pady 5 -padx 10 -side top -expand yes -fill x
    pack .startwnd.menu1.bottom.specialtests .startwnd.menu1.bottom.ende -pady 5 -padx 10 -side bottom -expand yes -fill x
    #pack .startwnd.menu1.bottom.checkoutrev .startwnd.menu1.bottom.specialtests .startwnd.menu1.bottom.ende -pady 5 -padx 10 -side bottom -expand yes -fill x
    #pack .startwnd.menu1.bottom.specialtests -pady 10 -padx 10 -side bottom -expand yes -fill x

    ttk::labelframe .startwnd.resframe.res1 -text "Results"
    label .startwnd.resframe.res1.lresfile -text "Result file:"
    ttk::combobox .startwnd.resframe.res1.eresfile -textvariable xuseresfile -state readonly -values $resfileslist
    button .startwnd.resframe.res1.weiter -text "   Show results   " -command {
        if {[string length $xuseresfile] == 0} {
            tk_messageBox -icon warning -title "error" -message "Please specify the result file." -type ok -parent .startwnd
        } else {
            set resfile $xuseresfile
            destroy .startwnd
            prepare_showres {} {}; #[getrevisions $probrev $probrev2]
        }
    }

    pack .startwnd.resframe.res1 -side top -pady 10 -padx 10 -expand yes -fill x
    pack .startwnd.resframe.res1.lresfile -pady 10 -padx 10 -side left
    pack .startwnd.resframe.res1.eresfile -pady 10 -padx 10 -expand yes -side left -fill x
    pack .startwnd.resframe.res1.weiter -pady 10 -padx 10 -side right

    label .startwnd.menu2.lworkspace -text "Workspace:    $workspace"
    pack .startwnd.menu2.lworkspace -padx 10 -pady 10 -expand yes -fill x

    wm geometry . 600x700+[expr {[winfo screenwidth .]/2 - 275}]+[expr {[winfo screenheight .]/2 - 300}]
}


proc set_runtests_window {testwnd} {
    eval global {putsDest putsistxtbox}
    global testwndGlob
    set testwndGlob $testwnd

    if {[tk windowingsystem] ne "aqua"} {
        ttk::scrollbar $testwnd.scrolly -command { $testwndGlob.txt yview } -orient vertical
        ttk::scrollbar $testwnd.scrollx -command { $testwndGlob.txt xview } -orient horizontal
    } else {
        scrollbar $testwnd.scrolly -command { $testwndGlob.txt yview } -orient vertical
        scrollbar $testwnd.scrollx -command { $testwndGlob.txt xview } -orient horizontal
    }
    text $testwnd.txt -font {Courier 10} -wrap none -xscrollcommand "$testwnd.scrollx set" -yscrollcommand "$testwnd.scrolly set"
    pack $testwnd.scrollx -side bottom -fill x
    pack $testwnd.scrolly -side right -fill y
    pack $testwnd.txt -side left -fill both -expand 1
    set putsDest "$testwnd.txt"
    set putsistxtbox 1
    update
    update idletasks
}



####################################################################################
# #########################################################
#  Probench test file collection part #
# #########################################################



proc input_main {} {
    eval global {tcl_platform argv argv0}

    bind .inputwnd <Escape> {
        eval global {homedir}
        cd $homedir; cd [file dirname $argv0]; # change to script directory
        readINI 0
        #showstart
        pack forget .inputwnd
        destroy .inputwnd
    }
    wm title .inputwnd "Probench Test File Collection Tool"
    wm geometry .inputwnd 700x300+[expr {[winfo screenwidth .]/2 - 350}]+[expr {[winfo screenheight .]/2 - 175}]

    global os
    if [containsPartNCS $tcl_platform(os) "windows"] {
     set os win
    } elseif [containsPartNCS $tcl_platform(os) "linux"] {
     set os linux
    } elseif [containsPartNCS $tcl_platform(os) "mac"] {
     set os mac
    } elseif [containsPartNCS $tcl_platform(os) "unix"] {
     set os unix
    } else {
     set os other
    }
    # #####################################################

    set stddir probtst_workspace



    global homedir
    set homedir [pwd]

    global workspace
    set workspace ""

    global workspaceini
    set workspaceini "workspce.dat"


    # change to script directory
    #cd $homedir
    cd [file dirname $argv0]

    #check if ini file exists
    set inifile revbench.ini

    if {[file exists $inifile]} {
        #puts "$inifile found..."
        if {[catch {
            set f [open $inifile]
            set inilist [split [read $f] "\n"]; #read ini file
            close $f
        }]} {
                    tk_messageBox -icon error -title "error" -message "Could not read $inifile.\nPlease check permissions." -type ok -parent .inputwnd
                    exit -1
        }

        set foundpos 0
        set workspace ""
        foreach {zeile} $inilist {
            set pos [string first "workspace=" $zeile]
            if $pos>-1 {
                set foundpos 1
                set workspace [string trim "[string range $zeile [expr {[string first "=" $zeile]+1}] end]" "\n ()"]
            }
        }
        if !{$foundpos} {
            if {[file exists [pwd]/$stddir]} {
                if {[file isdirectory [pwd]/$stddir]} {
                    set thismsg "Workspace found at \"[pwd]/$stddir\".\nDo you want to continue?"
                } else {
                    tk_messageBox -icon error -title "workspace error" -message "Error: workspace \"[pwd]/$stddir\" could not be accessed. There is a file with the same name." -type ok -parent .inputwnd
                    exit -1
                }
            } else {
                set thismsg "\"$inifile\" does not contain a workspace. If you proceed, a new workspace will be created at \"[pwd]/$stddir\".\nDo you want to continue?"
            }
            set clicked [tk_messageBox -icon warning -title "Could not read ini-file" -message $thismsg -type yesno -parent .inputwnd]
            switch -- $clicked {
                yes { set workspace [pwd]/$stddir }
                no  { exit -1 }
            }
        } else {
            if {[file exists $workspace] && ![file isdirectory $workspace]} {
                tk_messageBox -icon error -title "workspace error" -message "Error: workspace \"$workspace\" could not be accessed. There is a file with the same name." -type ok -parent .inputwnd
                exit -1
            } elseif {![file exists $workspace]} {
                set clicked [tk_messageBox -icon warning -title "create workspace" -message "Last used workspace \"$workspace\" does not exist. Do you want to create it?\n(Select no if you want to use the default workspace instead)" -type yesnocancel -parent .inputwnd]
                switch -- $clicked {
                    yes {   createWorkspaceDir $workspace }
                    no  {   set clicked2 [tk_messageBox -icon question -title "Create default workspace?" -message "Do you want to create a new workspace at \"[pwd]/$stddir\" (default workspace directory)?" -type yesno -parent .inputwnd]
                            switch -- $clicked2 {
                                yes {   set workspace [pwd]/$stddir
                                        createWorkspaceDir $workspace
                                    }
                                no  { exit -1 }
                            }
                        }
                    cancel { exit -1 }
                }
            }
        }
    } else {
        puts "Checking workspace..."
        if !{[file exists [pwd]/$stddir]} {
            set clicked [tk_messageBox -icon question -title "No workspace" -message "No workspace found. Do you want to create a new workspace at \"[pwd]/$stddir\"?" -type yesno -parent .inputwnd]
            switch -- $clicked {
                yes { set workspace [pwd]/$stddir  }
                no  { exit -1 }
            }
        } else {
            if {[file isdirectory [pwd]/$stddir]} {
                set workspace [pwd]/$stddir
            } else {
                tk_messageBox -icon error -title "workspace error" -message "Error: workspace [pwd]/$stddir could not be accessed. There is a file with the same name." -type ok -parent .inputwnd
                exit -1
            }
        }
        set inilist ""
    }


    if {![file exists $workspace]} {
        createWorkspaceDir $workspace;
    }

    if {![file exists $workspace]} {
        tk_messageBox -icon error -title "workspace error" -message "Error: Workspace \"$workspace\" could not be accessed/created. Please check permissions." -type ok -parent .inputwnd
        exit -1
    }
    puts "used workspace: $workspace"

    if { [catch {
        set f [open $inifile w]; #write ini file
        puts $f "#probtester_ini_file"
        puts $f "workspace=$workspace"
        close $f
    } ] } {
        tk_messageBox -icon error -title "Permission denied" -message "Could not write to [pwd]/$inifile. Please check permissions." -type ok -parent .inputwnd
        exit -1
    }


    input_showdiag

}


proc createWorkspaceDir {xdir} {
    if { [catch { file mkdir $xdir } ] } {
        tk_messageBox -icon error -title "Permission denied" -message "Could not create workspace: $xdir. Please check permissions." -type ok -parent .inputwnd
        exit -1
    }
}


proc deleteDir {xdir} {
    if { [catch { file delete -force $xdir } ] } {
        tk_messageBox -icon error -title "Permission denied" -message "Could not delete files in workspace: $xdir. Please check permissions." -type ok -parent .inputwnd
        exit -1
    }
}




proc input_showdiag {} {
    eval global {workspace os}



    global xinput
    global xaddfile
    global xlabel
    global xparam
    set xinput ""
    set xaddfile ""
    set xlabel ""
    set xparam ""

    frame .inputwnd.work
    frame .inputwnd.middle
    frame .inputwnd.middle.work1
    frame .inputwnd.middle.work2
    frame .inputwnd.middle.work2.a
    frame .inputwnd.middle.work2.b
    frame .inputwnd.menu

    pack .inputwnd.work -side top -fill both
    pack .inputwnd.middle -side top -fill both
    pack .inputwnd.middle.work1 -side left
    pack .inputwnd.middle.work2 -side right -expand yes -fill both
    pack .inputwnd.middle.work2.a -side left -expand yes -fill both
    pack .inputwnd.middle.work2.b -side right -fill y
    pack .inputwnd.menu -side bottom -fill both

    button .inputwnd.menu.weiter -text "Add to collection" -command {
        if {[string length $xinput] == 0} {
            tk_messageBox -icon error -title "error" -message "Please choose a test file collection. You may choose an existing collection or write a new name." -type ok -parent .inputwnd
        } elseif {[string length $xaddfile] == 0} {
            tk_messageBox -icon error -title "error" -message "Please choose the test file you want to add." -type ok -parent .inputwnd
        } elseif {![file exists $xaddfile]} {
            tk_messageBox -icon error -title "error" -message "The test file $xaddfile does not exist." -type ok -parent .inputwnd
        } elseif {[string first " " $xlabel] > -1} {
            tk_messageBox -icon error -title "error" -message "Labels may not contain spaces." -type ok -parent .inputwnd
        } elseif {[string first " " $xinput] > -1} {
            tk_messageBox -icon error -title "error" -message "Test file collection name may not contain spaces." -type ok -parent .inputwnd
        } else {
            set thisdir [pwd]
            cd $workspace
            if {[string length $xinput] > 6} {
                if {[string last ".input" $xinput] > [expr {[string length $xinput]-7}]} {
                    set inputfile "${xinput}"
                } else {
                    set inputfile "${xinput}.input"
                }
            } else {
                    set inputfile "${xinput}.input"
            }
            if {[catch {
                if {[file exists $inputfile]} {
                    set fwrite [open $inputfile a]
                    puts $fwrite "${xaddfile}\@${xparam}\@${xlabel}"
                    close $fwrite
                } else {
                    set fwrite [open $inputfile w]
                    puts $fwrite "${xaddfile}\@${xparam}\@${xlabel}"
                    close $fwrite
                }
            }]} {
                    tk_messageBox -icon error -title "error" -message "Could not write to file $inputfile in workspace $workspace.\nPlease check permissions." -type ok -parent .inputwnd
                    exit -1
            }
            cd $thisdir
            set clicked [tk_messageBox -icon info -title "Saved." -message "File was added to $xinput. Do you want to add more files?" -type yesno -parent .inputwnd]
            switch -- $clicked {
                yes {  }
                no  {
                    eval global {homedir}
                    cd $homedir; cd [file dirname $argv0]; # change to script directory
                    readINI 0
                    #showstart
                    pack forget .inputwnd
                    destroy .inputwnd
                }
            }

            set xlabel ""
            set xaddfile ""
            if {[catch {set inputfileslist [split [glob *.input] " "]}]} {
                set inputfileslist ""
            }

        }
    }
    button .inputwnd.menu.back -text " Back to Main Menu " -command {
        eval global {homedir}
        cd $homedir; cd [file dirname $argv0]; # change to script directory
        readINI 0
        #showstart
        pack forget .inputwnd
        destroy .inputwnd
    }

    label .inputwnd.work.ltop -text "Please choose the test file you want to add. You may choose a label an the parameters for probcli, too.\
                        "

    label .inputwnd.middle.work1.lrev -text "Test file (B-machine):"
    entry .inputwnd.middle.work2.a.erev -textvariable xaddfile
    button .inputwnd.middle.work2.b.choosefile -text "Choose file..." -command {
        set xaddfile [input_openTest]
    }

    label .inputwnd.middle.work1.lsource -text "Label:"
    entry .inputwnd.middle.work2.a.esource -textvariable xlabel

    label .inputwnd.middle.work1.lmake -text "parameters for probcli:"
    entry .inputwnd.middle.work2.a.emake -textvariable xparam

    cd $workspace
    global inputfileslist
    set inputfileslist ""
    if {[catch {set inputfileslist [split [glob *.input] " "]}]} {
        set inputfileslist ""
    }

    label .inputwnd.middle.work1.lterm -text "Test file collection:"
    ttk::combobox .inputwnd.middle.work2.a.eterm -textvariable xinput -values $inputfileslist

    label .inputwnd.middle.work1.lworkspace -text "Workspace:"
    label .inputwnd.middle.work2.a.eworkspace -anchor w -text $workspace

    pack .inputwnd.work.ltop -padx 5 -pady 10 -expand yes -side top

    pack .inputwnd.middle.work1.lrev .inputwnd.middle.work1.lsource .inputwnd.middle.work1.lmake .inputwnd.middle.work1.lterm -side top -padx 3 -pady 3
    pack .inputwnd.middle.work2.a.erev .inputwnd.middle.work2.a.esource .inputwnd.middle.work2.a.emake .inputwnd.middle.work2.a.eterm -side top -expand yes -padx 3 -pady 3 -fill x
    pack .inputwnd.middle.work2.b.choosefile  -side top -padx 3 -pady 3

    #button .inputwnd.menu.ende -text "        Exit        " -command exit
    #pack .inputwnd.menu.weiter .inputwnd.menu.back .inputwnd.menu.ende -pady 10 -padx 10 -side left -expand yes -fill x
    pack .inputwnd.menu.weiter .inputwnd.menu.back -pady 10 -padx 10 -side left -expand yes -fill x

}


proc input_openTest {} {
    set type {{"B files" {.mch .ref .imp .csp .eventb}} {"All files" {*}}}
    set thisfile [tk_getOpenFile -filetypes $type -parent .inputwnd]
    return $thisfile
}


proc input_openprobcli {} {
    set type {{"probcli" {probcli}} {"All files" {*}}}
    #set thisfile [tk_getOpenFile -filetypes $type -parent .startwnd]
    set thisfile [tk_chooseDirectory -mustexist true -parent .startwnd -title "Choose path to a local probcli"]
    return $thisfile
}
####################################################################################
####################################################################################
####################################################################################
# #########################################################
#  Probench checkout part
# #########################################################



proc checkout_main {} {
    eval global {tcl_platform argv argv0}

    #toplevel .checkoutwnd
    bind .checkoutwnd <Escape> {
        eval global {homedir}
        cd $homedir; cd [file dirname $argv0]; # change to script directory
        readINI 0
        #showstart
        pack forget .checkoutwnd
        destroy .checkoutwnd
    }
    wm title .checkoutwnd "Probench Checkout Tool"
    wm geometry .checkoutwnd 700x350+[expr {[winfo screenwidth .]/2 - 350}]+[expr {[winfo screenheight .]/2 - 175}]

    global os
    if [containsPartNCS $tcl_platform(os) "windows"] {
     set os win
    } elseif [containsPartNCS $tcl_platform(os) "linux"] {
     set os linux
    } elseif [containsPartNCS $tcl_platform(os) "mac"] {
     set os mac
    } elseif [containsPartNCS $tcl_platform(os) "unix"] {
     set os unix
    } else {
     set os other
    }
    # #####################################################

    set stddir probtst_workspace


    global homedir
    set homedir [pwd]

    global workspace
    set workspace ""

    global workspaceini
    set workspaceini "workspce.dat"


    # change to script directory
    #cd $homedir
    cd [file dirname $argv0]

    #check if ini file exists
    set inifile revbench.ini

    if {[file exists $inifile]} {
        #puts "$inifile found..."
        if {[catch {
            set f [open $inifile]
            set inilist [split [read $f] "\n"]; #read ini file
            close $f
        }]} {
                    tk_messageBox -icon error -title "error" -message "Could not read $inifile.\nPlease check permissions." -type ok -parent .checkoutwnd
                    exit -1
        }

        set foundpos 0
        set workspace ""
        foreach {zeile} $inilist {
            set pos [string first "workspace=" $zeile]
            if $pos>-1 {
                set foundpos 1
                set workspace [string trim "[string range $zeile [expr {[string first "=" $zeile]+1}] end]" "\n ()"]
            }
        }
        if !{$foundpos} {
            if {[file exists [pwd]/$stddir]} {
                if {[file isdirectory [pwd]/$stddir]} {
                    set thismsg "Workspace found at \"[pwd]/$stddir\".\nDo you want to continue?"
                } else {
                    tk_messageBox -icon error -title "workspace error" -message "Error: workspace \"[pwd]/$stddir\" could not be accessed. There is a file with the same name." -type ok -parent .checkoutwnd
                    exit -1
                }
            } else {
                set thismsg "\"$inifile\" does not contain a workspace. If you proceed, a new workspace will be created at \"[pwd]/$stddir\".\nDo you want to continue?"
            }
            set clicked [tk_messageBox -icon warning -title "Could not read ini-file" -message $thismsg -type yesno -parent .checkoutwnd]
            switch -- $clicked {
                yes { set workspace [pwd]/$stddir }
                no  { exit -1 }
            }
        } else {
            if {[file exists $workspace] && ![file isdirectory $workspace]} {
                tk_messageBox -icon error -title "workspace error" -message "Error: workspace \"$workspace\" could not be accessed. There is a file with the same name." -type ok -parent .checkoutwnd
                exit -1
            } elseif {![file exists $workspace]} {
                set clicked [tk_messageBox -icon warning -title "create workspace" -message "Last used workspace \"$workspace\" does not exist. Do you want to create it?\n(Select no if you want to use the default workspace instead)" -type yesnocancel -parent .checkoutwnd]
                switch -- $clicked {
                    yes {   createWorkspaceDir $workspace }
                    no  {   set clicked2 [tk_messageBox -icon question -title "Create default workspace?" -message "Do you want to create a new workspace at \"[pwd]/$stddir\" (default workspace directory)?" -type yesno -parent .checkoutwnd]
                            switch -- $clicked2 {
                                yes {   set workspace [pwd]/$stddir
                                        createWorkspaceDir $workspace
                                    }
                                no  { exit -1 }
                            }
                        }
                    cancel { exit -1 }
                }
            }
        }
    } else {
        puts "Checking workspace..."
        if !{[file exists [pwd]/$stddir]} {
            set clicked [tk_messageBox -icon question -title "No workspace" -message "No workspace found. Do you want to create a new workspace at \"[pwd]/$stddir\"?" -type yesno -parent .checkoutwnd]
            switch -- $clicked {
                yes { set workspace [pwd]/$stddir  }
                no  { exit -1 }
            }
        } else {
            if {[file isdirectory [pwd]/$stddir]} {
                set workspace [pwd]/$stddir
            } else {
                tk_messageBox -icon error -title "workspace error" -message "Error: workspace [pwd]/$stddir could not be accessed. There is a file with the same name." -type ok -parent .checkoutwnd
                exit -1
            }
        }
        set inilist ""
    }


    if {![file exists $workspace]} {
        createWorkspaceDir $workspace;
    }

    if {![file exists $workspace]} {
        tk_messageBox -icon error -title "workspace error" -message "Error: Workspace \"$workspace\" could not be accessed/created. Please check permissions." -type ok -parent .checkoutwnd
        exit -1
    }
    puts "used workspace: $workspace"

    if { [catch {
        set f [open $inifile w]; #write ini file
        puts $f "#probtester_ini_file"
        puts $f "workspace=$workspace"
        close $f
    } ] } {
        tk_messageBox -icon error -title "Permission denied" -message "Could not write to [pwd]/$inifile. Please check permissions." -type ok -parent .checkoutwnd
        exit -1
    }


    checkout_showdiag

}





proc checkout_runall {getrev fromsource termcmd svncmd makecmd} {
    eval global {workspace workspaceini os silentrun sourcedir notempdelete}

    cd $workspace


    if [string match $os win] {
        set exportcmd "set"
        set trenner " && "
    } else {
        set exportcmd "export"
        set trenner ";"
    }


    set ii 0
    set cmd01 ""
    set cmd02 ""
    set cmd03 ""
    set cmd04 ""
    set cmd05 ""
    set cmd06 ""

    if [string length $termcmd]>0 {
        foreach {teil} [split $termcmd " "] {
            incr ii 1
            switch -- $ii {
                1 { set cmd01 $teil }
                2 { set cmd02 $teil }
                3 { set cmd03 $teil }
                4 { set cmd04 $teil }
                5 { set cmd05 $teil }
                6 { set cmd06 $teil }
                7 { checkout_errmsg "Too many parameters for terminal command."
                    exit -1
                  }
            }
        }
    }


    set shellskript "echo getting repository information from ${fromsource}...$trenner \
                $svncmd info --xml $fromsource\@$getrev > $workspace/lastrevinfo.xml$trenner\
                echo Finished."



    if { [catch {
            switch -- $ii {
                0  { puts [exec -- $shellskript] }
                1  { puts [exec -- $cmd01 $shellskript] }
                2  { puts [exec -- $cmd01 $cmd02 $shellskript] }
                3  { puts [exec -- $cmd01 $cmd02 $cmd03 $shellskript] }
                4  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $shellskript] }
                5  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $cmd05 $shellskript] }
                6  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $cmd05 $cmd06 $shellskript]}
                default { checkout_errmsg "Too many parameters for command."
                          exit -1
                        }
            }
        } ] } {
                    checkout_errmsg "An error occurred.\n\nErrorInfo:\n$::errorInfo"
                    cancelcheckout
                    return
    }

    if {![file exists $workspace/lastrevinfo.xml]} {
                    checkout_errmsg "Could not write to workspace $workspace.\nPlease check permissions."
                    cancelcheckout
                    return
    }

    if {[catch {
            set f [open $workspace/lastrevinfo.xml]
            set lastrevinfo [read $f]; #[split [read $f] "\n"]
            close $f
    }]} {
            checkout_errmsg "Could not read from workspace.\nPlease check permissions."
            cancelcheckout
            return
    }


    set getrevdate ""
            set pos [string first "revision=\"" $lastrevinfo]
            if $pos>-1 {
                set foundrevnr 1
                set part [string range $lastrevinfo [expr {$pos+10}] end]
                set getrev [string trim "[string range $part 0 [expr {[string first "\"" $part] - 1}]]" " \n"]
            }
            set pos [string first "\<date\>" $lastrevinfo]
            if $pos>-1 {
                set foundrevdate 1
                set part [string range $lastrevinfo [expr {$pos+6}] end]
                set getrevdate [string trim "[string range $part 0 [expr {[string first "\<\/date\>" $part] - 1}]]" " \n"]
            }

    deleteDir "$workspace/lastrevinfo.xml"

    set revtmpdir "rev$getrev/tmp"
    set revdir "rev$getrev"
    set exportdir "PROB_HOME=$workspace/$revtmpdir"

    if { [catch { deleteDir "$workspace/$revdir"; file mkdir $workspace/$revtmpdir } ] } {
                    checkout_errmsg "Could not write to workspace $workspace.\nPlease check permissions."
                    cancelcheckout
                    return
    }


    set shellskript "echo check out from repository ${fromsource}...$trenner \
                $svncmd export --force -r $getrev --depth files $fromsource/${sourcedir} $workspace/$revtmpdir$trenner \
                $svncmd export --force -r $getrev $fromsource/${sourcedir}lib $workspace/$revtmpdir/lib$trenner \
                $svncmd export --force -r $getrev $fromsource/${sourcedir}lib_platform_specific $workspace/$revtmpdir/lib_platform_specific$trenner \
                $svncmd export --force -r $getrev $fromsource/${sourcedir}src $workspace/$revtmpdir/src$trenner \
                $svncmd export --force -r $getrev $fromsource/${sourcedir}extensions $workspace/$revtmpdir/extensions$trenner \
                $svncmd export --force -r $getrev $fromsource/${sourcedir}plugins $workspace/$revtmpdir/plugins$trenner \
                $svncmd export --force -r $getrev $fromsource/${sourcedir}gradle $workspace/$revtmpdir/gradle$trenner \
                $svncmd export --force -r $getrev $fromsource/${sourcedir}tcl $workspace/$revtmpdir/tcl$trenner \
                cd $workspace/$revtmpdir
                echo setting PROB_HOME...$trenner \
                $exportcmd $exportdir$trenner \
                echo running make$trenner \
                $makecmd$trenner \
                sleep 1; \
                # sicstus$trenner \
                echo Finished."


    if { [catch {
            switch -- $ii {
                0  { puts [exec -- $shellskript] }
                1  { puts [exec -- $cmd01 $shellskript] }
                2  { puts [exec -- $cmd01 $cmd02 $shellskript] }
                3  { puts [exec -- $cmd01 $cmd02 $cmd03 $shellskript] }
                4  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $shellskript] }
                5  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $cmd05 $shellskript] }
                6  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $cmd05 $cmd06 $shellskript]}
                default { checkout_errmsg "Too many parameters for command."
                            cancelcheckout
                            return
                        }
            }
    } ] } {
                    checkout_errmsg "An error occurred.\n\nErrorInfo:\n$::errorInfo"
                    #puts "error: $::errorInfo"
                    cancelcheckout
                    return
    }

                #exit 99

    if { [catch {
        if {[file exists $workspace/$revtmpdir/probcli]} {
            file rename -force $workspace/$revtmpdir/probcli $workspace/$revdir
        }
        if {[file exists $workspace/$revtmpdir/probcli.exe]} {
            file rename -force $workspace/$revtmpdir/probcli.exe $workspace/$revdir
        }
        file rename -force $workspace/$revtmpdir/lib $workspace/$revdir
        # for csp copy parseCspForPl from lib_platform_specific to lib
        if [string match $os mac] {
            file copy -force $workspace/$revtmpdir/lib_platform_specific/macos_intel/parseCspForPl $workspace/$revdir/lib/parseCspForPl
        } elseif [string match $os linux] {
            #no differece between linux and linux_64?
            file copy -force $workspace/$revtmpdir/lib_platform_specific/linux/parseCspForPl $workspace/$revdir/lib/parseCspForPl
        } elseif [string match $os win] {
            file copy -force $workspace/$revtmpdir/lib_platform_specific/windows/parsecspforpl.exe $workspace/$revdir/lib/parsecspforpl.exe
        }

    } ] } {
                    checkout_errmsg "Building was not successful."
                    if {!$notempdelete} {deleteDir "$workspace/$revdir"}
                    exit -1
    }
    if {!$notempdelete} {deleteDir "$workspace/$revtmpdir"}


    if {[string match $os win]} {
        set probclinew "$workspace/$revdir/probcli.exe"
    } else {
        set probclinew "$workspace/$revdir/probcli"
    }
    if {![file exists $probclinew]} {
        checkout_errmsg "Building was not successful (probcli file does not exist: $probclinew)."
        #puts "building was not successful"
        if {!$notempdelete} {deleteDir "$workspace/$revdir"}
        exit -1
    }

    cd $workspace

    set oldworkspaceini ""
    if { [catch {
            if {[file exists $workspaceini]} {
                set f [open $workspaceini r]
                set oldworkspaceini [split [read $f] "\n"]
                close $f
                if {[lsearch $oldworkspaceini ${getrev}*]<0} {
                    #add revision information to workspaceini
                    set f [open $workspaceini a]
                    puts $f "$getrev (date: ${getrevdate})"
                    close $f
                }
            } else {
                set f [open $workspaceini w]
                puts $f "$getrev (date: ${getrevdate})"
                close $f
            }
    } ] } {
        checkout_errmsg "Could not write to $workspaceini.\n\nPlease check permissions."
        exit -1
    }

    if {!$silentrun} {
        tk_messageBox -icon info -message "Revision $getrev was successfully checked out and can be used with revbench.tcl." -type ok -parent .checkoutwnd
    } else {
        putsTxt "Revision $getrev was successfully checked out."
    }
    return $getrev
}




############################# GIT-CHECKOUT ##########################################
proc checkout_runall_GIT {getrev fromsource termcmd gitcmd makecmd gitfromdate gittodate reposdirname} {
    #use EITHER getrev: get a specific revision (and gitfromdate=0, gittodate=0)
    #OR (gitfromdate AND gittodate): get the newest revision within the choosen dates (and getrev=0)
    #OR only gittodate: get the newest revision valid on that date (and getrev=0, gitfromdate=0)
    #(use 0 for non-choosen options)
    #date format: {YYYY-MM-DD} (including {})
    eval global {workspace workspaceini os silentrun sourcedir}

    cd $workspace


    if [string match $os win] {
        set exportcmd "set"
        set trenner " && "
    } else {
        set exportcmd "export"
        set trenner ";"
    }


    set ii 0
    set cmd01 ""
    set cmd02 ""
    set cmd03 ""
    set cmd04 ""
    set cmd05 ""
    set cmd06 ""

    if [string length $termcmd]>0 {
        foreach {teil} [split $termcmd " "] {
            incr ii 1
            switch -- $ii {
                1 { set cmd01 $teil }
                2 { set cmd02 $teil }
                3 { set cmd03 $teil }
                4 { set cmd04 $teil }
                5 { set cmd05 $teil }
                6 { set cmd06 $teil }
                7 { checkout_errmsg "Too many parameters for terminal command."
                    exit -1
                  }
            }
        }
    }

    #git@cobra.cs.uni-duesseldorf.de:prob/prob_prolog.git
    set revtmpdir "revTEMP/tmp"
    set exportdir "PROB_HOME=$workspace/$revtmpdir/$reposdirname"

    if { [catch { deleteDir "$workspace/$revtmpdir"; file mkdir $workspace/$revtmpdir } ] } {
                    checkout_errmsg "Could not write to workspace $workspace.\nPlease check permissions."
                    cancelcheckout
                    return
    }
    cd $workspace/$revtmpdir
    if {$gittodate==0} {
      set shellskript "echo clone repository ${fromsource}...$trenner \
                cd $workspace/$revtmpdir$trenner \
                $gitcmd clone $fromsource$trenner \
                cd $workspace/$revtmpdir/$reposdirname$trenner \
                echo checkout develop$trenner \
                $gitcmd checkout develop$trenner \
                echo getting repository information from ${fromsource}...$trenner \
                $gitcmd log --pretty=format:\"<revision=\"%H\"\><date>%ci</date>\" --date=iso > $workspace/lastrevinfo.xml$trenner\
        sleep 1$trenner \
                echo Finished."
    } else {
     if {$gitfromdate==0} {
      set shellskript "echo clone repository ${fromsource}...$trenner \
                cd $workspace/$revtmpdir$trenner \
                $gitcmd clone $fromsource$trenner \
                cd $workspace/$revtmpdir/$reposdirname$trenner \
                echo checkout develop$trenner \
                $gitcmd checkout develop$trenner \
                echo getting repository information from ${fromsource}...$trenner \
                $gitcmd log --pretty=format:\"<revision=\"%H\"\><date>%ci</date>\" --date=iso --until=\@${gittodate} > $workspace/lastrevinfo.xml$trenner\
        sleep 1$trenner \
                echo Finished."
     } else {
      set shellskript "echo clone repository ${fromsource}...$trenner \
                cd $workspace/$revtmpdir$trenner \
                $gitcmd clone $fromsource$trenner \
                cd $workspace/$revtmpdir/$reposdirname$trenner \
                echo checkout develop$trenner \
                $gitcmd checkout develop$trenner \
                echo getting repository information from ${fromsource}...$trenner \
                $gitcmd log --pretty=format:\"<revision=\"%H\"\><date>%ci</date>\" --date=iso --since=\@${gitfromdate} --until=\@${gittodate} > $workspace/lastrevinfo.xml$trenner\
        sleep 1$trenner \
                echo Finished."
      }
    }
    #all relevant informations in lastrevinfo.xml are in the same format as svn info xml now (EXCEPT different date format)



    if { [catch {
            switch -- $ii {
                0  { puts [exec -- $shellskript] }
                1  { puts [exec -- $cmd01 $shellskript] }
                2  { puts [exec -- $cmd01 $cmd02 $shellskript] }
                3  { puts [exec -- $cmd01 $cmd02 $cmd03 $shellskript] }
                4  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $shellskript] }
                5  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $cmd05 $shellskript] }
                6  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $cmd05 $cmd06 $shellskript]}
                default { checkout_errmsg "Too many parameters for command."
                          exit -1
                        }
            }
        } ] } {
                    checkout_errmsg "An error occurred.\n\nErrorInfo:\n$::errorInfo"
                    cancelcheckout
                    return
    }

    if {![file exists $workspace/lastrevinfo.xml]} {
                    checkout_errmsg "Could not write to workspace $workspace.\nPlease check permissions."
                    cancelcheckout
                    return
    }

    if {[catch {
            set f [open $workspace/lastrevinfo.xml]
            set lastrevinfo [read $f]; #[split [read $f] "\n"]
            close $f
    }]} {
            checkout_errmsg "Could not read from workspace.\nPlease check permissions."
            cancelcheckout
            return
    }


    if [string first "-" $getrev]>-1 {
            set foundrevdate -1
            set getrevdate $getrev
            set getrevdate [string trim $getrev " \n{}"]
            #vv: putsTxt "GETREVDATE: $getrevdate"
            #set getrevdate "2013-03-04"
            #set pos [string first "revision=\"" $lastrevinfo]
            #if $pos>-1 {
            #    set foundrevnr 1
            #    set part [string range $lastrevinfo [expr {$pos+10}] end]
            #    set getrev [string trim "[string range $part 0 [expr {[string first "\"" $part] - 1}]]" " \n"]
            #}
            #set pos [string first "\<date\>" $lastrevinfo]
            #if $pos>-1 {
            #    set foundrevdate 1
            #    set part [string range $lastrevinfo [expr {$pos+6}] end]
            #    set getrevdate [string trim "[string range $part 0 [expr {[string first "\<\/date\>" $part] - 1}]]" " \n"]
            #}

            set pos [string first "\<date\>$getrevdate" $lastrevinfo]

            if $pos>-1 {
                set foundrevdate 1
                #vv: putsTxt "FOUNDREVDATE $pos"
                set part [string range $lastrevinfo [expr {$pos - 52}] [expr {$pos + 25 + 13}]]; #column <revision=...><date>...</date>
                #vv: putsTxt $part
                set getrev [string range $part [expr {[string first "revision=" $part] + 9}] [expr {[string first ">" $part [expr {[string first "revision=" $part]}] ] - 1}]  ]; #extract the revision number (40 characters long)
                #vv: putsTxt "REV=***${getrev}***"

                #set getrevdate [string trim "[string range $part 0 [expr {[string first "\<\/date\>" $part] - 1}]]" " \n"]
                #putsTxt $getrevdate
                putsTxt "REV=$getrev"
                putsTxt "REVDATE=***${getrevdate}***"
            }
    } elseif [string first "develop" $getrev]>-1 {
      set foundrevdate 0
            set pos [string first "\<revision=" $lastrevinfo]

            if $pos>-1 {
                #set foundrev 1
                putsTxt "FOUNDREV $pos"
                set part [string range $lastrevinfo [expr {$pos - 0}] [expr {$pos + 52 + 25 + 13}]]; #column <revision=...><date>...</date>
                putsTxt $part
                #set getrevdate [string range $part [expr {[string first "\<date\>" $part] + 6}] [expr {[string first "\<\/date\>" $part [expr {[string first "\<date\>" $part]}] ] - 1}]  ]; #extract the revision number (40 characters long)
                set getrevdate [string range $part [expr {[string first "\<date\>" $part] + 6}] [expr {[string first "\<date\>" $part] + 6 + 9}]  ]; #extract the revision number (40 characters long)
                #vv putsTxt "REVDATE=***${getrevdate}***"
                set getrev [string range $part [expr {[string first "revision=" $part] + 9}] [expr {[string first ">" $part [expr {[string first "revision=" $part]}] ] - 1}]  ]; #extract the revision number (40 characters long)
            }
                putsTxt "REV=$getrev"
                putsTxt "REVDATE=***${getrevdate}***"
    } else {
      set foundrevdate 0
            set pos [string first "\<revision=${getrev}" $lastrevinfo]

            if $pos>-1 {
                #set foundrev 1
                putsTxt "FOUNDREV $pos"
                set part [string range $lastrevinfo [expr {$pos - 0}] [expr {$pos + 52 + 25 + 13}]]; #column <revision=...><date>...</date>
                putsTxt $part
                #set getrevdate [string range $part [expr {[string first "\<date\>" $part] + 6}] [expr {[string first "\<\/date\>" $part [expr {[string first "\<date\>" $part]}] ] - 1}]  ]; #extract the revision number (40 characters long)
                set getrevdate [string range $part [expr {[string first "\<date\>" $part] + 6}] [expr {[string first "\<date\>" $part] + 6 + 9}]  ]; #extract the revision number (40 characters long)
                #vv putsTxt "REVDATE=***${getrevdate}***"
            }
                putsTxt "REV=$getrev"
                putsTxt "REVDATE=***${getrevdate}***"
    }

    deleteDir "$workspace/lastrevinfo.xml"

    if $foundrevdate<0 {
      #looked for revision date, but not found
                    checkout_errmsg "No revision found with date $getrevdate"
                    cancelcheckout
                    return

    }

    #set revtmpdir "rev$getrev/tmp"
    set revdir "rev$getrev"
    #set exportdir "PROB_HOME=$workspace/$revtmpdir"

    if { [catch { deleteDir "$workspace/$revdir"; file mkdir $workspace/$revdir } ] } {
                    checkout_errmsg "Could not write to workspace $workspace.\nPlease check permissions."
                    cancelcheckout
                    return
    }


    if [string match $os win] {
        set copycmd "xcopy "
        set copyparam " /E "
    } else {
        set copycmd "cp "
        set copyparam " -r "
    }

    puts "from: $fromsource\nsourcedir: ${sourcedir}\ngetrev: $getrev\ngitcmd: $gitcmd\ntrenner: $trenner\ncopycmd: $copycmd\ncopyparam: $copyparam\nexportcmd: $exportcmd\nexportdir: $exportdir\nmakecmd: $makecmd\nworkspace: $workspace\nreposdirname: $reposdirname"



    if [string match $getrev develop] {
        set shellskript "echo check out revision $getrev...$trenner \
                cd $workspace/$revtmpdir/$reposdirname$trenner \
                $gitcmd checkout $getrev$trenner \
                cd $workspace/$revtmpdir/$reposdirname$trenner \
                echo setting PROB_HOME...$trenner \
                $exportcmd $exportdir$trenner \
                echo running make$trenner \
                $makecmd$trenner \
                echo Finished."
        #sleep 17$trenner \
        #        echo Finished."
    } else {
        set shellskript "echo check out revision $getrev...$trenner \
                cd $workspace/$revtmpdir/$reposdirname$trenner \
                $gitcmd checkout $getrev$trenner \
                cd $workspace/$revtmpdir/$reposdirname$trenner \
                $exportcmd $exportdir$trenner \
                echo running make$trenner \
                $makecmd$trenner \
                $gitcmd checkout develop$trenner \
                echo Finished."
        #set shellskript "echo check out revision $getrev...$trenner \
        #        cd $workspace/$revtmpdir/$reposdirname$trenner \
        #        $gitcmd checkout $getrev$trenner \
        #echo WAITING....$trenner \
        #sleep 7$trenner\
        #        cd $workspace/$revtmpdir/$reposdirname$trenner \
        #        $exportcmd $exportdir$trenner \
        #        echo running make$trenner \
        #        $makecmd$trenner \
        #        $gitcmd checkout develop$trenner \
        #echo FINISHED**************************************************************************************$trenner \
        #sleep 17$trenner \
        #        echo Finished."




#        set shellskript "echo check out revision $getrev...$trenner \
#                cd $workspace/$revtmpdir/$reposdirname$trenner \
#                $gitcmd checkout develop$trenner \
#                $exportcmd $exportdir$trenner \
#                echo running make$trenner \
#                $makecmd$trenner \
#                $gitcmd checkout -f $getrev$trenner \
#       echo WAITING....$trenner \
#       sleep 17$trenner\
#                cd $workspace/$revtmpdir/$reposdirname$trenner \
#                $exportcmd $exportdir$trenner \
#                echo running make$trenner \
#                $makecmd$trenner \
#                $gitcmd checkout develop$trenner \
#        echo FINISHED**************************************************************************************$trenner \
#        sleep 17$trenner \
#                echo Finished."
    }
                #nach makecmd: # sicstus$trenner \





    if { [catch {
            switch -- $ii {
                0  { puts [exec -- $shellskript] }
                1  { puts [exec -- $cmd01 $shellskript] }
                2  { puts [exec -- $cmd01 $cmd02 $shellskript] }
                3  { puts [exec -- $cmd01 $cmd02 $cmd03 $shellskript] }
                4  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $shellskript] }
                5  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $cmd05 $shellskript] }
                6  { puts [exec -- $cmd01 $cmd02 $cmd03 $cmd04 $cmd05 $cmd06 $shellskript]}
                default { checkout_errmsg "Too many parameters for command."
                            cancelcheckout
                            return
                        }
            }
    } ] } {
                    checkout_errmsg "An error occurred.\n\nErrorInfo:\n$::errorInfo"
                    #puts "error: $::errorInfo"
                    cancelcheckout
                    return
    }


    if { [catch {
        if {[file exists $workspace/$revtmpdir/$reposdirname/probcli]} {
            file rename -force $workspace/$revtmpdir/$reposdirname/probcli $workspace/$revdir
        }
        if {[file exists $workspace/$revtmpdir/$reposdirname/probcli.exe]} {
            file rename -force $workspace/$revtmpdir/$reposdirname/probcli.exe $workspace/$revdir
        }
    } ] } {
                    checkout_errmsg "Building was not successful (probcli does not exist)."
                    deleteDir "$workspace/$revtmpdir"
                    deleteDir "$workspace/$revdir"
                    exit -1
    }
    if { [catch {
        file rename -force $workspace/$revtmpdir/$reposdirname/lib $workspace/$revdir
        # for csp copy parseCspForPl from lib_platform_specific to lib
        if [string match $os mac] {
            #file copy -force $workspace/$revtmpdir/$reposdirname/lib_platform_specific/macos_intel/parseCspForPl $workspace/$revdir/lib/parseCspForPl
            file copy -force $workspace/$revtmpdir/$reposdirname/lib_platform_specific/macos_intel/ $workspace/$revdir/lib
        } elseif [string match $os linux] {
            #evntl linux und linux_64 unterscheiden?
            #file copy -force $workspace/$revtmpdir/$reposdirname/lib_platform_specific/linux/parseCspForPl $workspace/$revdir/lib/parseCspForPl
            #file copy -force $workspace/$revtmpdir/$reposdirname/lib_platform_specific/linux/* $workspace/$revdir/lib
            file copy -force $workspace/$revtmpdir/$reposdirname/lib_platform_specific/linux_64/ $workspace/$revdir/lib
        } elseif [string match $os win] {
            #file copy -force $workspace/$revtmpdir/$reposdirname/lib_platform_specific/windows/parsecspforpl.exe $workspace/$revdir/lib/parsecspforpl.exe
            file copy -force $workspace/$revtmpdir/$reposdirname/lib_platform_specific/windows/ $workspace/$revdir/lib
        }

    } ] } {
                    checkout_errmsg "Building probcli was probably successful but could not find the library directory (os: $os)."
                    deleteDir "$workspace/$revtmpdir"
                    deleteDir "$workspace/$revdir"
                    exit -1
    }
    deleteDir "$workspace/$revtmpdir"


    if {[string match $os win]} {
        set probclinew "$workspace/$revdir/probcli.exe"
    } else {
        set probclinew "$workspace/$revdir/probcli"
    }
    if {![file exists $probclinew]} {
        checkout_errmsg "Building was not successful (probcli file does not exist: $probclinew)."
        #puts "building was not successful"
        deleteDir "$workspace/$revdir"
        exit -1
    }

    cd $workspace

    set oldworkspaceini ""
    if { [catch {
            if {[file exists $workspaceini]} {
                set f [open $workspaceini r]
                set oldworkspaceini [split [read $f] "\n"]
                close $f
                if {[lsearch $oldworkspaceini ${getrev}*]<0} {
                    #add revision information to workspaceini
                    set f [open $workspaceini a]
                    puts $f "$getrev (date: ${getrevdate})"
                    close $f
                }
            } else {
                set f [open $workspaceini w]
                puts $f "$getrev (date: ${getrevdate})"
                close $f
            }
    } ] } {
        checkout_errmsg "Could not write to $workspaceini. Please check permissions."
        exit -1
    }

    if {!$silentrun} {
        tk_messageBox -icon info -message "Revision $getrev was successfully checked out and can be used with revbench.tcl." -type ok -parent .checkoutwnd
    } else {
        putsTxt "Revision $getrev was successfully checked out."
    }
    return $getrev
}
############################# END OF GIT-CHECKOUT ###################################




proc checkout_errmsg {msg} {
            eval global {silentrun}
            if {!$silentrun} {
                    tk_messageBox -icon error -title "error" -message $msg -type ok -parent .checkoutwnd
            } else {
                    putsTxt $msg
            }
}

proc cancelcheckout {} {
        eval global {homedir argv0}
        cd $homedir; cd [file dirname $argv0]; # change to script directory
#        readINI 0
#        showstart
#        pack forget .checkoutwnd
#        destroy .checkoutwnd
}



proc checkout_showdiag {} {
    eval global {workspace os}


    global xsource
    global xsourcetype
    global xmake
    global xterm
    global xsvn
    global xrev
    global xreposdirname; #name of the root directory of the repository (prob_prolog)
    eval global {sourcedir sourcedirSVN sourcedirGIT}

    #set xsource "https://cobra.cs.uni-duesseldorf.de/prob/"
    set xsource "git@cobra.cs.uni-duesseldorf.de:prob/prob_prolog.git"

    #set xsourcetype "svn"; #either svn or git
    set xsourcetype "git"; #either svn or git


    if [string match $os win] {
        set xmake "Make_probcli_Win.bat"
        set xterm "cmd /k"
    } else {
        set xmake "make"
        set xterm "xterm -l -e"
    }
    set xsvn $xsourcetype; #either svn or git
    set xrev ""
    set xreposdirname "prob_prolog"; #root directory of repository

    frame .checkoutwnd.work
    frame .checkoutwnd.middle
    frame .checkoutwnd.middle.work1
    frame .checkoutwnd.middle.work2
    frame .checkoutwnd.menu

    pack .checkoutwnd.work -side top -fill both
    pack .checkoutwnd.middle -side top -fill both
    pack .checkoutwnd.middle.work1 -side left
    pack .checkoutwnd.middle.work2 -side right -expand yes -fill both
    pack .checkoutwnd.menu -side bottom -fill both

    button .checkoutwnd.menu.weiter -text "Check out revision" -command {
        if {[string length $xrev] == 0} {
            tk_messageBox -icon error -title "error" -message "Please specify the revision." -type ok -parent .checkoutwnd
        } elseif {[string length $xmake] == 0} {
            tk_messageBox -icon error -title "error" -message "Please specify the build command (i.e. \"make\")." -type ok -parent .checkoutwnd
        } elseif {[string length $xsource] == 0} {
            tk_messageBox -icon error -title "error" -message "Please specify the repository." -type ok -parent .checkoutwnd
        } elseif {(![string match $xsourcetype svn] && ![string match $xsourcetype git])} {
            tk_messageBox -icon error -title "error" -message "Please specify the repository type." -type ok -parent .checkoutwnd
        } elseif {([string match $xsourcetype git] && [string length $xreposdirname] == 0)} {
            tk_messageBox -icon error -title "error" -message "Please specify the name of the root directory (i.e. prob_prolog)." -type ok -parent .checkoutwnd
        } else {
            if [string match $os win] { console show }
            pack forget .checkoutwnd.work
            pack forget .checkoutwnd.middle
            pack forget .checkoutwnd.menu.weiter
            pack forget .checkoutwnd.menu.back
            pack forget .checkoutwnd.menu.ende

            frame .checkoutwnd.runinfo
            label .checkoutwnd.runinfo.lrun -text "Getting and building revision.\nThis may take several minutes..."
            pack .checkoutwnd.runinfo -fill both -expand yes
            pack .checkoutwnd.runinfo.lrun -expand yes -fill both -padx 10 -pady 10
            update
            set xsvn $xsourcetype; #either svn or git
            if [string match $xsourcetype svn] {
                set sourcedir $sourcedirSVN
                checkout_runall $xrev $xsource $xterm $xsvn $xmake
            } elseif [string match $xsourcetype git] {
                set sourcedir $sourcedirGIT
                checkout_runall_GIT $xrev $xsource $xterm $xsvn $xmake 0 0 $xreposdirname
            } else {
              tk_messageBox -icon error -title "error" -message "Error: unknown repository type." -type ok -parent .checkoutwnd
              eval global {homedir}
              cd $homedir; cd [file dirname $argv0]; # change to script directory
              readINI 0
              #showstart
              pack forget .checkoutwnd
              destroy .checkoutwnd
              return
            }
            #frame .checkoutwnd.runinfo
            #label .checkoutwnd.runinfo.lrun -text "Checking out revision $xrev from $xsource.\nThis may take several minutes..."
            #pack .checkoutwnd.runinfo -fill both -expand yes
            #pack .checkoutwnd.runinfo.lrun -expand yes -fill both -padx 10 -pady 10
            #svnco $xrev $xsource $xterm $xsvn
            #.runinfo.lrun config -text "Running \"$xmake\"...\nThis may take several minutes..."
            #runmake $xrev $xmake $xterm
            puts "Finished."
            eval global {homedir}
            cd $homedir; cd [file dirname $argv0]; # change to script directory
            readINI 0
            #showstart
            pack forget .checkoutwnd
            destroy .checkoutwnd
        }
    }
    button .checkoutwnd.menu.back -text " Back to Main Menu " -command {
        eval global {homedir}
        cd $homedir; cd [file dirname $argv0]; # change to script directory
        readINI 0
        #showstart
        pack forget .checkoutwnd
        destroy .checkoutwnd
    }

    label .checkoutwnd.work.ltop -text "Please specify the revision. It will be checked out from repository, built and stored in workspace.\
                             \n\"Revision\" may be the number of a revision, a date written as \{YYYY-MM-DD\} or HEAD."

    label .checkoutwnd.middle.work1.lrev -text "Revision:"
    entry .checkoutwnd.middle.work2.erev -textvariable xrev

    label .checkoutwnd.middle.work1.lsource -text "Repository Root:"
    entry .checkoutwnd.middle.work2.esource -textvariable xsource

    frame .checkoutwnd.middle.work2.esourcetypefield
    label .checkoutwnd.middle.work1.lsourcetype -text "Repository Type:"
    radiobutton .checkoutwnd.middle.work2.esourcetypefield.esourcetypesvn -variable xsourcetype -value svn -text "Subversion"
    radiobutton .checkoutwnd.middle.work2.esourcetypefield.esourcetypegit -variable xsourcetype -value git -text "Git"
    pack .checkoutwnd.middle.work2.esourcetypefield.esourcetypesvn .checkoutwnd.middle.work2.esourcetypefield.esourcetypegit -side left -padx 3 -pady 0 -fill x

    label .checkoutwnd.middle.work1.lmake -text "Build-command:"
    entry .checkoutwnd.middle.work2.emake -textvariable xmake

    label .checkoutwnd.middle.work1.lterm -text "Terminal-command:"
    entry .checkoutwnd.middle.work2.eterm -textvariable xterm

    label .checkoutwnd.middle.work1.lworkspace -text "Workspace:"
    label .checkoutwnd.middle.work2.eworkspace -anchor w -text $workspace

    pack .checkoutwnd.work.ltop -padx 5 -pady 10 -expand yes -side top

    pack .checkoutwnd.middle.work1.lrev .checkoutwnd.middle.work1.lsource .checkoutwnd.middle.work1.lsourcetype .checkoutwnd.middle.work1.lmake .checkoutwnd.middle.work1.lterm .checkoutwnd.middle.work1.lworkspace -side top -padx 3 -pady 3
    pack .checkoutwnd.middle.work2.erev .checkoutwnd.middle.work2.esource .checkoutwnd.middle.work2.esourcetypefield .checkoutwnd.middle.work2.emake .checkoutwnd.middle.work2.eterm .checkoutwnd.middle.work2.eworkspace -side top -expand yes -padx 3 -pady 3 -fill x

    #button .checkoutwnd.menu.ende -text "        Exit        " -command exit
    #pack .checkoutwnd.menu.weiter .checkoutwnd.menu.back .checkoutwnd.menu.ende -pady 10 -padx 10 -side left -expand yes -fill x
    pack .checkoutwnd.menu.weiter .checkoutwnd.menu.back -pady 10 -padx 10 -side left -expand yes -fill x

}






####################################################################################
# under construction #
####################################################################################
proc specialCheckoutTest {} {
    eval global {tcl_platform argv argv0 inputfileslist revslist resfileslist probrev probrev2 uselocal useeveryXrevisionINPUT}

    global probpath
    set probpath ""


    #pack [frame .startwnd] -fill both -expand yes -anchor nw -side top


    eval global {workspace xinput xrepeatings xtimeout xspecialopts xusedrev xusedrevB xuseresfile xlocalpath os}
    set uselocal 0
    #checkout
    global xsource
    global xsourcetype
    global xmake
    global xterm
    global xsvn
    global xrev
    global xreposdirname
<<<<<<< HEAD
    #set xsource "git@cobra.cs.uni-duesseldorf.de:prob/prob-prolog.git"
=======
    #set xsource "git@cobra.cs.uni-duesseldorf.de:prob/prob_prolog.git"
>>>>>>> 1.4.0
    set xsource "https://cobra.cs.uni-duesseldorf.de/prob/"
    set xsourcetype "svn"; #either svn or git
    if [string match $os win] {
        set xmake "Make_probcli_Win.bat"
        set xterm "cmd /k"
    } else {
        set xmake "make"
        set xterm "xterm -l -e"
    }
    set xsvn $xsourcetype; #either svn or git
    set xrev ""
    set xreposdirname "prob_prolog"


    frame .specialtestswnd.work
    frame .specialtestswnd.probframe
    frame .specialtestswnd.middle
    frame .specialtestswnd.middle.work1
    frame .specialtestswnd.middle.work2
    frame .specialtestswnd.menu1
    frame .specialtestswnd.menu1.top
    frame .specialtestswnd.menu1.bottom
    frame .specialtestswnd.infospecialframe
    frame .specialtestswnd.resframe
    frame .specialtestswnd.menu2

    pack .specialtestswnd.work -side top -fill both
    pack .specialtestswnd.probframe -side top -fill both
    pack .specialtestswnd.middle -side top -fill both
    pack .specialtestswnd.middle.work1 -side left -fill y
    pack .specialtestswnd.middle.work2 -side right -expand yes -fill both
    pack .specialtestswnd.infospecialframe -side top -fill both
    pack .specialtestswnd.menu1 -side top -fill both
    pack .specialtestswnd.menu1.top -side left -expand yes -fill both
    pack .specialtestswnd.menu1.bottom -side right -expand yes -fill both
    pack .specialtestswnd.resframe -side top -fill both
    pack .specialtestswnd.menu2 -side bottom -fill both

    button .specialtestswnd.menu1.top.weiter -text "Run tests" -command {
        if {([string length $xusedrev] == 0) || ([string length $xusedrevB] == 0)} {
            tk_messageBox -icon warning -title "error" -message "Please specify the revisions" -type ok -parent .specialtestswnd
        } elseif {[string length $xinput] == 0} {
            tk_messageBox -icon warning -title "error" -message "Please specify the input file." -type ok -parent .specialtestswnd
        } elseif {[string length $xrepeatings] == 0} {
            tk_messageBox -icon warning -title "error" -message "Please specify the number of repeatings." -type ok -parent .specialtestswnd
        } elseif {[containsPartNCS $xspecialopts "--timeout"]} {
            tk_messageBox -icon warning -title "error" -message "Please set a value for \"timeout for probcli (ms)\" instead of using --timeout option." -type ok -parent .specialtestswnd
        } else {


            if [string match $os win] { console show }
            pack forget .specialtestswnd.work
            pack forget .specialtestswnd.middle
            pack forget .specialtestswnd.menu.weiter
            pack forget .specialtestswnd.menu.back
            pack forget .specialtestswnd.menu.ende
            pack forget .specialtestswnd.infospecialframe
            pack forget .specialtestswnd.probframe
            pack forget .specialtestswnd.menu1.top
            pack forget .specialtestswnd.menu1.bottom.ende

            frame .specialtestswnd.runinfo
            label .specialtestswnd.runinfo.lrun -text "Getting and building revisions ${xusedrev} to ${xusedrevB}.\nThis may take several minutes..."
            pack .specialtestswnd.runinfo -fill both -expand yes
            pack .specialtestswnd.runinfo.lrun -expand yes -fill both -padx 10 -pady 10

            if {$xusedrev > $xusedrevB} {
                set dummy $xusedrev
                set xusedrev $xusedrevB
                set xusedrevB $dummy
            }



            set silentrun 1
            update
            if [string match $xsourcetype svn] {
                 for {set i $xusedrev} {$i <= $xusedrevB} {incr i $incrsteps} {
                     if [file exists $workspace/rev$i/probcli] {
                       putsTxt "revision $i found in workspace"
                     } else {
                       putsTxt "getting and building revision: $i"
                       eval global {sourcedir sourcedirSVN}
                       set sourcedir $sourcedirSVN
                       checkout_runall $i $xsource $xterm $xsvn $xmake
                     }
                 }
                 if ![file exists $workspace/rev$xusedrevB/probcli] {
                   #use last revision of range in every case
                   putsTxt "getting and building revision: $xusedrevB"
                   #checkout_runall $xusedrevB $xsource $xterm $xsvn $xmake
                 }
            } elseif [string match $xsourcetype git] {
                 for {set i $xusedrev} {$i <= $xusedrevB} {incr i $incrsteps} {
                     if [file exists $workspace/rev$i/probcli] {
                       putsTxt "revision $i found in workspace"
                     } else {
                       putsTxt "getting and building revision: $i"
                       eval global {sourcedir sourcedirGIT}
                       set sourcedir $sourcedirGIT
                       checkout_runall_GIT $i $xsource $xterm $xsvn $xmake 0 0 $xreposdirname
                     }
                 }
                 if ![file exists $workspace/rev$xusedrevB/probcli] {
                   #use last revision of range in every case
                   putsTxt "getting and building revision: $xusedrevB"
                   eval global {sourcedir sourcedirGIT}
                   set sourcedir $sourcedirGIT
                   checkout_runall_GIT $xusedrevB $xsource $xterm $xsvn $xmake 0 0 $xreposdirname
                 }
            } else {
              tk_messageBox -icon error -title "error" -message "Error: unknown repository type." -type ok -parent .specialtestswnd
              eval global {homedir}
              cd $homedir; cd [file dirname $argv0]; # change to script directory
              readINI 0
              #showstart
              pack forget .specialtestswnd
              destroy .specialtestswnd
              return
            }
            #frame .checkoutwnd.runinfo
            #label .checkoutwnd.runinfo.lrun -text "Checking out revision $xrev from $xsource.\nThis may take several minutes..."
            #pack .checkoutwnd.runinfo -fill both -expand yes
            #pack .checkoutwnd.runinfo.lrun -expand yes -fill both -padx 10 -pady 10
            #svnco $xrev $xsource $xterm $xsvn
            #.runinfo.lrun config -text "Running \"$xmake\"...\nThis may take several minutes..."
            #runmake $xrev $xmake $xterm
            puts "Revisions checked out."
            eval global {homedir}
            cd $homedir; cd [file dirname $argv0]; # change to script directory
            #######################end of checkout
            readINI 0
            set silentrun 0

            #########################perform tests
            if {[string length $xtimeout] == 0} {set xtimeout 0}
            #set xuseresfile "[string range $xinput 0 [string last "." $xinput]]res"
            set xuseresfile "[string range $xinput 0 [string last "." $xinput]]performancetests.res"
            if [file exists $workspace/$xuseresfile] {
              set xnr 0
              while {[file exists "$workspace/$xuseresfile.$xnr"]} {incr xnr 1 }
              file rename -force  "$workspace/$xuseresfile" "$workspace/$xuseresfile.$xnr"
              #puts $xnr
            }

            set resfile $xuseresfile

            #if {[string length $xusedrev] > 0} {
              # use probcli from workspace (checked out revisions)
              set uselocal 0
              if {[string first " " [string trim $xusedrev " "]]>-1} {
                set probrev [string trim "[string range $xusedrev 0 [string first " " $xusedrev]]" " ()"]
              } else {
                set probrev [string trim $xusedrev " ()"]
              }
            #} else {
              # use probcli from local path
            #  set probrev $xlocalpath
            #  set uselocal 1
            #}
            set probrev2 0
            if {[string length $xusedrevB] > 0} {
                if {[string first " " [string trim $xusedrevB " "]]>-1} {
                    set probrev2 [string trim "[string range $xusedrevB 0 [string first " " $xusedrevB]]" " ()"]
                } else {
                    set probrev2 [string trim $xusedrevB " ()"]
                }
            }
            destroy .specialtestswnd
            destroy .startwnd

            wm geometry . 1x1+[expr {([winfo screenwidth .] / 2)}]+[expr {([winfo screenheight .] / 2)}]
            toplevel .testwindow
            wm geometry .testwindow 800x600+[expr {[winfo screenwidth .]/2 - 400}]+[expr {[winfo screenheight .]/2 - 300}]
            wm transient .testwindow .
            set_runtests_window .testwindow

            if {$uselocal} {
              set probpath $probrev
            } else {
              set probpath $workspace/rev$probrev
            }
            prepare_runtests .testwindow $probrev $probrev2 0 0 $useeveryXrevisionINPUT
        }
    }
    #button .specialtestswnd.menu1.bottom.ende -text "Exit" -command exit
    button .specialtestswnd.menu1.bottom.ende -text " Back to main menu " -command {
         pack forget .specialtestswnd
         destroy .specialtestswnd
        }


    label .specialtestswnd.work.ltop -text "Find performance differences within a range of revisions.\n \
         Please choose input file, range of revisions and options. \
         "

    pack [label .specialtestswnd.infospecialframe.labelspec -text "*all valid probcli options (i.e. -mc 1000) may be used here.\n \
         *Warning: Machine specific options for probcli defined in input file will be ignored!"] -padx 5 -pady 2 -expand yes -side top


    global incrsteps
    set incrsteps 1
    ttk::labelframe .specialtestswnd.probframe.probS -text "Range of revisions"
    label .specialtestswnd.probframe.probS.lbl1 -text "between:" -pady 3
    set xusedrev ""
    set xusedrevB ""
    entry .specialtestswnd.probframe.probS.erev -textvariable xusedrev
    label .specialtestswnd.probframe.probS.lbl2 -text "and:" -pady 3
    entry .specialtestswnd.probframe.probS.erev2 -textvariable xusedrevB
    label .specialtestswnd.probframe.probS.lbl3 -text "step:" -pady 3
    entry .specialtestswnd.probframe.probS.erev3 -textvariable incrsteps
    pack .specialtestswnd.probframe.probS -side top -pady 10 -padx 10 -expand yes -fill x
    pack .specialtestswnd.probframe.probS.lbl1 -pady 10 -padx 10 -side left
    pack .specialtestswnd.probframe.probS.erev -pady 10 -padx 10 -expand yes -side left -fill x
    pack .specialtestswnd.probframe.probS.lbl2 -pady 10 -padx 10 -side left
    pack .specialtestswnd.probframe.probS.erev2 -pady 10 -padx 10 -expand yes -side left -fill x
    pack .specialtestswnd.probframe.probS.lbl3 -pady 10 -padx 10 -side left
    pack .specialtestswnd.probframe.probS.erev3 -pady 10 -padx 10 -expand yes -side left -fill x


    label .specialtestswnd.middle.work1.lsource -text "Input file:" -pady 3
    ttk::combobox .specialtestswnd.middle.work2.esource -textvariable xinput -state readonly -values $inputfileslist
    #button .specialtestswnd.middle.work2.esource.btnSource -text ""
    label .specialtestswnd.middle.work1.lsourcemachine -text "Machine:" -pady 3
    set machinelist "old"
    set xinputmachine ""
    ttk::combobox .specialtestswnd.middle.work2.esourcemachine -textvariable xinputmachine -values $machinelist -postcommand [list on_post .specialtestswnd.middle.work2.esourcemachine]
    proc on_post {widget} { eval global {xinput}; $widget configure -values [list $xinput] }
    #proc on_post {widget} { $widget configure -values [list a b c [clock seconds]] }

    #LOCAL probcli
    #ttk::labelframe .specialtestswnd.probframe.probL -text "OR use local probcli"
    #  label .specialtestswnd.probframe.probL.lbl1 -text "Local probcli:" -pady 3
    #entry .specialtestswnd.probframe.probL.edt1 -textvariable xlocalpath
    #button .specialtestswnd.probframe.probL.btn1 -text "Choose path..." -command {
    #    set xlocalpath [input_openprobcli]
    #}
    #pack .specialtestswnd.probframe.probL -side top -pady 10 -padx 10 -expand yes -fill x
    #pack .specialtestswnd.probframe.probL.edt1 -pady 10 -padx 10 -expand yes -side left -fill x
    #pack .specialtestswnd.probframe.probL.btn1 -pady 10 -padx 10 -side right



    label .specialtestswnd.middle.work1.lterm -text "Repeatings:" -pady 3
    spinbox .specialtestswnd.middle.work2.eterm -from 1 -to 100 -validate key -vcmd {string is integer %P} -textvariable xrepeatings

    label .specialtestswnd.middle.work1.ltimeout -text "Timeout for probcli (ms):"
    spinbox .specialtestswnd.middle.work2.etimeout -from 0 -to 100000000 -validate key -vcmd {string is integer %P} -textvariable xtimeout

    label .specialtestswnd.middle.work1.lspezial -text "Options for probcli*:"
    entry .specialtestswnd.middle.work2.espezial -textvariable xspecialopts

    pack .specialtestswnd.work.ltop -padx 5 -pady 10 -expand yes -side top

    #NEW: pack .specialtestswnd.middle.work1.lsource .specialtestswnd.middle.work1.lsourcemachine .specialtestswnd.middle.work1.lterm .specialtestswnd.middle.work1.ltimeout .specialtestswnd.middle.work1.lspezial -side top -padx 3 -pady 3 -fill y
    #NEW: pack .specialtestswnd.middle.work2.esource .specialtestswnd.middle.work2.esourcemachine .specialtestswnd.middle.work2.eterm .specialtestswnd.middle.work2.etimeout .specialtestswnd.middle.work2.espezial -side top -expand yes -padx 3 -pady 3 -fill x
    #OLD:
    pack .specialtestswnd.middle.work1.lsource  .specialtestswnd.middle.work1.lterm .specialtestswnd.middle.work1.ltimeout .specialtestswnd.middle.work1.lspezial -side top -padx 3 -pady 3 -fill y
    pack .specialtestswnd.middle.work2.esource  .specialtestswnd.middle.work2.eterm .specialtestswnd.middle.work2.etimeout .specialtestswnd.middle.work2.espezial -side top -expand yes -padx 3 -pady 3 -fill x
    #end OLD

    #old
    #pack .specialtestswnd.menu1.top.weiter .specialtestswnd.menu1.top.createinput -pady 10 -padx 10 -side top -expand yes -fill x
    #pack .specialtestswnd.menu1.bottom.checkoutrev .specialtestswnd.menu1.bottom.ende -pady 10 -padx 10 -side bottom -expand yes -fill x
    #
    pack .specialtestswnd.menu1.top.weiter .specialtestswnd.menu1.bottom.ende -pady 10 -padx 10 -side bottom -expand yes -fill x


    label .specialtestswnd.menu2.lworkspace -text "Workspace:    $workspace"
    pack .specialtestswnd.menu2.lworkspace -padx 10 -pady 10 -expand yes -fill x

    wm geometry . 600x700+[expr {[winfo screenwidth .]/2 - 275}]+[expr {[winfo screenheight .]/2 - 300}]
}



####################################################################################
####################################################################################


main
