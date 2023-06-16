#!/usr/bin/env tclsh


#  Probench Revision Benchmark tool for ProB
#  autobench only version without GUI



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

    #save results to codespeed server
    global savetocodespeed
    set savetocodespeed 1

    #directory at codespeed server
    global sourcedir
    set sourcedir "trunk/prolog"

    #use local probcli
    global uselocal
    set uselocal 0


    global getrevdate
    set getrevdate ""

    global homedir
    set homedir [pwd]
    cd [file dirname $argv0]

    set arglist [split $argv " "]
    if {[lsearch [string tolower $arglist] -?]>-1||[lsearch [string tolower $arglist] --help]>-1||[lsearch [string tolower $arglist] -help]>-1} {
        puts "ProBench version without GUI for automatic benchmarks\n\npossible parameters:\n\
         -autobench     : performs benchmark from autobench.input and saves the result to a running codespeed server.\n\
         -env <NAME>    : define the environment name (case sensitive) used for codespeed (must be defined in codespeed first!) (default is Cobra)\n\
         -rev <NUMBER>  : define the revision of probench (default is latest revision)\n\
         -path <PATH>   : use probcli in <PATH> (use -rev <NUMBER> to define which revision is in <PATH>)\n\
                          (default is getting from repository and building)\n\
         -silent        : do not save autobench results to a local csv file (only to codespeed server)\n\
         -nocodespeed   : do not save results to the codespeed server
         -noterminal    : do not show xterm terminal during getting and building\n\
         -input <FILE>  : use the given input file instead of autobench.input\n\
         -currentdate   : use current date instead of revision date (revision date is taken from svn)\n\
         -iterations <NUMBER> : number of iterations for benchmarks (default: 2)\n\
         -timeout <MS>  : timeout for probcli (ms), 0=no timeout (default: 0)\n\n\
         To use an repository other than default,\n\
         create a file \"probench.repos\" in workspace directory that contains the repository url.\n\
         Default is https://cobra.cs.uni-duesseldorf.de/prob/\n"
        exit -1
    }



        global silentautobench
        set silentautobench 0; #only with -path: no temporary files; save results only to codespeed server (not csv)

        global veryverbose
        set veryverbose 0; #very verbose: use -veryverbose


        global executeB2TLA
        if {[lsearch [string tolower $arglist] -notla]>-1||[lsearch [string tolower $arglist] --notla]>-1} {
          set executeB2TLA 0; #do not run B2TLA
          puts "B2TLA will not be executed"
        } else {
          set executeB2TLA 1; #run B2TLA, too
        }
        global tlapath
        set tlapath ""



    setvars1

    if {[lsearch [string tolower $arglist] -autorun]>-1||[lsearch [string tolower $arglist] --autorun]>-1||
         [lsearch [string tolower $arglist] -autobench]>-1||[lsearch [string tolower $arglist] --autobench]>-1} {
        #if {[tk windowingsystem] eq "win32"} {
        #    console show
        #    console eval {wm geometry . 150x50+00+0}
        #}
        puts "autobench modus active...\n"
        set silentrun 1

        global globEnvironment
        set globEnvironment ""
        global globRevision
        set globRevision ""
        global globRevDir
        set globRevDir ""
        global globInputFile
        set globInputFile ""
        global globRepeatings
        set globRepeatings 2
        global globTimeout
        set globTimeout 0


        #for fdr (csp)
        global fdrpath
        set fdrpath ""
        global cspspecs
        set cspspecs ""
        # ###

        #global silentautobench (wird schon oben definiert)
        set silentautobench 0; #only with -path: no temporary files; save results only to codespeed server (not csv)
        global showterminal
        set showterminal 1; #0=no xterm terminal during checkout
        global overwritedate
        set overwritedate 0; #1=using current date instead of revision date
        set foundautobenchenv 0; #globEnvironment contains used environment for autobench
        set foundautobenchrev 0; #globRevision contains used revision for autobench
        set foundautobenchdir 0; #globRevDir contains used directory that contains probcli
        set foundfdrdir       0; #global fdr path
        set foundtladir       0; #global tla path
        set foundinputfilearg 0; #globInputFile contains used input file for autobench
        set foundrepeatings   0; #globRepeatings contains the number of iterations
        set foundtimeout      0; #globTimeout contains timeout for probcli (0=no timeout)
        foreach {xarg} [split $arglist " "] {
          if {$foundautobenchenv} {
             set globEnvironment $xarg
             set foundautobenchenv 0
             puts "used environment: $globEnvironment \n"
          }
          if {$foundautobenchrev} {
             set globRevision $xarg
             set foundautobenchrev 0
             #if {!($globRevision eq "SELF")} {
             #  puts "revision: $globRevision \n"
             #}
          }
          if {$foundautobenchdir} {
             set globRevDir [file normalize $xarg]
             set foundautobenchdir 0
             puts "used probcli-path: $globRevDir \n"
          }
          if {$foundfdrdir} {
             set fdrpath [file normalize $xarg]
             set foundfdrdir 0
             puts "used fdr path: $fdrpath \n"
          }
          if {$foundtladir} {
             if {$executeB2TLA} {
               set tlapath [file normalize $xarg]
               set foundtladir 0
               puts "used tla path: $tlapath \n"
             } else {
               puts "incompatible options: -notla and -tlapath. -tlapath ignored."
             }
          }
          if {$foundinputfilearg} {
             set globInputFile [file normalize $xarg]
             set foundinputfilearg 0
             puts "used input file: $globInputFile \n"
          }
          if {$foundrepeatings} {
             if {[string is integer -strict $xarg]} {
                if {$xarg>0} {
                  set globRepeatings $xarg
                } else {
                  puts "invalid argument: -iterations can only be a positive integer"
                  exit -1
                }
             } else {
                  puts "invalid argument: $xarg is not a valid argument for -iterations (must be a positive integer)"
                  exit -1
             }
             set foundrepeatings 0
             puts "iterations: $globRepeatings \n"
          }
          if {$foundtimeout} {
             if {[string is integer -strict $xarg]} {
                if {$xarg>=0} {
                  set globTimeout $xarg
                } else {
                  puts "invalid argument: -timeout can only be a positive integer (or 0=no timeout)"
                  exit -1
                }
             } else {
                  puts "invalid argument: $xarg is not a valid argument for -timeout (must be a positive integer or 0=no timeout)"
                  exit -1
             }
             set foundtimeout 0
             puts "timeout: $globTimeout \n"
          }
          if {([string tolower $xarg] eq "-env")||([string tolower $xarg] eq "--e")} {
            set foundautobenchenv 1
          }
          if {([string tolower $xarg] eq "-rev")||([string tolower $xarg] eq "-revision")||([string tolower $xarg] eq "--r")} {
            set foundautobenchrev 1
          }
          if {([string tolower $xarg] eq "-path")||([string tolower $xarg] eq "--p")} {
            set foundautobenchdir 1
          }
          if {([string tolower $xarg] eq "-fdrpath")} {
            set foundfdrdir 1
          }
          if {([string tolower $xarg] eq "-tlapath")} {
            set foundtladir 1
          }
          if {([string tolower $xarg] eq "-silent")||([string tolower $xarg] eq "-readonly")} {
            puts "Silent mode - results will be saved to codespeed only, not to a local csv file\n"
            set silentautobench 1
            set showterminal 0
          }
          if {([string tolower $xarg] eq "-veryverbose")} {
            puts "vv\n"
            set veryverbose 1
          }
          if {([string tolower $xarg] eq "-nocodespeed")} {
            puts "Local mode - results will be saved to a local csv file only, not to the codespeed server\n"
            set savetocodespeed 0
          }
          if {([string tolower $xarg] eq "-input")} {
            set foundinputfilearg 1
          }
          if {([string tolower $xarg] eq "-iterations")||([string tolower $xarg] eq "-repeatings")} {
            set foundrepeatings 1
          }
          if {([string tolower $xarg] eq "-timeout")} {
            set foundtimeout 1
          }
          if {([string tolower $xarg] eq "-showterminal")} {
            puts "using xterm...\n"
            set showterminal 1
          }
          if {([string tolower $xarg] eq "-noterminal")} {
            puts "...\n"
            set showterminal 0
          }
          if {([string tolower $xarg] eq "-currentdate")} {
            puts "using current date instead of revision date...\n"
            set overwritedate 1
          }
        }
        if {! ($globRevDir eq "")} {
            if {(![file exists $globRevDir/probcli])&&(![file exists $globRevDir/probcli.sh])&&(![file exists $globRevDir/probcli.exe])} {
                puts "error: probcli not found in given path $globRevDir"
                if {[file exists $globRevDir] && ![file isdirectory $globRevDir]} {
                  puts "$globRevDir is not a valid directory.\nPlease specify only the directory of probcli (not the full path including file name)"
                }
                exit -1
            }
            if {($globRevision eq "")} {
                if {![file exists $globRevDir/revision.info]} {
                    puts "Please specify the revision number of $globRevDir/probcli\n(use -rev <NUMBER>)"
                    exit -1
                } else {
                  set f [open $globRevDir/revision.info]
                  set thisentry [read $f]
                  close $f

                  if {[string first " " [string trim $thisentry " "]]>-1} {
                    set globRevision [string trim "[string range $thisentry 0 [string first " " $thisentry]]" " ()"]
                  } else {
                    set globRevision [string trim $thisentry " ()"]
                  }
                }

            } else {
                  if {($globRevision eq "SELF")} {
                    #get revision number from probcli (probcli -svers)
                    set probcliversioninfoX [exec $globRevDir/probcli -svers]
                    puts $probcliversioninfoX
                    #exec $globRevDir/probcli -svers >$globRevDir/probclirevision.dat
                    if { ([string first "(" $probcliversioninfoX] == -1) || ([string first "(" $probcliversioninfoX] == -1)} {
                       puts "No version info found."
                    } else {
                       set globRevision [string range $probcliversioninfoX [string first "(" $probcliversioninfoX]+1 [string first ")" $probcliversioninfoX]-1]
                       #puts $globRevision
                    }
                }
                puts "probcli revision $globRevision"
            }
        }
        if {(($globRevDir eq "")) && ($silentautobench)} {
          puts "ERROR: Silent (read-only) autobench modus can only be used with -path <PATH OF PROBCLI>. (see --help)"
          exit -1
        }


        if {[readINI 1]} {
            performautorun
        }
    } elseif {[readINI 1]} {
        showstart
    }
}



proc performautorun {} {
    set autoruntesting 1

    eval global {os resfile vers argv0 workspace putsDest putsistxtbox}
    eval global {tcl_platform argv inputfileslist revslist resfileslist probrev probrev2}
    eval global {workspace homedir globRevision globRevDir globInputFile showterminal}

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

    if {($globInputFile eq "")} {
        set xinput "autobench.input"
    } else {
        set xinput $globInputFile
    }
    set xuseresfile "[string range $xinput 0 [string last "." $xinput]]res"

    set resfile $xuseresfile

    set thisdir [pwd]
    cd $homedir; cd [file dirname $argv0]; cd $workspace
    #putsTxt "$homedir\n$argv0\n$thisdir\n[pwd]\n$xinput"
    if {![file exists $xinput]} {
      if {($globInputFile eq "")} {
        putsTxt "***error: you have to create an input file \"autobench.input\" first.\n
                    Try to run probench (gui version) without parameters and create the input file\n
                    (all tests in autobench.input will be performed during autorun)\n
                    If you want to use an other input file, use -input <FILENAME> ***\n"
      } else {
        putsTxt "***error: Input file \"$xinput\" not found.\n
                    Try to run probench (gui version) without parameters and create an input file***\n"
      }
      exit -1
    }
    set foundnewrepos ""
    if {[file exists "probench.repos"]} {
        set frepos [open "probench.repos"]
        set newreposlst [split [read $frepos] "\n"]; #read list of repositories
        close $frepos
        foreach {repostmp} $newreposlst {
          set repostmp [string trim $repostmp " "]
          if {[string range $repostmp 0 0] ne "#"} {
            set foundnewrepos $repostmp
            break
          }
        }
        if {$foundnewrepos ne ""} {
          puts "used repository (from probench.repos): $foundnewrepos"
        } else {
          puts "probench.repos exists, but does not contain a repository url. Default repository used."
        }
    }
    cd $thisdir




    global xsource
    global xmake
    global xterm
    global xsvn
    global xrev
    if {$foundnewrepos eq ""} {
       #set xsource "https://cobra.cs.uni-duesseldorf.de/prob/"
       set xsource "git@tuatara.cs.uni-duesseldorf.de:prob/prob_prolog.git"
    } else {
       set xsource $foundnewrepos
    }

    if [string match $os win] {
        set xmake "Make_probcli_Win.bat"
        set xterm "cmd /k"
    }  else {
        set xmake "make"
        set xterm "xterm -e"
        # set xterm "xterm -e"
    }
    if {!($showterminal)} {
        set xterm ""; #keine Terminalausgabe
    }

    #set xsvn "svn"
    set xsvn "git"
    global xreposdirname
    set xreposdirname "prob_prolog"
    if {($globRevision eq "")} {
        #set xrev "HEAD"
        set xrev "develop"
    } else {
        set xrev $globRevision
    }


    eval global {globRepeatings globTimeout}
    set xrepeatings 2
    if {[string is integer -strict $globRepeatings]} {
         if {$globRepeatings>0} {
            set xrepeatings $globRepeatings
         }
    }
    set xtimeout 0
    if {[string is integer -strict $globTimeout]} {
         if {$globTimeout>=0} {
            set xtimeout $globTimeout
         }
    }



   if {($globRevDir eq "")} {
      #getting version from svn


    ##################################################################
    putsTxt "step 1: getting and building current revision..."
    ##################################################################

    #if [string match $os win] { console show }

    #get and build current revision
    #instant_checkout

    #frame .checkoutwnd.runinfo
    #label .checkoutwnd.runinfo.lrun -text "Getting and building revision.\nThis may take several minutes..."
    #pack .checkoutwnd.runinfo -fill both -expand yes
    #pack .checkoutwnd.runinfo.lrun -expand yes -fill both -padx 10 -pady 10


            if [string match $xsvn svn] {
                set currentrev [checkout_runall $xrev $xsource $xterm $xsvn $xmake]
            } else {
                #git
                set currentrev [checkout_runall_GIT $xrev $xsource $xterm $xsvn $xmake 0 0 $xreposdirname]
            }

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

    #set xrepeatings 1
    #set xtimeout 0
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


    set probpath $workspace/rev$probrev
    prepare_runtests . $probrev $probrev2 1 1 ""; #    prepare_runtests .testwindow $probrev $probrev2 1 1
   #end of  if ($globRevDir eq "") ...
   } else {
      #use probcli from path...............
      ##################################################################
      putsTxt "performing benchmarks..."
      ##################################################################

      #set xrepeatings 1
      #set xtimeout 0
      set xspecialopts ""
      set xusedrev $globRevision
      set xusedrevB ""
      set probrev2 0
      set probrev $globRevision
      set probpath $globRevDir


      set thisdir1 [pwd]
      eval global {overwritedate}
      if {!($overwritedate)} {
        get_repos_info $globRevision $xsource $xterm $xsvn
      }
      cd $thisdir1
      eval global {getrevdate}

      prepare_runtests . $probrev 0 1 1 $globRevDir
   }


    set putsDest puts
    set putsistxtbox 0

    if {$codespeedfailed} {
      set xusecsvfile "[string range $resfile 0 [string last "." $resfile]]csv"
      #putsTxt "\nwarning: could not save results to codespeed. Is the server running? Does the environment exist (case-sensitive)?\nResults were saved to $xusecsvfile only."
      putsTxt "\nwarning: could not save results to codespeed. Is the server running? Does the environment exist (case-sensitive)?\n"
      exit -1; #no successful codespeed connection
    } else {
              eval global {savetocodespeed}
              if {$savetocodespeed} {
                 puts "Results saved to codespeed."
              } else {
                 puts "-nocodespeed"
              }
    }
    putsTxt "Finished."

    eval global {aftererror}
    if {$aftererror > 0} {
        exit -1
    } else {
        eval global {filesnotfounderror}
        if {$filesnotfounderror > 0} {
          exit -1; # if "some files not found - error" should be ignored, set exit code to 0 here
        } else {
          exit 0
        }
    }
}


proc autobench_save_result {resvalue} {

    #puts "debug: resvalue = ${resvalue}"
    if [containsPart $resvalue "res_probcli("] {
      set resvalue [string trim $resvalue "res_probcli( );"]
      set abExec "probcli"
    } elseif [containsPart $resvalue "res_fdr2("] {
       set resvalue [string trim $resvalue "res_fdr2( );"]
      set abExec "fdr2"
    } else {
       set resvalue [string trim $resvalue "res_B2TLA( );"]
      set abExec "B2TLA"
    }
    #puts "debug: resultisfdr2 = ${resultisfdr2}"
    #putsTxt "RESULT: $resvalue"
    set resvalue [split $resvalue ","]
    #putsTxt "RESULT: $resvalue"

    set abURL "ignored"; #ignored yet, set in python-file (http://localhost:8000/ or http://tuatara.cs.uni-duesseldorf.de:8000)
    set abProj "ProBench"
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
      foreach {abRev abBenchFile abOpts abBenchName abRes abTime abErr abStdDev abMax abMin} $resvalue {
          #putsTxt "RESULT VALUE: $abRes"
          if {$abBenchName eq ""} {
              set abBenchName [file tail $abBenchFile]
          }
          #puts "debug: foreach split: $abRev $abBenchFile $abOpts $abBenchName $abRes $abTime $abErr \n ...debug continued: unsplitted: $resvalue"
          #codespeed server URL, project, revision(id), benchmark name, result value, interpreter(executable), environment(host)
          #puts "python savecodespeed_probench.py $abURL $abProj $abRev $abBenchName $abRes $abExec $abEnvironment"
          eval global {veryverbose}
          if {$veryverbose} {
            puts "python savecodespeed_probench.py $abURL $abProj $abRev $abBenchName $abRes $abExec $abEnvironment $abRevDate $abRevTime $abStdDev $abMax $abMin"
          }
          time "catch {exec python savecodespeed_probench.py $abURL $abProj $abRev $abBenchName $abRes $abExec $abEnvironment $abRevDate $abRevTime $abStdDev $abMax $abMin} xxres xxopts" 1
          if [containsPart $xxopts "-errorinfo"] {
              putsTxt "error while trying to store data to codespeed ($abProj $abRev $abBenchName $abRes $abExec $abEnvironment $abRevDate  $abRevTime $abStdDev $abMax $abMin):\n[dict get $xxopts -errorinfo]"
              eval global {codespeedfailed}
              set codespeedfailed 1
          } else {
              eval global {savetocodespeed}
              if {$savetocodespeed} {
                 puts "Results saved to Codespeed ($abProj $abRev $abBenchName $abRes $abExec $abEnvironment $abRevDate  $abRevTime $abStdDev $abMax $abMin)."
              } else {
                 puts "-nocodespeed"
              }
          }
      }
    }
    cd $thisdir
}



proc showresults {testswnd revlst} {
  puts "\n***not possible in non-gui version***\n"
}


proc preparediag {dreslist drevtag drev dtesttag dtests clrlst dwidth testswnd} {
  puts "\n***not possible in non-gui version***\n"

}

proc showdiaginfo {tsts clrs} {
  puts "\n***not possible in non-gui version***\n"
}


proc calldrawdiag {canv xres} {
  puts "\n***not possible in non-gui version***\n"
}



proc contains {list el} {expr {[lsearch -exact $list $el] >= 0}}

proc containsNCS {list el} {expr {[lsearch -exact -nocase $list $el] >= 0}}

proc containsPart {list el} {expr {[string first $el $list] > -1}}

proc containsPartNCS {list el} {expr {[string first [string tolower $el] [string tolower $list]] > -1}}


proc drawtable {canv xres xtests xrevs} {

  puts "\n***not possible in non-gui version***\n"

}




proc drawdiag {canv x0 y0 x1 y1 dat} {

    puts "\n***not possible in non-gui version***\n"

}

proc drawYlabels {canv x0 ybase y1 scaley maxval} {
  puts "\n***not possible in non-gui version***\n"

}



proc swap {_a _b} {
    upvar 1 $_a a $_b b
    foreach {a b} [list $b $a] break
}


proc maxLst {l} {
    set res [lindex $l 0]
    foreach ele [lrange $l 1 end] {
        if {$ele > $res} {
            set res $ele
        }
    }
    return $res
}

proc minLst {l} {
    set res [lindex $l 0]
    foreach ele [lrange $l 1 end] {
        if {$ele < $res} {
            set res $ele
        }
    }
    return $res
}


proc setvars1 {} {
  #set path variables and remember OS

  eval global {tcl_platform argv0 argv argc}
  global os
  global probrev
  global probrev2
  global resfile
  global vers
  global timestamp

  set vers 1; #internal version number

  set probrev 0; #will be overwritten by showstart-procedure
  set probrev2 0; #will be overwritten by showstart-procedure

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
  #set variables for test runs

  eval global {tcl_platform argv0 argv argc os probrev resfile vers xinput probpath}
  global params
  global mchpath
  global testlist

  set params {-mc 1000}; # Standardparameter
  set mchpath ${probpath}/test; #Standardpfad fuer mch-files

  set arglist [split $argv " "]

  set returnval [setvars3]

  set testlist $xinput


  if [lsearch [string tolower $arglist] -mchdir]>-1 {
     set mchpath [lindex $arglist [expr {[lsearch [string tolower $arglist] -mchdir]+1}]]
     if ![file isdirectory $mchpath] {
          errorproc 1 warning "path not valid" "Error: \"$mchpath\" is not a valid path."
     }
  } else {
     #putsTxt "uses default mchpath"
  }

  return $returnval
}


proc setvars3 {} {
  eval global {probrev probpath}

  if [file isdirectory $probpath] {
     if {[file exists $probpath/probcli]||[file exists $probpath/probcli.sh]||[file exists $probpath/probcli.exe]} {
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
          #if $activetests {
          #  set activewnd .testwindow
          #} else {
          #  set activewnd .
          #}
          #if {($msgicon eq "info") || ($msgicon eq "question") || ($msgicon eq "warning") || ($msgicon eq "error")} {
          #      tk_messageBox -icon $msgicon -title $msgtitle -message $msg -type ok -parent $activewnd
          #} else {
          #      tk_messageBox -icon error -title $msgtitle -message $msg -type ok -parent $activewnd
          #}
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
  puts "\n***not possible in non-gui version***\n"

}



proc canceltests {showmsg} {
    #go back to main menu
    set activetests 0
    #showstart
    if {$showmsg} {
        eval global {silentrun}
        #if {!$silentrun} {
        #  tk_messageBox -icon error -title "Workspace corrupt" -message "No tests were performed." -type ok -parent .
        #} else {
          putsTxt "error: workspace corrupt: no tests were performed."
        #}
    }
    return
}


proc prepare_runtests {testwnd fromrev torev silent autobench fixpath} {
    # perform tests (silent=used for autorun only, otherwise false)
    eval global {activetests os probpath probrev revslist workspace resfile silentrun silenterrs silentautobench uselocal}
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
    global filesnotfounderror
    set filesnotfounderror 0


    set activetests 1
    if {![setvars2]} {
                canceltests 1
                return
    }
    #tests
    if !{$aftererror} {
        readmchfiledat
    }

    if {($torev ne 0)&&(!$uselocal)} {
        if {$fromrev > $torev} {
            set dummy $fromrev
            set fromrev $torev
            set torev $dummy
        }
    }

    if {!($fixpath eq "")} {
      #use probcli from PATH fixpath
      set thisdir [pwd]
      set probrevlist $fromrev
      set probpath $fixpath
      if [file isdirectory $probpath] {
        if {[file exists $probpath/probcli]||[file exists $probpath/probcli.sh]||[file exists $probpath/probcli.exe]} {
            #putsTxt "probcli found in $probpath..."
        } else {
            errorproc -1 error "probcli not found" "Error: probcli not found in path \"$probpath\"."
            canceltests 1
            return
        }
      } else {
          errorproc -1 error "probcli not found" "Error: \"$probpath\" is not a valid directory."
          canceltests 1
          return
      }

    } else {
    #GETTING FROM SVN

    if {[expr {$torev ne 0}] && [expr {$fromrev ne $torev}]} {
        foreach {thisentry} $revslist {
            if {[string first " " [string trim $thisentry " "]]>-1} {
                set thisrev1 [string trim "[string range $thisentry 0 [string first " " $thisentry]]" " ()"]
            } else {
                set thisrev1 [string trim $thisentry " ()"]
            }
            if {[expr {$thisrev1 >= $fromrev}] && [expr {$thisrev1 <= $torev}]} {
                lappend probrevlist $thisrev1
            }
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
        if {($fixpath eq "")} {
          set probpath $workspace/rev$probrev
        }
        cd $thisdir
        if {![setvars3]} {
                #if setvars3 returns false, probcli could not be found in the specific workspace directory
                cd $thisdir
                canceltests 1
                return
        }
    }
    } ; #end of SVN-ONLY (NON-PATH)

    # now do a single run without measuring
    set probrev $fromrev
    if {($fixpath eq "")} {
      set probpath $workspace/rev$probrev
    }
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
        #set errmsg "The following files could not be found: \n\n$filenotfoundlist\n\n\nDo you want to continue?"
        #set clicked [tk_messageBox -icon warning -title "Could not find all machine files" -message $errmsg -type yesno -parent .]
        #    switch -- $clicked {
        #        yes {  }
        #        no  {
        #                cd $thisdir
        #                canceltests 0
        #                return
        #            }
            }
      } else {
          set thisfilenotfoundlist ""
          foreach {thisfilenotfound} $filenotfoundlist {
             set thisfilenotfoundlist "$thisfilenotfoundlist\n   $thisfilenotfound"
          }
          lappend silenterrs "The following files could not be found: $thisfilenotfoundlist"
          putsTxt "warning: The following [llength $thisfilenotfoundlist] benchmark files could not be found: $thisfilenotfoundlist"
      }
    }
    # now perform tests
    foreach {thisrev} $probrevlist {
        set probrev $thisrev
        if {($fixpath eq "")} {
          set probpath $workspace/rev$probrev
        }
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
        set thisfilenotfoundlist ""
        foreach {thisfilenotfound} $filenotfoundlist {
             set thisfilenotfoundlist "$thisfilenotfoundlist\n   $thisfilenotfound"
        }
        set errmsg "The following [llength $thisfilenotfoundlist] files could not be found (other tests were performed as well): $thisfilenotfoundlist\n\n"
        set errtype warning
        set filesnotfounderror 1
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
    if {($silentautobench)} {
      set errmsg "${errmsg}silent mode - no logfile written."
    } else {
      set errmsg "${errmsg}You can review detailed informations in logfile:\n \"$workspace/$xuselogfile\"."
    }
    #message will be displayed at the end of this procedure


    #write logfile
    set xuselogfile "[string range $resfile 0 [string last "." $resfile]]log"
    #msgbox $xuselogfile
    set thisdir [pwd]
    cd $workspace

    if {!($silentautobench)} {
        set fwrite [open $xuselogfile w]
        puts $fwrite "Log File for input file [string range $resfile 0 [string last "." $resfile]]input, created at [clock format [clock seconds] -format {%Y-%m-%d %H:%M:%S}]."

        puts $fwrite "\nused revisions:\n"
        foreach {entry} $probrevlist {
            puts $fwrite $entry
        }
        puts $fwrite "\n"
        if {[llength $filenotfoundlist]>0} {
            puts $fwrite "The following [llength $filenotfoundlist] files could not be found:"
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
    }


    set activetests 0
    if !{$aftererror} {
        putsTxt "Benchmarks finished."
        #if ![string match $os win] { exit }
        if {!$silent} {
            #prepare_showres $testwnd $probrevlist
        }
    }
    if {[string length $errmsg]>0} {
        if {$silent} {
            set putsDest puts
            set putsistxtbox 0
            puts "Benchmark summary:\n$errmsg"
	    errorproc -1 $errtype "Finished." $errmsg
        } else {
            errorproc -1 $errtype "Finished." $errmsg
        }
    }
}



proc runtests {performtests} {
  eval global {mchpath probpath params mchfilelist resfile vers timestamp workspace \
               xrepeatings filenotfoundlist xtimeout xspecialopts silentrun silentautobench \
               savetocodespeed executeB2TLA};

    putsTxt "Please wait..."

    set thisdir [pwd]
    cd $workspace
    if {($performtests)&&(!($silentautobench))} {
        set fwrite [open $resfile a]
        puts $fwrite "Begin_tests(${vers},${timestamp});"
    }
    if [file isdirectory $mchpath] {
        cd ${mchpath}
        #putsTxt "Switch to mch-path = [pwd]"
    }
    cd $thisdir

    putsTxt "[llength $mchfilelist] benchmarks..."
    set benchcount 0
    foreach {x} $mchfilelist {
        set mchname ""
        incr benchcount
        #putsTxt "Benchmark $benchcount of [llength $mchfilelist]..."
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
                set mchfile [file normalize "[string range $x 0 [expr {[string first "@" $x]-1}]]"]
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
                set mchfile [file normalize "$x"]
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
                    putsTxt "running probcli with ${mchfile} (${usedparams}) ($benchcount of [llength $mchfilelist])..."
                    set mchname [string trim $mchname " "]
                    set benchresvalue [runprob $mchfile $usedparams $mchname $xrepeatings $performtests]
                    eval global {fdrpath}
                    set cspbenchvalue -1
                    if {$fdrpath ne ""} {
                       set cspbenchvalue [runfdr2  $mchfile "" $mchname $xrepeatings]
                    }
                    if {$executeB2TLA} {
                       set tlabenchvalue [runB2TLA $mchfile $mchname $xrepeatings]
                    } else {
                       set tlabenchvalue -1
                    }
                    if {!($silentautobench)} {
                      puts $fwrite $benchresvalue
                      if {$cspbenchvalue > -1} {
                           puts $fwrite $cspbenchvalue
                      }
                    }
                    if {$performtests==2} {
                        #autobench
                        if {$savetocodespeed} {
                           #puts "debug: benchresvalue = ${benchresvalue}"
                           autobench_save_result $benchresvalue
                           if {$cspbenchvalue > -1} {
                                #puts "debug: cspresvalue = ${cspbenchvalue}"
                                autobench_save_result $cspbenchvalue
                           }
                           if {$tlabenchvalue > -1} {
                                autobench_save_result $tlabenchvalue
                           }
                        } else {
                           puts "-nocodespeed\n"
                        }
                    }
                } else {
                    #putsTxt "dummy test run with ${mchfile} (${usedparams})..."
                    putsTxt "initial run of probcli with ${mchfile} ($benchcount/[llength $mchfilelist])..."
                    set mchname [string trim $mchname " "]
                    #runprob $mchfile $usedparams $mchname 1 $performtests

                }
            } else {
                if ![string match [string trim $mchfile " "] ""] {
                    if {$performtests} {
                        if {[lsearch $filenotfoundlist "${mchfile}"] < 0} {
                            #add only if not yet in list
                            putsTxt "ERROR: the testmachine $mchfile does not exist. (PARAM:${mchparams}) \=\> skipped, continue with next file..."
                            if {!($silentautobench)} {
                              puts $fwrite "error_filenotfound(${mchfile},${timestamp});"
                            }
                            lappend filenotfoundlist "${mchfile}"
                        }
                    } else {
                        lappend filenotfoundlist "${mchfile}"
                    }
                }
            }
        }
    }
    if {($performtests)&&(!($silentautobench))} {
        puts $fwrite "End_tests(${vers},${timestamp});"
        close $fwrite
    }

    if {!$silentrun} {
      #wm transient .testwindow {}
    }
}



proc meanMeasurements {measurements} {
    if {[llength $measurements] == 0} {
        # no mean to be calculated
        return -1
    }
    set commandTime 0
    set len [llength $measurements]
    for {set i 0} {$i < $len} {incr i} {
         incr commandTime [lindex $measurements $i]
    }
    return [expr {$commandTime/double($len)}]
}

proc standardDeviation {mean measurements} {
    set len [llength $measurements]
    set quadratSumm 0
    foreach i $measurements {
         set quadratSumm [expr {$quadratSumm + (($i - $mean)**2)}]
    }
    if {$len > 1} {
        return [expr {sqrt($quadratSumm/double($len-1))}]
    } else {
        return 0.0
    }
}


proc runfdr2 {cspfile fdrparams benchname mrepeatings} {
    eval global {fdrpath probrev cspspecs timestamp}
    if {[file extension $cspfile] == ".csp"} {
        set ausgabe "0.0 microseconds"

        puts "csp file: running fdr2 ($cspfile)..."

        if {[file exists "${fdrpath}/fdr2"]} {
            # path = homedir/fdr2/bin
            for {set i 0} {$i < $mrepeatings} {incr i} {
              set execOutput [time "catch {exec ${fdrpath}/fdr2 batch ${cspfile}} results options" 1]
              set execOutput [string trimleft $execOutput]
              lappend runtimesList [string range ${execOutput} 0 [expr {[string first " " ${execOutput}]-1}]]
            }
            set runtime [meanMeasurements $runtimesList]
            set newStdDevRuntime [standardDeviation $runtime $runtimesList]
            set newMaxRuntime [maxLst $runtimesList]
            set newMinRuntime [minLst $runtimesList]
            #OLD: set ausgabe [time "catch {exec ${fdrpath}/fdr2 batch ${cspfile}} results options" $mrepeatings]
        } else {
            puts "fdr2 not found in given path: $fdrpath"
            #set ausgabe [time "catch {exec fdr2 batch ${cspfile}} results options" $mrepeatings]
        }

        #OLD: set ausgabe [string trimleft ${ausgabe} " "]
        # Vom String "ausgabe" nur die Zeichen 0 bis vor dem ersten Leerzeichen nehmen und in "runtime" speichern
        #OLD: set runtime [string range ${ausgabe} 0 [expr {[string first " " ${ausgabe}]-1}]]

        #putsTxt "fdr2 runtime ${runtime} microseconds"
        set runtime [ expr { $runtime / 1000000.0 } ]
        set newMaxRuntime [ expr { $newMaxRuntime / 1000000.0 } ]
        set newMinRuntime [ expr { $newMinRuntime / 1000000.0 } ]
        set newStdDevRuntime [ expr { $newStdDevRuntime / 1000000.0 } ]
        putsTxt "fdr2 runtime ${runtime} seconds (max: ${newMaxRuntime}, min: ${newMinRuntime}, stddev: ${newStdDevRuntime}) "

        lappend cspspecs "[file tail $cspfile] @ rev. $probrev: ${runtime}s"
        set cspres "res_fdr2(${probrev},${cspfile},\"\",${benchname},${runtime},${timestamp},\"\",${newStdDevRuntime},${newMaxRuntime},${newMinRuntime});"
        return $cspres
    } else {
      #puts "not a csp file (nothing to do for runfdr2)"
      return -1
    }
}

proc runB2TLA {bfile benchname mrepeatings} {
    eval global {probpath probrev cspspecs timestamp tlapath}
    if {[string compare "$tlapath" ""] == 0} {
      set b2tlaCmd "/var/lib/jenkins/local_slave/workspace/b2tla/build/b2tla"
    } else {
      set b2tlaCmd $tlapath
    }
    if {[lsearch [file split $bfile] "TLC"] >= 0} {
        # getting the path of the TLC tool for setting the CLASSPATH for TLC
        # bfile == "/usr/tmp/TLC/examples/ex_TLC.mch" => tlcpath == "/usr/tmp/TLC/"
        set tlcpath "[lindex [split $bfile "TLC"] 0]TLC/tla2tools.jar"
        set ausgabe "0.0 microseconds"
        puts "specific tla_mch file: running B2TLA TOOL (bfile: $bfile) (tclpath: $tlcpath) (b2tlaCmd: $b2tlaCmd) ..."

        if {[file exists "${b2tlaCmd}/B2TLA.jar"]} {
            # path = homedir/fdr2/bin
            for {set i 0} {$i < $mrepeatings} {incr i} {
              puts "Executing B2TLA tool..."
              set execOutput [time {execpipe "java -cp ${tlcpath} -jar ${b2tlaCmd}/B2TLA.jar ${bfile} -tmp"}]
              set execOutput [string trimleft $execOutput]
              lappend runtimesList [string range ${execOutput} 0 [expr {[string first " " ${execOutput}]-1}]]
            }
            set runtime [meanMeasurements $runtimesList]
            set newStdDevRuntime [standardDeviation $runtime $runtimesList]
            set newMaxRuntime [maxLst $runtimesList]
            set newMinRuntime [minLst $runtimesList]
            #OLD: set ausgabe [time "catch {exec ${fdrpath}/fdr2 batch ${cspfile}} results options" $mrepeatings]
            #OLD: set ausgabe [string trimleft ${ausgabe} " "]
            # Vom String "ausgabe" nur die Zeichen 0 bis vor dem ersten Leerzeichen nehmen und in "runtime" speichern
            #OLD: set runtime [string range ${ausgabe} 0 [expr {[string first " " ${ausgabe}]-1}]]

            set runtime [ expr { $runtime / 1000000.0 } ]
            set newMaxRuntime [ expr { $newMaxRuntime / 1000000.0 } ]
            set newMinRuntime [ expr { $newMinRuntime / 1000000.0 } ]
            set newStdDevRuntime [ expr { $newStdDevRuntime / 1000000.0 } ]
            putsTxt "B2TLA runtime ${runtime} seconds (max: ${newMaxRuntime}, min: ${newMinRuntime}, stddev: ${newStdDevRuntime}) "

            #lappend cspspecs "[file tail $cspfile] @ rev. $probrev: ${runtime}s"
            set tlares "res_B2TLA(${probrev},${bfile},\"\",${benchname},${runtime},${timestamp},\"\",${newStdDevRuntime},${newMaxRuntime},${newMinRuntime});"
            return $tlares
        } else {
            puts "B2TLA has been not found in given path: $tlcpath"
            #set ausgabe [time "catch {exec fdr2 batch ${cspfile}} results options" $mrepeatings]
            return -1
        }

    } else {
      puts "not a mch file (nothing to do for B2TLA)"
      return -1
    }
}

proc execpipe {COMMAND} {
   if { [catch {open "| $COMMAND 2>@stdout"} FILEHANDLE] } {
      return "Can't open pipe for '$COMMAND'"
   }
   set PIPE $FILEHANDLE
   fconfigure $PIPE -buffering none

   set OUTPUT ""

   while { [gets $PIPE DATA] >= 0 } {
      puts $DATA
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


proc runprob {mchfile1 mchparams1 mchname1 mrepeatings performtests} {
    eval global {probpath probrev timestamp argv0 errmachines1 errmachines1detail errmachines2 errmachines2detail \
                errmachines3 errmachines3detail errmachines4 errmachines4detail succmachines}

    set tmpfile tester.tmp
    file delete $tmpfile

    set ausgabe "0.0 0"
    set iserror 0

    #OLD: first run without timing (now performed before test run)
    #catch {exec ${probpath}/probcli ${mchfile1} ${mchparams1}}

    if {[file exists "${probpath}/probcli"]} {
      set probcmdX "${probpath}/probcli"
    } elseif {[file exists "${probpath}/probcli.sh"]} {
      set probcmdX "${probpath}/probcli.sh"
    } elseif {[file exists "${probpath}/probcli.exe"]} {
      set probcmdX "${probpath}/probcli.exe"
    } else {
      puts "."
      set probcmdX "${probpath}/probcli"
    }

    #now run for "mrepeatings" times and time...
    for {set i 0} {$i < $mrepeatings} {incr i} {
              set execOutput [time "catch {exec ${probcmdX} ${mchfile1} ${mchparams1} >>~/probclitest.txt} results options" 1]
              set execOutput [string trimleft $execOutput]
              # chars 0..(first space) of execOutput is the runtime in microseconds, save it to runtimesList
              lappend runtimesList [string range ${execOutput} 0 [expr {[string first " " ${execOutput}]-1}]]
    }

    set runtime [meanMeasurements $runtimesList]
    set newStdDevRuntime [standardDeviation $runtime $runtimesList]
    set newMaxRuntime [maxLst $runtimesList]
    set newMinRuntime [minLst $runtimesList]

    #OLD:
    #if {[file exists "${probpath}/probcli"]} {
    #  set ausgabe [time "catch {exec ${probpath}/probcli ${mchfile1} ${mchparams1} >>~/probclitest.txt} results options" $mrepeatings]
    #} elseif {[file exists "${probpath}/probcli.sh"]} {
    #  set ausgabe [time "catch {exec ${probpath}/probcli.sh ${mchfile1} ${mchparams1}} results options" $mrepeatings]
    #} elseif {[file exists "${probpath}/probcli.exe"]} {
    #  set ausgabe [time "catch {exec ${probpath}/probcli.exe ${mchfile1} ${mchparams1}} results options" $mrepeatings]
    #} else {
    #  puts "."
    #  set ausgabe [time "catch {exec ${probpath}/probcli ${mchfile1} ${mchparams1} >>~/probclitest.txt} results options" $mrepeatings]
    #}
    #set ausgabe [string trimleft ${ausgabe} " "]
    # Vom String "ausgabe" nur die Zeichen 0 bis vor dem ersten Leerzeichen nehmen und in "runtime" speichern
    #set runtime [string range ${ausgabe} 0 [expr {[string first " " ${ausgabe}]-1}]]



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
                eval global {silentautobench}
                if {$mchname1 ne ""} {
                    if {($silentautobench)} {
                      lappend errmachines2 "\nother error: [file tail $mchname1] @ rev. $probrev, details:\n$thiserrinfo\n(end of details for [file tail $mchname1])"
                    } else {
                      lappend errmachines2 "other error (see logfile for details): [file tail $mchname1] @ rev. $probrev"
                    }
                    lappend errmachines2detail "#### begin detailed error info: [file tail $mchname1] @ rev. $probrev (machine file: $mchfile1), details:\n$thiserrinfo\
                            \n##### end of detailed error info for [file tail $mchname1] @ rev #####\n\n"
                } else {
                    if {($silentautobench)} {
                      lappend errmachines2 "\nother error: [file tail $mchfile1 @ rev. $probrev, details:\n$thiserrinfo\n(end of details for [file tail $mchfile1)"
                    } else {
                      lappend errmachines2 "other error (see logfile for details): [file tail $mchfile1] @ rev. $probrev"
                    }
                    lappend errmachines2detail "##### begin detailed error info: [file tail $mchfile1] @ rev. $probrev, details:\n$thiserrinfo\
                            \n##### end of detailed error info for ([file tail $mchfile1] @ rev) #####\n\n"
                }
                #msgbox "[file tail $mchfile1] with rev. $probrev: *OTHER*:\n$thiserrinfo"
                putsTxt "other error while executing probcli"
            }
        }

        #putsTxt $ausgabe
        putsTxt $execOutput
    }



    set runtime [ expr { $runtime / 1000000.0 } ]
    set newStdDevRuntime [ expr { $newStdDevRuntime / 1000000.0 } ]
    set newMaxRuntime [ expr { $newMaxRuntime / 1000000.0 } ]
    set newMinRuntime [ expr { $newMinRuntime / 1000000.0 } ]

    if {$performtests} {
        #putsTxt "runtime ${runtime} microseconds"
        putsTxt "runtime ${runtime} seconds (max: ${newMaxRuntime}, min: ${newMinRuntime}, stddev: ${newStdDevRuntime}) "
        if {$mchname1 ne ""} {
            lappend succmachines "[file tail $mchname1] @ rev. $probrev: ${runtime}s"
        } else {
            lappend succmachines "[file tail $mchfile1] @ rev. $probrev: ${runtime}s"
        }
    }

    set mchres "res_probcli(${probrev},${mchfile1},${mchparams1},${mchname1},${runtime},${timestamp},${iserror},${newStdDevRuntime},${newMaxRuntime},${newMinRuntime});"
    return $mchres
}







#proc msgbox {msg} { tk_messageBox -icon question -title "Debug" -message $msg }

#proc instant_checkout {} {
            #wm geometry . 1x1+[expr {[winfo screenwidth .]/2}]+[expr {[winfo screenheight .]/2}]
            #toplevel .checkoutwnd
            #wm geometry .checkoutwnd 700x300+[expr {[winfo screenwidth .]/2 - 350}]+[expr {[winfo screenheight .]/2 - 175}]
            #wm transient .checkoutwnd .
            #wm withdraw .
#}

proc readINI {firstrun} {
    eval global {homedir argv0  silentautobench}
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
                    puts "Could not read $inifile.\nPlease check permissions."
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
                    puts "Workspace found at \"[pwd]/$stddir\".\nWill continue."
                    set workspace [pwd]/$stddir
                } else {
                    puts "Error: workspace \"[pwd]/$stddir\" could not be accessed. There is a file with the same name."
                    exit -1
                }
            } else {
                puts "\"$inifile\" does not contain a workspace. A new workspace will be created at \"[pwd]/$stddir\".\n"
                set workspace [pwd]/$stddir
            }
        } else {
            if {[file exists $workspace] && ![file isdirectory $workspace]} {
                puts "Error: workspace \"$workspace\" could not be accessed. There is a file with the same name."
                exit -1
            } elseif {![file exists $workspace]} {
                puts "Last used workspace \"$workspace\" does not exist. Will use the default workspace instead."

                    # createWorkspaceDir $workspace


                                    set workspace [pwd]/$stddir
                                    createWorkspaceDir $workspace





            }
        }
    } else {
        puts "Checking workspace..."
        if !{[file exists [pwd]/$stddir]} {
            puts "No workspace found. Will create a new workspace at \"[pwd]/$stddir\"."

                set workspace [pwd]/$stddir


        } else {
            if {[file isdirectory [pwd]/$stddir]} {
                set workspace [pwd]/$stddir
            } else {
                puts "Error: workspace [pwd]/$stddir could not be accessed. There is a file with the same name."
                exit -1
            }
        }
        set inilist ""
    }


    if {![file exists $workspace]} {
        createWorkspaceDir $workspace;
    }

    if {![file exists $workspace]} {
        puts "Error: Workspace \"$workspace\" could not be accessed/created. Please check permissions."
        exit -1
    }
    puts "used workspace: $workspace"

    if {!($silentautobench)} {
        if { [catch {
            set f [open $inifile w]; #write ini file
            puts $f "#probtester_ini_file"
            puts $f "workspace=$workspace"
            close $f
        } ] } {
            puts "Could not write to [pwd]/$inifile. Please check permissions."
            exit -1
        }
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
    if {[file exists $workspaceini]} {
        #puts "$workspaceini found..."
        set f [open $workspaceini]
        set revslist [lsort -dictionary [split [read $f] "\n"]]; #read workspace ini file
        close $f
    } else {
        #puts "No workspace index file found."
        #tk_messageBox -icon warning -title "No workspace index" -message "The workspace does not contain an index file. You will have to check out at least one revision before you can run tests." -type ok -parent .
        #exit
    }

    cd $homedir; cd [file dirname $argv0]; # change to script directory

    return $returnval
}



proc showstart {} {
    #eval global {tcl_platform argv argv0 inputfileslist revslist resfileslist probrev probrev2}

  puts "\n***This non-gui version can only be used for automatic benchmarks.***\n"
        puts "possible parameters:\n\
         -autobench     : performs benchmark from autobench.input and saves the result to a running codespeed server.\n\
         -env <NAME>    : define the environment name (case sensitive) used for codespeed (must be defined in codespeed first!) (default is Cobra)\n\
         -rev <NUMBER>  : define the revision of probench (default is latest revision)\n\
         -path <PATH>   : use probcli in <PATH> (use -rev <NUMBER> to define which revision is in <PATH>)\n\
                          (default is getting from repository and building)\n\
         -silent        : do not save autobench results to a local csv file (only to codespeed server)\n\
         -nocodespeed   : do not save results to the codespeed server (only to a local csv file)
         -noterminal    : do not show xterm terminal during getting and building\n\
         -input <FILE>  : use the given input file instead of autobench.input\n\
         -currentdate   : use current date instead of revision date (revision date is taken from svn)\n\
         -iterations <NUMBER> : number of iterations for benchmarks (default: 2)\n\
         -timeout <MS>  : timeout for probcli (ms), 0=no timeout (default: 0)\n\n\
         To use an repository other than default,\n\
         create a file \"probench.repos\" in workspace directory that contains the repository url.\n\
         Default is https://cobra.cs.uni-duesseldorf.de/prob/\n"

}


proc set_runtests_window {testwnd} {
  puts "\n***not possible in non-gui version***\n"

}



####################################################################################
# #########################################################
#  Probench test file collection part #
# #########################################################



proc input_main {} {
  puts "\n***not possible in non-gui version***\n"

}


proc createWorkspaceDir {xdir} {
    if { [catch { file mkdir $xdir } ] } {
        puts "Could not create workspace: $xdir. Please check permissions."
        exit -1
    }
}


proc deleteDir {xdir} {
    if { [catch { file delete -force $xdir } ] } {
        puts "Could not delete files in workspace: $xdir. Please check permissions."
        exit -1
    }
}




proc input_showdiag {} {
  puts "\n***not possible in non-gui version***\n"

}


proc input_openTest {} {
  puts "\n***not possible in non-gui version***\n"

}
####################################################################################
####################################################################################
####################################################################################
# #########################################################
#  Probench checkout part
# #########################################################



proc checkout_main {} {
  puts "\n***not possible in non-gui version***\n"

}





proc get_repos_info  {getrev fromsource termcmd svncmd} {
    eval global {workspace workspaceini os silentrun}

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
                $svncmd info --xml --non-interactive $fromsource\@$getrev > $workspace/lastrevinfo.xml$trenner\
                echo Finished."



    if { [catch {
            switch -- $ii {
                0  {
                     #puts [exec -- $shellskript]
                     time "catch {exec $svncmd info --xml --non-interactive $fromsource\@$getrev > $workspace/lastrevinfo.xml}" 1
                   }
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

    eval global {getrevdate}
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
    #puts "DATUM: $getrevdate"
    deleteDir "$workspace/lastrevinfo.xml"

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

    #git@tuatara.cs.uni-duesseldorf.de:prob/prob_prolog.git
    set revtmpdir "revTEMP/tmp"
    set exportdir "PROB_HOME=$workspace/$revtmpdir/$reposdirname"

    puts "Vars: exportdir = $exportdir; gittodate=$gittodate; gitfromdate=$gitfromdate"

    if { [catch { deleteDir "$workspace/$revtmpdir"; file mkdir $workspace/$revtmpdir } ] } {
                    checkout_errmsg "Could not write to workspace $workspace.\nPlease check permissions."
                    cancelcheckout
                    return
    }
    cd $workspace/$revtmpdir
    if {$gittodate==0} {
      puts "getting info about current repo"
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
                sleep 17$trenner \
                echo Finished."
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
                sleep 17$trenner \
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
                    checkout_errmsg "Building was not successful."
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





proc checkout_runall {getrev fromsource termcmd svncmd makecmd} {
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

    set getrev [get_repos_info $getrev $fromsource $termcmd $svncmd]
    eval global {getrevdate}

    set revtmpdir "rev$getrev/tmp"
    set revdir "rev$getrev"
    set exportdir "PROB_HOME=$workspace/$revtmpdir"

    if { [catch { deleteDir "$workspace/$revdir"; file mkdir $workspace/$revtmpdir } ] } {
                    checkout_errmsg "Could not write to workspace $workspace.\nPlease check permissions."
                    cancelcheckout
                    return
    }


    set shellskript "echo check out from repository ${fromsource}...$trenner \
                $svncmd export --force -r $getrev --depth files $fromsource/$sourcedir/ $workspace/$revtmpdir$trenner \
                $svncmd export --force -r $getrev $fromsource/$sourcedir/lib $workspace/$revtmpdir/lib$trenner \
                $svncmd export --force -r $getrev $fromsource/$sourcedir/lib_platform_specific $workspace/$revtmpdir/lib_platform_specific$trenner \
                $svncmd export --force -r $getrev $fromsource/$sourcedir/src $workspace/$revtmpdir/src$trenner \
                $svncmd export --force -r $getrev $fromsource/$sourcedir/extensions $workspace/$revtmpdir/extensions$trenner \
                $svncmd export --force -r $getrev $fromsource/$sourcedir/plugins $workspace/$revtmpdir/plugins$trenner \
                $svncmd export --force -r $getrev $fromsource/$sourcedir/gradle $workspace/$revtmpdir/gradle$trenner \
                $svncmd export --force -r $getrev $fromsource/$sourcedir/tcl $workspace/$revtmpdir/tcl$trenner \
                cd $workspace/$revtmpdir
                echo setting PROB_HOME...$trenner \
                $exportcmd $exportdir$trenner \
                echo running make$trenner \
                $makecmd$trenner \
                # sicstus$trenner \
                echo Finished."




    if { [catch {
            switch -- $ii {
                0  {
                     #puts [exec -- $shellskript]
                      time "catch {exec $svncmd export --force -r $getrev --depth files $fromsource/$sourcedir/ $workspace/$revtmpdir}"
                      time "catch {exec $svncmd export --force -r $getrev $fromsource/$sourcedir/lib $workspace/$revtmpdir/lib}"
                      time "catch {exec $svncmd export --force -r $getrev $fromsource/$sourcedir/lib_platform_specific $workspace/$revtmpdir/lib_platform_specific}"
                      time "catch {exec $svncmd export --force -r $getrev $fromsource/$sourcedir/src $workspace/$revtmpdir/src}"
                      time "catch {exec $svncmd export --force -r $getrev $fromsource/$sourcedir/extensions $workspace/$revtmpdir/extensions}"
                      time "catch {exec $svncmd export --force -r $getrev $fromsource/$sourcedir/plugins $workspace/$revtmpdir/plugins}"
                      time "catch {exec $svncmd export --force -r $getrev $fromsource/$sourcedir/gradle $workspace/$revtmpdir/gradle}"
                      time "catch {exec $svncmd export --force -r $getrev $fromsource/$sourcedir/tcl $workspace/$revtmpdir/tcl}"
                      cd $workspace/$revtmpdir
                      time "catch {exec $exportcmd $exportdir}"
                      time "catch {exec $makecmd}"
                   }
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
            #evntl linux und linux_64 unterscheiden?
            file copy -force $workspace/$revtmpdir/lib_platform_specific/linux/parseCspForPl $workspace/$revdir/lib/parseCspForPl
        } elseif [string match $os win] {
            file copy -force $workspace/$revtmpdir/lib_platform_specific/windows/parsecspforpl.exe $workspace/$revdir/lib/parsecspforpl.exe
        }

    } ] } {
                    checkout_errmsg "Building was not successful."
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
        checkout_errmsg "Building was not successful."
        #puts "building was not successful"
        deleteDir "$workspace/$revdir"
        exit -1
    }


    #############
    if { [catch {
                set f [open $workspace/$revdir/revision.info w]
                puts $f "$getrev (date: ${getrevdate})"
                close $f
    } ] } {
        checkout_errmsg "Could not write revision.info to $workspace/$revdir. Please check permissions. Will continue anyway..."
    }
    #############
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
         putsTxt "Revision $getrev was successfully checked out."
        #tk_messageBox -icon info -message "Revision $getrev was successfully checked out and can be used with revbench.tcl." -type ok -parent .checkoutwnd
    } else {
        putsTxt "Revision $getrev was successfully checked out."
    }
    return $getrev
}


proc checkout_errmsg {msg} {
            eval global {silentrun}
            if {!$silentrun} {
                    putsTxt $msg
                    #tk_messageBox -icon error -title "error" -message $msg -type ok -parent .checkoutwnd
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
 putsTxt "Revision $getrev was successfully checked out."
}
####################################################################################
####################################################################################


main
