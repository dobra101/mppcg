
   global fileName
   global modState
   global undoVar

   set fileName "None"
   set modState ""
   set undoVar  0


   text .t -background white -wrap none
   # Example 1: The Modified event will update a text label
   bind .t <<Modified>>  updateState
   # Example 2: The Selection event will create a tag that
   #            duplicates the selection
   bind .t <<Selection>> duplicateSelection

   frame .l
   label .l.l -text "File: "
   label .l.f -textvariable fileName
   label .l.m -textvariable modState

   grid .l.l -sticky w   -column 0 -row 0
   grid .l.f -sticky w   -column 1 -row 0
   grid .l.m -sticky e   -column 2 -row 0

   grid columnconfigure .l 1 -weight 1

   frame .b
   button .b.l -text "Load"   -width 8 -command loadFile
   button .b.s -text "Save"   -width 8 -command saveFile
   button .b.i -text "Indent" -width 8 -command blockIndent

   checkbutton .b.e -text "Enable Undo" -onvalue 1 -offvalue 0 -|   |   variable undoVar
   trace variable undoVar w setUndo
   button .b.u -text "Undo"     -width 8 -command "undo"
   button .b.r -text "Redo"     -width 8 -command "redo"
   button .b.m -text "Modified" -width 8 -command ".t edit modified on"

   grid .b.l -row 0 -column 0
   grid .b.s -row 0 -column 1
   grid .b.i -row 0 -column 2
   grid .b.e -row 0 -column 3
   grid .b.u -row 0 -column 4
   grid .b.r -row 0 -column 5
   grid .b.m -row 0 -column 6

   grid columnconfigure .b 0 -weight 1
   grid columnconfigure .b 1 -weight 1
   grid columnconfigure .b 2 -weight 1
   grid columnconfigure .b 3 -weight 1
   grid columnconfigure .b 4 -weight 1
   grid columnconfigure .b 5 -weight 1


   grid .l -sticky ew   -column 0 -row 0
   grid .t -sticky news -column 0 -row 1
   grid .b -sticky ew   -column 0 -row 2

   grid rowconfigure    . 1 -weight 1
   grid columnconfigure . 0 -weight 1



   proc updateState {args} {
      global modState

      # Check the modified state and update the label
      if { [.t edit modified] } {
         set modState "Modified"
      } else {
         set modState ""
      }
   }


   proc setUndo {args} {
      global undoVar

      # Turn undo on or off
      if { $undoVar } {
         .t configure -undo 1
      } else {
         .t configure -undo 0
      }
   }

   proc undo {} {
      # edit undo throws an exception when there is nothing to
      # undo. So catch it.
      if { [catch {.t edit undo}] } {
         bell
      }
   }

   proc redo {} {
      # edit redo throws an exception when there is nothing to
      # undo. So catch it.
      if { [catch {.t edit redo}] } {
         bell
      }
   }

   proc loadFile {} {

      set file [tk_getOpenFile]
      if { ![string equal $file ""] } {
         set fileName $file
         set f [open $file r]
         set content [read $f]
         set oldUndo [.t cget -undo]

         # Turn off undo. We do not want to be able to undo
         # the loading of a file
         .t configure -undo 0
         .t delete 1.0 end
         .t insert end $content
         # Reset the modified state
         .t edit modified 0
         # Clear the undo stack
         .t edit reset
         # Set undo to the old state
         .t configure -undo $oldUndo
      }
   }

   proc saveFile {} {
      # The saving bit is not actually done
      # So the contents in the file are not updated

      # Saving clears the modified state
      .t edit modified 0
      # Make sure there is a separator on the undo stack
      # So we can get back to this point with the undo
      .t edit separator
   }

   proc blockIndent {} {
      set indent "   "

      # Block indent should be treated as one operation from
      # the undo point of view

      # if there is a selection
      if { ![catch {.t index sel.first} ] } {
         scan [.t index sel.first] "%d.%d" startline startchar
         scan [.t index sel.last]  "%d.%d" stopline  stopchar
         if { $stopchar == 0 } {
            incr stopline -1
         }

         # Get the original autoseparators state
         set oldSep [.t cget -autoseparators]
         # Turn of automatic insertion of separators
         .t configure -autoseparators 0
         # insert a separator before the edit operation
         .t edit separator
         for {set i $startline} { $i <= $stopline} {incr i} {
            .t insert "$i.0" $indent
         }
         .t tag add sel $startline.0 "$stopline.end + 1 char"
         # insert a separator after the edit operation
         .t edit separator
         # put the autoseparators back in their original state
         .t configure -autoseparators $oldSep
      }
   }

   proc duplicateSelection {args} {
      .t tag configure dupsel -background tomato
      .t tag remove dupsel 1.0 end

      if { ![catch {.t index sel.first} ] } {
         eval .t tag add dupsel [.t tag ranges sel]
      }
   }

