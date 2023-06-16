proc procLoadPromelaFile {} {
    global strFilename curFileTypeOpened lib_dir app_dir cspstrFilename text_editor_arr
    if {$strFilename != ""} {
        set curFileTypeOpened "Promela"
        set cspstrFilename ""
        set promelastrFilename {}
        append promelastrFilename $strFilename ".pl"
        if [file exists $promelastrFilename] { file delete $promelastrFilename }
        set MacJava6 "/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Commands/java"
        if [file exists $MacJava6] {set MyJava $MacJava6} else {set MyJava "java"}
        # puts "exec $MyJava -jar $lib_dir/Promela.jar $strFilename $app_dir"
        exec $MyJava -jar $lib_dir/Promela.jar $strFilename $app_dir

	    if [file exists $promelastrFilename] {
            procEnableReopen
            procShowSourceCode $strFilename
            if [prolog tcltk_open_promela_file('$promelastrFilename')] {
              # the file exists, so load it.
                procFinishLoading
            } else {
                procErrorBoxWithErrorMessages "Loading of parsed Promela file failed!"
                procFinishUnsuccessfulLoading
            }
            if {!$text_editor_arr(ctext_package)} {
    			      procDoOnTheFlyPromelaSyntaxColouring  .main.frmSource.text
            } else {
                procDoPromelaSyntaxColouring .main.frmSource.text
            }
	    } else {
		   tkErrorBox "Parsing of Promela file failed!\nNote: the parser requires Java6 or newer."
		   procFinishUnsuccessfulLoading
	    }
    }
}


proc procDoOnTheFlyPromelaSyntaxColouring {textWidget} {
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {
        ctext::clearHighlightClasses $textWidget

        prolog preferences:get_preference(sh_operators,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClass $textWidget syntax_operator $col {atomic d_step run printf}
        ctext::addHighlightClassForRegexp $textWidget syntax_operator1 $col {(\?|!|->|;|-(-)?|\+(\+)?|\*|::|\]|\[)}

        prolog preferences:get_preference(sh_type_colour,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClass $textWidget syntax_type $col {mtype chan active proctype of byte init bool int typedef}
        ctext::addHighlightClassForRegexp $textWidget syntax_type1 $col {(\{|\}|\#define)}

        prolog preferences:get_preference(sh_top_level_keywords,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClass $textWidget syntax_keywords $col {if fi else goto skip do od for rof break unless provided}

        prolog preferences:get_preference(sh_assignments_colour,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClass $textWidget syntax_assignment $col {assert timeout}
        ctext::addHighlightClassForRegexp $textWidget syntax_assignment1 $col {(::|--)}

        prolog preferences:get_preference(sh_logical_colour,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClass $textWidget syntax_logical $col {full nfull true empty nempty}
        ctext::addHighlightClassForRegexp $textWidget syntax_logical1 $col {(==|\!=|>|<|<=|>=|=<|&&|\|\|)}

        prolog preferences:get_preference(sh_comments,Colour)
	set col $prolog_variables(Colour)
        ctext::addHighlightClassForRegexp $textWidget syntax_comment1 $col {%[^\n\r]*}
        ctext::addHighlightClassForRegexp $textWidget multiline_comment $col {/\*(.|[\n\r]*)*\*/}

        prolog preferences:get_preference(sh_unsupported_background,Colour)
        set col $prolog_variables(Colour)
        ctext::addHighlightClass $textWidget syntax_unsupported black {extern inline}
        $textWidget tag configure syntax_unsupported -background $col

        procHighlightTextSpecial $textWidget 1.0 end
  }
}


proc procDoPromelaSyntaxColouring {textWidget} {
  procUpdateSyntaxColours $textWidget
  if [prolog preferences:get_preference(do_syntax_highlighting,true)] {

      procMarkRegExpressionPair $textWidget {/\*} {\*/} syntax_comment

	  set pattern {\?|!|->|;|-(-)?|\+(\+)?|\*|::|\]|\[|((?=\m)(atomic|d_step|run|printf)(?=\M))}
	  procMarkRegExpression $textWidget $pattern syntax_operator

	  set pattern {(?=\m)(mtype|chan|active|proctype|of|byte|init|bool|int|typedef)(?=\M)|#define|\{|\}}
	  procMarkRegExpression $textWidget $pattern syntax_type

	  set pattern {((?=\m)(if|fi|else|goto|skip|do|od|for|rof|break|unless|provided)(?=\M))}
	  procMarkRegExpression $textWidget $pattern syntax_keyword

	  set pattern {(?=\m)(assert|timeout)(?=\M)|::|--|(=(?=\M))}
	  procMarkRegExpression $textWidget $pattern syntax_assignment

	  set pattern {((?=\m)(full|nfull|true|empty|nempty)(?=\M))|==|\!=|>|<|<=|>=|=<|&&|\|\|}
	  procMarkRegExpression $textWidget $pattern syntax_logical

	  set pattern {%(.*)}
	  procMarkRegExpression $textWidget $pattern syntax_comment
      # puts "Phase 6"

	  set pattern {(?=\m)(extern|inline)(?=\M)}
	  procMarkRegExpression $textWidget $pattern syntax_unsupported
  }
}
