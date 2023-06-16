#!/usr/bin/env tclsh

 package provide excel 1.1

   namespace eval excel:: {
           variable workbooks 0
           variable workbooksArray
           variable workSheets
           variable workSheetsArray
           variable styles
           variable columnDefault
           variable data
           variable rowCounter
           variable columnsIndex
           array set columnsIndex [list A 1 B 2 C 3 D 4 E 5 F 6 G 7 H 8 I 9 J 10 K\
                     11 L 12 M 13 N 14 O 15 P 16 Q 17 R 18 S 19 T 20 U 21 V 22 W 23 X 24 Y 25 Z 26 AA 27 AB 28 AC 29 AD 30 AE 31]
   }
   proc excel::createWorkbook {} {
   #
   # @comment  create a workbook pointer
   # @result         pointer to created workbook
   #
           incr excel::workbooks
           set workbookName workbook$excel::workbooks
           set excel::workbooksArray($workbookName) 1
           return $workbookName
   }

   proc excel::createWorkSheet {workbook name} {
   #
   # @comment  create a worksheet pointer
   # @argument workbook pointer to a workbook
   # @argument name name of the worksheet
   # @result         pointer to a worksheet
   #
           variable data
           if {[info exists excel::workbooksArray($workbook)]} {
                   if {![info exists ::excel::workSheets($workbook)]} {
                           set excel::workSheets($workbook) 1
                   } else {
                           incr excel::workSheets($workbook)
                   }
                   set workSheetName workSheet[string range ${workbook} 8 end].$excel::workSheets($workbook)
                   set data(workSheet,$::excel::workSheets($workbook),name) $name
                   set data(workSheet,$::excel::workSheets($workbook)) $workSheetName
                   set data(workSheet,$workSheetName) 1
                   set excel::rowCounter($workSheetName) 0
                   return $workSheetName
           } else {
                   error "$workbook is not a valid workbook"
           }
   }
   proc excel::createStyle {workbook args} {
   #
   # @comment  create an excel style
   # @argument workbook pointer to a workbook
   # @argument args argument list
   # @result style pointer
   #
           variable data
           if {[info exists excel::styles($workbook)]} {
           incr excel::styles($workbook)
           } else {
           set excel::styles($workbook) 2
           }
           set styleName s$excel::styles($workbook)
           foreach {name value} $args {
   # check that name is valid
                   if {[lsearch "-font -fontcolor -background -bold" $name]==-1} {
                           error "style option $name option is not supported"
                   }
                   set data($workbook,styles,$styleName,$name) $value
           }
           return $styleName

   }
   proc excel::setColumnType {workSheet columnIndex type} {
   #
   # @comment define a column type
   # @argument workSheet pointer to a workSheet
   # @argument columnIndex index of column
   # @argument type of column
   # @result  column type is changed
   #
           variable data
           _checkSpreadSheet $workSheet
           set data($workSheet,row,$columnIndex,type) [string totitle $type]
   }

   proc excel::_checkSpreadSheet {workSheet} {
           variable data
           if {![info exists data(workSheet,$workSheet)]} {
                   error "$workSheet is not a valid workSheet"
           }
   }

   proc excel::addRow {workSheet columnsDataList} {
   #
   # @comment add row to excel worksheet
   # @argument workSheet pointer to a workSheet
   # @argument args list of variables
   # @result row id
   #
           variable data
           set i 0
           incr excel::rowCounter($workSheet)
           set data($workSheet,$excel::rowCounter($workSheet),length) [llength $columnsDataList]
           foreach arg $columnsDataList {
                   incr i
                   if {[llength $arg]>1} {
                           if {[lsearch [list String Number] [lindex $arg 1]]!=-1} {

                                   if {[llength $arg]>2} {
                                           set data($workSheet,$excel::rowCounter($workSheet),$i,style) [lindex $arg end]
                                   }
                                   set data($workSheet,$excel::rowCounter($workSheet),$i,type) [string totitle [lindex $arg end-1]]
                                   set value [lindex $arg 0]
                           } else {
                                   set value $arg
                           }
                   } else {
                           set value $arg
                   }
                   set data($workSheet,$excel::rowCounter($workSheet),$i,data) $value
           }
                   return row$excel::rowCounter($workSheet)
   }
   proc excel::asXml {workbook} {
   #
   # @comment returns excel workbook as xml
   # @argument workbook pointer to a workbook
   # @result workbook xml representation
   #
           variable data
           variable rowCounter
           set xml "<?xml version='1.0'?>\
   <?mso-application progid='Excel.Sheet'?>\
   <Workbook xmlns='urn:schemas-microsoft-com:office:spreadsheet'\
   xmlns:o='urn:schemas-microsoft-com:office:office'\
   xmlns:x='urn:schemas-microsoft-com:office:excel'\
   xmlns:ss='urn:schemas-microsoft-com:office:spreadsheet'\
   xmlns:html='http://www.w3.org/TR/REC-html40'>\
   <DocumentProperties xmlns='urn:schemas-microsoft-com:office:office'>\
   <Author>Ashrait</Author>\
   <Created>[clock format [clock seconds] -format {%Y-%m-%dT%H:%M:%SZ}]</Created>\
   <Company>Xor Technologies</Company>\
   </DocumentProperties>\
   <Styles>\
    <Style ss:ID='Default' ss:Name='Normal'>\
      <Alignment ss:Vertical='Bottom'/>\
      <Font x:CharSet='177'/>\
     </Style>\
     <Style ss:ID='s21'>\
      <Alignment ss:Horizontal='Center' ss:Vertical='Bottom'/>\
      <Font x:Family='Swiss' ss:Color='#000080' ss:Bold='1'/>\
      <Interior ss:Color='#99CCFF' ss:Pattern='Solid'/>\
     </Style>\
     <Style ss:ID='s22'>\
      <Alignment ss:Vertical='Bottom'/>\
      <Borders>\
       <Border ss:Position='Top' ss:LineStyle='Double' ss:Weight='3'/>\
      </Borders>\
      <Font x:CharSet='177' x:Family='Swiss' ss:Bold='1'/>\
     </Style>"
   if {[info exists excel::styles($workbook)]} {
           for {set d 2} {$d<=$excel::styles($workbook)} {incr d} {
                   set styleName s$d
                   append xml "<Style ss:ID='$styleName'><Alignment ss:Vertical='Bottom'/>"
                   if {[info exists data($workbook,styles,$styleName,-font)] || [info exists data($workbook,styles,$styleName,-fontcolor)]} {
                           append xml "<Font x:CharSet='177'"
                           if {[info exists data($workbook,styles,$styleName,-font)]} {
                                   append xml " ss:FontName='$data($workbook,styles,$styleName,-font)'"
                           }
                           if {[info exists data($workbook,styles,$styleName,-fontcolor)]} {
                                   append xml " ss:Color='$data($workbook,styles,$styleName,-fontcolor)'"
                           }
                           if {[info exists data($workbook,styles,$styleName,-bold)]} {
                                   append xml " ss:Bold='1'"
                           }
                           append xml "/>"
                   }
                   if {[info exists data($workbook,styles,$styleName,-background)]} {
                           append xml "<Interior ss:Color='$data($workbook,styles,$styleName,-background)' ss:Pattern='Solid'/>"
                   }
                   append xml "</Style>"
           }
   }
           append xml "</Styles>"
           for {set d 1} {$d<=$excel::workSheets($workbook)} {incr d} {
                   append xml "<Worksheet ss:Name='$excel::data(workSheet,$d,name)'>\
   <Table x:FullColumns='1' x:FullRows='1'>"
                   set workSheet $excel::data(workSheet,$d)
                   for {set i 1} {$i<=$excel::rowCounter($workSheet)} {incr i} {
                           append xml "<Row>"
                           for {set j 1} {$j<=$data($workSheet,$i,length)} {incr j} {

                                   set dataValue $data($workSheet,$i,$j,data)
                                   if {[string index $dataValue 0]=="="} {
                                           append xml "<Cell ss:Formula='$dataValue'"
                                           set dataValue ""
                                           set numeric 1
                                   } else {
                                           if {[string is double -strict $dataValue]} {
                                                   set numeric 1
                                           } else {
                                                   set numeric 0
                                           }
                                           append xml "<Cell"
                                   }
                                   if {[info exists data($workSheet,$i,$j,type)]} {
                                           set type $data($workSheet,$i,$j,type)
                                   } else {
                                           if {[info exists data($workSheet,row,$j,type)]} {
                                           set type $data($workSheet,row,$j,type)
                                           } elseif {$numeric} {
                                                   set type "Number"
                                           } else {
                                                   set type "String"
                                           }
                                   }
                                   if {[info exists data($workSheet,$i,$j,style)]} {
                                           append xml " ss:StyleID='$data($workSheet,$i,$j,style)'>"
                                   } else {
                                           append xml ">"
                                   }
                                   append xml "<Data ss:Type='$type'>$dataValue</Data></Cell>"
                           }
                           append xml "</Row>"
                   }
                   append xml "</Table></Worksheet>"
           }
           append xml "</Workbook>"
   }
   proc excel::deleteWorkbook {workbook} {
   #
   # @comment  delete a workbook pointer
   # @argument workbook pointer to a workbook
   # @result undecoded string
   #
           variable data
           for {set d 1} {$d<=$excel::workSheets($workbook)} {incr d} {
                   array unset data $d
                           set workSheet $excel::data(workSheet,$d)
                   for {set i 1} {$i<=$excel::rowCounter($workSheet)} {incr i} {
                           array unset data $workSheet*
                   }
                   unset $excel::rowCounter($workSheet)
           }
   }
   proc excel::addTitle {workSheet columnsDataList} {
   #
   # @comment  delete a workbook pointer
   # @argument workbook pointer to a workbook
   # @result undecoded string
   #
           foreach arg $columnsDataList {
                   lappend newArgs  [list $arg String s21]
           }
           addRow $workSheet $newArgs
   }
   proc excel::addTotal {workSheet columnsDataList} {
   #
   # @comment  delete a workbook pointer
   # @argument workbook pointer to a workbook
   # @result undecoded string
   #
           foreach arg $columnsDataList {
                   lappend newArgs  [list $arg String s22]
           }
           addRow $workSheet  $newArgs
   }
   proc excel::setCell {workSheet row column value} {
   #
   # @comment  delete a workbook pointer
   # @argument workbook pointer to a workbook
   # @result undecoded string
   #
           variable data
                   set data($workSheet,$row,$excel::columnsIndex($column),data) $value
   }
   proc excel::getCurrentRow {workSheet} {
   #
   # @comment  delete a workbook pointer
   # @argument workbook pointer to a workbook
   # @result undecoded string
   #
           return $excel::rowCounter($workSheet)
   }
