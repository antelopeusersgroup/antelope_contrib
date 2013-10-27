proc anzadp { orb }  {

global aparams
global Pfile
global aparlist
global adpars
global ASpecpar
global Aspecpar
global Font12
global AWinPar 
global AWinEnt 
global AWinEntL
global AMainWin
global apfdasarr
global aorbselect
global adassel
global adcsel
global archive_msg 
global pfdasid
global pfdas


set AMainWin .anzadp
set ASpecpar "null"
set Aspecpar ""
set aparams " "
set aparlist ""
set allpar    allpar
set noallpar  noallpar
set selpar    selpar
set pfdas 	""
set pfdc 	""
set pfdasid 	""
set archive_msg "START_ARCHIVE"

toplevel $AMainWin
wm title $AMainWin "ANZA DP LISTIN"
#
set daspar { _BUFDEL _BATT _CRC _CHKSUM _DASDN _NOCMD _SEC _MSEC _RTXRCV _RTXREQ _RTXSKP }
set dcpar { _1PPS _SEQERR _STAT _UNLOCK _LLOCK } 
 
# Read parameter file

  if { [catch "pfgetlist @$Pfile#AnzaDc" error] } {
     puts "can't find DC specifications in a $Pfile parameter file\n"
     exit 1
  }  else { 
     set pfdc [pfgetlist @$Pfile#AnzaDc]
  }

  if { [catch "pfget $Pfile AnzaSelectDc" error] } {
       set adcsel ""
  } else {
       set adcsel [pfget $Pfile AnzaSelectDc] 
  }
# 
  if { [catch "pfget $Pfile OrbSelect" error] } {
       set aorbselect ""
  } else {
       set aorbselect [pfget $Pfile OrbSelect] 
  } 
  
  if { [catch "pfget $Pfile AnzaSelectDas" error] } {
       set adassel ""
  } else {
       set adassel [pfget $Pfile AnzaSelectDas] 
  }
 

  if { [catch "pfget $Pfile ParFile" error] } {
       puts "can't find ParFile specifications in a $Pfile parameter file"
      exit 1
  }  else { 
      set parfile [pfget $Pfile ParFile] 
  }

  if { [catch "pfgetlist @$parfile#Site" error] } {
     puts "can't find Das specifications in a $parfile parameter file"
     exit 1
  }  else { 
     set apfdasarr  [pfgetlist @$parfile#DasId]
     foreach pfd  $apfdasarr  {
       set notblank 0;
       set tmp [split $pfd " "]
       set id [lindex $tmp 0]
       if { [string compare $id 0] != 0 }  {
          lappend pfdasid $id
          set all [llength $tmp]
          for {set j 1} {$j < $all} {incr j}  {
               set newpar [lindex $tmp $j]
               set newpar1 [string trim $tmp " "]
               if { [string length $newpar ] > 0 }  {
                  incr notblank
                  if { $notblank == 1 }  {
                       lappend pfdas $newpar 
                       set j $all
	          } 
	       } 
           } 
       }

     }
  }

  set i 1
 
  foreach site $pfdc {
      foreach par $dcpar  {
         set newpar $site$par
         lvarcat aparlist $i
         set adpars($i) $newpar
         set pbut($i) [format $AMainWin.par.par.but%s $i] 
         incr i
      }
  }
 
  foreach site $pfdas {
      foreach par $daspar  {
         set newpar $site$par
         lvarcat aparlist $i
         set adpars($i) $newpar
         set pbut($i) [format $AMainWin.par.par.but%s $i] 
         incr i
      }
  }

# set widgets 
 
frame $AMainWin.par -relief ridge -bg DarkSeaGreen -borderwidth 4 
listbox $AMainWin.par.par -selectmode multiple  \
        -font $Font12 -width 30 -height 18 \
        -relief sunken  \
        -bg honeydew \
        -selectbackground IndianRed \
	-yscrollcommand "$AMainWin.par.scroll set" 
#
set AWinPar $AMainWin.par.par

set num [llength $aparlist]

loop i 0 $num {
   set ind [lindex $aparlist $i]
   $AWinPar insert end $adpars($ind) 
}

bind $AWinPar <ButtonRelease-1> {
   set par [selection get];
   anza_selectpars $par $AWinPar "nullpar" 
}

scrollbar $AMainWin.par.scroll \
    -bg DarkSeaGreen \
    -command "$AWinPar yview"

#
frame $AMainWin.mbar -relief ridge -bd 2 -bg DarkSeaGreen
    
menubutton $AMainWin.mbar.func \
         -font $Font12 \
         -text DIAGPAR \
         -underline 0 \
         -menu $AMainWin.mbar.func.menu \
         -bg IndianRed \
         -activebackground red 

menu $AMainWin.mbar.func.menu  -bg honeydew -activebackground honeydew  
$AMainWin.mbar.func.menu add command -label "SelectAll" \
         -command "anza_selectpars nullpar $AMainWin.par.par $allpar " \
	 -activebackground red
$AMainWin.mbar.func.menu add command -label "DeleteAll" \
         -command "anza_selectpars nullpar $AMainWin.par.par $noallpar " \
         -activebackground red
$AMainWin.mbar.func.menu add command -label "Select" \
         -command "anza_makentry $AMainWin.mbar;anza_selectpars nullpar $AMainWin.par.par $selpar" \
         -activebackground red

button $AMainWin.mbar.quit \
 -font $Font12 -text "DISMISS" -bd 2 \
 -activebackground red -bg IndianRed -command "destroy $AMainWin"

pack $AMainWin.mbar.func -side left -padx 1m -pady 1m 
pack $AMainWin.mbar.quit -side right -padx 1m -pady 1m 

pack $AMainWin.par.scroll -side right -fill y 
pack $AMainWin.par.par -fill both -expand 1 

frame $AMainWin.cmd -relief ridge -bd 2 -bg DarkSeaGreen 
button $AMainWin.cmd.showpar \
         -font $Font12 \
         -text "SHOWPAR" \
         -bd 3 \
         -relief raised\
         -bg IndianRed \
         -activebackground red \
         -command "showanza $orb"

button $AMainWin.cmd.save \
         -font $Font12 \
         -text $archive_msg \
         -bd 3 \
         -relief raised\
         -bg IndianRed \
         -activebackground red \
         -command "check_archive $orb {$aorbselect} {$AMainWin.cmd.save} "

#pack $AMainWin.cmd.showpar $AMainWin.cmd.save -side left -padx 1m -pady 1m -fill x 
pack $AMainWin.cmd.showpar -padx 1m -pady 1m -fill x 

pack $AMainWin.mbar -fill x -side top 
pack $AMainWin.par -side top -fill both -expand 1
pack $AMainWin.cmd -side bottom -fill x 
}
proc anza_selectpars { arg win mode } {

   global aparams aparlist adpars ASpecpar

   switch $mode {

      nullpar
         {
            set aparams $arg
         }

     noallpar  
         {
           set num [llength $aparlist]
           incr num -1
           $win selection clear 0 $num
           set ASpecpar "null"
           set aparams " "
         }

      allpar
         {
           set num [llength $aparlist]
           set last $num
           incr num -1
           incr last 1
           $win selection set 0 $num
           loop i 1 $last  {
              lappend aparams $adpars($i)
           }
         }

      selpar
         {
            set want "" 
            set gotone 0
 
            set want $ASpecpar
            set ASpecpar "null"
             
            set num [llength $aparlist]
            incr num 1 
            loop i 1 $num  {
            
               if { [string match $want $adpars($i)] }  { 

                  if { $gotone == 0 } { set gotone $i }
                  $win selection set [expr $i-1]
                  lappend aparams $adpars($i)

               }
            }
                  
            if { $gotone != 0 } { $win see $gotone }
         }

   }

}

proc anza_makentry {win}  {

    global AWinEnt AWinEntL Font12 AWinPar ASpecpar Aspecpar

    label $win.enlable \
	-font $Font12 \
        -bd 2 -relief ridge \
	-text "SELECT PARAMETERS:" \
        -bg IndianRed 


    entry $win.entry \
	-width 15  \
	-relief sunken   \
	-bd 2   \
	-textvariable Aspecpar \
        -bg honeydew

    pack $win.entry $win.enlable -side right -padx 1m -pady 2m

    set AWinEnt $win.entry
    set AWinEntL $win.enlable
 
    bind $win.entry <Return>  {
      set ASpecpar $Aspecpar

      destroy $AWinEnt 
      destroy $AWinEntL
      anza_selectpars "nullpar" $AWinPar "selpar"
 
    }
} 