proc gainpar { orb }  {

global Pfile
global gparams
global gparlist
global gpars
global gSpecpar
global gspecpar
global Font12
global GWinPar 
global GWinEnt 
global GWinEntL
global gainsel
global gparfile

set GW .gsite
toplevel $GW
wm title $GW "DASes for GAIN"
set gSpecpar "null"
set gspecpar ""
set gparams " "
set allpar    "allpar"
set noallpar  "noallpar"
set selpar    "selpar"
set gparlist ""
set tmplist ""

# Read parameter file
 
  if { [catch "pfget $Pfile SelectDas" error] } {
       set gainsel ""
  } else {
       set gainsel [pfget $Pfile SelectDas] 
  }
 
# 
  if { [catch "pfget $Pfile ParFile" error] } {
     puts "can't find ParFile specifications in a $Pfile parameter file"
     exit 1
  }  else { 
       set gparfile [pfget $Pfile ParFile] 
  }
  
  if { [catch "pfgetlist @$gparfile#Site" error] } {
     puts "can't find Site specifications in a $gparfile parameter file"
     exit 1
  }  else { 
      set sitelist  [pfgetlist @$gparfile#Site]
      foreach pfd  $sitelist  {
         set notblank 0;
	 set tmp [split $pfd " "]
         set net [lindex $tmp 0] 
         if { [string match BBA* $net ] } {
	    set all [llength $tmp]
	    for {set j 1} {$j < $all} {incr j}  {
		set newpar [lindex $tmp $j]
		set newpar1 [string trim $tmp " "]
                if { [string length $newpar ] > 0 }  {
		    incr notblank
		    if { $notblank == 2 }  {
	               lappend tmplist $newpar
		       set j $all
                    }
		}
	    }
	 }
      }
  }

set stalist [lsort $tmplist] 
set i 1
set new "null"

foreach newpar $stalist {
     if { [string compare $new $newpar] != 0 }  {
	set new $newpar
	lvarcat gparlist $i
        set gpars($i) $newpar
        set pbut($i) [format $GW.par.par.but%s $i] 
        incr i
     }
}
 
frame $GW.par -relief ridge -bg DarkSeaGreen -borderwidth 4 
listbox $GW.par.par -selectmode multiple  \
        -font $Font12 -width 30 -height 18 \
        -relief sunken  \
        -bg honeydew \
        -selectbackground IndianRed \
	-yscrollcommand "$GW.par.scroll set" 
#
set GWinPar $GW.par.par

set num [llength $gparlist]

loop i 0 $num {
   set ind [lindex $gparlist $i]
   $GWinPar insert end $gpars($ind) 
}

bind $GW.par.par <ButtonRelease-1>  {
   set par [selection get];
   selectgpars $par $GWinPar "nullpar" 
}

scrollbar $GW.par.scroll \
    -bg DarkSeaGreen \
    -command "$GW.par.par yview"

#
frame $GW.mbar -relief ridge -bd 2 -bg DarkSeaGreen
    
menubutton $GW.mbar.func \
         -font $Font12 \
         -bd 2\
         -text DIAGPAR \
         -underline 0 \
         -menu $GW.mbar.func.menu \
         -bg IndianRed \
         -activebackground red 

menu $GW.mbar.func.menu  -bg honeydew -activebackground honeydew  
$GW.mbar.func.menu add command -label "SelectAll" \
         -command "selectgpars nullpar $GW.par.par $allpar " \
	 -activebackground red
$GW.mbar.func.menu add command -label "DeleteAll" \
         -command "selectgpars nullpar $GW.par.par $noallpar " \
         -activebackground red
$GW.mbar.func.menu add command -label "Select" \
         -command "gmakentry $GW.mbar;selectgpars nullpar $GW.par.par $selpar" \
         -activebackground red

button $GW.mbar.quit \
 -font $Font12 -text "DISMISS" -bd 2 \
 -activebackground red -bg IndianRed -command " selectgpars nullpar $GW.par.par $noallpar; destroy $GW "

pack $GW.mbar.func -side left -padx 1m -pady 1m 
pack $GW.mbar.quit -side right -padx 1m -pady 1m 

pack $GW.par.scroll -side right -fill y
pack $GW.par.par -fill both -expand 1

frame $GW.cmd -relief ridge -bd 2 -bg DarkSeaGreen 
button $GW.cmd.showpar \
         -font $Font12 \
         -text "SHOWGAIN" \
         -bd 3 \
         -relief raised \
         -bg IndianRed \
         -activebackground red \
         -command "showgain $orb"

pack $GW.cmd.showpar -padx 1m -pady 1m -expand 1 -fill x 

pack $GW.mbar -fill x -side top 
pack $GW.par $GW.cmd -side top -fill both -expand 1
pack $GW.cmd -side bottom -fill x
}
proc selectgpars { arg win mode } {

   global gparams gparlist gpars gSpecpar

   switch $mode {

      nullpar
         {
            set gparams $arg
         }

     noallpar  
         {
           set num [llength $gparlist]
           incr num -1
           $win selection clear 0 $num
           set gSpecpar "null"
           set gparams " "
         }

      allpar
         {
           set num [llength $gparlist]
           set last $num
           incr num -1
           incr last 1
           $win selection set 0 $num
           loop i 1 $last  {
              lappend gparams $gpars($i)
           }
         }

      selpar
         {
            set want "" 
            set gotone 0
            set want $gSpecpar
            set gSpecpar "null"
             
            set num [llength $gparlist]
            incr num 1 
            loop i 1 $num  {
               if { [string match $want $gpars($i)] }  { 
                  if { $gotone == 0 } { set gotone $i }
                  $win selection set [expr $i-1]
                  lappend gparams $gpars($i)

               }
            }
                  
            if { $gotone != 0 } { $win see $gotone }
         }

   }

}

#if { [regexp -nocase $want $gpars($i)] }  { 

proc gmakentry {win}  {

    global GWinEnt GWinEntL Font12 GWinPar gSpecpar gspecpar

    label $win.enlable \
	-font $Font12 \
        -bd 2 -relief ridge \
	-text "SELECT PARAMETERS:" \
        -bg IndianRed 


    entry $win.entry \
	-width 15  \
	-relief sunken   \
	-bd 2   \
	-textvariable gspecpar \
        -bg honeydew

    pack $win.entry $win.enlable -side right -padx 1m -pady 2m

    set GWinEnt $win.entry
    set GWinEntL $win.enlable
 
    bind $win.entry <Return>  {
      set gSpecpar $gspecpar
      destroy $GWinEnt 
      destroy $GWinEntL
      selectgpars "nullpar" $GWinPar "selpar"
 
    }
 
}