proc daspar { orb }  {

global Pfile
global bparams
global bparlist
global dpars
global bSpecpar
global bspecpar
global Font12
global WinPar 
global WinEnt 
global WinEntL
global bpfdasarr
global borbselect
global bdassel
global bdcsel
global archive_msg
global pparfile


set DasWin .daspar
toplevel $DasWin
wm title $DasWin "DP LIST"
set bSpecpar "null"
set bspecpar ""
set bparams " "
set bparlist ""
set allpar    "allpar"
set noallpar  "noallpar"
set selpar    "selpar"
set bpfdas       ""
set bpfdc        ""
set bpfdasid     ""
set archive_msg "START_ARCHIVE"
set tmplist ""


set daspar { _BUFF _BATT _RESET _PLL _RTXRCV _RTXREQ _RTXSKP _TEMP }

# Read parameter file
 
  if { [catch "pfgetlist @$Pfile#Dc" error] } {
     puts "can't find DC specifications in a $Pfile parameter file\n"
     exit 1
  }  else { 
     set bpfdc [pfgetlist @$Pfile#Dc]
  }
 
  if { [catch "pfget $Pfile SelectDas" error] } {
       set bdassel ""
  } else {
       set bdassel [pfget $Pfile SelectDas] 
  }
 
  if { [catch "pfget $Pfile SelectDc" error] } {
       set bdcsel ""
  } else {
       set bdcsel [pfget $Pfile SelectDc] 
  }
# 
  if { [catch "pfget $Pfile OrbSelect" error] } {
       set borbselect ".*"
  } else {
       set borbselect [pfget $Pfile OrbSelect] 
  } 


  if { [catch "pfget $Pfile ParFile" error] } {
       puts "can't find ParFile specifications in a $Pfile parameter file"
      exit 1
  }  else { 
      set pparfile [pfget $Pfile ParFile] 
  }

  if { [catch "pfgetlist @$pparfile#Site" error] } {
     puts "can't find Das specifications in a $pparfile parameter file"
     exit 1
  }  else { 
     set bpfdasarr  [pfgetlist @$pparfile#Site]
     foreach pfd  $bpfdasarr  {
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

set bpfdas [lsort $tmplist]
set new "null"
set i 1
 
foreach site $bpfdas {
    if { [string compare $new $site] != 0 }  {
       set new $site
       foreach par $daspar  {
	   set newpar $site$par
           lvarcat bparlist $i
           set dpars($i) $newpar
           set pbut($i) [format $DasWin.par.par.but%s $i] 
           incr i
       }
    }
}
 
frame $DasWin.par -relief ridge -bg DarkSeaGreen -borderwidth 4 
listbox $DasWin.par.par -selectmode multiple  \
        -font $Font12 -width 30 -height 18 \
        -relief sunken  \
        -bg honeydew \
        -selectbackground IndianRed \
	-yscrollcommand "$DasWin.par.scroll set" 
#
set WinPar $DasWin.par.par

set num [llength $bparlist]

loop i 0 $num {
   set ind [lindex $bparlist $i]
   $WinPar insert end $dpars($ind) 
}

bind $DasWin.par.par <ButtonRelease-1>  {
   set par [selection get];
   selectpars $par $WinPar "nullpar" 
}

scrollbar $DasWin.par.scroll \
    -bg DarkSeaGreen \
    -command "$DasWin.par.par yview"

#
frame $DasWin.mbar -relief ridge -bd 2 -bg DarkSeaGreen
    
menubutton $DasWin.mbar.func \
         -font $Font12 \
         -bd 2\
         -text DIAGPAR \
         -underline 0 \
         -menu $DasWin.mbar.func.menu \
         -bg IndianRed \
         -activebackground red 

menu $DasWin.mbar.func.menu  -bg honeydew -activebackground honeydew  
$DasWin.mbar.func.menu add command -label "SelectAll" \
         -command "selectpars nullpar $DasWin.par.par $allpar " \
	 -activebackground red
$DasWin.mbar.func.menu add command -label "DeleteAll" \
         -command "selectpars nullpar $DasWin.par.par $noallpar " \
         -activebackground red
$DasWin.mbar.func.menu add command -label "Select" \
         -command "makentry $DasWin.mbar;selectpars nullpar $DasWin.par.par $selpar" \
         -activebackground red

button $DasWin.mbar.quit \
 -font $Font12 -text "DISMISS" -bd 2 \
 -activebackground red -bg IndianRed -command "destroy $DasWin "

pack $DasWin.mbar.func -side left -padx 1m -pady 1m 
pack $DasWin.mbar.quit -side right -padx 1m -pady 1m 

pack $DasWin.par.scroll -side right -fill y
pack $DasWin.par.par -fill both -expand 1

frame $DasWin.cmd -relief ridge -bd 2 -bg DarkSeaGreen 
button $DasWin.cmd.showpar \
         -font $Font12 \
         -text "SHOWPAR" \
         -bd 3 \
         -relief raised \
         -bg IndianRed \
         -activebackground red \
         -command "showpar $orb"

button $DasWin.cmd.save \
         -font $Font12 \
         -text $archive_msg\
         -bd 3 \
         -bg IndianRed \
         -activebackground red \
         -command "check_archive $orb {$borbselect} {$DasWin.cmd.save} "

#pack $DasWin.cmd.showpar $DasWin.cmd.save -side left -padx 1m -pady 1m -expand 1 -fill x 
pack $DasWin.cmd.showpar -padx 1m -pady 1m -expand 1 -fill x 

pack $DasWin.mbar -fill x -side top 
pack $DasWin.par $DasWin.cmd -side top -fill both -expand 1
pack $DasWin.cmd -side bottom -fill x
}
proc selectpars { arg win mode } {

   global bparams bparlist dpars bSpecpar

   switch $mode {

      nullpar
         {
            set bparams $arg
         }

     noallpar  
         {
           set num [llength $bparlist]
           incr num -1
           $win selection clear 0 $num
           set bSpecpar "null"
           set bparams " "
         }

      allpar
         {
           set num [llength $bparlist]
           set last $num
           incr num -1
           incr last 1
           $win selection set 0 $num
           loop i 1 $last  {
              lappend bparams $dpars($i)
           }
         }

      selpar
         {
            set want "" 
            set gotone 0
            set want $bSpecpar
            set bSpecpar "null"
             
            set num [llength $bparlist]
            incr num 1 
            loop i 1 $num  {
               if { [string match $want $dpars($i)] }  { 
                  if { $gotone == 0 } { set gotone $i }
                  $win selection set [expr $i-1]
                  lappend bparams $dpars($i)

               }
            }
                  
            if { $gotone != 0 } { $win see $gotone }
         }

   }

}

#if { [regexp -nocase $want $dpars($i)] }  { 

proc makentry {win}  {

    global WinEnt WinEntL Font12 WinPar bSpecpar bspecpar

    label $win.enlable \
	-font $Font12 \
        -bd 2 -relief ridge \
	-text "SELECT PARAMETERS:" \
        -bg IndianRed 


    entry $win.entry \
	-width 15  \
	-relief sunken   \
	-bd 2   \
	-textvariable bspecpar \
        -bg honeydew

    pack $win.entry $win.enlable -side right -padx 1m -pady 2m

    set WinEnt $win.entry
    set WinEntL $win.enlable
 
    bind $win.entry <Return>  {
      set bSpecpar $bspecpar
      destroy $WinEnt 
      destroy $WinEntL
      selectpars "nullpar" $WinPar "selpar"
 
    }
 
}