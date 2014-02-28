proc dcpar { orb } {

global dcsrcid
global dctmwdg 
global dccompar 
global dcparval 
global dcpsite
global dcsitepar
global dcnpar_in_site
global lcmdwidget
global lcmdscroll
global DcWin
global numcmd
global cmdarr
global maxcmd
global Font10
global Font12
global Font14
global dcstarted
global dcrtp
global Pfile

set maxcmd 12
set dcstarted 0
set null "\0"

set DcWin .dcpar
toplevel $DcWin
set pardc { _RECNCT _TCPDEL _TCPSND _BATT } 
 
#
set pfdc [pfgetlist @$Pfile#Dc]
 
set i 1
 

foreach site $pfdc {
    lappend dcname $site
    foreach par $pardc  {
       set newpar $site$par
       incr i
    }
}
 
set dccom { ACFAIL HAZARD RECNCT TCPDEL TCPSND BATT  } 
set dcspec { CLOCK MSTAT ASTAT M1OC M2OC M3OC MOC AOC }

set ncom 7
set nspec 9

set i 1
set sid 1

set dcnum [llength $dcname]
 
set total 1
loop i 0 $dcnum {

   set sitetotal 1   
   set sta($sid) [lindex $dcname $i]

   set dcpsite($sid) [format "%s" $sta($sid)]
   set fsta($dcpsite($sid)) [format "$DcWin.par.s%s" $dcpsite($sid)]
   set fstac($dcpsite($sid)) [format "$DcWin.par.s%s.com" $dcpsite($sid)]
   set fstam($dcpsite($sid)) [format "$DcWin.par.s%s.main" $dcpsite($sid)]

   set stalbl($dcpsite($sid)) [format "$DcWin.par.s%s.com.name" $dcpsite($sid)]
   set cparfr($dcpsite($sid)) [format "$DcWin.par.s%s.com.par" $dcpsite($sid)]
   set cvalfr($dcpsite($sid)) [format "$DcWin.par.s%s.com.val" $dcpsite($sid)]

   set mparfr($dcpsite($sid)) [format "$DcWin.par.s%s.main.par" $dcpsite($sid)]
   set mvalfr($dcpsite($sid)) [format "$DcWin.par.s%s.main.val" $dcpsite($sid)]

   set chid 1
 
#  Set DC common parameters  
 
   foreach chan $dccom {
       set par $sta($sid)_$chan
       set dcsrcid($total) $par
       set dcsitepar($sid,$sitetotal) $par
       set dccompar($sid,$chid) $par
       set parname($par) $chan 
       set cparlbl($par) [format "$DcWin.par.s%s.com.par.p%s" $dcpsite($sid) $chan]
       set cparval($par) [format "$DcWin.par.s%s.com.val.val_%s" $dcpsite($sid) $chan]
       set dcparval($par) [format "$DcWin.par.s%s.com.val.val_%s" $dcpsite($sid) $chan]
       incr chid
       incr total
       incr sitetotal
   }

#  Set DC MAIN&AUX CLOCK parameters  
 
   set chid 1

   foreach chan $dcspec {
       set par $sta($sid)_$chan
       set dcsitepar($sid,$sitetotal) $par
       set spar($sid,$chid) $par 
       set parname($par) $chan 

       set mparlbl($par) [format "$DcWin.par.s%s.main.par.p%s" $dcpsite($sid) $chan]
       set mparval($par) [format "$DcWin.par.s%s.main.val.val_%s" $dcpsite($sid) $chan]
       set dcparval($par) [format "$DcWin.par.s%s.main.val.val_%s" $dcpsite($sid) $chan]
       set dcsrcid($total) $par
       incr total
       incr sitetotal
       incr chid
   }

   set dcnpar_in_site($dcpsite($sid)) $sitetotal
   incr sid
}
 
# set window 
wm title $DcWin "DC DIAGNOSTIC PARAMETERS"

frame $DcWin.par -relief raised -bd 6 -bg DarkSeaGreen

loop i 0 $dcnum  {
   
   set sid $i
   incr sid
   frame $fsta($dcpsite($sid)) -bd 4 -relief sunken -bg DarkSeaGreen
   frame $fstac($dcpsite($sid)) -bd 2 -relief sunken -bg DarkSeaGreen
   frame $fstam($dcpsite($sid)) -bd 2 -relief sunken -bg DarkSeaGreen

   frame $cparfr($dcpsite($sid)) -bd 2 -relief sunken -bg DarkSeaGreen
   frame $cvalfr($dcpsite($sid)) -relief flat -bg DarkSeaGreen

   frame $mparfr($dcpsite($sid)) -relief sunken -bg DarkSeaGreen
   frame $mvalfr($dcpsite($sid)) -relief sunken -bg DarkSeaGreen

   label $stalbl($dcpsite($sid)) \
         -font $Font12 \
         -text $dcpsite($sid) \
         -relief ridge \
         -width 8 \
         -bd 3 \
         -bg IndianRed 

   loop j 1 $ncom  {
      set par $dccompar($sid,$j) 
      label $cparlbl($par) \
            -font $Font10 \
            -text $parname($par) \
            -relief ridge \
            -width 8 \
            -bd 2 \
            -bg DarkSeaGreen 

      pack $cparlbl($par) -side left -fill both -expand 1 
   }
  
   pack $stalbl($dcpsite($sid)) -side left -fill y
   pack $cparfr($dcpsite($sid)) \
        $cvalfr($dcpsite($sid)) \
        -side top -fill both -expand 1  
   
   loop j 1 $ncom  {
      set par $dccompar($sid,$j) 
      label $cparval($par) \
            -font $Font10 \
            -text " "  \
            -relief sunken \
            -width 8 \
            -bd 2 \
            -bg LemonChiffon

      pack $cparval($par) -side left -fill both -expand 1 
   } 
   loop j 1 $nspec  {
      set par $spar($sid,$j) 

# MAIN CLOCK 

      label $mparlbl($par) \
            -font $Font10 \
            -text $parname($par) \
            -relief ridge \
            -width 8 \
            -bd 2 \
            -bg LightSteelBlue 
      
      pack $mparlbl($par) -fill both -expand 1 

      label $mparval($par) \
            -font $Font10 \
            -text " "  \
            -relief sunken \
            -width 8 \
            -bd 2 \
            -bg honeydew
      pack $mparval($par)  -fill both -expand 1
      
   } 
   pack $mparfr($dcpsite($sid)) $mvalfr($dcpsite($sid)) -side left \
        -fill both -expand 1  
  
   pack $fstac($dcpsite($sid)) -fill both -expand 1
   pack $fstam($dcpsite($sid)) -fill both -expand 1  
   
   
}
set nsite $dcnum
incr nsite

loop i 1 $nsite  {
  pack $fsta($dcpsite($i)) -fill both -expand 1
}
 
set tmpstr [exec epoch now]
set timestr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]  

frame $DcWin.control -relief raised -bg DarkSeaGreen -bd 4
frame $DcWin.time -relief raised -bg DarkSeaGreen -bd 4

label $DcWin.time.stlbl \
      -font $Font12 \
      -text "Start Time:" \
      -relief ridge \
      -bd 2 \
      -bg IndianRed 

label $DcWin.time.stime \
      -font $Font12 \
      -text $timestr \
      -relief sunken \
      -width 24 \
      -bd 2 \
      -bg LemonChiffon 

label $DcWin.time.crntlbl \
      -font $Font12 \
      -text "Current Time:" \
      -bd 2 \
      -bg IndianRed \
      -relief ridge

label $DcWin.time.crnttim \
      -font $Font12 \
      -text "00:00" \
      -relief sunken \
      -bg LemonChiffon  \
      -bd 2 \
      -width 24

button $DcWin.control.start \
      -font $Font12 \
      -text "START" \
      -bd 3 \
      -activebackground red \
      -bg IndianRed \
      -command " get_dcpar $orb "

button $DcWin.control.quit \
      -font $Font12 \
      -text "STOP" \
      -bd 3 \
      -activebackground red \
      -bg IndianRed \
      -command "stopdc; set dcstarted 0"

button $DcWin.control.exit \
      -font $Font12 \
      -text "DISMISS" \
      -bd 3 \
      -activebackground red \
      -bg IndianRed \
      -command " stopdc; clear_cmd dcrtp $dcstarted; destroy $DcWin"

pack $DcWin.time.stlbl $DcWin.time.stime \
     $DcWin.time.crntlbl $DcWin.time.crnttim \
     -side left -padx 1m -pady 1m -ipadx 1m -ipady 1m \
     -fill both -expand 1 

pack $DcWin.control.start $DcWin.control.quit $DcWin.control.exit \
     -side left -padx 1m -pady 1m -fill both -expand 1 

#
# Window to display LAST COMMANDs
#
frame $DcWin.lcmd -relief ridge -bg DarkSeaGreen -borderwidth 4 
label $DcWin.lcmd.lbl \
         -font $Font12 -text "DC LAST COMMAND" -bd 3 \
         -relief ridge -bg DarkSeaGreen

set lcmdwidget $DcWin.lcmd.cmds
set lcmdscroll $DcWin.lcmd.scroll
set numcmd 1

listbox $lcmdwidget \
        -font $Font14 \
        -width 35 \
        -height 5 \
        -relief sunken \
        -bg honeydew \
        -yscrollcommand  "$lcmdscroll set"

scrollbar $lcmdscroll \
    -bg DarkSeaGreen \
    -command "$lcmdwidget yview"

loop i 0 $maxcmd {
    $lcmdwidget insert end ""
}
 
#
#
pack $DcWin.lcmd.lbl -fill both -expand 1
pack $DcWin.lcmd.scroll -side right -fill y 
pack $DcWin.lcmd.cmds -fill both -expand 1 
 
pack $DcWin.control $DcWin.par $DcWin.time $DcWin.lcmd \
     -side top -fill both -expand 1 

#

bind $DcWin <Control-c> {destroy $DcWin}
bind $DcWin <Control-q> {destroy $DcWin}
focus $DcWin

set dctmwdg $DcWin.time.crnttim

}
proc stopdc { } {

   global dcrtp dcstarted cmdhistory crnt_cmd

 
    if { $dcstarted == 1 } {
 
       dcrtp stop
       set tmp [exec epoch now]
       set timstr [format "%s %s" [lindex $tmp 2] [lindex $tmp 3]]
       $cmdhistory insert $crnt_cmd "$timstr\t Stop DC status display"
       incr crnt_cmd 1
    }

}

proc get_dcpar { orb }  {

   global dcnpar_in_site dcpsite dcsrcid dctmwdg dcsitepar 
   global dcparval lcmdwidget 
   global dcstarted cmdhistory crnt_cmd      

   set none .* 
   set dcmatch .*BSP

   dcrt dcrtp $orb 
   dcrtp orbsel $dcmatch
   dcrtp select $dcmatch $none
#puts "   dcrtp orbsel $dcmatch\n"
#puts "   dcrtp select $dcmatch $none \n"
   #dcrtp verbose 
#
# Set alarm for DC ACFAIL
#
   dcrtp alarm alarmsig
#
# Set alarm for DC BATT LOW
#
   dcrtp balarm battalarm
   dcrtp parcallback showval
   dcrtp timwidget $dctmwdg
#
   dcrtp lcmd showlcmd $lcmdwidget
   set npar [array size dcsrcid]
   incr npar 1                
   loop i 1 $npar {
       dcrtp add $dcsrcid($i)        
   }
   
   set nsite [array size dcpsite]
   incr nsite 

   loop i 1 $nsite  {
   
      set nch $dcnpar_in_site($dcpsite($i))
      loop j 1 $nch  {
          set par $dcsitepar($i,$j) 
          dcrtp widget $par null $dcparval($par) null 
      } 
   }

#  Start

   set tmp [exec epoch now]
   set timstr [format "%s %s" [lindex $tmp 2] [lindex $tmp 3]]
   $cmdhistory insert $crnt_cmd "$timstr\t Start DC status display"
   incr crnt_cmd 1
   
   set dcstarted 1
   dcrtp getdcpar

}

proc showlcmd { win lcmd } {
    global lcmdwidget maxcmd numcmd cmdarr

    
    if { $numcmd >= $maxcmd }  {
       set numcmd 1
    }
    set cmdarr($numcmd) $lcmd
     
    $lcmdwidget insert [expr $numcmd-1] $cmdarr($numcmd)

    incr numcmd 1
     
}
