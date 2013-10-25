proc radcmd { } {

global Pfile
global radval 
global cmdrad 
global rdcmd
global RadWin
global Font14
global Font12
global rcmd_list
global ready
global rcmdhlp
global rparfile
global dcname
global empty
global cmdhis 
global radtype
global maxwait 

set cmdhis 0
#set dcname "" 
set RadWin .radCmd
toplevel $RadWin
wm title $RadWin "MASTER/SLAVE RADIO COMMANDS"
#
set ready 0
set empty "null"

# Read parameter file
 
  if { [catch "pfgetlist @$Pfile#RadCmd " error] } {
     puts "can't get a listing Radio commands.\n"
     exit 1
  }  else { 
     set allcmds [pfgetlist @$Pfile#RadCmd]
  }

  if { [catch "pfget $Pfile ParFile" error] } {
     puts "can't get a parameter file specification. Will take pkt.pf .\n"
     set rparfile "pkt.pf"
  }  else { 
     set maxwait [pfget $Pfile MaxWaitTime]
  }
 
  if { [catch "pfget $Pfile MaxWaitTime" error] } {
     puts "can't get a MaxWaitTime specification. Will set it to 20 seconds.\n"
     set maxwait 20
  }  else { 
     set rparfile [pfget $Pfile ParFile]
  }
 
  set i 1
  foreach tmp $allcmds {
      set cmd [split $tmp :]
      set cmdtmp [lindex $cmd 0]
      lappend rcmd_list $cmdtmp
      set cmds($i) cmdtmp
      set cmdline($cmdtmp) [format "%s - %s"  [lindex $cmd 0] [lindex $cmd 1]]
      set rcmdhlp($cmdtmp) [format "%s" [lindex $cmd 1]]
      set dasnum($cmdtmp) [lindex $cmd 2]
      set cmdrad ""              
      set radval ""              
      set rdcmd($cmdtmp) 0        
      set cmdbut($cmdtmp) [format $RadWin.rdcmd.but%s $cmdtmp]
      incr i
  }
 
# set widgets 
 
frame $RadWin.rdcmd -relief ridge -bg DarkSeaGreen -borderwidth 4 
 
foreach tmp $rcmd_list {
    set crnt_win $cmdbut($tmp)
    if {  [winfo exist $crnt_win] == 1 } { destroy $crnt_win }
    checkbutton $cmdbut($tmp) \
    -text $cmdline($tmp) \
    -font $Font14 \
    -variable rdcmd($tmp) \
    -command "rcheckbut $tmp " \
    -bg  honeydew \
    -activebackground IndianRed \
    -anchor w
}
foreach tmp $rcmd_list {
    pack $cmdbut($tmp) -side top -fill both -expand 1 
}
 
frame $RadWin.dcadd -relief ridge -bd 4 -bg DarkSeaGreen 
label $RadWin.dcadd.name \
      -text "DC_IP_ADDRESS:" \
      -bg DarkSeaGreen \
      -font $Font14 \
      -bd 4 -relief ridge

entry $RadWin.dcadd.value \
      -width 20 \
      -font $Font14 \
      -relief sunken -bd 2 \
      -textvariable dcname \
      -bg  honeydew

pack $RadWin.dcadd.name $RadWin.dcadd.value -side left -padx 1m -pady 2m \
      -fill x -expand 1 
  
frame $RadWin.cmd -relief ridge -bd 4 -bg DarkSeaGreen 
button $RadWin.cmd.quit \
 -font $Font12 -text "DISMISS" -bd 3 -relief raised \
 -activebackground red -bg IndianRed -command "destroy $RadWin "
 
button $RadWin.cmd.send \
 -font $Font12 -text "SEND_COMMAND" -bd 3 -relief raised \
 -activebackground red -bg IndianRed -command "sendrcmd "
 
button $RadWin.cmd.clear \
 -font $Font12 -text "CLEAR" -bd 3 -relief raised \
 -activebackground red -bg IndianRed -command "clear_rsettings $empty "
 
pack $RadWin.cmd.send  $RadWin.cmd.clear $RadWin.cmd.quit \
      -side left -padx 2m -pady 2m -fill x -expand 1
 
pack $RadWin.rdcmd -side top -fill both -expand 1
pack $RadWin.dcadd $RadWin.cmd -fill x 
}

proc sendrcmd { } {

    global rcmd_list dcname cmdrad rdcmd 
    global cmdhis cmdhistory crnt_cmd rcmdhlp 
    global radval rparfile empty maxwait


    foreach tmp $rcmd_list {
    
       if { $rdcmd($tmp) == 1 } {      
           if { $cmdrad == "" }  {
	      set msg "You MUST specify DAS ID/station name/radio name before proceeding further!"
	      error_msg $msg
	      clear_rsettings $empty
	      break
	   }
           if { $radval == "" }  {
	      set msg "You MUST specify parameter value before proceeding further!"
#puts "$msg\n"
	      error_msg $msg
	      clear_rsettings $empty
	      break
	   } 
#puts "radval == $radval\n" 

	   set tmpstr [exec epoch now]
           set timstr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]   

#puts "dcc $dcname $tmp $cmdrad $rparfile $radval \n"
           dcc $dcname $tmp $cmdrad $rparfile $radval 0 $maxwait
           if { $cmdhis == 1 }  {
	      $cmdhistory insert $crnt_cmd "$timstr Send $tmp ( $rcmdhlp($tmp) ) to $dcname $cmdrad $radval"
              incr crnt_cmd 1
	      set cmdhis 0
	   }
	   break;
       }
   }

}
proc rcheckbut { butnam } {
   global cmdrad dcname ready  empty


   clear_rsettings $butnam

   if { $dcname == "" } {
      clear_rsettings $empty
      set msg "You MUST specify a DC IP address before proceeding further!"
      error_msg $msg
      tkwait variable ready
   } 
 
   get_rvalues $butnam 

}

proc test_radtype { butnam }  {

   global radval cmdrad empty

   set master 0;

   if { [string compare $radval ""] == 0 ||
        [string compare $cmdrad ""] == 0 }  {
        clear_rsettings $empty
        set msg "You must specify parameters for $butnam command!"
        error_msg $msg
   } else {
       if { [string compare $cmdrad "MMM1" ] == 0 ||
            [string compare $cmdrad "MMM2" ] == 0 ||
            [string compare $cmdrad "MMM3" ] == 0  } {
            set master 1
       }

       if { [string compare $butnam "PR" ] == 0 } {
          if { $master != 1 }  {
                clear_rsettings $empty
	        set msg "\tYou choose SLAVE radio!\nCommand $butnam can be sent only to MASTER"
	        error_msg $msg
          }
       }  
   
       if { [string compare $butnam "RT" ] == 0 } {
          if { $master == 1 }  {
                clear_rsettings $empty
	        set msg "\tYou choose MASTER radio!\nCommand $butnam can be sent only to SLAVE"
	        error_msg $msg
          }
       }  
   }
   
}

proc get_rvalues { butnam }  {
   
    global radval cmdrad RadWin Font14
 
set locwin $RadWin.dasbox
set but $butnam

if {  [winfo exist $locwin] == 1 } { destroy $locwin }

frame $RadWin.dasbox -relief ridge -bg honeydew -bd 4 
message $RadWin.dasbox.msg \
        -width 10c \
        -justify left \
        -relief ridge \
        -bd 2 \
        -bg honeydew \
        -fg maroon \
        -font $Font14 \
        -text "Command $butnam requires RADIOID/DASID or name\n \
               of a site which command will be sent to.\n \
               It also requires a parameter value to be set."   

frame $RadWin.dasbox.das -relief ridge -bg honeydew -bd 4 
label $RadWin.dasbox.das.name \
      -text "Enter staname/dasid/radioID:" \
      -font $Font14 \
      -bg DarkSeaGreen 

entry $RadWin.dasbox.das.value \
      -width 20 \
      -relief sunken \
      -textvariable cmdrad \
      -bg  honeydew

pack $RadWin.dasbox.das.name $RadWin.dasbox.das.value \
     -side left -fill both -expand 1 

frame $RadWin.dasbox.val -relief ridge -bg honeydew -bd 4 
if { [string compare $butnam "CN"] == 0 } {
  set val "(range 1-E)"
} else  { 
  set val "(range 0-9)"
}

label $RadWin.dasbox.val.name \
      -text "Enter parameter value $val:" \
      -font $Font14 \
      -bg DarkSeaGreen 

entry $RadWin.dasbox.val.value \
      -width 20 \
      -relief sunken \
      -textvariable radval \
      -bg  honeydew

pack $RadWin.dasbox.val.name $RadWin.dasbox.val.value \
     -side left -fill both -expand 1 

button $RadWin.dasbox.return \
 -font $Font14 -text "RETURN" -bd 2 -relief raised \
 -activebackground red \
 -bg IndianRed \
 -command "test_radtype $butnam; verify_value $locwin"
 
pack $RadWin.dasbox.msg $RadWin.dasbox.das $RadWin.dasbox.val $RadWin.dasbox.return \
     -side top -fill both -expand 1 
pack $RadWin.dasbox -fill both -expand 1


}
proc clear_rsettings { butnam } {
        global rcmd_list dcname radval cmdrad rdcmd
	 
    foreach tmp $rcmd_list {
      if { [string compare $butnam $tmp] != 0 } {
            set rdcmd($tmp) 0
      }
    }
    set cmdrad ""
    set radval ""
}

proc radiowin { } {

    global Font16

set RWin .rwinin
          
   set wexist [winfo exist $RWin]

   if { $wexist != 0 }  {
      destroy $RWin
   }
    
toplevel $RWin

frame $RWin.rad -relief ridge -bg honeydew -bd 4 

message $RWin.rad.msg \
        -width 10c \
        -justify left \
        -relief ridge \
        -bd 2 \
        -bg DarkSeaGreen \
        -fg black \
        -font $Font16 \
        -text "SPECIFY  RADIO:"

button $RWin.rad.master \
 -font $Font16 -text "MASTER" -bd 2 -relief raised \
 -activebackground red \
 -bg maroon \
 -command "destroy $RWin; radcmd master "
 
button $RWin.rad.slave \
 -font $Font16 -text "SLAVE" -bd 2 -relief raised \
 -activebackground red \
 -bg maroon \
 -command "destroy $RWin; radcmd slave "
 
pack $RWin.rad.msg -fill both -expand 1 
pack $RWin.rad.master $RWin.rad.slave -side left -fill both -expand 1 
pack $RWin.rad
}

