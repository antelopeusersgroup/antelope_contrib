proc dccmd { } {

global Pfile
global dasnum 
global cmddas 
global dccmd
global CmdWin
global Font14
global Font12
global cmd_list
global clcdone
global ready
global cmdhlp
global cmdhis
global parfile
global dcname
global chval
global gainval
global dcwin
global empty
global dcwait

set cmdhis 0
set empty "null"
#set dcname "" 
set CmdWin .dcCmd
toplevel $CmdWin
wm title $CmdWin "DC/DAS COMMANDS"
#
set clcdone 0
set ready 0

# Read parameter file
 
  if { [catch "pfgetlist @$Pfile#DcCmd " error] } {
     puts "can't get a listing DC/DAS commands.\n"
     exit 1
  }  else { 
     set allcmds [pfgetlist @$Pfile#DcCmd]
  }

  if { [catch "pfget $Pfile ParFile" error] } {
     puts "can't get a parameter file specification. Will take pkt.pf .\n"
     set parfile "pkt.pf"
  }  else { 
     set parfile [pfget $Pfile ParFile]
  }
  if { [catch "pfget $Pfile MaxWaitTime" error] } {
     puts "can't get MaxWaitTime specification. Will set it to 20 seconds.\n"
     set dcwait 20 
  }  else { 
     set dcwait [pfget $Pfile MaxWaitTime]
  }
 
  set i 1
  foreach tmp $allcmds {
      set cmd [split $tmp :]
      set cmdtmp [lindex $cmd 0]
      lappend cmd_list $cmdtmp
      set cmds($i) cmdtmp
      set cmdline($cmdtmp) [format "%s - %s"  [lindex $cmd 0] [lindex $cmd 1]]
      set cmdhlp($cmdtmp) [format "%s" [lindex $cmd 1]]
      set dasnum($cmdtmp) [lindex $cmd 2]
      set cmddas($cmdtmp) ""              
      set dccmd($cmdtmp) 0        
      set cmdbut($cmdtmp) [format $CmdWin.dccmd.but%s $cmdtmp]
      incr i
  }
 
# set widgets 
 
frame $CmdWin.dccmd -relief ridge -bg DarkSeaGreen -borderwidth 4 
 
foreach tmp $cmd_list {
    set crnt_win $cmdbut($tmp)
    if {  [winfo exist $crnt_win] == 1 } { destroy $crnt_win }
    checkbutton $cmdbut($tmp) \
    -text $cmdline($tmp) \
    -font $Font14 \
    -variable dccmd($tmp) \
    -command "checkbut $tmp " \
    -bg  honeydew \
    -activebackground IndianRed \
    -anchor w
}
foreach tmp $cmd_list {
    pack $cmdbut($tmp) -side top -fill both -expand 1 
}
 
frame $CmdWin.dcadd -relief ridge -bd 4 -bg DarkSeaGreen 
label $CmdWin.dcadd.name \
      -text "DC_IP_ADDRESS:" \
      -bg DarkSeaGreen \
      -font $Font14 \
      -bd 4 -relief ridge

entry $CmdWin.dcadd.value \
      -width 20 \
      -font $Font14 \
      -relief sunken -bd 2 \
      -textvariable dcname \
      -bg  honeydew

pack $CmdWin.dcadd.name $CmdWin.dcadd.value -side left -padx 1m -pady 2m \
      -fill x -expand 1 
  
frame $CmdWin.cmd -relief ridge -bd 4 -bg DarkSeaGreen 
button $CmdWin.cmd.quit \
 -font $Font12 -text "DISMISS" -bd 3 -relief raised \
 -activebackground red -bg IndianRed -command "destroy $CmdWin "
 
button $CmdWin.cmd.send \
 -font $Font12 -text "SEND_COMMAND" -bd 3 -relief raised \
 -activebackground red -bg IndianRed -command "sendcmd "
 
button $CmdWin.cmd.clear \
 -font $Font12 -text "CLEAR" -bd 3 -relief raised \
 -activebackground red -bg IndianRed -command "clear_settings $empty "
 
pack $CmdWin.cmd.send  $CmdWin.cmd.clear $CmdWin.cmd.quit \
      -side left -padx 2m -pady 2m -fill x -expand 1
 
pack $CmdWin.dccmd -side top -fill both -expand 1
pack $CmdWin.dcadd $CmdWin.cmd -fill x 
}
proc clear_settings { butnam } {
    global cmd_list dcname cmddas dccmd dasnum

    foreach tmp $cmd_list {
        if { [string compare $butnam $tmp] != 0 } {
             set dccmd($tmp) 0
             set cmddas($tmp) ""
        }
    }
} 
proc sendcmd { } {

    global cmd_list dcname cmddas dccmd dasnum
    global cmdhistory crnt_cmd cmdhlp empty cmdhis
    global parfile chval gainval dcwait
 
    foreach tmp $cmd_list {
                 
    
       if { $dccmd($tmp) == 1 } {      
           set tmpstr [exec epoch now]
           set timstr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]   

           if { $cmddas($tmp) == "" }  {
              if { $dasnum($tmp) == 0 } { 
#                puts "dcc $dcname $tmp dc 0 0 0 $dcwait\n" 
                 dcc $dcname $tmp "dc" 0 0 0 $dcwait 
                 if { $cmdhis == 1 }  {
		    $cmdhistory insert $crnt_cmd "$timstr Send $tmp ( $cmdhlp($tmp) ) to $dcname $cmddas($tmp)"
                    incr crnt_cmd 1
		    set cmdhis 0
		 }
              } 
              if { $dasnum($tmp) > 0 } { 
                 set dccmd($tmp) 0 
                 set msg "$tmp was seclectet without arguments!\n\tWill skip $tmp\n" 
		 error_msg $msg
              }
           }  else  { 
	      if { $tmp == "PG" } {
#		 puts "dcc $dcname $tmp $cmddas($tmp) $parfile $chval $gainval $dcwait\n"
		 dcc $dcname $tmp $cmddas($tmp) $parfile $chval $gainval $dcwait
                 if { $cmdhis == 1 }  {
		   $cmdhistory insert $crnt_cmd "$timstr Send $tmp ( $cmdhlp($tmp) ) to $dcname $cmddas($tmp) $chval $gainval"
                   incr crnt_cmd 1
		   set cmdhis 0
	         }
	      }  else {
#                 puts "dcc $dcname $tmp $cmddas($tmp) $parfile 0 0 $dcwait \n"
                 dcc $dcname $tmp $cmddas($tmp) $parfile 0 0 $dcwait
                 if { $cmdhis == 1 }  {
		   $cmdhistory insert $crnt_cmd "$timstr Send $tmp ( $cmdhlp($tmp) ) to $dcname $cmddas($tmp)"
                   incr crnt_cmd 1
		   set cmdhis 0
	         }
	      }
           }
 
       }
   }

}
proc test_name { cmd }  {
     
    global cmddas empty 

              if { [string compare $cmddas($cmd) "MAIN"] == 0 }  {
                 set cmddas($cmd) "01"
              } elseif { [string compare $cmddas($cmd) "main"] == 0 }  {
                 set cmddas($cmd) "01"
              } elseif { [string compare $cmddas($cmd) "AUX"] == 0 }  {
                 set cmddas($cmd) "02"
              } elseif { [string compare $cmddas($cmd) "aux"] == 0 }  {
                 set cmddas($cmd) "02"
              }  else {
                 puts "An $cmddas($cmd) clock was specified.You must specify only MAIN or AUX\n"
                 clear_settings $empty
              }
}
proc checkbut { butnam } {
   global empty dcname dccmd dasnum cmddas ready clcdone 



   if { $dcname == "" } {
      clear_settings $empty
      set msg "You MUST specify a DC IP address before proceeding further!"
      error_msg $msg
      tkwait variable ready
   } 
 

   if { $dccmd($butnam) == 1 } {
#puts "command $butnam was selected for $dcname \n"
         if { $dasnum($butnam) == 1 }  {
            get_dasnames $butnam 
#puts "The following DASes were slected for $butnam command: $cmddas($butnam)\n"
         } 
         if { $dasnum($butnam) == 2 }  {
            get_newip $butnam 
#puts "The following IP was selected for $butnam command: $cmddas($butnam)\n"
         } 
         if { $dasnum($butnam) == 3 }  {
            get_newradio $butnam 
#puts "The following IP was selected for $butnam command: $cmddas($butnam)\n"
         } 
         if { $dasnum($butnam) == 4 }  {
            get_newclock $butnam
            tkwait variable clcdone
            test_name $butnam
            
         } 
         if { $dasnum($butnam) == 5 }  {
            set_gain $butnam
            
         } 
    } else {
         if  { $dasnum($butnam) == 1 }  {
             set cmddas($butnam) "" 
         }
   }
}

proc get_newip { butnam }  {
   
    global cmddas CmdWin Font14
  
frame $CmdWin.dasbox -relief ridge -bg honeydew -bd 4 
message $CmdWin.dasbox.msg \
        -width 10c \
        -justify left \
        -relief ridge \
        -bd 2 \
        -bg honeydew \
        -fg maroon \
        -font $Font14 \
        -text "Command $butnam will permanently change a DC IP address.\n \
               New IP address should be specified in form xxx.xxx.xxx.xxx "

frame $CmdWin.dasbox.das -relief ridge -bg honeydew -bd 4 
label $CmdWin.dasbox.das.name \
      -text "Specify new IP address for DC:" \
      -font $Font14 \
      -bg DarkSeaGreen 

entry $CmdWin.dasbox.das.value \
      -width 20 \
      -relief sunken \
      -textvariable cmddas($butnam) \
      -bg  honeydew

pack $CmdWin.dasbox.das.name $CmdWin.dasbox.das.value \
     -side left -fill both -expand 1 

button $CmdWin.dasbox.return \
 -font $Font14 -text "RETURN" -bd 2 -relief raised \
 -activebackground red \
 -bg IndianRed \
 -command "destroy $CmdWin.dasbox"
 
pack $CmdWin.dasbox.msg $CmdWin.dasbox.das $CmdWin.dasbox.return \
     -side top -fill both -expand 1 
pack $CmdWin.dasbox -fill both -expand 1


}

proc get_newradio { butnam }  {
   
    global cmddas CmdWin Font14
  
frame $CmdWin.dasbox -relief ridge -bg honeydew -bd 4 
message $CmdWin.dasbox.msg \
        -width 10c \
        -justify left \
        -relief ridge \
        -bd 2 \
        -bg honeydew \
        -fg maroon \
        -font $Font14 \
        -text "Command $butnam will turn ON/OFF Master Radio.\n \
               You must specify Master Radio number"

frame $CmdWin.dasbox.das -relief ridge -bg honeydew -bd 4 
label $CmdWin.dasbox.das.name \
      -text "Specify 1, 2, or 3:" \
      -font $Font14 \
      -bg DarkSeaGreen 

entry $CmdWin.dasbox.das.value \
      -width 20 \
      -relief sunken \
      -textvariable cmddas($butnam) \
      -bg  honeydew

pack $CmdWin.dasbox.das.name $CmdWin.dasbox.das.value \
     -side left -fill both -expand 1 

button $CmdWin.dasbox.return \
 -font $Font14 -text "RETURN" -bd 2 -relief raised \
 -activebackground red \
 -bg IndianRed \
 -command "destroy $CmdWin.dasbox"
 
pack $CmdWin.dasbox.msg $CmdWin.dasbox.das $CmdWin.dasbox.return \
     -side top -fill both -expand 1 
pack $CmdWin.dasbox -fill both -expand 1

}
proc get_newclock { butnam }  {
   
    global cmddas CmdWin Font14 clcdone 
  
frame $CmdWin.dasbox -relief ridge -bg honeydew -bd 4 
message $CmdWin.dasbox.msg \
        -width 10c \
        -justify left \
        -relief ridge \
        -bd 2 \
        -bg honeydew \
        -fg maroon \
        -font $Font14 \
        -text "Command $butnam will turn ON/OFF clock.\n "

frame $CmdWin.dasbox.das -relief ridge -bg honeydew -bd 4 
label $CmdWin.dasbox.das.name \
      -text "Specify MAIN or AUX:" \
      -font $Font14 \
      -bg DarkSeaGreen 

entry $CmdWin.dasbox.das.value \
      -width 20 \
      -relief sunken \
      -textvariable cmddas($butnam) \
      -bg  honeydew

pack $CmdWin.dasbox.das.name $CmdWin.dasbox.das.value \
     -side left -fill both -expand 1 

button $CmdWin.dasbox.return \
 -font $Font14 -text "RETURN" -bd 2 -relief raised \
 -activebackground red \
 -bg IndianRed \
 -command "incr clcdone 1; destroy $CmdWin.dasbox"
 
pack $CmdWin.dasbox.msg $CmdWin.dasbox.das $CmdWin.dasbox.return \
     -side top -fill both -expand 1 
pack $CmdWin.dasbox -fill both -expand 1

}
proc set_gain { butnam }  {
   
    global cmddas CmdWin Font14
    global chval gainval

frame $CmdWin.gainbox -relief ridge -bg honeydew -bd 4 
message $CmdWin.gainbox.msg \
        -width 10c \
        -justify left \
        -relief ridge \
        -bd 2 \
        -bg honeydew \
        -fg maroon \
        -font $Font14 \
        -text "Command $butnam requires the following parameters:\n \
	       channel id, gain value, DAS ID or name of a site \n \
	       which command will be sent to."

frame $CmdWin.gainbox.das -relief ridge -bg honeydew -bd 4 
label $CmdWin.gainbox.das.name \
      -text "Enter stationname/das id:" \
      -font $Font14 \
      -bg DarkSeaGreen 

entry $CmdWin.gainbox.das.value \
      -width 20 \
      -relief sunken \
      -textvariable cmddas($butnam) \
      -bg  honeydew

frame $CmdWin.gainbox.chan -relief ridge -bg honeydew -bd 4 
label $CmdWin.gainbox.chan.name \
      -text "Enter channel id:" \
      -font $Font14 \
      -bg DarkSeaGreen 

entry $CmdWin.gainbox.chan.value \
      -width 20 \
      -relief sunken \
      -textvariable chval \
      -bg  honeydew

frame $CmdWin.gainbox.gain -relief ridge -bg honeydew -bd 4 
label $CmdWin.gainbox.gain.name \
      -text "Enter gain value:" \
      -font $Font14 \
      -bg DarkSeaGreen 

entry $CmdWin.gainbox.gain.value \
      -width 20 \
      -relief sunken \
      -textvariable gainval \
      -bg  honeydew

set locwin $CmdWin.gainbox
set but $butnam
pack $CmdWin.gainbox.das.name $CmdWin.gainbox.das.value \
     -side left -fill both -expand 1 

pack $CmdWin.gainbox.chan.name $CmdWin.gainbox.chan.value \
     -side left -fill both -expand 1 

pack $CmdWin.gainbox.gain.name $CmdWin.gainbox.gain.value \
     -side left -fill both -expand 1 

button $CmdWin.gainbox.return \
 -font $Font14 -text "RETURN" -bd 2 -relief raised \
 -activebackground red \
 -bg IndianRed \
 -command "verify_value $locwin"
 
pack $CmdWin.gainbox.msg \
     $CmdWin.gainbox.das \
     $CmdWin.gainbox.chan \
     $CmdWin.gainbox.gain \
     $CmdWin.gainbox.return \
     -side top -fill both -expand 1 
pack $CmdWin.gainbox -fill both -expand 1


}
proc get_dasnames { butnam }  {
   
    global cmddas CmdWin Font14
  
frame $CmdWin.dasbox -relief ridge -bg honeydew -bd 4 
message $CmdWin.dasbox.msg \
        -width 10c \
        -justify left \
        -relief ridge \
        -bd 2 \
        -bg honeydew \
        -fg maroon \
        -font $Font14 \
        -text "Command $butnam requires ID number or name\n \
               of a site which command will be sent to.\n \
               Multiple sites can be specified separated by comma."

frame $CmdWin.dasbox.das -relief ridge -bg honeydew -bd 4 
label $CmdWin.dasbox.das.name \
      -text "Enter station/radio name(s)/id:" \
      -font $Font14 \
      -bg DarkSeaGreen 

entry $CmdWin.dasbox.das.value \
      -width 20 \
      -relief sunken \
      -textvariable cmddas($butnam) \
      -bg  honeydew

set locwin $CmdWin.dasbox
set but $butnam
pack $CmdWin.dasbox.das.name $CmdWin.dasbox.das.value \
     -side left -fill both -expand 1 

button $CmdWin.dasbox.return \
 -font $Font14 -text "RETURN" -bd 2 -relief raised \
 -activebackground red \
 -bg IndianRed \
 -command "verify_value $locwin"
 
pack $CmdWin.dasbox.msg $CmdWin.dasbox.das $CmdWin.dasbox.return \
     -side top -fill both -expand 1 
pack $CmdWin.dasbox -fill both -expand 1


}
proc get_dcname {  }  {
   
    global dcname CmdWin Font14 ready dcwin
  
frame $CmdWin.intbox -relief ridge -bg honeydew -bd 4 
set dcwin $CmdWin.intbox

message $CmdWin.intbox.msg \
        -width 10c \
        -justify left \
        -relief ridge -bd 4 \
        -font $Font14 \
        -fg maroon \
        -bg  honeydew \
        -text "You MUST specify a DC IP address before proceeding further!"

frame $CmdWin.intbox.dcadd \
        -bg DarkSeaGreen \
        -relief ridge \
        -bd 4 

label $CmdWin.intbox.dcadd.name \
        -text "DC_IP_ADDRESS:" \
        -bg DarkSeaGreen \
        -relief ridge -bd 4 \
        -font $Font14

entry $CmdWin.intbox.dcadd.value \
        -width 20 \
        -relief sunken \
        -bd 2 \
        -textvariable dcname \
        -bg  honeydew \
        -font $Font14
        
pack $CmdWin.intbox.dcadd.name $CmdWin.intbox.dcadd.value \
     -side left -padx 1m -pady 2m -fill x -expand 1

button $CmdWin.intbox.return \
 -font $Font14 -text "RETURN" -bd 2 -relief raised \
 -activebackground red -bg IndianRed -command "verify_dcip"
 
pack $CmdWin.intbox.msg $CmdWin.intbox.dcadd $CmdWin.intbox.return \
      -side top -fill both -expand 1 
pack $CmdWin.intbox -fill both -expand 1
#
focus $CmdWin.intbox

}
proc verify_value { win } {
    global cmddas
    
    destroy $win
}

proc verify_dcip { } {
   
    global dcname ready dcwin

   if { $dcname == "" } {
      set ready 0
   } else {
      if {  [winfo exist $dcwin] == 1 } { destroy $dcwin }
      incr ready 1
   }

}
