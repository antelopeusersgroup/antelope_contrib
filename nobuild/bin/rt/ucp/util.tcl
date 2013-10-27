proc alarmsig { dcname param stime } {

   global Pfile
   global sendto
   global crnt_color
   global Font12
   global Font14
   global beepmsg

   set wname .alarm_$dcname

   set wexist [winfo exist $wname]

   if { $wexist == 0 }  {

      set AlarmWin $wname 
      if [string compare $param ACOK]  {
          if { [catch "pfget $Pfile SendAlarmTo" error] } {
              set sendto ""
          } else {
              set sendto [pfget $Pfile SendAlarmTo]
          } 
      
          toplevel $AlarmWin
          wm title $AlarmWin "ALARM"

		# WARNING from DC xxx.xxx.xxx.xxx
		#
		#    GENERATOR FAILED TO START
		#      at DDD:HH:MM:SS (UTC)
		#
		#  Turn off audible alarm?   y/n
 
          set crnt_color red
          set mess "WARNING from DC $dcname!\n\nGENERATOR FAILED TO START\nat $stime\n"

#
#  Send an email to operator
#
	  set tmp [split $sendto ":"]
	  foreach snd $tmp {
	     exec echo $mess | mailx -v -s "DC ALARM" $snd
	  }
#
#
# 
          message $AlarmWin.msg -relief ridge \
              -bd 2 \
              -bg $crnt_color \
              -justify center \
              -width 22c \
              -font $Font14 \
              -text $mess 
             
          set beepwin $AlarmWin.beep
          set beepmsg BEEP_OFF
    
          button $beepwin \
              -font $Font12 \
              -text $beepmsg \
              -bd 3 \
              -activebackground red \
              -bg DarkSeaGreen \
              -command "change_beep $beepwin"
      
          button $AlarmWin.quit \
              -font $Font12 \
              -text "DISMISS" \
              -bd 3 \
              -activebackground red \
              -bg DarkSeaGreen \
              -command "destroy $AlarmWin"
        
          pack $AlarmWin.msg -fill both -expand 1 
          pack $AlarmWin.beep $AlarmWin.quit \
               -side left -fill both -expand 1 

#
          dcrtp beep 1
       }
   }  else {
         
      set win $AlarmWin.msg

      if [string compare $param ACOK]  {
         changecol $win
      }  else {
         destroy $AlarmWin
      }   
   }

}
proc clear_cmd { cmd up }  {

    if { $up == 1 } {
	$cmd clear $cmd 
    }

}

proc battalarm { dcname param bval stime } {

   global Pfile
   global sendto
   global crnt_color
   global Font12
   global Font14
   global beepmsg

   set wname .batalarm_$dcname

   set wexist [winfo exist $wname]

   if { $wexist == 0 }  {

      set BAlarmWin $wname 

      if [string compare $param BATTOK]  {
          if { [catch "pfget $Pfile SendAlarmTo" error] } {
              set sendto ""
          } else {
              set sendto [pfget $Pfile SendAlarmTo]
          } 
      
          toplevel $BAlarmWin
          wm title $BAlarmWin "ALARM"

		# WARNING from DC xxx.xxx.xxx.xxx
		#
		#    LOW BATTERY VOLTAGE 
		#    at DDD:HH:MM:SS (UTC)
		#
		#  Turn off audible alarm?   y/n
 
          set crnt_color red
          set mess "WARNING from DC $dcname!\n\nLOW BATTERY VOLATAGE: $bval\nat $stime\n"

#
#  Send an email to operator
#
	  set tmp [split $sendto ":"]
	  foreach snd $tmp {
	     exec echo $mess | mailx -v -s "LOW BATTERY" $snd
	  }
#
#
# 
          message $BAlarmWin.msg -relief ridge \
              -bd 2 \
              -bg $crnt_color \
              -justify center \
              -width 22c \
              -font $Font14 \
              -text $mess 
             
          set beepwin $BAlarmWin.beep
          set beepmsg BEEP_OFF
    
          button $beepwin \
              -font $Font12 \
              -text $beepmsg \
              -bd 3 \
              -activebackground red \
              -bg DarkSeaGreen \
              -command "change_beep $beepwin"
      
          button $BAlarmWin.quit \
              -font $Font12 \
              -text "DISMISS" \
              -bd 3 \
              -activebackground red \
              -bg DarkSeaGreen \
              -command "destroy $BAlarmWin"
        
          pack $BAlarmWin.msg -fill both -expand 1 
          pack $BAlarmWin.beep $BAlarmWin.quit \
               -side left -fill both -expand 1 

#
          dcrtp beep 1
       }
   }  else {
         
      set win $BAlarmWin.msg

      if [string compare $param BATTOK]  {
         changecol $win
      }  else {
         destroy $BAlarmWin
      }   
   }

}

proc change_beep { win } {

     global beepmsg

     if [string compare $beepmsg BEEP_OFF]   {
        dcrtp beep 1
        set beepmsg "BEEP_OFF"
     }  else {
        dcrtp beep 0
        set beepmsg "BEEP_ON"
     }

     $win configure -text $beepmsg 
}

proc changecol {win}  {
    
     global crnt_color

     if [string compare $crnt_color red]  {
        
        set crnt_color red
     
     }  else {
        set crnt_color yellow
     }

#puts "  $win config -bg $crnt_color\n"

     $win config -bg $crnt_color

}

#
#
#
proc check_archive { orb match win } {
    
     global archive_msg

     if [string compare $archive_msg STOP_ARCHIVE]   {
        start_archive $orb $match
        set archive_msg "STOP_ARCHIVE"
     }  else {
        stop_archive 
        set archive_msg "START_ARCHIVE"
     }
 
     $win configure -text $archive_msg 
 
}

proc start_archive { orb match } {

    global Font12 global Font14
 
    if { ![file exist PARAM] } {
       exec mkdir PARAM &
    }

    set start 0
    set name "par2db"
 
    set pid [ exec $env(ANTELOPE)/bin/check_pid $name ]                        
    echo $pid
    set onepid [lindex $pid 0]
 
    if { [string compare $onepid none] }   {
       set out [ exec  ps -elo user,pid,stime,args | grep $name | grep -v grep ]

       set ArcWin .arch
       toplevel $ArcWin
       wm title $ArcWin "ARCHIVE"

       set msg "Archiving program $name is running already:\n$out\nDo you want start another $name?"

       message $ArcWin.msg -relief ridge \
          -bd 2 \
          -bg honeydew \
          -justify center \
          -width 27c \
          -font $Font14 \
          -text $msg 
         
 
       button $ArcWin.yes \
          -font $Font12 \
          -text "YES"\
          -bd 3 \
          -activebackground red \
          -bg DarkSeaGreen \
          -command "startarc $orb {$match} {$pid} 1; destroy $ArcWin"
 
       button $ArcWin.no \
          -font $Font12 \
          -text "NO" \
          -bd 3 \
          -activebackground red \
          -bg DarkSeaGreen \
          -command "startarc $orb {$match} {$pid} 0; destroy $ArcWin"
 
       pack $ArcWin.msg -fill both -expand 1 
       pack $ArcWin.yes $ArcWin.no \
           -side left -fill both -expand 1 
 
    }  else  {
       startarc $orb $match {$pid} 1
 
    }

}
proc startarc { orb match pid start }  {

    global cmdhistory crnt_cmd

    set onepid [lindex $pid 0]

    if { $start == 1 }  {

        exec par2db -m $match -i 100 $orb PARAM/par >& par2db_$onepid &
     
       set tmpstr [exec epoch now]
       set timestr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]  
       $cmdhistory insert $crnt_cmd "$timestr\t Start par2db -m $match -i 100 $orb PARAM/par"
       incr crnt_cmd 1
   } 
}

#
proc stop_archive { } {

     global cmdhistory crnt_cmd

     set name "par2db"
 
     set pid [ exec $env(ANTELOPE)/bin/check_pid $name ]                        
     echo $pid
     set onepid [lindex $pid 0]
 
     if { [string compare $onepid none] }   {
        set num  [llength $pid]
        loop i 0 $num {
          set onepid [lindex $pid $i]
          exec  kill -9 $onepid  
     
          set tmpstr [exec epoch now]
          set timestr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]  
          $cmdhistory insert $crnt_cmd "$timestr\t Stop par2db with pid=$onepid"
          incr crnt_cmd 1
        }
     }
  
}

#
proc showval { win val newopt }  {
 
     if { $newopt != "none" }  {
       
        $win config -text $val -bg $newopt
     } else {
 
        $win config -text $val
     }
}
 
proc radiowin { } {

    global Font16

set RWin .rwinin
          
   set wexist [winfo exist $RWin]

   if { $wexist != 0 }  {
      destroy $RWin
   }
    
toplevel $RWin
wm title $RWin "RADIOS"

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


proc msg_msg { msg } {

    global global cmdhis Font16

    set cmdhis 1
    set MsgWin .msgWin
          
   set wexist [winfo exist $MsgWin]

   if { $wexist != 0 }  {
      destroy $MsgWin
   }
    
toplevel $MsgWin

frame $MsgWin.msg -relief ridge -bg honeydew -bd 4 

message $MsgWin.msg.msg \
        -width 10c \
        -justify left \
        -relief ridge \
        -bd 2 \
        -bg maroon \
        -fg black \
        -font $Font16 \
        -text $msg 

button $MsgWin.msg.return \
 -font $Font16 -text "DISMISS" -bd 2 -relief raised \
 -activebackground red \
 -bg IndianRed \
 -command "destroy $MsgWin "
 
pack $MsgWin.msg.msg $MsgWin.msg.return -side top -fill both -expand 1 
pack $MsgWin.msg
}

proc error_msg { msg } {

    global Font16

set ErrWin .errWin
          
   set wexist [winfo exist $ErrWin]

   if { $wexist != 0 }  {
      destroy $ErrWin
   }
    
toplevel $ErrWin
wm title $ErrWin "ERROR"

frame $ErrWin.err -relief ridge -bg honeydew -bd 4 

message $ErrWin.err.msg \
        -width 10c \
        -justify left \
        -relief ridge \
        -bd 2 \
        -bg maroon \
        -fg black \
        -font $Font16 \
        -text $msg 

button $ErrWin.err.return \
 -font $Font16 -text "DISMISS" -bd 2 -relief raised \
 -activebackground red \
 -bg IndianRed \
 -command "destroy $ErrWin "
 
pack $ErrWin.err.msg $ErrWin.err.return -side top -fill both -expand 1 
pack $ErrWin.err
}

