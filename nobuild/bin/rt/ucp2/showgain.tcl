proc showgain {orb} {

global gorbselect
global gainsel
global gdcsel
global gparams
global Font8
global Font12
global Font14
global gsrcid
global gtmwgd 
global gsitepar 
global gparval 
global gpsite
global gnpar_in_site
global gstarted
global gainrtp 
global gainwin
global gparfile

set i 1
set sid 1
set chid 1
set gstarted 0
set null "\0"

if { $gainwin > 10 } {
   puts "too much window reopens!\n"
   destroy .
}
 
set GainWin ".gain_$gainwin"
incr gainwin 1
toplevel $GainWin
wm title $GainWin "CHANNEL GAIN"

set nsite [array size gpsite]
if { $nsite > 0 }  {
       foreach i [array names gpsite]  {
	 
#puts "$nsite $i $gpsite($i)\n"
	 
          unset gpsite($i)
      }
}
 
set i 1
 
set sparams [lsort $gparams]

foreach par $sparams {

    set sta($sid) $par
    set gpsite($sid) [format "%s" $par]
    set fsta($gpsite($sid)) [format "$GainWin.par.s%s" $par]
    set stalbl($gpsite($sid)) [format "$GainWin.par.s%s.name" $par]
    set parfr($gpsite($sid)) [format "$GainWin.par.s%s.par" $par]
    set valfr($gpsite($sid)) [format "$GainWin.par.s%s.val" $par]
    for { set chid 1 } { $chid <= 9 } { incr chid }  {  
       set chan($i) [format "chan%s" $chid]
       set elem [format "%s_%s" $par $chan($i)]
       set gsrcid($i) $elem             
       set gsitepar($sid,$chid) $elem             
       set parname($elem) $chan($i)
       set parlbl($elem) [format "$GainWin.par.s%s.par.p%s" $par $chan($i)]
       set gparval($elem) [format "$GainWin.par.s%s.val.val_%s" $par $chan($i)]
       incr i
#puts "$sid:$chid $par $elem $fsta($gpsite($sid)) $parlbl($elem) $gparval($elem)) \n "
   }
   set gnpar_in_site($gpsite($sid)) 10    
   incr sid
}

#
# set window 


frame $GainWin.par -relief ridge -bd 6 -bg DarkSeaGreen

set nsite [array size gpsite]
incr nsite 

loop i 1 $nsite  {

   frame $fsta($gpsite($i)) -bd 2 -relief ridge -bg DarkSeaGreen
   frame $parfr($gpsite($i)) -bd 2 -relief ridge -bg DarkSeaGreen
   frame $valfr($gpsite($i)) -relief ridge -bg DarkSeaGreen

   label $stalbl($gpsite($i)) \
         -font $Font12 \
         -text $gpsite($i) \
         -relief ridge \
         -width 8 \
         -bd 2 \
         -bg IndianRed 

   set nch $gnpar_in_site($gpsite($i))
   loop j 1 $nch  {
      set par $gsitepar($i,$j) 
      label $parlbl($par) \
            -font $Font8 \
            -text $parname($par) \
            -relief ridge \
            -width 8 \
            -bd 2 \
            -bg peru

      pack $parlbl($par) -side left -fill both -expand 1 
   }
   loop j 1 $nch  {
      set par $gsitepar($i,$j) 
      label $gparval($par) \
            -font $Font12 \
            -text " "  \
            -relief sunken \
            -width 8 \
            -bd 1 \
            -bg LemonChiffon

      pack $gparval($par) -side left -fill both -expand 1 
   } 
  
   pack $stalbl($gpsite($i)) -side left -fill y 
   pack $parfr($gpsite($i)) \
        $valfr($gpsite($i)) \
        -side top -fill both -expand 1  
}
loop i 1 $nsite  {
  pack $fsta($gpsite($i)) -fill both -expand 1
}
 
set tmpstr [exec epoch now]
set timestr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]  

frame $GainWin.control -relief ridge -bg DarkSeaGreen -bd 4

label $GainWin.control.stlbl \
      -font $Font12 \
      -text "Start Time:" \
      -relief ridge \
      -bd 2 \
      -bg IndianRed 

label $GainWin.control.stime \
      -font $Font12 \
      -text $timestr \
      -relief sunken \
      -width 24 \
      -bd 2 \
      -bg LemonChiffon 

label $GainWin.control.crntlbl \
      -font $Font12 \
      -text "Current Time:" \
      -bd 2 \
      -bg IndianRed \
      -relief ridge

label $GainWin.control.crnttim \
      -font $Font12 \
      -text "00:00" \
      -relief sunken \
      -bg LemonChiffon  \
      -bd 2 \
      -width 24

button $GainWin.control.start \
      -font $Font12 \
      -text "START" \
      -relief raised \
      -bd 2 \
      -bg IndianRed \
      -activebackground red \
      -command " getgain $orb "

button $GainWin.control.quit \
      -font $Font12 \
      -text "STOP" \
      -relief raised \
      -bd 2 \
      -activebackground red \
      -bg IndianRed \
      -command "stopgrtp; set gstarted 0"

button $GainWin.control.exit \
      -font $Font12 \
      -text "DISMISS" \
      -bd 2 \
      -relief raised \
      -activebackground red \
      -bg IndianRed \
      -command "stopgrtp; clear_cmd gainrtp $gstarted; destroy $GainWin "

pack $GainWin.control.stlbl $GainWin.control.stime \
     $GainWin.control.crntlbl $GainWin.control.crnttim \
     -side left -padx 1m -pady 1m -ipadx 1m -ipady 1m 

pack $GainWin.control.exit $GainWin.control.quit $GainWin.control.start \
     -side right -padx 1m -pady 1m 

#pack $GainWin.par $GainWin.control -side top -fill both -expand 1 
pack $GainWin.par -side top -fill both -expand 1 
pack $GainWin.control -side bottom -fill x 

#

bind $GainWin <Control-c> {destroy $GainWin}
bind $GainWin <Control-q> {destroy $GainWin}
focus $GainWin

set gtmwgd $GainWin.control.crnttim

}

proc getgain { orb }  {

   global gainrtd  gnpar_in_site gpsite gsrcid gtmwgd gsitepar 
   global gparval gstarted
   global gainsel gparfile  
   global cmdhistory crnt_cmd 

   dprt gainrtp $orb 
# do borbselect
# puts "gainrtp orbsel $gainsel \n"

   gainrtp orbsel $gainsel
 
# specify patterns for "regex" 
 
# gainrtp verbose 
   gainrtp parcallback showval
   gainrtp timwidget $gtmwgd
#
   set npar [array size gsrcid]
   incr npar 1                
   loop i 1 $npar {
       gainrtp add $gsrcid($i)        
   }

   set nsite [array size gpsite]
   incr nsite 

   loop i 1 $nsite  {
   
      set nch $gnpar_in_site($gpsite($i))
      loop j 1 $nch  {
          set par $gsitepar($i,$j) 
          gainrtp widget $par null $gparval($par) null 
      } 
   }

#  Start
   
   set tmpstr [exec epoch now]
   set timestr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]  
   $cmdhistory insert $crnt_cmd "$timestr\t Start BBA gain display"
   incr crnt_cmd 1
   
   set gstarted 1
#   gainrtp verbose
   gainrtp getdaspar $gparfile 1

}

proc stopgrtp { }  {

    global gainrtp gstarted
    global cmdhistory crnt_cmd
  


    if { $gstarted == 1 } {

       gainrtp stop
       set tmpstr [exec epoch now]
       set timestr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]  
       $cmdhistory insert $crnt_cmd "$timestr\t Stop BBA gain display"
       incr crnt_cmd 1

    }

}