proc showpar {orb} {

global borbselect
global bdassel
global bdcsel
global bparams
global Font8
global Font12
global Font14
global srcid
global tmwdg 
global sitepar 
global parval 
global psite
global npar_in_site
global bstarted
global dasrtp 
global daswin

set i 1
set sid 1
set chid 1
set bstarted 0
set null "\0"

if { $daswin > 10 } {
   puts "too much window reopens!\n"
   destroy .
}
 
set DasVWin ".dasval_$daswin"
incr daswin 1
toplevel $DasVWin
wm title $DasVWin "DAS PARAMETERS"

set nsite [array size psite]
if { $nsite > 0 }  {
   foreach i [array names psite]  {
 
      unset psite($i)
   }
}
 
#    set npar [array size srcid]
#    incr npar 1                
#    loop i 1 $npar {
#       dasrtp add $srcid($i)        
#    }
#
set i 1
 
set sparams [lsort $bparams]

foreach par $sparams {

    set srcid($i) $par
    set tmpstr [split $par _]
    set sta($i) [format "%s" [lindex $tmpstr 0]]
    set chan($i) [format "%s" [lindex $tmpstr 1]]
    if { $i == 1 }  {
       set psite($sid) [format "%s" $sta($i)]
       set fsta($psite($sid)) [format "$DasVWin.par.s%s" $psite($sid)]
       set stalbl($psite($sid)) [format "$DasVWin.par.s%s.name" $psite($sid)]
       set parfr($psite($sid)) [format "$DasVWin.par.s%s.par" $psite($sid)]
       set valfr($psite($sid)) [format "$DasVWin.par.s%s.val" $psite($sid)]
    }
    set diff [string compare $psite($sid) $sta($i) ]

    if { $diff != 0 }  {

       incr sid
       set chid 1
       set psite($sid) [format "%s"  $sta($i)]
       set fsta($psite($sid)) [format "$DasVWin.par.s%s" $psite($sid)]
       set stalbl($psite($sid)) [format "$DasVWin.par.s%s.name" $psite($sid)]
       set parfr($psite($sid)) [format "$DasVWin.par.s%s.par" $psite($sid)]
       set valfr($psite($sid)) [format "$DasVWin.par.s%s.val" $psite($sid)]
    }
    set sitepar($sid,$chid) $par 
    set parname($par) $chan($i) 
    set parlbl($par) [format "$DasVWin.par.s%s.par.p%s" $psite($sid) $chan($i)]
    set parval($par) [format "$DasVWin.par.s%s.val.val_%s" $psite($sid) $chan($i)]
  
#puts "$sid:$chid $par $fsta($psite($sid)) $parlbl($par) $parval($par)) \n "


    incr chid
    set npar_in_site($psite($sid)) $chid
    incr i
}

#
# set window 


frame $DasVWin.par -relief ridge -bd 6 -bg DarkSeaGreen

set nsite [array size psite]
incr nsite 

loop i 1 $nsite  {

   frame $fsta($psite($i)) -bd 2 -relief ridge -bg DarkSeaGreen
   frame $parfr($psite($i)) -bd 2 -relief ridge -bg DarkSeaGreen
   frame $valfr($psite($i)) -relief ridge -bg DarkSeaGreen

   label $stalbl($psite($i)) \
         -font $Font12 \
         -text $psite($i) \
         -relief ridge \
         -width 8 \
         -bd 2 \
         -bg IndianRed 

   set nch $npar_in_site($psite($i))
   loop j 1 $nch  {
      set par $sitepar($i,$j) 
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
      set par $sitepar($i,$j) 
      label $parval($par) \
            -font $Font12 \
            -text " "  \
            -relief sunken \
            -width 8 \
            -bd 1 \
            -bg LemonChiffon

      pack $parval($par) -side left -fill both -expand 1 
   } 
  
   pack $stalbl($psite($i)) -side left -fill y 
   pack $parfr($psite($i)) \
        $valfr($psite($i)) \
        -side top -fill both -expand 1  
}
loop i 1 $nsite  {
  pack $fsta($psite($i)) -fill both -expand 1
}
 
set tmpstr [exec epoch now]
set timestr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]  

frame $DasVWin.control -relief ridge -bg DarkSeaGreen -bd 4

label $DasVWin.control.stlbl \
      -font $Font12 \
      -text "Start Time:" \
      -relief ridge \
      -bd 2 \
      -bg IndianRed 

label $DasVWin.control.stime \
      -font $Font12 \
      -text $timestr \
      -relief sunken \
      -width 24 \
      -bd 2 \
      -bg LemonChiffon 

label $DasVWin.control.crntlbl \
      -font $Font12 \
      -text "Current Time:" \
      -bd 2 \
      -bg IndianRed \
      -relief ridge

label $DasVWin.control.crnttim \
      -font $Font12 \
      -text "00:00" \
      -relief sunken \
      -bg LemonChiffon  \
      -bd 2 \
      -width 24

button $DasVWin.control.start \
      -font $Font12 \
      -text "START" \
      -relief raised \
      -bd 2 \
      -bg IndianRed \
      -activebackground red \
      -command " bbagetnew $orb "

button $DasVWin.control.quit \
      -font $Font12 \
      -text "STOP" \
      -relief raised \
      -bd 2 \
      -activebackground red \
      -bg IndianRed \
      -command "stoprtd; set bstarted 0"

button $DasVWin.control.exit \
      -font $Font12 \
      -text "DISMISS" \
      -bd 2 \
      -relief raised \
      -activebackground red \
      -bg IndianRed \
      -command "stoprtd; clear_cmd dasrtp $bstarted; destroy $DasVWin "

pack $DasVWin.control.stlbl $DasVWin.control.stime \
     $DasVWin.control.crntlbl $DasVWin.control.crnttim \
     -side left -padx 1m -pady 1m -ipadx 1m -ipady 1m 

pack $DasVWin.control.exit $DasVWin.control.quit $DasVWin.control.start \
     -side right -padx 1m -pady 1m 

#pack $DasVWin.par $DasVWin.control -side top -fill both -expand 1 
pack $DasVWin.par -side top -fill both -expand 1 
pack $DasVWin.control -side bottom -fill x 

#

bind $DasVWin <Control-c> {destroy $DasVWin}
bind $DasVWin <Control-q> {destroy $DasVWin}
focus $DasVWin

set tmwdg $DasVWin.control.crnttim

}

proc bbagetnew { orb }  {

   global rtd global npar_in_site psite srcid tmwdg sitepar 
   global parval bstarted
   global borbselect bdcsel bdassel   
   global cmdhistory crnt_cmd 

   dprt dasrtp $orb 
# do borbselect
   dasrtp orbsel $borbselect
 
# specify patterns for "regex" 
 
   dasrtp select $bdcsel $bdassel 
 
#  dasrtp verbose 
   dasrtp parcallback showval
   dasrtp timwidget $tmwdg
#
   set npar [array size srcid]
   incr npar 1                
   loop i 1 $npar {
       dasrtp add $srcid($i)        
   }

   set nsite [array size psite]
   incr nsite 

   loop i 1 $nsite  {
   
      set nch $npar_in_site($psite($i))
      loop j 1 $nch  {
          set par $sitepar($i,$j) 
          dasrtp widget $par null $parval($par) null 
      } 
   }

#  Start
   
   set tmpstr [exec epoch now]
   set timestr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]  
   $cmdhistory insert $crnt_cmd "$timestr\t Start BBA DP display"
   incr crnt_cmd 1
   
   set bstarted 1
   dasrtp getdaspar pkt.pf 0
       

}

proc stoprtd { }  {

    global dasrtp bstarted
    global cmdhistory crnt_cmd
  


    if { $bstarted == 1 } {

       dasrtp stop
       set tmpstr [exec epoch now]
       set timestr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]  
       $cmdhistory insert $crnt_cmd "$timestr\t Stop BBA DP display"
       incr crnt_cmd 1

    }

}