proc showanza {orb } {

global aorbselect
global adassel
global adcsel
global apfdasarr
global aparams
global Font10
global Font12
global Font14
global AVWin
global asrcid
global atmwdg 
global asitepar 
global aparval 
global apsite
global anpar_in_site
global astarted
global anzartp 
global anzawin
global pfdas
global pfdasid 

set sid 1
set chid 1
set astarted 0
set null "\0"


if { $anzawin > 10 } {
   puts "too much window reopens!\n"
   destroy .
}

set AVWin ".anzarval_$anzawin"
incr anzawin 1

set nsite [array size apsite]
if { $nsite > 0 }  {
   foreach i [array names apsite]  {

      unset apsite($i)
   }
}

    set npar [array size asrcid]
    incr npar 1                
    loop i 1 $npar {
      unset asrcid($i)        
    }

set i 1

set sparams [lsort $aparams]

foreach par $sparams {
    set asrcid($i) $par
    set tmpstr [split $par _]
    set sta($i) [format "%s" [lindex $tmpstr 0]]
    set chan($i) [format "%s" [lindex $tmpstr 1]]
    if { $i == 1 }  {
       set apsite($sid) [format "%s" $sta($i)]
       set fsta($apsite($sid)) [format "$AVWin.par.s%s" $apsite($sid)]
       set stalbl($apsite($sid)) [format "$AVWin.par.s%s.name" $apsite($sid)]
       set parfr($apsite($sid)) [format "$AVWin.par.s%s.par" $apsite($sid)]
       set valfr($apsite($sid)) [format "$AVWin.par.s%s.val" $apsite($sid)]
    }
    set diff [string compare $apsite($sid) $sta($i) ]

    if { $diff != 0 }  {

       incr sid
       set chid 1
       set apsite($sid) [format "%s"  $sta($i)]
       set fsta($apsite($sid)) [format "$AVWin.par.s%s" $apsite($sid)]
       set stalbl($apsite($sid)) [format "$AVWin.par.s%s.name" $apsite($sid)]
       set parfr($apsite($sid)) [format "$AVWin.par.s%s.par" $apsite($sid)]
       set valfr($apsite($sid)) [format "$AVWin.par.s%s.val" $apsite($sid)]
    }
    set asitepar($sid,$chid) $par 
    set parname($par) $chan($i) 
    set parlbl($par) [format "$AVWin.par.s%s.par.p%s" $apsite($sid) $chan($i)]
    set aparval($par) [format "$AVWin.par.s%s.val.val_%s" $apsite($sid) $chan($i)]
  
    incr chid
    set anpar_in_site($apsite($sid)) $chid
    incr i
}

#
# set window 

toplevel $AVWin
wm title $AVWin "ANZA DIAGNOSTIC PARAMETERS"

frame $AVWin.par -relief ridge -bd 6 -bg DarkSeaGreen

set nsite [array size apsite]
incr nsite 

loop i 1 $nsite  {

   frame $fsta($apsite($i)) -bd 4 -relief ridge -bg DarkSeaGreen
   frame $parfr($apsite($i)) -bd 2 -relief ridge -bg DarkSeaGreen
   frame $valfr($apsite($i)) -bd 2 -relief ridge -bg DarkSeaGreen

   label $stalbl($apsite($i)) \
         -font $Font14 \
         -text $apsite($i) \
         -relief ridge \
         -width 8 \
         -bd 2 \
         -bg IndianRed 

   set nch $anpar_in_site($apsite($i))
   loop j 1 $nch  {
      set par $asitepar($i,$j) 
      label $parlbl($par) \
            -font $Font10 \
            -text $parname($par) \
            -relief ridge \
            -width 8 \
            -bd 1 \
            -bg peru 

      pack $parlbl($par) -side left -fill both -expand 1 
   }
   loop j 1 $nch  {
      set par $asitepar($i,$j) 

      label $aparval($par) \
            -font $Font12 \
            -text " "  \
            -relief sunken \
            -width 8 \
            -bd 2 \
            -bg LemonChiffon

       pack $aparval($par) -side left -fill both -expand 1 
   } 
  
   pack $stalbl($apsite($i)) -side left -fill y
   pack $parfr($apsite($i)) \
        $valfr($apsite($i)) \
        -side top -fill both -expand 1  
}
loop i 1 $nsite  {
  pack $fsta($apsite($i)) -fill both -expand 1 
}
 
set tmpstr [exec epoch now]
set timestr [format "%s %s" [lindex $tmpstr 2] [lindex $tmpstr 3]]  

frame $AVWin.control -relief raised -bg DarkSeaGreen -bd 4

label $AVWin.control.stlbl \
      -font $Font12 \
      -text "Start Time:" \
      -relief ridge \
      -bd 2 \
      -bg IndianRed 

label $AVWin.control.stime \
      -font $Font12 \
      -text $timestr \
      -relief sunken \
      -width 24 \
      -bd 2 \
      -bg LemonChiffon 

label $AVWin.control.crntlbl \
      -font $Font12 \
      -text "Current Time:" \
      -bd 2 \
      -bg IndianRed \
      -relief ridge

label $AVWin.control.crnttim \
      -font $Font12 \
      -text "00:00" \
      -relief sunken \
      -bg LemonChiffon  \
      -bd 2 \
      -width 24

button $AVWin.control.start \
      -font $Font12 \
      -text "START" \
      -bd 2 \
      -relief raised \
      -bg IndianRed \
      -activebackground red \
      -command " getnew_anza $orb "

button $AVWin.control.quit \
      -font $Font12 \
      -text "STOP" \
      -bd 2 \
      -relief raised \
      -activebackground red \
      -bg IndianRed \
      -command "stoprtd_anza; set astarted 0"

button $AVWin.control.exit \
      -font $Font12 \
      -text "DISMISS" \
      -relief raised \
      -bd 2 \
      -activebackground red \
      -bg IndianRed \
      -command "stoprtd_anza; clear_cmd anzartp $astarted; destroy $AVWin "

pack $AVWin.control.stlbl $AVWin.control.stime \
     $AVWin.control.crntlbl $AVWin.control.crnttim \
     -side left -padx 1m -pady 1m -ipadx 1m -ipady 1m -fill x

pack $AVWin.control.exit $AVWin.control.quit $AVWin.control.start \
     -side right -padx 1m -pady 1m -fill x 

pack $AVWin.par -side top -fill both -expand 1
pack $AVWin.control -side top -fill x 

#

bind $AVWin <Control-c> {destroy $AVWin}
bind $AVWin <Control-q> {destroy $AVWin}
focus $AVWin

set atmwdg $AVWin.control.crnttim

}

proc getnew_anza { orb }  {

   global anpar_in_site apsite asrcid atmwdg asitepar 
   global aparval astarted apfdasarr
   global aorbselect adcsel adassel   
   global cmdhistory crnt_cmd pfdasid pfdas

   dprt anzartp $orb 

# do orbselect
   anzartp orbsel $aorbselect
#puts "anzartp orbsel $aorbselect \n"

# specify patterns for "regex" 

   anzartp select $adcsel $adassel 
#puts "anzartp select $adcsel $adassel\n"

#
# DC stores DAS parameters based on sta ID
# Set STA - STA_ID matching; take info from ucp parameter file 
# 
   set num [llength $pfdasid]
   loop i 0 $num {
       set das [lindex $pfdas $i]
       set dasid [lindex $pfdasid $i]
#puts "anzartp setid  $das $dasid \n"
       anzartp setid  $das $dasid 
   }

# 
# introduce a "callbacking" routine.
# 

#  anzartp verbose 
   anzartp parcallback showval

# 
# Set a Time widget
# 

   anzartp timwidget $atmwdg
# 
#  set "diagpars" widgets 
# 
   set npar [array size asrcid]
   incr npar 1                
   loop i 1 $npar {

#puts " add $asrcid($i) \n" 
       anzartp add $asrcid($i)        
   }

   set nsite [array size apsite]
   incr nsite 

#puts "nsite: $nsite\n"

   loop i 1 $nsite  {
   
      set nch $anpar_in_site($apsite($i))
      loop j 1 $nch  {
          set par $asitepar($i,$j) 
          anzartp widget $par null $aparval($par) null 
#puts "anzartp widget $par $aparval($par)\n"
      } 
   }

#  Start
   
   set astarted 1

#puts "Start \n"

   set tmp [exec epoch now]
   set timstr [format "%s %s" [lindex $tmp 2] [lindex $tmp 3]]   
   $cmdhistory insert $crnt_cmd "$timstr\t Start ANZA DP display"
   incr crnt_cmd 1
   
   anzartp getanzapar
  
}

proc stoprtd_anza { }  {

   global anzartp astarted cmdhistory crnt_cmd


    if { $astarted == 1 } {

       anzartp stop
       set tmp [exec epoch now]
       set timstr [format "%s %s" [lindex $tmp 2] [lindex $tmp 3]]   
       $cmdhistory insert $crnt_cmd "$timstr\t Stop ANZA DP display"
       incr crnt_cmd 1
    }

}