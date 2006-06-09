proc CommandCheckoff {w args} { 
	frame $w 
	button $w.b 
	set command_configured 0
	set commandcolor ""
	set nocommandcolor ""
	checkbutton $w.cb -command "$w.b config -state normal"
	pack $w.cb -side left -fill y 
	pack $w.b -side left -fill x -expand 1

	while { ! [lempty $args] } {
		set option [lvarpop args]
		switch -- $option {
		-label	{ set value [lvarpop args] ; \
			  $w.b config -text $value }
		-labeljustify { set value [lvarpop args] ; \
			        $w.b config -justify $value }
		-labelanchor { set value [lvarpop args] ; \
			       $w.b config -anchor $value }
		-variable { 
			set value [lvarpop args] 	
			global $value
			$w.cb config -variable $value
		}
		-background { set value [lvarpop args] ; \
			      $w.b config -background $value ; \
			      $w.cb config -background $value ; \
			      $w config -background $value }
		-command { 
			set value [lvarpop args] 
			if { $value != "" } {
				set command_configured 1
			}
			$w.b config -command \
			     "$w.cb select; \
			     $w.b config -state disabled; \
			     $value"
			}
		-commandcolor {
			if { $value != "" } {
			     set commandcolor [lvarpop args] 
			}
			}
		-nocommandcolor {
			if { $value != "" } {
			     set nocommandcolor [lvarpop args] 
			}
			}
		}
	}
	if { $command_configured == 1 && $commandcolor != "" } {

		$w.b config -activebackground $commandcolor

	} elseif { $command_configured == 0 && $nocommandcolor != "" } {

		$w.b config -activebackground $nocommandcolor
	}

	return $w
}
