proc CommandCheckoff {w args} { 
	frame $w
	button $w.b 
	checkbutton $w.cb -command "$w.b config -state normal"
	pack $w.cb -side left
	pack $w.b -side left -fill x -expand 1
	while { ! [lempty $args] } {
		set option [lvarpop args]
		switch -- $option {
		-label	{ set value [lvarpop args] ; $w.b config -text $value }
		-variable { 
			set value [lvarpop args] 	
			global $value
			$w.cb config -variable $value
		}
		-background { set value [lvarpop args] ; $w.b config -background $value }
		-command { 
			set value [lvarpop args] 
			$w.b config -command \
			     "$w.cb select; $w.b config -state disabled; $value"
			}
		}
	}
	return $w
}
