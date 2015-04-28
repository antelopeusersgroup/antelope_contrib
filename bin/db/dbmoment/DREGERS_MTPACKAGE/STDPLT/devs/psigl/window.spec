/* Assumptions:
	nothing else INSIDE the page definition is using gsave and grestore.
	if there is, you must then NOT use gsave/grestore, but use
	initclip to reset clipping.
window:
	if in_window 
		unwindow
	gsave
	clip with new path
	in_window = true

unwindow:
	if in_window then
		grestore
		in_window = false
	end

	
