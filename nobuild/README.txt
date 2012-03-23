
This 'nobuild' directory of the Antelope contributed-code repository 
contains programs that for one reason or another are not to be 
automatically built by BRTT for inclusion with the official Antelope
release. The source codes for these programs are left in contrib 
as a resource for the community. Many of them may still have value 
for programmer edification or non-operational use. Most, however, 
are offered by the author(s) with absolutely no promise of support or 
even proper function.

Ideally the directory for each subprogram will include 
a README.txt file from the author explaining why the program or 
library is not to be compiled into Antelope proper. (The 
file 'template_for_nobuild_README.txt' contains a template for the 
creation of such README.txt files for new additions to the 'nobuild'
directory).

Although the top-level Makefile for the Antelope contributed
code repository does not invoke compilation in the 'nobuild'
directory, the 'nobuild' directory itself does contain lower-level 
Makefiles, such that one could in principle execute 
	
	cd nobuild
	make install

to compile all the elements within (or similarly in any of the 
subdirectories). In practice this would be ill-advised,
however, ranging from a mildly bad idea to an extremely bad 
idea, depending on the reasons why each program or library was moved
to 'nobuild' initially. A relatively safer strategy, if you have 
continued interest in a particular program, would be to change 
into the directory for that particular program only, then execute 

	make install

(presuming of course that your Antelope environment is properly set, 
for explanations of which you should refer to the Antelope documentation). 

For more information on any of these programs, please contact the 
author(s) of the program directly (in the best cases, hopefully 
most cases, the author will have left their name and contact information
at the end of the manual page for the program), taking into account
beforehand their comments in the README.txt file. 
