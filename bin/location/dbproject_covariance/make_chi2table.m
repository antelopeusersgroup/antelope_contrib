%MAKE_CHI2TABLE
%
%	make_chi2table
%	Matlab M-file to generate a table of chi-squared values 
%	for a given confidence level, specified between 0 and 1 
%	as the CONF argument. The MAX_DOF freedom specifies the 
%	largest number of degrees of freedom to put in the table.
%	The table always starts at 1 degree of freedom. FILENAME
%	is the name of an Antelope parameter file into which 
%	the results will be written, usually 'chi2.pf'. CONF 
%	may be specified as a vector of multiple values, in which 
%	case tables are calculated for all specified values.

function make_chi2table( conf, max_dof, filename )

if nargin ~= 3, error('Usage: make_chi2table( conf, max_dof, filename )'); end

mypf=dbpf;

chi2 = cell( length(conf), 1 );

for i=1:length(conf),
   chi2{i}.conf = conf(i);
   chi2{i}.values=cellstr(num2str(chi2inv(conf(i),[1:max_dof])'));
end

pfput( mypf,'chi2',chi2 )
pfwrite( mypf, filename )
clear( mypf );

return
