### DBMOMENT - earth models

The synthetic traces required by the inversion will be built using the specified earth model on the **dbmoment** parameter file. This archive provide several examples for such model. The original distribution of Dreger’s code includes the Southern California model [SOCAL_MODEL.pf](https://github.com/antelopeusersgroup/antelope_contrib/blob/master/bin/db/dbmoment/MODELS/SOCAL_MODEL.pf). This is a 1D velocity structure that the code FKRPROG will use to generate the Green’s functions. 

The files will be installed in the contributed code folder in Antelope’s main folder. The **dbmoment** parameter file will need to have an absolute reference to this file since the PFPATH environmental variable WILL NOT have this path defined. Anyone can create a new model for a new region of interest and push that new file back to GITHUB for others to have the option to analyze data from that region. 
