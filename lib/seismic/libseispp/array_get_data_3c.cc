ThreeComponentEnsemble 
  *array_get_data(TimeSeriesEnsemble *array_get_data(SeismicArray& stations, 
	Hypocenter& hypo,string phase, 
        TimeWindow data_window, double tpad,
        DatascopeHandle& dbwf, 
	StationChannelMap& scmap,
	MetadataList& ensemble_mdl, MetadataList& member_mdl,
        AttributeMap& am)
{
	ThreeComponentEnsemble *result=NULL;
	StationTime times=ArrayPredictedArrivals(stations,hypo,phase);
	TimeWindow arrival_range=StationTimeRange(times);
	TimeWindow read_window(arrival_range.start-tpad+data_window.start,
                        arrival_range.end+tpad+data_window.end);
	// We call this constructor to get all data AND require that site
	// and sitechan information will be loaded into Metadata area of 
	// each member of the ensemble.
	try {
		TimeSeriesEnsemble rawdata(dbwf,read_window,
			"none","none",true,true,false);
	} catch (...) {throw;}
	// This procedure can be called because we can assume the data are now
	// segmented here.  i.e. each sta:chan defines a unique member
	StaChanSort(rawdata);
	//
	// DAta are now assumed sorted by sta:chan.  This means we should be able
	// to bundle all data in groups of 3.  As we work through data we use
	// the StationChannelMap object to sort out the channel hierarchy.
	//
	try {
		string current_sta=rawdata.member[0].get_string("sta");
		ThreeComponentChannelMap tccm=scmap.channels(current_sta);
		vector<int> comp,prec,member_index;
		string chan;
		for(i=0;i<rawdata.members.size();++i)
		{
			string nextsta=rawdata.member[i].get_string("sta");
			if(nextsta==current_sta)
			{
				try {
					chan=rawdata.member[i].get_string("chan");
					comp.push_back(tccm.component(chan));
					prec.push_back(tccm.precedence(chan));
					member_index.push_back(i);
				} 
				catch (MetadataError mde)
				{
					mde.log_error();
					throw SeisppError("array_get_data:  required attribute chan is not defined in input data");
				}
				catch (SeisppError sde)
				{
					cerr << "array_get_data:  Problem with member "
						<< i 
						"= station "<<current_sta
						<< " of raw input ensemble"<<endl;
					sde.log_error();
				}
			}
			else
			{
			//
			// Land here when a grouping is completed and we have
			// to sort out which channels are to be kept.
			//
				int ncomponents=comp.size();
				if(ncomponents==3)
				{
					// nothing to sort out in this case
					// just create the new 3c member

				}
				//
				// Note this silently skips stations when there
				// are less than 3 components recorded
				//
				else if(ncomponents>3)
				{
				}
				else
				{
					// only land here if ncomponents<3 meaning bad data
					cerr << "
