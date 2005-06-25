namespace SEISPP
{
enum Filter_Type {highpass,lowpass,bandpass,WAA,WAV,WAD,DIF,DIF2,INT,INT2,DEMEAN};

class TimeInvariantFilter
{
public:
	Filter_Type type;
	double fmin();
	double fmax();
	int fmin_poles();
	int fmax_poles();
	string type_description();
	TimeInvariantFilter(){f1=0.0;f2=0.0;npole1=0;npole2=0;};
	TimeInvariantFilter(string);
	TimeInvariantFilter(double flow, int npl, double fhigh, int fph);
	TimeInvariantFilter(const TimeInvariantFilter&);
	TimeInvariantFilter& operator=(const TimeInvariantFilter&);
	void apply(int ns, double *s,double dt);
	void apply(int ns, float *s,double dt);
	void apply(TimeSeries& ts);
	void apply(ThreeComponentSeismogram& tce);
	void apply(Dbptr tr);

private:
	string filter_spec;
	// keep these here to avoid having to parse this 
	double f1,f2;  // low and high corner respectively
	int npole1,npole2;
};
// helpers
void FilterEnsemble(TimeSeriesEnsemble& ensemble,
		TimeInvariantFilter& filter);
void FilterEnsemble(ThreeComponentEnsemble& ensemble,
		TimeInvariantFilter& filter);
}  // End namespace SEISPP


