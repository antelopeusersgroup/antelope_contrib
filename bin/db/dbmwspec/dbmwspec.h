/* Include file for dbspectra codes */
typedef struct spectra_phase_spec_
{
        char *phase_reference;
        double start,end;
	float tbwp;
} Spectra_phase_specification;
/* WARNING:  This parameter must be consistent with size parameters in
fortran code mwspec_par.i called MXDATA */
#define MAXSAMPLES 10000
/* gap fill magic number*/
#define GAP 9999.9E32

/* function prototypes */
void powspc_(int *, float *, int *,float *,float *,
	float *,float *,float *,float *,float *,int *,int *);
int save_spectrum(Dbptr, Spectra_phase_specification *,
		float *,float *,int, int, float, double, char *,int, int);
int correct_for_response(float *,float *,int, Dbptr);
