
typedef struct {
    float           real,
                    imag;
}               complex;

extern double mycabs ( double real, double imag );
extern float c_abs ( complex z );
extern complex * c_cos ( complex z );
extern complex * c_div ( complex a, complex b );
extern complex * c_mult ( complex a, complex b );
extern complex * c_ad ( complex a, complex b );
extern complex * c_sub ( complex a, complex b );
extern complex * c_exp ( complex z );
extern complex * c_log ( complex z );
extern complex * c_sin ( complex z );
extern complex * c_sqrt ( complex z );


