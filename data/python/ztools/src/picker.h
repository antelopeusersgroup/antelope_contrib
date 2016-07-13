#ifndef ADD_H
#define ADD_H
#include <iostream>
#include <vector>
#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <iterator>
#include <Eigen/Dense>
#include <Eigen/Eigenvalues>

using std::vector;
using std::inner_product;
using std::distance;
using std::max_element;
using std::min_element;

void trigger(vector<double> cft,
             double t_on,
             double t_off,
             double min_dur,
             double dt,
             int &start,
             int &stop,
             double &best_peak);

vector<double> stalta(vector<double> const &tr,
                      unsigned int n_sta,
                      unsigned int n_lta,
                      double t_on,
                      double t_off);

vector<double> lstalta(vector<double> const &tr,
                       unsigned int n_sta,
                       unsigned int n_lta);

vector<double> kurtosis(vector<double> const &tr, int n_kurt);

vector<int> find_maxima(vector<double> const &tr);

vector<double> diff(vector<double> const &tr, double dt);

std::vector<size_t>
argsort(const std::vector<double> &v);

class Polarizer {
  private:
    int w_len;
    double dt;
    vector<double> moving_cov(const vector<double>&, const vector<double>&);
  public:
    Polarizer(double, double);
    vector<double> filter(const vector<double>&, const vector<double>&,
                          const vector<double>&);
};

class ShearPicker {
    private:
        double dt;
        double k_len;
        double sta;
        double lta;
        double t_on;
        double t_off;
        double min_dur;
        int p_pick;
        vector<double> cftS;
        vector<double> cftK;
        vector<double> fltr;
    public:
        ShearPicker(double, double, double, double,
                    double, double, double, int, vector<double>);
        void pick(const vector<double>&, double&, double&);
        vector<double> get_cftS() { return cftS; };
        vector<double> get_cftK() { return cftK; };
        vector<double> polarize(const vector<double>&);
};

#endif
