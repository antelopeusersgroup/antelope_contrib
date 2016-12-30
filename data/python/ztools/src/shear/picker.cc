#include "picker.h"
void trigger(vector<double> cft,
             double t_on,
             double t_off,
             double min_dur,
             double dt,
             int &start,
             int &stop,
             double &best_peak) {
    vector<double>::size_type i;
    vector<int> ons, offs;
    int min_npts;
    int state(0);
    start = -1;
    stop = -1;
    best_peak = 0;
    min_npts = int(min_dur/dt);
    // Find each trigger
    for (i = 0; i < cft.size(); i++) {
        if (state == 1) {
            if (cft[i] < t_off) {
                offs.push_back(i);
                state = 0;
            }
        }
        else {
            if (cft[i] >= t_on) {
                ons.push_back(i);
                state = 1;
            }
        }
    }
    if (state) offs.push_back(cft.size()-1);
    if (ons.size() != offs.size()) {
        std::cout << "Error: trigger ons not same length as trigger offs\n";
    }
    vector<double>::iterator idx = cft.begin();
    vector<double>::iterator tmp_peak;
    for (i = 0; i < ons.size(); i++) {
        if ((offs[i] - ons[i]) < min_npts) continue;
        tmp_peak = max_element(idx+ons[i], idx+offs[i]);
        if (*tmp_peak > best_peak) {
            best_peak = *tmp_peak;
            start = ons[i];
            stop = offs[i];
        }
    }
    return;
}

vector<double> stalta(vector<double> const &tr,
                      unsigned int n_sta,
                      unsigned int n_lta,
                      double t_on,
                      double t_off) {
    // Exponential smoothing
    vector<double> cft(tr.size());
    double alpha(1./n_sta), beta(1./n_lta), ialpha, ibeta;
    double sta, lta, sq;
    size_t i;
    int state(0);

    ialpha = 1 - alpha;
    ibeta = 1 - beta;
    sta = 0;
    lta = 1e-30;
    for (i = 1; i < tr.size(); i++) {
        sq = abs(tr[i]);
        sta = alpha * sq + ialpha * sta;
        if (state == 0) {
            lta = beta * sq + ibeta * lta;
            cft[i] = sta/lta;
            if (cft[i] > t_on) {
                state = 1;
            }
        }
        else {
            cft[i] = sta/lta;
            if (cft[i] < t_off) {
                state = 0;
            }
        }
        if (i < n_lta) {
            cft[i] = 0;
        }
    }
    return cft;
}

vector<double> lstalta(vector<double> const &tr,
                       unsigned int n_sta,
                       unsigned int n_lta) {
    // Locking STA/LTA (not-recursive)
    vector<double> cft(tr.size(), 0);
    double sta(0), lta(0);
    size_t i, j;
    for (i = 0; i < n_lta; i++) {
        lta += fabs(tr[i]);
    }
    lta /= n_lta;
    for (i = n_lta; i < tr.size(); i++) {
        sta = 0;
        for (j = i-n_sta; j < i; j++) {
            sta += fabs(tr[j]);
        }
        sta /= n_sta;
        cft[i-n_sta] = sta/lta;
    }
    return cft;
}

vector<double> kurtosis(vector<double> const &tr, int n_kurt) {
    vector<double> cft(tr.size(), 0);
    size_t i, j;
    double square, temp, quad, numer, denom, inv(1./n_kurt);
    for (i = n_kurt-1; i < tr.size(); i++) {
        square = 0;
        quad = 0;
        for (j = i-n_kurt+1; j < i+1; j++) {
            temp = tr[j]*tr[j];
            square += temp;
            quad += temp*temp;
        }
        numer = inv*quad;
        denom = pow(inv*square, 2.0);
        cft[i] = numer/denom - 3.0;
        if (isnan(cft[i])) {
            cft[i] = 0;
        }
    }
    return cft;
}

vector<int> find_maxima(vector<double> const &tr) {
    size_t i;
    vector<int> peaks;
    for (i = 1; i < tr.size()-1; i++) {
        if (tr[i] > tr[i-1] && tr[i] > tr[i+1]) {
            peaks.push_back(i);
        }
    }
    return peaks;
}

vector<double> diff(vector<double> const &tr, double dt) {
    vector<double> cft(tr.size(), 0);
    size_t i;
    for (i = 1; i < tr.size(); i++) {
        cft[i-1] = (tr[i]-tr[i-1])/dt;
    }
    return cft;
}

std::vector<size_t>
argsort(const std::vector<double> &v) {
    // initialize original index locations
    std::vector<size_t> idx(v.size());
    if (v[0] >= v[1] && v[1] >= v[2]) {
        idx[2] = 0;
            idx[1] = 1;
            idx[0] = 2;
    }
    else if (v[0] >= v[2] && v[2] >= v[1]) {
        idx[2] = 0;
            idx[1] = 2;
            idx[0] = 1;
    }
    else if (v[1] >= v[2] && v[2] >= v[0]) {
        idx[2] = 1;
            idx[1] = 2;
            idx[0] = 0;
    }
    else if (v[1] >= v[0] && v[0] >= v[2]) {
        idx[2] = 1;
            idx[1] = 0;
            idx[0] = 2;
    }
    else if (v[2] >= v[1] && v[1] >= v[0]) {
        idx[2] = 2;
            idx[1] = 1;
            idx[0] = 0;
    }
    else if (v[2] >= v[0] && v[0] >= v[1]) {
        idx[2] = 2;
            idx[1] = 0;
            idx[0] = 1;
    }
    return idx;
}

Polarizer::Polarizer (double win_len, double delta) {
    dt = delta;
    w_len = int(win_len/dt);
}

vector<double> Polarizer::filter (const vector<double> &Z,
                                  const vector<double> &N,
                                  const vector<double> &E) {
    double r, phi;
    vector<double> NN, EE, ZZ, NZ, NE, ZE, fltr(Z.size(), 1), temp(3,0);
    vector<double>::iterator i;
    vector<size_t> order(3);
    int j(w_len);
    Eigen::MatrixXd C = Eigen::MatrixXd::Constant(3,3,0);
    NN = moving_cov(N, N);
    EE = moving_cov(E, E);
    ZZ = moving_cov(Z, Z);
    NE = moving_cov(N, E);
    NZ = moving_cov(N, Z);
    ZE = moving_cov(Z, E);
    for (i = fltr.begin() + w_len; i < fltr.end(); i++) {
        C(0,0) = EE[j];
        C(1,1) = NN[j];
        C(2,2) = ZZ[j];
        C(0,1) = NE[j];
        C(0,2) = ZE[j];
        C(1,2) = NZ[j];
        C(1,0) = C(0,1);
        C(2,0) = C(0,2);
        C(2,1) = C(1,2);
        Eigen::EigenSolver<Eigen::MatrixXd> solver(C);
        Eigen::VectorXd U = solver.eigenvalues().real().cast<double>();
        Eigen::VectorXd::Map(&temp[0], U.size()) = U;
        //order = sort_indexes(temp);
        order = argsort(temp);
        phi = abs(solver.eigenvectors().col(order[2])(2));
        r = 1 - (U(order[0])+U(order[1]))/(2*U(order[2]));
        *i = r * (1-phi);
        j++;
    }
    return fltr;
}

vector<double> Polarizer::moving_cov (const vector<double> &X,
                                      const vector<double> &Y) {
    vector<double> C(X.size(), 0);
    vector<double>::iterator i;
    int j(w_len);
    C[w_len-1] = inner_product(X.begin(), X.begin()+w_len, Y.begin(), 0.0);
    for (i = C.begin() + w_len; i != C.end(); i++) {
        *i = *(i-1) + X[j]*Y[j] - X[j-w_len]*Y[j-w_len];
        j++;
    }
    return C;
}

ShearPicker::ShearPicker(double delta,
                         double k_dur,
                         double l_sta,
                         double l_lta,
                         double th_on,
                         double th_off,
                         double min_dur_len,
                         int p_pickk,
                         vector<double> pol_filter) {
    dt = delta;
    k_len = k_dur;
    sta = l_sta;
    lta = l_lta;
    t_on = th_on;
    t_off = th_off;
    min_dur = min_dur_len;
    p_pick = p_pickk;
    fltr = pol_filter;
}

vector<double> ShearPicker::polarize(const vector<double>& tr) {
    size_t i;
    vector<double> out(tr.size(),0);
    for (i = 0; i < tr.size(); i++) {
        out[i] = tr[i] * fltr[i];
    }
    return out;
}

void ShearPicker::pick(const vector<double> &tr, double &s_pick, double &snr) {
    int n_kurt, n_sta, n_lta, idx, i, start, stop;
    double peak_grad, t_sp, temp;
    vector<int> maxima;
    vector<double> kurt, pol_tr;
    vector<double>::iterator it;

    n_kurt = int(k_len/dt);
    n_sta = int(sta/dt);
    n_lta = int(lta/dt);

    // Calculate k-rate
    kurt = kurtosis(tr, n_kurt);
    cftK = diff(kurt, dt);

    // First pass STA/LTA on non-polarized trace
    cftS = lstalta(tr, n_sta, n_lta);
    trigger(cftS, t_on, t_off, min_dur, dt, start, stop, snr);
    if (start == -1 || stop == -1) {
        s_pick = -1;
        snr = -1;
        return;
    }

    // Polarize traces
    pol_tr = polarize(tr);

    // Calculate STA/LTA and try to find correct trigger window
    cftS = lstalta(pol_tr, n_sta, n_lta);

    // Calculate the peak SNR and it's location
    idx = distance(cftS.begin(), max_element(cftS.begin()+n_lta, cftS.end()));

    // Calculate window duration using several criteria
    t_sp = 1.5;
    if (p_pick != -1) {
        if (p_pick < idx) {
            t_sp = (idx - p_pick)*dt;
        }
    }
    if (t_sp < 1.5) t_sp = 1.5;
    if (t_sp > 6.0) t_sp = 6.0;

    // Window around trial S-pick to refine through k-rate
    start = idx - int(0.5*t_sp/dt);
    stop = idx + int(0.5*t_sp/dt);
    if (stop > cftK.size()) stop = cftK.size()-1;
    it = cftK.begin();
    idx = distance(it, max_element(it+start, it+stop));

    // Final pick refinement to kurtosis minima
    start = idx - int(0.1/dt);
    it = kurt.begin() + start;
    it = min_element(it, it+int(0.1/dt));
    idx = distance(kurt.begin(), it);

    if (idx - p_pick < int(0.4*dt)) {
        s_pick = -1;
        snr = -1;
        return;
    }

    s_pick = idx*dt;

    cftK = kurt;

    kurt = kurtosis(tr, int(0.5/dt));
    start = idx - int(0.25/dt);
    it = kurt.begin() + start;
    it = max_element(it, it+int(0.5/dt));
    std::cout << *it << std::endl;

    return;

}
