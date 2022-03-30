import ztools.shear.dbshear as algorithm

def shear_pick(Z, N, E, dt):
    from __main__ import pfile
    cov_len = pfile['cov_len']
    min_dt = pfile['min_dt']
    k_len = pfile['k_len']
    out = algorithm.dbshear(Z, N, E, cov_len, dt, k_len)
    s1_pick, s2_pick, snr_s1, snr_s2 = out

    # Checking for the various possible pick results
    if s1_pick > 0 and s2_pick > 0:
        if snr_s1 > snr_s2:
            s_pick = s1_pick
            snr = snr_s1
            chan = 0
        else:
            s_pick = s2_pick
            snr = snr_s2
            chan = 1
    elif s1_pick > 0:
        s_pick = s1_pick
        snr = snr_s1
        chan = 0
    elif s2_pick > 0:
        s_pick = s2_pick
        snr = snr_s2
        chan = 1
    else:
        s_pick = None
        snr = None
        chan = None
    return s_pick, snr, chan
