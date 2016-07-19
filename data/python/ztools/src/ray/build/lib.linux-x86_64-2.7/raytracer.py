import ztools.ray.ray_tracer_cc as rt
import numpy as np

class RayTracer:
    def __init__(self, layers, vels, evdp, Q_vals=None):
        """
        Inputs:
            layers = layer depths
            vels   = velocity values at specific depths w/ linear interp
            evdp   = source depth in km
            Q_vals = list of Q values at specific depths [None]
        """
        if Q_vals is None:
            self.Q_vals = np.ones(len(layers))*1000
        self.evdp = evdp
        ta = []
        x = []
        t = []
        t_stars = []
        for inc in np.arange(50, 180):
            xx, tt, t_star = rt.ray_trace(layers, vels, Q_vals, inc, evdp)
            if np.isnan(xx):
                continue
            x.append(xx)
            t.append(tt)
            t_stars.append(t_star)
            ta.append(inc)
        x = np.array(x)
        t = np.array(t)
        t_stars = np.array(t_stars)
        idx = np.argsort(x)
        x = x[idx]
        t = t[idx]
        t_stars = t_stars[idx]

        self.x = x
        self.t = t
        self.t_stars = t_stars

    def inc(self, x):
        return self.x_inc(x)

    def t_star(self, x):
        try:
            return np.interp(x, self.x, self.t_stars)
        except:
            print "ERROR: %f,%f outside INTERP RANGE FOR TSTAR" % (x, self.evdp)
            print "X RANGE", self.x[0], self.x[-1]
            print "T RANGE", self.t[0], self.t[-1]
            print "T* RANGE", self.t_stars[0], self.t_stars[-1]
            raise ValueError

    def tt(self, x):
        return self.x_t(x)
