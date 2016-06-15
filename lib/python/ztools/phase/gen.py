#functions that don't fit elsewhere

import matplotlib.pyplot as plt
import numpy as np
import sys
import os
from os.path import splitext
import ztools.phase.algorithm as alg

#------------------------------------------------
def next_pow_2(x):
    return 1<<(x-1).bit_length()

#-------------------------------------------------------------
def xcf(x, y):
    xlen = np.size(x)
    ylen = np.size(y)
    if len(x) != len(y):
        print "Error: lengths of X, Y different in XCF"
        sys.exit()
    olen = xlen + ylen - 1
    wlen = next_pow_2(olen)
    corr = np.fft.irfft(np.fft.rfft(x, n=wlen) * np.fft.rfft(y[::-1], n=wlen))
    xcov = np.sqrt(np.dot(x,x))
    ycov = np.sqrt(np.dot(y,y))
    return corr[:olen]/(xcov*ycov)

#-------------------------------------------------------------
def get_tt(dist, evdp, l_depth, l_vel, tol=0.01):
    x, t = alg.get_tt(dist, evdp, l_depth, l_vel, tol)
    return x, t

#-------------------------------------------------------------
def basicplot(trace):
    plt.plot(range(len(trace)), trace)
    plt.show()

#-------------------------------------------------------------
def plot(t, trace, picks=None):
    f = plt.figure()
    ax = plt.axes()
    ax.plot(t, trace, 'k')
    ymin, ymax = ax.get_ylim()
    for i in xrange(len(picks)):
        if i == 0:
            ax.vlines(picks[i], ymin, ymax, color='r')
        if i == 1:
            ax.vlines(picks[i], ymin, ymax, color='b')
        if i >= 2:
            ax.vlines(picks[i], ymin, ymax, color='k')
    plt.show()

#-------------------------------------------------------------
def plotter(*args, **kwargs):
    """
    Subplot function. Assumes the first argument is for the x axis
    (time usually). kwargs is a keyword list of items like picks,
    time series colors, etc
    """
    nplot = len(args)-1
    if 'sharex' in kwargs:
        sharex = kwargs['sharex']
    else:
        sharex = True
    if 'sharey' in kwargs:
        sharey = kwargs['sharey']
    else:
        sharey = False
    if 'plotpk' not in kwargs:
        kwargs['plotpk'] = [True]*nplot

    f, ax = plt.subplots(nplot, sharex=sharex, sharey=sharey)
    for i in xrange(1,nplot+1):
        max_yticks = 4
        yloc = plt.MaxNLocator(max_yticks)
        if nplot > 1:
            ax[i-1].yaxis.set_major_locator(yloc)
        else:
            ax.yaxis.set_major_locator(yloc)
        if 'color' in kwargs:
            # Change line/color info?
            if len(args[i]) > 2:
                if nplot > 1:
                    ax[i-1].plot(args[0], args[i], kwargs['color'][i-1])
                else:
                    ax.plot(args[0], args[i], kwargs['color'])
            elif len(args[i]) == 2:
                if nplot > 1:
                    ax[i-1].plot(args[0], args[i][0], 'r', args[0], args[i][1], 'b')
                else:
                    ax.plot(args[0], args[i][0], 'r', args[0], args[i][1], 'b')
        else:
            if nplot > 1:
                ax[i-1].plot(args[0], args[i], 'k')
            else:
                print len(args[0]), len(args[i])
                ax.plot(args[0], args[i], 'k')
        if 'picks' in kwargs:
            # plot picks?
            if nplot > 1:
                ymin, ymax = ax[i-1].get_ylim()
            else:
                ymin, ymax = ax.get_ylim()
            if kwargs['plotpk'][i-1]:
                for j in xrange(len(kwargs['picks'])):
                    if j == 0:
                        if nplot > 1:
                            ax[i-1].vlines(kwargs['picks'][j],
                                           ymin,
                                           ymax,
                                           color='r',
                                           linewidth=1.75)
                        else:
                            ax.vlines(kwargs['picks'][j],
                                      ymin,
                                      ymax,
                                      color='r',
                                      linewidth=1.75)
                    elif j == 1:
                        if nplot > 1:
                            ax[i-1].vlines(kwargs['picks'][j],
                                           ymin,
                                           ymax,
                                           color='b')
                        else:
                            ax.vlines(kwargs['picks'][j],
                                      ymin,
                                      ymax,
                                      color='b')
                    elif j == 2:
                        if nplot > 1:
                            ax[i-1].vlines(kwargs['picks'][j],
                                           ymin,
                                           ymax,
                                           color='g')
                        else:
                            ax.vlines(kwargs['picks'][j],
                                      ymin,
                                      ymax,
                                      color='g')
                    elif j == 3:
                        if nplot > 1:
                            ax[i-1].vlines(kwargs['picks'][j],
                                           ymin,
                                           ymax,
                                           color='m')
                        else:
                            ax.vlines(kwargs['picks'][j],
                                      ymin,
                                      ymax,
                                      color='m')
                    elif j == 4:
                        if nplot > 1:
                            ax[i-1].vlines(kwargs['picks'][j],
                                           ymin,
                                           ymax,
                                           color='y')
                        else:
                            ax.vlines(kwargs['picks'][j],
                                      ymin,
                                      ymax,
                                      color='y')
                    else:
                        if nplot > 1:
                            ax[i-1].vlines(kwargs['picks'][j],
                                           ymin,
                                           ymax,
                                           color='k')
                        else:
                            ax.vlines(kwargs['picks'][j],
                                      ymin,
                                      ymax,
                                      color='k')
        if 'text' in kwargs:
            if nplot > 1:
                ax[i-1].text(0.9,0.8, kwargs['text'][i-1], ha='center', va='center',
                transform=ax[i-1].transAxes)
            else:
               ax.text(0.9,0.8, kwargs['text'], ha='center', va='center',
                transform=ax.transAxes)
        if 'text2' in kwargs:
            if nplot > 1:
                ax[i-1].text(0.9,0.6, kwargs['text2'][i-1], ha='center', va='center',
                transform=ax[i-1].transAxes)
            else:
                ax.text(0.9,0.6, kwargs['text2'], ha='center', va='center',
                transform=ax.transAxes)
        if 'text3' in kwargs:
            if nplot > 1:
                ax[i-1].set_ylabel(kwargs['text3'][i-1])
            else:
                ax.set_ylabel(kwargs['text3'])
        if 'plot_box' in kwargs:
            if kwargs['plot_box'] == True:
                if nplot > 1:
                    my_ymin, my_ymax = ax[i-1].get_ylim()
                    ax[i-1].fill_between(range(24, 41),
                                         [my_ymax for k in range(24, 41)],
                                         y2=[my_ymin for k in range(24, 41)],
                                         color='0.85')
                    ax[i-1].set_xlim((args[0][0], args[0][-1]))
                    ax[i-1].set_ylim(my_ymin, my_ymax)
                else:
                    my_ymin, my_ymax = ax.get_ylim()
                    ax.fill_between(range(24, 41),
                                    [my_ymax for i in range(24, 41)],
                                    y2=[my_ymin for i in range(24, 41)],
                                    color='0.85')
                    ax.set_ylim(-200, 200)
                print my_ymin, my_ymax

    plt.show()
    return

#-------------------------------------------------------------
def normalize(trace):
    maxVal = np.max(trace)
    minVal = np.min(trace)
    if np.abs(maxVal) > np.abs(minVal):
        absmax = np.abs(maxVal)
    else:
        absmax = np.abs(minVal)
    return trace/absmax

#-------------------------------------------------------------
def frange(limit1, limit2 = None, increment = 1.):
    """
    Range function that accepts floats (and integers).

    Usage:
    frange(-2, 2, 0.1)
    frange(10)
    frange(10, increment = 0.5)

    The returned value is an iterator.  Use list(frange) for a list.
    """

    if limit2 is None:
        limit2, limit1 = limit1, 0.
    else:
        limit1 = float(limit1)

    count = int(np.ceil((limit2 - limit1)/increment))
    return (limit1 + n*increment for n in range(count))

#-------------------------------------------------------------
