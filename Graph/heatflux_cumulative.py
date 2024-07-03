import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
import pandas as pd
import numpy as np

CALCSTEP = 5000

def main():
    inputdat = pd.read_table("Data/heatflux.dat", header=None, delim_whitespace=True)
    stp = inputdat[inputdat.keys()[0]]
    itfs_up = inputdat[inputdat.keys()[1]]
    itfs_low = inputdat[inputdat.keys()[2]]
    phtm_up = inputdat[inputdat.keys()[3]]
    phtm_low = inputdat[inputdat.keys()[4]]

    itfs_up_cumulative = []
    itfs_low_cumulative = []
    phtm_up_cumulative = []
    phtm_low_cumulative = []
    for i in range(max(stp)-CALCSTEP):
        itfs_up_cumulative.append(itfs_up[i] * i * pow(10, -15))
        itfs_low_cumulative.append(itfs_low[i] * i * pow(10, -15))
        phtm_up_cumulative.append(phtm_up[i] * i * pow(10, -15))
        phtm_low_cumulative.append(phtm_low[i] * i * pow(10, -15))

    plt.xlabel("Time[fs]")
    plt.ylabel("Heat[J/m^2]")
    plt.xlim(0, max(stp)-CALCSTEP)
    #plt.ylim(0, 5*10**9)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
    
    #plt.yticks(np.arange(0, 301, 50))
    plt.grid()

    plt.plot(stp-CALCSTEP, itfs_up_cumulative, label="Upper(Interface)", color="red")
    plt.plot(stp-CALCSTEP, itfs_low_cumulative, label="Lower(Interface)", color="blue")
    plt.plot(stp-CALCSTEP, phtm_up_cumulative, label="Upper(Phantom)", color="magenta")
    plt.plot(stp-CALCSTEP, phtm_low_cumulative, label="Lower(Phantom)", color="cyan")

    plt.legend()
    plt.show()

main()