import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
import pandas as pd
import numpy as np

def main():
    inputdat = pd.read_table("Data/heatflux.dat", header=None, delim_whitespace=True)
    stp = inputdat[inputdat.keys()[0]]
    itfs_up = inputdat[inputdat.keys()[1]]
    itfs_low = inputdat[inputdat.keys()[2]]
    phtm_up = inputdat[inputdat.keys()[3]]
    phtm_low = inputdat[inputdat.keys()[4]]

    plt.xlabel("Time[fs]")
    plt.ylabel("Heatflux[W/m^2]")
    #plt.xlim(0, max(z))
    #plt.ylim(0, 5*10**9)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
    
    #plt.yticks(np.arange(0, 301, 50))
    plt.grid()

    plt.plot(stp, itfs_up, label="Upper(Interface)", color="red")
    plt.plot(stp, itfs_low, label="Lower(Interface)", color="blue")
    plt.plot(stp, phtm_up, label="Upper(Phantom)", color="magenta")
    plt.plot(stp, phtm_low, label="Lower(Phantom)", color="cyan")

    plt.legend()
    plt.show()

main()