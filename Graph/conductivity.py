import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
import pandas as pd
import numpy as np

def main():
    inputdat = pd.read_table("Data/conductivity.dat", header=None, delim_whitespace=True)
    stp = inputdat[inputdat.keys()[0]]
    k_phtm_up = inputdat[inputdat.keys()[1]]
    k_itfs_up = inputdat[inputdat.keys()[2]]
    k_itfs_low = inputdat[inputdat.keys()[3]]
    k_phtm_low = inputdat[inputdat.keys()[4]]


    plt.xlabel("Time[fs]")
    plt.ylabel("Heat-Conductivity[W/mK]")
    #plt.xlim(0, max(z))
    #plt.ylim(0, 200)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
    
    #plt.yticks(np.arange(0, 301, 50))
    plt.grid()    
    plt.plot(stp, k_phtm_up, label="Upper(Phantom)", color="red")
    #plt.plot(stp, k_itfs_up, label="Upper(Interface)", color="magenta")
    #plt.plot(stp, k_itfs_low, label="Lower(Interface)", color="blue")
    plt.plot(stp, k_phtm_low, label="Lower(Phantom)", color="magenta")

    plt.legend()
    plt.show()

main()