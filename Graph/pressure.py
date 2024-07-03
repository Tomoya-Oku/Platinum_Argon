import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
import pandas as pd
import numpy as np

def main():
    inputdat = pd.read_table("Data/pressure.dat", header=None, delim_whitespace=True)
    stp = inputdat[inputdat.keys()[0]]
    pressure = inputdat[inputdat.keys()[1]]

    plt.xlabel("Time[fs]")
    plt.ylabel("Pressure[Pa]")
    #plt.xlim(0, max(z))
    #plt.ylim(0, 500)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
    
    #plt.yticks(np.arange(0, 301, 50))
    plt.grid()    
    plt.plot(stp, pressure, color="black")
    plt.show()

main()