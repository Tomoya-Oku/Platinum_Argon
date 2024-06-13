import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
import pandas as pd
import numpy as np

maxstep = 10000

def main():
    input_dat = pd.read_table("Data/energy_Total.dat",header=None, delim_whitespace=True)

    time = np.arange(0, maxstep+1, 100)
    tote = input_dat[input_dat.keys()[0]]
    pote = input_dat[input_dat.keys()[1]]
    kine = input_dat[input_dat.keys()[2]]

    plt.xlabel("Time[fs]")
    plt.ylabel("Energy[J]")
    plt.xlim(0, len(time)*100)
    #plt.ylim(-8e-16, -7e-16)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
    
    plt.grid(linestyle="dotted")

    plt.plot(time, tote, label="Total Energy", color="red")
    plt.plot(time, pote, label="Potential Energy", color="blue")
    plt.plot(time, kine, label="Kinetic Energy", color="green")
    plt.legend()
    plt.show()

###################################################################################################

main()