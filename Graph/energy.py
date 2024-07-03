import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
import pandas as pd
import numpy as np

def main():
    input_dat = pd.read_table("Data/energy.dat",header=None, delim_whitespace=True)

    step = input_dat[input_dat.keys()[0]]
    tote = input_dat[input_dat.keys()[1]]
    pote = input_dat[input_dat.keys()[2]]
    kine = input_dat[input_dat.keys()[3]]

    plt.xlabel("Time[fs]")
    plt.ylabel("Energy[J]")
    #plt.xlim(0, len(step)*10)
    #plt.ylim(-8e-16, -7e-16)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
    
    plt.grid(linestyle="dotted")

    plt.plot(step, tote, label="Total Energy", color="red")
    plt.plot(step, pote, label="Potential Energy", color="blue")
    plt.plot(step, kine, label="Kinetic Energy", color="green")
    plt.legend()
    plt.show()

main()