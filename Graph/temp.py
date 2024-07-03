import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
import pandas as pd
import numpy as np

def main():
    temp = pd.read_table("Data/temp.dat",header=None, delim_whitespace=True)
    
    time = temp[temp.keys()[0]]
    U_Pt = temp[temp.keys()[1]]
    L_Pt = temp[temp.keys()[2]]
    AR = temp[temp.keys()[3]]

    plt.yticks(np.arange(0, 1000, 50))

    plt.xlabel("Time[fs]")
    plt.ylabel("Temperature[K]")
    # plt.xlim(0, maxstep)
    # plt.ylim(0, 300)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
    
    plt.grid()

    plt.plot(time, U_Pt, label="Upper Pt", color="red")
    plt.plot(time, L_Pt, label="Lower Pt", color="blue")
    plt.plot(time, AR, label="Ar", color="green")

    plt.legend(loc="best")
    plt.show()

main()