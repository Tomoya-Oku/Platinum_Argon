import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
import pandas as pd
import numpy as np

maxstep = 20000

def main():
    temp_all = pd.read_table("Data/temperature.dat",header=None, delim_whitespace=True)
    temp_itf = pd.read_table("Data/temp_interface.dat",header=None, delim_whitespace=True)
    temp_ptm = pd.read_table("Data/temp_phantom.dat",header=None, delim_whitespace=True)

    time = np.arange(0, maxstep+1, 100)
    
    U_Pt_all = temp_all[temp_all.keys()[0]]
    L_Pt_all = temp_all[temp_all.keys()[1]]

    U_Pt_itf = temp_itf[temp_itf.keys()[0]]
    L_Pt_itf = temp_itf[temp_itf.keys()[1]]

    U_Pt_ptm = temp_ptm[temp_ptm.keys()[0]]
    L_Pt_ptm = temp_ptm[temp_ptm.keys()[1]]

    plt.yticks(np.arange(0, 1000, 50))

    plt.xlabel("Time[fs]")
    plt.ylabel("Temperature[K]")
    plt.xlim(0, maxstep)
    plt.ylim(0, 300)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
    
    plt.grid()

    plt.plot(time, U_Pt_all, label="Upper Pt(ALL)", color="red")
    plt.plot(time, L_Pt_all, label="Lower Pt(ALL)", color="blue")

    plt.plot(time, U_Pt_itf, label="Upper Pt(INTERFACE)", color="red", ls='--')
    plt.plot(time, L_Pt_itf, label="Lower Pt(INTERFACE)", color="blue", ls='--')

    plt.plot(time, U_Pt_ptm, label="Upper Pt(PHANTOM)", color="red", ls=':')
    plt.plot(time, L_Pt_ptm, label="Lower Pt(PHANTOM)", color="blue", ls=':')

    plt.legend(loc="upper right")
    plt.show()

main()