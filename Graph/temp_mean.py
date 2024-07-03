import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
import pandas as pd
import numpy as np

def main():
    temp = pd.read_table("Data/temp.dat", header=None, delim_whitespace=True)
    
    time = temp[temp.keys()[0]]
    U_Pt = temp[temp.keys()[1]]
    L_Pt = temp[temp.keys()[2]]
    Ar = temp[temp.keys()[3]]

    U_Pt_sum = 0.0
    L_Pt_sum = 0.0
    Ar_sum = 0.0

    for i in range(len(time)):
        U_Pt_sum += U_Pt[i]
        L_Pt_sum += L_Pt[i]
        Ar_sum += Ar[i]
    
    U_Pt_mean = U_Pt_sum / len(U_Pt)
    L_Pt_mean = L_Pt_sum / len(L_Pt)
    Ar_mean = Ar_sum / len(Ar)

    print("Upper Pt mean temp: " + str(U_Pt_mean))
    print("Lower Pt mean temp: " + str(L_Pt_mean))
    print("Ar mean temp: " + str(Ar_mean))


    plt.yticks(np.arange(0, 1000, 50))

    plt.xlabel("Time[fs]")
    plt.ylabel("Temperature[K]")
    # plt.xlim(0, maxstep)
    # plt.ylim(0, 300)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
    
    plt.grid()

    plt.plot(time, U_Pt, label="Upper Pt", color="red")
    plt.plot(time, L_Pt, label="Lower Pt", color="blue")
    plt.plot(time, Ar, label="Ar", color="green")

    plt.legend(loc="best")
    plt.show()

main()