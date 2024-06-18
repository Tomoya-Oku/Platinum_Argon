import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
import pandas as pd
import numpy as np

maxstep = 20000
partition = 20

def main():
    step = int(maxstep/100)

    inputdat = pd.read_table("Data/temp_layer.dat", header=None, delim_whitespace=True)
    temp_itf = pd.read_table("Data/temp_interface.dat",header=None, delim_whitespace=True)
    temp_ptm = pd.read_table("Data/temp_phantom.dat",header=None, delim_whitespace=True)

    data = inputdat[inputdat.keys()[0]]

    U_Pt_itf = temp_itf[temp_itf.keys()[0]]
    L_Pt_itf = temp_itf[temp_itf.keys()[1]]
    U_Pt_ptm = temp_ptm[temp_ptm.keys()[0]]
    L_Pt_ptm = temp_ptm[temp_ptm.keys()[1]]

    time = np.arange(0, maxstep+1, 100)
    layer = [[0] * (step+1) for i in range(partition+1)] #layer[20][200]

    # layer[0][0] = data[0]
    # layer[0][1] = data[20]
    # layer[0][2] = data[40]
    # ...
    # layer[0][199] = data[4000]

    # layer[1][0] = data[1]
    # layer[1][1] = data[21]
    # layer[1][2] = data[41]
    # ...
    # layer[1][199] = data[4001]

    # ...

    # layer[19][0] = data[19]
    # layer[19][1] = data[39]
    # layer[19][2] = data[59]
    # ...
    # layer[19][199] = data[4019]

    for i in range(partition):
        for j in range(step+1):
            layer[i][j] = data[i+20*j]

    plt.xlabel("Time[fs]")
    plt.ylabel("Temperature[K]")
    plt.xlim(0, maxstep)
    plt.ylim(0, 500)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
    
    plt.yticks(np.arange(0, 301, 50))
    plt.grid()

    plt.plot(time, U_Pt_ptm, label="Upper Pt(PHANTOM)", color="red", ls=':')
    plt.plot(time, U_Pt_itf, label="Upper Pt(INTERFACE)", color="red")

    for i in range(partition):
        label = "Layer " + str(i+1)
        if (i == 0): plt.plot(time, layer[i], label=label, color="orangered")
        if (i == 1): plt.plot(time, layer[i], label=label, color="orangered", ls=":")
        if (i == 18): plt.plot(time, layer[i], label=label, color="green", ls=":")
        if (i == 19): plt.plot(time, layer[i], label=label, color="green")

    
    plt.plot(time, L_Pt_itf, label="Lower Pt(INTERFACE)", color="blue")
    plt.plot(time, L_Pt_ptm, label="Lower Pt(PHANTOM)", color="blue", ls=':')

    labels = ["Upper Pt(PHANTOM)", "Upper Pt(INTERFACE)", "Layer 1", "Layer 2", "Layer 19", "Layer 20", "Lower Pt(INTERFACE)", "Lower Pt(PHANTOM)"]
    plt.legend(labels, loc='upper left', bbox_to_anchor=(1, 1))
    plt.savefig("temp_layer.png", format="png", dpi=300, bbox_inches='tight')
    plt.show()

main()