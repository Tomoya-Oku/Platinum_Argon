import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
import pandas as pd
import numpy as np

select_step1 = 10000 # 使用するステップ1
select_step2 = 20000 # 使用するステップ2
select_step3 = 100000 # 使用するステップ3

def main():
    inputdat = pd.read_table("Data/temp_distribution.dat", header=None, delim_whitespace=True)
    stp = inputdat[inputdat.keys()[0]]
    layer = inputdat[inputdat.keys()[1]]
    z = inputdat[inputdat.keys()[2]]
    temp = inputdat[inputdat.keys()[3]]

    plt.xlabel("z[Å]")
    plt.ylabel("Temperature[K]")
    #plt.xlim(0, max(z))
    #plt.ylim(0, 500)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
    
    #plt.yticks(np.arange(0, 301, 50))
    plt.grid()    

    new_z1 = []
    new_temp1 = []
    new_z2 = []
    new_temp2 = []
    new_z3 = []
    new_temp3 = []

    for i in range(len(stp)):
        if (stp[i] == select_step1):
            new_z1.append(z[i])
            new_temp1.append(temp[i])
    
    for i in range(len(stp)):
        if (stp[i] == select_step2):
            new_z2.append(z[i])
            new_temp2.append(temp[i])

    for i in range(len(stp)):
        if (stp[i] == select_step3):
            new_z3.append(z[i])
            new_temp3.append(temp[i])

    plt.scatter(new_z1, new_temp1, label="step " + str(select_step1))
    plt.scatter(new_z2, new_temp2, label="step " + str(select_step2))
    plt.scatter(new_z3, new_temp3, label="step " + str(select_step3))

    plt.legend()
    plt.show()

main()