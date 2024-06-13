import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter

N = 50

def Normal_distribution():
    # 標準正規分布のパラメータ
    mu = 0
    sigma = 1

    # 標準正規分布に従うデータの生成
    x_N = np.linspace(mu - 4*sigma, mu + 4*sigma, 1000)
    y_N = (1/(np.sqrt(2*np.pi)*sigma)) * np.exp(-0.5*((x_N-mu)/sigma)**2)

    return x_N, y_N

def main():
    input_dat = pd.read_table("Normal_Distribution.dat", header=None, delim_whitespace=True)

    #datデータの読み込み
    data = input_dat[input_dat.keys()[0]]

    #確率分布の近似計算
    x_data = np.linspace(min(data), max(data), N+1)
    dx = (max(data) - min(data)) / N

    data_length = len(x_data)

    y_data = []
    for i in range(len(x_data)):
        count = 0
        for j in range(len(data)):
            # x_dataの前後dx/2にある速度データvの個数を数える
            if (x_data[i] - (dx/2) <= data[j]) and (data[j] < x_data[i] + (dx/2)):
                count += 1
        print("Progress: " + "{:.1f}".format(i / data_length * 100) + "%")
        y_data.append(count / len(data))

    x_N, y_N = Normal_distribution()

    #グラフの設定，表示
    plt.xlabel("x")
    plt.ylabel("Probability")

    #plt.xlim(0, xlim)
    #plt.ylim(0, ylim)

    plt.grid(linestyle="dotted")

    plt.plot(x_data, y_data, label="Sample", color="black")
    plt.plot(x_N, y_N, label="Normal Distribution", color="red")
    plt.legend()
    plt.show()

main()