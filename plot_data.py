import matplotlib.pyplot as plt

if __name__=='__main__':
    #Read data
    days = []
    y = []

    f = open('GSC_raw_final.dat', 'r')

    for l in f:
        d, c = l.split()
        days.append(float(d))
        y.append(float(c))

    f.close()

    #Plot data
    plt.plot(days, y)
    plt.xlabel('Days')
    plt.ylabel('Counts')
    plt.title('Data plot')
    plt.grid(True)
    plt.savefig("data_plot.png")
    plt.show()
