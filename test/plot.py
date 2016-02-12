#!/usr/bin/python

import numpy as np
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
import matplotlib.pyplot as plt
import sys

def main():
    if len(sys.argv) != 3:
        print "Pass points file and facets file."
        exit(1)
    
    points = np.loadtxt(sys.argv[1])
    facets = np.loadtxt(sys.argv[2], dtype=int)

    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.plot(points[:, 0], points[:, 1], points[:, 2], '.', color='red')

    print facets.shape

    for i in range(0, facets.shape[0]):
        x = np.zeros(facets.shape[1])
        y = np.zeros(facets.shape[1])
        z = np.zeros(facets.shape[1])
        for j in range(0, facets.shape[1]):
            x[j] = points[facets[i, j], 0]
            y[j] = points[facets[i, j], 1]
            z[j] = points[facets[i, j], 2]
        simplex = Poly3DCollection([zip(x,y,z)])
        ax.add_collection3d(simplex)
    plt.show()

if __name__ == "__main__":
    main()
