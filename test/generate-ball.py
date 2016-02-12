#!/usr/bin/python

import numpy as np

def main():
    n = 100000
    phi = np.random.uniform(0, 2*np.pi, n)
    theta = np.random.uniform(0, np.pi, n)
    r = np.random.uniform(0, 1, n)
    x = r*np.cos(phi)*np.sin(theta)
    y = r*np.sin(phi)*np.sin(theta)
    z = r*np.cos(theta)
    for i in range(0, n):
        print '%f %f %f' %(x[i],y[i],z[i])

if __name__ == "__main__":
    main()
