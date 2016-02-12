#pragma once

#include <stdbool.h>

int qhull_wrapper(int d, int m, double *indata, char *cmd, int *n, int **outdata, char **errmsg);
void cleanup();
