#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <libqhull/libqhull.h>
#include <libqhull/qset.h>
#include <libqhull/poly.h>
#include <pthread.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "qhull_wrapper.h"

static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
static char *outbuf = NULL;
static size_t outbuf_size = 0;
static char *errbuf = NULL;
static size_t errbuf_size = 0;
static FILE *outfile = NULL;
static FILE *errfile = NULL;
static bool initialized = 0;

int qhull_wrapper(int d, int m, double *indata, char *cmd, int *n, int **outdata, char **errmsg)
{
  int lock_ret;
  #ifdef NDEBUG
  lock_ret = pthread_mutex_lock(&lock);
  #else
  lock_ret = pthread_mutex_trylock(&lock);
  if (lock_ret == EBUSY)
  {
    printf("qhull_wrapper.c: Lock busy. Waiting.\n");
    lock_ret = pthread_mutex_lock(&lock);
  }
  #endif
  if (lock_ret)
    return lock_ret;
  #ifndef NDEBUG
  printf("qhull_wrapper.c: Lock obtained.\n");
  #endif

  if (!initialized)
  {
    #ifdef NDEBUG
    outfile = open_memstream(&outbuf, &outbuf_size);
    errfile = open_memstream(&errbuf, &errbuf_size);
    #else
    printf("qhull_wrapper.c: Initializing.\n");
    printf("qhull_wrapper.c: Note: In debug mode, so error messages will not be relayed to Haskell, but instead go straight to stdout and stderr.\n");
    outfile = stdout;
    errfile = stderr;
    #endif
    initialized = true;
  }

  int code = qh_new_qhull(d, m, indata, 0, cmd, outfile, errfile);

  if (code)
  {
    #ifdef NDEBUG
    fflush(errfile); // Makes errbuf_size valid.
    //fflush(outfile); // Makes outbuf_size valid.
    //*outmsg = (char *)malloc(outbuf_size+1); // We're not saving the out message afterall.
    *errmsg = (char *)malloc(errbuf_size+1);
    //memcpy(*outmsg, outbuf, outbuf_size);
    //(*outmsg)[outbuf_size] = '\0';
    memcpy(*errmsg, errbuf, errbuf_size);
    (*errmsg)[errbuf_size] = '\0';
    rewind(outfile);
    rewind(errfile);
    #else
    printf("qhull_wrapper.c: qh_new_qhull failed with code %d.\n", code);
    #endif

    cleanup();
    return code;
  }

  *n = qh num_facets;
  *outdata = (int *)malloc((d+1)*(*n)*sizeof(int));  // We're assuming the "Qt" option so that facets are triangulated.
  //*k = (int *)malloc((*n)*sizeof(int));  // Not needed when triangulating.
  facetT *facet;
  int i = 0;
  FORALLfacets
  {
    if (!(facet->upperdelaunay)){
    //printf("toporient=%d        orientCLOCK=%d\n", facet->toporient, qh_ORIENTclock);
    vertexT *vertex, **vertexp;
    int j = 0;
    if (!(facet->toporient) != (!qh_ORIENTclock)) // Only do if toporiented OR clockwise oriented.
    {
      FOREACHvertex_ (facet->vertices)
      {
        (*outdata)[(d+1)*i + j] = qh_pointid(vertex->point);
	      j++;
      }
    }
    else
    {
      FOREACHvertexreverse12_ (facet->vertices)
      {
        (*outdata)[(d+1)*i + j] = qh_pointid(vertex->point);
      	j++;
      }
    }

    /*
    (*k)[i] = j;
    while (j <= d)
    {
      (*outdata)[(d+1)*i + j] = -1;
      j++;
    }
    */
    #ifndef NDEBUG
    if (j < d)
      printf("qhull_wrapper.c: Facet %d only has %d vertices. Expected %d.\n", i, j, d);
    #endif

    i++;
    }
    *n = i;
  }
  #ifndef NDEBUG
  if (i < *n)
    printf("qhull_wrapper.c: Only saw %d facets. Expected %d.\n", i, *n);
  #endif

  cleanup();
  return 0;
}

void cleanup()
{
  qh_freeqhull(!qh_ALL);
  int freecurlong, freetotlong;
  qh_memfreeshort(&freecurlong, &freetotlong);
  #ifndef NDEBUG
  if (freecurlong || freetotlong)
    printf("qhull_wrapper.c: Failed to free %d bytes of memory.\n", freetotlong);
  #endif

  pthread_mutex_unlock(&lock);
}
