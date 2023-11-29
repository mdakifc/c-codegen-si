#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <stdarg.h>
#define MAX(...) \
    ({ \
        int values[] = {__VA_ARGS__}; \
        int num_values = sizeof(values) / sizeof(values[0]); \
        int max_value = values[0]; \
        for (int i = 1; i < num_values; ++i) { \
            if (values[i] > max_value) { \
                max_value = values[i]; \
            } \
        } \
        max_value; \
    })
#define MIN(...) \
    ({ \
        int values[] = {__VA_ARGS__}; \
        int num_values = sizeof(values) / sizeof(values[0]); \
        int min_value = values[0]; \
        for (int i = 1; i < num_values; ++i) { \
            if (values[i] < min_value) { \
                min_value = values[i]; \
            } \
        } \
        min_value; \
    })

static FILE *random_values_file = NULL;
static int N = 450 << 20;
static char buf[450 << 20];
static size_t size = 0;
static int exceeded = 0;

int max(int a, int b) { return a > b ? a : b; }
int min(int a, int b) { return a < b ? a : b; }

int ap(int (*f)(int, int), int n,...) {
    va_list args;
    va_start(args, n);
    
    int res = va_arg(args, int);
    for (int i = 1; i < n; i++) {
        int cur = va_arg(args, int);
        res = f(res, cur);
    }
    
    va_end(args);
    return res;
}


void* bumpAllocate(size_t s) {
  if (s + size >= N) {
    if (!exceeded) printf("Limit Exceeded.\n");
    exceeded = 1;
    return malloc(s);
  }
  void* ret = &buf[size];
  size += s;
  return ret;
}

int readRand() {
  if (random_values_file == NULL) {
    random_values_file = fopen("randomValues.txt", "r");
  }
  int v;
  fscanf(random_values_file, "%d", &v);
  return v;
}
