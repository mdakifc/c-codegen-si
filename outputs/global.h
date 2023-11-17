#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

static FILE *random_values_file = NULL;
static int N = 450 << 20;
static char buf[450 << 20];
static size_t size = 0;
static int exceeded = 0;

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
