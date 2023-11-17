#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

static FILE *random_values_file = NULL;

int readRand() {
  if (random_values_file == NULL) {
    random_values_file = fopen("randomValues.txt", "r");
  }
  int v;
  fscanf(random_values_file, "%d", &v);
  return v;
}
