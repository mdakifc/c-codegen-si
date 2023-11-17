#include "global.h"
void f0(int p0,
        int p1,
        int p2,
        int p3,
        int p4,
        int p5,
        int p6,
        int p7,
        int p8,
        int p9)
{
    struct timeval start57;
    struct timeval end58;
    gettimeofday(&start57, 0);
    int sI0 = 0;
    int sI1 = 0;
    int * * AI0 = 0;
    int * AI1 = 0;
    int * * AI2 = 0;
    int * * AI3 = 0;
    int i0 = 0;
    int i1 = 0;
    int i2 = 0;
    int i3 = 0;
    int i4 = 0;
    int i5 = 0;
    int i6 = 0;
    AI0 = malloc(sizeof(void *) * p0);
    for (i0 = 0; i0 < p0; i0++)
    {
        AI0[i0] = malloc(sizeof(int) * p1);
        for (i1 = 0; i1 < p1; i1++)
            AI0[i0][i1] = readRand();
    }
    AI1 = malloc(sizeof(int) * p2);
    for (i0 = 0; i0 < p2; i0++)
        AI1[i0] = readRand();
    AI2 = malloc(sizeof(void *) * p3);
    for (i0 = 0; i0 < p3; i0++)
    {
        AI2[i0] = malloc(sizeof(int) * p4);
        for (i1 = 0; i1 < p4; i1++)
            AI2[i0][i1] = readRand();
    }
    AI3 = malloc(sizeof(void *) * p5);
    for (i0 = 0; i0 < p5; i0++)
    {
        AI3[i0] = malloc(sizeof(int) * p6);
        for (i1 = 0; i1 < p6; i1++)
            AI3[i0][i1] = readRand();
    }
    float sF0 = 0;
    float sF1 = 0;
    float * * AF0 = 0;
    float * AF1 = 0;
    int i7 = 0;
    int i8 = 0;
    int i9 = 0;
    AF0 = malloc(sizeof(void *) * p7);
    for (i0 = 0; i0 < p7; i0++)
    {
        AF0[i0] = malloc(sizeof(float) * p8);
        for (i1 = 0; i1 < p8; i1++)
            AF0[i0][i1] = (float) readRand() / (float) 2147483647 * 1000000;
    }
    AF1 = malloc(sizeof(float) * p9);
    for (i0 = 0; i0 < p9; i0++)
        AF1[i0] = (float) readRand() / (float) 2147483647 * 1000000;
    gettimeofday(&end58, 0);
    printf("Execution Time of declaration and initialization: %lf\n",
           (end58.tv_sec * 1000000.0 + end58.tv_usec - (start57.tv_sec * 1000000.0 + start57.tv_usec)) / 1000000.0);
    struct timeval start38;
    struct timeval end39;
    gettimeofday(&start38, 0);
    {
        sI0 = AI3[(i2 * 5 % p5 + p5) % p5][((i2 + 2) % p6 + p6) % p6];
        for (i2 = 3; i2 < 9294; i2 += 5)
        
#ifdef SI_COUNT
#pragma clang loop scalar_interpolation_count(SI_COUNT)
#endif

            for (i6 = 10; i6 < 5862; i6 += 1)
            {
                AF1[((i6 + 5) % p9 + p9) % p9] = AF1[((i2 + 4) % p9 + p9) % p9];
                sF1 = AF1[(i2 * 1 % p9 + p9) % p9] + AF1[(i2 * 5 % p9 + p9) % p9];
                sI1 = sI0;
                AI0[((i6 + 5) % p0 + p0) % p0][((i6 + 5) % p1 + p1) % p1] = sI0 - (sI1 & sI1 + (AI1[((i6 - 5) % p2 + p2) % p2] + AI2[((i2 - 0) % p3 + p3) % p3][(i6 * 2 % p4 + p4) % p4]));
            }
        AF0[((i2 - 3) % p7 + p7) % p7][((i2 + 0) % p8 + p8) % p8] = sF1;
    }
    gettimeofday(&end39, 0);
    printf("Execution Time of the loop: %lf\n",
           (end39.tv_sec * 1000000.0 + end39.tv_usec - (start38.tv_sec * 1000000.0 + start38.tv_usec)) / 1000000.0);
    struct timeval start43;
    struct timeval end44;
    gettimeofday(&start43, 0);

#ifdef SI_COUNT
#pragma clang loop scalar_interpolation_count(SI_COUNT)
#endif

    for (i8 = 8; i8 < 6481; i8 += 4)
    {
        AF0[((i8 + 2) % p7 + p7) % p7][(i8 * 3 % p8 + p8) % p8] = sF0 * (sF1 + (sF0 + AF0[(i8 * 2 % p7 + p7) % p7][((i8 + 1) % p8 + p8) % p8])) - sF1;
        AF0[(i8 * 2 % p7 + p7) % p7][((i8 - 5) % p8 + p8) % p8] = sF1 - (sF1 + (AF0[((i8 - 2) % p7 + p7) % p7][(i8 * 2 % p8 + p8) % p8] - (AF1[(i8 * 3 % p9 + p9) % p9] + AF0[(i8 * 5 % p7 + p7) % p7][((i8 + 3) % p8 + p8) % p8])));
        AI1[((i8 + 5) % p2 + p2) % p2] = sI0;
    }
    gettimeofday(&end44, 0);
    printf("Execution Time of the loop: %lf\n",
           (end44.tv_sec * 1000000.0 + end44.tv_usec - (start43.tv_sec * 1000000.0 + start43.tv_usec)) / 1000000.0);
    struct timeval start48;
    struct timeval end49;
    gettimeofday(&start48, 0);
    {
        AI2[(i2 * 4 % p3 + p3) % p3][((i2 - 3) % p4 + p4) % p4] = ((AI1[((i2 - 4) % p2 + p2) % p2] ^ sI0) + AI1[(i2 * 4 % p2 + p2) % p2] ^ AI2[((i2 - 5) % p3 + p3) % p3][((i2 + 1) % p4 + p4) % p4]) & AI2[(i2 * 2 % p3 + p3) % p3][((i2 - 0) % p4 + p4) % p4];
        for (i2 = 2; i2 < 1229; i2 += 4)
        
#ifdef SI_COUNT
#pragma clang loop scalar_interpolation_count(SI_COUNT)
#endif

            for (i7 = 4; i7 < 4868; i7 += 5)
            {
                AI2[((i7 - 1) % p3 + p3) % p3][((i2 - 4) % p4 + p4) % p4] = AI0[((i2 - 2) % p0 + p0) % p0][((i7 - 3) % p1 + p1) % p1];
                AF1[((i7 - 2) % p9 + p9) % p9] = sF1 - AF1[((i2 - 1) % p9 + p9) % p9] + sF1;
                sF1 = sF1 + (AF0[((i7 - 5) % p7 + p7) % p7][((i2 + 2) % p8 + p8) % p8] * AF1[(i7 * 2 % p9 + p9) % p9] + AF1[(i2 * 2 % p9 + p9) % p9]);
                sI0 = AI3[(i2 * 5 % p5 + p5) % p5][(i2 * 2 % p6 + p6) % p6];
            }
        sF0 = sF0 + sF0 - sF1 - sF0;
    }
    gettimeofday(&end49, 0);
    printf("Execution Time of the loop: %lf\n",
           (end49.tv_sec * 1000000.0 + end49.tv_usec - (start48.tv_sec * 1000000.0 + start48.tv_usec)) / 1000000.0);
    struct timeval start53;
    struct timeval end54;
    gettimeofday(&start53, 0);
    {
        AF1[(i3 * 2 % p9 + p9) % p9] = AF0[(i3 * 5 % p7 + p7) % p7][((i3 - 2) % p8 + p8) % p8];
        AI3[(i3 * 1 % p5 + p5) % p5][((i3 - 4) % p6 + p6) % p6] = sI0;
        for (i3 = 10; i3 < 5950; i3 += 5)
        
#ifdef SI_COUNT
#pragma clang loop scalar_interpolation_count(SI_COUNT)
#endif

            for (i0 = 1; i0 < 665; i0 += 4)
            {
                AI2[((i0 + 4) % p3 + p3) % p3][((i3 + 0) % p4 + p4) % p4] = (AI3[(i3 * 2 % p5 + p5) % p5][((i0 + 2) % p6 + p6) % p6] * sI0 ^ sI0 ^ sI0) & AI0[(i0 * 1 % p0 + p0) % p0][(i0 * 2 % p1 + p1) % p1] - sI1 + AI1[(i0 * 4 % p2 + p2) % p2] + AI2[(i0 * 2 % p3 + p3) % p3][((i3 + 0) % p4 + p4) % p4];
                sF0 = AF0[(i3 * 3 % p7 + p7) % p7][((i0 - 5) % p8 + p8) % p8];
                sI1 = AI3[(i3 * 4 % p5 + p5) % p5][(i3 * 5 % p6 + p6) % p6] & sI1;
            }
        sF0 = sF0;
    }
    gettimeofday(&end54, 0);
    printf("Execution Time of the loop: %lf\n",
           (end54.tv_sec * 1000000.0 + end54.tv_usec - (start53.tv_sec * 1000000.0 + start53.tv_usec)) / 1000000.0);
}
int main()
{
    {
        int p0;
        int p1;
        int p2;
        int p3;
        int p4;
        int p5;
        int p6;
        int p7;
        int p8;
        int p9;
        scanf("%d %d %d %d %d %d %d %d %d %d",
              &p0,
              &p1,
              &p2,
              &p3,
              &p4,
              &p5,
              &p6,
              &p7,
              &p8,
              &p9);
        f0(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9);
    }
}
