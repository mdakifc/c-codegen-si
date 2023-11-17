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
        int p9,
        int p10,
        int p11,
        int p12,
        int p13,
        int p14)
{
    struct timeval start63;
    struct timeval end64;
    gettimeofday(&start63, 0);
    int sI0 = 0;
    int sI1 = 0;
    int sI2 = 0;
    int sI3 = 0;
    int sI4 = 0;
    int * * * AI0 = 0;
    int * AI1 = 0;
    int * AI2 = 0;
    int i0 = 0;
    int i1 = 0;
    int i2 = 0;
    int i3 = 0;
    int i4 = 0;
    AI0 = bumpAllocate(sizeof(void *) * p0);
    for (i0 = 0; i0 < p0; i0++)
    {
        AI0[i0] = bumpAllocate(sizeof(void *) * p1);
        for (i1 = 0; i1 < p1; i1++)
        {
            AI0[i0][i1] = bumpAllocate(sizeof(int) * p2);
            for (i2 = 0; i2 < p2; i2++)
                AI0[i0][i1][i2] = rand();
        }
    }
    AI1 = bumpAllocate(sizeof(int) * p3);
    for (i0 = 0; i0 < p3; i0++)
        AI1[i0] = rand();
    AI2 = bumpAllocate(sizeof(int) * p4);
    for (i0 = 0; i0 < p4; i0++)
        AI2[i0] = rand();
    float sF0 = 0;
    float sF1 = 0;
    float sF2 = 0;
    float sF3 = 0;
    float * AF0 = 0;
    float * * * AF1 = 0;
    float * * * AF2 = 0;
    float * * * AF3 = 0;
    int i5 = 0;
    int i6 = 0;
    int i7 = 0;
    int i8 = 0;
    int i9 = 0;
    int i10 = 0;
    int i11 = 0;
    int i12 = 0;
    int i13 = 0;
    int i14 = 0;
    AF0 = bumpAllocate(sizeof(float) * p5);
    for (i0 = 0; i0 < p5; i0++)
        AF0[i0] = (float) rand() / (float) 2147483647 * 1000000;
    AF1 = bumpAllocate(sizeof(void *) * p6);
    for (i0 = 0; i0 < p6; i0++)
    {
        AF1[i0] = bumpAllocate(sizeof(void *) * p7);
        for (i1 = 0; i1 < p7; i1++)
        {
            AF1[i0][i1] = bumpAllocate(sizeof(float) * p8);
            for (i2 = 0; i2 < p8; i2++)
                AF1[i0][i1][i2] = (float) rand() / (float) 2147483647 * 1000000;
        }
    }
    AF2 = bumpAllocate(sizeof(void *) * p9);
    for (i0 = 0; i0 < p9; i0++)
    {
        AF2[i0] = bumpAllocate(sizeof(void *) * p10);
        for (i1 = 0; i1 < p10; i1++)
        {
            AF2[i0][i1] = bumpAllocate(sizeof(float) * p11);
            for (i2 = 0; i2 < p11; i2++)
                AF2[i0][i1][i2] = (float) rand() / (float) 2147483647 * 1000000;
        }
    }
    AF3 = bumpAllocate(sizeof(void *) * p12);
    for (i0 = 0; i0 < p12; i0++)
    {
        AF3[i0] = bumpAllocate(sizeof(void *) * p13);
        for (i1 = 0; i1 < p13; i1++)
        {
            AF3[i0][i1] = bumpAllocate(sizeof(float) * p14);
            for (i2 = 0; i2 < p14; i2++)
                AF3[i0][i1][i2] = (float) rand() / (float) 2147483647 * 1000000;
        }
    }
    gettimeofday(&end64, 0);
    printf("Execution Time of declaration and initialization: %lf\n",
           (end64.tv_sec * 1000000.0 + end64.tv_usec - (start63.tv_sec * 1000000.0 + start63.tv_usec)) / 1000000.0);
    struct timeval start54;
    struct timeval end55;
    gettimeofday(&start54, 0);
    {
        AF0[((i4 - 2) % p5 + p5) % p5] = AF1[(i4 * 1 % p6 + p6) % p6][(i4 * 5 % p7 + p7) % p7][((i4 - 3) % p8 + p8) % p8] + sF2;
        AI0[(i4 * 4 % p0 + p0) % p0][(i4 * 1 % p1 + p1) % p1][(i4 * 4 % p2 + p2) % p2] = sI2;
        for (i4 = 3; i4 < 1127; i4 += 3)
        
#ifdef SI_COUNT
#pragma clang loop scalar_interpolation_count(SI_COUNT)
#endif

            for (i9 = 1; i9 < 4377; i9 += 2)
            {
                sI0 = sI4 | AI2[(i4 * 1 % p4 + p4) % p4];
                AF3[(i9 * 5 % p12 + p12) % p12][((i9 - 4) % p13 + p13) % p13][((i9 - 3) % p14 + p14) % p14] = sF0;
                AI0[(i4 * 4 % p0 + p0) % p0][((i9 + 3) % p1 + p1) % p1][((i9 - 5) % p2 + p2) % p2] = sI0;
            }
        AI2[((i4 + 3) % p4 + p4) % p4] = sI0 & AI0[((i4 + 1) % p0 + p0) % p0][((i4 + 4) % p1 + p1) % p1][(i4 * 5 % p2 + p2) % p2];
        sF0 = sF3;
    }
    gettimeofday(&end55, 0);
    printf("Execution Time of the loop: %lf\n",
           (end55.tv_sec * 1000000.0 + end55.tv_usec - (start54.tv_sec * 1000000.0 + start54.tv_usec)) / 1000000.0);
    struct timeval start59;
    struct timeval end60;
    gettimeofday(&start59, 0);
    {
        sI2 = sI3;
        AI0[((i2 + 2) % p0 + p0) % p0][(i2 * 5 % p1 + p1) % p1][(i2 * 2 % p2 + p2) % p2] = AI2[((i2 + 3) % p4 + p4) % p4];
        for (i2 = 9; i2 < 2477; i2 += 2)
        {
            sF2 = AF0[((i2 + 4) % p5 + p5) % p5];
            AI0[((i11 - 5) % p0 + p0) % p0][((i2 + 2) % p1 + p1) % p1][((i11 + 2) % p2 + p2) % p2] = sI0;
            sF2 = sF3 + AF2[(i2 * 3 % p9 + p9) % p9][(i11 * 2 % p10 + p10) % p10][((i11 + 4) % p11 + p11) % p11] - sF0;
            AI1[((i11 + 5) % p3 + p3) % p3] = sI1;
            sI0 = sI4;
            for (i11 = 2; i11 < 7861; i11 += 4)
            
#ifdef SI_COUNT
#pragma clang loop scalar_interpolation_count(SI_COUNT)
#endif

                for (i14 = 6; i14 < 6563; i14 += 5)
                {
                    AI0[((i14 - 5) % p0 + p0) % p0][((i14 - 5) % p1 + p1) % p1][((i11 + 2) % p2 + p2) % p2] = sI4 * sI3;
                    sF2 = sF3;
                    sI4 = sI2;
                    AF1[((i11 - 0) % p6 + p6) % p6][((i11 + 2) % p7 + p7) % p7][((i14 - 2) % p8 + p8) % p8] = AF3[((i2 - 5) % p12 + p12) % p12][((i2 - 4) % p13 + p13) % p13][((i2 + 0) % p14 + p14) % p14];
                }
            sF3 = sF0 - AF0[((i11 - 1) % p5 + p5) % p5];
            AI0[((i11 + 1) % p0 + p0) % p0][((i2 + 1) % p1 + p1) % p1][(i11 * 5 % p2 + p2) % p2] = sI0 ^ sI0;
            AI2[(i11 * 4 % p4 + p4) % p4] = sI3;
        }
        sI4 = AI1[((i2 + 4) % p3 + p3) % p3];
        AF3[((i2 + 0) % p12 + p12) % p12][((i2 - 0) % p13 + p13) % p13][((i2 + 1) % p14 + p14) % p14] = sF1 + sF2;
        AI2[((i2 - 1) % p4 + p4) % p4] = sI3;
        sF0 = AF2[((i2 - 3) % p9 + p9) % p9][(i2 * 4 % p10 + p10) % p10][((i2 - 0) % p11 + p11) % p11] - AF1[((i2 - 5) % p6 + p6) % p6][((i2 + 1) % p7 + p7) % p7][((i2 - 3) % p8 + p8) % p8] + AF1[((i2 + 5) % p6 + p6) % p6][((i2 - 0) % p7 + p7) % p7][((i2 - 2) % p8 + p8) % p8] * (AF3[((i2 - 4) % p12 + p12) % p12][((i2 + 4) % p13 + p13) % p13][(i2 * 1 % p14 + p14) % p14] + AF1[((i2 + 5) % p6 + p6) % p6][((i2 - 5) % p7 + p7) % p7][((i2 + 0) % p8 + p8) % p8] - sF3);
        sI3 = AI1[((i2 + 0) % p3 + p3) % p3];
    }
    gettimeofday(&end60, 0);
    printf("Execution Time of the loop: %lf\n",
           (end60.tv_sec * 1000000.0 + end60.tv_usec - (start59.tv_sec * 1000000.0 + start59.tv_usec)) / 1000000.0);
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
        int p10;
        int p11;
        int p12;
        int p13;
        int p14;
        scanf("%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
              &p0,
              &p1,
              &p2,
              &p3,
              &p4,
              &p5,
              &p6,
              &p7,
              &p8,
              &p9,
              &p10,
              &p11,
              &p12,
              &p13,
              &p14);
        f0(p0,
           p1,
           p2,
           p3,
           p4,
           p5,
           p6,
           p7,
           p8,
           p9,
           p10,
           p11,
           p12,
           p13,
           p14);
    }
}
