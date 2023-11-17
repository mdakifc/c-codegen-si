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
        int p13)
{
    struct timeval start64;
    struct timeval end65;
    gettimeofday(&start64, 0);
    int sI0 = 0;
    int sI1 = 0;
    int sI2 = 0;
    int sI3 = 0;
    int sI4 = 0;
    int * * AI0 = 0;
    int * AI1 = 0;
    int * * AI2 = 0;
    int i0 = 0;
    int i1 = 0;
    int i2 = 0;
    int i3 = 0;
    int i4 = 0;
    AI0 = bumpAllocate(sizeof(void *) * p0);
    for (i0 = 0; i0 < p0; i0++)
    {
        AI0[i0] = bumpAllocate(sizeof(int) * p1);
        for (i1 = 0; i1 < p1; i1++)
            AI0[i0][i1] = rand();
    }
    AI1 = bumpAllocate(sizeof(int) * p2);
    for (i0 = 0; i0 < p2; i0++)
        AI1[i0] = rand();
    AI2 = bumpAllocate(sizeof(void *) * p3);
    for (i0 = 0; i0 < p3; i0++)
    {
        AI2[i0] = bumpAllocate(sizeof(int) * p4);
        for (i1 = 0; i1 < p4; i1++)
            AI2[i0][i1] = rand();
    }
    float sF0 = 0;
    float sF1 = 0;
    float * * * AF0 = 0;
    float * * AF1 = 0;
    float * * * AF2 = 0;
    float * AF3 = 0;
    int i5 = 0;
    int i6 = 0;
    int i7 = 0;
    int i8 = 0;
    int i9 = 0;
    int i10 = 0;
    int i11 = 0;
    int i12 = 0;
    int i13 = 0;
    AF0 = bumpAllocate(sizeof(void *) * p5);
    for (i0 = 0; i0 < p5; i0++)
    {
        AF0[i0] = bumpAllocate(sizeof(void *) * p6);
        for (i1 = 0; i1 < p6; i1++)
        {
            AF0[i0][i1] = bumpAllocate(sizeof(float) * p7);
            for (i2 = 0; i2 < p7; i2++)
                AF0[i0][i1][i2] = (float) rand() / (float) 2147483647 * 1000000;
        }
    }
    AF1 = bumpAllocate(sizeof(void *) * p8);
    for (i0 = 0; i0 < p8; i0++)
    {
        AF1[i0] = bumpAllocate(sizeof(float) * p9);
        for (i1 = 0; i1 < p9; i1++)
            AF1[i0][i1] = (float) rand() / (float) 2147483647 * 1000000;
    }
    AF2 = bumpAllocate(sizeof(void *) * p10);
    for (i0 = 0; i0 < p10; i0++)
    {
        AF2[i0] = bumpAllocate(sizeof(void *) * p11);
        for (i1 = 0; i1 < p11; i1++)
        {
            AF2[i0][i1] = bumpAllocate(sizeof(float) * p12);
            for (i2 = 0; i2 < p12; i2++)
                AF2[i0][i1][i2] = (float) rand() / (float) 2147483647 * 1000000;
        }
    }
    AF3 = bumpAllocate(sizeof(float) * p13);
    for (i0 = 0; i0 < p13; i0++)
        AF3[i0] = (float) rand() / (float) 2147483647 * 1000000;
    gettimeofday(&end65, 0);
    printf("Execution Time of declaration and initialization: %lf\n",
           (end65.tv_sec * 1000000.0 + end65.tv_usec - (start64.tv_sec * 1000000.0 + start64.tv_usec)) / 1000000.0);
    struct timeval start50;
    struct timeval end51;
    gettimeofday(&start50, 0);

#ifdef SI_COUNT
#pragma clang loop scalar_interpolation_count(SI_COUNT)
#endif

    for (i2 = 2; i2 < 2046; i2 += 4)
    {
        AI0[((i2 + 2) % p0 + p0) % p0][((i2 - 2) % p1 + p1) % p1] = sI1;
        AF2[((i2 - 3) % p10 + p10) % p10][((i2 + 4) % p11 + p11) % p11][((i2 - 1) % p12 + p12) % p12] = sF1 - (AF0[((i2 + 0) % p5 + p5) % p5][(i2 * 5 % p6 + p6) % p6][(i2 * 1 % p7 + p7) % p7] - sF1);
    }
    gettimeofday(&end51, 0);
    printf("Execution Time of the loop: %lf\n",
           (end51.tv_sec * 1000000.0 + end51.tv_usec - (start50.tv_sec * 1000000.0 + start50.tv_usec)) / 1000000.0);
    struct timeval start55;
    struct timeval end56;
    gettimeofday(&start55, 0);

#ifdef SI_COUNT
#pragma clang loop scalar_interpolation_count(SI_COUNT)
#endif

    for (i12 = 7; i12 < 2921; i12 += 3)
    {
        sI2 = AI1[(i12 * 1 % p2 + p2) % p2];
    }
    gettimeofday(&end56, 0);
    printf("Execution Time of the loop: %lf\n",
           (end56.tv_sec * 1000000.0 + end56.tv_usec - (start55.tv_sec * 1000000.0 + start55.tv_usec)) / 1000000.0);
    struct timeval start60;
    struct timeval end61;
    gettimeofday(&start60, 0);

#ifdef SI_COUNT
#pragma clang loop scalar_interpolation_count(SI_COUNT)
#endif

    for (i5 = 3; i5 < 9327; i5 += 5)
    {
        sI2 = (AI0[(i5 * 1 % p0 + p0) % p0][((i5 - 3) % p1 + p1) % p1] ^ sI2 + sI2) - AI0[(i5 * 4 % p0 + p0) % p0][((i5 - 4) % p1 + p1) % p1];
        sI1 = sI0;
        AF2[((i5 - 5) % p10 + p10) % p10][((i5 + 3) % p11 + p11) % p11][((i5 + 1) % p12 + p12) % p12] = sF1 * sF1;
        AI1[((i5 + 2) % p2 + p2) % p2] = sI2;
    }
    gettimeofday(&end61, 0);
    printf("Execution Time of the loop: %lf\n",
           (end61.tv_sec * 1000000.0 + end61.tv_usec - (start60.tv_sec * 1000000.0 + start60.tv_usec)) / 1000000.0);
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
        scanf("%d %d %d %d %d %d %d %d %d %d %d %d %d %d",
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
              &p13);
        f0(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13);
    }
}
