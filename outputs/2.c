int i0;
int A0[1000];
int A1[1000];
int s0;
int s1;
int main()
{
    s0 = A0[i0];
    for (int i1; i1 < 1000; i1++)
        for (int i2; i2 < 1000; i2++)
            A0[i2] = A1[i0];
    s0 = A1[i0];
    for (int i1; i1 < 1000; i1++)
        for (int i2; i2 < 1000; i2++)
            s1 = A1[i0];
    for (int i1; i1 < 1000; i1++)
        for (int i2; i2 < 1000; i2++)
            s1 = A0[i2];
    for (int i1; i1 < 1000; i1++)
        A0[i1] = A1[i1] * A0[i1];
    for (int i1; i1 < 1000; i1++)
        A1[i1] = A1[i0];
    s0 = s1;
    for (int i1; i1 < 1000; i1++)
        for (int i2; i2 < 1000; i2++)
            s1 = s0 - A0[i0];
    s0 = s0;
    s1 = s1 * A0[i0];
    A1[i0] = A0[i0];
    for (int i1; i1 < 1000; i1++)
        A1[i1] = A0[i1];
    for (int i1; i1 < 1000; i1++)
        for (int i2; i2 < 1000; i2++)
            A1[i2] = s1;
    for (int i1; i1 < 1000; i1++)
        A0[i0] = A1[i1] - A1[i1];
    A1[i0] = s1;
    for (int i1; i1 < 1000; i1++)
        for (int i2; i2 < 1000; i2++)
            A0[i2] = A1[i0];
    A0[i0] = A0[i0];
    A1[i0] = s1;
}
