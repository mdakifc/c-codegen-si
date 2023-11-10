int N = 10000000;
int prime[10000000];

int main() {
    for (int p = 2; p < N; p++) prime[p] = 1;
    for (int p = 2; p < N; p++) {
        if (prime[p])
        for (int x = p*p; x < N; x += p) {
            prime[x] = 0;
        }
    }
}
