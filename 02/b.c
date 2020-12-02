#include<stdio.h>

int main(void) {
    size_t nCorrect = 0;

    size_t l;
    size_t u;
    char c;
    char s[256];
    while (scanf("%ld-%ld %c: %s\n", &l, &u, &c, s) != EOF) {
        if (s[l - 1] == c ^ s[u - 1] == c) ++nCorrect;
    }
    printf("%ld\n", nCorrect);
}
