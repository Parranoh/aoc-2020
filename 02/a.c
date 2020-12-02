#include<stdio.h>

int main(void) {
    size_t nCorrect = 0;

    size_t l;
    size_t u;
    char c;
    char s[256];
    while (scanf("%ld-%ld %c: %s\n", &l, &u, &c, s) != EOF) {
        size_t count = 0;
        char *p = s;
        while (*p) {
            if (*p++ == c) ++count;
        }
        if (l <= count && count <= u) ++nCorrect;
    }
    printf("%ld\n", nCorrect);
}
