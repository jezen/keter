#!/usr/bin/env -S tcc -run

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define CHUNK 50

int main(int argc, char **argv)
{
    FILE *hSrc, *hDst;
    char buf[CHUNK];
    size_t bytesRead;

    if (argc != 4) {
        printf("Slow file copy.\nUsage: %s <DELAY (milliseconds)> <SRC> <DST>\n", argv[0]);
        return 1;
    }

    int delay = atoi(argv[1]) * 1000;
    char *src = argv[2];
    char *dst = argv[3];

    if (!(hSrc = fopen(src, "r"))) {
        printf("Error: can't open SRC file!\n");
        return 1;
    }

    if (!(hDst = fopen(dst, "w"))) {
        printf("Error: can't open DST file!\n");
        return 1;
    }

    while (!feof(hSrc)) {
        bytesRead = fread(buf, sizeof(char), CHUNK, hSrc);
        usleep(delay);
        fwrite(buf, sizeof(char), bytesRead, hDst);
        fflush(hDst);
    }

    fclose(hSrc);
    fclose(hDst);

    return 0;
}
