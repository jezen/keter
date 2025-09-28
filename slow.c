#!/usr/bin/env -S tcc -run

#include <stdio.h>
#include <unistd.h>

#define CHUNK 50
#define DELAY 300*1000 // microseconds

int main(int argc, char **argv)
{
    FILE *h, *oh;
    char buf[CHUNK];
    size_t bytesRead;

    if (argc != 3) {
        printf("Slow file copy.\nUsage: %s <SRC> <DST>\n", argv[0]);
        return 1;
    }

    h = fopen(argv[1], "r");
    if (h == NULL) {
        printf("Error: can't open SRC file!\n");
        return 1;
    }

    oh = fopen(argv[2], "w");
    if (oh == NULL) {
        printf("Error: can't open DST file!\n");
        return 1;
    }

    while (!feof(h)) {
        bytesRead = fread(buf, sizeof(char), CHUNK, h);
        usleep(DELAY);
        fwrite(buf, sizeof(char), bytesRead, oh);
        fflush(oh);
    }

    fclose(h);
    fclose(oh);

    return 0;
}
