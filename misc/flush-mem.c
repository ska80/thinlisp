/**
 *
 * Module:      flush-mem.c
 *
 * Copyright (c) 1999 The Thinlisp Group
 * All Rights Reserved.
 *
 * This file is part of ThinLisp.
 *
 * ThinLisp is open source; you can redistribute it and/or modify it
 * under the terms of the ThinLisp License as published by the ThinLisp
 * Group; either version 1 or (at your option) any later version.
 *
 * ThinLisp is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * For additional information see <http://www.thinlisp.org/>
 *
 * Author Jim Allard
 *
 * This file contains an experiment in mallocing big chunks of memory
 * then exiting to see if this can flush cache before it's really
 * needed.
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

void usage(void) {
  printf("Usage: flush-mem <megs>\n");
  exit(-1);
}

#define MEG 1048576
#define MEGS_PER_MALLOC 10


int main(int argc, char **argv) {
  int megs, megs_done, i;
  int stuff[MEG/sizeof(int)];
  char *new_mem;

  if (argc != 2)
    usage();
  errno = 0;
  megs = strtol(argv[1], NULL, 10);
  if (errno != 0) {
    perror("Error reading megs argument");
    usage();
  }

  for (i=0; i < (MEG/sizeof(int)); ++i) {
    stuff[i] = i + ((i & 0xef) << 16);
  }

  printf("Mallocing in %d Meg increments: ", MEGS_PER_MALLOC);
  fflush(stdout);
  for (megs_done = 0; megs_done<megs; megs_done += MEGS_PER_MALLOC) {
    new_mem = (char *)malloc(MEG * MEGS_PER_MALLOC);
    if (new_mem == NULL) {
      perror("Error mallocing memory");
      exit(-2);
    } else {
      printf(" %d", megs_done + MEGS_PER_MALLOC);
      fflush(stdout);
      for (i=0; i<MEGS_PER_MALLOC; ++i) {
	memcpy((void *)new_mem, (void *)stuff, MEG);
	new_mem += MEG;
      }
      printf("-");
      fflush(stdout);
    }
  }
  printf("\nDone, exiting.\n");
  return 0;
}
