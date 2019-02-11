#include <stdio.h>

FILE *replace_stdin(const char *path) {
  FILE *fp = stdin;
  stdin = fopen(path, "r");
  return fp;
}

FILE *replace_stdout(const char *path) {
  FILE *fp = stdout;
  stdout = fopen(path, "w");
  return fp;
}

void restore_stdin(FILE *fp) {
  fclose(stdin);
  stdin = fp;
}

void restore_stdout(FILE *fp) {
  fclose(stdout);
  stdout = fp;
}
