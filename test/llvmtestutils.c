#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

FILE *replace_stdin(const char *path) {
  FILE *fp = stdin;
  stdin = fopen(path, "r");
  return fp;
}

int replace_stdout(const char *path) {
  fflush(stdout);

  int original_stdout = fileno(stdout);
  int stdout_copy = dup(original_stdout);

  int fd = open(path, O_WRONLY);
  dup2(fd, original_stdout);
  close(fd);

  return stdout_copy;
}

void restore_stdin(FILE *fp) {
  fclose(stdin);
  stdin = fp;
}

void restore_stdout(int stdout_copy) {
  fflush(stdout);

  int original_stdout = fileno(stdout);
  dup2(stdout_copy, original_stdout);
  close(stdout_copy);
}
