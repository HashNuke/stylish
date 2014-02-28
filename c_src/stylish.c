#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char byte;

int foo(int x) {
  return x+1;
}

int bar(int y) {
  return y*2;
}


int main() {
  int fn, arg, res;
  byte buf[100];

  while (read_cmd(buf) > 0) {
    fn = buf[0];
    arg = buf[1];
    
    if (fn == 1) {
      res = foo(arg);
    }
    else if (fn == 2) {
      res = bar(arg);
    }

    buf[0] = res;
    write_cmd(buf, 1);
  }
}