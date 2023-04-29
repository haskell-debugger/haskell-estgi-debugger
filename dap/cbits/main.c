#include <stdio.h>

int factorial (int n);

int main (int argv, char **argc) {
  printf("factorial(5) is equal to %i\n", factorial(5));
}

int factorial (int n) {
  if (n == 0) {
    return 1;
  }
  return n * factorial (n - 1);
}
