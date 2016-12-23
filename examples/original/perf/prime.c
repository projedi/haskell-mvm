#include <math.h>
#include <stdio.h>

int leastFactor(int n) {
  if (n == 0) {
      return 0;  
  }
  if (n == 1) { 
      return 1;
  }
  if (n % 2 == 0) {
      return 2;
  }
  if (n % 3 == 0) {
      return 3;
  }
  if (n % 5 == 0) {
      return 5;  
  }
  double m = sqrt(n);
  int i;
  i = 7;
  while (i<=m) {
      if (n % i==0) {
          return i;
      }
      if (n%(i+4)==0) {
          return i+4;
      }
      if (n%(i+6)==0)  {
          return i+6;
      }
      if (n%(i+10)==0) {
          return i+10;
      }
      if (n%(i+12)==0) {
          return i+12;
      }
      if (n%(i+16)==0) {
          return i+16;
      }
      if (n%(i+22)==0) {
          return i+22;
      }
      if (n%(i+24)==0) {
          return i+24;
      }
      i += 30;
  }
  return n;
}

void factorize(int n)  {
    int least;
    least = n;
    printf("%d = 1", n);
    while (n > 1) {
        least = leastFactor(n);
        if (n % least != 0) {
            printf("Bug!\n");
        }
        printf(" * %d", least);
        n = n / least;
    }
    printf("\n");
}

int main() {
  int n = 10000000;
  int count = 1000000;
  // for (n = 10000000; n <= (10000000+count); ++n) {
      factorize(n);
  // }
  return 0;
}
