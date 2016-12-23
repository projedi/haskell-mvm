#include <math.h>
#include <stdio.h>

double func(double d) {
    return d*d - d*d*d + 2 + sin(d);
}

double newton(double xbase) {
  double x = xbase;
  double xp = x - 0.01;
  double y = func(x);
  double yp = func(yp);
  int repeat = 0;
  while ((fabs(y) > 1e-8) && (repeat < 10000))  {
      double temp = x;
      y = func(x);
      x = x - (x - xp) * y / (y - yp);      
      xp = temp;
      yp = y;
      repeat += 1;
  }
  return x;
}

int main() {
  int repeat = 100000;
  double root;
  // for (repeat = 0; repeat <= 100000; ++repeat) {
    root = newton(10 + repeat / 400.0);
  // }
  printf("root=%g\n", root);
  return 0;
}
