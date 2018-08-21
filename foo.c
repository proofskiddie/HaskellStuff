#include <stdio.h>
#include "HsFFI.h"
#include "ffi_test_stub.h"

int main(int argc, char *argv[]) {
   hs_init(&argc, &argv);
   int i;
   for (i = 0; i < 5; ++i)
       printf("%d\n", foo(2500)); 
   hs_exit();
   return 0;
}
