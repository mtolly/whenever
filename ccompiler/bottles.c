#include <stdbool.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>

int count1 = 1;
int count2 = 1;
int count3 = 1;
int count4 = 1;

bool done()
{
  return !count1 && !count2 && !count3 && !count4;
}

int main(int argc, char **argv)
{
  srand(time(NULL));
  while (true)
  {
    if (done()) return 0;
    switch (rand() % 4)
    {
      case 0: // line 1
        if (!count1) break;
        // defer (4 || N(1)<N(2) || N(2)<N(3))
        if (count4 || (count1 < count2) || (count2 < count3)) break;
        // print(N(1)+" bottles of beer on the wall, "+N(1)+" bottles of beer,")
        printf("%d", count1);
        printf(" bottles of beer on the wall, ");
        printf("%d", count1);
        printf(" bottles of beer,\n");
        count1--;
        break;

      case 1: // line 2
        if (!count2) break;
        // defer (4 || N(1) == N(2))
        if (count4 || (count1 == count2)) break;
        // print("Take one down and pass it around,")
        printf("Take one down and pass it around.\n");
        count2--;
        break;

      case 2: // line 3
        if (!count3) break;
        // defer (4 || N(2) == N(3))
        if (count4 || (count2 == count3)) break;
        // print(N(1)+" bottles of beer on the wall.")
        printf("%d", count1);
        printf(" bottles of beer on the wall.\n");
        count3--;
        break;

      case 3: // line 4
        if (!count4) break;
        // 1#98,2#98,3#98
        count1 += 98;
        count2 += 98;
        count3 += 98;
        count4--;
        break;
    }
  }
}
