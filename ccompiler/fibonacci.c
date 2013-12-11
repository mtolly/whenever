#include <stdbool.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>

long count1 = 1;
long count2 = 1;
long count3 = 1;
long count4 = 1;
long count5 = 1;
long count6 = 1;
long count7 = 1;
long count8 = 1;
long count9 = 1;

/*
int *count(int line)
{
  switch (line)
  {
    case 1: return &count1;
    case 2: return &count2;
    case 3: return &count3;
    case 4: return &count4;
    case 5: return &count5;
    case 6: return &count6;
    case 7: return &count7;
    case 8: return &count8;
    case 9: return &count9;
  }
}
*/

bool done()
{
  return !count1 && !count2 && !count3 && !count4
    && !count5 && !count6 && !count7 && !count8 && !count9;
}

void zero_all()
{
  if (count1 < 0) count1 = 0;
  if (count2 < 0) count2 = 0;
  if (count3 < 0) count3 = 0;
  if (count4 < 0) count4 = 0;
  if (count5 < 0) count5 = 0;
  if (count6 < 0) count6 = 0;
  if (count7 < 0) count7 = 0;
  if (count8 < 0) count8 = 0;
  if (count9 < 0) count9 = 0;
}

int main(int argc, char **argv)
{
  srand(time(NULL));
  while (true)
  {
    zero_all();
    bool again = false;
    if (done()) return 0;
    switch (rand() % 9)
    {
      case 0: // line 1
        if (!count1) break;
        if (count1) again = true;
        if (count3 || (count1 <= count2) || (count7 > 99)) break;
        count2 += count1;
        count3++;
        count7++;
        if (!again) count1--;
        break;

      case 1: // line 2
        if (!count2) break;
        if (count2) again = true;
        if (count3 || (count2 <= count1) || (count7 > 99)) break;
        count1 += count2;
        count3++;
        count7++;
        if (!again) count2--;
        break;

      case 2: // line 3
        if (!count3) break;
        if (count5) break;
        printf("%ld\n", count1 + count2);
        fflush(stdout);
        count3--;
        break;

      case 3: // line 4
        if (!count4) break;
        if (count5) break;
        printf("1\n");
        fflush(stdout);
        count4--;
        break;

      case 4: // line 5
        if (!count5) break;
        count4++;
        count3--;
        count7++;
        count5--;
        break;

      case 5: // line 6
        if (!count6) break;
        if (count4) break;
        count3++;
        count6--;
        break;

      case 6: // line 7
        // if (!count7) break;
        // count7++;
        // count7--;
        break;

      case 7: // line 8
        if (!count8) break;
        if (count7 < 100) break;
        count1 = 0;
        count2 = 0;
        count7 -= 100;
        count3--;
        count8--;
        break;

      case 8: // line 9
        if (!count9) break;
        if (count3 || count6) break;
        count1++;
        count3++;
        count9--;
        break;
    }
  }
}
