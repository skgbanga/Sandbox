/* Say you have an array for which the ith element is the price of a given stock on day i. */

/* Design an algorithm to find the last profit. You may complete as many transactions as you like (ie, buy one and sell one share of the stock multiple times). However, you may not engage in multiple transactions at the same time (ie, you must sell the stock before you buy again). */

#include <vector>
#include <iostream>

int maximum_profit(const std::vector<int>& prices)
{
   int last = -1;
   int profit = 0;

   // read vector from behind
   int sz = prices.size();
   for (int i = sz - 1; i >= 0; --i)
   {
      // we got slightly lower than previous last, money to be made!
      if (prices[i] < last)
         profit = profit + (last - prices[i]);

      last = prices[i];
   }
   return profit;
}

int main(int argc, char *argv[])
{
   const int N = 10;
   std::vector<int> prices { 5, 3, 56, 34, 2, 12, 24, 12, 29, 10 };

   auto profit = maximum_profit(prices);

   std::cout << profit << std::endl;
   return 0;
}

