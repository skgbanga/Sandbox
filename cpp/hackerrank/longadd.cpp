// add two large strings (they can be +/-)
#include <iostream>
#include <string>
#include <cassert>

enum class Sign : uint8_t
{
   lhs_neg,
   rhs_neg,
   both_neg,
   both_pos
};

// both lhs and rhs size() > 0
Sign getSign(const std::string& lhs, const std::string& rhs)
{
   static const char sign = '-';

   if (lhs[0] == sign && rhs[0] == sign)
      return Sign::both_neg;
   if (lhs[0] == sign)
      return Sign::lhs_neg;
   if (rhs[0] == sign)
      return Sign::rhs_neg;
   return Sign::both_pos;
}

// both the elements have size > 0
std::string add(std::string lhs, std::string rhs)
{
   std::string result = "";

   bool carry = false;
   int i = lhs.size() - 1; int j = rhs.size() - 1;
   while (i >= 0 && j >= 0)
   {
      int x = lhs[i] - 48;
      int y = rhs[j] - 48;

      if (carry)
      {
         carry = false;
         ++x;
      }

      int sum = x + y;
      if (sum >= 10)
      {
         carry = true;
         sum -= 10;
      }
      result.insert(result.begin(), sum + 48);

      --i;
      --j;
   }

   auto lamda = [&result, &carry](int k, std::string str)
   {
      while (k >= 0)
      {
         int x = str[k] - 48;

         if (carry)
         {
            carry = false;
            ++x;
         }

         if (x == 10) // cannot be more than 10
         {
            x -= 10;
            carry = true;
         }
         result.insert(result.begin(), x + 48);
         --k;
      }
   };

   lamda(i, lhs);
   lamda(j, rhs);

   // if we are still not done with carry, we need to add one to the beginning
   if (carry)
      result.insert(result.begin(), '1');

   return result;
};

// lhs > rhs, lhs - rhs
std::string sub_order(std::string lhs, std::string rhs)
{
   std::string result = "";
   // j <= i
   int i = lhs.size() - 1;
   int j = rhs.size() - 1;
   bool carry = false;

   while (j >= 0)
   {
      int x = lhs[i] - 48;
      int y = rhs[j] - 48;
      if (carry)
      {
         carry = false;
         --x;
      }

      int diff = x - y;
      if (diff < 0)
      {
         diff += 10;
         carry = true;
      }

      result.insert(result.begin(), diff + 48);

      --i;
      --j;
   }

   // we need to go through the remaining lhs
   while (i >= 0)
   {
      int x = lhs[i] - 48;
      if (carry)
      {
         carry = false;
         --x;
      }

      if (x < 0)
      {
         carry = true;
         x += 10;
      }
      result.insert(result.begin(), x + 48);

      --i;
   }

   // carry has to be false here
   assert(!carry);
   return result;
}

// +ve string: a - b
std::string sub(std::string lhs, std::string rhs)
{
   if (lhs < rhs)
      return "-" + sub_order(rhs, lhs);
   return sub_order(lhs, rhs);
}


std::string addition(std::string lhs, std::string rhs)
{
   // check for size = 0 case
   if (lhs.size() == 0)
      return rhs;

   if (rhs.size() == 0)
      return lhs;

   auto sign = getSign(lhs, rhs);
   switch (sign)
   {
      case Sign::lhs_neg:
         return sub(rhs, lhs.substr(1));
      case Sign::rhs_neg:
         return sub(lhs, rhs.substr(1));
      case Sign::both_neg:
         return "-" + add(lhs.substr(1), rhs.substr(1));
      case Sign::both_pos:
         return add(lhs, rhs);
      default:
         return "";
   }

   // should never be here
   assert(false);
   return "";
}

int main(int argc, char *argv[])
{
   std::string lhs = "-9865";
   std::string rhs = "879";
   auto result = addition(lhs, rhs);

   std::cout << result << std::endl;
   return 0;
}
