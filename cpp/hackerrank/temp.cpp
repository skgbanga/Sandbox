#include <vector>
#include <unordered_set>
#include <string>
#include <algorithm>
#include <iostream>

template <typename T>
void print_vec(const std::vector<T>& vec)
{
   for (const auto& elem : vec)
      std::cout << elem.word << " ";
   std::cout << std::endl;
}

// solution 1 - brute force (recursive solution)
int word_longest_chain_recurs(const std::string& word, const std::unordered_set<std::string>& dict)
{
   if (dict.count(word) == 0) // element doesn't exist - stop now
      return 0;

   int count = 1; // init the count to 1 since this word exist
   for (int i = 0; i < word.size(); ++i)
   {
      // create a copy of the word by removing ths ith character
      auto cpy = word;
      cpy.erase(i, 1); // remove one character
      auto result = word_longest_chain_recurs(cpy, dict);
      count = std::max(count, result + 1);
   }

   return count;
}

// solution 2 - dynamic programming
// we keep track of the list of the words along with how many times they have been incremented
// and in the end we go through the entire list once to find the maximal number
int word_longest_chain_dp(const std::string& word, const std::unordered_set<std::string>& dict)
{
   struct Data
   {
      Data(std::string&& str, int c)
         : word(std::move(str)),
           count(c)
      {  }
      std::string word {""};
      int count;
   };
   std::vector<Data> vec;
   // we are gradually trying to build all the substr of the word from ground up

   auto wcpy = word;
   std::sort(wcpy.begin(), wcpy.end());

   // initialize vec
   std::for_each(wcpy.begin(), wcpy.end(), [&vec, &dict](char c)
         {
            std::string temp (1, c);
            if (dict.count(temp) != 0)
               vec.emplace_back(std::move(temp), 1);
         });

   for (int i = 1; i < wcpy.size(); ++i) // a loop to be executed n - 1 times
   {
      int sz = vec.size();
      for (int j = 0; j < sz; ++j) // for each element present in the vec
      {
         // try out all the possible combinations in wcpy
         auto& elem = vec[j];
         for (auto c : wcpy)
         {
            auto cpy = elem.word;
            cpy.push_back(c);

            // now see if this cpy is part of the dictionary
            if (dict.count(cpy) != 0)
               vec.emplace_back(std::move(cpy), elem.count + 1);
         }
      }
   }

   // return the element which has been constructed maximum number of times
   auto lamda = [](const Data& lhs, const Data& rhs)
   {
      return lhs.count < rhs.count;
   };
   auto iter = std::max_element(vec.begin(), vec.end(), lamda);
   return iter->count;
}

int longestChain(const std::vector<std::string>& words) {
   // construct a dictionary first so that we can look things up quickly
   std::unordered_set<std::string> dict;
   for (const auto& word : words)
      dict.insert(word);

   // we have out dictionary populated correctly now
   int maximum = 0;
   for (const auto& word : words)
      maximum = std::max(word_longest_chain_recurs(word, dict), maximum);

   return maximum;
}

int main(int argc, char *argv[])
{
   std::vector<std::string> vec { "a", "b", "ba", "bca", "bda", "bdca" };
   std::cout << longestChain(vec) << std::endl;
   return 0;
}
