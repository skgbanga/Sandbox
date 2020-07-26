#include <string>
#include <iostream>
#include <vector>

// Each Node can have at max 26 children
struct TrieNode {
    // Initialize your data structure here.
    TrieNode(std::string value)
       : value_(value)
    {  }

    std::string value_;
    std::vector<TrieNode> children_;
    bool intermediate {true};
};

class Trie  {
public:

    // Inserts a word into the trie.
    void insert(std::string word) {
       std::string subword = "";
       auto* node = &root_; // pointer to the root
       for (auto x : word)
       {
          subword.push_back(x);
          auto& children = node->children_;
          auto found = std::find_if(children.begin(), children.end(),
                [&](const TrieNode& x)
                {
                  return x.value_ == subword;
                });
          if (found == children.end())
          {
             // we are seeing this for the first time, add to the children
             children.emplace_back(subword);
             node = &children.back();
          }
          else
          {
             node = &(*found); // pointer to the element
          }
       }

       // set final node's intermediate to false
       // to imply that it contains a value
       node->intermediate = false;
    }

    // Returns if the word is in the trie.
    bool search_impl(std::string word, bool intermediate_check) {
       std::string subword = "";
       auto* node = &root_;
       for (auto x : word)
       {
          subword.push_back(x);
          auto& children = node->children_;
          auto found = std::find_if(children.begin(), children.end(),
                [&](const TrieNode& x)
                {
                  return x.value_ == subword;
                });
          if (found == children.end())
             return false;
          node = &(*found); // pointer to the element
       }

       if (intermediate_check)
          return !node->intermediate;

       return true;
    }


    bool search(std::string word)
    {
       return search_impl(word, true);
    }

    // Returns if there is any word in the trie
    // that starts with the given prefix.
    bool startsWith(std::string prefix) {
       return search_impl(prefix, false);
    }

private:
    TrieNode root_ {""};
};

int main(int argc, char *argv[])
{
   Trie trie;
   trie.insert("Sandeep");
   trie.insert("Sandy");

   std::cout << std::boolalpha << trie.startsWith("Sand") << std::endl;
   return 0;
}
