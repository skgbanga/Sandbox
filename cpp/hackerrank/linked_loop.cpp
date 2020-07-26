#include <iostream>
#include <memory>
#include <array>

template <typename T>
struct Node
{
   T value {};
   Node* next {nullptr};
};

template <typename T>
bool detect_loop(T* head)
{
   auto slow = head;
   auto fast = head->next;
   while (fast)
   {
      fast = fast->next;
      if (!fast)
         return false;
      fast = fast->next;
      slow = slow->next;
      if (fast == slow) // they will hit each other if there is a loop
         return true;
   }
   return false;
}

template <typename T>
void print_list(T* head)
{
   while (head)
   {
      std::cout << head->value << ' ';
      head = head->next;
   }
}

template <typename T>
void delete_list(T* head) // would call deletes even when passing raw pointers
{
   while (head)
   {
      auto* next = head->next;
      delete head;
      head = next;
   }
}

int main(int argc, char *argv[])
{
   constexpr int num = 13;
   using NodeType = Node<int>;

   // construct list
   std::array<NodeType, num> nodeArray;
   for (int i = 0; i < num; ++i)
   {
      nodeArray[i].value = i;
      if (i == num - 1)
         break;
      nodeArray[i].next = &nodeArray[i + 1];
   }

   // get head of the list
   auto& head = nodeArray[0];

   // Create a loop
   // get 3rd element
   auto third = &head;
   for (int i = 0; i < 3; ++i)
      third = third->next;

   // get 9th element
   auto ninth = &head;
   for (int i = 0; i < 9; ++i)
      ninth = ninth->next;

   auto tenth = ninth->next;
   ninth->next = third;

   auto result = detect_loop(&head);
   std::cout << "Loop detected: " << ((result) ? "Yes" : "No") << '\n';

   return 0;
}
