#include <cassert>
#include <iostream>
#include <memory>
#include <queue>
#include <random>
#include <stdexcept>

using namespace std;

int randint(int m, int n) {
  static random_device rd;
  static mt19937 gen(rd());
  uniform_int_distribution<> dist(m, n);
  return dist(gen);
}

struct Node {
  Node(int v) : value(v) {}
  int value;
  unique_ptr<Node> left = nullptr;
  unique_ptr<Node> right = nullptr;
};

class MinHeap {
 public:
  void insert(int value) {
    if (!top_) {
      top_ = make_unique<Node>(value);
    } else {
      bubble_down(top_.get(), value);
    }
  }

  int extract_min() {
    if (!top_) {
      throw std::runtime_error("nothing");
    } else {
      auto value = top_->value;
      if (no_children(top_.get())) {
        top_ = nullptr;
      } else {
        bubble_up(top_.get(), nullptr);
      }
      return value;
    }
    assert(false);
  }

  void print() {
    if (!top_) {
      return;
    }

    using item = pair<int, Node*>;
    queue<item> q;
    q.push(make_pair(0, top_.get()));

    int last = 0;
    while (q.size()) {
      auto head = q.front();
      q.pop();

      if (head.first != last) {
        cout << '\n';
        last = head.first;
      }
      cout << head.second->value << ' ';
      if (head.second->left) {
        q.push(make_pair(head.first + 1, head.second->left.get()));
      }
      if (head.second->right) {
        q.push(make_pair(head.first + 1, head.second->right.get()));
      }
    }
    cout << '\n';
  }

 private:
  bool no_children(Node* node) { return (!node->left) && (!node->right); }
  void bubble_down(Node* head, int value) {
    assert(head);

    if (value < head->value) {
      swap(value, head->value);
    }

    if (!head->left) {
      head->left = make_unique<Node>(value);
      return;
    }
    if (!head->right) {
      head->right = make_unique<Node>(value);
      return;
    }

    auto num = randint(0, 1);
    if (num == 0) {
      bubble_down(head->left.get(), value);
    } else {
      assert(num == 1);
      bubble_down(head->right.get(), value);
    }
  }

  void bubble_up(Node* node, Node* parent) {
    cout << "Bubbling up " << node->value << " "
         << ((parent) ? parent->value : -1) << '\n';
    assert(node);

    if (no_children(node)) {
      cout << "Reached end with " << node->value << '\n';
      break_link(node, parent);
      return;
    }
    // has atleast one children
    if (!node->left) {
      swap(node->value, node->right->value);
      bubble_up(node->right.get(), node);
      return;
    }
    if (!node->right) {
      swap(node->value, node->left->value);
      bubble_up(node->left.get(), node);
      return;
    }

    Node* selected = nullptr;
    if (node->left->value >= node->right->value) {
      selected = node->right.get();
    } else {
      selected = node->left.get();
    }

    swap(node->value, selected->value);
    bubble_up(selected, node);
  }

  void break_link(Node* node, Node* parent) {
    cout << "Breaking link with " << node->value << " " << parent->value
         << '\n';
    assert(node);
    assert(parent);

    // this will call dtor as well
    cout << node << " " << parent << " " << parent->left.get() << " "
         << parent->right.get() << '\n';
    if (node == parent->left.get()) {
      cout << "left set null\n";
      parent->left == nullptr;
      cout << parent->left.get() << '\n';
    } else if (node == parent->right.get()) {
      cout << "right set null\n";
      parent->right = nullptr;
    } else {
      cout << "None set\n";
    }
    cout << parent->left.get() << " " << parent->right.get() << '\n';
  }

  unique_ptr<Node> top_ = nullptr;
};

void print(vector<int>& nums) {
  for (auto num : nums) {
    cout << num << ", ";
  }
  cout << '\n';
}

int main() {
  // vector<int> nums;
  // for (int i = 0; i < 10; ++i) {
  //   nums.push_back(randint(100, 200));
  // }
  // print(nums);

  vector<int> nums = {144, 118, 147, 126, 184, 180, 135, 178, 135, 194};
  MinHeap heap{};
  for (auto num : nums) {
    heap.insert(num);
  }

  heap.print();
  cout << "===============\n";
  // for (int i = 0; i < 10; ++i) {
  auto value = heap.extract_min();
  cout << "min: " << value << '\n';
  heap.print();
  cout << "==\n";
  //}
}
