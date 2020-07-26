#include <iostream>

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
};

void flatten(TreeNode* root) {
    // handle bad test input
    if (!root)
        return;

    // base cases
    if ((root->left == nullptr) && (root->right == nullptr)) // already flattened
        return;

    // atleast one of the subtree has some values  
    // left is nullptr
    if (root->left == nullptr)
    {
        flatten(root->right);
        return;
    }

    // right is nullptr
    if (root->right == nullptr)
    {
        flatten(root->left);
        root->right = root->left;
        return;
    }

    // both the subtrees are present
    // recursive step
    flatten(root->left);
    flatten(root->right);

    // we have two linked list on both the subtrees
    // traverse to the end of left subtree
    auto* end = root->left;
    while (end->right != nullptr)
        end = end->right;

    // save root's right (flattened)
    auto* right = root->right;

    // re-wire everything
    root->right = root->left;
    end->right = right;

    // we are good
}

int main()
{
   TreeNode root(1);
   TreeNode left(2);

   root.left = &left;

   flatten(&root);

   std::cout << root.val << std::endl;
   std::cout << root.right->val << std::endl;

   return 0;
}
