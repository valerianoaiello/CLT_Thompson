#ifndef C_GROUP_OPERATION_H
#define C_GROUP_OPERATION_H

#include "TreeDiagram.h"
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

class CGroupOperation {
public:
    static std::vector<std::string> generateCompleteBinaryTree(int length);

    static int alternating_sum(const std::string& word);

    static bool is_in_oriented_subgroup(const TreeDiagram& treeDiagram);

    static std::pair<TreeDiagram, TreeDiagram> findCommonTree(const TreeDiagram& treeDiagramOne, const TreeDiagram& treeDiagramTwo);

    static TreeDiagram multiplicationTreeDiagrams(const TreeDiagram& treeDiagramOne, const TreeDiagram& treeDiagramTwo);

    static TreeDiagram multiplyManyTreeDiagrams(const std::vector<TreeDiagram>& collectionOfTreeDiagram);

    static std::vector<std::vector<int>> generateSequences(int n, int d);

    static int unnormalized_moment(int sequence_index, int exponent_power);

    static std::vector<int> find_nonzero_indices(const std::vector<int>& array);

    static std::pair<std::vector<int>, std::vector<int>> find_normal_form(TreeDiagram tree_diagram);
};

#endif // C_GROUP_OPERATION_H
