#include "c_group_operations.h"

std::vector<std::string> CGroupOperation::generateCompleteBinaryTree(int length) {
    int totalStrings = 1 << length;  // Total number of possible strings (2^length)
    std::vector<std::string> result;

    // Iterate through all possible binary strings
    for (int i = 0; i < totalStrings; ++i) {
        std::string binaryString;

        // Construct binary string by checking each bit from most significant to least significant
        for (int j = length - 1; j >= 0; --j) {
            binaryString.push_back((i & (1 << j)) ? '1' : '0');  // Set '1' or '0' based on the corresponding bit
        }

        // Add the binary string to the result vector
        result.push_back(binaryString);
    }

    return result;
}

int CGroupOperation::alternating_sum(const std::string& word) {
    int sum = 0;
    for (std::size_t i = 0; i < word.length(); ++i) {
        if (i % 2 == 0) {
            sum += static_cast<int>(word[i] - '0');
        } else {
            sum -= static_cast<int>(word[i] - '0');
        }
    }
    return sum;
}

bool CGroupOperation::is_in_oriented_subgroup(const TreeDiagram& treeDiagram) {
    for (std::size_t i = 0; i < treeDiagram.getTop().size(); ++i) {
        const std::string& wordPlus = treeDiagram.getTop()[i];
        const std::string& wordMinus = treeDiagram.getBottom()[i];

        if ((alternating_sum(wordPlus) - alternating_sum(wordMinus)) % 2 != 0) {
            return false;
        }
    }
    return true;
}

std::pair<TreeDiagram, TreeDiagram> CGroupOperation::findCommonTree(const TreeDiagram& treeDiagramOne,
                                                                    const TreeDiagram& treeDiagramTwo) {
    std::vector<std::string> treePlus = treeDiagramOne.getTop();
    std::vector<std::string> treeMinus = treeDiagramOne.getBottom();
    std::vector<std::string> treePlusPrime = treeDiagramTwo.getTop();
    std::vector<std::string> treeMinusPrime = treeDiagramTwo.getBottom();

    if (treeMinus == treePlusPrime) {
        return {treeDiagramOne, treeDiagramTwo};
    } else {
        // treePlusNew will be the new top tree of the tree diagram treeDiagramOne.
        std::vector<std::string> treePlusNew;
        // treeMinusPrimeNew will be the new bottom tree of the tree diagram treeDiagramTwo
        std::vector<std::string> treeMinusPrimeNew;

        std::vector<std::string> tree_complete_temp;
        // common_tree will be the bottom tree of treeDiagramOne and the top tree of treeDiagramTwo
        std::vector<std::string> common_tree;

        int numLeavesTreeDiagramOne = treeDiagramOne.getNumberLeaves();
        int numLeavesTreeDiagramTwo = treeDiagramTwo.getNumberLeaves();

        int maxLength = max(numLeavesTreeDiagramOne, numLeavesTreeDiagramTwo) - 1;

        common_tree = generateCompleteBinaryTree(maxLength);

        for (int i = 0; i < numLeavesTreeDiagramOne; i++) {
            if (treeMinus[i].size() < maxLength) {
                tree_complete_temp = generateCompleteBinaryTree(maxLength - treeMinus[i].size());
                for (int j = 0; j < tree_complete_temp.size(); j++) {
                    treePlusNew.push_back(treePlus[i] + tree_complete_temp[j]);
                }
            } else {
                treePlusNew.push_back(treePlus[i]);
            }
        }

        for (int i = 0; i < numLeavesTreeDiagramTwo; i++) {
            if (treePlusPrime[i].size() < maxLength) {
                tree_complete_temp = generateCompleteBinaryTree(maxLength - treePlusPrime[i].size());
                for (int j = 0; j < tree_complete_temp.size(); j++) {
                    treeMinusPrimeNew.push_back(treeMinusPrime[i] + tree_complete_temp[j]);
                }
            } else {
                treeMinusPrimeNew.push_back(treeMinusPrime[i]);
            }
        }

        TreeDiagram treeDiagramOneNew = TreeDiagram(treePlusNew, common_tree);
        TreeDiagram treeDiagramTwoNew = TreeDiagram(common_tree, treeMinusPrimeNew);

        if (treeDiagramOneNew.getNumberLeaves() != treeDiagramTwoNew.getNumberLeaves()) {
            std::cout << "ERRORE IN FINDCOMMONTREE" << std::endl;
        }
        return {treeDiagramOneNew, treeDiagramTwoNew};
    }
}

TreeDiagram CGroupOperation::multiplicationTreeDiagrams(const TreeDiagram& treeDiagramOne,
                                                        const TreeDiagram& treeDiagramTwo) {
    if (treeDiagramOne.getNumberLeaves() == 1) {
        return treeDiagramTwo;
    } else if (treeDiagramTwo.getNumberLeaves() == 1) {
        return treeDiagramOne;
    } else if (treeDiagramOne.getTop() == treeDiagramTwo.getBottom() &&
               treeDiagramOne.getBottom() == treeDiagramTwo.getTop()) {
        return TreeDiagram(); // Costruttore di default
    } else {

        std::vector<std::string> treePlus = treeDiagramOne.getTop();
        std::vector<std::string> treeMinus = treeDiagramOne.getBottom();
        std::vector<std::string> treePlusPrime = treeDiagramTwo.getTop();
        std::vector<std::string> treeMinusPrime = treeDiagramTwo.getBottom();

