#include "TreeDiagram.h"

using namespace std;

// Constructor
TreeDiagram::TreeDiagram(const vector<string>& topTree, const vector<string>& bottomTree) :
    top(topTree),
    bottom(bottomTree) {}

// Getter method for the top tree
const vector<string>& TreeDiagram::getTop() const {
    return top;
}

// Getter method for the bottom tree
const vector<string>& TreeDiagram::getBottom() const {
    return bottom;
}

// Get the number of leaves in the top tree
int TreeDiagram::getNumberLeaves() const {
    return top.size();
}

// Create an inverse tree diagram by swapping top and bottom trees
TreeDiagram TreeDiagram::inverseTreeDiagram() const {
    return TreeDiagram(bottom, top);
}

// Print the top and bottom trees
void TreeDiagram::printTreeDiagram() {
    cout << "top tree: ";
    for (const auto &item : top) {
        cout << item << " ";
    }
    cout << endl;

    cout << "bottom tree: ";
    for (const auto &item : bottom) {
        cout << item << " ";
    }
    cout << endl;
}

// Static method to create a specific tree diagram X_0, the first generator of the group in the infinite presentation
TreeDiagram TreeDiagram::createX0() {
    return TreeDiagram({"00", "01", "1"}, {"0", "10", "11"});
}

// Static method to perform a right shift homomorphism on an input tree diagram
TreeDiagram TreeDiagram::rightShiftHomomorphism(const TreeDiagram& inputTree) {
    vector<string> treePlusDeepCP = inputTree.getTop();
    vector<string> treeMinusDeepCP = inputTree.getBottom();

    for (size_t i = 0; i < treePlusDeepCP.size(); ++i) {
        treePlusDeepCP[i] = "1" + treePlusDeepCP[i];
        treeMinusDeepCP[i] = "1" + treeMinusDeepCP[i];
    }

    treePlusDeepCP.insert(treePlusDeepCP.begin(), "0");
    treeMinusDeepCP.insert(treeMinusDeepCP.begin(), "0");

    return TreeDiagram(treePlusDeepCP, treeMinusDeepCP);
}

// Static method to perform a left shift homomorphism on an input tree diagram
TreeDiagram TreeDiagram::leftShiftHomomorphism(const TreeDiagram& inputTree) {
    vector<string> treePlusDeepCP = inputTree.getTop();
    vector<string> treeMinusDeepCP = inputTree.getBottom();

    for (size_t i = 0; i < treePlusDeepCP.size(); ++i) {
        treePlusDeepCP[i] = "0" + treePlusDeepCP[i];
        treeMinusDeepCP[i] = "0" + treeMinusDeepCP[i];
    }

    treePlusDeepCP.insert(treePlusDeepCP.end(), "1");
    treeMinusDeepCP.insert(treeMinusDeepCP.end(), "1");

    return TreeDiagram(treePlusDeepCP, treeMinusDeepCP);
}

// Static method to perform a flip automorphism on an input tree diagram
TreeDiagram TreeDiagram::flipAutomorphism(const TreeDiagram& inputTree) {
    vector<string> treePlusDeepCP = inputTree.getTop();
    vector<string> treeMinusDeepCP = inputTree.getBottom();

    for (size_t i = 0; i < treePlusDeepCP.size(); ++i) {
        for (size_t j = 0; j < treePlusDeepCP[i].size(); ++j) {
            if (treePlusDeepCP[i][j] == '0') {
                treePlusDeepCP[i][j] = '1';
            } else if (treePlusDeepCP[i][j] == '1') {
                treePlusDeepCP[i][j] = '0';
            }
        }

        for (size_t j = 0; j < treeMinusDeepCP[i].size(); ++j) {
            if (treeMinusDeepCP[i][j] == '0') {
                treeMinusDeepCP[i][j] = '1';
            } else if (treeMinusDeepCP[i][j] == '1') {
                treeMinusDeepCP[i][j] = '0';
            }
        }
    }

    sort(treePlusDeepCP.begin(), treePlusDeepCP.end());
    sort(treeMinusDeepCP.begin(), treeMinusDeepCP.end());

    return TreeDiagram(treePlusDeepCP, treeMinusDeepCP);
}

// Static method to reduce a tree diagram by eliminating a pair of opposing carets
TreeDiagram TreeDiagram::reduceTreeDiagram(const TreeDiagram& treeDiagram) {
    std::vector<std::string> treePlus = treeDiagram.getTop();
    std::vector<std::string> treeMinus = treeDiagram.getBottom();

    if (!treePlus.empty() && !treeMinus.empty() && treePlus.size() > 1 && treeMinus.size() > 1) {
        for (int i = treePlus.size() - 2; i >= 0; --i) {
            if (i + 1 < treePlus.size() && treePlus[i].substr(0, treePlus[i].size() - 1) == treePlus[i + 1].substr(0, treePlus[i + 1].size() - 1) && treePlus[i].back() == '0' && treePlus[i + 1].back() == '1'
                && treeMinus[i].substr(0, treeMinus[i].size() - 1) == treeMinus[i + 1].substr(0, treeMinus[i + 1].size() - 1) && treeMinus[i].back() == '0' && treeMinus[i + 1].back() == '1') {
                treePlus[i] = treePlus[i].substr(0, treePlus[i].size() - 1);
                treeMinus[i] = treeMinus[i].substr(0, treeMinus[i].size() - 1);
                treePlus.erase(treePlus.begin() + i + 1);
                treeMinus.erase(treeMinus.begin() + i + 1);
            }
        }
    }

    TreeDiagram treeDiagramReduced(treePlus, treeMinus);
    return treeDiagramReduced;
}
