#ifndef TREEDIAGRAM_H
#define TREEDIAGRAM_H

#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

class TreeDiagram {
private:
    std::vector<std::string> top;     // Top tree 
    std::vector<std::string> bottom;  // Bottom tree 

public:
    // The default tree diagram represents the identity element of the group
    TreeDiagram(const std::vector<std::string>& topTree = {"0"}, const std::vector<std::string>& bottomTree = {"0"});

    // Getter method for the top tree
    const std::vector<std::string>& getTop() const;

    // Getter method for the bottom tree
    const std::vector<std::string>& getBottom() const;

    // Get the number of leaves in the top tree
    int getNumberLeaves() const;

    // Create an inverse tree diagram by swapping top and bottom trees
    TreeDiagram inverseTreeDiagram() const;

    // Print the top and bottom trees
    void printTreeDiagram();

    // Static method to create a specific tree diagram X_0, the first generator of the group in the infinite presentation
    static TreeDiagram createX0();

    // Static method to perform a right shift homomorphism on an input tree diagram
    static TreeDiagram rightShiftHomomorphism(const TreeDiagram& inputTree);

    // Static method to perform a left shift homomorphism on an input tree diagram
    static TreeDiagram leftShiftHomomorphism(const TreeDiagram& inputTree);

    // Static method to perform a flip automorphism on an input tree diagram
    static TreeDiagram flipAutomorphism(const TreeDiagram& inputTree);

    // Static method to reduce a tree diagram by eliminating a pair of opposing carets
    static TreeDiagram reduceTreeDiagram(const TreeDiagram& treeDiagram);
};

#endif // TREEDIAGRAM_H
