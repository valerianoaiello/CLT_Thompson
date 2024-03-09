/*
In this code we create a class TreeDiagram to make computations in the Thompson group F.

The function multiplicationTreeDiagrams implements the multiplication in the group.

The function unnormalized_moment computer the unnormalized moments of the chromatic polynomial evaluated at 2.
*/

#include <iostream>
#include <fstream>
#include <cmath>
#include <vector>
#include <algorithm>
#include <string>

using std::cout;
using std::vector;
using std::string;
using std::endl;
using std::max;

class TreeDiagram {
private:
    vector<string> top;     // Top tree 
    vector<string> bottom;  // Bottom tree 

public:
    // The default tree diagram represents the identity element of the group
    TreeDiagram(const vector<string>& topTree = {"0"}, const vector<string>& bottomTree = {"0"}) :
        top(topTree),
        bottom(bottomTree) {}

    // Getter method for the top tree
    const vector<string>& getTop() const {
        return top;
    }

    // Getter method for the bottom tree
    const vector<string>& getBottom() const {
        return bottom;
    }

    // Get the number of leaves in the top tree
    int getNumberLeaves() const {
        return top.size();
    }

    // Create an inverse tree diagram by swapping top and bottom trees
    TreeDiagram inverseTreeDiagram() const {
        return TreeDiagram(bottom, top);
    }

    // Print the top and bottom trees
    void printTreeDiagram() {
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
    static TreeDiagram createX0(int version = 2) {
        if (version == 2) {
            return TreeDiagram({"00", "01", "1"}, {"0", "10", "11"});
        } else if (version == 3) {
            return TreeDiagram({"00", "01", "02", "1", "2"}, {"0", "1", "20", "21", "22"});
        } else {
            // Add error handling or default behavior if needed
            return TreeDiagram();
        }
    }

    // Static method to perform a right shift homomorphism on an input tree diagram
    static TreeDiagram rightShiftHomomorphism(const TreeDiagram& inputTree) {
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

    // Static method to perform a flip automorphism on an input tree diagram
    static TreeDiagram flipAutomorphism(const TreeDiagram& inputTree) {
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

        std::sort(treePlusDeepCP.begin(), treePlusDeepCP.end());
        std::sort(treeMinusDeepCP.begin(), treeMinusDeepCP.end());

        return TreeDiagram(treePlusDeepCP, treeMinusDeepCP);
    }

    // Static method to reduce a tree diagram by eliminating a pair of opposing carets
    static TreeDiagram reduceTreeDiagram(const TreeDiagram& treeDiagram) {
        vector<string> treePlus = treeDiagram.getTop();
        vector<string> treeMinus = treeDiagram.getBottom();

        // This is just a check to avoid errors
        if (treeMinus.size() != treePlus.size()) {
            cout << "DANGER, THE TREES DO NOT HAVE THE SAME SIZE" << treeMinus.size() << " " << treePlus.size() << endl;
            TreeDiagram treeDiagramReduced(treePlus, treeMinus);
            treeDiagramReduced.printTreeDiagram();
            return treeDiagramReduced;
        }

        if (!treePlus.empty() && !treeMinus.empty() && treePlus.size()>1 == treeMinus.size()>1) {
            for (int i = treePlus.size() - 2; i >= 0; --i) {
                if (i + 1 < treePlus.size() && treePlus[i].substr(0, treePlus[i].size() - 1) == treePlus[i + 1].substr(0, treePlus[i + 1].size() - 1) && treePlus[i].back() == '0' &&  treePlus[i+1].back() == '1' 
                    && treeMinus[i].substr(0, treeMinus[i].size() - 1) == treeMinus[i + 1].substr(0, treeMinus[i + 1].size() - 1) && treeMinus[i].back() == '0' &&  treeMinus[i+1].back() == '1') {
                    treePlus[i] = treePlus[i].substr(0, treePlus[i].size() - 1);
                    treeMinus[i] = treeMinus[i].substr(0, treeMinus[i].size() - 1);
                    treePlus.erase(treePlus.begin() + i + 1);
                    treeMinus.erase(treeMinus.begin() + i + 1);
                }
            }
        }//asdf        cout<<vett[2].back() <<endl;


        TreeDiagram treeDiagramReduced(treePlus, treeMinus);
        return treeDiagramReduced;
    }
};




/*
This function takes a non-negative integer 'length' as an input and returns 
a complete tree of height 'length', that is with 2^length number of leaves.
*/
vector<string> generateCompleteBinaryTree(int length) {
    int totalStrings = 1 << length;  // Total number of possible strings (2^length)
    vector<string> result;

    // Iterate through all possible binary strings
    for (int i = 0; i < totalStrings; ++i) {
        string binaryString;
        
        // Construct binary string by checking each bit from most significant to least significant
        for (int j = length - 1; j >= 0; --j) {
            binaryString.push_back((i & (1 << j)) ? '1' : '0');  // Set '1' or '0' based on the corresponding bit
        }
        
        // Add the binary string to the result vector
        result.push_back(binaryString);
    }

    return result;
}



/*
This function calculates the weight function associated with the oriented subgroup on any given binary word.
*/
int alternating_sum(const string& word) {
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

/*
This function checks if a given element of F, treeDiagram, is in the oriented subgroup or not.
*/
bool is_in_oriented_subgroup(const TreeDiagram& treeDiagram) {
for (std::size_t i = 0; i < treeDiagram.getTop().size(); ++i) {
    const string& wordPlus = treeDiagram.getTop()[i];
    const string& wordMinus = treeDiagram.getBottom()[i];

    if ((alternating_sum(wordPlus) - alternating_sum(wordMinus)) % 2 != 0) {
        return false;
    }
}
return true;
}



/*
The function findCommonTree takes two TreeDiagrams as input, treeDiagramOne and treeDiagramTwo, 
it returns two new TreeDiagrams, treeDiagramOneNew and treeDiagramTwoNew, that represent
the same elements in F, but with the first having bottom tree equal to the top tree of the second.
This function allows to compute the multiplication of two elements in the function
multiplicationTreeDiagrams
*/
std::pair<TreeDiagram, TreeDiagram> findCommonTree(const TreeDiagram& treeDiagramOne, const TreeDiagram& treeDiagramTwo) {
    vector<string> treePlus = treeDiagramOne.getTop();
    vector<string> treeMinus = treeDiagramOne.getBottom();
    vector<string> treePlusPrime = treeDiagramTwo.getTop();
    vector<string> treeMinusPrime = treeDiagramTwo.getBottom();

    if (treeMinus == treePlusPrime) {
        return {treeDiagramOne, treeDiagramTwo};
    } else {
        // treePlusNew will be the new top tree of the tree diagram treeDiagramOne.
        vector<string> treePlusNew;
        // treeMinusPrimeNew will be the new bottom tree of the tree diagram treeDiagramTwo
        vector<string> treeMinusPrimeNew;

        vector<string> tree_complete_temp;
        // common_tree will be the bottom tree of treeDiagramOne and the top tree of treeDiagramTwo
        vector<string> common_tree;

        int numLeavesTreeDiagramOne = treeDiagramOne.getNumberLeaves();
        int numLeavesTreeDiagramTwo = treeDiagramTwo.getNumberLeaves();

        int maxLength = max(numLeavesTreeDiagramOne, numLeavesTreeDiagramTwo)-1;
//        int maxLength = max(find_max_length(treePlus), find_max_length(treePlusPrime));
        common_tree = generateCompleteBinaryTree(maxLength);

        for (int i=0; i<numLeavesTreeDiagramOne; i++){
            if(treeMinus[i].size()<maxLength){
                tree_complete_temp = generateCompleteBinaryTree(maxLength-treeMinus[i].size());
                for (int j=0; j<tree_complete_temp.size(); j++){
                    treePlusNew.push_back(treePlus[i]+tree_complete_temp[j]);
                }
            } else {
                treePlusNew.push_back(treePlus[i]);
            }
            
        }

        for (int i=0; i<numLeavesTreeDiagramTwo; i++){
            if(treePlusPrime[i].size()<maxLength){
                tree_complete_temp = generateCompleteBinaryTree(maxLength-treePlusPrime[i].size());
                for (int j=0; j<tree_complete_temp.size(); j++){
                    treeMinusPrimeNew.push_back(treeMinusPrime[i]+tree_complete_temp[j]);

                }
            } else {
                treeMinusPrimeNew.push_back(treeMinusPrime[i]);    
            }

        }

        TreeDiagram treeDiagramOneNew = TreeDiagram(treePlusNew, common_tree);
        TreeDiagram treeDiagramTwoNew = TreeDiagram(common_tree, treeMinusPrimeNew);
        
        if (treeDiagramOneNew.getNumberLeaves() != treeDiagramTwoNew.getNumberLeaves()){
            cout << "ERRORE IN FINDCOMMONTREE" <<endl;
        }
        return {treeDiagramOneNew, treeDiagramTwoNew};

    }
}

/*
The function multiplicationTreeDiagrams implements the multiplication of two elements of F.
The inputs are two TreeDiagrams: treeDiagramOne, treeDiagramTwo.
The output is their product: treeDiagramProd.
*/
TreeDiagram multiplicationTreeDiagrams(const TreeDiagram& treeDiagramOne, const TreeDiagram& treeDiagramTwo) {
    if (treeDiagramOne.getNumberLeaves() == 1) {
        return treeDiagramTwo;
    } else if (treeDiagramTwo.getNumberLeaves() == 1) {
        return treeDiagramOne;
    } else if (treeDiagramOne.getTop() == treeDiagramTwo.getBottom() && treeDiagramOne.getBottom() == treeDiagramTwo.getTop()) {
        return TreeDiagram(); // Costruttore di default
    } else {

        vector<string> treePlus = treeDiagramOne.getTop();
        vector<string> treeMinus = treeDiagramOne.getBottom();
        vector<string> treePlusPrime = treeDiagramTwo.getTop();
        vector<string> treeMinusPrime = treeDiagramTwo.getBottom();

        auto tupleContainingUnreducedTreeDiagrams = findCommonTree(treeDiagramOne, treeDiagramTwo);

        TreeDiagram treeDiagramOnePrime = tupleContainingUnreducedTreeDiagrams.first;
        TreeDiagram treeDiagramTwoPrime = tupleContainingUnreducedTreeDiagrams.second;

        vector<string> prodTreePlusTemp = treeDiagramOnePrime.getTop();
        vector<string> prodTreeMinusTemp = treeDiagramTwoPrime.getBottom();

        if (prodTreeMinusTemp.size()<1) {
            cout << "NEW DANGER" << endl;
        }
        TreeDiagram treeDiagramProd(prodTreePlusTemp, prodTreeMinusTemp);

        while (true) {
            size_t lengthTreePlusBefore = treeDiagramProd.TreeDiagram::getNumberLeaves();
            treeDiagramProd = TreeDiagram::reduceTreeDiagram(treeDiagramProd);
            size_t lengthTreePlusAfter = treeDiagramProd.TreeDiagram::getNumberLeaves();

            if (lengthTreePlusBefore == lengthTreePlusAfter ) {
                break;
            }
        }
        
        return treeDiagramProd;
    }
}


/*
The function multiplyManyTreeDiagrams has as input a collection of TreeDiagrams, collectionOfTreeDiagram.
The output is their product.
*/
TreeDiagram multiplyManyTreeDiagrams(const vector<TreeDiagram>& collectionOfTreeDiagram) {
    size_t numberOfElements = collectionOfTreeDiagram.size();
    TreeDiagram product;

    if (numberOfElements == 0) {
        return product;
    } else if (numberOfElements == 1) {
        return collectionOfTreeDiagram[0];
    } else {
        for (size_t i = 0; i < numberOfElements; ++i) {
            product = multiplicationTreeDiagrams(product, collectionOfTreeDiagram[i]);
        }
        return product;
    }
}

/* 
Function to generate all sequences with repetitions of length d
 from the vector {0, ..., n-1}
*/ 
vector<vector<int>> generateSequences(int n, int d) {
    // Create a vector containing elements from 0 to n-1
    vector<int> elements;
    for (int i = 0; i < n; i++) {
        elements.push_back(i);
    }

    // Initialize the result vector to store generated sequences
    vector<vector<int>> result;
    
    // Initialize vector to hold indices for combinations
    vector<int> combinationIndices(d, 0);

    do {
        // Use the indices to access elements in the 'elements' vector
        vector<int> currentCombination;
        for (int i = 0; i < d; ++i) {
            currentCombination.push_back(elements[combinationIndices[i]]);
        }

        // Add the current combination to the result
        result.push_back(currentCombination);

        // Generate the next combination with repetitions
        int i = d - 1;
        while (i >= 0 && combinationIndices[i] == static_cast<int>(elements.size()) - 1) {
            --i;
        }

        if (i >= 0) {
            ++combinationIndices[i];
            for (int j = i + 1; j < d; ++j) {
                combinationIndices[j] = 0;
            }
        } else {
            break; // All combinations with repetitions have been generated
        }
    } while (true);

    return result;
}




/* This function calculates the unnormalized mmoments, that is
(s_{sequence_index})^(exponent_power) * (sqrt{exponent_power*sequence_index})^exponent_power
*/
int unnormalized_moment(int sequence_index, int exponent_power) {
    // Base case for exponent_power equal to 0
    if (exponent_power == 0) {
        return 1;
    } 
    // If exponent_power is odd, the result is 0
    else if (exponent_power % 2 == 1) {
        return 0;
    } 
    // If exponent_power is positive
    else if (exponent_power > 0) {
        // Create necessary elements
        int sum = 0; 
        TreeDiagram x_0 = TreeDiagram::createX0();
        vector<TreeDiagram> listOfElements = {x_0, x_0.inverseTreeDiagram()};
        
        // Generate additional elements up to sequence_index
        for (int i = 1; i < sequence_index; ++i) {
            listOfElements.push_back(TreeDiagram::rightShiftHomomorphism(listOfElements.back()));
            listOfElements.push_back(listOfElements.back().inverseTreeDiagram());
        }

        // Generate all possible combinations of indices for summands
        vector<vector<int>> collection_of_indices_of_summands = generateSequences(listOfElements.size(), exponent_power);        
        TreeDiagram summand;

        // Iterate through each combination of indices
        for (int i = 0; i < collection_of_indices_of_summands.size(); i++) {
            vector<TreeDiagram> factors_in_summand;

            // Create factors for the current summand
            for (int j = 0; j < exponent_power; j++) {
                factors_in_summand.push_back(listOfElements[collection_of_indices_of_summands[i][j]]);
            } 
            // Multiply the factors to obtain the current summand
            summand = multiplyManyTreeDiagrams(factors_in_summand);

            // Check if the summand is in the oriented subgroup
            if (is_in_oriented_subgroup(summand)) {
                sum += 1;
            }
            summand = TreeDiagram({"0"}, {"0"}); // Reset summand for the next iteration
        } 

        return sum;
    }

    return 0; // Default case
}


// Function to find nonzero indices in a vector
std::vector<int> find_nonzero_indices(const std::vector<int>& array) {
    std::vector<int> nonzero_indices;
    for (int i = 0; i < array.size(); ++i) {
        if (array[i] != 0) {
            nonzero_indices.push_back(i);
        }
    }
    return nonzero_indices;
}

/*
Function to find the normal form of a binary tree diagram.
The input is a TreeDiagram. The output are two arrays of integers.
The first contains exponents of the positive part of the normal form,
the second that of the negative part, for example {{0, 1, 0}, {1, 0, 0}}
means that the element has normal form x_1 x_0^{-1}.
*/ 
std::pair<std::vector<int>, std::vector<int>> find_normal_form(TreeDiagram tree_diagram) {
    int length = tree_diagram.getNumberLeaves();
    vector<int> positive_part(length, 0);
    vector<int> negative_part(length, 0);

    auto top_tree = tree_diagram.getTop();
    auto bottom_tree = tree_diagram.getBottom();

    positive_part[0] = top_tree[0].size() - 1;
    negative_part[0] = bottom_tree[0].size() - 1;

    size_t last_digit_1_position;
    size_t the_last_digit_0_before_last_digit_1;
//    size_t find_last_not_of;
    // Iterate over each level of the tree
    positive_part[0] = top_tree[0].length()-1;
    for (int i = 1; i < length; ++i) {
        if (top_tree[i].back() == '0') {
            last_digit_1_position = top_tree[i].find_last_of('1');
            the_last_digit_0_before_last_digit_1 = top_tree[i].substr(0, last_digit_1_position + 1).find_last_of('0');
            if (the_last_digit_0_before_last_digit_1 != std::string::npos){
                positive_part[i] = top_tree[i].length() - top_tree[i].substr(0, last_digit_1_position + 1).length();
            }
            else{
                positive_part[i] = top_tree[i].length() - top_tree[i].substr(0, last_digit_1_position + 1).length() - 1;

            }
        } 

        // Calculate negative part for the current level
        last_digit_1_position = bottom_tree[i].find_last_of('1');
        the_last_digit_0_before_last_digit_1 = bottom_tree[i].substr(0, last_digit_1_position + 1).find_last_of('0');
        if (bottom_tree[i].back() == '0') {
            last_digit_1_position = bottom_tree[i].find_last_of('1');
            the_last_digit_0_before_last_digit_1 = bottom_tree[i].substr(0, last_digit_1_position + 1).find_last_of('0');
            if (the_last_digit_0_before_last_digit_1 != std::string::npos){
                negative_part[i] = top_tree[i].length() - top_tree[i].substr(0, last_digit_1_position + 1).length();
            }
            else{
                negative_part[i] = top_tree[i].length() - top_tree[i].substr(0, last_digit_1_position + 1).length() - 1;

            }
        } 
    }

    // Find nonzero indices in positive and negative parts
    auto nonzero_indices_positive_part = find_nonzero_indices(positive_part);
    auto nonzero_indices_negative_part = find_nonzero_indices(negative_part);

    // Calculate lengths of positive and negative parts
    int length_positive_part = (!nonzero_indices_positive_part.empty()) ? nonzero_indices_positive_part.back() + 1 : 0;
    int length_negative_part = (!nonzero_indices_negative_part.empty()) ? nonzero_indices_negative_part.back() + 1 : 0;

    // Resize vectors to the maximum length between positive and negative parts
    if (length_positive_part + length_negative_part > 0) {
        int max_length = std::max(length_positive_part, length_negative_part);
        positive_part.resize(max_length + 1);
        negative_part.resize(max_length + 1);
    } else {
        positive_part.clear();
        negative_part.clear();
    }

    return std::make_pair(positive_part, negative_part);
}

int main() {

    TreeDiagram x_0 = TreeDiagram::createX0();
    cout << "x_0: " << endl;

    TreeDiagram x_1 = TreeDiagram::rightShiftHomomorphism(x_0);

    int d = 4;
    for (int n = 1; n<4; ++n){
        cout << "\nmoment d= " << d << ", n=" << n << ", unnormalized_moment= "<< unnormalized_moment(n, d) << endl;
    }

    TreeDiagram prodotto;
    vector<int> formanormale;

    prodotto = multiplicationTreeDiagrams(x_1, x_0);
    formanormale = find_normal_form(prodotto).first;
    cout<< "x1x0\n";
    prodotto.printTreeDiagram();
    
    for (int i=0; i< formanormale.size(); ++i){
        cout<< " " << formanormale[i];
    }
    cout<<endl;

    prodotto = x_1;
    cout<< "x1\n";
    prodotto.printTreeDiagram();
    formanormale = find_normal_form(prodotto).first;
    
    for (int i=0; i< formanormale.size(); ++i){
        cout<< " " << formanormale[i];
    }
    cout<<endl;

    prodotto = x_0;
    formanormale = find_normal_form(prodotto).first;
    cout<< "x0\n";

    prodotto.printTreeDiagram();
    
    for (int i=0; i< formanormale.size(); ++i){
        cout<< " " << formanormale[i];
    }
    cout << endl;
   


    return 0;
} 