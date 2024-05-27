/*
In this file we implement the structure of the Thompson group F.
Each element of F is described by a pair of vectors, the first for the top tree, 
the second for the bottom tree, whose components are string (each string is a word in '0' and '1' 
representing a leaf, the word describes the shortest path from the root of tree to the leaf, 
'0'= left edge, '1'= right edge).

The class 'TreeDiagram' allows the user to define elements of F, take their inverses 'inverseTreeDiagram', 
get the number of leaves 'getNumberLeaves', print them 'printTreeDiagram',
get the top tree 'getTop', get the bottom tree 'getBottom', 
create the generator x_0 'createX0', apply the right shift homomorphism 'rightShiftHomomorphism',
apply a left shift homomorphism 'leftShiftHomomorphism',
apply the flip automorphism 'flipAutomorphism', apply a reduction move on a tree diagram
'reduceTreeDiagram'. 

The function 'multiplicationTreeDiagrams' implements the multiplication in the group.
The function 'unnormalized_chromatic_moment' computes the unnormalized moments of the chromatic polynomial evaluated at 2.
*/

#include <iostream>
#include <fstream>
#include <cmath>
#include <vector>
#include <algorithm>
#include <string>
#include <chrono>
//#include <omp.h> // Include OpenMP header

using std::cout;
using std::cin;
using std::vector;
using std::string;
using std::endl;    
using std::max;
using std::pair;
using std::make_pair;

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
    static TreeDiagram createX0() {
        return TreeDiagram({"00", "01", "1"}, {"0", "10", "11"});
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

    // Static method to perform a left shift homomorphism on an input tree diagram
    static TreeDiagram leftShiftHomomorphism(const TreeDiagram& inputTree) {
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
        }


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

// Function to find the length of the longest string among all arrays
size_t find_longest_string_length(const std::vector<std::string>& arr1,
                                  const std::vector<std::string>& arr2,
                                  const std::vector<std::string>& arr3,
                                  const std::vector<std::string>& arr4) {
    size_t max_length = 0;

    // Iterate over the first array
    for (const string& str : arr1) {
        max_length = max(max_length, str.length());
    }

    // Iterate over the second array
    for (const string& str : arr2) {
        max_length = max(max_length, str.length());
    }

    // Iterate over the third array
    for (const string& str : arr3) {
        max_length = max(max_length, str.length());
    }

    // Iterate over the fourth array
    for (const string& str : arr4) {
        max_length = max(max_length, str.length());
    }

    return max_length;
}

/*
The function findCommonTree takes two TreeDiagrams as input, treeDiagramOne and treeDiagramTwo, 
it returns two new TreeDiagrams, treeDiagramOneNew and treeDiagramTwoNew, that represent
the same elements in F, but with the first having bottom tree equal to the top tree of the second.
This function allows to compute the multiplication of two elements in the function
multiplicationTreeDiagrams
The variable 'minimal_search' if equal to 0 means that we look for the minimal common tree.
*/
pair<TreeDiagram, TreeDiagram> findCommonTree(const TreeDiagram& treeDiagramOne, const TreeDiagram& treeDiagramTwo, const int minimal_search=0) {

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

        int numberLeavesTreeDiagramOne = treeDiagramOne.getNumberLeaves();
        int numberLeavesTreeDiagramTwo = treeDiagramTwo.getNumberLeaves();
        int maxLength;

        if (minimal_search == 0){

            maxLength = find_longest_string_length(treePlus, treeMinus, treePlusPrime, treeMinusPrime);

        }
        else{
            maxLength = max(numberLeavesTreeDiagramOne, numberLeavesTreeDiagramTwo)-1;
        }
        
        common_tree = generateCompleteBinaryTree(maxLength);

        for (int i=0; i<numberLeavesTreeDiagramOne; ++i){
            if(treeMinus[i].length()<maxLength){
                tree_complete_temp = generateCompleteBinaryTree(maxLength-(treeMinus[i].length()));
                for (int j=0; j<tree_complete_temp.size(); ++j){
                    treePlusNew.push_back(treePlus[i]+tree_complete_temp[j]);
                }
            } else {
                treePlusNew.push_back(treePlus[i]);
            }
            
        }

        for (int i=0; i<numberLeavesTreeDiagramTwo; ++i){
            if(treePlusPrime[i].length()<maxLength){
                tree_complete_temp = generateCompleteBinaryTree(maxLength-(treePlusPrime[i].length()));
                for (int j=0; j<tree_complete_temp.size(); ++j){
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
The function 'multiplicationTreeDiagrams' implements the multiplication of two elements of F.
The inputs are two TreeDiagrams: treeDiagramOne, treeDiagramTwo.
The output is their product: treeDiagramProd.
The variable 'minimal_search' if equal to 0 means that we look for the minimal common tree when we use the function 
'findCommonTree'.
*/
TreeDiagram multiplicationTreeDiagrams(const TreeDiagram& treeDiagramOne, const TreeDiagram& treeDiagramTwo, int minimal_search = 0) {
    if (treeDiagramOne.getNumberLeaves() == 1) {
        return treeDiagramTwo;
    } else if (treeDiagramTwo.getNumberLeaves() == 1) {
        return treeDiagramOne;
    } else if (treeDiagramOne.getTop() == treeDiagramTwo.getBottom() && treeDiagramOne.getBottom() == treeDiagramTwo.getTop()) {
        return TreeDiagram(); // Default constructor
    } else {

        vector<string> treePlus = treeDiagramOne.getTop();
        vector<string> treeMinus = treeDiagramOne.getBottom();
        vector<string> treePlusPrime = treeDiagramTwo.getTop();
        vector<string> treeMinusPrime = treeDiagramTwo.getBottom();

        auto tupleContainingUnreducedTreeDiagrams = findCommonTree(treeDiagramOne, treeDiagramTwo, minimal_search);

        TreeDiagram treeDiagramOnePrime = tupleContainingUnreducedTreeDiagrams.first;
        TreeDiagram treeDiagramTwoPrime = tupleContainingUnreducedTreeDiagrams.second;

        vector<string> prodTreePlusTemp = treeDiagramOnePrime.getTop();
        vector<string> prodTreeMinusTemp = treeDiagramTwoPrime.getBottom();

        if (prodTreeMinusTemp.size()<1) {
            cout << "DANGER" << endl;
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
The function 'multiplyManyTreeDiagrams' has as input a collection of TreeDiagrams, collectionOfTreeDiagram.
The output is their product.
The variable 'minimal_search' if equal to 0 means that we look for the minimal common tree when we use the function 
'findCommonTree'.
*/
TreeDiagram multiplyManyTreeDiagrams(const vector<TreeDiagram>& collectionOfTreeDiagram, const int minimal_search = 0) {
    size_t numberOfElements = collectionOfTreeDiagram.size();
    TreeDiagram product;


    if (numberOfElements == 0) {
        return product;
    } else if (numberOfElements == 1) {
        return collectionOfTreeDiagram[0];
    } else {
        for (size_t i = 0; i < numberOfElements; ++i) {
            product = multiplicationTreeDiagrams(product, collectionOfTreeDiagram[i], minimal_search);
        }
        return product;
    }
}

/* 
The function 'generateSequences' generates all sequences (with repetitions) of length d
from the vector {0, ..., n-1}
*/ 
vector<vector<int>> generateSequences(int n, int d) {
    // Create a vector containing elements from 0 to n-1
    vector<int> elements;
    for (int i = 0; i < n; ++i) {
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



/* 
The function 'find_nonzero_indices' finds the nonzero indices in a vector.
*/
vector<int> find_nonzero_indices(const vector<int>& array) {
    vector<int> nonzero_indices;
    for (int i = 0; i < array.size(); ++i) {
        if (array[i] != 0) {
            nonzero_indices.push_back(i);
        }
    }
    return nonzero_indices;
}

/*
The function 'find_string_in_array' takes a vector of strings 'array_of_strings' and a string 'input_string'
as inputs and returns a pair as an output. The first entry of the output is boolean variable,
with value true if the string was found, false otherwise, 
the second entry is the index of the vector where the string was found.
*/
pair<bool, int> find_string_in_array(const vector<string>& array_of_strings, const string& input_string) {
    for (int i = 0; i < array_of_strings.size(); ++i) {
        if (array_of_strings[i] == input_string) {
        // Returns a pair with the flag true (the string is found) and the corresponding index.
            return make_pair(true, i);
        }
    }
    // If the string is not found, returns a pair with the flag false and a non-significant index.
    return make_pair(false, -1);
}

/*
This function takes a binary word 'input_string' and "cleans" it by removing 
all the zeroes at the end. 
If the word consists only of '0's, it returns the string "0".
*/
string remove_trailing_zeroes(const string& input_string) {
    // Find the position of the last non-zero character
    size_t last_non_zero_pos = input_string.find_last_not_of('0');
    
    // If no non-zero character found, return an empty string
    if (last_non_zero_pos == std::string::npos)
        return "0";

    // Extract the substring containing non-zero characters
    return input_string.substr(0, last_non_zero_pos + 1);
}


/*
The function 'action_on_dyadic_rational' takes a TreeDiagram and a dyadic rational 'input_string'
(expressed as a binary word, a string) as inputs and returns the binary word 
representing the output
*/
string action_on_dyadic_rational(const TreeDiagram& group_element, std::string& input_string){
    // 'longest_word_in_group_element' contains the length of the longest word in the       
    // top and bottom trees    
    int longest_word_in_group_element = find_longest_string_length(group_element.getTop(), group_element.getBottom(), vector<string>{"0"}, vector<string>{"0"});

    vector<string> top_tree = group_element.getTop();
    vector<string> bottom_tree = group_element.getBottom();

    // 'zero_pad' will host the additional zeroes to add at the end of 'input_string'
    string zero_pad;

    // 'output_string' contains the binary word that represents the image of 'input_string'
    // under the action of 'group_element'
    string output_string;

    // 'group_element_expanded' will host another representative of 'group_element', used in case 
    // the length of 'input_string' is longer that the length of the longest word in
    // the top/bottom trees of 'group_element' is shorter than the length of 'input_string'
    TreeDiagram group_element_expanded;

    // 'identity_element' is will host a non-reduced representative of the neutral element of the group.
    // Together with the function 'findCommonTree' will allow us to find a representative of
    // 'group_element' where its top tree contains 'input_string'
    TreeDiagram identity_element;

    // The variable 'length_difference' contains the difference between the length of the longest word
    // in the top/bottom trees of 'group_element' and 'input_string'
    int length_difference;
    input_string = remove_trailing_zeroes(input_string);

    if (input_string.length() <= longest_word_in_group_element){

        length_difference = (longest_word_in_group_element) - input_string.length();

        zero_pad = string(length_difference, '0');

        input_string = input_string + zero_pad;
        identity_element = TreeDiagram(generateCompleteBinaryTree(longest_word_in_group_element), generateCompleteBinaryTree(longest_word_in_group_element));
        group_element_expanded = findCommonTree(identity_element, group_element).second;

    }
    else {

        int length_word_in_tree;

        length_difference = input_string.length() - longest_word_in_group_element;

        identity_element = TreeDiagram(generateCompleteBinaryTree(input_string.length()), generateCompleteBinaryTree(input_string.length()));
        group_element_expanded = findCommonTree(identity_element, group_element).second;

    }

    pair<bool, int> result = find_string_in_array(group_element_expanded.getTop(), input_string);
    if (result.first){
        output_string = remove_trailing_zeroes(group_element_expanded.getBottom()[result.second]);
    }

    return output_string;
}


/*
The function 'is_in_parabolic_subgroup' checks if a given element of F, treeDiagram, is in the oriented subgroup or not.
*/
bool is_in_parabolic_subgroup(const TreeDiagram& treeDiagram, string dyadic_rational) {
    dyadic_rational = remove_trailing_zeroes(dyadic_rational);
    string image_of_dyadic_rational = action_on_dyadic_rational(treeDiagram, dyadic_rational);

    if (remove_trailing_zeroes(image_of_dyadic_rational) == remove_trailing_zeroes(dyadic_rational)) {
        return true;
    }
    
    return false;
}


/*
The function 'find_normal_form' finds the normal form of a binary tree diagram.
The input is a TreeDiagram. The output are two arrays of integers.
The first contains exponents of the positive part of the normal form,
the second that of the negative part, for example {{0, 1, 0}, {1, 0, 0}}
means that the element has normal form x_1 x_0^{-1}.
*/ 
pair<vector<int>, vector<int>> find_normal_form(TreeDiagram tree_diagram) {
    int length = tree_diagram.getNumberLeaves();
    vector<int> positive_part(length, 0);
    vector<int> negative_part(length, 0);

    auto top_tree = tree_diagram.getTop();
    auto bottom_tree = tree_diagram.getBottom();

    positive_part[0] = top_tree[0].size() - 1;
    negative_part[0] = bottom_tree[0].size() - 1;

    size_t last_digit_1_position;
    size_t the_last_digit_0_before_last_digit_1;

    // Iterate over each level of the tree
    positive_part[0] = top_tree[0].length()-1;
    negative_part[0] = bottom_tree[0].length()-1;
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
                negative_part[i] = bottom_tree[i].length() - bottom_tree[i].substr(0, last_digit_1_position + 1).length();
            }
            else{
                negative_part[i] = bottom_tree[i].length() - bottom_tree[i].substr(0, last_digit_1_position + 1).length() - 1;

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
        positive_part.resize(1);
        negative_part.resize(1);
    }

    return make_pair(positive_part, negative_part);
}

/*
The function 'print_arrays' takes a pair of arrays as input and prints them.
*/
void print_arrays(const pair<vector<int>, vector<int>>& arrays) {
    cout << "First array:" << endl;
    for (int i : arrays.first) {
        cout << i << " ";
    }
    cout << endl;

    cout << "Second array:" << endl;
    for (int i : arrays.second) {
        cout << i << " ";
    }
    cout << endl;
}


/* 
The function 'unnormalized_chromatic_moment' calculates the unnormalized mmoments, that is
(s_{sequence_index})^(exponent_power) * (sqrt{exponent_power*sequence_index})^exponent_power.
The parameter 'minimal_search' if set equal to 0 means that the function 'findCommonTree'
used in 'multiplyManyTreeDiagrams'



versione parallellizata 

int unnormalized_chromatic_moment(int sequence_index, int exponent_power, int minimal_search = 0) {
    auto start = std::chrono::high_resolution_clock::now();

    int sum = 0;
    #pragma omp parallel for num_threads(4) reduction(+:sum)
    for (int i = 0; i < sequence_index; ++i) {
        if (exponent_power == 0 || exponent_power % 2 == 1) continue;

        int local_sum = 0;
        std::vector<TreeDiagram> listOfElements;
        TreeDiagram x_0 = TreeDiagram::createX0();
        listOfElements.push_back(x_0);
        listOfElements.push_back(x_0.inverseTreeDiagram());

        for (int j = 1; j < sequence_index; ++j) {
            listOfElements.push_back(TreeDiagram::rightShiftHomomorphism(listOfElements.back()));
            listOfElements.push_back(listOfElements.back().inverseTreeDiagram());
        }

        std::vector<std::vector<int>> collection_of_indices_of_summands = generateSequences(listOfElements.size(), exponent_power);
        TreeDiagram summand;

        for (int j = 0; j < collection_of_indices_of_summands.size(); ++j) {
            std::vector<TreeDiagram> factors_in_summand;

            for (int k = 0; k < exponent_power; ++k) {
                factors_in_summand.push_back(listOfElements[collection_of_indices_of_summands[j][k]]);
            }

            summand = multiplyManyTreeDiagrams(factors_in_summand, minimal_search);

            if (is_in_oriented_subgroup(summand)) {
                local_sum += 1;
            }

            summand = TreeDiagram({"0"}, {"0"});
        }

        #pragma omp critical
        sum += local_sum;
    }

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    std::cout << "Tempo trascorso: " << elapsed.count() << " secondi" << std::endl;

    return sum;
}


int unnormalized_chromatic_moment(int sequence_index, int exponent_power, int minimal_search = 0) {

    auto start = std::chrono::high_resolution_clock::now();

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
    //    #pragma omp parallel for reduction(+:sum) 
        #pragma omp parallel for num_threads(4) reduction(+:sum)
        for (int i = 0; i < collection_of_indices_of_summands.size(); ++i) {
            vector<TreeDiagram> factors_in_summand;

            // Create factors for the current summand
            for (int j = 0; j < exponent_power; ++j) {
                factors_in_summand.push_back(listOfElements[collection_of_indices_of_summands[i][j]]);
            } 
            // Multiply the factors to obtain the current summand
            summand = multiplyManyTreeDiagrams(factors_in_summand, minimal_search);

            // Check if the summand is in the oriented subgroup
            if (is_in_oriented_subgroup(summand)) {
                sum += 1;
            }
            summand = TreeDiagram({"0"}, {"0"}); // Reset summand for the next iteration
        } 
        // Fine del conteggio del tempo
        auto end = std::chrono::high_resolution_clock::now();

        // Calcola la differenza di tempo
        std::chrono::duration<double> elapsed = end - start;

        // Stampare il tempo trascorso
        std::cout << "Tempo trascorso: " << elapsed.count() << " secondi" << std::endl;

        return sum;
    }

    return 0; // Default case
}
*/
int unnormalized_chromatic_moment(int sequence_index, int exponent_power, int minimal_search = 0) {

    auto start = std::chrono::high_resolution_clock::now();

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
        for (int i = 0; i < collection_of_indices_of_summands.size(); ++i) {
            vector<TreeDiagram> factors_in_summand;

            // Create factors for the current summand
            for (int j = 0; j < exponent_power; ++j) {
                factors_in_summand.push_back(listOfElements[collection_of_indices_of_summands[i][j]]);
            } 
            // Multiply the factors to obtain the current summand
            summand = multiplyManyTreeDiagrams(factors_in_summand, minimal_search);

            // Check if the summand is in the oriented subgroup
            if (is_in_oriented_subgroup(summand)) {
                sum += 1;
                cout << "NEW ELEMENT" << endl; 
                print_arrays(find_normal_form(summand));
            }
            summand = TreeDiagram({"0"}, {"0"}); // Reset summand for the next iteration
        } 
        // Fine del conteggio del tempo
        auto end = std::chrono::high_resolution_clock::now();

        // Calcola la differenza di tempo
        std::chrono::duration<double> elapsed = end - start;

        // Stampare il tempo trascorso
        std::cout << "Tempo trascorso: " << elapsed.count() << " secondi" << std::endl;

        return sum;
    }

    return 0; // Default case
}



/* 
The function 'parabolic_moment' calculates the mmoments, that is the value of the
matrix coefficient in the quasi-regular representation corresponding to the vector 
of the identity for (s_{sequence_index})^(exponent_power).
'dyadic_rational' is a binary word representing the dyadic rational fixed by the parabolic subgroup.
*/
int unnormalized_parabolic_moment(int sequence_index, int exponent_power, string dyadic_rational, int minimal_search = 0) {

    // We remove the zeroes at the end of 'dyadic_rational' and find the shortest word representing it
    dyadic_rational = remove_trailing_zeroes(dyadic_rational);

    if (exponent_power > 0) {
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
        for (int i = 0; i < collection_of_indices_of_summands.size(); ++i) {
            vector<TreeDiagram> factors_in_summand;

            // Create factors for the current summand
            for (int j = 0; j < exponent_power; ++j) {
                factors_in_summand.push_back(listOfElements[collection_of_indices_of_summands[i][j]]);
            } 
            // Multiply the factors to obtain the current summand
            summand = multiplyManyTreeDiagrams(factors_in_summand, minimal_search);

            // Check if the summand is in the oriented subgroup
            if (is_in_parabolic_subgroup(summand, dyadic_rational)) {
                sum += 1;
            }
            summand = TreeDiagram({"0"}, {"0"}); // Reset summand for the next iteration
        } 

        return sum;
    }

    return 0; // Default case
}

int main() {


    int d = 4;
    int n = 4;
//    cout << "CIAO \n";
//    for (int n=1; n<5; ++n){
//        cout << unnormalized_chromatic_moment(n,d, 1) << ", ";
//    }
    cout << unnormalized_chromatic_moment(n,d, 0) << ", \n";
//    cout << unnormalized_chromatic_moment(n,d, 1) << ", ";

    cout << endl;
    return 0;
}

//clang++ -std=c++11 -g -fopenmp -o c_group_operations_copy c_group_operations_copy.cpp