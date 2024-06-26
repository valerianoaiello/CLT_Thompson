"""
GROUP OPERATIONS IN THE BROWN-THOMPSON GROUP F


In this file we implement the structure of the Thompson group F.
Each element of F is described by a pair of vectors, the first for the top tree, 
the second for the bottom tree, whose components are string (each string is a word in '0' and '1' 
representing a leaf, the word describes the shortest path from the root of tree to the leaf, 
'0'= left edge, '1'= right edge).

The class 'TreeDiagram' allows the user to define elements of F, take their inverses 'inverse_tree_diagram', 
get the number of leaves 'get_number_leaves', print them 'print_tree_diagram',
create the generator x_0 'create_x_0'. 

There are functions that implement the right shift homomorphism 'right_shift_homomorphism',
the flip automorphism 'flip_automorphism', that apply a reduction move on a tree diagram
'reduceTreeDiagram', find the normal form of an element 'find_normal_form'. 

The function 'multiplicationTreeDiagrams' implements the multiplication in the group.
The function 'unnormalized_moment' computes the unnormalized moments of the chromatic polynomial evaluated at 2.
"""
import copy
import numpy as np
from itertools import product
import time
import numpy as np
import multiprocessing

#import sys
#sys.path.append('/Users/valerianoaiello/Documents/GitHub/CLT_Thompson/src/')
#sys.path.append('/Users/valerianoaiello/Documents/GitHub/Thompson-knot-theory/src/')
#from generators_F import *


class Tree_diagram:
  """
  Description of Tree Diagrams
  Binary tree diagrams are represented using pairs of binary words.

  We describe ternary tree diagrams with pairs of ternary words. 

  Method: inverse_tree_diagram
  The inverse_tree_diagram method computes the inverse of an element in F. The inversion is achieved by simply exchanging the trees in the ternary tree diagram.

  Method: get_number_leaves
  The get_number_leaves method returns the count of leaves for each ternary tree in the tree diagram.

  Method: inverse
  The inverse method calculates the inverse of an element in F.
  """
  def __init__(self, top_tree = ["0"], bottom_tree = ["0"]):
      self.top = top_tree
      self.bottom = bottom_tree
  def get_number_leaves(self):
    return len(self.top)
  def inverse_tree_diagram(self):
    return Tree_diagram(self.bottom, self.top)
  def print_tree_diagram(self):
    print("top tree", self.top)
    print("bottom tree", self.bottom)
  @classmethod
  def create_x_0(self):
    # Create and return an instance representing the generator x_0. 
    return Tree_diagram(['00', '01', '1'], ['0', '10', '11'])



def right_shift_homomorphism(tree_diagram: Tree_diagram) -> Tree_diagram:
  """
  Given a binary tree diagram represented as a pair (tree_plus, tree_minus), 
  this function returns the image under the right shift homomorphism.
  """
  tree_plus_deep_cp = copy.deepcopy(tree_diagram.top)
  tree_minus_deep_cp = copy.deepcopy(tree_diagram.bottom) 

  for i in range(len(tree_plus_deep_cp)):
      tree_plus_deep_cp[i] = "1"+ tree_plus_deep_cp[i] 
      tree_minus_deep_cp[i] = "1"+ tree_minus_deep_cp[i] 
  tree_plus_deep_cp.insert(0, '0')
  tree_minus_deep_cp.insert(0, '0')
  shifted_tree_diagram = Tree_diagram(tree_plus_deep_cp, tree_minus_deep_cp)
  return shifted_tree_diagram




def flip_automorphism(tree_diagram: Tree_diagram) -> Tree_diagram:
  """
  This represents the flip automorphism on F. This is an automorphism of order 2, 
  and at the level of tree diagrams, it is achieved by reflecting the trees about a vertical line.
  """  
  tree_plus_deep_cp = copy.deepcopy(tree_diagram.top)
  tree_minus_deep_cp = copy.deepcopy(tree_diagram.bottom) 

  for i in range(len(tree_plus_deep_cp)):
      for j in range(len(tree_plus_deep_cp[i])):
        if tree_plus_deep_cp[i][j] == '0':
          tree_plus_deep_cp[i] = tree_plus_deep_cp[i][:j] + '1' + tree_plus_deep_cp[i][j+1:]
        elif tree_plus_deep_cp[i][j] == '1':
          tree_plus_deep_cp[i] = tree_plus_deep_cp[i][:j] + '0' + tree_plus_deep_cp[i][j+1:]
      for j in range(len(tree_minus_deep_cp[i])):
        if tree_minus_deep_cp[i][j] == '0':
          tree_minus_deep_cp[i] = tree_minus_deep_cp[i][:j] + '1' + tree_minus_deep_cp[i][j+1:]
        elif tree_minus_deep_cp[i][j] == '1':
          tree_minus_deep_cp[i] = tree_minus_deep_cp[i][:j] + '0' + tree_minus_deep_cp[i][j+1:]
  tree_plus_deep_cp.sort()
  tree_minus_deep_cp.sort()
  return Tree_diagram(tree_plus_deep_cp, tree_minus_deep_cp)

def generate_complete_binary_tree(height: int) -> list:
  """
  This function returns the words representing a complete binary tree, producing the labels of its leaves 
  (which are words composed of the letters {0, 1}). The height of the tree must be at least 1.
  """
  if height == 0:
    return [""]
  elif height == 1:
    return ["0", "1"]
  else:
    return ["0" + i for i in generate_complete_binary_tree(height-1)]+ ["1" + i for i in generate_complete_binary_tree(height-1)]

def flatten_list(outer_list: list) -> list:
  """
  This function flattens a list. It is utilized in the find_common_tree function.
  """
  flat_list = []
  for element in outer_list:
    if type(element) is list:
        for item in element:
            flat_list.append(item)
    else:
        flat_list.append(element)
  return flat_list
 
def find_common_tree(tree_diagram_one: Tree_diagram, tree_diagram_two: Tree_diagram):
  """
  This function takes two binary tree diagrams as input, represented by pairs (tree_plus, tree_minus) 
  and (tree_plus_prime, tree_minus_prime). It finds a representative such that the second tree 
  of the first tree diagram is equal to the first tree of the second tree diagram. 
  This facilitates the multiplication of the elements.
  """

  tree_plus = copy.deepcopy(tree_diagram_one.top)
  tree_minus = copy.deepcopy(tree_diagram_one.bottom)
  tree_plus_prime = copy.deepcopy(tree_diagram_two.top)
  tree_minus_prime = copy.deepcopy(tree_diagram_two.bottom)

  if tree_minus == tree_plus_prime:
    return tree_plus, tree_minus, tree_plus_prime, tree_minus_prime
  else:
    max_length = max(len(max(tree_minus, key=len)), len(max(tree_plus_prime, key=len)))
    for i in range(len(tree_plus)):
      if len(tree_minus[i])<max_length:
          temp = tree_plus[i]
          temp_completion = generate_complete_binary_tree(max_length-len(tree_minus[i]))
          tree_plus[i]= [temp + word for word in temp_completion] 
    for i in range(len(tree_minus_prime)):
      if len(tree_plus_prime[i])<max_length:
          temp_2 = tree_minus_prime[i]
          temp_completion_2 = generate_complete_binary_tree(max_length-len(tree_plus_prime[i]))
          tree_minus_prime[i]= [temp_2 + word for word in temp_completion_2]
  tree_minus = generate_complete_binary_tree(max_length)    
  tree_plus_prime = generate_complete_binary_tree(max_length)    

  tree_plus = flatten_list(tree_plus)
  tree_minus_prime = flatten_list(tree_minus_prime)
  tree_diagram_one_mod = Tree_diagram(tree_plus, tree_minus)
  tree_diagram_two_mod = Tree_diagram(tree_plus_prime, tree_minus_prime)

  return tree_diagram_one_mod, tree_diagram_two_mod




def find_normal_form(tree_diagram: Tree_diagram) -> list:
  """
  The following function takes a binary tree diagram and returns its normal form in terms of two numpy arrays: 
  one for the positive part and one for the negative part.
  The positive part is described by a vector of the form [a_0, ..., a_n], 
  and the negative part is described by a vector of the form [b_0, ..., b_n]. 
  Here, a_0, ..., a_n, b_0, ..., b_n are all non-negative integers. 
  """
  length = tree_diagram.get_number_leaves()
  positive_part = np.zeros(length)
  negative_part = np.zeros(length)

  top_tree = tree_diagram.top
  bottom_tree = tree_diagram.bottom

  positive_part[0] = len(top_tree[0])-1
  negative_part[0] = len(bottom_tree[0])-1

  for i in range(1, length):
    if top_tree[i][-1] == '0' and top_tree[i][0] == '1':
      positive_part[i] = len(top_tree[i])-len(top_tree[i].rstrip('0')) -1
    elif top_tree[i][-1] == '0':
      positive_part[i] = len(top_tree[i])-len(top_tree[i].rstrip('0'))
    if bottom_tree[i][-1] == '0' and bottom_tree[i][0] == '1':
      negative_part[i] = len(bottom_tree[i])-len(bottom_tree[i].rstrip('0'))-1
    elif bottom_tree[i][-1] == '0':
      negative_part[i] = len(bottom_tree[i])-len(bottom_tree[i].rstrip('0'))
  
  nonzero_indices_positive_part = np.nonzero(positive_part)[-1]
  nonzero_indices_negative_part = np.nonzero(negative_part)[-1] 
  if len(nonzero_indices_positive_part)>0:
    length_positive_part = nonzero_indices_positive_part[-1] + 1
  else:
    length_positive_part = 0

  if len(nonzero_indices_negative_part)>0:
    length_negative_part = nonzero_indices_negative_part[-1] + 1
  else:
    length_negative_part = 0

  if length_positive_part + length_negative_part >0:
    max_length = max(length_positive_part, length_negative_part)
    positive_part = positive_part[:max_length + 1]
    negative_part = negative_part[:max_length + 1]
  else:   
    positive_part = np.array([])
    negative_part = np.array([])

  return positive_part, negative_part


def generate_complete_binary_tree(height: int) -> list:
  """
  This function returns the words representing a complete binary tree, 
  producing the labels of its leaves (these are words composed of the letters {0, 1}). 
  The height of the tree must be at least 0.
  """
  if height == 0:
    return [""]
  elif height == 1:
    return ["0", "1"]
  else:
    return ["0" + i for i in generate_complete_binary_tree(height-1)]+ ["1" + i for i in generate_complete_binary_tree(height-1)]


def reduce_tree_diagram(tree_diagram: Tree_diagram) -> Tree_diagram:
  """
  tree_plus and tree_minus are lists containing binary words, with digits in {0, 1}, each representing a ternary tree. 
  Both trees have the same number of leaves. The function reduce_tree_diagram takes two binary trees and applies 
  a reduction move if possible. The function returns a pair of binary tree diagrams.
  """  
  tree_plus = copy.deepcopy(tree_diagram.top)
  tree_minus = copy.deepcopy(tree_diagram.bottom)

  for i in range(len(tree_plus)-2,-1,-1):
    if i+1<len(tree_plus) and tree_plus[i][:-1] == tree_plus[i+1][:-1] and (tree_plus[i][-1] == tree_plus[i][-1] == "0") and (tree_plus[i+1][-1] == tree_plus[i+1][-1] == "1") and tree_minus[i][:-1] == tree_minus[i+1][:-1] and (tree_minus[i][-1] == tree_minus[i][-1] == "0") and (tree_minus[i+1][-1] == tree_minus[i+1][-1] == "1"):
        tree_plus[i] = tree_plus[i][:-1]
        tree_minus[i] = tree_minus[i][:-1]
        del tree_plus[i+1]
        del tree_minus[i+1]
  tree_diagram_reduced =  Tree_diagram(tree_plus, tree_minus)
  return tree_diagram_reduced

def find_common_tree(tree_diagram_one: Tree_diagram, tree_diagram_two: Tree_diagram) -> list:
  """
  This function takes two binary tree diagrams as input, represented by pairs (tree_plus, tree_minus) 
  and (tree_plus_prime, tree_minus_prime). It finds a representative such that the second tree of the 
  first tree diagram is equal to the first tree of the second tree diagram. This facilitates the multiplication of the elements.
  """  
  tree_plus = copy.deepcopy(tree_diagram_one.top)
  tree_minus = copy.deepcopy(tree_diagram_one.bottom)
  tree_plus_prime = copy.deepcopy(tree_diagram_two.top)
  tree_minus_prime = copy.deepcopy(tree_diagram_two.bottom)

  if tree_minus == tree_plus_prime:
    return tree_diagram_one, tree_diagram_two
  else:
    max_length = max(len(max(tree_minus, key=len)), len(max(tree_plus_prime, key=len)))
    for i in range(len(tree_plus)):
      if len(tree_minus[i])<max_length:
          temp = tree_plus[i]
          temp_completion = generate_complete_binary_tree(max_length-len(tree_minus[i]))
          tree_plus[i]= [temp + word for word in temp_completion] 
    for i in range(len(tree_minus_prime)):
      if len(tree_plus_prime[i])<max_length:
          temp_2 = tree_minus_prime[i]
          temp_completion_2 = generate_complete_binary_tree(max_length-len(tree_plus_prime[i]))
          tree_minus_prime[i]= [temp_2 + word for word in temp_completion_2]
  tree_minus = generate_complete_binary_tree(max_length)    
  tree_plus_prime = generate_complete_binary_tree(max_length)    

  tree_plus = flatten_list(tree_plus)
  tree_minus_prime = flatten_list(tree_minus_prime)
  tree_diagram_one_mod = Tree_diagram(tree_plus, tree_minus)
  tree_diagram_two_mod = Tree_diagram(tree_plus_prime, tree_minus_prime)
  return tree_diagram_one_mod, tree_diagram_two_mod

def multiplication_tree_diagrams(tree_diagram_one: Tree_diagram, tree_diagram_two: Tree_diagram) -> Tree_diagram:
  """
  The function takes two tree diagrams, described by two pairs of ternary words, 
  and finds a representative of their product (prod_tree_plus_temp, prod_tree_minus_temp). 
  This representative is then transformed into its reduced form using the function reduce_tree_diagram in a do-while loop.
  """
  if tree_diagram_one.get_number_leaves() == 1:
    return tree_diagram_two
  elif tree_diagram_two.get_number_leaves() == 1:
    return tree_diagram_one
  elif tree_diagram_one.top == tree_diagram_two.bottom and tree_diagram_one.bottom == tree_diagram_two.top:
    return Tree_diagram()
  else:
    tree_plus = copy.deepcopy(tree_diagram_one.top)
    tree_minus = copy.deepcopy(tree_diagram_one.bottom)
    tree_plus_prime = copy.deepcopy(tree_diagram_two.top)
    tree_minus_prime = copy.deepcopy(tree_diagram_two.bottom)

    tuple_containing_unreduced_tree_diagrams  = find_common_tree(Tree_diagram(tree_plus, tree_minus), Tree_diagram(tree_plus_prime, tree_minus_prime)) 
    tree_diagram_one_prime = tuple_containing_unreduced_tree_diagrams[0]
    tree_diagram_two_prime = tuple_containing_unreduced_tree_diagrams[1]
    prod_tree_plus_temp = copy.deepcopy(tree_diagram_one_prime.top) 
    prod_tree_minus_temp = copy.deepcopy(tree_diagram_two_prime.bottom)

    tree_diagram_prod = Tree_diagram(prod_tree_plus_temp, prod_tree_minus_temp)
    while True:
        lenght_tree_plus = len(tree_diagram_prod.top)
        tree_diagram_prod = reduce_tree_diagram(tree_diagram_prod)
        if lenght_tree_plus == len(tree_diagram_prod.top):
            break
  return tree_diagram_prod

def multiply_many_tree_diagrams(collection_of_tree_diagram: list) -> Tree_diagram:
  """
  This function multiplies several elements of F. The input is a list whose elements are binary tree diagrams.
  """
  number_of_elements = len(collection_of_tree_diagram)
  product = Tree_diagram()
  if number_of_elements ==0:
    return product
  elif number_of_elements == 1:
    return collection_of_tree_diagram[0]
  else:
    for i in range(len(collection_of_tree_diagram)):
      product = multiplication_tree_diagrams(product, collection_of_tree_diagram[i])
    return product
  return product  

def power_tree_diagram(tree_diagram: Tree_diagram, exponent: int) -> Tree_diagram:
  """
  This function calculates the power of an element in F. 
  The element is stored in the variable tree_diagram, while exponent is an integer.
  """
  result = Tree_diagram()
  if exponent == 0:
    return result
  elif exponent >= 0:
    result = multiplication_tree_diagrams(tree_diagram, power_tree_diagram(tree_diagram, exponent-1)) 
    return result
  elif exponent < 0:
    result = multiplication_tree_diagrams(tree_diagram, power_tree_diagram(tree_diagram, exponent+1)) 
    return result
  return result

def alternating_sum(word: str) -> int:
  """
  This function takes a binary word a_0a_1...a_n, that is a string in '0' and '1', and computes the alternating sum 
  -a_0+a_1-a_2+a_3...
  """
  sum = 0
  for i in range(len(word)):
    if i%2 == 0:
      sum += int(word[i])
    else:
      sum -= int(word[i])
  return sum

def is_in_oriented_subgroup(tree_diagram: Tree_diagram) -> bool:
  """
  This function checks if the element of F, a Tree_diagram, is in the oriented subgroup. 
  If it is, it returns True, otherwise it returns False.
  """  
  for i in range(len(tree_diagram.top)):
    word_plus = tree_diagram.top[i]
    word_minus = tree_diagram.bottom[i]
    if alternating_sum(word_plus) % 2 != alternating_sum(word_minus) % 2:
      return False
  return True

def generate_sequences(list1, d):
    """
    This function generates all the possible sequences of length 'd' of the elements of 'list1'.
    """
    result = []

    for combination in product(list1, repeat=d):
        result.append(list(combination))

    return result 

def moment(sequence_index: int, exponent_power: int):
  """
  The function 'unnormalized_moment' calculates the unnormalized mmoments, that is
  (s_{sequence_index})^(exponent_power) * (sqrt{exponent_power*sequence_index})^exponent_power
  """
  if exponent_power == 0:  
    return 1
  elif exponent_power%2 == 1:
    return 0 
  elif exponent_power > 0:
    # we create all the elements that we need
    x_0 = Tree_diagram.create_x_0()
    list_of_elements = [x_0]
    for i in range(1, sequence_index):
      list_of_elements.append(right_shift_homomorphism(list_of_elements[-1]))
    
    # Use map to apply the inverse function to each element
    inverse_elements = list(map(lambda x: x.inverse_tree_diagram(), list_of_elements))

    # This list contains all possibile sequences of length 'exponent_power' of the elements of 'list_of_elements' and 'inverse_elements'
    summands = generate_sequences(list_of_elements + inverse_elements, exponent_power)
    sum = 0


    # Calculate the elapsed time
    elapsed_time = 0
    for i in range(len(summands)):
      start_time = time.time()
      item_of_summand = multiply_many_tree_diagrams(summands[i])
      end_time = time.time()

      if is_in_oriented_subgroup(item_of_summand):
        # Record end time
        elapsed_time += end_time - start_time
        sum += 1 
    print("elapsed time: ", elapsed_time)
    return sum
  

def unnormalized_moment_parallel(sequence_index: int, exponent_power: int):
  """
  The function 'unnormalized_moment_parallel' calculates the unnormalized mmoments in a parallel way.
  """
  if exponent_power == 0:  
    return 1
  elif exponent_power%2 == 1:
    return 0 
  elif exponent_power > 0:
    # we create all the elements that we need
    x_0 = Tree_diagram.create_x_0()
    list_of_elements = [x_0]
    for i in range(1, sequence_index):
      list_of_elements.append(right_shift_homomorphism(list_of_elements[-1]))
    
    # Use map to apply the inverse function to each element
    inverse_elements = list(map(lambda x: x.inverse_tree_diagram(), list_of_elements))

    # This list contains all possibile sequences of length 'exponent_power' of the elements of 'list_of_elements' and 'inverse_elements'
    summands = generate_sequences(list_of_elements + inverse_elements, exponent_power)

    ### BEGINNING OF PARALLEL COMPUTATIONS
    num_cores = multiprocessing.cpu_count()
    
    # Create a multiprocessing pool
    pool = multiprocessing.Pool(processes=num_cores)
    
    # Map the square function to each number in parallel
    results = pool.map(check_if_oriented, summands)

    sum = np.sum(results)


    return sum
  
"""
The function 'check_if_oriented' is used in 'unnormalized_moment_parallel'. 
It takes a list as an input, the list contains elements of F, 
it calculates their product and returns 1 if the element is in the oriented subgroup,
0 otherwise.
"""
def check_if_oriented(array: list):
  sum = 0
  for i in range(len(array)):
    product_of_elements = multiply_many_tree_diagrams(array)

    if is_in_oriented_subgroup(product_of_elements):
      # Record end time
      sum = 1 
  return sum

if __name__ == '__main__':
  n = 3
  d = 8
  print(unnormalized_moment_parallel(n,d))
#  print(find_normal_form(multiplication_tree_diagrams(right_shift_homomorphism(Tree_diagram.create_x_0()), Tree_diagram.create_x_0())))
