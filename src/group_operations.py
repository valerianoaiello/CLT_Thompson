"""
GROUP OPERATIONS IN THE BROWN-THOMPSON GROUP F
"""

import copy
import numpy as np




import sys
sys.path.append('/Users/valerianoaiello/Documents/GitHub/CLT_Thompson/src/')
#sys.path.append('/Users/valerianoaiello/Documents/GitHub/Thompson-knot-theory/src/')
from generators_F import *


"""
Description of Binary Tree Diagrams
Binary tree diagrams are represented using pairs of binary words.

We describe ternary tree diagrams with pairs of ternary words. 

Method: inverse_tree_diagram
The inverse_tree_diagram method computes the inverse of an element in F. The inversion is achieved by simply exchanging the trees in the ternary tree diagram.

Method: get_number_leaves
The get_number_leaves method returns the count of leaves for each ternary tree in the tree diagram.

Method: inverse
The inverse method calculates the inverse of an element in F.
"""
class Tree_diagram:
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



"""
Given a binary tree diagram represented as a pair (tree_plus, tree_minus), 
this function returns the image under the right shift homomorphism.
"""
def right_shift_homomorphism(tree_diagram: Tree_diagram) -> Tree_diagram:
  tree_plus_deep_cp = copy.deepcopy(tree_diagram.top)
  tree_minus_deep_cp = copy.deepcopy(tree_diagram.bottom) 

  for i in range(len(tree_plus_deep_cp)):
      tree_plus_deep_cp[i] = "1"+ tree_plus_deep_cp[i] 
      tree_minus_deep_cp[i] = "1"+ tree_minus_deep_cp[i] 
  tree_plus_deep_cp.insert(0, '0')
  tree_minus_deep_cp.insert(0, '0')
  shifted_tree_diagram = Tree_diagram(tree_plus_deep_cp, tree_minus_deep_cp)
  return shifted_tree_diagram



"""
This represents the flip automorphism on F. It possesses an order of 2, 
and at the level of tree diagrams, it is achieved by reflecting the trees about a vertical line.
"""
def flip_automorphism(tree_diagram: Tree_diagram) -> Tree_diagram:
  
  tree_plus_deep_cp = copy.deepcopy(tree_diagram.top)
  tree_minus_deep_cp = copy.deepcopy(tree_diagram.bottom) 

  for i in range(len(tree_plus_deep_cp)):
      for j in range(len(tree_plus_deep_cp[i])):
        if tree_plus_deep_cp[i][j] == '0':
          tree_plus_deep_cp[i] = tree_plus_deep_cp[i][:j] + '2' + tree_plus_deep_cp[i][j+1:]
        elif tree_plus_deep_cp[i][j] == '2':
          tree_plus_deep_cp[i] = tree_plus_deep_cp[i][:j] + '0' + tree_plus_deep_cp[i][j+1:]
      for j in range(len(tree_minus_deep_cp[i])):
        if tree_minus_deep_cp[i][j] == '0':
          tree_minus_deep_cp[i] = tree_minus_deep_cp[i][:j] + '2' + tree_minus_deep_cp[i][j+1:]
        elif tree_minus_deep_cp[i][j] == '2':
          tree_minus_deep_cp[i] = tree_minus_deep_cp[i][:j] + '0' + tree_minus_deep_cp[i][j+1:]
  tree_plus_deep_cp.sort()
  tree_minus_deep_cp.sort()
  return Tree_diagram(tree_plus_deep_cp, tree_minus_deep_cp)

"""
This function returns the words representing a complete binary tree, producing the labels of its leaves 
(which are words composed of the letters {0, 1}). The height of the tree must be at least 1.
"""
def generate_complete_binary_tree(height: int) -> list:
  if height == 0:
    return [""]
  elif height == 1:
    return ["0", "1", "2"]
  else:
    return ["0" + i for i in generate_complete_binary_tree(height-1)]+ ["1" + i for i in generate_complete_binary_tree(height-1)]+ ["2" + i for i in generate_complete_binary_tree(height-1)] 




"""
This function flattens a list. It is utilized in the find_common_tree function.
"""
def flatten_list(outer_list: list) -> list:
    flat_list = []
    for element in outer_list:
        if type(element) is list:
            for item in element:
                flat_list.append(item)
        else:
            flat_list.append(element)
    return flat_list



 
"""
This function takes two binary tree diagrams as input, represented by pairs (tree_plus, tree_minus) 
and (tree_plus_prime, tree_minus_prime). It finds a representative such that the second tree 
of the first tree diagram is equal to the first tree of the second tree diagram. 
This facilitates the multiplication of the elements.
"""
def find_common_tree(tree_diagram_one: Tree_diagram, tree_diagram_two: Tree_diagram):
  
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




"""
The following function takes a binary tree diagram and returns its normal form in terms of two numpy arrays: 
one for the positive part and one for the negative part.
The positive part is described by a vector of the form [a_0, ..., a_n], 
and the negative part is described by a vector of the form [b_0, ..., b_n]. 
Here, a_0, ..., a_n, b_0, ..., b_n are all non-negative integers. 
This function identifies the vectors with the smallest n such that at least one between a_n and b_n is non-zero.
"""
def find_normal_form(tree_diagram: Tree_diagram) -> list:
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


"""
This function returns the words representing a complete binary tree, 
producing the labels of its leaves (these are words composed of the letters {0, 1}). 
The height of the tree must be at least 0.
"""
def generate_complete_binary_tree(height: int) -> list:
  if height == 0:
    return [""]
  elif height == 1:
    return ["0", "1"]
  else:
    return ["0" + i for i in generate_complete_binary_tree(height-1)]+ ["1" + i for i in generate_complete_binary_tree(height-1)]


"""
tree_plus and tree_minus are lists containing binary words, with digits in {0, 1}, each representing a ternary tree. 
Both trees have the same number of leaves. The function reduce_tree_diagram takes two binary trees and applies 
a reduction move if possible. The function returns a pair of binary tree diagrams.
"""
def reduce_tree_diagram(tree_diagram: Tree_diagram) -> Tree_diagram:
  
  tree_plus = copy.deepcopy(tree_diagram.top)
  tree_minus = copy.deepcopy(tree_diagram.bottom)

  for i in range(len(tree_plus)-2,-1,-1):
    if i+1<len(tree_plus) and tree_plus[i][:-1] == tree_plus[i+1][:-1] and tree_minus[i][:-1] == tree_minus[i+1][:-1]:
        tree_plus[i] = tree_plus[i][:-1]
        tree_minus[i] = tree_minus[i][:-1]
        del tree_plus[i+1]
        del tree_minus[i+1]
  tree_diagram_reduced =  Tree_diagram(tree_plus, tree_minus)
  return tree_diagram_reduced



"""
This function takes two binary tree diagrams as input, represented by pairs (tree_plus, tree_minus) 
and (tree_plus_prime, tree_minus_prime). It finds a representative such that the second tree of the 
first tree diagram is equal to the first tree of the second tree diagram. This facilitates the multiplication of the elements.
"""
def find_common_tree(tree_diagram_one: Tree_diagram, tree_diagram_two: Tree_diagram) -> list:
  
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


"""
The function takes two tree diagrams, described by two pairs of ternary words, 
and finds a representative of their product (prod_tree_plus_temp, prod_tree_minus_temp). 
This representative is then transformed into its reduced form using the function reduce_tree_diagram in a do-while loop.
"""
def multiplication_tree_diagrams(tree_diagram_one: Tree_diagram, tree_diagram_two: Tree_diagram) -> Tree_diagram:
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


"""
This function multiplies several elements of F. The input is a list whose elements are binary tree diagrams.
"""
def multiply_many_tree_diagrams(collection_of_tree_diagram: list) -> Tree_diagram:
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


"""
This function calculates the power of an element in F. 
The element is stored in the variable tree_diagram, while exponent is an integer.
"""
def power_tree_diagram(tree_diagram: Tree_diagram, exponent: int) -> Tree_diagram:
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

"""
  x_0 = Tree_diagram(['00', '01', '1'], ['0', '10', '11'])
  x_1 = right_shift_homomorphism(x_0)

  x_0inv = x_0.inverse_tree_diagram()
  x_1inv = x_1.inverse_tree_diagram()
"""
if __name__ == '__main__':


  identity = Tree_diagram(generate_complete_binary_tree(3), generate_complete_binary_tree(3))
  identity.print_tree_diagram()
  reduce_tree_diagram(identity).print_tree_diagram()
  reduce_tree_diagram(reduce_tree_diagram(reduce_tree_diagram(identity))).print_tree_diagram()
  find_common_tree(x_0,x_1)[0].print_tree_diagram()
  find_common_tree(x_0,x_1)[1].print_tree_diagram()
  product = power_tree_diagram(x_0, 2)
  print("product new")
  product.print_tree_diagram()
  print(type(flatten_list(generate_complete_binary_tree(2))))

"""
if __name__ == '__main__':
  x_0
  print(from_tree_to_graph(x_0))
  x_0.print_tree_diagram()
"""
