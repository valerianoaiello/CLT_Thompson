"""
CONSTRUCTION OF LINK DIAGRAMS FROM ELEMENTS OF THE BROWN-THOMPSON GROUP

The purpose of this module is to take elements of the Bornw-Thompson group and produce a representation of
the corresponding link in such a way that SAGE can understand it,
so that we can later compute the corresponding knot invariants (such as the HOMFLY polynomial)
"""

import copy
import numpy as np

import sys
sys.path.append('/afs/cern.ch/work/v/vaiello/private/Thompson-knot-theory/src')
from group_operations import * 

"""
Given a word (= a string), it adds a + at the beginning.
"""
def add_plus_sign(word):
  return '+' + word

"""
Given a word (= a string), it adds a - at the beginning.
"""
def add_minus_sign(word):
  return '-' + word


"""
This function finds the biggest common word in two words representing an overstrand passing through a crossing.
It returns the last character of both words.
"""
def remove_common_subword_up_to_exchange_syllables(word_one, word_two):
  if word_one[:-1] == word_two[:-1]:
    word_one = word_one[-1]
    word_two = word_two[-1]
    return word_one, word_two
  elif word_one[:-1] == exchange_syllables(word_two)[:-1]:
    word_one = word_one[-1]
    word_two = exchange_syllables(word_two)[-1]
    return word_one, word_two
  elif exchange_syllables(word_one)[:-1] == word_two[:-1]:
    word_one = exchange_syllables(word_one)[-1]
    word_two = word_two[-1]
    return word_one, word_two
  elif exchange_syllables(word_one)[:-1] == exchange_syllables(word_two)[:-1]:
    word_one = exchange_syllables(word_one)[-1]
    word_two = exchange_syllables(word_two)[-1]
    return word_one, word_two
  return word_one, word_two

"""
This function finds the sign in the middle of the word and then exchages the end with the beginning,
for example, it turns words like +01+0 into +0+01 
"""
def exchange_syllables(word):
  if '+' in word[1:]:
    index = word[1:].index('+')
    word = word[index+1 : ]+ word[:index +1]
  elif '-' in word[1:]:
    index = word[1:].index('-')
    word = word[index+1 : ]+ word[:index +1]
  return word



"""
With this "move" we go from the leaf of one tree to the correspongind leaf of the other tree.
"""
def pass_through_leaves(current_position, tree_diagram, walk_within_component_):

  graph = from_tree_to_graph(tree_diagram)
  if current_position[0] == '+' and current_position[1:] in tree_diagram.top and graph[current_position][0] not in walk_within_component_:
    walk_within_component_ = walk_within_component_ + [graph[current_position][0]]
  elif current_position[0] == '-' and current_position[1:] in tree_diagram.bottom:
    index_leaf = tree_diagram.bottom.index(current_position[1:])
    new_position = tree_diagram.top[index_leaf]
    if ('+' + new_position) not in walk_within_component_:
      walk_within_component_ = walk_within_component_ + ['+' + new_position]
    
  return walk_within_component_

"""
This is the first type of move.
It's permormed entirely in the same tree.
It starts moving upward in the top tree, downward in the bottom tree.
"""
def basic_move_1(current_position, tree_diagram, walk_within_component_):
  graph = from_tree_to_graph(tree_diagram)
  if current_position[-1]  == '0': # We move from a left edge to a right edge
      if current_position not in walk_within_component_:
        walk_within_component_ = walk_within_component_ + [current_position]
      walk_within_component_ = walk_within_component_+ [current_position[:-1]]
      walk_within_component_ = walk_within_component_ + [current_position[:-1] + '2']
  elif current_position  == '+1': # We enter the middle edge below the top root and exit at the middle edge above the bottom root.  
    if current_position not in walk_within_component_:
      walk_within_component_ = walk_within_component_ + [current_position]
    walk_within_component_ = walk_within_component_ + ['+']
    walk_within_component_ = walk_within_component_ + ['-']
    walk_within_component_ = walk_within_component_ + ['-1']
  elif current_position  == '-1': # We enter the middle edge above the bottom root and exit at the middle edge below the top root.  
    if current_position not in walk_within_component_:
      walk_within_component_ = walk_within_component_ + [current_position]
    walk_within_component_ = walk_within_component_ + ['-']
    walk_within_component_ = walk_within_component_ + ['+']
    walk_within_component_ = walk_within_component_ + ['+1']
  elif current_position[-1]  == '1' and len(current_position) > 2:
    if current_position not in walk_within_component_:
      walk_within_component_ = walk_within_component_ + [current_position]
    walk_within_component_ = walk_within_component_ + [current_position[:-1]]
  elif current_position[-1]  == '2': # We move from a right edge to a left edge
    if current_position not in walk_within_component_:
      walk_within_component_ = walk_within_component_ + [current_position]
    walk_within_component_ = walk_within_component_ + [current_position[:-1]]
    walk_within_component_ = walk_within_component_ + [current_position[:-1] + '0']

  return walk_within_component_


"""
This is the second type of move.
It's permormed entirely in the same tree.
It starts moving downward in the top tree, upward in the bottom tree.
"""
def basic_move_2(current_position, tree_diagram, walk_within_component_):
  graph = from_tree_to_graph(tree_diagram)
  if (current_position[0] == '+' and current_position[1:] in tree_diagram.top) or (current_position[0] == '-' and current_position[1:] in tree_diagram.bottom):
    walk_within_component_ = pass_through_leaves(current_position, tree_diagram, walk_within_component_)
  else:
    walk_within_component_ = walk_within_component_ + [current_position + '1']
  return walk_within_component_


"""
This function determines if the direction is standard or not.
Standard = upward (in toptree)
         = downward (in bottom tree)
Non-standard = downward (in top tree)
             = upward (in bottom tree)
"""
def direction_of_movement(tree_diagram, walk_within_component_):
  if len(walk_within_component_) == 0:
    return 'standard'
  else:
    current_position = walk_within_component_[-1]
    previous_position = walk_within_component_[-2]
    if current_position[0] == '+' and previous_position == current_position[:-1]:
      return 'non-standard'
    elif current_position[0] == '+' and previous_position[:-1] == current_position:
      return 'standard'
    elif previous_position[0] != current_position[0]:
      return 'standard'    
    elif current_position[0] == '-' and previous_position == current_position[:-1]:
      return 'non-standard'
    else:
      return 'standard'

"""
This function describes all the paths within each connected components of the link diagram.
It starts from the label of a leaf,  moves upward, which is a signed string in the letters 0, 1, 2.
This function returns a list containing sublists, each sublist describes a connected component by listing all the vertices of the tree diagram
meet when moving along the component (by convention the walk starts at a leaf of the top tree and goes upwward).
"""
def walk_in_tree_diagram(tree_diagram):
  graph = from_tree_to_graph(tree_diagram)

  walk = []
  leaves_top_tree = list(map(add_plus_sign, tree_diagram.top))

#  leaves_top_tree = ['+' + word for word in tree_diagram.top]
  for leaf in leaves_top_tree:
    current_position = leaf
    walk_within_component_ = []
    if not any(current_position in sublist for sublist in walk):
      walk_within_component_ = walk_within_component(leaf, tree_diagram, walk_within_component_)
      walk.append(walk_within_component_)
  return walk 



"""
This function describes the path within one connected components of the link diagram.
It starts from the label of a leaf(called "current_position"),  moves upward, current_position, which is a signed string in the letters 0, 1, 2.
"""
def walk_within_component(current_position, tree_diagram, walk_within_component_):
  graph = from_tree_to_graph(tree_diagram)

  while True:
    walk_within_component_temp = copy.deepcopy(walk_within_component_)
    direction =  direction_of_movement(tree_diagram, walk_within_component_)
    if direction == 'standard':
      walk_within_component_ = basic_move_1(current_position, tree_diagram, walk_within_component_)
    else:
      walk_within_component_ = basic_move_2(current_position, tree_diagram, walk_within_component_)
    if walk_within_component_ == walk_within_component_temp:
      break
    current_position = walk_within_component_[-1]

  return walk_within_component_

"""
This function changes the set of labels {label_1, label_2, ..., label_n} for describing the edges of a link diagram into 1, ..., n.
"""
def change_labels_into_numbers(link_diagram_edge_labelled):
  number_of_crossings = len(link_diagram_edge_labelled)
  edge_labels = []
  for i in range(number_of_crossings):
    for j in range(4):
      if link_diagram_edge_labelled[i][j] not in edge_labels:
        edge_labels = edge_labels + [link_diagram_edge_labelled[i][j]]
      link_diagram_edge_labelled[i][j] = edge_labels.index(link_diagram_edge_labelled[i][j])+1
  return link_diagram_edge_labelled

"""
This function finds the sign in the middle of the word and then exchages the end with the beginning,
for example, it turns words like +01+0 into +0+01 
"""
def check_same_edge(word_one, word_two, tree_diagram):
#  leaves_top_tree = ['+' + word for word in tree_diagram.top]

  leaves_top_tree = list(map(add_plus_sign, tree_diagram.top))


  if word_one == word_two or word_one == exchange_syllables(word_two):
    return True
  else:
    sub_word_one_a = []
    sub_word_one_b = []

    sub_word_two_a = []
    sub_word_two_b = []

    if '+' in word_one[1:]: 
      index = word_one[1:].index('+')
      sub_word_one_a = word_one[index+1 : ]
      sub_word_one_b = word_one[:index +1]
    elif '-' in word_one[1:]:
      index = word_one[1:].index('-')
      sub_word_one_a = word_one[index+1 : ]
      sub_word_one_b = word_one[:index +1]
    first_subwords = [sub_word_one_a, sub_word_one_b]

    if '+' in word_two[1:]:
      index = word_two[1:].index('+')
      sub_word_two_a = word_two[index+1 : ]
      sub_word_two_b = word_two[:index +1]
    elif '-' in word_two[1:]:
      index = word_two[1:].index('-')
      sub_word_two_a = word_two[index+1 : ]
      sub_word_two_b = word_two[:index +1]
    second_subwords = [sub_word_two_a, sub_word_two_b]

    for leaf in leaves_top_tree:
      if leaf in first_subwords and leaf in second_subwords:
        return True 
  return False


"""
This function looks for the second occurrence of "element" in a list of list.
"index" is the index of the current occurrence of "element".
This function will be used tree_diagram_to_link_diagram to find the two times when we pass through a crossing.
"""
def find_second_occurrence(element, index_sublist, index_in_sublist, list_of_lists):
  index_two = []
  for i in range(index_sublist, len(list_of_lists)):
    if i == index_sublist:
      for j in range(index_in_sublist, len(list_of_lists[i])):
        if list_of_lists[i][j] == element and (i != index_sublist or j != index_in_sublist):
          index_two = [i, j]
          return index_two
    else:
      for j in range(len(list_of_lists[i])):
        if list_of_lists[i][j] == element and (i != index_sublist or j != index_in_sublist):
          index_two = [i, j]
          return index_two

  return index_two






"""
This function takes a ternary tree diagrams and returns a list containing pairs of sublists. 
Each pairs has as first element the vertices of the strand passing under the crossing (in the order that they are met moving in 
the chosen direction), the second second element is the strand passing over the crossing.
"""
def tree_diagram_to_link_diagram(tree_diagram):
  link = walk_in_tree_diagram(tree_diagram)  

  number_of_components = len(link)

  planar_diagram = []

# We add a sign to the leaves of the top tree and bottom tree so that it is easier 
# to compare them with data produced by "walk_in_tree_diagram".
# Then we concatenate these two trees in a sigle list. 
  top_tree = tree_diagram.top
#  top_tree = ['+' + leaf for leaf in top_tree]
  top_tree = list(map(add_plus_sign, top_tree))

  bottom_tree = tree_diagram.bottom
#  bottom_tree = ['-' + leaf for leaf in bottom_tree]
  bottom_tree = list(map(add_minus_sign, bottom_tree))

  whole_tree = top_tree + bottom_tree 

  for i in range(number_of_components):
    crossing = []
    size_of_component = len(link[i])
    for j in range(size_of_component):
      strand_one = []
      strand_two = []
      if link[i][j] not in whole_tree:
        strand_one = [link[i][j-1], link[i][j], link[i][j+1]]
        index_second_occurrence = find_second_occurrence(link[i][j], i, j, link)
        if len(index_second_occurrence)>0:
          index_component = find_second_occurrence(link[i][j], i, j, link)[0]
          index_within_component = find_second_occurrence(link[i][j], i, j, link)[1]

          strand_two = [link[index_component][index_within_component-1], link[index_component][index_within_component], link[index_component][index_within_component+1]]
          
          crossing = strand_one + strand_two
          under_crossing,over_crossing = find_under_over_strand(crossing)

          planar_diagram.append(find_under_over_strand(crossing))
  return planar_diagram



"""
This function takes a 6-tuple of ternary words, which describes a crossing, and returns 
a pair given by two vectors of length 3. 
The first vectors contains the vertices met moving on the over strand (according to the orientation)
The first vectors contains the vertices met moving on the under strand (according to the orientation)
"""
def find_under_over_strand(crossing):
  indices = [0, 2, 3, 5]
  crossing_temp = copy.deepcopy(crossing)
  over_crossing_ = copy.deepcopy(crossing)
  under_crossing_ = []
#  print('ZZ', crossing)
  for i in indices:
    if crossing[i][-1] == '1' and i == 0:
      index = i
      under_crossing_ = crossing_temp[i : i+3]
    elif crossing[i][-1] == '1' and i == 2:
      index = 0
      under_crossing_ = crossing_temp[0 : i+1]
    elif crossing[i][-1] == '1' and i == 3:
      index = i
      under_crossing_ = crossing_temp[i : i+3]
    elif crossing[i][-1] == '1' and i == 5:
      index = i - 2
      under_crossing_ = crossing_temp[i-2 : i+1]

#  for i in under_crossing_:
#    over_crossing_.remove(i)
  for j in range(index + 2, index -1, -1):
    del over_crossing_[j]
#  print(under_crossing_, over_crossing_)

  return under_crossing_, over_crossing_


"""
This function converts the crossing described by labelling the vertices 
into crossing where the labels are on the edges.
"""
def tree_diagram_to_planar_diagram(tree_diagram):
  link_diagram_edge_labelled = []
  edge_labels = []
  
  number_of_leaves = tree_diagram.get_number_leaves()

  if number_of_leaves == 1:
    link_diagram_edge_labelled = [[2, 1, 1, 2]]
    return link_diagram_edge_labelled
  else:
    link_diagram = tree_diagram_to_link_diagram(tree_diagram)
    number_of_crossings = len(link_diagram)

    j = 0
    for i in range(number_of_crossings):
  # edge_one is the strand passing under the crossing, edge_two is the one passing over the crossing.
      edge_one_in = link_diagram[i][0][0] + link_diagram[i][0][1]
      edge_one_out = link_diagram[i][0][1] + link_diagram[i][0][2]
      edge_two_in = link_diagram[i][1][0] + link_diagram[i][1][1]
      edge_two_out = link_diagram[i][1][1] + link_diagram[i][1][2]

      crossing = [edge_one_in, edge_two_in, edge_one_out, edge_two_out]

      difference_edge_two_in_out = remove_common_subword_up_to_exchange_syllables(edge_two_in, edge_two_out)
      if (len(edge_one_in)>len(edge_one_out)) and (int(difference_edge_two_in_out[0])>int(difference_edge_two_in_out[1])):
        if '+' in edge_two_out:      
          crossing = [edge_one_in, edge_two_out, edge_one_out, edge_two_in]
          link_diagram_edge_labelled.append(crossing)  
        else:
          crossing = [edge_one_in, edge_two_in, edge_one_out, edge_two_out]
          link_diagram_edge_labelled.append(crossing)    
      elif (len(edge_one_in)<len(edge_one_out)) and (int(difference_edge_two_in_out[0])>int(difference_edge_two_in_out[1])):      
        if '+' in edge_two_out:      
          crossing = [edge_one_in, edge_two_in, edge_one_out, edge_two_out]
          link_diagram_edge_labelled.append(crossing) 
        else:
          crossing = [edge_one_in, edge_two_out, edge_one_out, edge_two_in]
          link_diagram_edge_labelled.append(crossing)    
      elif (len(edge_one_in)>len(edge_one_out)) and (int(difference_edge_two_in_out[0])<int(difference_edge_two_in_out[1])):      
        if '+' in edge_two_out:      
          crossing = [edge_one_in, edge_two_in, edge_one_out, edge_two_out]
          link_diagram_edge_labelled.append(crossing)
        else:
          crossing = [edge_one_in, edge_two_out, edge_one_out, edge_two_in]
          link_diagram_edge_labelled.append(crossing)       
      elif (len(edge_one_in)<len(edge_one_out)) and (int(difference_edge_two_in_out[0])<int(difference_edge_two_in_out[1])):      
        if '+' in edge_two_out:      
          crossing = [edge_one_in, edge_two_out, edge_one_out, edge_two_in]
          link_diagram_edge_labelled.append(crossing) 
        else:
          crossing = [edge_one_in, edge_two_in, edge_one_out, edge_two_out]
          link_diagram_edge_labelled.append(crossing) 
              


  # These lines identify the leaves of the top tree with those of the bottom tree
    top_tree = tree_diagram.top
#    top_tree = ['+' + leaf for leaf in top_tree]
    top_tree = list(map(add_plus_sign, top_tree))


    bottom_tree = tree_diagram.bottom
#    bottom_tree = ['-' + leaf for leaf in bottom_tree]
    bottom_tree = list(map(add_minus_sign, bottom_tree))

    for i in range(len(link_diagram_edge_labelled)):
      for j in range(len(link_diagram_edge_labelled[i])):
        for k in range(len(bottom_tree)):
          if bottom_tree[k] in link_diagram_edge_labelled[i][j]:
            link_diagram_edge_labelled[i][j] = link_diagram_edge_labelled[i][j].replace(bottom_tree[k], top_tree[k])  

    # Now we remove duplicates (if edges are described in the two different ways, we remove one of them), 
    # for example we remove one among: +01+0 and +0+01
    for i in range(len(link_diagram_edge_labelled)):
      for j in range(i, len(link_diagram_edge_labelled)):
        for k in range(4):
          for l in range(4):
            if check_same_edge(link_diagram_edge_labelled[i][k], link_diagram_edge_labelled[j][l], tree_diagram):
              link_diagram_edge_labelled[i][k] = link_diagram_edge_labelled[j][l]

    link_diagram_edge_labelled = change_labels_into_numbers(link_diagram_edge_labelled)
  return link_diagram_edge_labelled


