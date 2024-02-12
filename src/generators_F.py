import sys

#from group_operations import Tree_diagram, right_shift_homomorphism
sys.path.append('/Users/valerianoaiello/Documents/GitHub/CLT_Thompson/src/')
#sys.path.append('/Users/valerianoaiello/Documents/GitHub/Thompson-knot-theory/src/')
from generators_F import *

"""
This file contains the generators of F: x_0, x_1 and their inverses.
"""

x_0 = Tree_diagram(['00', '01', '1'], ['0', '10', '11'])
x_1 = right_shift_homomorphism(x_0)

x_0inv = x_0.inverse_tree_diagram()
x_1inv = x_1.inverse_tree_diagram()

if __name__ == '__main__':
    print("Hello")