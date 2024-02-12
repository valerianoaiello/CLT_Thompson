"""
Here we create the data for the balls in the Cayley graph.
"""
import csv
import pandas as pd
import numpy as np
import glob
import os
import sys
#from sage.all import *
sys.path.append('/Users/valerianoaiello/Documents/GitHub/Thompson-knot-theory/src/')
from group_operations import *
 


"""
This function generates all the paths from the identity of F_3 to the elements of radius "radius".
"""
def generate_paths(radius: int) -> np.ndarray:
    v = np.array([i for i in range(-3,4) if i != 0])
    l = np.array([v for i in range(radius)])

    w = np.array(np.meshgrid(*l)).T.reshape(-1, radius)
    return w


"""
The following function checks if a path contains two consecutive entries of opposite sign 
(if this happens the path is not minimal)
"""
def check_reduced_path(path):
    first_entries = path[:-1]
    last_entries = path[1:]
    if np.count_nonzero(first_entries + last_entries) == len(path) -1:
        return True
    else:
        return False

"""
This function takes a path starting from the identity of F_3 and returns the product of all the elements.
The path is described by a vector whose entries are 3, 2, 1, -1, -2, -3 representing x_2^-1, x_1^-1, x_0^-1, x_0, x_1, x_2.  
"""
def multiply_elements_in_path(path_of_tree_diagrams):
  global x_0
  global x_0inv
  global x_1
  global x_1inv
  global x_2
  global x_2inv
  x_0 = Tree_diagram(['00', '01', '02', '1', '2'], ['0', '1', '20', '21', '22'])
  x_1 = right_shift_homomorphism(x_0)  x_1 = right_shift_homomorphism(x_0)

  x_0inv = x_0.inverse_tree_diagram()
  x_1inv = x_1.inverse_tree_diagram()
 
  number_of_elements = len(path_of_tree_diagrams)
  product = Tree_diagram()
  if number_of_elements == 0:
    return product
  elif number_of_elements>0:
    for i in range(len(path_of_tree_diagrams)):
      if path_of_tree_diagrams[i] == 1 :
        product = multiplication_tree_diagrams(product, x_0)
      elif path_of_tree_diagrams[i] == -1 :
        product = multiplication_tree_diagrams(product, x_0inv)
      elif path_of_tree_diagrams[i] == 2:
        product = multiplication_tree_diagrams(product, x_1)
      elif path_of_tree_diagrams[i] == -2:
        product = multiplication_tree_diagrams(product, x_1inv)
      elif path_of_tree_diagrams[i] == 3:
        product = multiplication_tree_diagrams(product, x_2)
      elif path_of_tree_diagrams[i] == -3:
        product = multiplication_tree_diagrams(product, x_2inv)
    return product

"""
<<<<<<< HEAD
=======
This function creates a dataset with the elements in the Cayley graph within a given radius.
There might be some duplicates. These will be removed later by checking the normal forms.
"""    
def create_dataset(radius: int, csv_path: str) -> None:
  with open(csv_path, 'w') as f:
      writer = csv.writer(f)
      writer.writerow([
          'word',
          'normal_form',
          'top_tree',
          'bottom_tree',
          'leaves',
      ])
      cont = 0 
      for i in range(1, radius + 1):
        paths_from_identity = generate_paths(i)
        number_of_elements, _ = paths_from_identity.shape
        for j in range(number_of_elements):
          if check_reduced_path(paths_from_identity[j][:]):
            single_path = paths_from_identity[j][:]
            element = multiply_elements_in_path(paths_from_identity[j][:])
            number_of_leaves = element.get_number_leaves()
            normal_form_of_element = find_normal_form(element)
            normal_form_of_element_listed = [list(normal_form_of_element[0]), list(normal_form_of_element[1])]
            writer.writerow(
                  [single_path] +
                  [normal_form_of_element_listed] +
                  [element.top] +
                  [element.bottom] +
                  [number_of_leaves] 
              )

"""
This function removes first and last characters from beginning and end of words,
it also removes blank spaces at beginning of word.
"""
def clean_word(word):
  word = word.lstrip()[1:-1]
  return word

"""
This function takes the data created by the function create_dataset 
and adds a column containing the "planar diagram" (this is a representation of 
a knot/link in SAGE).
"""
def add_planar_diagram_to_dataset(csv_path: str) -> None:

  with open(csv_path,'r') as csvinput:
      with open(csv_path[:-4]+'_improved.csv', 'w') as csvoutput:
          writer = csv.writer(csvoutput, lineterminator='\n')
          reader = csv.reader(csvinput)

          all = []
          row = next(reader)
          row.append('planar_diagram')
          all.append(row)
          writer.writerow(row)

          for row in reader:
              raw_top_tree = row[2][1:-1].split(',')
              raw_bottom_tree = row[3][1:-1].split(',')
              top_tree = list(map(clean_word, raw_top_tree))
              bottom_tree = list(map(clean_word, raw_bottom_tree))
              row.append(tree_diagram_to_planar_diagram(Ternary_tree_diagrams(top_tree, bottom_tree)))
              writer.writerow(row)   
          writer.writerows(all)

"""
This function is used for dataset were we removed duplicates.
"""
def add_planar_diagram_to_dataset_bis(csv_path: str) -> None:
  with open(csv_path,'r') as csvinput:
      with open(csv_path[:-4]+'_improved.csv', 'w') as csvoutput:
          writer = csv.writer(csvoutput, lineterminator='\n')
          reader = csv.reader(csvinput)

          all = []
          row = next(reader)
          row.append('planar_diagram')
          all.append(row)
          writer.writerow(row)
          cont = 0

          for row in reader:              
            cont += 1
            if cont % 50000 == 0:
              print(cont)            
            raw_top_tree = row[-3][1:-1].split(',')
            raw_bottom_tree = row[-2][1:-1].split(',')
            top_tree = list(map(clean_word, raw_top_tree))
            bottom_tree = list(map(clean_word, raw_bottom_tree))
            row.append(tree_diagram_to_planar_diagram(Ternary_tree_diagrams(top_tree, bottom_tree)))
            writer.writerow(row)

"""
This function removes duplicates from the collection of elements of the group.
This is done by checking the normal form.
"""
def remove_duplicate(csv_path: str) -> None:

  df = pd.read_csv(csv_path)
  df.drop_duplicates(subset=['normal_form'], keep='last', inplace=True)
  df.to_csv(csv_path[:-4]+'_no_duplicates.csv') #, 'w')


"""
>>>>>>> d4ae643d04089f2e9ae9908c302a4d0c7275529a
This function takes a word as a word (= string), 
removes all ', ", ], [, and returns it as an integer.
It is used to split the normal form (which is an array) into its components.
"""
def clea_word_for_pd(word):
  word = word.strip(',')
  word = word.strip()
  word = word.strip('[')
  word = word.strip(']')
  return word




"""
This function merges several datasets in folder (stored in csv_path).
"""
def merge_data(csv_path: str):
  joined_files = os.path.join(csv_path, "*.csv")
  # A list of all joined files is returned
  joined_list = glob.glob(joined_files)
  # Finally, the files are joined
  df = pd.concat(map(pd.read_csv, joined_list), ignore_index=True)
  return df

"""
This function determines the maximum length of the vectors in the positive_part of the normal form 
(this is also the maximum of the length of vectors in negative_part).
We need this information to know in how many columns we will split the positive_part and negative_part.
"""
def find_max_length_normal_form(csv_path):
  temp_max = 0
  with open(csv_path,'r') as csvinput:
    reader = csv.reader(csvinput)
    for row in reader:              
      length_norm_form = len(row[-1].split(','))
      if length_norm_form > temp_max:
        temp_max = length_norm_form
  return temp_max
 
<<<<<<< HEAD
=======
"""
This function creates a data set and creates two new columns from that containing the normal form:
one column contains the positive part, one the negative.
"""
def split_data_in_PN_parts(csv_path: str) -> None:
  with open(csv_path,'r') as csvinput:
      with open(csv_path[:-4]+'_sep_PN.csv', 'w') as csvoutput:
          writer = csv.writer(csvoutput, lineterminator='\n')
          reader = csv.reader(csvinput)

          all = []
          row = next(reader)
          row.append('positive_part')
          row.append('negative_part')
          all.append(row)
          writer.writerow(row)
          cont = 0

          for row in reader:              
            cont += 1
            if cont % 50000 == 0:
              print(cont)      
            normal_form = row[-7]                  
            normal_form = [item.strip(', [') for item in normal_form[2:-2].split(']')]
            row.append(normal_form[0])
            row.append(normal_form[1])
            writer.writerow(row)     

"""
With this function we split the columns containing the positive part and negative part into several columns.
In our dataset only the first 20 generators appear, so we split them in 20 + 20 columns.
"""
def split_positive_and_negative_parts(csv_path: str) -> None:
  with open(csv_path,'r') as csvinput:
      with open(csv_path[:-4]+'_final_split.csv', 'w') as csvoutput:
          writer = csv.writer(csvoutput, lineterminator='\n')
          reader = csv.reader(csvinput)

          all = []
          row = next(reader)
          labels_positive_part = ["x_{}".format(i) for i in range(20)]
          labels_negative_part = ["-x_{}".format(i) for i in range(20)]
          row = row + labels_positive_part
          row = row + labels_negative_part 
          all.append(row)
          writer.writerow(row)

          for row in reader:         
            positive_part = row[-2]                  
            negative_part = row[-1]                  
            positive_part = [item.strip() for item in positive_part.split(',')]
            negative_part = [item.strip() for item in negative_part.split(',')]
            length = len(positive_part)
            positive_part = positive_part + [str(0)]*(20-length)
            negative_part = negative_part + [str(0)]*(20-length)
            for i in range(20):
              row.append(positive_part[i])
              row.append(negative_part[i])

            writer.writerow(row)  



"""
This function adds the Homfly polynomial for each element in our dataset.
"""
def add_homfly_diagram_to_dataset_bis(csv_path: str) -> None:
  with open(csv_path,'r') as csvinput:
      with open(csv_path[:-4]+'_homfly.csv', 'w') as csvoutput:
          writer = csv.writer(csvoutput, lineterminator='\n')
          reader = csv.reader(csvinput)

          all = []
          row = next(reader)
          row.append('number_components')
          row.append('homfly')
          all.append(row)
          writer.writerow(row)
          cont = 0

          for row in reader:              
            cont += 1
            if cont % 50000 == 0:
              print(cont)            
            planar_diagram = row[-1][1:-1]
            planar_diagram = row[-1][1:-2].split(']')
            planar_diagram = [clea_word_for_pd(item).split(',') for item in planar_diagram]
            planar_diagram = [list(map(int, item)) for item in planar_diagram]
            L = Link(planar_diagram)
            number_components = L.number_of_components()
            polynomial = L.homfly_polynomial(normalization='vz')
            row.append(number_components)
            row.append(polynomial)
            writer.writerow(row)
              
"""
This function adds the Jones polynomial for each element in our dataset.
"""
def add_jones_diagram_to_dataset_bis(csv_path: str) -> None:
  with open(csv_path,'r') as csvinput:
      with open(csv_path[:-4]+'jones.csv', 'w') as csvoutput:
          writer = csv.writer(csvoutput, lineterminator='\n')
          reader = csv.reader(csvinput)

          all = []
          row = next(reader)
          row.append('jones')
          all.append(row)
          writer.writerow(row)
          cont = 0

          for row in reader:              
            cont += 1
            if cont % 50000 == 0:
              print(cont)            
            planar_diagram = row[-1][1:-2].split(']')
            planar_diagram = [clea_word_for_pd(item).split(',') for item in planar_diagram]
            planar_diagram = [list(map(int, item)) for item in planar_diagram]
            L = Link(planar_diagram)
            polynomial = L.jones_polynomial()
            row.append(polynomial)
            writer.writerow(row)


>>>>>>> d4ae643d04089f2e9ae9908c302a4d0c7275529a
if __name__ == '__main__':
  csv_path = '/afs/cern.ch/work/v/vaiello/private/Thompson-knot-theory/src/radiu5_improved_homfly_sep_PN_final_split.csv'
#  create_dataset(5, csv_path)
#  remove_duplicate(csv_path)
#  add_planar_diagram_to_dataset(csv_path)
#  add_homfly_diagram_to_dataset_bis(csv_path)
#  csv_path = '/afs/cern.ch/work/v/vaiello/private/Thompson-knot-theory/src/radiu5_improved_homfly.csv'
#  split_data_in_PN_parts(csv_path)
#  split_positive_and_negative_parts(csv_path)
  df = pd.read_csv(csv_path)
  print(df.info())
  print(df.head())

