Student: Brandon Ward
Student #: 101038470

Loading File
To execute, cd into directory containing random-binary-tree-mutations.hs
:load random-binary-tree-mutations.hs

Question 1
to test congruential number generator use the following commands once file is loaded;
findCongruentialNumber x 
where x <= 8

For convenience I’ve added a testing function;
findCongruentialNumbers x y
where x <= 8 & y = n for X0…Xn

Questions 2-3

To Load a Tree;
let VARIABLE_NAME = OPERATION PARAM1 PARAM2
where VARIABLE_NAME is the tree name 
and OPERATION is one of ‘Add’ ‘Subtract’ ‘Multiply’ or ‘Divide’
and PARAM1 & PARAM2 is either (Value (Val FLOAT)), (Value (Lit ‘x’)), or another Tree

To show a tree;
showTree TREE_NAME
where TREE_NAME is the VARIABLE_NAME from loading a tree

to graphically view a tree;
drawGraphicalTree TREE_NAME 
where TREE_NAME is the VARIABLE_NAME from loading a tree

Question 4
To randomly mutate a tree;
mutateTree TREE_NAME
where TREE_NAME is the VARIABLE_NAME from loading a tree

