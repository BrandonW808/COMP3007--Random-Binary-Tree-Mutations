{\rtf1\ansi\ansicpg1252\cocoartf1671
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural\partightenfactor0

\f0\fs24 \cf0 Student: Brandon Ward\
Student #: 101038470\
\
Loading File\
To execute, cd into directory containing comp3007_f18_101038470_a3.hs\
:load comp3007_f18_101038470_a3.hs\
\
Question 1\
to test congruential number generator use the following commands once file is loaded;\
findCongruentialNumber x \
where x <= 8\
\
For convenience I\'92ve added a testing function;\
findCongruentialNumbers x y\
where x <= 8 & y = n for X0\'85Xn\
\
Questions 2-3\
\
To Load a Tree;\
let VARIABLE_NAME = OPERATION PARAM1 PARAM2\
where VARIABLE_NAME is the tree name \
and OPERATION is one of \'91Add\'92 \'91Subtract\'92 \'91Multiply\'92 or \'91Divide\'92\
and PARAM1 & PARAM2 is either (Value (Val FLOAT)), (Value (Lit \'91x\'92)), or another Tree\
\
To show a tree;\
showTree TREE_NAME\
where TREE_NAME is the VARIABLE_NAME from loading a tree\
\
to graphically view a tree;\
drawGraphicalTree TREE_NAME \
where TREE_NAME is the VARIABLE_NAME from loading a tree\
\
Question 4\
To randomly mutate a tree;\
mutateTree TREE_NAME\
where TREE_NAME is the VARIABLE_NAME from loading a tree\
\
}