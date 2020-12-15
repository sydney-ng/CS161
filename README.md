# UCLA's CS 161 - Fundamentals of Artificial Intelligence

### Professor: Adnan Darwiche
### Quarter: Spring 2020

This is an undergraduate course that introduces the fundamental problem solving and knowledge representation paradigms of artificial intelligence.
This clas uses the LISP programming language and includes problem solving techniques including problem spaces,
brute-force and heuristic search, two-player games, constraint-satisfaction problems, and planning techniques. These assignments use predicate calculus, non-monotonic inference, 
probabilistic reasoning, production systems, semantic nets, frames, scripts, and semantic primitives as a way of examining machine logic. 

## Skills Demonstrated 
These assignments demonstrate how artificial intelligence algorithms can be implemented using functional programming languages (such as Lisp). Demonstration of the knowledge regarding 
natural language processing, speech, vision, and neural networks are showcased through these projects. 


# Projects 
Description of the following work: 

## Assignment 1

This examines how machines use trees to make decisions. Trees lie at the basic foundation of artificial intelligence. In this project, these foundations are demonstrated through the manipulatiton of 
tree operations such as tree ordering, determination tree height, conversion of lists into B-Trees, and the generation of sublists. 

## Assignment 2  

This assigment explores game theory. The cannibal game is a game that uses depth first iterative game theory. This problem can be solved as a tree problem. After coding alogorithms that attempt to implement a solution for this game using breadth first search (BFS), depth first search (DFS), we are able to show that 
depth first iterative deepening is the most efficient solution. 

 ## Assignment 3 
 
 This assignment is a solution to the classic game, Sokoban -- which is an exemplary example of artificial intelligence's ability to use functional logic to find solutions. 
 We are able to find a solution using a game state representation to generate next viable steps for players, maximize their efficiency, and ultimately solve the game. 
 
## Assignment 4 

The 3-SAT problem is another classic algorithms & complexity type of problem that uses backtracking to find a solution. We need to assign each variable in a clause a value so that the Conjunctive Normal Form (CNF) can be satisified. 
With three variables, the solution is simple. However, when you have `n clauses` with `n variables`, the problem becomes more complex. This code is my interpretation of how to solve this problem. 

## Assignment 6 

This assignment uses logic that is modelded in computer algorithms for machine learning. These include: disjunction, conjunction, negation, implication,
equivalence, existential quantification, and universal quantification. I write a Lisp program to convert graph coloring problems into SAT problems. 
I then will use SAT solvers tot solve the problems. If given a node index, a color index, and the maximum color index, we are able to find the output for a solution. 
