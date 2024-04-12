# AFP Project - Spreadsheet

For our AFP project, we decided to build a Spreadsheet-like tool, on top of a webserver. In doing so, we worked with different aspects of application development - front-end development in the browser, back-end server work with Haskell, and writing algorithms for classic datastructures such as graphs.

A spreadsheet is a tool for organizing, analyzing and storing information in a tabular form, where each cell can contain primitive data, formulas and functions. To help with the propagation of information through the cells, we use two graphs - The forward graph and the backward graph. Every node in our forward graph points to nodes that depend on it for information; this graph is used for the propagation of changes through our spreadsheet. The backward graph is for convenience and a trade-off between time and space. If a dependency is changed because of a change in a formula, instead of going through every edge of a node in the forward graph, in the backward graph we point to every nodes' parent. Thus if we change that node to depend on another parent, or no parent at all, we know this immediately through the backward graph.

Graphs in Haskell are (in)famously not a "solved" problem. There are 4 main approaches, each with comparable performance: 

(i) Data.Graph
(ii) Alga
(iii) FGL
(iv) Hash-Graph

All of them have comparable performance (as shown [here](https://github.com/haskell-perf/graphs/tree/master), each with their own niggles. In short -

(i) Data.Graph is poor at repeated modifications to our graph - something that is vital to a spreadsheet.
(ii) Alga is probably the most modern and well developed solution with lots of graph algorithms, and yet we could only leverage this after constructing these graphs in what Alga calls "Algebraic Graphss", we didn't think this was worth it.
(iii) FGL had some bits and pieces that we wanted, and some that didn't exist, which meant we had to write it ourselves.
(iv) And finally Hash-Graph, a graph library based on constructing graphs built on top of Hash Maps. This library wasn't really used by a lot of people.

Because of all the above reasons, for better or for worse, we decided to stick to our placeholder solution - A graph based on Data.Map. 

## Important Datatypes

### Graph

Our graph uses an adjacency list representation

```Haskell
type Graph = Map (Int, Int) [(Int, Int)]
```

Where every coordinate, points to other coordinates.

### Array

Our array holds the formula for how the information inside a cell is built, along with it's evaluated value

```Haskell
type Arr   = Map (Int, Int) (Formula Int, Int)
```

### Formula, Target and Operations

We were able to support the following features of a spreadsheet:

- Raw data input (For example, just a number "42")
- Reference based information - Both absolute and relative. Absolute reference points to an exact location in our spreadsheet, whereas a relative reference points to a cell based on the cell that we write this formula in
- Basic arithmetic operations

```Haskell
data Formula a where
  Raw :: a -> Formula a 
  Ref :: Target -> Target -> Formula a
  Op  :: (a -> a -> a) -> Formula a -> Formula a -> Formula a
  Un  :: (a -> a) -> Formula a -> Formula a
  deriving Show

data Target 
  = Loc Int 
  | Rel Int
  deriving Show
```

### Other Stuff

There are also some not-so-important datatypes used in Elm for the frontend part of our application, and for our middleware. These are isomorphic, so we aren't describing these in detail here.

We use Scotty as our web framework, and use JSON to marshall data between Elm and Haskell. 

## Important Concepts and Techniques

Several classic graph algorithms were implemented by us as part of this project.

### DFS

For cycle checking.
