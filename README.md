# Sudoku Solver

Solves Sudoku using DFS and backtracking

### Usage

To run for one of the given boards:
```bash
sbt "run 3"
```

Alternatively, from within SBT shell, execute ```run 4``` to run board 4.

To pass your own board, pass the board as a comma separated string, as the first argument after run
```bash
sbt "run 1,2,,,,4,5,,,3,,,,,,"
```
For empty values, don't put a space or any other character. In the example above, 3rd entry would be initialized with 
blank