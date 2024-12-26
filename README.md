# RPP-Hound

## Dependencies

- ABC (Java Library)
- Joern
- cplint

## Running RPP-Hound

### Code Property Graph

Convert a program into its code property graph using Joern.

### Translating CPG into ProbLog

`python3 rpp-hound.py rt path/to/cpg`

### Calculating Path Probabilities

`python3 rpp-hound.py paths path/to/facts.pl`

## Analysis Specifications

### CPG to ProbLog

The analysis for translating a code property graph into a set of ProbLog facts is implemented in `analysis.scala`.

### ProbLog to Paths

The ProbLog rules for conducting rare path analysis are specified in `coverage.pl`.
Running RPP-Hound will produce a set of files, each one representing a program path.