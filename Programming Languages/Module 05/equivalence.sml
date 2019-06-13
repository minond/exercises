(*

Different definitions of equivalence:

- PL Equivalance: compares function signatures
- Asymptotic equivalence: compares algorithm performance
- Systems equivalence: compares real-world performance

- PL Equivalance: given same inputs, same outputs and effects:
  - Good: lets us replace bad max with good max
  - Bad: ignores performance in the extreme

- Asymptotic equivalence: ignore constant factors
  - Good: focus on the alhorithm and efficiency for large inputs
  - Bad: ignores "four times faster"

- Systems equivalence: account for constant overheads and performance tuning
  - Good: faster means different (and better)
  - Bad: beware overtuning on "wrong" inputs; definition does not let you "swap"
      in a different algorithm

*)
