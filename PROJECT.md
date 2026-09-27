# Project plan

- [X] Simple terminals (string, "any character")
- [X] Sequence and ordered-choice
- [X] Positive/negative lookahead
- [X] Optional
- [X] Kleene expansion
- [ ] Character classes
  - [X] Simple expansion to choice
  - [ ] Transformation into internal "switch" (see below)
- [ ] Left-recursion elimination and/or prohibition
  - [X] Simple left recursion
  - [X] Prohibit indirect left-recursion
  - [ ] [Grow-LR algorithm](https://web.cs.ucla.edu/~todd/research/pepm08.pdf)
- [ ] Optimizations
  - [ ] Grammar simplification
    - [ ] Peephole 
      - [X] Flatten redundant sequences and choices (only one participant)
      - [X] Flatten direct nesting of sequences and choices (seq-in-seq or choice-in-choice)
      - [ ] Left-factor choices between direct terminals with shared prefixes into "Switch" operator
    - [X] Eliminate redundant rules generated from Kleene expansion
    - [X] Inline simple, non-recursive non-terminals
  - [ ] Memoization analysis and virtual inlining
- [ ] Position tracking
- [ ] Memoization table
- [X] Erlang code generation
- [ ] Code support
  - [ ] User-defined sub-expression bindings ("labels->variables")
  - [ ] User-defined result-evaluation ("code blocks")
  - [ ] Code generation for included transformations
    - [ ] Kleene expansion (cons operations)
    - [ ] LR-elimination (preserve left-associativity by lifting)

# Bugs from WIP code

- [X] From episode 7: Recursion elimination does not call the tail in the tail
  rule, meaning we only "recurse" at most once.
- [ ] Code generation for deep case expressions (many choices) results in non-unique variable names and warnings like `Warning: variable '_Remainder2' is already bound. If you mean to ignore this value, use '_' or a different underscore-prefixed name`

# Future optimizations/transformations

## `mini_erl` IR

- `case` clauses matching `_` that contain `case` expressions with the same subject can be flattened/inlined into the parent `case` expression. This could be applied recursively to get the "switch" construct "for free".
 
```erlang
case Input of 
    Something -> 
        %% ...
        ok; 
    _ -> case Input of 
            SomethingElse -> 
                %% ...
                ok;
            _ -> {error, no_match}
        %% some clauses
    end
end.

%% Becomes:
case Input of
    Something -> ok;
    SomethingElse -> ok;
    _ -> {error, no_match}
end
```

- Dataflow analysis: find which variable bindings in patterns are used in-scope
  and promote them to real variables, replacing unused with simple ignores
  instead of underscored names. Will help cut down on unused variable warnings.
