% Homework Assignment
% Stefan Haller

Exercise 1
==========

> Explain your initial guess procedure! Why is your approach sensible?

The first step of the Expectation Maximization algorithm is the initial guess.
The procedure must be done in a sufficient and robust way, because it has a
great impact on the outcome of the whole algorithm. The EM-algorithm will only
find a local maxima, so the initial guess determines which local maxima will be
found.

These are the most important things the initial guess must provide:

(1) All possible motifs must be included in the initial guess. If a motif is not
    reproducible by the initial guess, the algorithm will throw away the correct
    motif by assigning a possibility of zero. This must be prevented by any
    means.

(2) The initial guess should not be static. Otherwise the EM algorithm will
    converge to the same local optimum every time. By randomizing the initial
    guess the chances are higher that we got a global optimum.

In my implementation the initial guess procedure uses the following techniques:

(1) The program runs multiple passes of the EM algorithm with a random
    initialization. It chooses for each of the sequence random start positions
    and builds the initial $M_{rel}$ matrix out of subsequences of length $K$.

    The EM algorithm works best if the right motif (or parts of it) are included
    in the initial guess. To ensure a good solution it is necessary that the
    number of random passes is high enough.

(2) After all the random passes are finished, the implementation counts the
    number occurrences for each motif. The motif with the highest match rate
    is selected and it is used to initialize the final EM pass.

The following pseudocode shows the idea of the algorithm:

| counts = new Map
| for $r\ =\ 1,\ \ldots,\ ROUNDS$
|     for $j = 1,\ \ldots,\ N$
|         choose random $i$ in range $[1,\ L-K]$
|         $Z^j = i$
|     samples = $subsequences(K,\ Z)$
|     calculate $f_{i, c}$, $p_{i,c}$ and $M_{rel}$ for sampled values
|     motifs = run EM algorithm with initial $M_{rel}$ and calculate motifs
|     for each $motif$ in $motifs$
|         $counts[motif]$ $+$$=$ $1$
| $bestMotif = maximumOf(counts)$
| run final EM algorithm with $bestMotif$ as initialization
| output the final results

Exercise 2
==========

> Implement the EM-algorithm as given in class!

The Haskell implementation of the EM-algorithm can be found in the tarball in
the directory “src”. The recommended way to build the project is to grab a copy
of the Haskell Platform and run the following commands in the root directory of
the tarball:

    cabal install --only-dependencies
    cabal build

Running the resulting executable can also be accomplished by using cabal:

    cabal run EM -f inputfile

For convenience there are also pre-built executable available:

    ./build/bin/EM -f inputfile

Running the program with the `--help` flags shows all possible command line
flags.

Exercise 3
==========

> Implement the following algorithm:
>
> | For $t = 1, \ldots,$ convergence
> |     Find $Z^{t+1} = \arg\max_Z Pr(S \mid Z, M^t)$.
> |     Find $M^{t+1} = \arg\max_M Pr(S \mid Z^t, M)$.

The algorithm is implemented in the source file `src/Alternative.hs`. To enable
the alternative algorithm just run the executable with the `-a` flag:

    cabal run EM -a -f inputfile

Or:

    ./build/bin/EM -a -f inputfile

Exercise 4
==========

> Compare (2) and (3) empirical and then give your best explanation of how they
> differ!

The two algorithms look quite similar, but there are some major differences.

The most obvious difference is that the EM algorithm builds a full probabilistic
model while the other one just keeps track of the start positions with the
highest probabilities. That being said, the original EM algorithm is a
generative method while the other one is not.

With generative models in general it is possible to recover all the parameters of
a probabilistic model. It would be also possible to generate values for the
model, e.g. for simulations.

The original EM algorithm has a higher run time than the modified algorithm.
Most of the time the EM algorithm gives the more sophisticated result. The
second algorithm also gives good results, but the error-rate is higher,
especially in the not obvious cases.

The number of iterations until convergence is much higher for the original EM
algorithm than for the other one. This also leads to the superior performance of
the alternative algorithm and probably the higher error rate.

One could consider the second variant of the algorithm a speed trade-off. For
example processing large databases of sequence data can be preprocessed by the
alternative algorithm to filter out sequences which do not contain a meaningful
motif. The remaining sequences can later be processed with the EM algorithm.

<!-- vim: set ts=4 sts=4 sw=4 tw=80 et ft=markdown spl=en spell: -->
