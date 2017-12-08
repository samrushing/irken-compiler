# Regular Expression to DFA using Derivatives.

See the paper ["Regular-expression derivatives reexamined"](https://www.mpi-sws.org/~turon/re-deriv.pdf) by Owens/Reppy/Turon.

The 'derivative' of a regex is a new regex that results from the
feeding of a single character/charset to the original regex.

For example, if you feed ``a`` to ``abc``, you get ``bc``.
If you feed ``a`` to ``a*b`` you get ``a*b`` again.

Using this technique, you can build a DFA directly from the regex.
The algorithm is elegant and simple.

The main trick is to 'canonicalize' each resulting derivative so that
we can tell when we've come across a derivative that we've already
seen.

As described in the paper, the resulting DFAs are often minimal or
near-minimal.

