Lexing and Parsing Utilities
----------------------------

Irken now has support for lexing and parsing.  The lexers are built
using the facilities in `lib/dfa` (derivative-based DFA) and the parsers
use an Earley parser (in `lib/parse/earley.scm`)

Included here are several utilities useful for developing lexers & parsers.

There are several different ways to create/specify lexers/parsers:

 1) directly in Irken code.
 2) via antlr-style grammar files. (using a `.g` extension)
 3) via s-expression format files. (using a `.sg` extension).

It's also possible to mix these depending on the application.

building
--------

Each of the utilities can be built with either the in-place compiler...

    $ self/compile demo/parse/lex2dot.scm

... or the installed compiler:

    $ cd demo/parse/
    $ irken lex2dot.scm

lex2dot
-------

    $ ./lex2dot
    Generate a lexer dfa (in dot format) to stdout.

    Usage: ./lex2dot <lexicon.sg>
        example: $ ./lex2dot sexp-lex.sg
      the input lexicon is in s-expression format.

This is useful for examining the DFA generated from a lexicon.
Debugging the regexps used to build tokens is a tricky business,
and carefully going over the graph can help identify problems.

    $ ./lex2dot meta-lex.sg
    digraph xxx {
    label = "meta-lex.sg";
    size="8,5"
    node [shape = circle];
    rankdir = LR;
    edge [fontsize = 10];
    node [ shape = circle, label = "0:({5 [A-Z_a-z]...[<0a><09> ]*})" ] 0;
    node [ shape = circle, label = "1:[]" ] 1;
    node [ shape = doublecircle, label = "2:({1/0-/ [<09> ]*//[^<0a>]*<0a>}\n|{0/0-0/ [<0a><09> ]*})" ] 2;
    ...

    $ ./lex2dot meta-lex.sg > /tmp/test.dot
    $ dot -Tsvg /tmp/test.dot > /tmp/test.svg
    $ open /tmp/test.svg

Note: you can usually open SVG files with a web browser.

lexgen
------

Once you've debugged your lexicon, you may want to generate code that
can be used by the Irken runtime to perform lexing without including
all the DFA code in your application.


    $ ./lexgen
    Generate a lexer dfa (in Irken) to stdout.

    Usage: ./lexgen <lexicon.sg> <basename>
        example: $ ./lexgen bcpl.sg bcpl > lexbcpl.scm
      <basename> is used to uniquely name the top-level Irken objects,
        for example table-bcpl, step-bcpl and dfa-bcpl.


sample usage:

    $ ./lexgen meta-lex.sg meta > metalextab.scm

The resulting file can be included in your application, and used by
the lexer in `lib/parse/lexer.scm` to tokenize files/streams.

parsetool
---------

This utility brings all the pieces together into one tool for language
experimentation. Once you have debugged your lexer, you can begin
working on your grammar.  Irken provides an [Earley
Parser)(https://en.wikipedia.org/wiki/Earley_parser), which can handle
any kind of grammar, including ambiguous ones.

Here's an example of a complete parser:

    ```scheme
    (parser
     (lexicon
      (WHITESPACE (reg "[ \n\t]+"))
      (COMMENT    (reg "[ \t]*//[^\n]*\n"))
      (COLON      (lit ":"))
      (VBAR       (lit "|"))
      (SEMICOLON  (lit ";"))
      (NAME       (reg "[A-Za-z_][A-Za-z_0-9]*"))
      )
     (filter WHITESPACE COMMENT)
     (grammar
      (syntax (syntax rule) rule)
      (rule   (NAME COLON exp SEMICOLON))
      (exp    (list VBAR exp) list)
      (list   (list term) term)
      (term   STRING NAME)
      )
     )
    ```

Note that all terminals require uppercase names.  This is a grammar
for grammars, or 'meta-grammar'.  The 'filter' sub-section indicates
tokens that will be removed before the parser sees them.

The first rule of the grammar denotes the entry point.  In this case
we start with the `syntax` non-terminal.  A `syntax` consists of one
or more `rules`.  Rules denote a non-terminal, which consists of one
or more alternatives made of terms concatenated together.  Terms are
either `STRING` or `NAME`.

This particular example parses an ANTLR-style grammar.  Other grammar
specification formats exist, like (Bauckus-Naur
Form)[https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form] or
`BNF`.  You could try your hand at writing a grammar for it!

Example usage:

    $ echo "(testing 1 2 3)" > /tmp/x.scm
    $ ./parsetool sexp.sg /tmp/x.scm
    (exps
      (exp
        (list
          (LPAREN "(")
          (exps
            (exps
              (exps
                (exps (exp (atom (SYMBOL "testing"))))
                (exp (atom (NUMBER "1"))))
              (exp (atom (NUMBER "2"))))
            (exp (atom (NUMBER "3"))))
          (RPAREN ")"))))

Here we've used an s-expression grammar to parse a simple s-expression.
The output is an s-expression representation of the parse tree for ``(testing 1 2 3)``.

If you've ever played with parsing tools before, you may agree that
these tools are *much* easier to use than e.g. lex & yacc.  Everything
is done dynamically, the lexer and parser are built on the fly.
Simple languages can be modeled in just a few minutes.
