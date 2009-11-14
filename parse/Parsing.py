#===============================================================================
# Copyright (c) 2007 Jason Evans <jasone@canonware.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#===============================================================================
#
# Release history:
#
# 1.3 (8 August 2007): Retroactively number public releases.
#
#                      Back-port to Python 2.4.
#
#                      Remove some magic surrounding epsilon, in order to
#                      generalize/simplify.
#
# 1.2 (6 May 2007): Fix some off-by-one errors in production count reporting.
#
#                   Add some missing code that helps detect which definitions
#                   are used/unused when building the parser.
#
# 1.1 (22 March 2007): Optimize/generalize Lr._production() by using argument
#                      list expansion.
#
# 1.0 (19 March 2007): Initial public release.
#
#===============================================================================
"""
The Parsing module implements an LR(1) parser generator, as well as the
runtime support for using a generated parser, via the Lr and Glr parser
drivers.  There is no special parser generator input file format, but the
parser generator still needs to know what classes/methods correspond to
various aspects of the parser.  This information is specified via
docstrings, which the parser generator introspects in order to generate a
parser.  Only one parser specification can be embedded in each module, but
it is possible to share modules between parser specifications so that, for
example, the same token definitions can be used by multiple parser
specifications.

The parsing tables are LR(1), but they are generated using a fast algorithm
that avoids creating duplicate states that result when using the generic
LR(1) algorithm.  Creation time and table size are on par with the LALR(1)
algorithm.  However, LALR(1) can create reduce/reduce conflicts that don't
exist in a true LR(1) parser.  For more information on the algorithm, see:

    A Practical General Method for Constructing LR(k) Parsers
    David Pager
    Acta Informatica 7, 249-268 (1977)

Parsing table generation requires non-trivial amounts of time for large
grammars.  Internal pickling support makes it possible to cache the most
recent version of the parsing table on disk, and use the table if the
current parser specification is still compatible with the one that was used
to generate the pickled parsing table.  Since the compatibility checking is
quite fast, even for large grammars, this removes the need to use the
standard code generation method that is used by most parser generators.

Parser specifications are encapsulated by the Spec class.  Parser instances
use Spec instances, but are themselves based on separate classes.  This
allows multiple parser instances to exist simultaneously, without requiring
multiple copies of the parsing tables.  There are two separate parser driver
classes:

  Lr : Standard Characteristic Finite State Machine (CFSM) driver, based on
       unambiguous LR(1) parsing tables.  This driver is faster than the Glr
       driver, but it cannot deal with all parsing tables that the Glr
       driver can.

  Glr : Generalized LR driver, capable of tracking multiple parse trees
        simultaneously, if the %split precedence is used to mark ambiguous
        actions.  This driver is closely based on Elkhound's design, which
        is described in a technical report:

            Elkhound: A Fast, Practical GLR Parser Generator
            Scott McPeak
            Report No. UCB/CSD-2-1214 (December 2002)
            http://www.cs.berkeley.edu/~smcpeak/elkhound/

Parser generator directives are embedded in docstrings, and must begin with
a '%' character, followed immediately by one of several keywords:

    Precedence : %fail %nonassoc %left %right %split
         Token : %token
  Non-terminal : %start %nonterm
    Production : %reduce

All of these directives are associated with classes except for %reduce.
%reduce is associated with methods within non-terminal classes.  The Parsing
module provides base classes from which precedences, tokens, and
non-terminals must be derived.  This is not as restrictive as it sounds,
since there is nothing preventing, for example, a master Token class that
subclasses Parsing.Token, which all of the actual token types then subclass.
Also, nothing prevents using multiple inheritance.

Folowing are the base classes to be subclassed by parser specifications:

  * Precedence
  * Token
  * Nonterm

The Parsing module implements the following exception classes:

  * Exception
  * SpecError
  * SyntaxError
  * AttributeError
"""
__all__ = ["Exception", "SpecError", "SyntaxError", "AttributeError",
	   "Nonterm", "Parser", "Precedence", "Spec", "Token", "Lr", "Glr"]

import cPickle
import exceptions
import re
import sys
import types

#===============================================================================
# Begin exceptions.
#

class Exception(exceptions.Exception):
    """
Top level Parsing exception class, from which all other Parsing
exception classes inherit.
"""

class AttributeError(Exception, exceptions.AttributeError):
    """
Attribute error, no different from the builtin exception, except that it
also derives from Parsing.Exception.
"""
    def __init__(self, str):
	self._str = str

    def __str__(self):
	return self._str

class SpecError(Exception):
    """
Specification error exception.  SpecError arises when the Spec
introspection machinery detects an error either during docstring parsing
or parser specification generation.
"""
    def __init__(self, str):
	self._str = str

    def __str__(self):
	return self._str

class SyntaxError(Exception, exceptions.SyntaxError):
    """
Parser syntax error.  SyntaxError arises when a Parser instance detects
a syntax error according to the Spec it is using, for the input being
fed to it.
"""
    def __init__(self, str):
	self._str = str

    def __str__(self):
	return self._str

#
# End exceptions.
#===============================================================================

class Precedence(object):
    """
Precedences can be associated with tokens, non-terminals, and
productions.  Precedence isn't as important for GLR parsers as for LR
parsers, since GLR parsing allows for parse-time resolution of
ambiguity.  Still, precedence can be useful for reducing the volume of
ambiguities that must be dealt with at run-time.

There are five precedence types: %fail, %nonassoc, %left, %right, and
%split.  Each precedence can have relationships with other precedences:
<, >, or =.  These relationships specify a directed acyclic graph (DAG),
which is used to compute the transitive closures of relationships among
precedences.  If no path exists between two precedences that are
compared during conflict resolution, parser generation fails.  < and >
are reflexive; it does not matter which is used.  Conceptually, the =
relationship causes precedences to share a node in the DAG.

During conflict resolution, an error results if no path exists in the
DAG between the precedences under consideration.  When such a path
exists, the highest precedence non-terminal or production takes
precedence.  Associativity only comes into play for shift/reduce
conflicts, where the terminal and the production have equivalent
precedences (= relationship).  In this case, the non-terminal's
associativity determines how the conflict is resolved.

The %fail and %split associativities are special because they can be
mixed with other associativities.  During conflict resolution, if
another action has non-%fail associativity, then the %fail (lack of)
associativity is overridden.  Similarly, %split associativity overrides
any other associativity.  In contrast, any mixture of associativity
between %nonassoc/%left/%right causes an unresolvable conflict.

       %fail : Any conflict is a parser-generation-time error.

               A pre-defined precedence, [none], is provided.  It has
               %fail associativity, and has no pre-defined precedence
               relationships.

   %nonassoc : Resolve shift/reduce conflicts by removing both
               possibilities, thus making conflicts a parse-time error.

       %left : Resolve shift/reduce conflicts by reducing.

      %right : Resolve shift/reduce conflicts by shifting.

      %split : Do not resolve conflicts; the GLR algorithm will split
               the parse stack when necessary.

               A pre-defined precedence, [split], is provided.  It has
               %split associativity, and has no pre-defined precedence
               relationships.

By default, all symbols have [none] precedence.  Each production
inherits the precedence of its left-hand-side nonterminal's precedence
unless a precedence is manually specified for the production.

Following are some examples of how to specify precedence classes:

  class P1(Parsing.Precedence):
      "%split p1"

  class p2(Parsing.Precedence):
      "%left" # Name implicitly same as class name.

  class P3(Parsing.Precedence):
      "%left p3 >p2" # No whitespace is allowed between > and p2.

  class P4(Parsing.Precedence):
      "%left p4 =p3" # No whitespace is allowed between = and p3.
"""
    def __init__(self, name, assoc, relationships):
	assert assoc in ["fail", "nonassoc", "left", "right", "split"]
	assert type(relationships) == dict

	self.name = name
	self.assoc = assoc
	self.relationships = relationships # Raw relationships specification.

	self.equiv = [self] # Set.  Precedences that have equivalent precedence.
	self.dominators = [] # Set.  Precedences that have higher precedence.

    def __repr__(self):
	equiv = [prec.name for prec in self.equiv]
	equiv.sort()
	domin = [prec.name for prec in self.dominators]
	domin.sort()
	return "[%%%s %s ={%s} <{%s}]" % (self.assoc, self.name, \
	  ",".join(equiv), ",".join(domin))

    # Important for pickling/unpickling.
    def __eq__(self, other):
	return self is other

class SymbolSpec(object):
    seq = 0

    def __init__(self, name, prec):
	assert type(name) == str

	self.name = name
	self.prec = prec
	self.firstSet = [] # Set.
	self.followSet = [] # Set.

	# Used for ordering symbols and hashing.
	self.seq = SymbolSpec.seq
	SymbolSpec.seq += 1

    def __repr__(self):
	return "%s" % self.name

    def __cmp__(self, other):
	return cmp(self.seq, other.seq)

    def __eq__(self, other):
	assert isinstance(other, SymbolSpec)
	return self.seq == other.seq

    def firstSetMerge(self, sym):
	if sym not in self.firstSet:
	    self.firstSet.append(sym)
	    return False
	else:
	    return True

    def followSetMerge(self, set):
	ret = True
	for sym in set:
	    if sym != epsilon and sym not in self.followSet:
		self.followSet.append(sym)
		ret = False
	return ret

class String(list):
    def __init__(self, args=[]):
	list.__init__(self, args)

	self.hash = self._hash()

    def __cmp__(self, other):
	assert isinstance(other, String)
	minLen = min(len(self), len(other))
	for i in xrange(minLen):
	    if self[i] < other[i]:
		return -1
	    elif self[i] > other[i]:
		return 1

	# Prefixes are identical.  Handle trailing characters, if any.
	if len(self) < len(other):
	    return -1
	elif len(self) == len(other):
	    return 0
	else:
	    assert len(self) > len(other)
	    return 1

    def __eq__(self, other):
	if len(self) == len(other):
	    for i in xrange(len(self)):
		if self[i] != other[i]:
		    return False
	    return True
	else:
	    return False

    def _hash(self):
	ret = 5381
	for sym in self:
	    ret = ((ret << 5) + ret) + sym.seq
	    ret &= 0xffffffffffffffff
	return ret

    def __hash__(self):
	return self.hash

class StringSpec(object):
    cache = {}

    def __init__(self, s):
	assert isinstance(s, String)
	for sym in s:
	    assert isinstance(sym, SymbolSpec)

	self.s = s
	if s in StringSpec.cache:
	    self.firstSet = StringSpec.cache[s]
	else:
	    # Calculate the first set for the string encoded by the s vector.
	    self.firstSet = [] # Set.
	    mergeEpsilon = True
	    for sym in self.s:
		hasEpsilon = False
		for elm in sym.firstSet:
		    if elm == epsilon:
			hasEpsilon = True
		    elif sym not in self.firstSet:
			self.firstSet.append(elm)
		if not hasEpsilon:
		    mergeEpsilon = False
		    break
	    # Merge epsilon if it was in the first set of every symbol.
	    if mergeEpsilon:
		self.firstSet.append(epsilon)

	    # Cache the result.
	    StringSpec.cache[s] = self.firstSet

class Symbol(object):
    def __init__(self, symSpec, parser):
	assert isinstance(symSpec, SymbolSpec)
	assert isinstance(parser, Lr)
	self.__symSpec = symSpec
	self.__parser = parser

    def __repr__(self):
	return "%r" % self.symSpec

    def __getSymSpec(self): return self.__symSpec
    def __setSymSpec(self): raise AttributeError
    symSpec = property(__getSymSpec, __setSymSpec)

    def __getParser(self): return self.__parser
    def __setParser(self): raise AttributeError
    parser = property(__getParser, __setParser)

class NontermSpec(SymbolSpec):
    def __init__(self, nontermType, name, qualified, prec):
	assert issubclass(nontermType, Nonterm) # Add forward decl for Lyken.

	SymbolSpec.__init__(self, name, prec)

	self.qualified = qualified
	self.nontermType = nontermType
	self.productions = [] # Set.

class Nonterm(Symbol):
    """
Non-terminal symbols have sets of productions associated with them.  The
productions induce a parse forest on an input token stream.  There is
one special non-terminal, which is denoted via the %start directive,
whereas all other non-terminals are denoted via the %nonterm directive.
In addition to productions (%reduce directives associated with class
methods), the merge() method may be called during resolution of
ambiguous parses.  See the merge() documentation for further details.

Following are examples of how to specify non-terminal classes and their
associated productions:

  class E(Parsing.Nonterm):
      "%start E"
      def __init__(self):
          Parsing.Nonterm.__init__(self)
          # ...

      # Productions.
      def reduceA(self, E, plus, T):
          "%reduce E plus T [split]"
          print "%r ::= %r %r %r." % (self, E, plus, T)

      def reduceB(self, T):
          "%reduce T"

  class T(Parsing.Nonterm):
        "%nonterm" # Name implicitly same as class name.
      def reduceA(self, T, star, F):
          "%reduce T star F"

      def reduceB(self, F):
          "%reduce F [p1]"

  class F(Parsing.Nonterm):
      "%nonterm F [p2]"
      def reduceA(self, lparen, E, rparen):
          "%reduce lparen E rparen"

      def reduceB(self, id):
          "%reduce id"
"""
    def __init__(self, parser):
	assert isinstance(parser, Lr)
	Symbol.__init__(self, parser._spec._sym2spec[type(self)], parser)

    def merge(self, other):
	"""
Merging happens when there is an ambiguity in the input that allows
non-terminals to be part of multiple overlapping series of
reductions.  If no merge() method is specified, the parser will
throw a syntax error upon encountering an ambiguity that confounds
reduction processing.  However, it may be useful to either discard
one of the possible parses, or to explicitly record the ambiguity in
the data structures being created during parsing.  In both of these
cases, the non-terminal-specific merge() is the place to do the
work; merge() returns an object that is stored by the parser onto
the parse stack.  In the case where merge() discards one of the
possible parses, it need only return the parse that is to be
preserved (self or other).

If multiple merges are necessary, they cause a series of merge()
calls.  The first alternative (self) may be the result of a previous
merge() call, whereas other will not have not been merged yet
(unless as the result of merging further down in the parse forest).

The alternative that is discarded is never touched by the parser
again, so if any immediate cleanup is necessary, it should be done
in merge().
"""
	raise SyntaxError, "No merge() for %r; merging %r <--> %r" % \
	  (type(self), self, other)

class Token(Symbol):
    """
Tokens are terminal symbols.  The parser is fed Token instances, which
is what drives parsing.  Typically, the user will define a class that
subclasses Parsing.Token and implement parser-specific machinery there,
then derive all actual token types from that class.

  class Token(Parsing.Token):
      def __init__(self, parser):
	  Parsing.Token.__init__(self, parser)
	  # ...

  class Plus(Token):
      "%token plus [p1]"

  class star(Token):
      "%token star [p2]" # Name implicitly same as class name.

  class lparen(Token):
      "%token [split]"

  class rparen(Token):
      "%token [none]" # [none] not necessary, since it's the default.

  class id(Token):
      "%token"
"""
    def __init__(self, parser):
	assert isinstance(parser, Lr)
	Symbol.__init__(self, parser._spec._sym2spec[type(self)], parser)
	self.__parser = parser

# AKA terminal symbol.
class TokenSpec(SymbolSpec):
    def __init__(self, tokenType, name, prec):
	assert issubclass(tokenType, Token)
	assert type(name) == str
	assert isinstance(prec, Precedence) or type(prec) == str

	SymbolSpec.__init__(self, name, prec)
	self.tokenType = tokenType

# <$>.
class EndOfInput(Token): pass
class EndOfInputSpec(TokenSpec):
    def __init__(self):
	TokenSpec.__init__(self, EndOfInput, "<$>", "none")
eoi = EndOfInputSpec()

# <e>.
class Epsilon(Token): pass
class EpsilonSpec(TokenSpec):
    def __init__(self):
	TokenSpec.__init__(self, Epsilon, "<e>", "none")
epsilon = EpsilonSpec()

class Production(object):
    seq = 0

    def __init__(self, method, qualified, prec, lhs, rhs):
	assert isinstance(prec, Precedence)
	assert isinstance(lhs, NontermSpec)
	if __debug__:
	    for elm in rhs:
		assert isinstance(elm, SymbolSpec)

	self.method = method
	self.qualified = qualified
	self.prec = prec
	self.lhs = lhs
	self.rhs = rhs

	# Used for hashing.
	self.seq = Production.seq
	Production.seq += 1

    def __getstate__(self):
	return (self.qualified, self.prec, self.lhs, self.rhs, self.seq)

    def __setstate__(self, data):
	# Convert qualified name to a function reference.
	(qualified, prec, lhs, rhs, seq) = data
	elms = qualified.split(".")
	method = sys.modules[elms[0]]
	for elm in elms[1:]:
	    method = method.__dict__[elm]

	# Set state.
	self.method = method
	self.qualified = qualified
	self.prec = prec
	self.lhs = lhs
	self.rhs = rhs
	self.seq = seq

    def __repr__(self):
	return "%r ::= %s. [%s]" % \
	  (self.lhs, " ".join(["%r" % elm for elm in self.rhs]), self.prec.name)

    # Optional callback method.
    #
    # Called when a production is reduced.
    def reduce(self, lhs, *rhs): pass

class NontermStart(Nonterm):
    def reduce(self, userStartSym, eoi):
	pass

class Start(Production):
    def __init__(self, startSym, userStartSym):
	Production.__init__(self, none, startSym, userStartSym)

class Item(object):
    def __init__(self, production, dotPos, lookahead):
	assert isinstance(production, Production)
	assert type(dotPos) == int
	assert dotPos >= 0
	assert dotPos <= len(production.rhs)
	assert type(lookahead) == list
	if __debug__:
	    for elm in lookahead:
		assert isinstance(elm, SymbolSpec)

	self.production = production
	self.dotPos = dotPos
	self.lookahead = {}
	for sym in lookahead:
	    self.lookahead[sym] = sym

	self.hash = (dotPos * Production.seq) + production.seq

    def __hash__(self):
	return self.hash

    def __eq__(self, other):
	assert isinstance(other, Item)

	return self.hash == other.hash

    def __cmp__(self, other):
	assert isinstance(other, Item)
	return self.hash.__cmp__(other.hash)

    def __repr__(self):
	strs = []
	strs.append("[%r ::=" % self.production.lhs)
	assert self.dotPos <= len(self.production.rhs)
	i = 0
	while i < self.dotPos:
	    strs.append(" %r" % self.production.rhs[i])
	    i += 1
	strs.append(" *")
	while i < len(self.production.rhs):
	    strs.append(" %r" % self.production.rhs[i])
	    i += 1
	syms = [sym for sym in self.lookahead.iterkeys()]
	syms.sort()
	strs.append("., %s] [%s]" % \
	  ("/".join(["%r" % sym for sym in syms]), \
	  self.production.prec.name))
	
	return "".join(strs)

    def lr0__repr__(self):
	strs = []
	strs.append("%r ::=" % self.production.lhs)
	assert self.dotPos <= len(self.production.rhs)
	i = 0
	while i < self.dotPos:
	    strs.append(" %r" % self.production.rhs[i])
	    i += 1
	strs.append(" *")
	while i < len(self.production.rhs):
	    strs.append(" %r" % self.production.rhs[i])
	    i += 1
	strs.append(". [%s]" % self.production.prec.name)

	return "".join(strs)

    def lookaheadInsert(self, sym):
	assert isinstance(sym, SymbolSpec)
	self.lookahead[sym] = sym

    def lookaheadDisjoint(self, other):
	sLookahead = self.lookahead
	oLookahead = other.lookahead

	for sSym in sLookahead.iterkeys():
	    if sSym in oLookahead:
		return False

	for oSym in oLookahead.iterkeys():
	    if oSym in sLookahead:
		return False

	return True

class ItemSet(dict):
    def __init__(self, args=[]):
	dict.__init__(self, args)
	self._added = {}

    def __repr__(self):
	kernel = [item for item in self.iterkeys()]
	kernel.sort()
	added = [item for item in self._added.iterkeys()]
	added.sort()
	return "ItemSet(kernel: %s, added: %r)" % \
	  (", ".join(["%r" % item for item in kernel]), \
	  ", ".join(["%r" % item for item in added]))

    def __hash__(self):
	# This works because integers never overflow, and addition is
	# transitive.
	ret = 0
	for item in self.iterkeys():
	    ret += item.hash
	return ret

    def __eq__(self, other):
	if len(self) != len(other):
	    return False
	for sItem in self.iterkeys():
	    if sItem not in other:
		return False
	return True

    def __iter__(self):
	for item in self.iterkeys():
	    assert item.production.lhs.name == "<S>" or item.dotPos != 0
	    yield item
	for item in self._added.iterkeys():
	    assert item.dotPos == 0
	    assert item.production.lhs.name != "<S>"
	    yield item

    # Merge a kernel item.
    def append(self, item):
	assert item.production.lhs.name == "<S>" or item.dotPos != 0

	if item in self:
	    self[item].lookahead.update(item.lookahead)
	else:
	    tItem = Item(item.production, item.dotPos, item.lookahead.keys())
	    self[tItem] = tItem

    # Merge an added item.
    def addedAppend(self, item):
	assert item.dotPos == 0
	assert item.production.lhs.name != "<S>"

	if item in self._added:
	    lookahead = self._added[item].lookahead
	    oldLen = len(lookahead)
	    lookahead.update(item.lookahead)
	    return (oldLen != len(lookahead))
	else:
	    self._added[item] = item
	    return True

    # Given a list of items, compute their closure and merge the results into
    # the set of added items.
    def _closeItems(self, items):
	# Iterate over the items until no more can be added to the closure.
	i = 0
	while i < len(items):
	    item = items[i]
	    rhs = item.production.rhs
	    dotPos = item.dotPos
	    if dotPos < len(rhs) \
	      and isinstance(rhs[dotPos], NontermSpec):
		for lookahead in item.lookahead.keys():
		    string = StringSpec( \
		      String(rhs[dotPos+1:] + [lookahead]))
		    lhs = rhs[dotPos]
		    for prod in lhs.productions:
			tItem = Item(prod, 0, string.firstSet)
			if self.addedAppend(tItem):
			    items.append(tItem)
	    i += 1

    # Calculate and merge the kernel's transitive closure.
    def closure(self):
	items = []
	for item in self.iterkeys():
	    rhs = item.production.rhs
	    dotPos = item.dotPos
	    if dotPos < len(rhs) and isinstance(rhs[dotPos], \
	      NontermSpec):
		for lookahead in item.lookahead.iterkeys():
		    string = StringSpec(String(rhs[dotPos+1:] + \
		      [lookahead]))
		    lhs = rhs[dotPos]
		    for prod in lhs.productions:
			tItem = Item(prod, 0, string.firstSet)
			if self.addedAppend(tItem):
			    items.append(tItem)
	self._closeItems(items)

    # Calculate the kernel of the goto set, given a particular symbol.
    def goto(self, sym):
	ret = ItemSet()
	for item in self:
	    rhs = item.production.rhs
	    dotPos = item.dotPos
	    if dotPos < len(rhs) and rhs[dotPos] == sym:
		tItem = Item(item.production, dotPos + 1, item.lookahead.keys())
		ret.append(tItem)
	return ret

    # Merge the kernel of other into this ItemSet, then update the closure.
    # It is not sufficient to copy other's added items, since other has not
    # computed its closure.
    def merge(self, other):
	items = []
	for item in other.iterkeys():
	    if item in self:
		lookahead = self[item].lookahead
		tLookahead = []
		for sym in item.lookahead.iterkeys():
		    if sym not in lookahead:
			lookahead[sym] = sym
			tLookahead.append(sym)
		if len(tLookahead) > 0:
		    tItem = Item(item.production, item.dotPos, tLookahead)
		    items.append(tItem)
	    else:
		tItem = Item(item.production, item.dotPos, \
		  item.lookahead.keys())
		self[tItem] = tItem
		items.append(tItem)

	if len(items) > 0:
	    self._closeItems(items)
	    return True
	else:
	    return False

    # Determine if self and other are weakly compatible, as defined by the
    # Pager(1977) algorithm.
    def weakCompat(self, other):
	# Check for identical kernel LR(0) items, and pair items, for later use.
	if len(self) != len(other):
	    return False
	pairs = []
	for sItem in self.iterkeys():
	    if sItem not in other:
		return False
	    oItem = other[sItem]
	    pairs.append((sItem, oItem))

	# Check for lookahead compatibility.
	for i in xrange(len(pairs)-1):
	    iPair = pairs[i]
	    isItem = iPair[0]
	    ioItem = iPair[1]
	    for j in xrange(i+1, len(pairs)):
		jPair = pairs[j]
		jsItem = jPair[0]
		joItem = jPair[1]

		if isItem.lookaheadDisjoint(joItem) \
		  and ioItem.lookaheadDisjoint(jsItem):
		    pass
		elif not isItem.lookaheadDisjoint(jsItem):
		    pass
		elif not ioItem.lookaheadDisjoint(joItem):
		    pass
		else:
		    return False
	return True

class Action(object):
    """
Abstract base class, subclassed by {Shift,Reduce}Action.
"""
    def __init__(self): pass

class ShiftAction(Action):
    """
Shift action, with assocated nextState.
"""
    def __init__(self, nextState):
	Action.__init__(self)
	self.nextState = nextState

    def __repr__(self):
	return "[shift %r]" % self.nextState

    def __eq__(self, other):
	if not isinstance(other, ShiftAction):
	    return False
	if self.nextState != other.nextState:
	    return False
	return True

class ReduceAction(Action):
    """
Reduce action, with associated production.
"""
    def __init__(self, production):
	Action.__init__(self)
	self.production = production

    def __repr__(self):
	return "[reduce %r]" % self.production

    def __eq__(self, other):
	if not isinstance(other, ReduceAction):
	    return False
	if self.production != other.production:
	    return False
	return True

class Spec(object):
    """
The Spec class contains the read-only data structures that the Parser
class needs in order to parse input.  Parser generation results in a
Spec instance, which can then be shared by multiple Parser instances.
"""
    def __init__(self, modules, pickleFile=None, pickleMode="rw",
		 skinny=True, logFile=None, graphFile=None, verbose=False):
	"""
modules : Either a single module, or a list of modules, wherein to
          look for parser generator directives in docstrings.

pickleFile : The path of a file to use for Spec pickling/unpickling.

pickleMode :  "r" : Unpickle from pickleFile.
              "w" : Pickle to pickleFile.
              "rw" : Unpickle/pickle from/to pickleFile.

skinny : If true, discard all data that are only strictly necessary
         while constructing the parsing tables.  This reduces
         available debugging context, but substantially reduces
         pickle size.

logFile : The path of a file to store a human-readable copy of the
          parsing tables in.

graphFile : The path of a file to store a graphviz representation
            (dot format) of the precedence relationship graph.

verbose : If true, print progress information while generating the
          parsing tables.
"""
	assert pickleFile == None or type(pickleFile) == str
	assert pickleMode in ["rw", "r", "w"]
	assert type(skinny) == bool
	assert logFile == None or type(logFile) == str
	assert graphFile == None or type(graphFile) == str
	assert type(verbose) == bool

	self._skinny = skinny
	self._verbose = verbose

	# Default (no) precedence.
	self._none = Precedence("none", "fail", {})
	self._split = Precedence("split", "split", {})

	# Symbols are maintained as two separate sets so that non-terminals and
	# terminals (tokens) can be operated on separately where needed.
	self._precedences = {self._none.name: self._none,
			     self._split.name: self._split}
	self._nonterms = {}
	self._tokens = {eoi.name: eoi, epsilon.name: epsilon}
	self._sym2spec = {EndOfInput: eoi, Epsilon: epsilon}
	self._productions = []

	self._userStartSym = None
	self._startSym = None
	self._startProd = None

	# Everything below this point is computed from the above (once
	# introspection is complete).

	self._itemSets = [] # Each element corresponds to an element in _action.
	self._itemSetsHash = None
	# LR parsing tables.  The tables conceptually contain one state per row,
	# where each row contains one element per symbol.  The table is
	# conceptually in row-major form, but each row is actually a dictionary.
	# If no entry for a symbol exists for a particular state, then input of
	# that symbol is an error for that state.
	self._action = []
	self._goto = []
	self._startState = None
	self._nActions = 0
	self._nConflicts = 0
	self._nImpure = 0 # Number of LR impurities (does not affect GLR).

	# Introspect modules and generate parse tables.
	if type(modules) == types.ModuleType:
	    # Wrap single module in a list.
	    modules = [modules]
	self._prepare(modules, pickleFile, pickleMode, logFile, graphFile)

    def __getPureLR(self):
	return (self._nConflicts + self._nImpure == 0)
    def __setPureLR(self): raise AttributeError
    pureLR = property(__getPureLR, __setPureLR)

    def __getConflicts(self): return self._nConflicts
    def __setConflicts(self): raise AttributeError
    conflicts = property(__getConflicts, __setConflicts)

    def __repr__(self):
	if self._skinny:
	    # Print a very reduced summary, since most info has been discarded.
	    return "Parsing.Spec: %d states, %d actions (%d split)" % \
	      (len(self._action), self._nActions, self._nImpure)

	lines = []

	#=======================================================================
	lines.append("Precedences:")
	deco = [(prec.name, prec) for prec in self._precedences.itervalues()]
	deco.sort()
	for elm in deco:
	    prec = elm[1]
	    lines.append("  %r" % prec)

	lines.append("Tokens:")
	syms = [sym for sym in self._tokens.itervalues()]
	syms.sort()
	for token in syms:
	    lines.append("  %r %r" % (token, token.prec))
	    lines.append("    First set: %r" % token.firstSet)
	    lines.append("    Follow set: %r" % token.followSet)

	lines.append("Non-terminals:")
	syms = [sym for sym in self._nonterms.itervalues()]
	syms.sort()
	for sym in syms:
	    lines.append("  %r %r" % (sym, sym.prec))
	    lines.append("    First set: %r" % sym.firstSet)
	    lines.append("    Follow set: %r" % sym.followSet)
	    lines.append("    Productions:")
	    prods = sym.productions[:]
	    prods.sort()
	    for prod in prods:
		lines.append("      %r" % prod)

	lines.append("Item sets:")
	for i in xrange(len(self._itemSets)):
	    lines.append("  %d: %r" % (i, self._itemSets[i]))
	#=======================================================================

	ntokens = len(self._tokens) - 1
	nnonterms = len(self._nonterms) - 1
	nproductions = len(self._productions) - 1
	nstates = len(self._action)
	lines.append(("Parsing.Spec: %d token%s, %d non-terminal%s, " + \
	  "%d production%s, %d state%s, %d action%s (%d split):") % \
	  (ntokens, ("s", "")[ntokens == 1], \
	  nnonterms, ("s", "")[nnonterms == 1], \
	  nproductions, ("s", "")[nproductions == 1], \
	  nstates, ("s", "")[nstates == 1], \
	  self._nActions, ("s", "")[self._nActions == 1], \
	  self._nImpure))
	if self.pureLR:
	    lines.append("Algorithm compatibility: GLR, LR")
	elif self._nConflicts == 0:
	    lines.append("Algorithm compatibility: GLR")
	else:
	    lines.append("Algorithm compatibility: None, due to ambiguity")
	lines.append("Parsing tables:")
	for i in xrange(len(self._action)):
	    lines.append("  %s" % ("=" * 78))
	    lines.append("  State %d:%s" % \
	      (i, ("", " (start state)")[self._startState == i]))
	    items = [item for item in self._itemSets[i]]
	    items.sort()
	    for item in items:
		lines.append(" %s%s" % (" " * (len("%d" % i) + 9),
		  item.lr0__repr__()))
	    lines.append("    Goto:")
	    syms = [sym for sym in self._goto[i]]
	    syms.sort()
	    for sym in syms:
		lines.append("    %15r : %r" % (sym, self._goto[i][sym]))
	    lines.append("    Action:")
	    syms = [sym for sym in self._action[i]]
	    syms.sort()
	    for sym in syms:
		for action in self._action[i][sym]:
		    conflict = "   "
		    for other in self._action[i][sym]:
			if action != other:
			    resolution = self._resolve(sym, other, action)
			    if resolution == "err":
				conflict = "XXX"
				break

		    if type(action) == ShiftAction:
			lines.append("%s %15r : %-6s %d [%s]" % \
			  (conflict, sym, "shift", action.nextState, \
			  sym.prec.name))
		    else:
			assert type(action) == ReduceAction
			lines.append("%s %15r : %-6s %r" % \
			  (conflict, sym, "reduce", action.production))

	ret = "\n".join(lines)
	return ret

    def _prepare(self, modules, pickleFile, pickleMode, logFile, graphFile):
	"""
Compile the specification into data structures that can be used by
the Parser class for parsing.
"""
	# Get the grammar specification.
	self._introspect(modules)

	# Augment grammar with a special start symbol and production:
	#
	#   <S> ::= S <$>.
	assert self._startSym == None
	assert isinstance(self._userStartSym, NontermSpec)
	self._startSym = NontermSpec(NontermStart, "<S>",
	  "%s.NontermStart" % __name__, self._none)
	self._startProd = Production(NontermStart.reduce.im_func,
				     "%s.NontermStart.reduce" % __name__,
				     self._none, self._startSym, \
				     [self._userStartSym, eoi])
	self._startSym.productions.append(self._startProd)
	self._nonterms["<S>"] = self._startSym
	self._productions.append(self._startProd)

	# Resolve references in the grammar specification.
	self._references(logFile, graphFile)

	# Check for a compatible pickle.
	compat = self._unpickle(pickleFile, pickleMode)

	if compat == "incompatible":
	    # Create the collection of sets of LR(1) items.
	    self._firstSets()
	    self._followSets()
	    self._items()

	if compat == "compatible":
	    # Just because the pickle was compatible does not mean that it is
	    # valid for parsing.
	    if self._nConflicts != 0:
		raise SpecError, \
		  "Compatible pickle is invalid due to conflicts (%d)" % \
		  self._nConflicts
	if compat in ["itemsets", "incompatible"]:
	    # Generate LR(1) parsing tables.
	    self._lr()

	    # Disambiguate actions.
	    self._disambiguate()

	    # Check for unused or ambiguous definitions, as well as reporting
	    # ambiguities.
	    try:
		self._validate(logFile)
	    finally:
		# Pickle the spec, if method parameters so dictate, even if
		# there were validation errors, so that the pickle might be
		# used in part during later runs.
		self._pickle(pickleFile, pickleMode)
	elif compat == "repickle":
	    # Pickle the spec, if method parameters so dictate.
	    self._pickle(pickleFile, pickleMode)

	if self._skinny:
	    # Discard data that are not needed during parsing.  Note that
	    # _pickle() also discarded data that don't even need to be pickled.
	    del self._precedences
	    del self._nonterms
	    del self._tokens
	    del self._productions

    # Introspect modules and find special parser declarations.  In order to be
    # a special class, the class must both 1) be subclassed from Token or
    # Nonterm, and 2) contain the appropriate %foo docstring.
    def _introspect(self, modules):
	if self._verbose:
	    print ("Parsing.Spec: Introspecting module%s to acquire formal" + \
	    " grammar specification...") % ("s", "")[len(modules) == 1]

	self._precedences["none"] = self._none
	self._precedences["split"] = self._split

	for module in modules:
	    d = module.__dict__
	    for k in d:
		v = d[k]
		if type(v) is types.TypeType and type(v.__doc__) == str:
		    dirtoks = v.__doc__.split(" ")

		    #===========================================================
		    # Precedence.
		    #
		    if issubclass(v, Precedence) and dirtoks[0] in \
		      ["%fail", "%nonassoc", "%left", "%right", "%split"]:
			name = k
			relationships = {}
			i = 1
			while i < len(dirtoks):
			    tok = dirtoks[i]
			    m = re.compile(r'([<>=])([A-Za-z]\w*)').match(tok)
			    if m:
				# Precedence relationship.
				if m.group(2) in relationships:
				    raise SpecError, \
				      ("Duplicate precedence " \
				      + "relationship: %s") \
				      % v.__doc__
				relationships[m.group(2)] = m.group(1)
			    else:
				m = re.compile(r'([A-Za-z]\w*)').match(tok)
				if m:
				    if i != 1:
					raise SpecError, \
					  ("Precedence name must come before " \
					  + "relationships: %s") \
					  % v.__doc__
				    name = m.group(1)
				else:
				    raise SpecError, \
				      "Invalid precedence specification: %s" % \
				      v.__doc__
			    i += 1

			if name in self._precedences:
			    raise SpecError, \
			      "Duplicate precedence name: %s" % v.__doc__
			if name in self._tokens:
			    raise SpecError, \
			      "Identical token/precedence names: %s" % v.__doc__
			if name in self._nonterms:
			    raise SpecError, \
			      "Identical nonterm/precedence names: %s" % \
			      v.__doc__
			prec = Precedence(name, dirtoks[0][1:], relationships)
			self._precedences[name] = prec
		    #===========================================================
		    # Token.
		    #
		    elif issubclass(v, Token) and dirtoks[0] in ["%token"]:
			name = k
			prec = None
			i = 1
			while i < len(dirtoks):
			    tok = dirtoks[i]
			    m = re.compile(r'\[([A-Za-z]\w*)\]').match(tok)
			    if m:
				if i < len(dirtoks) - 1:
				    raise SpecError, \
				      ("Precedence must come last in token " \
				      + "specification: %s") % v.__doc__
				prec = m.group(1)
			    else:
				m = re.compile(r'([A-Za-z]\w*)').match(tok)
				if m:
				    name = m.group(1)
				else:
				    raise SpecError, \
				      "Invalid token specification: %s" % \
				      v.__doc__
			    i += 1
			if name in self._precedences:
			    raise SpecError, \
			      "Identical precedence/token names: %s" % v.__doc__
			if name in self._tokens:
			    raise SpecError, \
			      "Duplicate token name: %s" % v.__doc__
			if name in self._nonterms:
			    raise SpecError, \
			      "Identical nonterm/token names: %s" % v.__doc__
			if prec == None:
			    prec = "none"
			token = TokenSpec(v, name, prec)
			self._tokens[name] = token
			self._sym2spec[v] = token
		    #===========================================================
		    # Nonterm.
		    #
		    elif issubclass(v, Nonterm) and \
		      dirtoks[0] in ["%start", "%nonterm"]:
			name = None
			prec = None
			i = 1
			while i < len(dirtoks):
			    tok = dirtoks[i]
			    m = re.compile(r'\[([A-Za-z]\w*)\]').match(tok)
			    if m:
				if i < len(dirtoks) - 1:
				    raise SpecError, \
				      ("Precedence must come last in " \
				      + "non-terminal specification: %s") % \
				      v.__doc__
				prec = m.group(1)
			    else:
				m = re.compile(r'([A-Za-z]\w*)').match(tok)
				if m:
				    name = m.group(1)
				else:
				    raise SpecError, \
				      "Invalid non-terminal specification: %s" \
				      % v.__doc__
			    i += 1
			if name == None:
			    name = k
			if prec == None:
			    prec = "none"
			if name in self._precedences:
			    raise SpecError, \
			      "Identical precedence/nonterm names: %s" % \
			      v.__doc__
			if name in self._tokens:
			    raise SpecError, \
			      "Identical token/nonterm names: %s" % v.__doc__
			if name in self._nonterms:
			    raise SpecError, \
			      "Duplicate nonterm name: %s" % v.__doc__
			nonterm = NontermSpec(v, name,
			  "%s.%s" % (module.__name__, name), prec)
			self._nonterms[name] = nonterm
			self._sym2spec[v] = nonterm

			if dirtoks[0] == "%start":
			    # Start symbol.
			    if self._userStartSym != None:
				raise SpecError, \
				  "Only one start non-terminal allowed: %s" \
				  % v.__doc__
			    self._userStartSym = nonterm
		    #===========================================================
	if not isinstance(self._userStartSym, NontermSpec):
	    raise SpecError, "No start symbol specified"

    # Resolve all symbolic (named) references.
    def _references(self, logFile, graphFile):
	# Build the graph of Precedence relationships.
	self._resolvePrec(graphFile)

	# Resolve Token-->Precedence references.
	for token in self._tokens.itervalues():
	    if type(token.prec) == str:
		token.prec = self._precedences[token.prec]

	# Resolve Nonterm-->Precedence references.
	for nonterm in self._nonterms.itervalues():
	    if type(nonterm.prec) == str:
		nonterm.prec = self._precedences[nonterm.prec]

	# Resolve Nonterm-->{Nonterm,Token,Precedence} references.
	for nonterm in self._nonterms.itervalues():
	    d = nonterm.nontermType.__dict__
	    for k in d:
		v = d[k]
		if type(v) is types.FunctionType and type(v.__doc__) == str:
		    dirtoks = v.__doc__.split(" ")
		    if dirtoks[0] == "%reduce":
			rhs = []
			prec = None
			for i in xrange(1, len(dirtoks)):
			    tok = dirtoks[i]
			    m = re.compile(r'([A-Za-z]\w*)').match(tok)
			    if m:
				# Symbolic reference.
				if tok in self._tokens:
				    rhs.append(self._tokens[tok])
				elif tok in self._nonterms:
				    rhs.append(self._nonterms[tok])
				else:
				    raise SpecError, \
				      ("Unknown symbol '%s' in reduction " \
				      + "specification: %s") % (tok, v.__doc__)
			    else:
				m = re.compile(r'\[([A-Za-z]\w*)\]').match(tok)
				if m:
				    # Precedence.
				    if i < len(dirtoks) - 1:
					raise SpecError, \
					  ("Precedence must come last in " \
					  + "reduction specification: %s") % \
					  v.__doc__
				    if m.group(1) not in self._precedences:
					raise SpecError, \
					  ("Unknown precedence in reduction " \
					  + "specification: %s") % v.__doc__
				    prec = self._precedences[m.group(1)]

			if prec == None:
			    # Inherit the non-terminal's precedence.
			    prec = nonterm.prec

			prod = Production(v, "%s.%s" % (nonterm.qualified, k), \
			  prec, nonterm, rhs)
			assert prod not in nonterm.productions
			nonterm.productions.append(prod)
			self._productions.append(prod)
	if self._verbose:
	    ntokens = len(self._tokens) - 1
	    nnonterms = len(self._nonterms) - 1
	    nproductions = len(self._productions) - 1
	    print \
	      "Parsing.Spec: %d token%s, %d non-terminal%s, %d production%s" % \
	      (ntokens, ("s", "")[ntokens == 1], \
	      nnonterms, ("s", "")[nnonterms == 1], \
	      nproductions, ("s", "")[nproductions == 1])

    # Build the graph of Precedence relationships.
    def _resolvePrec(self, graphFile):
	# Resolve symbolic references and populate equiv/dominators.
	for precA in self._precedences.itervalues():
	    for precBName in precA.relationships:
		if precBName not in self._precedences:
		    raise SpecError, \
		      ("Precedence '%s' specifies a relationship with " + \
		      "unknown Precedence '%s'") % (precA, precBName)
		precB = self._precedences[precBName]
		rel = precA.relationships[precBName]
		if rel == "=":
		    precA.equiv.append(precB)
		elif rel == "<":
		    if precB not in precA.dominators:
			precA.dominators.append(precB)
		elif rel == ">":
		    if precA not in precB.dominators:
			precB.dominators.append(precA)
		else:
		    assert False

	# Create equivalence classes for all Precedence classes.  Since the
	# Precedence classes are equivalent, they also share dominator sets.
	for precA in self._precedences.itervalues():
	    for precB in precA.equiv[:]:
		if not precB.equiv is precA.equiv:
		    # Merge the sets of equivalent Precedence classes.
		    for prec in precB.equiv:
			if prec not in precA.equiv:
			    precA.equiv.append(prec)
		    # Share the equiv set.
		    for prec in precA.equiv:
			prec.equiv = precA.equiv

	# Use the equivalence classes to merge dominator sets and share them.
	for precA in self._precedences.itervalues():
	    for precB in precA.equiv[1:]:
		# Merge the sets of dominator Precedence classes.
		for prec in precB.dominators:
		    if prec not in precA.dominators:
			precA.dominators.append(prec)
		# Share the dominator set.
		precB.dominators = precA.dominators

	# Write graphviz precedence graph to graphFile, if graphFile was
	# specified.
	if graphFile != None:
	    f = open(graphFile, "w+")
	    if self._verbose:
		print \
		  "Parsing.Spec: Writing graphviz precedence graph to '%s'..." \
		  % graphFile
	    print >> f, 'digraph Precedence {'
	    print >> f, '    graph [bgcolor=black, labeljust="l"]'
	    print >> f, \
	      ('    node [shape=record, style=filled, color=black, ' + \
	      'fillcolor=gray, fontname=Helvetica, fontsize=10.0]')
	    print >> f, '    edge [color=gray]'
	    for precA in self._precedences.itervalues():
		if precA == precA.equiv[0]:
		    print >> f, \
		      ('    Precedence_%s [label="{%s}"]') % (precA.name, \
		      "\\n".join(["%s (%s)" % (p.name, p.assoc) \
		      for p in precA.equiv]))
		    for precB in precA.dominators:
			print >> f, '    Precedence_%s -> Precedence_%s' % \
			  (precB.equiv[0].name, precA.equiv[0].name)
	    print >> f, '}'
	    f.close()

	# Iteratively build dominator sets until no more work can be done.
	done = False
	while not done:
	    done = True
	    for precA in self._precedences.itervalues():
		if precA == precA.equiv[0]: # No need to do more than this.
		    for precB in precA.dominators[:]:
			for precC in precB.equiv:
			    if precC not in precA.dominators:
				precA.dominators.append(precC)
				done = False
			for precC in precB.dominators:
			    for precD in precC.equiv:
				if precD not in precA.dominators:
				    precA.dominators.append(precD)
				    done = False

	# Check for cycles in the graph.
	cycles = []
	for precA in self._precedences.itervalues():
	    for precB in [precA] + precA.equiv:
		if precB in precA.dominators:
		    cycles.append( \
		      "Precedence relationship cycle involving '%s'" % \
		      precA.name)
	if len(cycles) > 0:
	    raise SpecError, "\n".join(cycles)

    # Store state to a pickle file, if requested.
    def _pickle(self, file, mode):
	if self._skinny:
	    # Discard data that don't need to be pickled.
	    del self._startSym
	    del self._startProd
	    del self._itemSets
	    del self._itemSetsHash
	    del self._startState

	if file != None and "w" in mode:
	    if self._verbose:
		print "Parsing.Spec: Creating %s Spec pickle in %s..." % \
		  (("fat", "skinny")[self._skinny], file)
	    f = open(file, "w")
	    cPickle.dump(self, f, protocol=cPickle.HIGHEST_PROTOCOL)
	    f.close()

    # Restore state from a pickle file, if a compatible one is provided.  This
    # method uses the same set of return values as does _compatible().
    def _unpickle(self, file, mode):
	if file != None and "r" in mode:
	    if self._verbose:
		print \
		  "Parsing.Spec: Attempting to use pickle from file \"%s\"..." \
		  % file
	    try:
		f = open(file, "r")
	    except IOError:
		if self._verbose:
		    error = sys.exc_info()
		    print "Parsing.Spec: Pickle open failed: Exception %s: %s" \
		      % (error[0], error[1])
		return "incompatible"

	    # Any exception at all in unpickling can be assumed to be due to
	    # an incompatible pickle.
	    try:
		spec = cPickle.load(f)
	    except:
		if self._verbose:
		    error = sys.exc_info()
		    print "Parsing.Spec: Pickle load failed: Exception %s: %s" \
		      % (error[0], error[1])
		return "incompatible"

	    compat = self._compatible(spec)
	    if compat == "incompatible":
		if self._verbose:
		    print "Parsing.Spec: Pickle in \"%s\" is incompatible." % \
		      file
		return compat

	    if self._verbose:
		print \
		  "Parsing.Spec: Using %s pickle in \"%s\" (%s)..." \
		  % (("fat", "skinny")[spec._skinny], file, compat)

	    if compat in ["compatible", "repickle"]:
		# Copy spec's data structures.
		self._precedences = spec._precedences
		self._action = spec._action
		self._goto = spec._goto
		if not self._skinny:
		    self._startState = spec._startState
		self._nActions = spec._nActions
		self._nConflicts = spec._nConflicts
		self._nImpure = spec._nImpure
	    elif compat == "itemsets":
		# Precedences are incompatible, so great care has to be taken
		# when copying from the pickle.  Overwrite all precedence
		# specifications in spec with the new ones, then copy over all
		# of the new symbols/productions (but not the new precedences,
		# of course).  This still leaves table generation, which is
		# done by the _prepare() method later.

		# Nonterminals.
		for key in self._nonterms:
		    nontermSelf = self._nonterms[key]
		    nontermSpec = spec._nonterms[key]
		    nontermSpec.prec = nontermSelf.prec
		    # Productions.
		    for prodSelf in nontermSelf.productions:
			for prodSpec in nontermSpec.productions:
			    if prodSelf.qualified == prodSpec.qualified:
				prodSpec.prec = prodSelf.prec
				break
			assert prodSelf.qualified == prodSpec.qualified
		# Tokens.
		for key in self._tokens:
		    tokenSelf = self._tokens[key]
		    tokenSpec = spec._tokens[key]
		    tokenSpec.prec = tokenSelf.prec
	    else:
		assert False

	    # Copy spec data structures that are usable regardless of whether
	    # the parsing tables need to be rebuilt.
	    self._nonterms = spec._nonterms
	    self._tokens = spec._tokens
	    self._sym2spec = spec._sym2spec
	    self._productions = spec._productions
	    self._userStartSym = spec._userStartSym
	    if not self._skinny:
		self._startSym = spec._startSym
		self._startProd = spec._startProd
		self._itemSets = spec._itemSets
		self._itemSetsHash = spec._itemSetsHash

	    return compat
	else:
	    return "incompatible"

    # Determine whether other is compatible with self.  Note that self is not
    # completely initialized; the idea here is to determine whether other's
    # data structures can be copied *before* doing the work of building parsing
    # tables.
    #
    # Itemsets and precedences are not directly related, other than that
    # symbols have precedences associated with them.  Therefore, we check for
    # the following cases:
    #
    #   "compatible" : Completely compatible.
    #
    #   "repickle" : Compatible, but pickle needs to be regenerated.
    #
    #   "itemsets" : Itemsets are compatible, but precedence specifications are
    #                not.
    #
    #   "incompatible" : No useful compatibility.
    def _compatible(self, other):
	ret = "compatible"

	if (not self._skinny) and other._skinny:
	    return "incompatible"
	elif self._skinny != other._skinny:
	    ret = "repickle"

	# Precedences.
	if len(self._precedences) != len(other._precedences):
	    if self._verbose:
		print "Parsing.Spec: Unequal number of precedences (%d vs %d)" \
		  % (len(self._precedences), len(other._precedences))
	    ret = "itemsets"
	for key in self._precedences:
	    if key not in other._precedences:
		if self._verbose:
		    print "Parsing.Spec: Missing precedence: %s" % key
		ret = "itemsets"
		continue
	    precA = self._precedences[key]
	    precB = other._precedences[key]
	    if precA.name != precB.name \
	      or precA.assoc != precB.assoc \
	      or len(precA.relationships) != len(precB.relationships):
		if self._verbose:
		    print "Parsing.Spec: Incompatible precedences: %r vs. %r" \
		      % (precA, precB)
		ret = "itemsets"
		continue
	    for prec in precA.relationships:
		rel = precA.relationships[prec]
		if prec not in precB.relationships \
		  or precB.relationships[prec] != rel:
		    if self._verbose:
			print \
			  "Parsing.Spec: Incompatible precedences: %r vs. %r" \
			  % (precA, precB)
		    ret = "itemsets"
		    break

	# Nonterminals.
	if len(self._nonterms) != len(other._nonterms):
	    if self._verbose:
		print \
		  "Parsing.Spec: Unequal number of non-terminals (%d vs %d)" \
		  % (len(self._nonterms), len(other._nonterms))
	    return "incompatible"
	for key in self._nonterms:
	    if key not in other._nonterms:
		if self._verbose:
		    print "Parsing.Spec: Missing non-terminal: %s" % key
		return "incompatible"
	    nontermA = self._nonterms[key]
	    nontermB = other._nonterms[key]
	    if nontermA.name != nontermB.name \
	      or nontermA.qualified != nontermB.qualified \
	      or nontermA.nontermType != nontermB.nontermType:
		if self._verbose:
		    print \
		      "Parsing.Spec: Incompatible non-terminals: %r vs. %r" \
		      % (nontermA, nontermB)
		return "incompatible"
	    if nontermA.prec.name != nontermB.prec.name:
		if self._verbose:
		    print \
		      ("Parsing.Spec: Differing precedences for " + \
		      "non-terminal: %r") % nontermA
		ret = "itemsets"

	    # Productions.
	    if len(nontermA.productions) != len(nontermB.productions):
		if self._verbose:
		    print \
		      "Parsing.Spec: Unequal number of productions (%d vs %d)" \
		      % (len(self._productions) - 1, \
		      len(other._productions) - 1)
		return "incompatible"
	    for prodA in nontermA.productions:
		match = False
		for prodB in nontermB.productions:
		    if prodA.qualified == prodB.qualified \
		      and prodA.lhs.name == prodB.lhs.name \
		      and len(prodA.rhs) == len(prodB.rhs):
			match = True
			for i in xrange(len(prodA.rhs)):
			    if prodA.rhs[i].name != prodB.rhs[i].name:
				match = False
				if self._verbose:
				    print \
				      ("Parsing.Spec: Incompatible" + \
				      " productions: %r vs. %r") \
				      % (prodA, prodB)
				break
			if prodA.prec.name != prodB.prec.name:
			    if self._verbose:
				print \
				  ("Parsing.Spec: Differing precedences " + \
				  "for production: %r") % prodA
			    ret = "itemsets"
		if not match:
		    return "incompatible"

	# Tokens.
	if len(self._tokens) != len(other._tokens):
	    if self._verbose:
		print "Parsing.Spec: Unequal number of tokens (%d vs %d)" \
		  % (len(self._tokens), len(other._tokens))
	    return "incompatible"
	for key in self._tokens:
	    if key not in other._tokens:
		if self._verbose:
		    print "Parsing.Spec: Missing token: %s" % key
		return "incompatible"
	    tokenA = self._tokens[key]
	    tokenB = other._tokens[key]
	    if tokenA.name != tokenB.name \
	      or tokenA.tokenType != tokenB.tokenType:
		if self._verbose:
		    print \
		      "Parsing.Spec: Incompatible tokens: %r vs. %r" \
		      % (tokenA, tokenB)
		return "incompatible"
	    if tokenA.prec.name != tokenB.prec.name:
		if self._verbose:
		    print \
		      "Parsing.Spec: Differing precedences for token: %r" \
		      % tokenA
		ret = "itemsets"

	# User start symbol.
	if self._userStartSym.name != other._userStartSym.name:
	    if self._verbose:
		print "Parsing.Spec: Differing start symbols: %s vs. %s" \
		  % (self._userStartSym.name, other._userStartSym.name)
	    return "incompatible"

	if other._skinny and ret == "itemsets":
	    # The itemsets have to be regenerated, since they weren't pickled.
	    ret = "incompatible"
	return ret

    # Check for unused prececence/token/nonterm/reduce specifications, then
    # throw a SpecError if any ambiguities exist in the grammar.
    def _validate(self, logFile):
	if self._verbose:
	    print "Parsing.Spec: Validating grammar..."

	lines = []
	if self._nConflicts > 0:
	    lines.append("Parsing.Spec: %d unresolvable conflict%s" % \
	      (self._nConflicts, ("s", "")[self._nConflicts == 1]))

	# Previous code guarantees that all precedence/token/nonterm names are
	# unique.  Therefore, we can build a single dictionary here that keys on
	# names.
	used = {}
	productions = []
	for itemSet in self._itemSets:
	    for item in itemSet:
		productions.append(item.production)
		used[item.production.prec.name] = item.production.prec
		for sym in [item.production.lhs] + item.production.rhs:
		    used[sym.name] = sym
		    used[sym.prec.name] = sym.prec

		for token in item.lookahead.iterkeys():
		    used[token.prec.name] = token.prec

	nUnused = 0

	# Precedences.
	for prec in self._precedences:
	    if prec not in [self._none.name, self._split.name]:
		if prec not in used:
		    nUnused += 1
		    lines.append("Parsing.Spec: Unused precedence: %r" % \
		      self._precedences[prec])

	# Tokens.
	for token in self._tokens:
	    if token not in [eoi.name, epsilon.name]:
		if token not in used:
		    nUnused += 1
		    lines.append("Parsing.Spec: Unused token: %s" % \
		      self._tokens[token])

	# Nonterms.
	for nonterm in self._nonterms:
	    if nonterm not in [self._startSym.name]:
		if nonterm not in used:
		    nUnused += 1
		    lines.append("Parsing.Spec: Unused nonterm: %s" % \
		      self._nonterms[nonterm])

	# Productions.
	for production in self._productions:
	    if production not in productions:
		nUnused += 1
		lines.append("Parsing.Spec: Unused production: %r" % production)

	if nUnused > 0:
	    lines.insert((1, 0)[self._nConflicts == 0], \
	      "Parsing.Spec: %d unused definition%s" % \
	      (nUnused, ("s", "")[nUnused == 1]))

	# Write to logFile, if one was specified.
	if logFile != None:
	    f = open(logFile, "w+")
	    if self._verbose:
		print "Parsing.Spec: Writing log to '%s'..." % logFile
	    f.write("%s" % "\n".join(lines + ["%r" % self]))
	    f.close()

	# Conflicts are fatal.
	if self._nConflicts > 0:
	    raise SpecError, ("%s" % ("\n".join(lines)))

	# Make sure to let the user know about unused symbols if verbosity is
	# enabled, and there weren't any conflicts to cause notification via an
	# exception.
	if self._verbose:
	    ntokens = len(self._tokens) - 1
	    nnonterms = len(self._nonterms) - 1
	    nproductions = len(self._productions) - 1
	    lines.append(
	      "Parsing.Spec: %d token%s, %d non-terminal%s, %d production%s" \
	      % (ntokens, ("s", "")[ntokens == 1], \
	      nnonterms, ("s", "")[nnonterms == 1], \
	      nproductions, ("s", "")[nproductions == 1]))
	    sys.stdout.write("%s\n" % "\n".join(lines))

    # Compute the first sets for all symbols.
    def _firstSets(self):
	# Terminals.
	# first(X) is X for terminals.
	for sym in self._tokens.itervalues():
	    sym.firstSetMerge(sym)

	# Non-terminals.
	#
	# Repeat the following loop until no more symbols can be added to any
	# first set.
	done = False
	while not done:
	    done = True
	    for name in self._nonterms:
		sym = self._nonterms[name]
		for prod in sym.productions:
		    # Merge epsilon if there is an empty production.
		    if len(prod.rhs) == 0:
			if not sym.firstSetMerge(epsilon):
			    done = False

		    # Iterate through the RHS and merge the first sets into
		    # this symbol's, until a preceding symbol's first set does
		    # not contain epsilon.
		    for elm in prod.rhs:
			containsEpsilon = False
			for elmSym in elm.firstSet:
			    if not sym.firstSetMerge(elmSym):
				done = False
			    if elmSym == epsilon:
				containsEpsilon = True
			if not containsEpsilon:
			    break

    # Compute the follow sets for all symbols.
    def _followSets(self):
	self._startSym.followSet = [epsilon]

	# Repeat the following loop until no more symbols can be added to any
	# follow set.
	done = False
	while not done:
	    done = True
	    for name in self._nonterms:
		sym = self._nonterms[name]
		for prod in sym.productions:
		    # For all A ::= aBb, merge first(b) into follow(B).
		    for i in xrange(len(prod.rhs) - 1):
			for j in xrange(i+1, len(prod.rhs)):
			    if not prod.rhs[i].followSetMerge( \
			      prod.rhs[j].firstSet):
				done = False
			    if epsilon not in prod.rhs[j].firstSet:
				break

		    # For A ::= ab, or A ::= aBb where first(b) contains <e>,
		    # merge follow(A) into follow(B).
		    for i in xrange(len(prod.rhs)-1, -1, -1):
			if not prod.rhs[i].followSetMerge(prod.lhs.followSet):
			    done = False
			if epsilon not in prod.rhs[i].firstSet:
			    break

    # Compute the collection of sets of LR(1) items.
    def _items(self):
	# Add {[S' ::= * S $., <e>]} to _itemSets.
	tItemSet = ItemSet()
	tItem = Item(self._startProd, 0, [epsilon])
	tItemSet.append(tItem)
	tItemSet.closure()
	self._itemSets.append(tItemSet)

	# List of state numbers that need to be processed.
	worklist = [0]
	if self._verbose:
	    nwork = len(worklist)
	    print "Parsing.Spec: Generating LR(1) itemset collection... ",
	    sys.stdout.write("+")
	    sys.stdout.flush()

	# itemSetsHash uses itemsets as keys.  A value is a list of _itemSets
	# indices; these itemsets are the ones referred to the key itemset.
	itemSetsHash = {tItemSet: [0]}

	syms = self._tokens.values() + self._nonterms.values()
	while len(worklist) > 0:
	    if self._verbose:
		if abs(len(worklist) - nwork) >= 10:
		    nwork = len(worklist)
		    sys.stdout.write("[%d/%d]" % \
		      (len(worklist), len(self._itemSets)))
		    sys.stdout.flush()

	    i = worklist.pop(0)
	    itemSet = self._itemSets[i]
	    for sym in syms:
		gotoSet = itemSet.goto(sym)
		if len(gotoSet) > 0:
		    merged = False
		    if gotoSet in itemSetsHash:
			for j in itemSetsHash[gotoSet]:
			    mergeSet = self._itemSets[j]
			    if mergeSet.weakCompat(gotoSet):
				merged = True
				if mergeSet.merge(gotoSet):
				    # Process worklist in MRU order.  This
				    # causes a depth-first traversal.
				    if j in worklist:
					worklist.remove(j)
				    else:
					if self._verbose:
					    sys.stdout.write(".")
					    sys.stdout.flush()
				    worklist.insert(0, j)
				break
		    if not merged:
			gotoSet.closure()
			worklist.append(len(self._itemSets))
			if gotoSet not in itemSetsHash:
			    itemSetsHash[gotoSet] = [len(self._itemSets)]
			else:
			    itemSetsHash[gotoSet].append(len(self._itemSets))
			self._itemSets.append(gotoSet)
			if self._verbose:
			    sys.stdout.write("+")
			    sys.stdout.flush()

	if self._verbose:
	    sys.stdout.write("\n")
	    sys.stdout.flush()
	self._itemSetsHash = itemSetsHash

    # Compute LR parsing tables.
    def _lr(self):
	# The collection of sets of LR(1) items already exists.
	assert len(self._itemSets) > 0
	assert len(self._action) == 0
	assert len(self._goto) == 0
	assert self._startState == None
	assert self._nConflicts == 0

	if self._verbose:
	    print \
	      "Parsing.Spec: Generating LR(1) parsing tables (%d state%s)... " \
	      % (len(self._itemSets), \
	      ("s", "")[len(self._itemSets) == 1]),
	    sys.stdout.flush()

	itemSetsHash = self._itemSetsHash

	for itemSet in self._itemSets:
	    if self._verbose:
		sys.stdout.write(".")
		sys.stdout.flush()
	    #===================================================================
	    # _action.
	    state = {}
	    self._action.append(state)
	    for item in itemSet:
		# X ::= a*Ab
		if item.dotPos < len(item.production.rhs):
		    sym = item.production.rhs[item.dotPos]
		    if isinstance(sym, TokenSpec):
			itemSetB = itemSet.goto(sym)
			for i in itemSetsHash[itemSetB]:
			    itemSetC = self._itemSets[i]
			    if itemSetC.weakCompat(itemSetB):
				self._actionAppend(state, sym, ShiftAction(i))

		    # Check if this is the start state.
		    if self._startState == None \
		      and item.production.lhs == self._startSym \
		      and item.dotPos == 0:
			assert len(item.production.rhs) == 2
			self._startState = len(self._action) - 1
		# X ::= a*
		elif item.dotPos == len(item.production.rhs):
		    for lookahead in item.lookahead.iterkeys():
			self._actionAppend(state, lookahead, \
			  ReduceAction(item.production))
		else:
		    assert False
	    #===================================================================
	    # _goto.
	    state = {}
	    self._goto.append(state)
	    for nonterm in self._nonterms.itervalues():
		itemSetB = itemSet.goto(nonterm)
		if itemSetB in itemSetsHash:
		    for i in itemSetsHash[itemSetB]:
			itemSetC = self._itemSets[i]
			if itemSetC.weakCompat(itemSetB):
			    assert nonterm not in state
			    state[nonterm] = i

	if self._verbose:
	    sys.stdout.write("\n")
	    sys.stdout.flush()

    # Add a symbol action to state, if the action doesn't already exist.
    def _actionAppend(self, state, sym, action):
	assert type(state) == dict
	assert isinstance(sym, SymbolSpec)
	assert isinstance(action, Action)

	if sym not in state:
	    state[sym] = [action]
	else:
	    actions = state[sym]
	    if action not in actions:
		state[sym].append(action)

    # Look for action ambiguities and resolve them if possible.
    def _disambiguate(self):
	assert self._nActions == 0
	assert self._nConflicts == 0
	assert self._nImpure == 0

	if self._verbose:
	    print "Parsing.Spec: Disambiguating LR(1) parsing tables... ",
	    sys.stdout.flush()

	for stateInd in xrange(len(self._action)):
	    state = self._action[stateInd]
	    if self._verbose:
		vRes = "."
		vNConflicts = 0
	    for sym in state:
		nConflicts = 0
		acts = [act for act in state[sym]]
		# Construct a list that corresponds to acts; each element
		# indicates whether to preserve the action.
		actStats = [True] * len(acts)

		# Fill in the cells of actStats.
		for i in xrange(len(acts)):
		    actI = acts[i]
		    for j in xrange(i+1, len(acts)):
			actJ = acts[j]
			res = self._resolve(sym, actI, actJ)
			if res == "neither":
			    actStats[i] = False
			    actStats[j] = False
			elif res == "old":
			    actStats[j] = False
			elif res == "both":
			    pass
			elif res == "new":
			    actStats[i] = False
			elif res == "err":
			    actStats[i] = False
			    actStats[j] = False
			    nConflicts += 1
			else:
			    assert False

		# Look for actions that can coexist or dominate all other
		# actions.
		newActs = []
		for j in xrange(len(acts)):
		    if actStats[j]:
			newActs.append(acts[j])
		# Replace the action set if there exists a valid resolution
		# among the actions.
		if len(newActs) > 0 or nConflicts == 0:
		    if self._verbose:
			if len(newActs) != len(acts):
			    vRes = "_"
		    state[sym] = newActs
		    nConflicts = 0
		elif self._verbose:
		    vNConflicts += nConflicts

		nActions = len(state[sym])
		if nActions > 1:
		    nImpure = nActions
		else:
		    nImpure = 0

		# Update summary stats.
		self._nActions += nActions
		self._nConflicts += nConflicts
		self._nImpure += nImpure

	    if self._verbose:
		if vNConflicts == 0:
		    sys.stdout.write("%s" % vRes)
		else:
		    sys.stdout.write("[%d:%d]" % (stateInd, vNConflicts))
		sys.stdout.flush()

	if self._verbose:
	    sys.stdout.write("\n")
	    sys.stdout.flush()

    # Compute how to resolve an action conflict.
    #
    # ret: "neither" : Discard both.
    #      "old"     : Keep old.
    #      "both"    : Keep both.
    #      "new"     : Keep new.
    #      "err"     : Unresolvable conflict.
    def _resolve(self, sym, oldAct, newAct):
	if type(oldAct) == ShiftAction:
	    oldPrec = sym.prec
	elif type(oldAct) == ReduceAction:
	    oldPrec = oldAct.production.prec
	else:
	    assert False

	if type(newAct) == ShiftAction:
	    newPrec = sym.prec
	elif type(newAct) == ReduceAction:
	    newPrec = newAct.production.prec
	else:
	    assert False

	if oldPrec in newPrec.dominators:
	    # Discard new action.
	    ret = "old"
	elif newPrec in oldPrec.dominators:
	    # Discard old action.
	    ret = "new"
	elif oldPrec in newPrec.equiv:
	    assert newPrec in oldPrec.equiv

	    if oldPrec.assoc == "split" or newPrec.assoc == "split":
		ret = "both"
	    elif type(newAct) == type(oldAct):
		assert type(newAct) == ReduceAction
		assert type(oldAct) == ReduceAction
		# Fatal reduce/reduce conflict.
		ret = "err"
	    else:
		if oldPrec.assoc != "fail" and newPrec.assoc != "fail" \
		  and oldPrec.assoc != newPrec.assoc:
		    # Conflicting associativity.
		    ret = "err"
		else:
		    # Determine associativity.  If only one of the actions has
		    # %fail associativity, it is overridden by the other.
		    if oldPrec.assoc == "fail":
			assoc = newPrec.assoc
		    else:
			assoc = oldPrec.assoc
		    assert assoc in ["fail", "nonassoc", "left", "right"]

		    if assoc == "fail":
			ret = "err"
		    elif assoc == "left":
			if type(oldAct) == ShiftAction:
			    ret = "new"
			else:
			    assert type(newAct) == ShiftAction
			    ret = "old"
		    elif assoc == "right":
			if type(oldAct) == ShiftAction:
			    ret = "old"
			else:
			    assert type(newAct) == ShiftAction
			    ret = "new"
		    elif assoc == "nonassoc":
			ret = "neither"
		    else:
			assert False
	else:
	    if newPrec in oldPrec.equiv:
		print "%r <--> %r" % (oldPrec, newPrec)
	    assert newPrec not in oldPrec.equiv
	    # No specified relationship between precedences.
	    ret = "err"

	return ret

class Lr(object):
    """
LR(1) parser.  The Lr class uses a Spec instance in order to parse
input that is fed to it via the token() method, and terminated via the
eoi() method.
"""
    def __init__(self, spec):
	if __debug__:
	    if type(self) == Lr:
		assert spec.pureLR
	assert spec._nConflicts == 0
	self._spec = spec
	self.reset()
	self._verbose = False

    def __getSpec(self): return self._spec
    def __setSpec(self, spec): return AttributeError
    spec = property(__getSpec, __setSpec)

    def __getStart(self): return self._start
    def __setStart(self, start): raise AttributeError
    start = property(__getStart, __setStart, """
A list of parsing results.  For LR parsing, there is only ever one
result, but for compatibility with the Glr interface, start is a
list.
""")

    def __getVerbose(self): return self._verbose
    def __setVerbose(self, verbose):
	assert type(verbose) == bool
	self._verbose = verbose
    verbose = property(__getVerbose, __setVerbose)

    def reset(self):
	self._start = None
	self._stack = [(Epsilon(self), 0)]

    def token(self, token):
	"""
Feed a token to the parser.
"""
	tokenSpec = self._spec._sym2spec[type(token)]
	self._act(token, tokenSpec)

    def eoi(self):
	"""
Signal end-of-input to the parser.
"""
	token = EndOfInput(self)
	self.token(token)

	assert self._stack[-1][0] == token # <$>.
	if self._verbose:
	    self._printStack()
	    print "   --> accept"
	self._stack.pop()

	top = self._stack[-1]
	self._start = [self._stack[1][0]]
	assert self._start[0].symSpec == self._spec._userStartSym

    def _act(self, sym, symSpec):
	if self._verbose:
	    self._printStack()
	    print "INPUT: %r" % sym

	while True:
	    top = self._stack[-1]
	    if symSpec not in self._spec._action[top[1]]:
		raise SyntaxError, ("Unexpected token: %r" % sym)

	    actions = self._spec._action[top[1]][symSpec]
	    assert len(actions) == 1
	    action = actions[0]

	    if self._verbose:
		print "   --> %r" % action
	    if type(action) == ShiftAction:
		self._stack.append((sym, action.nextState))
		break
	    else:
		assert type(action) == ReduceAction
		self._reduce(action.production)

	    if self._verbose:
		self._printStack()

    def _printStack(self):
	    print "STACK:",
	    for node in self._stack:
		print "%r" % node[0],
	    print
	    print "      ",
	    for node in self._stack:
		print "%r%s" % (node[1], \
		  (" " * (len("%r" % node[0]) - len("%r" % node[1])))),
	    print

    def _reduce(self, production):
	nRhs = len(production.rhs)
	rhs = []
	for i in xrange(len(self._stack) - nRhs, len(self._stack)):
	    rhs.append(self._stack[i][0])

	r = self._production(production, rhs)

	for i in xrange(nRhs):
	    self._stack.pop()

	top = self._stack[-1]
	self._stack.append((r, self._spec._goto[top[1]][production.lhs]))

    def _production(self, production, rhs):
	sym = production.lhs.nontermType(self)
	nRhs = len(rhs)
	assert nRhs == len(production.rhs)
	r = production.method(sym, *rhs)

	# Python's method definition syntax makes returning self from %reduce
	# methods cumbersome, so translate None here.
	if r == None:
	    r = sym

	return r

#===============================================================================
# Begin graph-structured stack (GSS) classes.
#

class Gss(list):
    """Graph-structured stack."""
    def __init__(self, glr):
	list.__init__(self)

	self._glr = glr

class Gsse(object):
    """Graph-structured stack edge."""
    def __init__(self, below, above, value):
	self.node = below
	above._edges.append(self)
	self.value = value

    def __repr__(self):
	return "{%r}" % self.value

    def __eq__(self, other):
	if self.node != other.node \
	  or self.value != other.value:
	    return False
	return True

class Gssn(object):
    """Graph-structured stack node."""
    def __init__(self, below, value, nextState):
	assert isinstance(below, Gssn) or below == None

	self._edges = []
	if below != None:
	    Gsse(below, self, value)
	self.nextState = nextState

    def __repr__(self):
	return "[%d]" % self.nextState

    def __getEdge(self):
	assert len(self._edges) == 1
	return self._edges[0]
    def __setEdge(self): raise AttributeError
    edge = property(__getEdge, __setEdge)

    def edges(self):
	for edge in self._edges:
	    yield edge

    def nodes(self):
	for edge in self._edges:
	    yield edge.node

    # Iterate over all paths of length pathLen.  Path length is measured as the
    # number of edges in the path, so a path of length 0 still consists of a
    # single node.
    #
    # Each path is encoded as a list that alternates between nodes and edges,
    # where the first and last elements are always nodes.
    #
    # <e>-grammars can cause cycles, which requires that we avoid infinite
    # recursion.
    def paths(self, pathLen=None):
	assert ((type(pathLen) == int and pathLen >= 0) or pathLen == None)

	for path in self._pathsRecurse(pathLen, []):
	    yield path

    def _pathsRecurse(self, pathLen, path):
	path.insert(0, self)
	if pathLen == None and len(self._edges) == 0:
	    yield path[:]
	elif pathLen != None and len(path) - 1 == pathLen * 2:
	    yield path[:]
	else:
	    for edge in self.edges():
		# Avoid infinite recursion due to <e>-production cycles.
		if len(path) < 3 or edge != path[1]:
		    path.insert(0, edge)
		    for x in edge.node._pathsRecurse(pathLen, path):
			yield x
		    path.pop(0)
	path.pop(0)

#
# End graph-structured stack (GSS) classes.
#===============================================================================

class Glr(Lr):
    """
GLR parser.  The Glr class uses a Spec instance in order to parse input
that is fed to it via the token() method, and terminated via the eoi()
method.
"""
    def __init__(self, spec):
	Lr.__init__(self, spec)

    def reset(self):
	self._start = None

	# Initialize with a stack that is in the start state.
	self._gss = Gss(self)
	top = Gssn(None, None, 0)
	self._gss.append(top)

	self._paths = []

    def token(self, token):
	"""
Feed a token to the parser.
"""
	if self._verbose:
	    print "%s" % ("-" * 80)
	    print "INPUT: %r" % token
	tokenSpec = self._spec._sym2spec[type(token)]
	self._act(token, tokenSpec)
	if len(self._gss) == 0:
	    raise SyntaxError, ("Unexpected token: %r" % token)

    def eoi(self):
	"""
Signal end-of-input to the parser.
"""
	token = EndOfInput(self)
	self.token(token)

	# Gather the start symbols from the stacks.
	self._start = []
	for top in self._gss:
	    for path in top.paths():
		assert len(path) == 5
		if self._verbose:
		    print "   --> accept %r" % path
		edge = path[1]
		assert isinstance(edge.value, Nonterm)
		assert edge.value.symSpec == self._spec._userStartSym
		self._start.append(edge.value)

	if len(self._start) == 0:
	    raise SyntaxError, "Unexpected end of input"

	if self._verbose:
	    print "Start: %r" % self._start
	    print "%s" % ("-" * 80)

    def _act(self, sym, symSpec):
	self._reductions(sym, symSpec)
	self._shifts(sym, symSpec)

    def _reductions(self, sym, symSpec):
	# epsilons is a dictionary that maps production-->[tops].  The purpose
	# is to avoid repeating the same epsilon production on a particular
	# stack top.  Ordinary productions do not require this care because we
	# can notice when a path has already been used for a production.
	epsilons = {}

	if self._verbose:
	    nReduces = 0

	# Enqueue work.
	workQ = []
	i = 0
	while i < len(self._gss):
	    top = self._gss[i]
	    if symSpec not in self._spec._action[top.nextState]:
		# Unexpected token for this stack.
		self._gss.pop(i)
	    else:
		for action in self._spec._action[top.nextState][symSpec]:
		    if type(action) == ReduceAction:
			if len(action.production.rhs) == 0:
			    if action.production not in epsilons:
				assert len([path for path in top.paths(0)]) == 1
				path = [p for p in top.paths(0)][0]
				epsilons[action.production] = [top]
				workQ.append((path, action.production))
				if self._verbose:
				    print "   --> enqueue(a) %r" % \
				      action.production
				    print "                  %r" % path
			    elif top not in epsilons[action.production]:
				assert len([path for path in top.paths(0)]) == 1
				path = [p for p in top.paths(0)][0]
				epsilons[action.production].append(top)
				workQ.append((path, action.production))
				if self._verbose:
				    print "   --> enqueue(b) %r" % \
				      action.production
				    print "                  %r" % path
			else:
			    # Iterate over all reduction paths through stack and
			    # enqueue them.
			    for path in top.paths(len(action.production.rhs)):
				workQ.append((path, action.production))
				if self._verbose:
				    print "   --> enqueue(c) %r" % \
				      action.production
				    print "                  %r" % path
		i += 1

	# Process the work queue.
	while len(workQ) > 0:
	    (path, production) = workQ.pop(0)

	    if self._verbose:
		print "   --> reduce %r" % production
		print "              %r" % path
		nReduces += 1

	    self._reduce(workQ, epsilons, path, production, symSpec)

	if self._verbose:
	    if nReduces > 0:
		self._printStack()

    def _reduce(self, workQ, epsilons, path, production, symSpec):
	assert len(path[1::2]) == len(production.rhs)

	# Build the list of RHS semantic values to pass to the reduction action.
	rhs = [edge.value for edge in path[1::2]]

	# Call the user reduction method.
	r = self._production(production, rhs)

	below = path[0]
	done = False
	for top in self._gss:
	    if top.nextState == \
	      self._spec._goto[below.nextState][production.lhs]:
		# top is compatible with the reduction result we want to add to
		# the set of stack tops.
		for edge in top.edges():
		    if edge.node == below:
			# There is already a below<--top link, so merge
			# competing interpretations.
			if self._verbose:
			    print "   --> merge %r <--> %r" % (edge.value, r)
			value = production.lhs.nontermType.merge(edge.value, r)
			if self._verbose:
			    if value == edge.value:
				print "             %s" % \
				  ("-" * len("%r" % edge.value))
			    else:
				print "             %s      %s" % \
				  ((" " * len("%r" % edge.value)), \
				  "-" * len("%r" % r))
			edge.value = value
			done = True
			break
		if not done:
		    # Create a new below<--top link.
		    edge = Gsse(below, top, r)
		    if self._verbose:
			print "   --> shift(b) %r" % top

		    # Enqueue reduction paths that were created as a result of
		    # the new link.
		    self._enqueueLimitedReductions(workQ, epsilons, edge, \
		      symSpec)
		    done = True
		break
	if not done:
	    # There is no compatible stack top, so create a new one.
	    top = Gssn(below, r, \
	      self._spec._goto[below.nextState][production.lhs])
	    self._gss.append(top)
	    if self._verbose:
		print "   --> shift(c) %r" % \
		  self._spec._goto[below.nextState][production.lhs]
	    self._enqueueLimitedReductions(workQ, epsilons, top.edge, symSpec)

    # Enqueue paths that incorporate edge.
    def _enqueueLimitedReductions(self, workQ, epsilons, edge, symSpec):
	for top in self._gss:
	    if symSpec in self._spec._action[top.nextState]:
		for action in self._spec._action[top.nextState][symSpec]:
		    if type(action) == ReduceAction:
			if len(action.production.rhs) == 0:
			    if self._spec._goto[top.nextState] \
			      [action.production.lhs] == top.nextState:
				# Do nothing, since enqueueing a reduction
				# would result in performing the same reduction
				# twice.
				pass
			    elif action.production not in epsilons:
				path = [top]
				epsilons[action.production] = [top]
				workQ.append((path, action.production))
				if self._verbose:
				    print "   --> enqueue(d) %r" % \
				      action.production
				    print "                  %r" % path
			    elif top not in epsilons[action.production]:
				path = [top]
				epsilons[action.production].append(top)
				workQ.append((path, action.production))
				if self._verbose:
				    print "   --> enqueue(e) %r" % \
				      action.production
				    print "                  %r" % path
			else:
			    # Iterate over all reduction paths through stack and
			    # enqueue them if they incorporate edge.
			    for path in top.paths(len(action.production.rhs)):
				if edge in path[1::2]:
				    workQ.append((path, action.production))
				    if self._verbose:
					print "   --> enqueue(f) %r" % \
					  action.production
					print "                  %r" % path

    def _shifts(self, sym, symSpec):
	prevGss = self._gss
	self._gss = Gss(self)

	if self._verbose:
	    nShifts = 0

	for topA in prevGss:
	    if symSpec in self._spec._action[topA.nextState]:
		for action in self._spec._action[topA.nextState][symSpec]:
		    if type(action) == ShiftAction:
			merged = False
			for topB in self._gss:
			    if topB.nextState == topA.nextState:
				Gsse(topA, topB, sym)
				merged = True
				break
			if not merged:
			    top = Gssn(topA, sym, action.nextState)
			    self._gss.append(top)
			    if self._verbose:
				print "   --> shift(a) %d" % action.nextState
				nShifts += 1
	if self._verbose:
	    if nShifts > 0:
		self._printStack()

    def _printStack(self):
	i = 0
	for top in self._gss:
	    for path in top.paths():
		if i == 0:
		    print "STK 0:",
		else:
		    print "    %d:" % i,
		for elm in path:
		    print "%r" % elm,
		print
		i += 1
