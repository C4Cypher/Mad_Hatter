Mad-hatter
==========

Mad-hatter is something of a work in progress.  I started on this project
with the intent of making a toy logic/functional programming language of
my own, along similar lines to Mercury. Indeed, so far the project is written
completely in Mercury.  I was frustrated with the strict limitations Mercury
placed on it's semantics for the sake of efficient compilation and wanted
to experiement with my own ideas for evauation of logic programs. However,
while I do have access to the source distribution for the Melbourne Mercury
Compiler, it's complexity and depth in implementation is a bit too deep for
me to grasp enough to experiment with.

Thus I decided to make my own logic programming language with blackjack and 
hookers.  I called it Mad-Hatter: The result of too much Mercury on the brain.
My primary goal with it is to learn through it's design and implementation.
As such, the specific design goals for the language have evolved over time
and will continue to evolve.

Design Goals *(so far)*
=======================

Powerful
--------

Expressive syntax that adapts Prolog style terms to ML or Haskell style 
functional code. Predicates, functions are first order and are to be handled
as closely to normal values as possible, including via unification. I'm
blurring the lines between dynamic and static typing by using a lazy constraint
system that leverages nondeterministic evaluation and tree pruning. This will
also apply to type classes, which, (hopefully) will facilitate extremely
flexible dependent typing mechanisms.

Pure
----

Mad Hatter is intended to (mostly) be entirely 'pure' in both a
functional and logical sense.  Side effects are to be represented explicitly
by new unique values returned by procedures.  Abstract concepts that cannot
be represented in completely pure code (such as IO operations) will be handled
through abstractions such as state passing or Monads.

Nondeterministic
----------------

Logic programs can be, by nature, nondeterministic. Not all procedures will
return a single ground value deterministically, but possibly multiple values,
or none. The Prolog Warren Abstract Machine handles this by evaluating each
nondeterministic result linearly (and naievely), until each output has been
handled.  I wish to take a different approach, by representing nondeterministic
output via tree structures, these disjunction/conjunction tries can represent
multiple results at once, can be queried efficiently, and can be pruned by
constraints, hopefully minimizing the amount of naive or semi-naieve
nondeterministic evaluation required. Furthermore, a procedure only fails, not
if it cannot produce a ground value as a result, but if it cannot produce *any*
result.  A procedure may succeed by placing additional constraints on free
variables, even if said variables are unable to be grounded. Leads us to ...

Lazy
----

Both Logic and Functional programming can leverage huge advantages both in time
and space efficiency by utilizing lazy evaluation teqhniques. The Haskell
langauge stands out for pushing this concept to it's limits with great effect.
The issue with this approach is that it can be difficult for these evaulation
strateges to grasph for newcomers to the Logic and Functional programming
paradigms.  I aim to take an 'eager' approach to ground values assigned to 
variables, yet a 'lazy' approach to constraints (be it type, class, or value 
constrant). If a procedure cannot logically bind a variable to a ground value,
it may instead bind it to a constraint that will be lazily applied to any
future value (ground or otherwise) the variable is unified with.

Design by Constraint
--------------------

Lorum ipsum

Flexible
-------

I have not yet settled on a backend or ends ... and I am painfully aware that
the language I am developing will be painfully slow and inefficient if
evaluated interpetively. 

Different applications and different platforms have different requirements
and tools availible to evaluate programs efficiently. To this end I aim
to make my back end code generation structure modular, so that the language
might be extended to multiple back ends (GCC, LLVM, CLI, etc), platforms
(Windows, Linux, ARM, etc), and practical applications (User applications, 
runtime libraries, server side services, etc).  Closely related ...

Fast *(sort of)*
----------------

Imperitive low level code is fast, functional code is easily compiled to 
imperative low level code. Logic programs are neither fast nor efficently 
compiled to deterministic functional or imperative code.  By nature, Mad-Hatter
will not be a quick or efficient *(naively)* interpreted scripting language.
However, I consider *Compile time* to be almost 'free' in terms of speed
concerns.  Yes, having a fast compiler is desireable; nobody wants to sit for
minutes to hours waiting on code to compile each time.  I aim to construct
a compiler that can either genrate *fast* compiled code out of the gate, if
possible, *OR* when everything *can't* be resolved and optimized at compile 
time, pre-compile as much as it can into a fast and efficient IR that can be
Just In Time compiled (hopefully at 'load' time, rather than real time) to
properly suit the given application's runtime requirements.

Given that I'm still hashing out the front end, these last goals are future
plans, but I'm considering them now as I design the langauge.