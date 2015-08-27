CHR2 to SQL converter
=====================

The library implements Constraint Handling Rules (CHR) solver by converting 
the rules into  set of SQL scripts. So the solver can work with constraints 
sets. The syntax is similar to the one defined by Peter Van Weert in "Extension 
and optimizing compilation of constraint handling rules" aka CHRv2.

One of the features CHR systems are considered different to rule systems is 
their matching algorithm. Rule systems typically use eager matching and CHR 
utilizes lazy matching. In eager matching (for example RETE algorithm) first 
all possible matches of rules LHS are built. And only after one of matching 
rule is selected and fired. Firing means executing matched rule body which in 
turn adds or removes objects from the set being matched. This invalidates 
previously eagerly computed match. The matching algorithms are incremental but 
nevertheless much of work done on matching is wasted. Incrementality maintenance 
overhead is significant too. On the other hand lazy algorithms (used in CHR 
systems) only compute matching for one rule to fire. Typically LEAPS is used in 
CHR systems as matching algorithm. And in papers advocating CHR this is 
reported to have better performance characteristics. CHR2SQL system is 
experimental step to even more eager direction. It not only eager in matching 
it is also applies all matched rules immediately without conflicts resolution 
and without matched constraint set invalidation. It still uses CHR syntax so 
that’s why CHR is in its name. This system preserves logical reading of CHR 
rules, but operational semantics is obviously different to state of art CHR 
systems.

Usually CHR systems conform to 2 levels of operational semantics, namely 
abstract and refined. The abstract one is simpler but not deterministic. 
Typically systems parallelizing solving process conform to just abstract 
semantics. It is considered not usable in practice. Systems conforming refined 
semantics are much more usable but they may be considered as usual general 
programming language programs, and they are hard to parallelize implicitly. 
There are extensions (utilized in CHRv2) adding notion of priority to rules. 
That priorities orders and execution of rules but still leaves space for easy 
parallelization (not implemented as far as I'm aware).

CHR2SQL implements some semantics which is somewhere between abstract and 
refined ones. And it doesn't implement priorities, but still it sounds quite 
usable on samples from CHR text books. I don’t have any formal proof and I don’t 
have any plans to do them. That’s more a kind of empirical vision. Original 
motivation for such system is from one of my projects utilizing equality 
saturation technique for some modeling task. It was implemented using Leuven 
CHR System on SWI-Prolog. But equality saturation requires huge number of 
constraints. So they not always fit in memory. The obvious solution to this is 
to use some DBMS. And this library is an attempt to implement such CHR system.

CHR2SQL commits rules actions in batches. It uses INSERT … SELECT statements,
UPDATEs for a large number of rows etc. Others CHR system has only one active 
constraint. It is very similar to just procedure call. And this makes such 
systems to be a general programming language. SQL version could use single 
active constraint too, and it would even fix memory problem for large 
constraints set, but it would lose many advantages SQL provides for batch data 
processing.

For example a simple solver from CHR2 thesis implementing Dijkstra’s shortest 
path algorithm.

```prolog
init @ +source(V) => dist(V,0).
keep_shortest @ +dist(V,D1), -dist(V,D2), D1=< D2.
label(D) @ +dist(V,D), +edge(V,C,U) => dist(U,D+C).

priority keep_shortest > label(_),
label(X) > label(Y) if X < Y.
```

It displays needs for dynamic priorities in the thesis. The first rule (init) 
starts the search, the second rule (keep_shortest) removes bigger found 
distance if 2 of them found to the same node. And the third (label) actually 
traverses the graph by edges.

If there were no priorities and the system would have abstract semantics because
it runs each rule in separate thread and if the third rule thread has bigger 
priority than the second the solver process may run in infinite loop if graph 
has cycles. CHR2SQL doesn't implement priorities but it successfully solves the
problem and stops. It is because it handles all data in batches. Even if there 
are two solver threads and the one implementing “label” manages to perform a few 
steps until “keep_shortest” runs, that “keep_shortest” will remove all the 
redundant nodes at once. It resembles breadth-first search for solution while 
original CHR uses depth-first. CHR2SQL does perform some redundant job but it 
is a kind of little amount of speculative execution, it can be ignored. These 
just 3 lines of code implements useful task and can cope with quite big data.


