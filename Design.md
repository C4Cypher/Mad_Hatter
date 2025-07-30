# A formal specification for the Mad Hatter programming language 
	by C4Cypher
# Defintions, Syntax and Semantics
	## Syntax
		### String
			A sequence of charachters, usually in the form of an unboxed char 
			array. When enclosed by single or double quotes, are parsed as
			literal string values.  Note that paired double or singue quotes
			inside of a string are parsed to represent a single single or 
			double quote charachter.   `"foo""bar" == 'foo"bar'`
			
		### White space
			White space charachters (chars) are spaces, tabs, carrige returns 
			and new lines, they are used to seperate other parsed tokens,
			otherwise the arrangement of which has no influence on 
			Mad Hatter's syntax. An un-interrupted series of white space
			charachters in source code are referred to as 'White Space'.
			
		### Literal
			A literal is a string of charachters that denote a literal 
			constant value, including strings contained with single or double
			quotes, integers or floating point values, they are parsed into
			values directly by the parser.
			
		### Identifier
		
			an identifier is a string of letters (upper or lower case), 
			underscores or digits that represents an element of a
			Mad Hatter program. An identifier may not start with a digit.
			
		### Operator
			An operator is a specialized identifier with unique handling by
			the parser, as such operators have unique handling and rules, 
			usually being parsed into compound terms. As such, the string
			that represent them are not subject to the same constraints as
			normal identifiers. Current implementation uses the Mercury Term 
			parser, thus follows Mercury's operator table with some 
			modifications.
			
		### Symbol `~ string`
			identifiers that start with a lower case letter are symbols, 
			strings	that represent the unique representation of atoms
			
		### Variable Name
			identifiers that start with an upper case letter or an underscore 
			are variable names. Variables are identified numerically by integer
			index, yet Mad Hatter may keep track of the string names that
			originally identified Variables during parsing in order to generate
			more useful error messages. Note that a variable identified by
			a single underscore `_` is considered to have no variable name.
			Variable names that start with an underscore retain their names,
			however denotationally they are considered to be 'unused'.
			Mad Hatter may generate warnings or error messages if such an
			'unused variable' is bound to more than one meaningful term,
			resulting in a unification.
			
		### Variable ID `var_id`
			A unique identifier for variables, meant to be generated and used
			internally by the compiler, usually visible only as the identifying
			component of a variable
			
		### Argument list `args`
			a list of terms, seperated by commas contained inside parens,
			curly braces or square braces
			
	## Semantics
	
		### term
			Fundamental element of a Mad Hatter program, serves as a node in 
			Mad Hatter's abstract syntax tree
			Union of the following:
			
			#### nil
				`nil`, `~ "nil"`	
				a nil value, absence of value, may unify with an empty tuple
				
			#### atom
				`atom(symbol)`
				an atomic term, or 'atom', represents a concept that holds no 
				other values outside of it's identifying symbol and any 
				relations that may give the atom further meaning
				
			#### variable
				`var(var_id)`
				a variable that may be bound to another term in a given
				context
				
			#### compound terms
				`cons(car::term, cdr::term)`
				`car(cdr)`
				`'()'(car, cdr)`
				an application of the head 'car' (a relation in the
				vast majority of cases) to the monoid argument 'cdr'
				to represent another term.
				
				A compound term with multiple arguments `car(term, term, ...)`
				Is syntax sugar for a tuple of the arguments as  the 'cdr'
				
				##### tuple
				`{ term, term, ... }` 
				`[ term | tuple ]`
				`[]`
				`[ term, term ]` == `[ term | [ term | [] ] ]`
				While tuples are represented as their own branch of the 'term'
				type in the Mercury prototype implementation of Mad Hatter,
				semantically tuples should be considered a type class of terms 
				that contains an ordered collection of terms.  
				
				Internally, tuples may be represented by either boxed arrays
				or linked list, but any tuple should be able to be indexed 
				like an array or deconstructed like a linked list. More
				importantly, the compiler is free to convert a tuple into
				a form of it's choosing at any time during compilation.
				
				The tuple constructors suggest which kind of structure should
				be used to store the tuple, `{}/1` for arrays and `[|]/2`  
				for linked lists.  Note that `[]/1` unifes with *any* empty
				tuple, and `[|]` will deconstruct the first element of *any*
				tuple, along with the remainder.  A comma seperated list in
				square brackets `[ term, term, term , ...]` will be pre-parsed
				into `[|]/2` `[]/1` form.
				
				Like comma seperated list constructors, arguments of a 
				compound term `car(arg, ...)` will be parsed as if the
				arguments were constructed with a tuple as mentioned above
				
			#### constraint
				`?term`
				The lazy operator `?/1` defines a constraint, a non-ground
				term with a build in guard relation that narrows which term
				any variable bound to the constraint may successfully unify 
				with. If the argument of the constraint does itself evaluate to
				(or just is) a ground term, it may itself be considered 
				semantically ground, yet the evaulation of said argument is
				delayed until the constraint is unified with another ground
				term and is treated as non ground until such a unification
				is ground, or the term is explicitly checked for groundness and
				contains no branches (disjunctions).
				
				If a constraint is unified with another constraint, a seperate
				check is made to see if the constraints contradict, failing
				the unification if so, and evaluating *both* terms into a
				conjunction of constraints `(?A, ?B)` if not.
				
				Note that a constraint behaves slightly differently depending
				on whether or not it is being unified directly with another
				term, or if a variable bound to it is being unified. Upon
				direct unification, the unification may be tabled, but the
				constraints themselves are not modified, however, if a variable
				bound to a constraint is unified, the variable being unified
				then becomes bound to the result of the unificaiton, either
				a ground term, a conjunction of constraints, or a failure in
				the event of a contradiction or direct failure to unify.
				
				Constraints and lazy terms repesent my most radical departure
				from traditional Prolog semantics, and I desired them to be
				a core component of the lanaguage and execution model, rather
				than a language extention, because my desire for a more fluid
				dependent types style of type system with some of the
				advantages of static typing ... I came to realize that
				a constraint propogation system would need to replace the
				traditional HM type and typeclass system entirely in order
				to facilitate such dependent typing.
				
				Effectively, every constraint forms a 'type class' of terms
				that it succeeds on ... some constraints may be bounded,
				forming a finite set or disjunction of terms it might unify
				with, or unbounded, the set of valid terms it successfully
				unifies with either being infinite, or in the case of some
				data value types (ints, floats), so close to infinite that they
				might as well be.
				
				This completely breaks the mold on traditional Prolog style
				programming, and I'm developing it as I go, hoping to see
				if I can get a decidable, functional programming language out
				of it, as such, the semantics for Mad Hatter as a whole, 
				and constraints in paticular may change as I fit things
				together.
				
	### Operations
	
		Symbol operator '~', `~ string`
		Return operator '->' '<-',  `term -> term`
		
			Subset of unifcation, indicates a one-way reduction of one term to
			another.
		
		The functor containing the relation that 
		
		Unification '=' ( <->?)
		
		propositional operation that holds if all arguments unify
		As a relation, returns a success containing the varia