A formal specification for the Mad Hatter programming language, by C4Cypher

Defintions, Syntax and Semantics
	Syntax
		String
			A sequence of charachters, usually in the form of an unboxed char 
			array. When enclosed by single or double quotes, are parsed as
			literal string values.  Note that paired double or singue quotes
			inside of a string are parsed to represent a single single or 
			double quote charachter.   "foo""bar" == 'foo"bar'
		White space
			White space charachters (chars)' are spaces, tabs, carrige returns 
			and new lines, they are used to seperate other parsed tokens,
			otherwise the arrangement of which has no influence on 
			Mad Hatter's syntax. An un-interrupted series of white space
			charachters in source code are referred to as 'White Space'.
		Literal
			A literal is a string of charachters that denote a literal 
			constant value, including strings contained with single or double
			quotes, integers or floating point values, they are parsed into
			values directly by the parser.
		Identifier
			an identifier is a string of letters (upper or lower case), 
			underscores or digits that represents an element of a
			Mad Hatter program. An identifier may not start with a digit.
		Operator
			An operator is a specialized identifier with unique handling by
			the parser, as such operators have unique handling and rules, 
			usually being parsed into compound terms. As such, the string
			that represent them are not subject to the same constraints as
			normal identifiers. Current implementation uses the Mercury Term 
			parser, thus follows Mercury's operator table with some 
			modifications.
		Symbol '~ string'
			identifiers that start with a lower case letter are symbols, 
			strings	that represent the unique representation of atoms
		Variable Name
			identifiers that start with an upper case letter or an underscore 
			are variable names. Variables are identified numerically by integer
			index, yet Mad Hatter may keep track of the string names that
			originally identified Variables during parsing in order to generate
			more useful error messages. Note that a variable identified by
			a single underscore '_' is considered to have no variable name.
			Variable names that start with an underscore retain their names,
			however denotationally they are considered to be 'unused'.
			Mad Hatter may generate warnings or error messages if such an
			'unused variable' is bound to more than one meaningful term,
			resulting in a unification.
		Variable ID 'var_id'
			A unique identifier for variables, meant to be generated and used
			internally by the compiler, usually visible only as the identifying
			component of a variable
		Argument list 'args'
			a list of terms, seperated by commas contained inside parens,
			curly braces or square braces
			
	Kinds
		Term
			Fundamental element of a Mad Hatter program, serves as a node in 
			Mad Hatter's abstract syntax tree
			Union of the following:
				~ "nil"	
					a nil value, absence of value, may unify with an empty
					tuple
				atom(symbol)
					an atomic term, or 'atom', represents a concept that
					holds no other values outside of it's identifying symbol
					and any relations that may give the atom further meaning
				var(var_id)
					a variable that may be bound to another term in a given
					context
				Compound terms
					cons(car::term, cdr::term)
						an application of the head `car` (a relation in the
						vast majority of cases) to the monoid argument 'cdr'
						to represent another term
					{ Argument}
					
	Operations
	Symbol operator '~', ~ string
	Return operator '->' '<-',  term -> term
		Subset of unifcation, indicates a one-way reduction of one term to
		another.
		
		The functor containing the relation that 
		
	Unification '=' ( <->?)
		propositional operation that holds if all arguments unify
		As a relation, returns a success containing the varia