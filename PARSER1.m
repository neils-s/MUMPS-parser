PARSER1
	; Copyright (C) 2013 Neils Schoenfelder
	; 
        ; This program is free software; you can redistribute it and/or
        ; modify it under the terms of the GNU General Public License
        ; as published by the Free Software Foundation; either version 2
        ; of the License, or (at your option) any later version.
        ; 
        ; This program is distributed in the hope that it will be useful,
        ; but WITHOUT ANY WARRANTY; without even the implied warranty of
        ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        ; GNU General Public License for more details.
        ; 
        ; You should have received a copy of the GNU General Public License
        ; along with this program; if not, write to the Free Software
        ;   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
        ; 
	; 
	d tester() q  ; Put in a hook that will redirect to the tester
	;
	;
	; =====  OVERVIEW  ====
	; This is a parser for any language that  can be expressed using a syntax diagram consisting only of:
	;	1) string literals
	;	2) options
	;	3) delimited lists
	;	4) named tokens.
	; Note that it's possible to draw a syntax diagram that isn't of this form, but that's actually pretty tricky.
	;
	;
	; ====  TECHNICAL TERMINOLOGY  ====
	; The parser uses a sytax map, which is of the following form:
	;	@map@(token1Name) = subtree1
	;	@map@(token2Name) = subtree2
	;	                 ...
	;	@map@(tokenName)  = subtree
	; where each subtree is a MUMPS array describing the structure of each corresponding token. The subtrees are described in greater detail below.
	; For now, just observe that the syntax map requires at least one token definition, and the first coordinate of the @map array consists of only token names.
	;
	; The structure of the subtrees, is of the generic form:
	;		@subtree = subtree type
	;		@subtree@("force") = the behavior of the parser to force when parsing any literals defined in this or deeper subtrees.
	;		@subtree@(subnodes) = additional data.
	; The additional data can be quite extensive, and include references to other tokens, lists of options, string literal, etc.
	;
	; We allow these 6 types of subtrees:
	;	1) "" to denote an empty subtree.  This is typically used with "options" and sometimes with "delimList".
	;	2) "literal" for string literals, 
	;	3) "subtreeChain" to allow us to easily concatenate multiple subtrees together.
	;	4) "options" to denote a list of options,
	;	5) "delimList" to denote a delimited list, 
	;	6) "token" to reference another token.  Effectively, this is just a named subtree.
	; The particular structure of each type of subtree is described below
	;
	; 1) empty:
	;		@subtree=""
	; This is a necessary part of any grammer.  It's often used as a subtree in "options" to allow truly optional grammer.
	; Subtrees of this type are not allowed to have any finer structure.  In other words, they are not allowed to have any subnodes.
	;
	; 2) literal:
	;		@subtree = "literal"
	;		@subtree@("value") = the specific string literal 
	;		@subtree@("force") = any behavior of the parser to force when parsing this literal subtree.
	;				(currently, the only allowed value is "forceToLowerCase" to force input to be automatically changed to lowercase)
	; This is the basic building block of any grammer.
	;
	; 3) subtreeChain:
	;		@subtree = "subtreeChain"
	;		@subtree(1) = the 1st subtree making up the subtree chain.  Any valid @map@ root is valid here (without the human-readable description).
	;		@subtree(2) = the 2nd subtree making up the subtree chain.  Any valid @map@ root is valid here (without the human-readable description).
	;		...
	;		@subtree(n) = the nth subtree making up the subtree chain.  Any valid @map@ root is valid here (without the human-readable description).
	;		@subtree@("force") = the behavior of the parser to force when parsing any literals defined in this or deeper subtrees.
	; This is the way to string together a list of other subtrees that must occur one after another.
	; For instance, you could use this type of subtree to define a structure of the form "(" + somethingElse + ")".
	;
	; 4) options:
	;		@subtree = "options"
	;		@subtree@(1) = 1st option.  Any valid subtree is valid here
	;		@subtree@(2) = 2nd option.  Any valid subtree is valid here
	;		...
	;		@subtree@(n) = nth option.  Any valid subtree is valid here.
	;		@subtree@("force") = the behavior of the parser to force when parsing any literals defined in this or deeper subtrees.
	; The straight-forward way to describe alternatives.
	;
	; 5) delimList:
	;		@subtree = "delimList"
	;		@subtree@("delimiter") = A subtree defining the delimiter(s) of the list.  Any valid subtree is valid here.
	;		@subtree@("content") = A subtree defining the contents of the list.
	;				       Any valid subtree is valid here, including nested delimited lists and options.
	;		@subtree@("force") = the behavior of the parser to force when parsing any literals defined in this or deeper subtrees.
	; Warning: It is frighteningly easy to create an ambiguous grammer if both the "delimiter" and "content" subtrees are complicated or contain overlapping symbols.
	;
	; 6) token:
	;		@subtree = "token"
	;		@subtree@("value") = the name of the token.  This should match some name occuring in the first coordinate of the @map
	;		@subtree@("force") = the behavior of the parser to force when parsing any literals defined in this or deeper subtrees.
	; The way to refer to any other token defined in 'map'
	;
	; The parser runs against a stream data structure containing lines of code to be parsed.
	; Multiline data can be parsed from the stream by including the appropriate carriage returns and line breaks as part of the stream.
	; (It is necessary to parse multiple lines in the ANSI 1995 MUMPS standard in order to handle external references properly.)
	; The stucture of the data to be parsed is necessarilly of the form:
	;	@codePointer = code to be parsed
	; Note that this structure pretty much sucks in MUMPS implementations where the max line length storable in a variable could be limited to 32000 characters.
	; It is, however, a great prototype of a stream reader, which is generic.
	;
	;
	; ====  EXAMPLES  ====
	; For example, say a token is made up of a literal string 'foo(', followed by a FEEP token, followed by a literal string ')bar'.
	; We could define this as follows:
	;               @map@(tokenName)="subtreeChain"
	;		@map@(tokenName,1)="literal"
	;		@map@(tokenName,1,"value")="foo("
	;		@map@(tokenName,2)="token"
	;		@map@(tokenName,2,"value")="FEEP"
	;		@map@(tokenName,3)="literal"
	;		@map@(tokenName,3,"value")=")bar"
	;
	; Here are some valid examples of maps:
	;	@map@("token1")="literal"
	;	@map@("token1","value")="TOKEN_ONE"
	; The only example of this token is 'TOKEN_ONE'
	;
	;       @map@("token2")="subtreeChain"
        ;	@map@("token2",1)="literal"
	;	@map@("token2",1,"value")="("
	;	@map@("token2",2)="token"
	;	@map@("token2",2,"value")="token1"
	;	@map@("token2",3)="literal"
	;	@map@("token2",3,"value")=")"
	; The only example of this token is '(TOKEN_ONE)'
	;
	;	@map@("token3") ="subtreeChain"
	;	@map@("token3",1)="literal"
	;	@map@("token3",1,"value")="[["
	;	@map@("token3",2)="delimList"
	;	@map@("token3",2,"delimiter")="literal"
	;	@map@("token3",2,"delimiter","value")="::"
	;	@map@("token3",2,"content")="token"
	;	@map@("token3",2,"content","value") = "token1"
	;	@map@("token3",3)="literal"
	;	@map@("token3",3,"value")="]]"
	; The following are examples:  '[[TOKEN_ONE]]' , '[[TOKEN_ONE::TOKEN_ONE]]' , and '[[TOKEN_ONE::TOKEN_ONE::TOKEN_ONE::TOLEN_ONE]]'
	; Notice that delimList does *not* allow empty lists of the content
	;
	; Here's a token describing an optional choice between token1, token3, and the string 'TOKEN_NULL' followed by the equal sign and an occurence of token2
	;	@map@("token4")="subtreeChain"
	;	@map@("token4",1)="options"
	;	@map@("token4",1,1)="token"
	;	@map@("token4",1,1,"value")="token1"
	;	@map@("token4",1,2)="token"
	;	@map@("token4",1,2,"value")="token3"
	;	@map@("token4",1,3)="literal"
	;	@map@("token4",1,3,"value")="TOKEN_NULL"
	;	@map@("token4",2)="literal"
	;	@map@("token4",2,"value")="="
	;	@map@("token4",3)="token"
	;	@map@("token4",3,"value")="token2"
	; Examples include:  'TOKEN_ONE=(TOKEN_ONE)' , '[[TOKEN_ONE]]=(TOKEN_ONE)' , and 'TOKEN_NULL=(TOKEN_ONE)'
	;
	; A token describing a case-insensitive version of token1
	;	@map@("token5")="token"
	;       @map@("token5","value")="token1"
        ;	@map@("token5","force")="forceToLowerCase"
	; Examples include 'TOKEN_ONE', 'token_one', and 'ToKeN_OnE'
	;
	; A token describing parenthesis with recursion:
	;	@map@("token6")="options"
	;	@map@("token6",1)="" ; the empty option
	;	@map@("token6",2)="subtreeChain"
	;	@map@("token6",2,1)="literal"
	;	@map@("token6",2,1,"value")="("
	;	@map@("token6",2,2)="token"
	;	@map@("token6",2,2,"value")="token6"
	;	@map@("token6",2,3)="literal"
	;	@map@("token6",2,3,"value")=")"
	;	@map@("token6",3)="token"
	;	@map@("token6",3,"value")=token1
	; Examples of token6 include "" (the empty string), TOKEN_ONE,  (TOKEN_ONE),  ((((TOKEN_ONE)))), etc.
	;
	;
	; ====  CAUTIONARY NOTES  ====
	; The parser uses depth-first recursive parsing without back-tracking, so it will go into an infinite loop if it hits a left-recursive definition.
	; An example of a left recursive definition in @map would be this:
	;		; The digits 0-9:
	;		@map@("digit") = "options"
	;		@map@("digit",1)="literal"
	;		@map@("digit",1,"value")="1"
	;		@map@("digit",2)="literal"
	;		@map@("digit",2,"value")="2"
	;		@map@("digit",3)="literal"
	;		@map@("digit",3,"value")="3"
	;		@map@("digit",4)="literal"
	;		@map@("digit",4,"value")="4"
	;		@map@("digit",5)="literal"
	;		@map@("digit",5,"value")="5"
	;		@map@("digit",6)="literal"
	;		@map@("digit",6,"value")="6"
	;		@map@("digit",7)="literal"
	;		@map@("digit",7,"value")="7"
	;		@map@("digit",8)="literal"
	;		@map@("digit",8,"value")="8"
	;		@map@("digit",9)="literal"
	;		@map@("digit",9,"value")="9"
	;		@map@("digit",10)="literal"
	;		@map@("digit",10,"value")="0"
	; This part of the map just defines the digits 0 through 9.  No recursion here.
	;
	;		; A generic expression of digit addition
	;		@map@("expr") = "options"
	;		@map@("expr",1) = "token"
	;		@map@("expr",1,"value")="digit"
	;		@map@("expr",2) = "token"
	;		@map@("expr",2,"value") = "sum"
	;
	;		; A sum of 2 expressions
	;               @map@("sum")="subtreeChain"
        ;		@map@("sum",1)="token"
	;		@map@("sum",1,"value")="expr"
	;		@map@("sum",2)="literal"
	;		@map@("sum",2,"value")="+"
	;		@map@("sum",3)="token"
	;		@map@("sum",3,"value")="expr"
	; The problem here is that to match a line of input to an "expr", we'd need to check if the line of code was a "sum".
	; But to check if it's a "sum", we need to check if it's an "expr"+"expr"; which, unfortunately, requires us to try and and match the line of input
	; to an "expr"...
	; This would trap us in an infinite loop.
	;
	; More generally this map admits a recursive definition that doesn't consume at least one character of the input from the left of the input string.
	; Any such map is called "left-recursive", and it will trap our parser in an infinite loop.
	;
	; The way to avoid left-recursive maps to make sure that every recursive cycle consumes at least one character of the input string.
	; (This is a simple monotonicity condition.)
	;
	;	
	; *******************************
	;
	;
	; This is the core parsing function that handles generic subtrees.
	; The parameters are as follows:
	;	subTreePointer - a string pointer to the fragment of map describing the subtree we're parsing with
	;	outParseTreePointer - a string pointer to the array in which the parse data will be held.
	;	mapPointer - a string pointer to the array holding the map for parsing the tokens.
	;		(The map defined by @mapPointer must not have any instances of left-recursion!)
	;	codePointer - a string pointer to the array holding the code to be parsed.
	;	startColumn - the column of @codePointer to start the parse at.
	;	forceParseAs - If we want to force the parsing as a specific token, this parameter is used.  If this isn't set, then every token in @map will be tried.
	; Returns the number of characters parsed.  Returns negative numbers in case of error.
parseSubtree(subTreePointer,outParseTreePointer,mapPointer,codePointer,startColumn) q:$g(subTreePointer)="" "-1,missing subtreePointer"
	; Recall that subtrees are of the generic form:
	;		@subtree = subtree type
	;		@subtree@(subnodes) = additional data.
	; We allow these types of subtrees:
	;	1) "" to denote an empty subtree.  This is typically used with "options" and sometimes with "delimList".
	;	2) "literal" for string literals, 
	;	3) "subtreeChain" to act as a string of subtrees when creating a new token is too much of a pain.
	;	4) "options" to denote a list of options,
	;	5) "delimList" to denote a delimited list, 
	;	6) "token" to reference another token
	;
	n subTreeType,charsParsed,literalValue,tokenName,contentSubTreePointer,delimiterSubTreePointer
	s subTreeType=@subTreePointer
	;
	; The subtrees allow parsing behavior to be forced
	i $g(@subTreePointer@("force"))'="" n %parseControlForcing s %parseControlForcing=@subTreePointer@("force")
	; The broadly scoped variable %parseControlForcing is in scope for this function and all called functions.
	; It will persist to deeper stack levels to change the parsing behavior.
	;
	i subTreeType="" d
	. i $d(@subTreePointer)<10 s charsParsed=0 s @outParseTreePointer=""
	. e  s charsParsed="-4,subtrees with no type should be empty arrays." ; this is one way to limit left-recursive structures in the grammer
	;
	i subTreeType="literal" d
	. s literalValue=@subTreePointer@("value")
	. s charsParsed=$$parseLiteral(literalValue,$g(%parseControlForce),outParseTreePointer,mapPointer,codePointer,startColumn)
	; 
	i subTreeType="subtreeChain" d
	. s charsParsed=$$parseSubtreeChain(subTreePointer,outParseTreePointer,mapPointer,codePointer,startColumn)
	;
	i subTreeType="options" d
	. s charsParsed=$$parseOptions(subTreePointer,outParseTreePointer,mapPointer,codePointer,startColumn)
	; 
	i subTreeType="delimList" d
	. s contentSubTreePointer=$name(@subTreePointer@("content"))
	. s delimiterSubTreePointer=$name(@subTreePointer@("delimiter"))
	. s charsParsed=$$parseDelimList(contentSubTreePointer,delimiterSubTreePointer,outParseTreePointer,mapPointer,codePointer,startColumn)
	;
	i subTreeType="token" d
	. s tokenName=@subTreePointer@("value")
	. s charsParsed=$$parseToken(tokenName,outParseTreePointer,mapPointer,codePointer,startColumn)
	;
	i '$d(charsParsed) s charsParsed="-2,unknown subTreeType: "_subTreeType BREAK  ; TODO:  Get rid of BREAK
	;
	q charsParsed
	;
	;
	; This parses subtreeChains.
	; As a reminder, the structure of a subTreeChain is:
	;		@subtree = "subtreeChain"
	;		@subtree@("force") = the behavior of the parser to force when parsing any literals defined in the deeper subtrees.
	;				(currently, the only allowed value is "forceToLowerCase" to force input to be automatically changed to lowercase)
	;				The forced behavior affects the parsing of this subtree and all sub-subtrees.
	;		@subtree(1) = the 1st subtree.  Any valid @map@ root is valid here (without the human-readable description).
	;		@subtree(2) = the 2nd subtree.  Any valid @map@ root is valid here (without the human-readable description).
	;		...
	;		@subtree(n) = the nth subtree.  Any valid @map@ root is valid here (without the human-readable description).
	; The parameters are as follows:
	;	subTreePointer - a string pointer to the fragment of map describing the subtree we're parsing with
	;	outParseTreePointer - a string pointer to the array in which the parse data will be held. This will hold a value of the form:
	;			      @outParseTreePointer@(1) = the parse tree of subtree 1
	;			      @outParseTreePointer@(2) = the parse tree of subtree 2
	;			      			      ...
	;			      @outParseTreePointer@(n) = the parse tree of subtree n
	;	mapPointer - a string pointer to the array holding the map for parsing the tokens.
	;		(The map defined by @mapPointer must not have any instances of left-recursion!)
	;	codePointer - a string pointer to the array holding the code to be parsed.
	;	startColumn - the column of @codePointer to start the parse at.
	; Returns the number of characters parsed or a negative number on an error.
parseSubtreeChain(subTreePointer,outParseTreePointer,mapPointer,codePointer,startColumn)
	q:$g(subTreePointer)="" "-1,no subTreePointer."
	n tmpOutParseRootPointer,totalCharsParsed,tempCodePosition,subTreePosition,tempSubtreePointer,tempOutParseTreePointer,charsParsed
	;
	; We need to ensure uniqueness of the parse tree pointer in the entire frame stack.
	; This is because we merge the parse tree pointers between the levels of the stack.
	s tmpOutParseRootPointer="tmpPrs"_$st
	n @tmpOutParseRootPointer
	;
	s totalCharsParsed=0
	s tempCodePosition=startColumn
	; We parse, keeping a running total of the total characters processed until hitting an error (parsing negative characters) or running out of subtrees in the subtreeChain.
	f subTreePosition=1:1 q:'$d(@subTreePointer@(subTreePosition))  d  q:totalCharsParsed<0
	. s tempSubtreePointer=$name(@subTreePointer@(subTreePosition))
	. s tempOutParseTreePointer=$name(@tmpOutParseRootPointer@(subTreePosition))
	. ;
	. s charsParsed=$$parseSubtree(tempSubtreePointer,tempOutParseTreePointer,mapPointer,codePointer,tempCodePosition)
	. ;
	. i charsParsed<0 s totalCharsParsed=charsParsed q   ; if an error condition was returned, we just pass that back.
	. s totalCharsParsed=totalCharsParsed+charsParsed
	. s tempCodePosition=tempCodePosition+charsParsed
	;
	i totalCharsParsed'<0 m @outParseTreePointer=@tmpOutParseRootPointer s @outParseTreePointer=$$readCodeStream(codePointer,startColumn,totalCharsParsed)
	;
	q totalCharsParsed
	;
	;
	; This handles the parsing of multiple options.  It's our only choice driver in the parser.
	; Recall that the "options" type of subtree looks like this:
	;		@subtree = "options"
	;		@subtree@(1) = 1st option.  Any valid subtree is valid here
	;		@subtree@(2) = 2nd option.  Any valid subtree is valid here
	;		...
	;		@subtree@(n) = nth option.  Any valid subtree is valid here.
	; This function works by parsing each subtree and recording the number of characters each subtree parsed as.
	; The subtree that parsed as the largest number of characters is returned.
	; When parsing options, it's not uncommon for their to be multiple valid options.  
	; For instance, your grammer could define both "digit" and "number", which would make the parsing of "5" ambiuous.
	; In determining which option is the "best" one we use 2 strategies to resolve the ambiguity:
	;	A. 'Maximal Munch'.  The subtree representing the parsing path that consumes the greatest number of characters is considered the correct one.
	;	B. 'Last defined'.  If there is a tie between the amount of characters parsed by different options, the last of the tied options is considered correct.
	; This is similar to the PEG parser (http://en.wikipedia.org/wiki/Parsing_expression_grammar).
	; For example, say we have a subtree like this:
	;		@subtree="options"
	;		@subtree@(1)="token"
	;		@subtree@(1,"value")="digit"
	;		@subtree@(2)="token"
	;		@subtree@(2,"value")="integer"
	;		@subtree@(3)="token"
	;		@subtree@(3,"value")="nonNegReal" ; non-negative real number
	; (Where the 'digit','integer', and 'nonNegReal' tokens are defined elsewhere in some reasonable way.)
	; If we're asked to parse the sybol "5", then the parse of digit, integer, and nonNegReal will all return that a single character was parsed.
	; In this case, the value will be treated as a nonNegReal.
	; Note that cooking up a grammer where the "best" option is what you want can be deceptively hard.  YOU HAVE BEEN WARNED!!!
	;
	; The parameters are as follows:
	;	subTreePointer - a string pointer to the fragment of map describing the subtree we're parsing with
	;	outParseTreePointer - a string pointer to the array in which the parse data will be held.  The parsed data will be held as
	;			      @outParseTreePointer = the subtree of the "best" option
	;	mapPointer - a string pointer to the array holding the map for parsing the tokens.
	;		(The map defined by @mapPointer must not have any instances of left-recursion!)
	;	codePointer - a string pointer to the array holding the code to be parsed.
	;	startColumn - the column of @codePointer to start the parse at.
	; Returns the number of characters parsed.  This is negative if an error is encountered.
parseOptions(subTreePointer,outParseTreePointer,mapPointer,codePointer,startColumn) q:$g(subTreePointer)="" "-1,no subTreePointer"
	n tmpOutParseRootPointer,whichOption,tempSubTreePointer,tempOutParseTreePointer,amountParsed,maxParsed,bestOption
	;
	; We need to ensure uniqueness of the parse tree pointer in the entire frame stack.
        ; This is because we merge the parse tree pointers between the levels of the stack.
        s tmpOutParseRootPointer="tmpPrs"_$st
        n @tmpOutParseRootPointer
        ;
	f whichOption=1:1 q:'$d(@subTreePointer@(whichOption))  d
	. s tempSubTreePointer=$name(@subTreePointer@(whichOption))
	. s tempOutParseTreePointer=$name(@tmpOutParseRootPointer@(whichOption))
	. ;
	. s amountParsed=$$parseSubtree(tempSubTreePointer,tempOutParseTreePointer,mapPointer,codePointer,startColumn)
	. ;
	. i amountParsed<+$g(maxParsed) k @tempOutParseTreePointer q  ; We've done at least as well with previous options, so clean up and try another option.
	. i amountParsed<0 k @tempOutParseTreePointer q  ; encountered an error in parsing that tree, so we throw it away.
	. ;
	. ; At this point, we know we have a new "best" option
	. s maxParsed=amountParsed
	. k:$g(bestOption) tempParseTree(bestOption) ; We've got a new best option, so we don't the data from the old best option
	. s bestOption=whichOption ; Note that this only gets set if at least on option returns non-negative (e.g. no error in parsing).
	;
	; Handle the edge case of an empty list of options.
	i '$g(bestOption) s maxParsed="-3,no error-free parse option found"
	e  m @outParseTreePointer=@tmpOutParseRootPointer@(bestOption)
	;
	q maxParsed
	;
	;
	; This parses a delimited list.
	; Note that in general, a delimited list is composed of a content subtree and a delimiter subtree, either of which could be empty.
	; Here are the rules for delimited lists:
	;	a) At least one instance of the list content needs to be present; 
	;	b) an instance of the list content needs to be the first thing in a delimited list; 
	;	c) an instance of the list content needs to be the last thing in a delimited list;
	;	d) any 2 instances of list content need to be separated by exactly one delimiter.
	; These rules are based on the representation of delimited lists as loops in syntax diagrams.
	; Note that allowing the empty subtree as a delimiter or a content makes delimited lists more broadly useful:
	;	- If the delimiter is the empty subtree, then the delimited list represents concatenation of *one* or more instances of the content.
	;	- If the content subtree is the empty subtree, then the delimited list represents concatenation of *zero* or more delimiters.
	; Because 'content' and 'delimiter' can be arbitrary subtrees there's a potential for all kinds of ambiguity.
	; One nasty case is this:
	;	@subtree@(1)="literal"
	;	@subtree@(1,"value")="("
	;	@subtree@(2)="delimList"
	;	@subtree@(2,"content")="token"
	;	@subTree@(2,"content","value")="digit" ; This is just some hypothetical token defining a numberic digit.
	;	@subTree@(2,"delimiter")="literal"
	;	@subTree@(2,"delimiter","value")=")"
	;	@subTree@(3)="literal"
	;	@subTree@(3,"value")=")"
	; In this subtree, the ")" symbol is used to end the subtree, and it's used as a list delimiter.
	; The following are syntacticly valid in this grammer:
	;	(1) , (1)2) , (1)2)2)1)
	; To deal with this kind of ambiguity, the parser tries to treat anything following a content as a delimiter.
	; This is part of the "maximal munch" strategy:  It tries to parse in a way that makes the delimList as large as possible.
	; Note that this is subtly different from parsing in a way that makes the *total parse* as long as possible.
	; The upshot is that the parser acts in a well-defined way, but it can be a bit tricky for poorly defined grammers.
	; YOU HAVE BEEN WARNED!!!
	;
	; The parameters are as follows:
	;	contentSubTreePointer - a string pointer to the fragment of map describing the content subtree we're parsing with
	;	delimiterSubtreePointer - a string pointer to the fragment of map describing the delimiter subtree we're parsing with
	;	outParseTreePointer - a string pointer to the array in which the parse data will be held.  The parse tree will have the form:
	;			      @outParseTreePointer@("contents",1) = subtree of 1st instance of list content
	;			      @outParseTreePointer@("delimiters",1)  = subtree of 1st delimiter
	;			      @outParseTreePointer@("contents",2) = subtree of 2nd instance of list content
	;			      					  ...
	;                             @outParseTreePointer@("delimiters",n-1)  = subtree of n-1st delimiter
	;			      @outParseTreePointer@("contents",n)= subtree of the nth instance of list content
	;	mapPointer - a string pointer to the array holding the map for parsing the tokens.
	;	codePointer - a string pointer to the array holding the code to be parsed.
	;	startColumn - the column of @codePointer to start the parse at.
	; Returns the number of characters parsed.  This is negative if an error is encountered.
parseDelimList(contentSubTreePointer,delimiterSubtreePointer,outParseTreePointer,mapPointer,codePointer,startColumn)
	q:$g(contentSubTreePointer)="" "-5, missing contentSubTreePointer"
	q:$g(delimiterSubtreePointer)="" "-6, missing delimiterSubtreePointer"
	;
	n tmpOutParseRootPointer,totalCharsParsed,charsParsed,delimCharsParsed,tempCodePosition,tempListObjectParseTreePointer,tempDelimiterParseTreePointer,listObjNum
	;
	; We need to ensure uniqueness of the parse tree pointer in the entire frame stack.
        ; This is because we merge the parse tree pointers between the levels of the stack.
        s tmpOutParseRootPointer="tmpPrs"_$st
        n @tmpOutParseRootPointer
	;
	s totalCharsParsed=0,charsParsed=0,delimCharsParsed=0
	s tempCodePosition=startColumn
	s tempListObjectParseTreePointer=$name(@tmpOutParseRootPointer@("content"))
	s tempDelimiterParseTreePointer=$name(@tmpOutParseRootPointer@("delimiter"))
	;
	f listObjNum=1:1 d  q:charsParsed<0  q:delimCharsParsed<0
	. s charsParsed=$$parseSubtree(contentSubTreePointer,tempListObjectParseTreePointer,mapPointer,codePointer,tempCodePosition)
	. ;
	. ; If this content subtree couldn't be parsed, we need to quit.
	. ; If the content followed a non-empty delimiter, then a parse error in content also invalidates the parse of the last delimiter
	. ; If no delimiter has been read yet, then this must be the first content subtree, in which case a parse error is a parse error in the entire delimited list.
	. i charsParsed<0 s:'$d(@tempDelimiterParseTreePointer) totalCharsParsed=charsParsed q
	. ;
	. ; Update the character count and merge in the parsed delimiter and character data
	. s totalCharsParsed=totalCharsParsed+charsParsed+delimCharsParsed
	. m @outParseTreePointer@("contents",listObjNum)=@tempListObjectParseTreePointer
	. m:$d(@tempDelimiterParseTreePointer) @outParseTreePointer@("delimiters",listObjNum-1)=@tempDelimiterParseTreePointer
	. k @tempListObjectParseTreePointer,@tempDelimiterParseTreePointer
	. ;
	. ; Now read ahead to the next delimiter
	. s tempCodePosition=tempCodePosition+charsParsed ; Advance the position in code to the start of our hypothetical delimiter.
	. s delimCharsParsed=$$parseSubtree(delimiterSubtreePointer,tempDelimiterParseTreePointer,mapPointer,codePointer,tempCodePosition)
	. s tempCodePosition=tempCodePosition+delimCharsParsed
	. ;
	. i delimCharsParsed<0 q  ; If a delimiter doesn't follow the last content, we've found the end of the list.
	. i delimCharsParsed=0,charsParsed=0 q  ; If both the delimiter and the content parsed to 0 characters, we quit to avoid an infinite loop
	;
	q totalCharsParsed
	;
	;
	; This function parses an arbitrary token.
	; This is actually pretty easy, since we just need to call parseSubtree on the content of @mapPointer@(token).
	; The parameters are as follows:
	;	token - The name of the token we're trying to parse the code as.
	;	outParseTreePointer - a string pointer to the array in which the parse data will be held.
	;			      @outParseTreePointer = subtree of parsed token
	;			      @outParseTreePointer@("token") = name of the token parsed
	;	mapPointer - a string pointer to the array holding the map for parsing the tokens.
	;	codePointer - a string pointer to the array holding the code to be parsed.
	;	startColumn - the column of @codePointer to start the parse at.
	; Returns the number of characters parsed.  This is negative if an error is encountered.
parseToken(token,outParseTreePointer,mapPointer,codePointer,startColumn)
	q:$g(token)="" "-7, no token specified for parsing."
	q:$g(mapPointer)="" "-8, no map pointer specified.  Can't resolve token definition."
	;
	n charsParsed,subTreePointer
	s subTreePointer=$name(@mapPointer@(token))
	s charsParsed=$$parseSubtree(subTreePointer,outParseTreePointer,mapPointer,codePointer,startColumn)
	s:charsParsed'<0 @outParseTreePointer@("token")=token
	;
	q charsParsed
	;
	;
	; This function parses the code to check for a specific string literal.
	; This literal can be multiple characters long, and can include unprintable characters
	; The parameters are as follows:
	;	literalValue - The name of the token we're trying to parse the code as.
	;	parseControlForce - a string that represents any special parsing controls to apply to parsing this literal
	;	outParseTreePointer - a string pointer to the array in which the parse data will be held.
	;			      If the literal value can be successfully parsed, it will be stored at @outParseTreePointer
	;	mapPointer - a string pointer to the array holding the map for parsing the tokens.
	;	codePointer - a string pointer to the array holding the code to be parsed.
	;	startColumn - the column of @codePointer to start the parse at.  This defaults to "1".
	; Returns the number of characters parsed.  This is negative if an error is encountered.
parseLiteral(literalValue,parseControlForce,outParseTreePointer,mapPointer,codePointer,startColumn) 
	q:$g(outParseTreePointer)="" "-1,no outParseTreePointer specified."
	q:$g(literalValue)="" "-9,an empty literal value is left-recursive and forces an infinite loop."
	;
	n literalLength,charsParsed
	s literalLength=$l(literalValue)
	s codeFragment=$$readCodeStream(codePointer,startColumn,literalLength)
	;
	i parseControlForce="forceToLowerCase" d
	. s codeFragment=$tr(codeFragment,"ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz")
	;
	; If the literalValue matches the next chunk of code, we'll return the length of the code parsed.  Otherwise, it's an error.
	i literalValue=codeFragment s charsParsed=literalLength
	e  s charsParsed="-10,literal value doesn't match next code fragments"
	;
	s:charsParsed>0 @outParseTreePointer=literalValue
	;
	q charsParsed
	;
	;
	; This function reads in the actual code that we're trying to parse.
	; The parameters are as follows:
	;	codePointer - a string pointer to the array holding the code to be parsed.
	;	startColumn - the column of @codePointer to start the parse at.  This defaults to "1".
	;	readLength - the number of characters to read in from @codePointer, starting at startColumn.
readCodeStream(codePointer,startColumn,readLength) q:$g(codePointer)="" "-1,codePointer required for parsing"
	s:+$g(startColumn)<1 startColumn=1
	i startColumn+readLength-1>$l(@codePointer) q "-10,overread error on code"
	;
	; TODO:  This only works on code input of a single line, which sucks.  Make this READ from a file stream or handle multiline data.
	;
	q $e(@codePointer,startColumn,startColumn+readLength-1)
	;
	;
	; ************************
	;
        ; This tag displays a computed parse tree
        ; The parameters of this function are
        ;       parseTreePointer - a string pointer to the computed parse tree
        ; This routine has no return value.
showParseTree(parseTreePointer) q:$g(parseTreePointer)="" "ERROR!!! no parseTreePointer!"
        i $d(@parseTreePointer)=0 w !,""
        e  zwrite:(+$system=47) @parseTreePointer@(*) zwrite:(+$system'=47) @parseTreePointer  ; TODO:  Make this not suck
        q
        ;
	;
	; **************************
	;
	; This function returns 1 exactly when we can expect a node to contain the complete definition of
	; a subtree.
definesCompleteSubtree(mapNode)
        q:$g(mapNode)="" "-1, the mapNode parameter is required for the definesCompleteSubtree function"
	n sublen,lastSubscript
	s sublen=$ql(mapNode)
	q:sublen=1 1 ; we're somewhere like @map@(tokenName)
	s lastSubscript=$qs(mapNode,sublen)
        q:lastSubscript=+lastSubscript 1 ; if the last subscript is numeric, it	always defines a new (possibly null) subtree
	q:lastSubscript="content" 1
	q:lastSubscript="delimiter" 1
	q:@mapNode="" 1 ; the null subtree is complete
        q 0
	;
	;
	; This function takes in a node of the grammer and returns 1 when the node defines a 'compiler directive' or other metadata.
	; At present, the only meta-data we allow is "force", which can be set to "forceToLowerCase", but we're being a bit more general here.
	; The function will return a negative in case of an error.
isMetaData(mapNode) q:$g(mapNode)="" "-1, the mapNode parameter is required by the isMetaData function."
	i $$definesCompleteSubtree(mapNode) q 0 ; subtrees are clearly not metadata
	i $qs(mapNode,$ql(mapNode))="value" q 0 ; tokens and literals are not metadata
	; Not actually checking just the "force" node adds some future-proofing to the code.
	q 1
	;
	;
	; *******************************
	;
	; This is a simple test-harness for the main parser tag.
tester(out,map) n code,counter,$ET,charsParsed
	s $ET="Write:(0=$STACK) "" Error '""_$zstatus_""' occurred at: ""_$st($st,""place"")"
	;
	i $g(map)="" s map="tempData" n @map 
	i $g(out)="" s out="tempOutData" n @out
	;
	; This part of the map just defines the digits.
	; There's just references to literals here.
	; The digits 1, 2, 3, 4, 5, 6, 7, 8, and 9.
	s @map@("nonzeroDigit")="options"
	s @map@("nonzeroDigit",1)="literal"
	s @map@("nonzeroDigit",1,"value")="1"
	s @map@("nonzeroDigit",2)="literal"
	s @map@("nonzeroDigit",2,"value")="2"
	s @map@("nonzeroDigit",3)="literal"
	s @map@("nonzeroDigit",3,"value")="3"
	s @map@("nonzeroDigit",4)="literal"
	s @map@("nonzeroDigit",4,"value")="4"
	s @map@("nonzeroDigit",5)="literal"
	s @map@("nonzeroDigit",5,"value")="5"
	s @map@("nonzeroDigit",6)="literal"
	s @map@("nonzeroDigit",6,"value")="6"
	s @map@("nonzeroDigit",7)="literal"
	s @map@("nonzeroDigit",7,"value")="7"
	s @map@("nonzeroDigit",8)="literal"
	s @map@("nonzeroDigit",8,"value")="8"
	s @map@("nonzeroDigit",9)="literal"
	s @map@("nonzeroDigit",9,"value")="9"
	;
	; The single digit '0'.
	s @map@("zeroDigit")="literal"
	s @map@("zeroDigit","value")="0"
	;
	; Now we group tokens together to define the digits 0-9.
	; There's still no recursion, though.
	; The digits 0-9
	s @map@("digit")="options"
	s @map@("digit",1)="token"
	s @map@("digit",1,"value")="zeroDigit"
	s @map@("digit",2)="token"
	s @map@("digit",2,"value")="nonzeroDigit"
	;
	; Defining finite-digit floating point numbers (in simplist form).
	; There's no recursion here.
	; Note that "delimList", "options", "subtree", and the empty tree get exercised here.
	; A floating point number with a finite number of digits
	s @map@("number")="subtreeChain"
	s @map@("number",1)="options" ; These options are the things that preceed the decimal point
	s @map@("number",1,1)="token"
	s @map@("number",1,1,"value")="zeroDigit"
	s @map@("number",1,2)="subtreeChain"
	s @map@("number",1,2,1)="token"
	s @map@("number",1,2,1,"value")="nonzeroDigit"
	s @map@("number",1,2,2)="delimList"
	s @map@("number",1,2,2,"delimiter")="token" ; making delimiters of digits and having empty list objects allows *zero* or more digits
	s @map@("number",1,2,2,"delimiter","value")="digit"
	s @map@("number",1,2,2,"content")="" ; There must be at least one content node in the parse, but that is easy since the content subtree can be empty
	s @map@("number",2)="options" ; These options are the things that can follow the decimal point
	s @map@("number",2,1)="" ; There might not even be a decimal point in the number
	s @map@("number",2,2)="subtreeChain"
	s @map@("number",2,2,1)="literal"
	s @map@("number",2,2,1,"value")="."
	s @map@("number",2,2,2)="delimList"
	s @map@("number",2,2,2,"delimiter")="" ; we don't care so much about a trail of zeros after the decimal point, so we just have a content of 'digit'
	s @map@("number",2,2,2,"content")="token" 
	s @map@("number",2,2,2,"content","value")="digit"
	;
	; Set up a token map for numbers and products of numbers.
	; Here, we test the handling of complex delimiters and contents in delimLists.
	; A number or product/quotient of numbers.
	s @map@("product")="delimList"
	s @map@("product","delimiter")="options" ; we can have either * or / as a delimiter in a product/quotient
	s @map@("product","delimiter",1)="literal"
	s @map@("product","delimiter",1,"value")="*"
	s @map@("product","delimiter",2)="literal"
	s @map@("product","delimiter",2,"value")="/"
	s @map@("product","content")="token"
	s @map@("product","content","value")="number"
	;
	; Set up a token map for simple addition and parenthesis.
	; Here's where we get to test our handling of recursion.
	; A generic expression of digit addition
	s @map@("expr")="options"
	s @map@("expr",1)="token"  ; The first option for an expression is any number or product/quotient of numbers
	s @map@("expr",1,"value")="product"
	s @map@("expr",2)="subtreeChain" ; This will define "product + expr".
	s @map@("expr",2,1)="token"
	s @map@("expr",2,1,"value")="product"
	s @map@("expr",2,2)="literal"
	s @map@("expr",2,2,"value")="+"
	s @map@("expr",2,3)="token"
	s @map@("expr",2,3,"value")="expr"
	s @map@("expr",3)="subtreeChain" ; This will define "(expr)".
	s @map@("expr",3,1)="literal"
	s @map@("expr",3,1,"value")="("
	s @map@("expr",3,2)="token"
	s @map@("expr",3,2,"value")="expr"
	s @map@("expr",3,3)="literal"
	s @map@("expr",3,3,"value")=")"
	;
	; Now we parse something
	s code(1)="1",code(2)="2.04",code(3)="0.3",code(4)="200.0004",code(5)="1+1",code(6)="1.3+5",code(7)="(3+1)",code(8)="(4)+12.1",code(9)="1+(2+(3+4.3))+2.1"
	s counter=""
	f  s counter=$o(code(counter)) q:counter=""  d
	. w !,!,"Testing parse of "_code(counter)_"...  "
	. s charsParsed=$$parseToken("expr",out,map,"code("_counter_")",1)
	. w !,"Return value of parse for '"_code(counter)_"' = "_charsParsed
	. w !,"Parsed string: '"_$e(code(counter),1,charsParsed)_"'"
	. ;
	. w !,"Parse tree:",!
	. d showParseTree(out)
	. k @out
	;
	q




