GENERATOR1
    ; Copyright (C) 2017-2020 Neils Schoenfelder
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
    ; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
    ; 
    ; 
    d tester() q  ; redirect to test harness
    ;
    ;
    ; =====  OVERVIEW  ====
    ; This is a statement generator for any language that  can be expressed using a syntax diagram consisting only of:
    ;   1) string literals
    ;   2) options
    ;   3) delimited lists
    ;   4) named tokens.
    ; Note that it's possible to draw a syntax diagram that isn't of this form, but that's actually pretty tricky.
    ;
    ; This function returns a parse tree.
    ;
    ; ====  TECHNICAL TERMINOLOGY  ====
    ; The statement generate uses a sytax map, which is of the following form:
    ;       @map@(token1Name) = subtree1
    ;       @map@(token2Name) = subtree2
    ;                        ...
    ;       @map@(tokenName)  = subtree
    ; where each subtree is a MUMPS array describing the structure of each corresponding token. 
    ; The subtrees are described in greater detail in the in-routine documentation comments of PARSER1.
    ;
    ; As a brief overview, the structure of the subtrees, is of the generic form:
    ;               @subtree = subtree type
    ;               @subtree@("force") = the behavior of the generator to force when generating subtrees (or deeper subtrees).
    ;               @subtree@(subnodes) = additional data.
    ; The additional data can be quite extensive, and include references to other tokens, lists of options, string literal, etc.
    ;
    ; We allow these 6 types of subtrees:
    ;       1) "" to denote an empty subtree.  This is typically used with "options" and sometimes with "delimList".
    ;       2) "literal" for string literals,
    ;       3) "subtreeChain" to allow us to easily concatenate multiple subtrees together.
    ;       4) "options" to denote a list of options,
    ;       5) "delimList" to denote a delimited list,
    ;       6) "token" to reference another token.  Effectively, this is just a named subtree.
    ;
    ; For examples of these token types and the structures of the tokens, see the comments in the ^PARSER1 routine.
    ;
    ;
    ; *******************************
    ; 
    ; This is the core statement generator function that handles generic subtrees.
    ; The parameters are as follows:
    ;   subTreePointer - a string pointer to the fragment of map describing the subtree we're using to generate a statment
    ;   generatedResultPointer - a string pointer to the scalar in which the generated syntax will be held.
    ;   mapPointer - a string pointer to the root of the array holding the grammer map for generating the tokens.
    ;       (The map defined by @mapPointer must not have any instances of left-recursion!)
    ;   depthSeed - A positive integer that is used to limit how long our generated string will be.
    ;               When generating a delimList or a list of options that includes a null token, the depthSeed is
    ;               used to influence probability that the delimList will be halted or the null option will be picked.
    ;               This provides a probabalistic halting condition for the code generator.
    ;               (Without this, the statements that we generate would be infinitely long.)
    ; Returns the number of characters built.  Returns negative numbers in case of error.
generateSubtree(subTreePointer,generatedResultPointer,mapPointer,depthSeed) 
    q:$g(subTreePointer)="" "-1,missing subtreePointer"
    q:$g(generatedResultPointer)="" "-1,missing generatedResultPointer"
    ; Recall that subtrees are of the generic form:
    ;       @subtree = subtree type
    ;       @subtree@(subnodes) = additional data.
    ; We allow these types of subtrees:
    ;   1) "" to denote an empty subtree.  This is typically used with "options" and sometimes with "delimList".
    ;   2) "literal" for string literals, 
    ;   3) "subtreeChain" to act as a string of subtrees when creating a new token is too much of a pain.
    ;   4) "options" to denote a list of options,
    ;   5) "delimList" to denote a delimited list, 
    ;   6) "token" to reference another token
    ;
    n subTreeType,charsBuilt,literalValue,contentSubTreePointer,delimiterSubTreePointer,tokenName
    s subTreeType=@subTreePointer
    ;
    i subTreeType="" d
    . i $d(@subTreePointer)<10 s charsBuilt=0
    . e  s charsBuilt="-4,subtrees with no type should be empty arrays." ; this is one way to limit left-recursive structures in the grammer
    ;
    i subTreeType="literal" d
    . s literalValue=@subTreePointer@("value")
    . s charsBuilt=$$generateLiteral(literalValue,generatedResultPointer)
    ;
    i subTreeType="subtreeChain" d
    . s charsBuilt=$$generateSubtreeChain(subTreePointer,generatedResultPointer,mapPointer,depthSeed)
    ;
    i subTreeType="options" d
    . s charsBuilt=$$generateOptions(subTreePointer,generatedResultPointer,mapPointer,depthSeed)
    ;
    i subTreeType="delimList" d
    . s contentSubTreePointer=$name(@subTreePointer@("content"))
    . s delimiterSubTreePointer=$name(@subTreePointer@("delimiter"))
    . s charsBuilt=$$generateDelimList(contentSubTreePointer,delimiterSubTreePointer,generatedResultPointer,mapPointer,depthSeed)
    ;
    i subTreeType="token" d
    . s tokenName=@subTreePointer@("value")
    . s charsBuilt=$$generateToken(tokenName,generatedResultPointer,mapPointer,depthSeed)
    ;
    i '$d(charsBuilt) s charsBuilt="-2,unknown subTreeType: "_subTreeType BREAK  ; TODO:  Get rid of BREAK
    q +$g(charsBuilt)
    ;
    ;
    ; This generates subtreeChains.
    ; As a reminder, the structure of a subTreeChain is:
    ;       @subtree = "subtreeChain"
    ;       @subtree@("force") = the behavior of the parser to force when parsing any literals defined in the deeper subtrees.
    ;               (An example value is "forceToLowerCase" to force input to be automatically changed to lowercase)
    ;               The forced behavior affects the parsing of this subtree and all sub-subtrees.
    ;       @subtree(1) = the 1st subtree.  Any valid @map@ root is valid here (without the human-readable description).
    ;       @subtree(2) = the 2nd subtree.  Any valid @map@ root is valid here (without the human-readable description).
    ;       ...
    ;       @subtree(n) = the nth subtree.  Any valid @map@ root is valid here (without the human-readable description).
    ; The parameters are as follows:
    ;   subTreePointer - a string pointer to the fragment of map describing the subtree we're parsing with
    ;   generatedResultPointer - a string pointer to the scalar in which the generated syntax will be held.
    ;   mapPointer - a string pointer to the array holding the map for parsing the tokens.
    ;       (The map defined by @mapPointer must not have any instances of left-recursion!)
    ;   depthSeed - a positive integer representing a probabalistic halting condition
    ; Returns the number of characters built or a negative number on an error.
generateSubtreeChain(subTreePointer,generatedResultPointer,mapPointer,depthSeed)
    n charsBuilt,whichChain
    ;
    f whichChain=1:1 q:'$d(@subTreePointer@(whichChain))  d
    . s charsBuilt=$g(charsBuilt)+$$generateSubtree($name(@subTreePointer@(whichChain)),generatedResultPointer,mapPointer,depthSeed)
    ;
    q +$g(charsBuilt)
    ;
    ;
    ; This generated data by choosing randomly between multiple options.
    ; Recall that the "options" type of subtree looks like this:
    ;       @subtree = "options"
    ;       @subtree@(1) = 1st option.  Any valid subtree is valid here
    ;       @subtree@(2) = 2nd option.  Any valid subtree is valid here
    ;       ...
    ;       @subtree@(n) = nth option.  Any valid subtree is valid here.
    ; This function works by randomly choosing among all of the options presented.
    ; If one of the options is blank, and if the value of $STACK is greater than the depthSeed parameter, 
    ; then the blank null option will be preferentially selected.  This probabalistically limits the length of the generated data.
    ; 
    ; The parameters are as follows:
    ;   subTreePointer - a string pointer to the fragment of map describing the subtree we're building with
    ;   generatedResultPointer - a string pointer to the scalar in which the generated syntax will be held.
    ;   mapPointer - a string pointer to the array holding the map that defines the grammar.
    ;       (The map defined by @mapPointer must not have any instances of left-recursion!)
    ;   depthSeed - A positive integer that is used to limit how long our generated string will be.
    ;               When generating a delimList or a list of options that includes a null token, the depthSeed is
    ;               used to influence probability that the delimList will be halted or the null option will be picked.
    ;               This provides a probabalistic halting condition for the code generator.
    ;               (Without this, the statements that we generate would be infinitely long.)
    ; Returns the number of characters built.  This is negative if an error is encountered.
generateOptions(subTreePointer,generatedResultPointer,mapPointer,depthSeed)
    n charsBuilt,optionCount,whichOption
    ;
    ; Pick an option and generate additional result based on that option
    s optionCount="" f  s optionCount=$o(@subTreePointer@(optionCount),-1) q:optionCount=""  q:optionCount=+optionCount
    s whichOption=$r(optionCount)+1
    s charsBuilt=$$generateSubtree($name(@subTreePointer@(whichOption)),generatedResultPointer,mapPointer,depthSeed) 
    ;
    q +$g(charsBuilt)
    ;
    ;
    ; This generates a delimited list.
    ; Note that in general, a delimited list is composed of a content subtree and a delimiter subtree, either of which could be empty.
    ; Here are the rules for delimited lists:
    ;   a) At least one instance of the list content needs to be present; 
    ;   b) an instance of the list content needs to be the first thing in a delimited list; 
    ;   c) an instance of the list content needs to be the last thing in a delimited list;
    ;   d) any 2 instances of list content need to be separated by exactly one delimiter.
    ; These rules are based on the representation of delimited lists as loops in syntax diagrams.
    ; Note that allowing the empty subtree as a delimiter or a content makes delimited lists more broadly useful:
    ;   - If the delimiter is the empty subtree, then the delimited list represents concatenation of *one* or more instances of the content.
    ;   - If the content subtree is the empty subtree, then the delimited list represents concatenation of *zero* or more delimiters.
    ; If the $STACK variable plus the position in the delimited list we're building exceeds depthSeed, the list building will be shortened.
    ; This shortening is done probabalistically.
    ;
    ; The parameters are as follows:
    ;   contentSubTreePointer - a string pointer to the fragment of map describing the content subtree of the grammar.
    ;   delimiterSubtreePointer - a string pointer to the fragment of map describing the delimiter subtree of the grammar.
    ;   generatedResultPointer - a string pointer to the scalar in which the generated syntax will be held.
    ;   mapPointer - a string pointer to the array holding the map for generating the tokens.
    ;   depthSeed - A positive integer that is used to limit how long our generated string will be.
    ;               When generating a delimList or a list of options that includes a null token, the depthSeed is
    ;               used to influence probability that the delimList will be halted or the null option will be picked.
    ;               This provides a probabalistic halting condition for the code generator.
    ;               (Without this, the statements that we generate would be infinitely long.)
    ; Returns the number of characters built.  This is negative if an error is encountered.
generateDelimList(contentSubTreePointer,delimiterSubTreePointer,generatedResultPointer,mapPointer,depthSeed)
    n charsBuilt
    ;
    f  s charsBuilt=$g(charsBuilt)+$$generateSubtree(contentSubTreePointer,generatedResultPointer,mapPointer,depthSeed)  q:$r(depthSeed)<$st  s charsBuilt=charsBuilt+$$generateSubtree(delimiterSubTreePointer,generatedResultPointer,mapPointer,depthSeed) ; Probabalisticly stop for large stack depths
    ;
    q +$g(charsBuilt)
    ;
    ;
    ;
    ;
    ; This function generates a token of the given type.
    ; This is actually pretty easy, since we just need to call generateSubtree on the content of @mapPointer@(token).
    ; The parameters are as follows:
    ;   token - The name of the token we're trying to generate code for.
    ;   generatedResultPointer - a string pointer to the scalar in which the generated syntax will be held.
    ;   mapPointer - a string pointer to the array holding the map that defines the grammar.
    ;   depthSeed - a positive integer that is used to probabalistically truncate the code generating.
    ; Returns the number of characters built.  This is negative if an error is encountered.
generateToken(tokenName,generatedResultPointer,mapPointer,depthSeed)
    q:$g(tokenName)="" "-7, no token specified for syntax generation."
    q:$g(mapPointer)="" "-8, no map pointer specified.  Can't resolve token definition."
    ;
    n charsBuilt
    s charsBuilt=$$generateSubtree($name(@mapPointer@(tokenName)),generatedResultPointer,mapPointer,depthSeed)
    ;
    q +$g(charsBuilt)
    ;
    ;
    ; This function generates a specific string literal.
    ; This literal can be multiple characters long, and can include unprintable characters
    ; The parameters are as follows:
    ;   literalValue - The name of the token we're generating.
    ;   generatedResultPointer - a string pointer to the scalar in which the parse data will be held.
    ;                 If the literal value can be successfully parsed, it will be added to @generatedResultPointer
    ; Returns the number of characters built.  This is negative if an error is encountered.
    ; (Of course, the number of characters built should be the length of the literalValue)
generateLiteral(literalValue,generatedResultPointer)
    q:$g(generatedResultPointer)="" "-1,no generatedResultPointer specified."
    q:$g(literalValue)="" "-9,an empty literal value is left-recursive and forces an infinite loop."
    ;
    s @generatedResultPointer=$g(@generatedResultPointer)_literalValue
    q $l(literalValue)
    ;
    ;
    ; *******************************
    ;
    ;
    ; This tag is test harness for the generator routine.
    ; The parameters are as follows:
    ;   grammer - A string pointer to an array holding a grammer
    ;   startToken - A root token in the grammer that will define where we start our parsing
    ;   depthSeed - A positive integer that is used to limit how long our generated string will be.
    ;               When generating a delimList or a list of options that includes a null token, the depthSeed is
    ;               used to influence probability that the delimList will be halted or the null option will be picked.
    ;               This provides a probabalistic halting condition for the code generator.
    ;               (Without this, the statements that we generate would be infinitely long.)
tester(grammer,startToken,depthSeed)
    ;
    ; Set up default grammer
    i $g(grammer)="" s grammer="tempGrammerMap" n @grammer d buildTestGrammer^PARSER1(grammer) 
    ; Check if the grammer is semi-reasonable
    i $d(@grammer)<10 w !,"Grammer is not well=defined." q
    ;
    ; Default in a random startToken if necessary
    i $g(startToken)="" d
    . n tokenCount,currentToken,targetToken
    . s currentToken=""
    . f tokenCount=0:1 s currentToken=$o(@grammer@(currentToken)) q:currentToken=""  ; count the number of tokens in the grammer
    . s targetToken=$r(tokenCount)+1
    . s startToken="",currentToken=0
    . f  s startToken=$o(@grammer@(startToken)),currentToken=currentToken+1 q:currentToken'<targetToken  q:startToken=""  ; iterate through the list to find a random start token
    ; 
    ; Check if the startToken is semi-reasonable
    i $g(startToken)="" w !,"No start token "_startToken_" is specified, and cannot be defaulted." q
    i $d(@grammer@(startToken))<1 w !,"Start token "_startToken_" is not part of the grammer." q
    ;
    ; Sanitize (and default) depthSeed
    s depthSeed=+$g(depthSeed)
    s:depthSeed<1 depthSeed=15
    ;
    w !,"Generating a string based on the grammer array in "_grammer_" of type "_startToken_"..."
    n charsBuilt,generatedResultPointer
    s generatedResultPointer="resultString"
    n @generatedResultPointer
    s charsBuilt=$$generateToken(startToken,generatedResultPointer,grammer,depthSeed)
    ; 
    w !,"Generated "_charsBuilt_" characters of syntax in the specified grammar.",!
    n zver,zwriteCommand
    s zver=$p($zv," "),zwriteCommand=$s(zver="GT.M":"zwrite @generatedResultPointer@(*)",zver="Cache":"zwrite @generatedResultPointer",1:"")
    x:zwriteCommand'="" zwriteCommand
    ;
    ; Now try to parse the generated string and make sure it fully parses
    w !,"Attempting to parse the generated string..."
    n charsParsed,outParseTreePointer
    s outParseTreePointer="parseTreeForGeneratedString" n @outParseTreePointer
    s charsParsed=$$parseToken^PARSER1(startToken,outParseTreePointer,grammer,generatedResultPointer,1)
    w !,"Parsed "_charsParsed_" characters of the generated string."
    w !,"This is a "_$s(charsParsed=charsBuilt:"success!",1:"failure!"),!
    d showParseTree^PARSER1(outParseTreePointer)
    ;
    q
    