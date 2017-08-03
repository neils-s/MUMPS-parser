PARSER2
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
    ; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
    ;
    ; 
    d tester() q
    ;
    ; ====  OVERVIEW  ====
    ; This contains generic tree manipulation tools.  These tools use a MUMPS array representing a tree to derive a new tree. 
    ; The intent is to apply these tree tools to the parse tree returned by the code in ^PARSER1, producing an abstract syntax tree.
    ; More generally, the tools can be applied to a dataflow tree to prune it to a more human-readable form, or they can even be applied
    ; to an abstract syntax tree to produce an optimized (or at least 'improved') version of the tree that embodies the same functionality
    ; but functions more effeciently.
    ;
        ; This routine contains a small collection of helper-functions that make it easier to use one tree as a 'template' to build another.
    ; These functions are applied recursively to the source tree, and will be used to build the destination tree.
    ; There is also a general 'evaluate' function that can evaluate a 'tree-building rule' expressed as MUMPS code.
    ;
    ; Note that these functions don't purport to represent an effecient tree transversal algorithm.
    ; Also, there's no guarantee that it's possible to use these functions to transform a source tree to a destination tree in a single pass;
    ; it may be necessary to transform a source tree to an intermediate tree before creating a destination tree.
    ; In other words, your tree may need to factor through several intermediate trees before reaching it's final form.
    ;
    ;
    ; ====  EXAMPLE GOALS  ====
    ; As an example of the kinds of tree-building actions we'd like to use these tools for, we present some common actions taken
    ; in transforming a parse tree to an abstract syntax tree (AST), and even creating an 'optimized' the abstract syntax tree.
    ;
    ; 1) Skipping extraneous terminal nodes
    ; The most basic operation in creating an AST from a parse tree is the removal of nodes representing things like parenthesis,
    ; since operational precendence is already encapsulated in the structure of the abstract syntax tree itself.
    ; In pictures, we'd like to be able to use a tree like this:
    ;           sum
    ;          / | \
    ;         1  -  expr
    ;              /  |  \
    ;             (  sum  )
    ;               / | \
    ;              2  +  3
    ;
    ; to build a tree like this:
    ;           sum
    ;          / | \
    ;         1  -  expr
    ;                 |   
    ;                sum  
    ;               / | \
    ;              2  +  3
    ; 
    ; 2) Skipping extraneous intervening nodes
    ; Another common step in creating an AST from a parse tree is excising nodes that contain only one child.
    ; These intervening nodes don't typically carry any useful information, so they can often just go without any loss.
    ; This is especially useful in LL(1) parsers, which tend to create parse trees with *lots* of extraneous intervening nodes.
    ; In pictures, we want to use a tree like this:
    ;           sum
    ;          / | \
    ;         1  -  expr
    ;                 |   
    ;                sum  
    ;               / | \
    ;              2  +  3
    ; as a template to build a tree like this:
    ;               sum
    ;              / | \
    ;             1  -  sum
    ;                  / | \
    ;                 2  +  3
    ;
    ; 3) Assigning different names to nodes in destination tree
    ; Using the contents of the child (or grand-child, or great grand-child, etc.) nodes to rename a node is a common
    ; step to take when creating an AST from a parse tree.
    ; In pictures, we want to use a tree like this:
    ;           sum
    ;          / | \
    ;         1  -  sum
    ;              / | \
    ;             2  +  3
    ;
    ; as a template to build a tree like this:
    ;           -
    ;          /|\
    ;         1 - +
    ;            /|\
    ;           2 + 3
    ;
    ; 4) Conditionally skipping nodes or a certain type
    ; Sometimes, in transforming a tree, we'll want to evaluate logic to determine if a node should be copied from the source tree to the
    ; destination tree.  For instance, we might not want to copy nodes that have a name of '+' or '-' if those nodes have no children.
    ; In pictures, we want a tree like this:
    ;           -
    ;          /|\
    ;         1 - +
    ;            /|\
    ;           2 + 3
    ;
    ; To act as a template to build a tree like this:
    ;           -
    ;          / \
    ;         1   +
    ;            / \
    ;           2   3
    ;
    ; 5) Skipping an entire subtree (a node and all it's subnodes)
    ; When a grammer is over-specified, or when lexing is done badly, it's possible to end up with lots of tokens in the parse tree
    ; that are below the level of the lexemes.  For example, it's possible to imagine a grammer that specifies an 'integer' as a 
    ; 'nonzero-digit' followed by a finite string of 'digits'.  In this case, the individual digits that make up the 'integer' have
    ; no semantic meaning, and all of that granular sub-structure can be removed.
    ; In pictures, we want a tree like this:
    ;               integer(103)
    ;                   /   \
    ;             non-zero   integer_piece
    ;                |         /      \
    ;                1      digit    integer_piece
    ;                         |        /      \
    ;                         0     digit    integer_piece
    ;                                  |       |
    ;                                 3         null
    ;
    ; to act as a template to build a tree like this: 
    ;                  integer(103)
    ;
    ; 6) Moving a subtree (a node and all it's subnodes) when building the destination tree
    ; Depending on the way the grammer is written, it's possible (likely) that nodes will end up in different parts of the tree when
    ; you'd prefer to have them nearby.
    ; As an example (stolen from http://eli.thegreenplace.net/2009/02/16/abstract-vs-concrete-syntax-trees/) consider the way the C
    ; parser handles pointer declarations.
    ; When the C parser is given a statement like "int * foo;", it returns a tree that is similar to
    ;                    declaration
    ;                    /    |     \
    ;           specifiers    |      ;
    ;            /        declarators
    ;   type_spec         |
    ;     /          declarator
    ;   INT           /      \
    ;              pointer   direct_declarator
    ;               /           \
    ;              *           identifier
    ;                                \
    ;                               foo
    ; OUCH!
    ; With the strategies we've outlined above, we can simplify to a tree like this:
    ;                    declaration
    ;                    /         \
    ;           type_spec       \
    ;                 /             declarator
    ;                INT              /      \
    ;                           pointer    identifier
    ;                                         \
    ;                                         foo
    ; Still ouch!
    ; By moving the 'pointer' subtree to lie under the 'INT' subnode, and re-applying the earlier strategies,
    ; we achieve this tree:
    ;                   declaration
    ;                    /        \   
    ;           type_spec  identifier
    ;                   |          |
    ;                  INT        foo
    ;                   |
    ;                pointer
    ; Nicer.
    ; The type information needed to type-check 'foo' is now stacked nicely.  Under this scheme, the dereference operator could be seen as popping off the
    ; 'pointer' off of the bottom of the 'type_spec' tree
    ;
    ;
    ; ====  DECISION MAKING ====
    ; The question remains:  How do we decide which nodes of the source get copied to the target, and how do we decide where on the target to copy them?
    ; To answer this question, a rule framework is applied.  This framework is designed around a few philosophical guidelines:
    ; A) All code needed for decision-making should be contained in this routine.
    ;    It's likely that external code will call into this routine, and it should be agnostic to the structure of this routine.
    ; B) Little or no raw MUMPS code should be needed for the decision making.
    ;    If we run raw code in this routine (even well-intentioned code), we're likely step on local variables and break something.
    ; C) The actions are distinctly separate from the decision-making.
    ;    Encouraging a strict wall between decision and action makes the code look much nicer.
    ; D) The rules and actions to apply should be structured as a tree.
    ;    A dichotomous tree is a very well known general structure for decision-making.
    ; E) The decision-making code should be designed to act via recursion.
    ;    The source tree, target tree, and even the rule tree all lend themselves to recursive traversal.
    ; F) At each stage of the recursion, the results of the tree-crawling on the current node should be available to the child nodes.
    ;    This will actually cause the tree-crawler to operate in a root-to-leaves direction.
    ;    This also means that some 'caching' mechanism can be used to pass information from the current node to the child nodes.
    ; G) On each node, the entire rule tree should be evaluated.
    ;    This *massively* simplifies the code.
    ; H) It shouldn't be necessary to capture all logic in a single rule-tree.
    ;    We can apply many rule trees, modifying the original source tree itteratively through a series of intermediate trees to a final target.
    ;    As an added bonus, this means we shouldn't need to run custom setup code in an 'initialization' phase.  Instead, we can create an
    ;    intermediate tree with the same structure as the source tree, but where the nodes have additional 'initialization data'.
    ;    The down-side of this guideline is that the transformation from source tree to final target tree could take *lots* of steps, and
    ;    therefore take lots of time.
    ;
    ; Here's a rough schematic of the algorithm for the tree crawler:
    ; a) Initialization phase
    ;   1) increment a %treeDepth counter up by 1.  The way to accomplish this without having to decrement later is to reNEW the %treeDepth variable
    ;   2) NEW a local variable at this %treeDepth level to serve as a cache for information. This cache will be accessible down the recursion stack.
    ;   3) NEW an array to hold the returns of the child nodes
    ; b) Evaluate the entire rule tree on this node.  The rule tree is structured like this
    ;              (parent)
    ;                  |
    ;               ACTION
    ;                  |
    ;                RULE
    ;               /    \
    ;           true?    false?
    ;            |          |
    ;        (child)      (child)
    ;   1) At each stage in the rule tree, we evaluate the ACTION first.  Here are some allowed actions
    ;         - Set the returnVal that will be passed up the tree
    ;         - Add a key/value pair to the cache at the current %treeDepth level
    ;         - Add a key/value pair to the cache at %treeDepth level 0
    ;         - Update the pointer to the target tree
    ;     - Set/build/destroy a node in the target tree
    ;         - Do nothing
    ;   2) After the action, evaluate the rule.  The rule will have access to this information
    ;         - Cached values.  The retrieval of a cached key/value pair will cascade down the stack to %treeDepth level 0 to try to retrieve data.
    ;     - Current returnVal.
    ;     - Array of data returned from the tree-crawler's action on the children of the current source node.
    ;     - The value at the current source node
    ;     - the value of the current target node
    ;     - Boolean logic
    ;   3) If the RULE is blank, we've reached a stopping point in the rule tree
    ;   4) If the RULE returns true, then the rule tree is followed down the 'true' branch to more ACTIONs and RULEs.
    ;   5) If the RULE returns false, then the rule tree is followed down the 'false' branch to more ACTIONs and RULEs.
    ;    By evaluating the rule tree before passing to the child nodes, we can take actions like building an analogue to the current node,
    ;    or cache information to pass to the child nodes when they're evaluated in the recursion step.
    ; c) Recursion step
    ;   1) Get a pointer to the first child node.
    ;   2) Starting at the first child, use an itterator to move over all of the child nodes, recursively call the tree-crawler for each one.
    ;   3) Collect the result of the tree-crawler on each child node in a local array.
    ; d) Create a local variable, returnVal, that will be returned up the chain to the calling instance of the tree-crawler.
    ; e) Set a local flag that signals that we've processed all child nodes
    ; f) Evaluate the entire rule tree on the node again.
    ;    This will allow the rule tree to act on data returned by the child nodes.
    ; g) Return the current returnVal to the calling instance of the tree-crawler.
    ;
    ;
    ; ====  TECHNICAL NEEDS  ====
    ; Motivated by the examples above, functions will be created to navigate the source and destination
    ; trees, to make decisions based on the contents of the source tree, and to build the destination
    ; tree.
    ;
    ; Tree navigation functions:
    ;   firstChild(aNode)
    ;   nextSibling(aNode)
    ;   parent(aNode)
    ;
    ; Tree decision functions:
    ;   getNodeValue(aNode)
    ;   cacheKeyValue(key,value,toGlobalLevel)
    ;   getCachedValue(key)
    ;   getReturnVal()
    ;   setReturnVal(value)
    ;   evalRule(aNode,rule)
    ;   applyAction(sourceNode,targetNode,actionCode)
    ;   evalDecisionTree(sourceNode,targetNode,decisionTree)
    ;
    ; Tree building functions:
    ;   buildChild(aNode,child)
    ;   buildSibling(aNode,sibling)
    ;   setNodeValue(aNode)
    ;
    ; Driver function
    ;   treeCrawler(sourceNode,targetNode,decisionTree)
    ;   startTreeCrawl(sourceRoot,targetRoot,decisionTree)
    ;
    ; ****************************************************
    ;
    ; ****  Tree Navigation  ****
    ;
    ; This function returns a string pointer to the parent of a given node.
    ; If a node has no parent (it's a root node), then a NULL is returned.
    ; In case of error, a negative number is returned. This is consistent because a MUMPS array name (lvn or glvn) can't start with a number.
parent(aNode) q:$g(aNode)="" "-1, the parent function requires a tree node argument"
    i $ql(aNode)=0 q "" ; a root node has no parent, by definition
    q $name(@aNode,$ql(aNode)-1)
    ;
    ;
    ; This function returns a pointer to the next sibling of the given node.
    ; This function moves in lexiconographical order accross the siblings (like the MUMPS $ORDER intrinsic function).
    ; The direction of operation can be reversed by by passing in a '-1' for the direction parameter
    ; If there is no next sibling, then NULL is returned.
    ; This is very similar to the MUMPS $ORDER intrinsic function, except that instead of a subscript value, a whole string pointer is returned.
    ; In case of error, a negative number is returned. This is consistent because a MUMPS array name (lvn or glvn) can't start with a number.
nextSibling(aNode,direction) q:$g(aNode)="" "-1, the nextSibling function requires a tree node argument"
    n parent,returnSubscript
    s parent=$$parent(aNode)
    i parent="" q "" ; A root node has no siblings
    i $g(direction)'=-1 s direction=1
    s returnSubscript=$o(@aNode,direction)
    i returnSubscript="" q "" ; We've run out of subscripts
    q $name(@parent@(returnSubscript))
    ;
    ;
    ; This function returns a pointer to the first child of a given node.
    ; The return value is a full string pointer to the child node, rather than just a single subscript.
    ; If there is no child, then NULL is returned.
    ; This is very similar to the behavior of the MUMPS $QUERY intrinsic function
    ; In case of error, a negative number is returned. This is consistent because a MUMPS array name (lvn or glvn) can't start with a number.
firstChild(aNode) q:$g(aNode)="" "-1, firstChild function requires a tree node argument"
    q $$nextSibling($name(@aNode@("")))
    ;
    ;
    ; This function returns a pointer to the last child of a given node.
    ; The 'last' here is taken in lexiconographical order (like the MUMPS $ORDER intrinsic function).
    ; This is useful when we want to create a new child node that doesn't overwrite any existing nodes.
    ; The return value is a full string pointer to the child node, rather than just a single subscript.
    ; If there is no child, then NULL is returned.
    ; In case of error, a negative number is returned. This is consistent because a MUMPS array name (lvn or glvn) can't start with a number.
lastChild(aNode) q:$g(aNode)="" "-1, lastChild function requires a tree node argument"
    q $$nextSibling($name(@aNode@("")),-1)
    ;
    ;
    ; ****  Tree Construction  *****
    ;
    ; This just sets the value at a specific node.
    ; This function will build the node if it doesn't already exist.
    ; If successful, a string pointer to the node just set is returned.
    ; (This is supposed to make it easy to to chain functions together).
    ; In case of error, a negative number is returned. This is consistent because a MUMPS array name (lvn or glvn) can't start with a number.
setNodeValue(aNode,value) q:$g(aNode)="" "-1, setNodeValue functions requires a tree node argument"
    s @aNode=$g(value)
    q aNode
    ;
    ;
    ; This function builds a child of the specified node at the given subscript.
    ; It will not overwrite an existing node, and will return an error if asked to build a node that already exists.
    ; If successful, this returns a string pointer to the node just created
    ; In case of error, a negative number is returned. This is consistent because a MUMPS array name (lvn or glvn) can't start with a number.
buildChild(aNode,childSubscript) q:$g(aNode)="" "-1, buildChild function requires a tree node argument"
    i $g(childSubscript)="" q "-2, buildChild function cannot build a node with an empty subscript"
    n targetNode
    s targetNode=$name(@aNode@(childSubscript))
    i $d(@targetNode) q "-3, buildChild cannot build "_targetNode_" because it already exisits"
    s @targetNode=""
    q targetNode
    ;
    ;
    ; This function builds a sibling of the specified node at the given subscript.
    ; It will not overwrite an existing node, and will return an error if asked to build a node that already exists.
    ; If successful, this returns a string pointer to the node just created
    ; In case of error, a negative number is returned. This is consistent because a MUMPS array name (lvn or glvn) can't start with a number.
buildSibling(aNode,siblingSubscript) q:$g(aNode)="" "-1, buildSibling function requires a tree node argument"
    i $g(siblingSubscript)="" q "-2, buildSibling function cannot build a node with an empty subscript"
    n parent,targetNode
    s parent=$$parent(aNode)
    i parent="" q "-3, buildSibling function cannot build a sibling of a root node"
    s targetNode=$name(@parent@(siblingSubscript))
    i $d(targetNode) q "-4, buildSibling cannot build "_targetNode_" because it already exisits"
    s @targetNode=""
    q targetNode
    ;
    ; This function destroys a node that has no children.
    ; It will not destroy a node that contains any children.
    ; If successful, this returns a string pointer to the parent of the node just destroyed.
    ; In case of error, a negative number is returned. This is consistent because a MUMPS array name (lvn or glvn) can't start with a number.
destroyNode(aNode) q:$g(aNode)="" "-1, destroyNode function requires a tree node argument"
    i $d(@aNode)>9 q "-2, destroyNode function cannot destroy a node with children."
    k @aNode
    q $$parent(aNode)
    ;
    ;
    ; ****  Tree Evaluation and Logic  ****
    ;
    ; This function returns the value held in the tree at aNode.
    ; If the node doesn't contain any value, this function will return NULL.
    ; In this way it's like the MUMPS intrinsic function $GET.
getNodeValue(aNode) q:$g(aNode)="" "" ; There's no way to return an error here, unfortunately.
    q $g(@aNode)
    ;
    ;
    ; This is just a useful helper function that glues a subscript onto an existing name.
    ; If the subscript is empty, this just returns the parentNode.
    ; It turns out that this is convenient when working with lastSubscript.
appendSubscript(parentNode,subscript) q:$g(parentNode)="" "-1, appendSubscript requires a parentNode argument"
    q:$g(subscript)="" parentNode
    q $name(@parentNode@(subscript))
    ;
    ;
    ; This function returns the last subscript of any node.
    ; Note that getting the leading part of aNode can be done with $$parent.
    ; In other words, aNode=$$appendSubscript($$parent(aNode),$$lastSubscript(aNode))
lastSubscript(aNode) q:$g(aNode)="" ""  ; unfortunately, we can't return an error here
    q:$ql(aNode)=0 "" ; if there's no last subscript
    q $qs(aNode,$ql(aNode))
    ;
    ;
    ; Stores data to the temporary data cache.
    ; The data must be in the form of a key-value pair where both the key and the value are strings.
    ; Moreover, the key cannot be empty.
    ; The temporary data store is a 'pile' of arrays: %dataCache0, %dataCache1, %dataCache2, ...
    ; When retrieving data from this 'pile', a search for a specified key is carried out progressing 'down' the pile.
    ; In other words, %dataCache15 will be check before %dataCache14, and %dataCache0 will be checked last.
    ; The search starts at the current value of %treeDepth.
    ; The upshot is that when a key-value pair is cached into the 'pile', it will be available during all processing of 'child' nodes on the source tree.
    ; In guarantee a value persist after the current node is processed, it needs to be cached to a level in the %dataCache below the current %treeDepth.
    ; There are 2 ways to do this:
    ;   a. Cache to the bottom of the pile by caching to %dataCache0
    ;   b. Cache to the level of the node's 'parent' at %dataCache_(%treeDepth-1)
    ; If you cache to the bottom of the data store pile, the data will persist throughout the entire tree crawl. This is a good way to set 'global' data.
    ; If you cache data to the parent level, it is visible to siblings of the current node.  Be careful about this, since some of the sibling nodes
    ; will already have been processed, and won't see this information.
    ; Moreover, when initially starting a tree crawl, the %treeDepth is 1, so the 'parent' of this level is the 'global' data store, %dataCache0.
    ; This function returns the value cached.
cacheKeyValue(key,value,specifyLevel) q:$g(key)="" "-1, cacheKeyValue requires a key"
    n tempDataStorePointer
    s specifyLevel=+$g(specifyLevel)
    i specifyLevel=1 s tempDataStorePointer="%dataCache0" ; 'global' data level
    e  i specifyLevel=-1 s tempDataStorePointer="%dataCache"_($g(%treeDepth)-1) ; 'parental' data level
    e  s tempDataStorePointer="%dataCache"_+$g(%treeDepth)
    s @tempDataStorePointer@(key)=$g(value)
    q $g(value)
    ;
    ;
    ; This retrieves a value associated to a specified key.
    ; The value will be retrieved from the temporary data store.
    ; The temporary data store is a 'pile' of arrays: %dataCache0, %dataCache1, %dataCache2, ...
    ; When retrieving data from this 'pile', a search for a specified key is carried out progressing 'down' the pile.
    ; In other words, %dataCache15 will be check before %dataCache14, and %dataCache0 will be checked last.
    ; The search starts at the current value of %treeDepth.
    ; The upshot is that when a key-value pair is cached into the 'pile', it will be available during all processing of 'child' nodes on the source tree.
    ; In order to make a value persist after the current node is processed, it needs to be cached to %dataStore0.
    ; This function returns the value cached.
getCachedValue(key) q:$g(key)="" "-1, getCachedValue requires a key"
    n tempTreeDepth,tempDataStorePointer,value
    f tempTreeDepth=+$g(%treeDepth):-1:0 d  q:$d(value)
    . s tempDataStorePointer="%dataCache"_tempTreeDepth
    . i $d(@tempDataStorePointer@(key)) s value=$g(@tempDataStorePointer@(key))
    q $g(value)
    ;
    ;
    ; Returns the value of %returnVal
getReturnVal() q $g(%returnVal)
    ;
    ;
    ; Sets the value of %returnVal
    ; Returns $GET(value) to allow chaining
setReturnVal(value) s %returnVal=$g(value)
    q $g(value)
    ;
    ;
    ; This will test if child nodes have been evaluated.
    ; It's useful for determining if we're evaluating the decision tree before or after the child nodes have been evaluated
childNodeReturnsExist() q ''$d(%childNodeReturns)
    ;
    ;
    ; This is an iterator over the child returns.
    ; It acts just like $ORDER
nextChildReturnNode(aChildNodePointer,direction) q $o(%childNodeReturns($g(aChildNodePointer)),$s($g(direction)=-1:-1,1:1))
    ;
    ;
    ; This retrieves the return value of a specific child node.
childReturnValue(aChildNodePointer) q:$g(aChildNodePointer)="" "" ; we can't return errors from this function
    q $g(%childNodeReturns(aChildNodePointer))
    ;
    ;
    ; This function finds the minimum value returned by any child functions.
    ; Sometimes a child function will return a blank, and we don't want this treated
    ; as 0.  In this case, the valForBlanks parameter can be used to specify how blanks are treated.
minChildReturn(valForBlanks) n aChildPointer,val,min
    s aChildPointer="",min=""
    f  s aChildPointer=$$nextChildReturnNode(aChildPointer) q:aChildPointer=""  d
    . s val=$$childReturnValue(aChildPointer)
    . i val="" s val=$g(valForBlanks)
    . i min="" s min=val q
    . i val<min s min=val
    q min
    ;
    ;
    ; This function finds the maximum value returned by any child functions.
    ; Sometimes a child function will return a blank, and we don't want this treated
    ; as 0.  In this case, the valForBlanks parameter can be used to specify how blanks are treated.
maxChildReturn(valForBlanks) n aChildPointer,val,max
    s aChildPointer="",max=""
    f  s aChildPointer=$$nextChildReturnNode(aChildPointer) q:aChildPointer=""  d
    . s val=$$childReturnValue(aChildPointer)
    . i val="" s val=$g(valForBlanks)
    . i max="" s max=val q
    . i val>max s max=val
    q max
    ;
    ;
    ; This sets the target node.
setTarget(aNode) q:$g(aNode)="" "-1, target node cannot be set to null."
    s %targetNode=aNode
    ; TODO:  Put in some sanity check here.
    q %targetNode
    ;
    ;
    ; This is a convenient way to get the target node
getTarget() q $g(%targetNode)
    ;
    ;
    ; This is a convenient way to get the source node.
    ; Note that there's no way to SET the source node.  This is managed completely by treeCrawler.
getSource() q $g(%sourceNode)
    ;
    ;
    ; Under normal operation, this returns 0 or 1 ONLY!!!
    ; In case of error, it can return another value as an error
evalRule(targetNode,sourceNode,rule)
    q:$g(sourceNode)="" "-1, evalRule requires sourceNode parameter"
        q:$g(targetNode)="" "-2, evalRule requires targetNode parameter"
    i $g(rule)="" q "" ; under normal operation we should never be passed a blank rule
    ;n $etrap
    ;s $etrap="g evalRuleError^PARSER2"
    i @rule q 1
    q 0
evalRuleError ; TODO:  Unwind the stack to the level of applyAction!!!
    n errorReturn
    s errorReturn="-3, MUMPS error evaluating rule '"_rule_"'.  Error:  "_$ecode
    s $ecode="" ; the error has been handled... ish
    q errorReturn ; This will happen at the level of 'evalRule'
    ;
    ;
    ; This function carries out the ACTION in the rule tree.
    ; If the action completes successfully, this function should return 1.
    ; Other return values represent error states.
applyAction(%sourceNode,%targetNode,actionCode)
    ; TODO: Finish this
    q:$g(%sourceNode)="" "-1, applyAction requires sourceNode parameter"
        q:$g(%targetNode)="" "-2, applyAction requires targetNode parameter"
    i $g(actionCode)="" q 1 ; A blank action is vaccuously correct
    ;n $etrap
    ;s $etrap="g applyActionError^PARSER2"
    x actionCode
    q 1
applyActionError ; TODO:  Unwind the stack to the level of applyAction!!!
    n errorReturn
    s errorReturn="-3, MUMPS error in action code '"_actionCode_"'.  Error:  "_$ecode
    s $ecode="" ; the error has been handled... ish
    q errorReturn ; This will happen at the level of 'applyAction'
    ;
    ;
    ; This function manages the evaluation of the rule tree
    ; It takes in a source root node, a destination root node, and an action/rule tree
    ; The parameters are:
    ;     sourceNode - a string pointer to the current node we're operating on in the source tree
    ;     targetNode - a string pointer to a node in the target tree.
    ;                  depending on how the design of the decision tree, this pointer may need to be updated.
    ;     decisionTree - a string pointer to an array containing the action/rule tree.
    ;                    The decision tree is made up of substructures like this:
    ;                                      (parent)
    ;                                          |
    ;                                       ACTION
    ;                                          |
    ;                                        RULE
    ;                                       /    \
    ;                                   true?    false?
    ;                                    |          |
    ;                                (child)      (child)
    ;                    The ACTION is any passed to $$applyAction, and RULE will be passed to $$evalRule.
    ;                        An example of such a tree is
    ;                               aDecisionTree("action")=""
    ;               aDecisionTree("rule")="$$parent(sourceNode)=""""" ; if the node has no parent
    ;               aDecisionTree(0,"action")="" ; if the rule evaluates to FALSE
    ;                           aDecisionTree(0,"rule")="" ; blank rules end the evaluation of the tree
    ;                           aDecisionTree(1,"action")="" ; if the rule evaluates to TRUE
    ;                           aDecisionTree(1,"rule")="$$firstChild(sourceNode)=""""" ; if the node has no child
    ;               aDecisionTree(1,0,"action")=""
    ;               aDecisionTree(1,0,"rule")=""
    ;               aDecisionTree(1,1,"action")="s %=$$cacheKeyValue(""one-node tree"",1)"
    ;               aDecisionTree(1,1,"rule")=""
    ; If parameters are missing, the function will return a negative number.
    ; Under normal operation, the return value of this function is set by the code in the ACTION nodes.
    ; In case of error, it will return a negative value.
evalDecisionTree(%sourceNode,%targetNode,decisionTree)
    q:$g(%sourceNode)="" "-1, evalDecisionTree requires sourceNode parameter"
    q:$g(%targetNode)="" "-2, evalDecisionTree requires targetNode parameter"
    q:$g(decisionTree)="" $$getReturnVal() ; if there's no decision tree, then nothing will change the return value
    ;
    n thisRule,evaluatedRule,thisAction,appliedAction
    s thisRule=""
    f  d  q:thisRule=""  q:$$getReturnVal()<0  s decisionTree=$name(@decisionTree@(evaluatedRule))
    . ; Action step
    . s thisAction=$g(@decisionTree@("action"))
    . s appliedAction=$$applyAction(%sourceNode,.%targetNode,thisAction) ; we need to allow actions to update the target node, but not source node
    . i appliedAction'=1 s %=$$setReturnVal("-4, "_appliedAction) q  ; In case of error, return it and stop the rule-crawl
    . ;
    . ; Rule step
    . s thisRule=$g(@decisionTree@("rule"))
    . q:thisRule=""  ; blank rules stop the rule-tree crawl
    . s evaluatedRule=$$evalRule(%sourceNode,%targetNode,thisRule) ; rules aren't allowed to manipulate either source or target nodes
    . i evaluatedRule'=0,evaluatedRule'=1 s %=$$setReturnVal("-5, "_evaluatedRule)  ; in case of eror, return it and stop the rule-crawl
    q $$getReturnVal()
    ;
    ;
    ; ****  Driver functions  ****
    ;
    ; This function is the main tree crawling routine.
    ; It will call itself recursively for each child of the current node.
    ; It also evaluates two different rule trees:  Once going down the tree before recursion, and once going back up after the recursion step.
    ; At least one rule tree needs to be passed in.
    ; Under normal operation, this function's return value is determined by the decision trees (especially the upwardDecisionTree).
    ; However, if an error is encountered, this function will return a negative number.
    ; Note that any return values that numerically evaluate to less than 0 will be treated as an error and will abort the tree crawl.
treeCrawler(%sourceNode,%targetNode,downwardDecisionTree,upwardDecisionTree)
    q:$g(%sourceNode)="" "-1, treeCrawler requires sourceNode parameter"
    q:$g(%targetNode)="" "-2, treeCrawler requires targetNode parameter"
    s downwardDecisionTree=$g(downwardDecisionTree)
    s upwardDecisionTree=$g(upwardDecisionTree)
    i downwardDecisionTree="",upwardDecisionTree="" q "-3, treeCrawler requires at least one decisionTree parameter"
    ;
    ; Initialization phase
    n tempTreeDepth,childNodePointer,childReturn
    s tempTreeDepth=$g(%treeDepth)
    n %treeDepth ; NEWwing %treeDepth keeps us from having to decrement it later.
    s %treeDepth=tempTreeDepth+1 ; This is much better than using $STACK, since we don't have to worry about nested subfunctions confusing us
    n @("%dataCache"_%treeDepth) ; create a data store at this %treeDepth.
    n %returnVal ; create a variable to store the return value.
    n %childNodeReturns ; create an array to store the returned data from the child nodes
    ;
    ; Pre-child-recursion action/rule phase 
    ; This is 'downward messaging'.  The evaluation will only have information from the current node or cached by higher nodes.
    ; Note that the decision tree can manipulate %targetNode but not %sourceNode
    s %returnVal=$$evalDecisionTree(%sourceNode,.%targetNode,downwardDecisionTree)
    q:%returnVal<0 %returnVal ; In case of an error, abort and return the error condition
    ;
    ; Recursion phase
    s %childNodeReturns="" ; This will ensure that $d(%childNodeReturns) evaluates to TRUE, which is a convenient flag for having evaluated the child nodes
    s childNodePointer=$$firstChild(%sourceNode)
    f  q:childNodePointer=""  d  q:childReturn<0  s childNodePointer=$$nextSibling(childNodePointer)
    . s childReturn=$$treeCrawler(childNodePointer,%targetNode,downwardDecisionTree,upwardDecisionTree) ; this can't manipulate either %sourceNode or %targetNode
    . i childReturn<0 s %returnVal=childReturn q ; If a child threw an error, we abort.
    . s %childNodeReturns(childNodePointer)=childReturn ; When there was no error, we make the return value available to the next action/rule phase
    ;
    ; Post-child-recursion action/Rule phase
    ; This is 'upward messaging'.  The rules will now evaluate using all previous information, plus the return values of the child nodes.
    q:%returnVal<0 %returnVal ; If we've hit an error, we abort
    q $$evalDecisionTree(%sourceNode,.%targetNode,upwardDecisionTree)
    ;
    ;
    ; This is the initial driver for the tree crawl.
    ; If the targetRoot and sourceRoot pointers don't point to existing nodes, it will create nodes
startTreeCrawl(sourceRoot,targetRoot,downwardDecisionTree,upwardDecisionTree)
    q:$g(sourceRoot)="" "-1, treeCrawler requires sourceNode parameter"
    q:$g(targetRoot)="" "-2, treeCrawler requires targetNode parameter"
    s upwardDecisionTree=$g(upwardDecisionTree)
    s downwardDecisionTree=$g(downwardDecisionTree)
    i downwardDecisionTree="",upwardDecisionTree="" q "-3, treeCrawler requires at least one decisionTree parameter"
    i $qs(sourceRoot,0)=$qs(targetRoot,0) q "-4, source and target trees must be completely disjoint."
    ;
    ;i '$d(sourceRoot) s @sourceRoot=""
    ;i '$d(targetRoot) s @targetRoot=""
    ;
    n %dataCache0 ; This will be where we store global-level key-value pairs
    q $$treeCrawler(sourceRoot,targetRoot,downwardDecisionTree,upwardDecisionTree) ; do the actual tree crawl.
    ;
    ;
    ;
    ; ****  Test Harnesses  ****
    ;
    ; This is a simple test harness
tester()
    d testNavigation(),testTreeCopy(),testTreeSerialize1()
    q
    ;
    ;
    ; This code tests the tree navigation functions
testNavigation() w !,!,"Testing tree navigation...",!
    n testArray,aNode,code,executable,output
    s testArray="root"
    s testArray(1)="one"
    s testArray(2)="two"
    s testArray(1,1)="one,one"
    s testArray(1,2)="one,two"
    s testArray(1,2,1)="one,two,one"
    ;
    f code="firstChild","nextSibling","parent" d
    . s aNode="testArray"
    . w !,"Testing "_code_"..."
    . f  d  s aNode=$q(@aNode) q:aNode=""
    . . s executable="s output=$$"_code_"(aNode)"
    . . x executable
    . . w !,"Input to "_code_" is '"_aNode_"'.   Output is '"_output_"'."
    . w !
    ;
    q
    ;
    ;
    ; This tests the ability to recursively copy a tree.
    ; The main idea is to test our ability to crawl a tree
testTreeCopy()
    n source,target,ruleTree
    s source="root"
    s source(1)="one"
    s source(2)="two"
    s source(1,1)="one,one"
    s source(1,2)="one,two"
    s source(1,2,1)="one,two,one"
    ;
    s sourceNode="source"
    s targetNode="target(""treeCopyOutput"")"
    ;
    ; This is a rule tree that simply copies the target tree to the source tree
    s ruleTree("action")="s %=$$setTarget($$appendSubscript($$getTarget(),$$lastSubscript($$getSource())))"
    s ruleTree("rule")="1"
    s ruleTree(0,"action")="s %=1/0 ; intentional error"
    s ruleTree(1,"action")="s %=$$setNodeValue($$getTarget(),$$getNodeValue($$getSource()))"
    s ruleTree(1,"rule")="" ; this ends the evaluation
    ;
    w !,$$startTreeCrawl(sourceNode,targetNode,"ruleTree")
    zwrite source
    zwrite target
    q
    ;
    ;
    ; This test the ability to recursively linearize a tree.
    ; The main idea is to test our ability to move nodes around between branches.
testTreeSerialize1()
    ; TODO:  Finish this
    n source,target,ruleTree
    s source="root"
    s source(1)="one"
    s source(2)="two"
    s source(1,1)="one,one"
    s source(1,2)="one,two"
    s source(1,2,1)="one,two,one"
    ;
    s sourceNode="source"
    s targetNode="target(""treeSerializeOutput"")"
    ;
    ; This is a rule tree that simply copies the target tree to the source tree in a serialized fashion
    s ruleTree("action")="s %=$$setNodeValue($$appendSubscript($$getTarget(),$$getSource()),$$getNodeValue($$getSource()))"
    s ruleTree("rule")=""
    ;
    w !,$$startTreeCrawl(sourceNode,targetNode,"ruleTree")
    zwrite source
    zwrite target
    q
    ;
    ;