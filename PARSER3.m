PARSER3
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
    d tester()
    q
    ;
    ; ====  OVERVIEW  ====
    ; This contains analysis tools to apply to a grammer (as defined by the syntax maps described in ^PARSER1).
    ; It also contains a few generic analysis tools that apply to the parse tree returned by the code in ^PARSER1.
    ; These tools are mainly just applications of the tree manipulation tools from ^PARSER2.
    ;
    ; The class of syntax diagrams that can be analyzed by these tags are those that consist of:
    ;       1) string literals
    ;       2) options
    ;       3) delimited lists
    ;       4) named tokens.
    ; Note that it's possible to draw a syntax diagram that isn't of this form, but that's actually 
    ; pretty tricky.
    ;
    ;
    ; ====  TECHNICAL TERMINOLOGY  ====
    ; The parser uses a sytax map, which is of the following form:
    ;       @map@(token1Name) = subtree1
    ;       @map@(token2Name) = subtree2
    ;                        ...
    ;       @map@(tokenName)  = subtree
    ; where each subtree is a MUMPS array describing the structure of each corresponding token. 
    ; The subtrees are described in greater detail below.
    ; For now, just observe that the syntax map requires at least one token definition, 
    ; and the first coordinate of the @map array consists of only token names.
    ;
    ; The structure of the subtrees, is of the generic form:
    ;               @subtree = subtree type
    ;               @subtree@("force") = the behavior of the parser to force when parsing any literals defined in this or deeper subtrees.
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
    ; This tag finds all other tokens referenced by a specified token
    ; This actually requires searching deeply in the token definition subtree, 
    ; since token references can be buried beneath other structures.
    ; Consider this example:
    ;       @map@("token3") ="subtreeChain"
    ;       @map@("token3",1)="literal"
    ;       @map@("token3",1,"value")="[["
    ;       @map@("token3",2)="delimList"
    ;       @map@("token3",2,"delimiter")="literal"
    ;       @map@("token3",2,"delimiter","value")="::"
    ;       @map@("token3",2,"listObject")="token"
    ;       @map@("token3",2,"listObject","value") = "token1"
    ;       @map@("token3",3)="literal"
    ;       @map@("token3",3,"value")="]]"
    ; Note that the reference to token1 is contained inside of a delimList reference.
    ; Another example is
    ;       @map@("token6")="options"
    ;       @map@("token6",1)="" ; the empty option
    ;       @map@("token6",2)="subtreeChain"
    ;       @map@("token6",2,1)="literal"
    ;       @map@("token6",2,1,"value")="("
    ;       @map@("token6",2,2)="token"
    ;       @map@("token6",2,2,"value")="token6"
    ;       @map@("token6",2,3)="literal"
    ;       @map@("token6",2,3,"value")=")"
    ;       @map@("token6",3)="token"
    ;       @map@("token6",3,"value")="token1"
    ; Here, there is a recursive reference to token6 and a reference to token1 inside of a subTreeChain.
    ; This code works by applying the tree-walking functions from ^PARSER2 to the grammer framework defined in ^PARSER1
countTokenReferences(outListPointer,mapPointer,token)
    ; First, a sanity-check on the parameters
    q:$g(outListPointer)="" "-1, outListPointer parameter is required by $$findReferences"
    q:$g(mapPointer)="" "-2, mapPointer parameter is required by $$findReferences"
    q:$g(token)="" "-3, token parameter is required by $$findReferences"
    q:'$d(@mapPointer@(token)) "-4, token "_token_" doesn't exist in the grammer map referenced by "_mapPointer
    ;
    n sourceNode,ruleTree ; This ruleTree will encompass the rules for finding all tokens
    s ruleTree("action")="" 
    s ruleTree("rule")="$$getNodeValue($$getSource())=""token""" ; check if the node of the map indicates a token pointer
    s ruleTree(0,"action")=""
    s ruleTree(0,"rule")="" ; if the node's not a token, we're done
    s ruleTree(1,"action")="s %=$$cacheKeyValue(""tokenName"",$$getNodeValue($$appendSubscript($$getSource(),""value"")))"
    s ruleTree(1,"rule")="$$getCachedValue(""tokenName"")'="""""
    s ruleTree(1,0,"action")="" ; we've found a reference to a token with no name?  This shouldn't happen, so an error would be appropriate here
    s ruleTree(1,0,"rule")=""
    s ruleTree(1,1,"action")="s %=$$cacheKeyValue(""tempTarget"",$$appendSubscript($$getTarget(),$$getCachedValue(""tokenName"")))"
    s ruleTree(1,1,"rule")=1
    s ruleTree(1,1,0,"action")="" ; this can't happen
    s ruleTree(1,1,0,"rule")=""
    s ruleTree(1,1,1,"action")="s %=$$setNodeValue($$getCachedValue(""tempTarget""),$$getNodeValue($$getCachedValue(""tempTarget""))+1)"
    s ruleTree(1,1,1,"rule")=""
    ;
    s sourceNode=$name(@mapPointer@(token))
    q $$startTreeCrawl^PARSER2(sourceNode,outListPointer,"ruleTree")
    ;
    ; *******************************
    ;
    ; This calls countTokenReferences for each token in a grammer to build a tree of token references.
    ; Note that these are only direct references.
    ; For instance, if token1 refers to token 2 in it's definition, and token2 refers to token1,
    ; the tree will contain these direct references, but won't directly include a reference from token1 to token1.
buildReferenceTree(outTreePointer,mapPointer)
    q:$g(outTreePointer)="" "-5, outTreePointer is a required parameter for $$buildReferenceTree"
    q:$g(mapPointer)="" "-6, mapPointer is a required parameter for $$buildReferenceTree"
    ;
    n token,outArrayPointer
    s token="" f  s token=$o(@mapPointer@(token)) q:token=""  d  q:$g(returned)<0
    . s returned=$$countTokenReferences($name(@outTreePointer@(token)),mapPointer,token)
    ;
    q:$g(returned)<0 returned
    q:'$d(returned) 0
    q 1
    ;
    ; *******************************
    ;
    ; This is the recursion step in Tarjan's algorithm.
    ; This function takes in a node in a graph and returns the lowest index of any node reachable by following edges of the graph.
    ; This function calls itself recursively.
    ; This function should not be called directly!!!  If you need this functionality, call dagStrongComponents instead.
    ; The parameters of this function are:
    ;       node - the current node
    ;       adjacencyMapPointer - a string pointer to an array containing the lists of adjacent nodes
    ;                 This should have the form
    ;                     @adjacencyMapPointer@(node,anotherNode) = <truthy>
    ;   nodeIndex - An array (pass by reference) containing the index of each node found so far
    ;           The root of nodeIndex will contain the largest index.
    ;   lowestReachableIndex - an array (pass by reference) containing the smallest index of any node in stackOfBackedgedNodes
    ;                  that can be reached by following edges from a given node.
    ;   assumeSelfAdjacent - If this is truthy, we'll assume each node has an implicit edge connecting it to itself.
    ;                This assumption is necessary in the true version of Tarjan's algorithm, because without it the strongly
    ;                connected components of the graph don't necessarily include all nodes in the graph.
    ;                When any cycle is "bad", though, it's nice if this assumption doesn't hold.  After all, we don't want
    ;                to try to sift out the true cycles of size 1 from the bajillion cycles we introduce by assuming all
    ;                nodes have an self-adjacent edge.
    ;   stackOfBackedgedNodes - An array (pass by reference) of this form:
    ;                        stackOfBackedgedNodes("stack") = n where n is the number of nodes on the stack
    ;                        stackOfBackedgedNodes("stack",1) = 1st node on the stack
    ;                        stackOfBackedgedNodes("stack",2) = 2nd node on the stack
    ;                                                        ...
    ;                        stackOfBackedgedNodes("stack",n) = nth node on the stack
    ;                        stackOfBackedgedNodes("index",<node>) = is <node> on the stack?
    ;                               This will be built and destroyed recursively by the algorithm
    ;       componentList - An array (pass by reference) that will store the list of discovered components.  It has the form:
    ;                        componentList(<node>) = <index of the root of the component the node is in>
    ;           This will be built recursively by the algorithm.
    ;           All nodes with the same value in componentList are in the same strongly connected component.
    ;           Depending on the setting of assumeSelfAdjacent, it's possible that some elements of componentList
    ;           will be of the form componentList(node)="".  These nodes are not in any strongly connected component.
    ; The crucial fact that makes Tarjan's algorithm work is that after each time lowestNodeReachable completes, the stackOfBackedgedNodes
    ; will retain 'Node' iff there is at least one path to another node with an index at most 'nodeIndex(Node)'.
    ; This function returns the lowest index that is reachable by following edges forward from the specified node.
lowestNodeReachable(node,adjacencyMapPointer,nodeIndex,lowestReachableIndex,assumeSelfAdjacent,stackOfBackedgedNodes,componentList)
    q:$g(node)="" "-1, node is a required parameter of $$lowestNodeReachable"
    i $g(nodeIndex(node))'="" q $g(lowestReachableIndex(node)) ; We only want to visit each node once
    n stackSize,aNode,lowestReachable,nodeStackLevel
    ;
    ; 1) Increment the index counter and assign that value as the index of the current node.
    s nodeIndex=$g(nodeIndex)+1
    s nodeIndex(node)=nodeIndex
    ;
    ; 2) Set the value of the lowest reachable node to a dummy value
    s (lowestReachableIndex(node),lowestReachable)=$s($g(assumeSelfAdjacent):nodeIndex,1:"")
    ;
    ; 3) Push this node onto the stackOfBackedgedNodes.
    ; By the end of this function, we'll have checked whether there is a path to anotherNode on stackOfBackedgedNodes with
    ; an index less than or equal to the index of the current node (nodeIndex).
    ; If there is no such path backwards to some other node on the stackOfBackedgedNodes, we'll remove this node from the stack.
    ; This will ensure that $$lowestReachable adds a node to the stackOfBackedgedNodes iff it has some back-edge to a node on the stackOfBackedgedNodes.
    s stackSize=$g(stackOfBackedgedNodes("stack"))+1
    s stackOfBackedgedNodes("stack")=stackSize
    s stackOfBackedgedNodes("stack",stackSize)=node
    s stackOfBackedgedNodes("index",node)=1
    ;
    ; 4) Visit all reachable unlabelled nodes, recursively calling $$lowestReachable.
    ; The main effect here is to move forward through the graph building up stackOfBackedgedNodes.
    ; By the time this recursive step is through, this stack will contain every node reachable from this one that contains a back-edge
    s aNode="" f  s aNode=$o(@adjacencyMapPointer@(node,aNode)) q:aNode=""  d
    . q:$g(nodeIndex(aNode))'=""  ; make sure we haven't visited aNode before
    . q:'$g(@adjacencyMapPointer@(node,aNode))  ; make sure @adjacencyMapPointer@(node,aNode) is truthy
    . s %=$$lowestNodeReachable(aNode,adjacencyMapPointer,.nodeIndex,.lowestReachableIndex,$g(assumeSelfAdjacent),.stackOfBackedgedNodes,.componentList)
    ;
    ; 5) Reset the lowest reachable index iff any reachable nodes that are on the stackOfBackedgedNodes have a lower index or can reach a lower index
    ; If there is a path from this node to another node on the stack with a lower index, then lowestReachable will be smaller than nodeIndex(node)
    s aNode="" f  s aNode=$o(@adjacencyMapPointer@(node,aNode)) q:aNode=""  d
    . q:'$g(stackOfBackedgedNodes("index",aNode))  ; only consider nodes that we know have a back-edge
    . i lowestReachable="" s lowestReachable=nodeIndex(aNode) ; We're treating "" as infinity, so anything is lower
    . i lowestReachable>nodeIndex(aNode) s lowestReachable=nodeIndex(aNode)
    . i lowestReachableIndex(aNode)'="",lowestReachable>lowestReachableIndex(aNode) s lowestReachable=lowestReachableIndex(aNode)
    s lowestReachableIndex(node)=lowestReachable
    ;
    ; 6) Pop dead-end nodes off of the stack, and add them to componentList
    ; These are nodes that have no outward edges to anything on the stackOfBackedgedNodes, or possibly even no outward edges at all.
    ; In either case, there is no path from a node with lowestReachable="" to anything lower on the stack.
    i lowestReachable="" d
    . s componentList(node)=""
    . k stackOfBackedgedNodes("index",node)
    . k stackOfBackedgedNodes("stack",stackSize) ; as a dead-end node, this is necessarilly the top of the stack
    . s stackOfBackedgedNodes("stack")=stackSize-1
    ;
    ; 7) If we've hit a node with a path back to itself, we can pop multiple elements of the stack.
    ; This happens exactly when 
    ;       - The stack contains a cycle from this node to itself,
    ;   - All higher nodes on the stack have back-edges to other nodes in the stack
    ;   - No higher node has an edge that goes to a node with a lower index than this one.
    i lowestReachable=nodeIndex(node) f nodeStackLevel=stackOfBackedgedNodes("stack"):-1:stackSize  d
    . ; Add aNode to the componentList
    . s aNode=$g(stackOfBackedgedNodes("stack",nodeStackLevel))
    . q:aNode="" ; this should never happen
    . s componentList(aNode)=lowestReachable
    . ;
    . ; Remove aNode from the stack
    . k stackOfBackedgedNodes("index",aNode)
    . k stackOfBackedgedNodes("stack",nodeStackLevel)
    . s stackOfBackedgedNodes("stack")=nodeStackLevel-1
    ;
    q lowestReachable
    ;
    ;
    ; This function computes the strongly connected components of a directed adjacency graph (DAG).
    ; This is done using a modified version of Tarjan's algorithm.  For more on this algorithm, see 
    ;      http://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
    ; The 5-cent version of the algorithm is this:
    ;     A.  Every time you visit a new node, give it a number, called "index".  
    ;         This is a unique label for each node and the index for a new node is always higher than previous nodes' number.
    ;     B.  In addition, each node is assigned a second number, called "LowInd".
    ;         This will be the smallest "index" of any node that can be reached by following arrows forward from the current node.
    ;     C.  Finally, use a stack to keep track of all of the nodes that have a 'back edge' that allows a path to an earlier node.
    ; Assigning "LowInd" is a bit tricky, and is typically done recursively.
    ; The main idea of the algorithm is that after every call to the recursive function 'lowestNodeReachable' the stack of
    ; back edges should contain only nodes that have a path back to other nodes that are also on the stack.
    ; This stack is what allows the algorithm to keep track of backwards paths to the current component.  Forward paths are
    ; handled by following the directed edges forward using recursion.
    ; 
    ; The specific modification we'll use here is is that the "LowInd" value in the algorithm need *not* be initialized to the same value as "index".
    ; Instead, depending on a function parameter, it can be set to a blank value, which is treated as positive infinity.
    ; The effect of this is that any node without an out-arrow will have a LowInd of infinity when the algorithm completes.
    ; In particular, isolated nodes with no edges will be distinguished from isolated nodes with a reflexive edge.
    ; The parameters of the function are:
    ;     outArray - A mumps array (pass in by referency) that contains a numeric indicator of which strongly connected component of the
    ;                graph that the node is contained in.  The numeric index is obtained by Tarjans algorithm, and the upshot is that
    ;        any 2 nodes with the same numeric index are in the same component.
    ;        Depending on the setting of assumeSelfAdjacent, it's possible to get a node index of null.  These nodes are not in any
    ;        strongly-connected componenet of the graph.
    ;        An example of outArray might be:
    ;             outArray("node1")=""
    ;             outArray("node2")=2
    ;             outArray("node3")=3
    ;             outArray("node4")=2
    ;        This example has a strongly-connected component containing just node3, which means it must have a reflexive edge;
    ;        it also has a strongly connected component containing node2 and node4.
    ;        Finally, note that node1 in this example doesn't belong to any strongly connected component.  Thus, node1 is not
    ;        part of any cycle.
    ;     adjacencyMapPointer - This parameter is a pointer to a datastructure representing the edges in a graph.
    ;                   An example element would be
    ;                     @adjacencyMapPointer@(nodeName,otherNodeName) = truthy value
    ;     assumeSelfAdjacent - The truthiness of this parameter determines how Tarjan's algorithm treats isolated nodes.
    ;                          If this parameter evaluates to TRUE, then each node is considered to be adjacent to itself.
    ;              In other words, if this parameter is TRUE, an implicit arrow will be assumed to point from each node to itself.
    ;              Note that the standard version of Tarjan's algorithm assumes these implicit arrows.
    ; This function returns the size of the largest strongly-connected component of the graph.
dagStrongComponents(outArray,adjacencyMapPointer,assumeSelfAdjacent)
    q:$g(adjacencyMapPointer)="" "-1, adjacencyMapPointer is a required parameter for $$dagStrongComponents"
    ;
    ; Initialization step
    n node,nodeIndex,componentSizes,index,lowestReachableIndex,maxSize,stackOfBackedgedNodes
    k outArray ; prevent leakage into the function from the caller
    s assumeSelfAdjacent=$g(assumeSelfAdjacent) ; Applying $GET once instead of applying it inside of a loop.
    ;
    ; Build 'outArray' as an index of the lowest reachable nodes using $$lowestNodeReachable
    s node=""
    f  s node=$o(@adjacencyMapPointer@(node)) q:node=""  s %=$$lowestNodeReachable(node,adjacencyMapPointer,.nodeIndex,.lowestReachableIndex,assumeSelfAdjacent,.stackOfBackedgedNodes,.outArray)
    ;
    ; Now count the size of the strongly connected components
    s maxSize=0,node="" f  s node=$o(outArray(node)) q:node=""  d:outArray(node)'=""
    . s index=outArray(node)
    . s componentSizes(index)=$g(componentSizes(index))+1
    . s:maxSize<componentSizes(index) maxSize=componentSizes(index)
    ;
    q maxSize
    ;
    ;
    ; *******************************
    ;
    ;
    ; This function returns 1 if we can prove that a specific token definition in @mapPointer never returns "".
    ; An example of a token that is provably non-null is:
    ;      @map@("token1")="literal"
    ;      @map@("token1","value")="FEEP"
    ; This token returns 'FEEP' only.  There is absolutely no way that it could ever be null.
    ; An example of a token that is *not* provably non-null is:
    ;      @map@("token2")="options"
    ;      @map@("token2",1)="" ; a null option
    ;      @map@("token2",2)="literal"
    ;      @map@("token2",2,"value")="FOO"
    ; This token can be either null or 'FOO'.
    ; In most cases, the problem won't be this clear-cut.  The difficulty is references to external tokens.
    ; We don't necessarily know whether an external token always returns non-null or not.
    ; For example, consider this token definition:
    ;      @map@("token3")="options"
    ;      @map@("token3",1)="literal"
    ;      @map@("token3",1,"value")="BAR"
    ;      @map@("token3",2)="token"
    ;      @map@("token3",2,"value")="token4"
    ;      @map@("token3",3)="token"
    ;      @map@("token3",3,"value")="token5"
    ; Unless we can say whether or not token4 and token5 can be proven to always return non-null, we have no idea if token3 is provably non-null.
    ; To work around this difficulty, we pass in a list of tokenAssumptions.  This is just an array whose subscripts are token names and whose
    ; values have a truthiness indicating whether or not that token is assumed to always be non-null.
    ; We can hopefully draw a useful conclusion about a token under these assumptions about the other tokens.
    ; Additionally, this function returns a reduced map that contains the definition of the specified token up to the first known provably non-null.
    ;
    ; The algorithm is a tree walk of the grammer in @mapPointer@(token) using the machinery from ^PARSER2.
    ; The following logic is used:
    ;        subtree type     Rule for $$provablyNonNull return
    ;        ------------     ---------------------------------
    ;    ""       Return 0
    ;    literal      Return 0 iff the literal value is "", else return 1
    ;    token        Return the tokenAssumption for the specific token
    ;    delimList    Check listObject recursively, returning 1 iff listObject is provably non-null
    ;    subtreechain     Check each subtree recursively, returning 1 iff *any* subtree is provably non-null
    ;    options      Check each subtree recursively, returning 1 iff *all* subtrees are provably non-null
    ;
    ; The parameters of this function are as follows:
    ;       token - The name of the token we're interested in.  This must appear as a first subscript in @mapPointer
    ;   mapPointer - A string pointer to the array defining the grammer
    ;   tokenAssumptions - An array (pass by reference) of this form:
    ;                  tokenAssumptions( <otherToken> ) = truthy or falsey
    ;              Here the truthiness of the value indicates whether the otherToken is provably non-null.
    ;              An otherToken which has no subscript in tokenAssumptions is assumed to possibly allow null values.
    ;              (In other words, when a token doesn't appear in the tokenAssumptions array, we assume it might be null.)
    ;   reducedMapPointer - A string pointer to an array where the reduced definition of token will be placed.
    ;               This reduced grammar will only include the definition of the token up to the first provably non-null portion.
    ;               The grammer returned here will be at most as large as the grammer definition in @mapPointer@(token).
provablyNonNull(token,mapPointer,%tokenAssumptions,reducedMapPointer)
    q:$g(token)="" "-1, the provablyNonNull function requires a token to operate on."
    q:$g(mapPointer)="" "-2, The provablyNonNull function requires a mapPointer parameter."
    q:$g(reducedMapPointer)="" "-3, The provablyNonNull function requires a reducedMapPointer parameter."
    ;
    n sourceNode,ruleTreeDown,ruleTreeUp
    s sourceNode=$name(@mapPointer@(token))
    ;
    ; <At this point, the cached subtree 'type' is inherited from the *parent* node.>
    ; <The cached 'non-null found' value will be inherited from the parent node based on the effects of a sibling node.>
    ; <The cached 'stop' value might be inherited from the parent node, but it can also be setup here and passed to children.>
    ;
    ; This ruleTree will be evaluated in the downward direction and used to copy (most of) the tree
    ; It will also be used to determine the type of subtree (subtreechain, delimList, etc.) we're in.
    s ruleTreeDown("rule")="$$getCachedValue(""stop"")" ; Can we save effort by stopping the copy immediately?
    s ruleTreeDown(0,"action")="s %=$$setTarget($$appendSubscript($$getTarget(),$$lastSubscript($$getSource())))" ; setting the target here to ease recursion
    s ruleTreeDown(0,"rule")="$$getCachedValue(""type"")=""subtreeChain""&$$getCachedValue(""non-null found"")" ; Have we found a sibling non-null in a chain?
    s ruleTreeDown(0,1,"action")="s %=$$cacheKeyValue(""stop"",1)" ; we shouldn't copy this node or any children
    s ruleTreeDown(0,0,"rule")="$$getCachedValue(""type"")=""delimList""&$$getCachedValue(""non-null found"")" ; Have we found non-null content in a delimList?
    s ruleTreeDown(0,0,1,"action")="s %=$$cacheKeyValue(""stop"",1)" ; we shouldn't copy this node or any children
    s ruleTreeDown(0,0,0,"rule")="$$isMetaData^PARSER1($$getSource())" ; catches 'force' subnodes
    s ruleTreeDown(0,0,0,1,"action")="s %=$$cacheKeyValue(""stop"",1)" ; we shouldn't copy this node or any children
    ;
    ; At this point, we've got every reason to copy the current node
    s ruleTreeDown(0,0,0,0,"action")="s %=$$setNodeValue($$getTarget(),$$getNodeValue($$getSource()))" ; Perform the actual copy
    s ruleTreeDown(0,0,0,0,"rule")="$$definesCompleteSubtree^PARSER1($$getSource())" ; For roots of subtrees, we'll want to cache off the 'type'
    s ruleTreeDown(0,0,0,0,1,"action")="s %=$$cacheKeyValue(""type"",$$getNodeValue($$getSource()))" ; this will hold the subtree 'type'
    ;
    ; <At this point, we've cached the subtree 'type' of the current node -- assuming it exists>
    ;
    ; In between the ruleTrees, the treeCrawler will recurse to the child subnodes.
    ;
    ; <At this point, the cached subtree 'type' is the type of the *current* node iff it defines a subtree, and the parent node otherwise.>
    ; <The cached 'non-null found' value still comes from the *parent* node, usaully based on the effect of a sibling node.>
    ; <The cached 'stop' flag could come from this node, or an ancestor.>
    ;
    ; This ruleTree will be evaluated as an upward decision tree
    ; First we make sure this isn't a node we can safely skip... might as well save effort when possible
    ; We only evaluate return values at the root of (possibly null) subtrees, and we can safely skip nodes that have the 'stop' flag.
    s ruleTreeUp("action")="s %=$$setReturnVal("""")" ; this is technically unnecessary
    s ruleTreeUp("rule")="'$$getCachedValue(""stop"")&$$definesCompleteSubtree^PARSER1($$getSource())" 
    ;
    ; NULL
    s ruleTreeUp(1,"rule")="$$getCachedValue(""type"")=""""" ; null token
    s ruleTreeUp(1,1,"action")="s %=$$setReturnVal(0)" ; clearly *not* provably non-null
    ;
    ; TOKEN
    s ruleTreeUp(1,0,"rule")="$$getCachedValue(""type"")=""token""" ; Use the tokenAssumptions to check nullity of an external token
    s ruleTreeUp(1,0,1,"action")="s %=$$setReturnVal(+$g(%tokenAssumptions($$getNodeValue($$appendSubscript($$getSource(),""value"")))))"
    s ruleTreeUp(1,0,1,"rule")="+$$getReturnVal()"
    s ruleTreeUp(1,0,1,1,"action")="s %=$$cacheKeyValue(""non-null found"",1,-1)" ; Info about non-null gets passed up to the parent node where siblings will see it
    ;
    ; DELIMLIST
    s ruleTreeUp(1,0,0,"rule")="$$getCachedValue(""type"")=""delimList""" ; A delimList is provably non-null iff it has a provably non-null content
    s ruleTreeUp(1,0,0,1,"action")="s %=$$setReturnVal($$childReturnValue($$appendSubscript($$getSource(),""content"")))" ; the content of the delimList
    s ruleTreeUp(1,0,0,1,"rule")="+$$getReturnVal()"
    s ruleTreeUp(1,0,0,1,1,"action")="s %=$$cacheKeyValue(""non-null found"",1,-1)" ; Info about non-null gets passed up to the parent node so siblings can see it
    ;
    ; OPTIONS
    s ruleTreeUp(1,0,0,0,"rule")="$$getCachedValue(""type"")=""options""" ; A list of options is provably non-null iff *all* subtrees are
    s ruleTreeUp(1,0,0,0,1,"action")="s %=$$setReturnVal($$minChildReturn(1))" ; checking minimum of subtree returns checks that all are 1
    s ruleTreeUp(1,0,0,0,1,"rule")="+$$getReturnVal()"
    s ruleTreeUp(1,0,0,0,1,1,"action")="s %=$$cacheKeyValue(""non-null found"",1,-1)" ; Info about non-null gets passed up to the parent node
    ;
    ; LITERAL
    s ruleTreeUp(1,0,0,0,0,"rule")="$$getCachedValue(""type"")=""literal"""
    s ruleTreeUp(1,0,0,0,0,1,"action")="s %=$$setReturnVal(""""'=$$getNodeValue($$appendSubscript($$getSource(),""value"")))" ; is the value non-null?
    s ruleTreeUp(1,0,0,0,0,1,"rule")="+$$getReturnVal()"
    s ruleTreeUp(1,0,0,0,0,1,1,"action")="s %=$$cacheKeyValue(""non-null found"",1,-1)" ; Info about non-null gets passed up to the parent node
    ;
    ; SUBTREECHAIN
    s ruleTreeUp(1,0,0,0,0,0,"rule")="$$getCachedValue(""type"")=""subtreeChain"""
    s ruleTreeUp(1,0,0,0,0,0,1,"action")="s %=$$setReturnVal($$maxChildReturn(0))" ; any subtree that is provably non-null will force a return value of 1 here
    s ruleTreeUp(1,0,0,0,0,0,1,"rule")="+$$getReturnVal()"
    s ruleTreeUp(1,0,0,0,0,0,1,1,"action")="s %=$$cacheKeyValue(""non-null found"",1,-1)" ; Info about non-null gets passed up to the parent node
    ;
    q $$startTreeCrawl^PARSER2(sourceNode,reducedMapPointer,"ruleTreeDown","ruleTreeUp")
    ;
    ;
    ; This tag creates a subtree of the map such that the subtree contains only the portion of the
    ; token definitions that will contain the first character.
    ; In other words, this subtree will be the possible ways to start the token.
    ; Suppose that we have this grammer for a token:
    ;     @map@("token1")="subtreeChain"
    ;     @map@("token1",1)="options"
    ;     @map@("token1",1,1)="" ; null is an option
    ;     @map@("token1",1,2)="token"
    ;     @map@("token1",1,2,"value")="token2"
    ;     @map@("token1",2)="literal"
    ;     @map@("token1",2,"value")="foo"
    ;     @map@("token1",3)="token"
    ;     @map@("token1",3,"value")="token3"
    ; Possible strings in this grammer for token1 are:
    ;       token3 token4
    ;       token2 token3 token4
    ; The tree that contains only the possible first portions of this grammer is
    ;     @map@("token1")="subtreeChain"
    ;     @map@("token1",1)="options"
    ;     @map@("token1",1,1)="" ; empty option
    ;     @map@("token1",1,2)="token"
    ;     @map@("token1",1,2,"value")="token2"
    ;     @map@("token1",2)="literal"
    ;     @map@("token1",2,"value")="foo"
    ; Notice that this is *not* a grammer that exactly describes the possible first tokens.
    ; Instead, it's the minimal part of the tree necessary to include every possible first character of the tokens.
    ; In fact, the subtree above allows these strings
    ;           foo
    ;       token2 foo
    ; Also notice that every token occuring in the subtree could be the leading token in a represention of the
    ; token being defined (token1, in the example above).
    ; 
    ; The algorithm to create this reduced tree is easy in principle and tricky in practice.
    ; The entire reason for the complication is that we don't know if a token can return NULL or not.
    ; For instance, say we have a grammer that allows a string of this form:
    ;            token1""token2
    ; If token1 is always non-null, then the reduced tree returned should only allow strings of this form:
    ;                token1
    ; On the other hand, token1 might be null under certain circumstances, in which case strings of this form should also be allowed:
    ;                token1""token2
    ; To work around this difficulty, we need to apply the provablyNonNull function itteratively, allowing it to prove that more and more
    ; tokens are provably non-null.
    ; Once provablyNonNull has stabilized the collection of provably non-null tokens, it's easy to pick out the set of possible leading elements
    ; of each token.
    ; Note that provablyNonNull *must* eventually stabilize for either of the following 2 reasons:
    ;       1) A token shown to be provably non-null cannot later be shown to have null as a possible return possibility.
    ;      This is a monotonicity condition, and there are finitely many tokens that can eventually be shown to be provably non-null.
    ;   2) Every pass of provablyNonNull will make the reduced map no larger than it was before.
    ;      This is a monotonicity condition, and the starting map is of finite size.
    ; Of course, we haven't placed a bound on how long it can take the provablyNonNull helper function to stabilize...
leadingReferences(mapPointer,reducedMapPointer)
    q:$g(mapPointer)="" "-1, mapPointer is a required parameter for the leadingReferences function"
    q:$g(reducedMapPointer)="" "-2, reducedMapPointer is a required parameter for the leadingReferences function"
    ;
    n token,tokenAssumptions,isTokenNonNull,assumptionsChanged,tempMapIn,tempMapOut
    s token=""
    m tempMapIn=@mapPointer
    ;
    f  s assumptionsChanged=0 d  q:assumptionsChanged=0
    . f  s token=$o(@mapPointer@(token)) q:token=""  d
    . . k tempMapOut
    . . s isTokenNonNull=$$provablyNonNull(token,"tempMapIn",.tokenAssumptions,"tempMapOut")
    . . i $g(tokenAssumptions(token))=isTokenNonNull q
    . . s assumptionsChanged=1,tokenAssumptions(token)=isTokenNonNull
    . . k tempMapIn(token) m tempMapIn=tempMapOut ; TODO:  make this more effecient, and avoid the Merge statement
    ;
    k @reducedMapPointer m @reducedMapPointer=tempMapIn
    q 1
    ;
    ; *******************************
    ;
    ; This tag analyzes the 'map' looking for instances of left-recursion in the grammer.
    ; This is a good sanity check on the 'map' because left-recursion will trap our recursive parser in an infinite loop.
    ; Algorithm:
    ;   1) Use leadingReference to produce a subtree of only possible leading characters
    ;   2) Use buildReferenceTree to reduce this new grammer to just a tree of token references.
    ;   3) Use dagStrongComponents to determine if any cycles could exist in the reduced grammar.
    ; Because leadingReferenceForToken returns a tree whose tokens are all of the possible ways a given token could
    ; begin, this algorithm will produce all possible ways that an infinite loop could occur in the parsers.
    ; The parameters of this function are
    ;       mapPointer - a string pointer to the array holding the map for parsing the tokens.
    ;   outList - an array (pass by reference) that holds the strong components of the directed adjacency graph
    ;         formed by the leading tokens of the grammer.
    ;         If this is non-empty, then there is an instance of left-recursion in the grammar.
    ; The function itself returns 1 or more when at least one instance of left-recursion is detected
    ; The function will return 0 when there is no left-recursion, and -1 in case of an error.
hasLeftRecursiveReferences(mapPointer,outList)
    q:$g(mapPointer)="" "-1, the mapPointer parameter is required by the leftRecursiveReferences function."
    n reducedMap,adjacencyGraph
    s %=$$leadingReferences(mapPointer,"reducedMap")
    s %=$$buildReferenceTree("adjacencyGraph","reducedMap")
    q $$dagStrongComponents(.outList,"adjacencyGraph")
    ;
    ;
    ; ********************************
    ;
    ; This tag is the test harness for this routine
tester() 
    d testCountTokenReferences(),testDagStrongComponents()
    d testprovablyNonNull(),testHasLeftRecursiveReferences()
    q
    ;
    ;
testCountTokenReferences()
    n map,outArray,myGrammerMap
    ;
    ; Here is a reasonable map of a single token in a grammar
    s map="myGrammerMap"
    s @map@("token4")="subtreeChain"
    s @map@("token4",1)="options"
    s @map@("token4",1,1)="token"
    s @map@("token4",1,1,"value")="token1"
    s @map@("token4",1,2)="token"
    s @map@("token4",1,2,"value")="token3"
    s @map@("token4",1,3)="literal"
    s @map@("token4",1,3,"value")="TOKEN_NULL"
    s @map@("token4",2)="literal"
    s @map@("token4",2,"value")="="
    s @map@("token4",3)="token"
    s @map@("token4",3,"value")="token2"
    ;
    w !,$$countTokenReferences("outArray",map,"token4")
    ;
    zwrite outArray
    q
    ;
    ;
testDagStrongComponents()
    n outArray,adjacencyMapPointer,assumeSelfAdjacent,anAdjacencyMap
    s adjacencyMapPointer="anAdjacencyMap",assumeSelfAdjacent=0
    s @adjacencyMapPointer@("token1","token2")=1
    s @adjacencyMapPointer@("token1","token3")=1
    s @adjacencyMapPointer@("token2","token3")=1
    s @adjacencyMapPointer@("token3","token4")=1
    s @adjacencyMapPointer@("token3","token1")=1 ; a cycle
    s @adjacencyMapPointer@("token4","token4")=1 ; a self-referential node
    s @adjacencyMapPointer@("token5","token6")=1 ; an isolated component
    w !,"The size of the largest strongly connected component of the adjacency graph is ",$$dagStrongComponents(.outArray,adjacencyMapPointer,assumeSelfAdjacent),!
    zwrite outArray
    ;
    k outArray,@adjacencyMapPointer
    s @adjacencyMapPointer@("token1","token2")=1
    s @adjacencyMapPointer@("token1","token3")=1
    s @adjacencyMapPointer@("token4","token2")=1
    s @adjacencyMapPointer@("token4","token3")=1
    w !,"The size of the largest strongly connected component of the adjacency graph is ",$$dagStrongComponents(.outArray,adjacencyMapPointer,assumeSelfAdjacent),!
    zwrite outArray
    ;
    k outArray,@adjacencyMapPointer
    s @adjacencyMapPointer@("token1","token2")=1
    s @adjacencyMapPointer@("token2","token3")=1
    s @adjacencyMapPointer@("token5","token1")=1
    s @adjacencyMapPointer@("token1","token4")=1
    s @adjacencyMapPointer@("token4","token5")=1
    w !,"The size of the largest strongly connected component of the adjacency graph is ",$$dagStrongComponents(.outArray,adjacencyMapPointer,assumeSelfAdjacent),!
    zwrite outArray
    ;
    k outArray,@adjacencyMapPointer
    s @adjacencyMapPointer@("token1","token3")=1
    s @adjacencyMapPointer@("token1","token2")=1
    s @adjacencyMapPointer@("token2","token1")=1
    s @adjacencyMapPointer@("token3","token2")=1
    s @adjacencyMapPointer@("token3","token4")=1
    s @adjacencyMapPointer@("token4","token3")=1
    w !,"The size of the largest strongly connected component of the adjacency graph is ",$$dagStrongComponents(.outArray,adjacencyMapPointer,assumeSelfAdjacent),!
    zwrite outArray
    ;
    q
    ;
    ;
testprovablyNonNull()
    n map,aGrammerMap,%tokenAssumptions,reducedMapPointer,aReducedMap,token,temp
    ;
    ; Here is a reasonable map of a few tokens in a grammar
    s map="aGrammerMap"
    ; 
    s @map@("token1")="subtreeChain"
    s @map@("token1",1)="options"
    s @map@("token1",1,1)="literal"
    s @map@("token1",1,1,"value")="1"
    s @map@("token1",1,2)="literal"
    s @map@("token1",1,2,"value")="2"
    s @map@("token1",1,3)="literal"
    s @map@("token1",1,3,"value")="3"
    s @map@("token1",2)="delimList"
    s @map@("token1",2,"content")="options"
    s @map@("token1",2,"content",1)="literal"
    s @map@("token1",2,"content",1,"value")="1"
    s @map@("token1",2,"content",2)="literal"
    s @map@("token1",2,"content",2,"value")="2"
    s @map@("token1",2,"content",3)="literal"
    s @map@("token1",2,"content",3,"value")="3"
    s @map@("token1",2,"content",4)="literal"
    s @map@("token1",2,"content",4,"value")="0"
    s @map@("token1",2,"delimiter")=""
    ;
    s @map@("token2")="delimList"
    s @map@("token2","content")=""
    s @map@("token2","delimiter")="options"
    s @map@("token2","delimiter",1)="token"
    s @map@("token2","delimiter",1,"value")="token1"
    s @map@("token2","delimiter",2)="token"
    s @map@("token2","delimiter",2,"value")="token3"
    ;
    s @map@("token3")="options"
    s @map@("token3",1)="literal"
    s @map@("token3",1,"value")="" ; yes, I know this is dumb
    s @map@("token3",2)="literal"
    s @map@("token3",2,"value")="foo"
    ;
    s @map@("token4")="subtreeChain"
    s @map@("token4",1)="options"
    s @map@("token4",1,1)="token"
    s @map@("token4",1,1,"value")="token1"
    s @map@("token4",1,2)="token"
    s @map@("token4",1,2,"value")="token3"
    s @map@("token4",1,3)="literal"
    s @map@("token4",1,3,"value")="0"
    s @map@("token4",2)="literal"
    s @map@("token4",2,"value")="="
    s @map@("token4",3)="token"
    s @map@("token4",3,"value")="token2"
    ;
    s reducedMapPointer="aReducedMap"
    s %tokenAssumptions("token1")=1
    s %tokenAssumptions("token3")=1
    ;
    f token="token1","token2","token3","token4" d
    . w !,"Is "_token_" provably non-null under the assumptions? ",$$provablyNonNull(token,map,.%tokenAssumptions,reducedMapPointer),!
    . k temp m temp=@reducedMapPointer@(token)
    . zwrite temp
    ;
    k @reducedMapPointer,%tokenAssumptions
    w !,!,$$leadingReferences(map,reducedMapPointer),!
    zwrite @reducedMapPointer
    ;
    q
    ;
    ;
testHasLeftRecursiveReferences()
    ; TODO:  Finish this!!!
    n map,aGrammerMap,outList
    s map="aGrammerMap"
    ; 
    s @map@("token1")="subtreeChain"
    s @map@("token1",1)="options"
    s @map@("token1",1,1)="literal"
    s @map@("token1",1,1,"value")="1"
    s @map@("token1",1,2)="literal"
    s @map@("token1",1,2,"value")="2"
    s @map@("token1",1,3)="literal"
    s @map@("token1",1,3,"value")="3"
    s @map@("token1",2)="delimList"
    s @map@("token1",2,"content")="options"
    s @map@("token1",2,"content",1)="literal"
    s @map@("token1",2,"content",1,"value")="1"
    s @map@("token1",2,"content",2)="literal"
    s @map@("token1",2,"content",2,"value")="2"
    s @map@("token1",2,"content",3)="literal"
    s @map@("token1",2,"content",3,"value")="3"
    s @map@("token1",2,"content",4)="literal"
    s @map@("token1",2,"content",4,"value")="0"
    s @map@("token1",2,"delimiter")=""
    ;
    s @map@("token2")="delimList"
    s @map@("token2","content")=""
    s @map@("token2","delimiter")="options"
    s @map@("token2","delimiter",1)="token"
    s @map@("token2","delimiter",1,"value")="token1"
    s @map@("token2","delimiter",2)="token"
    s @map@("token2","delimiter",2,"value")="token3"
    ;
    s @map@("token3")="options"
    s @map@("token3",1)="literal"
    s @map@("token3",1,"value")="" ; yes, I know this is dumb
    s @map@("token3",2)="literal"
    s @map@("token3",2,"value")="foo"
    ;
    s @map@("token4")="subtreeChain"
    s @map@("token4",1)="options"
    s @map@("token4",1,1)="token"
    s @map@("token4",1,1,"value")="token1"
    s @map@("token4",1,2)="token"
    s @map@("token4",1,2,"value")="token3"
    s @map@("token4",1,3)="literal"
    s @map@("token4",1,3,"value")="0"
    s @map@("token4",2)="literal"
    s @map@("token4",2,"value")="="
    s @map@("token4",3)="token"
    s @map@("token4",3,"value")="token2"
    ;
    w !,"Left-recursion detected:  ",$$hasLeftRecursiveReferences(map,.outList)>0
    w !,"Strong components of leading tokens:",!
    zwrite outList
    ;
    q
    ;
    q