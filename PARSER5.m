ROUTINE PARSER5
PARSER5
    ; Copyright (C) 2021 Neils Schoenfelder
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
    d tester()
    ;
    ;
    ; ==== OVERVIEW =====
    ; This is a parsser for any languaged that can expressed as a railroad track syntax diagram.
    ;
    ;
    ; ===== TECHNICAL TERMINOLOGY ====
    ; To model a railroad-track-style syntax diagram, we use kind of directed graph.
    ; The nodes of this graph can be string literals, or can refer to other graphs.
    ;
    ; Define an "Ordered Path Graph" as a directed graph with the following properties:
    ;    1) There is some unique node with no incoming arrows.
    ;       This is the "start" node of the ordered path graph.
    ;    2) There is some unique node with no outgoing arrows.
    ;       This is the "end" node of the ordered path graph.
    ;    3) For each node, there is some fixed ordering of it's outgoing arrows.
    ;    4) Each node contains a non-empty list of "node types" describing it.
    ;       For us, each "node type" will just be a string describing an element of the grammar.
    ;       Note that for a given node, this stack may consist of just a single empty string.
    ;    5) Each node contains an "expected Parseable" object that is either
    ;          i.  A string literal (possibly the empty string), or
    ;          ii. A pointer to another ordered path graph
    ;              In practice, all of our graphs will be named.
    ; We also need functions that map from nodes of this graph to
    ;    a) The arrows out from that node
    ;    b) The arrows into that node.
    ;
    ; A "parse position" is some pointer into the input string of code that we're trying to parse.
    ; Typically, this parse position will just be an integer that we can feed to a $extract function.
    ; In the future, it might be changed to be a line number + character offset, or something else.
    ; Whatever form it takes, there has to be a way to add an integer to it.
    ; This will be used to 'move our read pointer' along the input string.
    ;
    ; Define a "parse path" to be an ordered list of tuples, each of which contains the following:
    ;    1) A node from an ordered path graph
    ;    2) An outgoing arrow from that node
    ;    3) a parse position describing how far we've read along the input string to get to this point
    ; Moreover, we require that the arrow for each tuple points to the node from the next tuple.
    ; This allows us to describe a chain of nodes and arrows from a directed graph.
    ;
    ;
    ; ==== DATA MODEL ====
    ; aGraph("nodes",1) = aNode
    ; aGraph("nodes",2) = anotherNode
    ;                  ...
    ;
    ; aGraph("arrows",1) = anArrow
    ; aGraph("arrows",2) = anotherArrow
    ;                   ...
    ;
    ; aNode = the data type that this node holds.  Either "string" or "subgraph"
    ; aNode("typeStack",1) = The string literal or the index of the subgraph
    ; aNode("typestack",2...) = higher levels of the definition stack
    ;
    ; anArrow("source") = the node this arrow originates from
    ; anArrow("target") = the node this arrow points to
    ;
    ; aGraph("index","arrowsFromNode",1) = arrows from the node held in aGraph("nodes",1)
    ; aGraph("index","arrowsToNode",1) = arrows pointing to the node held in aGraph("nodes",1)
    ; for example, aGraph("index","arrowsToNode",13,56) is valued iff node 13 is pointed to by arrow 56.
    ;
    ; aGraph("index","startNode") = aNode number
    ; aGraph("index","endNode") = aNode number
    ;
    ; inputString = the string to parse.  A mumps scalar, though it may contain CR and LF characters.
    ; aParsePosition = an integer reresenting the current column in the inputString
    ;
    ; path(1,"arrow") = the ID for anArrow
    ; path(1,"parsePosition") = the parsePosition in the input string that got us to the source node of the arrow
    ; path(2,"arrow") = the ID for anotherArrow.  Note that the source of anotherArrow must be the target of anArrow.
    ;
    ; subgraphTemplates("subgraph name") = a graph (including the "nodes", "arrows", and "index")
    ; 
    ;
    ; ==== SKETCH OF ALGORITHMS ====
    ; Given 2 graphs, we can surgically paste one into the other by replacing a specified node.
    ; Define a way of surgically adding graphs as follows:
    ;    SurgicallyAddSugbraph(aGraph, aNode, subGraphTemplate)
    ;        If aNode is not part of aGraph, then throw an error
    ;        Let inArrows be the collection of arrows into aNode
    ;        Let outArrows be the collection of arrows leaving aNode
    ;        Let typeStack be the stack of node types from aNode
    ;        Let newSubGraph be a copy of subGraphTemplate
    ;        for each node in newSubGraph, 
    ;           prepend typeStack to the list of types for the given node
    ;        let subStartNode be the start node of newSubGraph
    ;        Let subEndNode be the endNode of newSubGraph
    ;        add all nodes and arrows of newSubGraph to aGraph
    ;        for each arrow in inArrows,
    ;           change the arrow so it points to subStartNode
    ;        for each arrow in outArrows,
    ;           change the arrow so it points from subEndNode
    ;        Delete aNode
    ;        Return aGraph
    ;
    ; Note that the SurgicallyAddSugbraph function mangles aGraph by replacing
    ; aNode with a copy of subGraphTemplate.  Also, the list of what's
    ; been replaced has been kept in the list of node types in the graph.
    ;
    ; Given a path, we can append a new node (and arrow) to it using this function:
    ;    AppendToPath(aGraph,aPath,aNode,parsePositionIncrement)
    ;        Let lastNode be the last node in aPath
    ;        Let lastArrow be the last arrow in aPath
    ;        If lastArrow doesn't point to aNode in aGraph, throw an error
    ;        get parsePosition from lastNode
    ;        Let newParsePosition = parsePosition + parsePositionIncrement
    ;        let firstOutArrow be the "first" outgoing arrow from aNode in aGraph
    ;        create a tuple containing aNode, firstOutArrow, and newParsePosition
    ;        Append this tuple to aPath (which is a list of such tuples)
    ;        retun aPath
    ; 
    ; We'll often take the "wrong branch" when moving through our graph, so we'll
    ; need to backtrack along our path to find the last node with more branches remaining.
    ; The following function does this for us.
    ;    backtrackToLastBranch(aPath)
    ;       Let lastTuple be the last tuple in aPath
    ;       If lastTuple == null
    ;           QUIT
    ;       Let lastArrow be the arrow from lastTuple
    ;       If lastArrow == null
    ;           remove lastTuple from aPath
    ;           GOTO backtrackToLastBranch+1
    ;       Let nextArrow be the next arrow in the given ordering of arrows that originate from the same node as lastArrow
    ;       If nextArrow == null
    ;           remove lastTuple from aPath
    ;           GOTO backtrackToLastBranch+1
    ;       Set the arrow in lastTuple to be nextArrow
    ;       QUIT
    ;
    ; We use these functions and objects to describe the general algorithm for parsing.
    ; The main idea is to repeatedly surgically insert new subgraphs into the 'main' syntax graph
    ; as needed.  Say, for example, if we have the following graph:
    ;              AB:       1  --- ab ---
    ;                         /            \
    ;                   "" --<              >-- ""
    ;                       2 \            /
    ;                          a -- AB -- b
    ; This graph can make strings like ab, aabb, aaabbb, and so forth.
    ; When parsing the string "aabb" with this graph, we'll not be successful on path (1),
    ; so we'll backtrack and try path (2), but that includes a symbol for the graph AB.
    ; The way we handle that graph symbol is to paste a copy of it into our graph, to give us this:
    ;                     1  --------------- ab ---------------  
    ;                      /                                    \
    ;                "" --<           1  --- ab ---              >-- ""
    ;                    2 \           /            \           /
    ;                       a -- "" --<              >-- "" -- b
    ;                                2 \            /
    ;                                   a -- AB -- b
    ; On this extended graph, the string "aabb" parses by taking path (2) and then path (1) as follows:
    ;                "" --<           1  --- ab ---              >-- ""
    ;                    2 \           /            \           /
    ;                       a -- "" --<              >-- "" -- b
    ; Straightening this path out a bit just gives
    ;                "" --- a -- "" -------- ab -------- "" -- b --- ""
    ; Which is exactly the path representation of the string "aabb".
    ;
    ; The graph expanding and traversing algorithm is given below.
    ;    Traverse(parsingGraph,aPath,stringToParse,indexedGraphTemplates)
    ;       ; ///// Data setup: //////
    ;       Let lastTuple = the last tuple in aPath
    ;       Let lastArrow = the arrow from lastTuple
    ;       If lastArrow == null,
    ;           DO backtrackToLastBranch(aPath)
    ;           GOTO Traverse+1
    ;       Let aNode be the node pointed at by lastArrow
    ;       Let expectedParsable = the expected parsable object from aNode
    ;       Let parsesPosition be the parse position from lastTuple
    ;       Let remainingString be the portion of stringToParse that starts at parsePosition
    ;       ; ///// Check for complete parsing: //////
    ;       If remainingString == "" && aNode == null
    ;           return aPath ;# In this case, we're done.  We've parsed the whole string and moved through the whole parse graph
    ;       ; ///// Check if this parse ended to early //////
    ;       If remainingString == "" || aNode == null
    ;           DO backtrackToLastBranch(aPath)
    ;       ; ////// Handle pointers to another path graph: ///////
    ;       Else If expectedParsable is a pointer to an ordered path graph in indexedGraphTemplates,
    ;           Let aGraphTemplate = the graph template from indexedGraphTemplates corresponding to expectedParsable
    ;           If aGraphTemplate==null
    ;              throw error:"Cannot find graph template for "_expectedParsable
    ;           DO SurgicallyAddSubgraph(aGraph, aNode, subGraphTemplate)
    ;       ; ////// Handle string literals: //////
    ;       Else If expectedParsable is a literal string,
    ;           If remainingString begins with expectedParsable, 
    ;              then AppendToPath(parsingGraph,aPath,aNode,length of expectedParsable)
    ;           Else DO backtrackToLastBranch(aPath)
    ;       ; ////// Sanity Check: /////
    ;       Else throw error:"expected parsable from node is not a string or a pointer to another graph"
    ;       GOTO Traverse+1
    ;
    ; The Traverse function doesn't have any recursion, so it's very MUMPS-friendly, and won't lead to <STACK> errors.
    ; Unfortunately, it does build a potentially HUGE graph by repeated surgical insertions of subgraphs.
    ; This could burn up a lot of in-process memory, leading to <STORE> errors.
    ; Alternatively, it could build the graph in a global, leading to a <FILEFULL> error.
    ; On the other hand, once we've built a large graph of the syntax, we can certainly re-use it for the next parse.
    ;
    ;
    ; ==== CAUTIONARY NOTE ====
    ; This algorithm will choke and die on a left-recursive graph definition.
    ; For example, let's say that we have the following graph:
    ;                                           "+"
    ;                                         /     \
    ;      Expression:    "" -- Expression --<       >-- Expression
    ;                                         \     /
    ;                                           "-" 
    ; If we tried to parse any string with that graph, we'd immediately hit an infinite loop.
    ; When we tried to replace the first "Expression" marker with a new subgraph, the new graph
    ; we generated in this way would again start with an "Expression" marker.  When we replaced
    ; that new "Expression" marker, we'd get another graph that also started with an "Expression"
    ; marker that we'd have to replace, and so on, and so on, and so on forever.
    ; 
    ; As always, with any Top-down parser, watch out for instances of left-recursive grammars.
    ;
    ;
    ; =====================================================================================
    ; ===================================== CORE CODE =====================================
    ;
    ; ====== Node handling code ======
    ; Functions that manipulate the nodes in a graph
    ;
    ;
    ; Given a string pointer to a graph, this function will add a node with the given dataType (string or subgraph)
    ; and typeStack to the graph.  The number of the newly added node will be returned.
    ; For example,
    ;       $$addNode("myGraph","string",.typeStack)
    ; might return a number like "65".
addNode(aGraph,dataType,typeStack)
    i $d(typeStack)<10 s $EC=",UtypeStack data is required,"
    i dataType'="string",dataType'="subgraph" s $EC=",Udatatype of '"_dataType_"' is invalid,"
    n newNode
    s newNode=1+$o(@aGraph@("nodes",""),-1)
    s @aGraph@("nodes",newNode)=dataType
    m @aGraph@("nodes",newNode,"typeStack")=typeStack
    q newNode
    ;
    ;
removeNode(aGraph,aNode)
    n arrowsIn,arrowsOut
    s arrowsIn=$$incomingArrows(aGraph,aNode)
    i $o(@arrowsIn@(""))'="" s $ECODE=",Ucannot remove node with incoming arrows,"
    s arrowsOut=$$outgoingArrows(aGraph,aNode)
    i $o(@arrowsOut@(""))'="" s $ECODE=",Ucannot remove node with outgoing arrows,"
    k @aGraph@("nodes",aNode)
    q 1
    ;
    ;
    ; Given a string pointer to a graph, and the number of a node in that graph,
    ; this function returns data held by this node.
    ; This data could be a string literal or the index of some subgraph template
nodeData(aGraph,aNode)
    q @aGraph@("nodes",aNode,"typeStack",1)
    ;
    ;
    ; Given a string pointer to a graph, a node number in that graph, and a
    ; dotted array, this function will copy the typeStack from the given
    ; node into the dotted "out" array.
    ; The type stack will be of this form:
    ;       out(1) = the data for this node
    ;       out(2) = the next higher level of the definition descent stack
    ;             ...
    ; The function returns 1 if successful
nodeStack(aGraph,aNode,out)
    k out
    m out=@aGraph@("nodes",aNode,"typeStack")
    q 1
    ;
    ;
    ; Given a string pointer to a graph, a node number in that graph, and a
    ; dotted array, this function will copy the typeStack from the dotted "in"
    ; array into the specified node on top of any data currently in the stack.
    ; The format of the "in" array should be
    ;       in(1) = data to put on the next stack level of aNode's type stack
    ;       in(2) = data to put on the level after that
    ;            ...
    ; The function returns a 1 if successful
addToNodeStack(aGraph,aNode,in)
    n stackHeight,stackPosition
    s stackPosition=""
    f  s stackPosition=$o(in(stackPosition)) q:stackPosition=""  d
    . s stackHeight=1+$o(@aGraph@("nodes",aNode,"typeStack",""),-1)
    . m @aGraph@("nodes",aNode,"typeStack",stackHeight)=in(stackPosition)
    q 1
    ;
    ;
    ; This function returns either "string" or "subgraph" when given a string
    ; pointer to a graph and a node number.
nodeDataType(aGraph,aNode)
    q @aGraph@("nodes",aNode)
    ;
    ;
    ; Sets the extra data scalar that's been cached in this node (if any)
    ; The dataPath parameter is a string containing a comma-delimited list.
    ; The scalar will be stored in the location in the node defined by the dataPath parameter.
setNodeExtraData(aGraph,aNode,dataPath,scalar)
    n leadingPath,pos
    s leadingPath=$na(@aGraph@("nodes",aNode,"typeStack",1))
    f pos=1:1:$l(dataPath,",") s leadingPath=$na(@leadingPath@($p(dataPath,",",pos)))
    s @leadingPath=scalar
    q 1
    ;
    ;
    ; Returns the extra data scalar that's been cached in this node (if any)
    ; The dataPath parameter is a string containing a comma-delimited list.
    ; The scalar will be stored in the location in the node defined by the dataPath parameter.
getNodeExtraData(aGraph,aNode,dataPath)
    n leadingPath,pos
    s leadingPath=$na(@aGraph@("nodes",aNode,"typeStack",1))
    f pos=1:1:$l(dataPath,",") s leadingPath=$na(@leadingPath@($p(dataPath,",",pos)))
    q $g(@leadingPath)
    ;
    ;
    ; ===== Arrow-handling functions =====
    ; These functions return information about the arrows in the graph.
    ; They also add or remove arrows.
    ;
    ;
    ; Given a string pointer to a graph, and the number of a node in that graph,
    ; this function returns a string pointer to a $ORDERable array of arrow numbers
    ; in aGraph that all point to aNode.
    ; For example:
    ;       stringPointerToOutput = $$incomingArrows("myGraph",2)
    ; will return a value such that 
    ;       s arrowNumber="" f  s arrowNumber=$O(@stringPointerToOutput@(arrowNumber)) q:arrowNumber=""
    ; will iterate over the arrow numbers from the graph that go into the node myGraph("nodes",2)
    ; In other words, for each arrowNumber, we have myGraph("arrows",arrowNumber,"target")=2
incomingArrows(aGraph,aNode)
    q $na(@aGraph@("index","arrowsToNode",aNode))
    ;
    ;
    ; Given a string pointer to a graph, and the number of a node in that graph,
    ; this function returns a string pointer to a $ORDERable array of arrow numbers
    ; in aGraph that all originate at aNode.
    ; For example:
    ;       stringPointerToOutput = $$outgoingArrows("myGraph",2)
    ; will return a value such that 
    ;       s arrowNumber="" f  s arrowNumber=$O(@stringPointerToOutput@(arrowNumber)) q:arrowNumber=""
    ; will iterate over the arrow numbers from the graph that go out from the node myGraph("nodes",2)
    ; In other words, for each arrowNumber, we have myGraph("arrows",arrowNumber,"source")=2
outgoingArrows(aGraph,aNode)
    q $na(@aGraph@("index","arrowsFromNode",aNode))
    ;
    ;
    ; Given an arrow, this one returns the "next" arrow that originates from the same source
nextOutgoingArrow(aGraph,anArrow)
    n sourceNode,outArrows,nextArrow
    s sourceNode=$$sourceNode(aGraph,anArrow)
    s outArrows=$$outgoingArrows(aGraph,sourceNode)
    s nextArrow=$o(@outArrows@(anArrow))
    q nextArrow
    ;
    ;
    ; Given a node, this returns the first arrow leaving this node.
firstOutgoingArrow(aGraph,aNode)
    q $o(@$$outgoingArrows(aGraph,aNode)@(""))
    ;
    ;
    ; Given a node, this returns the first arrow leaving this node.
firstIncomingArrow(aGraph,aNode)
    q $o(@$$incomingArrows(aGraph,aNode)@(""))
    ;
    ;
    ; Finds the arrow from sourceNode to targetNode.
    ; If no such arrow exists, an empty string is returned.
findArrow(aGraph,sourceNode,targetNode)
    n sourceArrows,targetArrows,anArrow
    s sourceArrows=$$outgoingArrows(aGraph,sourceNode)
    s targetArrows=$$incomingArrows(aGraph,targetNode)
    f  s anArrow=$o(@sourceArrows@($g(anArrow))) q:anArrow=""  q:$d(@targetArrows@(anArrow))
    q anArrow
    ;
    ;
    ; This function takes in a string pointer to a graph, along with the numbers
    ; of two nodes in the graph.  It adds an arrow between these nodes if needed.
    ; The function returns the ID number of the arrow from the source to the
    ; target node.
    ; The parameter 'thisArrow' is optional
addArrow(aGraph,sourceNode,targetNode,thisArrow) 
    ; A sanity check to see if we actually need to add this arrow
    n thatArrow
    s thatArrow=$$findArrow(aGraph,sourceNode,targetNode) ; check if we already have an arrow from the source to the target
    i thatArrow'="" q thatArrow
    ; A sanity check to make sure we're not adding an incoming arrow to the start node or an outgoing arrow to the end node
    i $$startNode(aGraph)=targetNode s $EC=",Ucannot add an incoming arrow to the start node,"
    i $$endNode(aGraph)=sourceNode s $EC=",Ucannot add an outgoing arrow to the end node,"
    ; Actually modify the graph
    s:'$d(thisArrow) thisArrow=1+$o(@aGraph@("arrows",""),-1) ; get a new number for this arrow
    s @aGraph@("arrows",thisArrow,"source")=sourceNode
    s @aGraph@("arrows",thisArrow,"target")=targetNode
    s @aGraph@("index","arrowsFromNode",sourceNode,thisArrow)=""
    s @aGraph@("index","arrowsToNode",targetNode,thisArrow)=""
    q thisArrow
    ;
    ;
    ; Takes in a string pointer to a graph and the ID number of an arrow to
    ; remove from the graph.
removeArrow(aGraph,arrowNumber)
    n oldSource,oldTarget
    s oldSource=@aGraph@("arrows",arrowNumber,"source")
    s oldTarget=@aGraph@("arrows",arrowNumber,"target")
    k @aGraph@("arrows",arrowNumber)
    k @aGraph@("index","arrowsFromNode",oldSource,arrowNumber)
    k @aGraph@("index","arrowsToNode",oldTarget,arrowNumber)
    q 1
    ;
    ;
    ; Moves an existing arrow to a new source and target nodes.
    ; This will return the ID number of the arrow if successful
repointArrow(aGraph,arrowNumber,newSource,newTarget)
    i $$removeArrow(aGraph,arrowNumber),$$addArrow(aGraph,newSource,newTarget,arrowNumber) q arrowNumber
    q "" ; This should never happen
    ;
    ;
    ; Given a string pointer to a graph and the number of an arrow in that graph,
    ; this function returns the number of the node that the arrow points to.
targetNode(aGraph,anArrow)
    q @aGraph@("arrows",anArrow,"target")
    ;
    ;
    ; Given a string pointer to a graph and the number of an arrow in that graph,
    ; this function returns the number of the node that the arrow originates from.
sourceNode(aGraph,anArrow)
    q @aGraph@("arrows",anArrow,"source")
    ;
    ;
    ; ===== Graph manipulation functions =====
    ; Functions in this section return information about the graph, or modify the
    ; graph in some way.  They are all concerned with the graph-level properties.
    ; For instance, these functions may start node of the graph, or modify its
    ; topology.
    ;
    ;
    ; Given a string pointer to a graph, this function returns the number of the start
    ; node for the graph.
startNode(aGraph)
    q @aGraph@("index","startNode")
    ;
    ;
    ; Sets the "start" node of an ordered path graph to the specified node
setStartNode(aGraph,aNode)
    i $$firstIncomingArrow(aGraph,aNode)'="" s $EC=",Ucannot set node '"_aNode_"' to start node since it has incoming arrows,"
    s @aGraph@("index","startNode")=aNode
    q $$startNode(aGraph)
    ;
    ;
    ; Given a string pointer to a graph, this function returns the number of the
    ; end node in that graph.
endNode(aGraph)
    q @aGraph@("index","endNode")
    ;
    ;
    ; Sets the "end" node of an ordered path graph to the specified node
setEndNode(aGraph,aNode)
    ;i $o(@$$outgoingArrows(aGraph,aNode)@(""))'="" s $EC=",Ucannot set node '"_aNode_"' to end node since it has outgoing arrows,"
    i $$firstOutgoingArrow(aGraph,aNode)'="" s $EC=",Ucannot set node '"_aNode_"' to end node since it has outgoing arrows,"
    s @aGraph@("index","endNode")=aNode
    q $$endNode(aGraph)
    ;
    ;
    ; When given a string pointer to a graph, a node number from that graph, and
    ; a string pointer to a collection of subgraph templates, this function will
    ; replace aNode with a copy of the subgraph that is found in aNode's data.
    ; If aNode holds a string literal, then no change will be made.
    ; If this function completes successfully, it will return the node number of
    ; the "start node" of the subgraph that has replaced aNode.
    ; This returned node is the new node that's in place of the one we just surgically
    ; removed and replaced.
surgicallyAddSugbraph(aGraph,aNode,subGraphTemplates)
    q:$$nodeDataType(aGraph,aNode)'="subgraph" "" ; Make sure the node data isn't a string literal
    n nodeData,subgraphCopy,oldNode,newNode,nodeStack,crosswalk,dataType,sourceNode
    n anArrow,targetNode,startNode,endNode,arrowsList,newStartNode,newEndNode
    s nodeData=$$nodeData(aGraph,aNode)
    m subgraphCopy=@subGraphTemplates@(nodeData)
    ; Add the nodeStack from aNode to each node in subgraphCopy
    s %=$$nodeStack(aGraph,aNode,.nodeStack)
    s oldNode=""
    f  s oldNode=$O(subgraphCopy("nodes",oldNode)) q:oldNode=""  s %=$$addToNodeStack("subgraphCopy",oldNode,.nodeStack)
    ; Now add the nodes of this subgraph into aGraph
    f  s oldNode=$o(subgraphCopy("nodes",oldNode)) q:oldNode=""  d
    . s %=$$nodeStack("subgraphCopy",oldNode,.nodeStack)
    . s dataType=$$nodeDataType("subgraphCopy",oldNode)
    . s newNode=$$addNode(aGraph,dataType,.nodeStack)
    . s crosswalk(oldNode)=newNode
    ; Reset the startNode or endNode of the new graph if necessary
    i aNode=$$startNode(aGraph) d
    . s newStartNode=crosswalk($$startNode("subgraphCopy"))
    . s %=$$setStartNode(aGraph,newStartNode)
    i aNode=$$endNode(aGraph) d
    . s newEndNode=crosswalk($$endNode("subgraphCopy"))
    . s %=$$setEndNode(aGraph,newEndNode)
    ; Add the arrows of the subgraph into aGraph
    s anArrow=""
    f  s anArrow=$o(subgraphCopy("arrows",anArrow)) q:anArrow=""  d
    . s sourceNode=crosswalk($$sourceNode("subgraphCopy",anArrow))
    . s targetNode=crosswalk($$targetNode("subgraphCopy",anArrow))
    . s %=$$addArrow(aGraph,sourceNode,targetNode) 
    ; Redirect the arrows into aNode to the startNode of the copy of the newly added subgraph
    s startNode=crosswalk($$startNode("subgraphCopy"))
    s arrowsList=$$incomingArrows(aGraph,aNode)
    f  s anArrow=$o(@arrowsList@(anArrow)) q:anArrow=""  s %=$$repointArrow(aGraph,anArrow,$$sourceNode(aGraph,anArrow),startNode)
    ; Redirect the arrows out of aNode so they originate from the end node of the newly added subgraph
    s endNode=crosswalk($$endNode("subgraphCopy"))
    s arrowsList=$$outgoingArrows(aGraph,aNode)
    f  s anArrow=$o(@arrowsList@(anArrow)) q:anArrow=""  s %=$$repointArrow(aGraph,anArrow,endNode,$$targetNode(aGraph,anArrow))
    ; Remove aNode
    s %=$$removeNode(aGraph,aNode)
    q startNode
    ;
    ;
    ; =====  Parse Position managers ======
    ; The parse position is the current location in a collection of text.
    ; Currently, this is just an integer for the position in a string, however,
    ; it could be something more complicated, in principle.  For example, the
    ; parse position may later be changed to a line and character count in a
    ; block of text, or a TAG+offset^|ENVIRONMENT|ROUTINE pointer.
    ;
    ;
    ; Sets the parse position to an acceptable initial value.
    ; Currently, the initial position is the integer 0.  However,
    ; in the future, when the parse position represents something
    ; more complex, the initial position may be crazier.
initializeParsePosition(parsePosition)
    k parsePosition s parsePosition=1
    q 1
    ;
    ; Reads the specified number of characters from the input string,
    ; starting at the parse position.
    ; If the next characters match expectations, then the parsePosition
    ; variable will be updated to a new position as well.
    ; If this function is successful, it will return the number of characters
    ; parsed (ie, the length of expectedText).  Otherwise returns "".
    ; We wrap this trivial operation in a helper function in the hope
    ; that someday parsePosition will be a line+collumn pair, rather
    ; than a simple integer.
readFromParsePosition(parsePosition,expectedText,inputText)
    n endReadPosition,readLength
    s readLength=$l(expectedText)
    s endReadPosition=parsePosition+readLength-1
    i expectedText'=$e(inputText,parsePosition,endReadPosition) q -1
    s parsePosition=parsePosition+readLength
    q readLength
    ;
    ; Returns truthy if we're at the end of the inputString
    ; We wrap this trivial operation in a helper function in the hope
    ; that someday parsePosition will be a line+collumn pair, rather
    ; than a simple integer.
parsePositionIsEnd(parsePosition,inputText)
    q $e(inputText,parsePosition)=""
    ;
    ;
    ; ===== Parse Path managers =====
    ; The parse path is a series of arrows from the ordered syntax graph that connect head-to-tail.
    ; In other words, the target of one arrow must be the source of the next arrow (if there is one).
    ; The source node of the last arrow in the parse path represents the last successfully parsed
    ; node of the syntax graph.
    ; The end node of the path (the target node of the last arrow in the path) is the node of the
    ; ordered syntax graph that we're currently trying to use to parse the input string.
    ;
    ;
    ; Takes in a string pointer to aPath, a string pointer to aGraph, the ID or an arrow in that graph,
    ; and a parse position along the input string.
    ; The arrow that is passed in must "connect" to the last arrow in the path (unless the path is empty).
    ; The parsePosition should represent the position on the input string *after* following the arrow that
    ; was passed in.
    ; If the parsePosition is not set for an arrow, it means that we haven't actually parsed using the
    ; target node of that last arrow
appendArrowToPath(aPath,aGraph,anArrow,parsePosition)
    n pathLength,lastArrow
    s pathLength=+$o(@aPath@(""),-1)
    i pathLength>0 d
    . s lastArrow=@aPath@(pathLength,"arrow")
    . i $$targetNode(aGraph,lastArrow)'=$$sourceNode(aGraph,anArrow) s $EC=",Uarrows don't form a connected path,"
    . i +parsePosition<@aPath@(pathLength,"parsePosition") s $EC=",Uparse position must be monitonically increasing,"
    s @aPath@(1+pathLength,"arrow")=anArrow
    s @aPath@(1+pathLength,"parsePosition")=parsePosition
    q pathLength
    ;
    ;
    ; Takes in a string pointer to aPath, a string pointer to aGraph, the ID or a node in that graph,
    ; and a parse position along the input string.
    ; The node that is passed in must be the ending to the last arrow in the path (unless the path is empty).
    ; This function will look up the first arrow leaving the given node and call appendArrowToPath.
appendNodeToPath(aPath,aGraph,newNode,parsePosition)
    n pathLength,nextArrow,lastNode ; ,lastArrow,outArrows
    s pathLength=$$endOfPath(aPath,aGraph,.lastNode)
    s nextArrow=$$findArrow(aGraph,lastNode,newNode)
    i nextArrow="" s $EC=",Uthe graph doesn't contain an arrow from the last node in the path to newNode,"
    q $$appendArrowToPath(aPath,aGraph,nextArrow,parsePosition)
    ;
    ;
    ; Removes the last arrow in the given path
removeLastArrowFromPath(aPath)
    s pathLength=+$o(@aPath@(""),-1)
    k @aPath@(pathLength) ; this will error if the path contains no arrows
    q +$o(@aPath@(""),-1)
    ;
    ;
    ; Pops off the last path elements until an unexplored branch is found.
    ; That unexplored arrow is appended onto the path.
    ; The new path length is returned by the function.
backtrackPathToLastBranch(aPath,aGraph)
    n STOP,pathLength,anArrow,parsePosition
    s anArrow=""
    s STOP=0
    f  d  q:STOP
    . s pathLength=+$o(@aPath@(""),-1)
    . i pathLength=0 s STOP=1 q  ; we've backtracked to the initial node
    . s anArrow=@aPath@(pathLength,"arrow")
    . s parsePosition=@aPath@(pathLength,"parsePosition")
    . k @aPath@(pathLength)
    . s anArrow=$$nextOutgoingArrow(aGraph,anArrow)
    . i anArrow'="" s STOP=1,%=$$appendArrowToPath(aPath,aGraph,anArrow,parsePosition)
    q +$o(@aPath@(""),-1) ; return the new path length
    ;
    ;
    ; Reads the end node and parse position from aPath.
    ; The dotted aNodeand parsePosition parameters will return the ID of
    ; the last node in the path, and the parsePosition of the target node
    ; of that arrow.
    ; This node is typically the one we're trying to use to parse the input string.
    ; This function also returns the path length.
endOfPath(aPath,aGraph,aNode,parsePosition)
    k aNode,parsePosition
    n pathLength,anArrow
    s pathLength=+$o(@aPath@(""),-1)
    i pathLength=0 q pathLength
    s anArrow=@aPath@(pathLength,"arrow")
    s aNode=$$targetNode(aGraph,anArrow)
    s parsePosition=@aPath@(pathLength,"parsePosition")
    q pathLength
    ;
    ;
    ; Sets the nodes of the path in the out parameter.
    ; Returns the number of nodes in the path
pathNodes(aPath,aGraph,out)
    n pos,anArrow,outPos
    k out
    f  s pos=$o(@aPath@($g(pos))) q:pos=""  d
    . s anArrow=@aPath@(pos,"arrow")
    . s outPos=1+$o(out(""),-1)
    . s out(outPos)=$$sourceNode(aGraph,anArrow)
    i $g(anArrow)'="" s outPos=outPos+1,out(outPos)=$$targetNode(aGraph,anArrow)
    q +$g(outPos)
    ;
    ;
    ; Returns the string parsed by this path
pathString(aPath,aGraph,nodeCount)
    n pos,aNode,pathString,out
    s:+$g(nodeCount)<1 nodeCount=$$pathNodes(aPath,aGraph,.out)
    s pathString=""
    f pos=1:1:nodeCount d
    . s aNode=out(pos)
    . i $$nodeDataType(aGraph,aNode)="string" s pathString=pathString_$$nodeData(aGraph,aNode)
    q pathString
    ;
    ;
    ; Returns the length of the input string that has been parsed in this path
pathStringLength(aPath,aGraph,nodeCount)
    q:$d(nodeCount) $l($$pathString(aPath,aGraph,nodeCount))
    q $l($$pathString(aPath,aGraph))
    ;
    ;
    ; ===== The main graph traverser =====
    ; This main driver function is responsible for finding the longest path through
    ; the ordered syntax graph that parses the input text.
    ;
    ; This function tries to find a parse path through a grammar graph.
    ; It does this by selectively expanding the grammar graph when necessary,
    ; and by appending and backtracking the path.
    ; It returns 1 if it found a path through the grammar that consumed the entire inputText.
    ; If it couldn't find a path through the grammar graph that used up all of the inputText,
    ; then it will return a 0.
    ;
    ; To get the actual longest path, you need to pass in a dotted longestPath parameter.
    ; Recall that the longestPath is made up of arrows.  So if the Traverse operation did
    ; not return a success, it means that the last arrow of the longestPath points at an
    ; end node that did not parse correctly.
Traverse(aGraph,indexedGraphTemplates,inputText,longestPath)
    n pathLength,parsePosition,lastNode,STOP,SUCCESS,nodeDataType,thisPath,thisPathStringLength,longestPathStringLength,newNode,expectedString
    k longestPath
    s STOP=0,SUCCESS=0
    f  d  q:STOP  ; Traverse the ordered grammar graph to find the first path through it.
    . s pathLength=$$endOfPath("thisPath",aGraph,.lastNode,.parsePosition) ; Get the last node in the path and the parse position corresponding to that node
    . i pathLength=0 d  ; Handle the case where the path is empty.
    . . s lastNode=$$startNode(aGraph) ; the path may be empty, so we need to start at the beginning of the graph
    . . s %=$$initializeParsePosition(.parsePosition) ; If the path is empty, we also need an initial parse position
    . ; Store the original node label in the node data stack.  Assuming we never recycle node numbers, this tracks the "lineage" of nodes.
    . i $$getNodeExtraData(aGraph,lastNode,"node ID")="",$$setNodeExtraData(aGraph,lastNode,"node ID",lastNode) ; Note that this data is only stored on nodes we try in our path
    . ; Specific logic based on whether the node holds a string, or a pointer to a sub-graph template
    . s nodeDataType=$$nodeDataType(aGraph,lastNode)
    . i nodeDataType="subgraph" d  q  ; If lastNode is a subgraph pointer, we need to surgically paste a copy of that subgraph into our graph
    . . s newNode=$$surgicallyAddSugbraph(aGraph,lastNode,indexedGraphTemplates) ; dynamically build the ordered syntax graph as needed
    . . i pathLength=0 q  ; If our path was empty, we don't need to tinker with the (non-existant) last arrow in the path
    . . s pathLength=$$removeLastArrowFromPath("thisPath") ; get rid of the arrow that pointed at lastNode
    . . i pathLength=0 q  ; If we've manipulated the first node of the syntax graph, we'll restart our parse with an empty path
    . . s %=$$appendNodeToPath("thisPath",aGraph,newNode,parsePosition) ; notice that in the next iteration of the loop, newNode will end up as lastNode
    . i nodeDataType="string" d  q
    . . s expectedString=$$nodeData(aGraph,lastNode) ; Get information from the graph that we're trying to parse against.
    . . ; If the next characters in the input text don't match the data from lastNode, we try backtrack to an unused branch, and stop the parse if we can't.
    . . i $$readFromParsePosition(.parsePosition,expectedString,inputText)<0 s:$$backtrackPathToLastBranch("thisPath",aGraph)<1 STOP=1 q
    . . ; If we get here, then the next characters match the last node in the path, so we add the next node to the parse path
    . . i lastNode'=$$endNode(aGraph) s %=$$appendArrowToPath("thisPath",aGraph,$$firstOutgoingArrow(aGraph,lastNode),parsePosition)
    . . s thisPathStringLength=$$pathStringLength("thisPath",aGraph)
    . . i thisPathStringLength>$g(longestPathStringLength) d  ; Check if we've found a path that parses more of the input text
    . . . k longestPath
    . . . m longestPath=thisPath
    . . . s longestPathStringLength=thisPathStringLength
    . . i lastNode=$$endNode(aGraph) d  ; We've hit the end of the syntax graph, so we need to do some special checking
    . . . i $$parsePositionIsEnd(parsePosition,inputText) d  q  ; We've also run out of characters to parse, so we're done
    . . . . s STOP=1,SUCCESS=1
    . . . . k longestPath
    . . . . m longestPath=thisPath
    . . . s:$$backtrackPathToLastBranch("thisPath",aGraph)<1 STOP=1 ; We've run out of input text but we haven't hit the end of the graph, so we try to backtrack.
    . s $EC=",Uunknown node data type '"_nodeDataType_"'," ; This should never happen!
    q SUCCESS
    ;
    ;
    ; ====================================================================================
    ; ================================== TEST HARNESSES ==================================
    ;
    ; TODO: Write test cases to make sure that surgically adding subgraphs also updates the start and end node if necessary
    ;
tester()
    d testAB()
    d testABB()
    d testAAABB()
    d testABABA()
    q
    ;
    ;
testAB()
    n myIndexedGraphTemplates,myGraph,replacementNode,longestPath,stringToParse,longestPathLength,longestPathNodes,SUCCESS
    ;
    ; Test graph building
    w !,"Building syntax template for an AB map..."
    w $s($$buildSyntaxMapForAB("myIndexedGraphTemplates"):"Success!",1:"Failure!")
    w ! zw myIndexedGraphTemplates
    ;
    ; Test surgical graph additions
    m myGraph=myIndexedGraphTemplates("AB") ; Make a local copy of the graph
    w !,"A graph built from the syntax template for AB:"
    w ! zw myGraph
    w !,"Surgically modifying this graph..."
    s replacementNode=$$surgicallyAddSugbraph("myGraph",3,"myIndexedGraphTemplates") ; merge in another copy of the AB graph for the AB node
    w $s(replacementNode]"":"Success!",1:"Failure!")
    w ! zw myGraph
    ;
    ; Test path parsing
    s stringToParse="AAABBB"
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    q
    ;
    ;
testABB()
    n myIndexedGraphTemplates,myGraph,longestPath,stringToParse,longestPathLength,longestPathNodes,SUCCESS
    ;
    ; Test graph building
    w !,"Building syntax template for an ABB map..."
    w $s($$buildSyntaxMapForABB("myIndexedGraphTemplates"):"Success!",1:"Failure!")
    w ! zw myIndexedGraphTemplates
    ;
    m myGraph=myIndexedGraphTemplates("ABB") ; Make a local copy of the graph
    ;
    ; Test path parsing
    s stringToParse="ABB"
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    ; Test path parsing on an unparsably short string
    s stringToParse="AB"
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    ; Test path parsing on an unparsably long string
    s stringToParse="ABBBB"
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    ; Test path parsing on an unparsable gibberish string
    s stringToParse="CAB"
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    ; Test path parsing on an empty string
    s stringToParse=""
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    q
    ;
    ;
testAAABB()
    n myIndexedGraphTemplates,myGraph,longestPath,stringToParse,longestPathLength,longestPathNodes,SUCCESS
    ;
    ; Test graph building
    w !,"Building syntax template for an AAABB map..."
    w $s($$buildSyntaxMapForAAABB("myIndexedGraphTemplates"):"Success!",1:"Failure!")
    w ! zw myIndexedGraphTemplates
    ;
    m myGraph=myIndexedGraphTemplates("AAABB") ; Make a local copy of the graph
    ;
    ; Test path parsing
    s stringToParse="ABB"
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    ; Test path parsing on a short string
    s stringToParse="A"
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    ; Test path parsing on an a long string
    s stringToParse="ABBBB"
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    ; Test path parsing on an unparsable string
    s stringToParse="BBBB"
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    ; Test path parsing on an unparsable gibberish string
    s stringToParse="CAB"
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    ; Test path parsing on an empty string
    s stringToParse=""
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    q
    ;
    ;
testABABA()
    n myIndexedGraphTemplates,myGraph,replacementNode,longestPath,stringToParse,longestPathLength,longestPathNodes,SUCCESS
    ;
    ; Test graph building
    w !,"Building syntax template for an ABABA map..."
    w $s($$buildSyntaxMapForABABA("myIndexedGraphTemplates"):"Success!",1:"Failure!")
    w ! zw myIndexedGraphTemplates
    ;
     m myGraph=myIndexedGraphTemplates("ABABA") ; Make a local copy of the graph
    ;
    ; Test path parsing
    s stringToParse="ABA" ; the shortest possible string in this grammar
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    s stringToParse="ABABABA" ; a slightly longer string in this grammar
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    s stringToParse="AB" ; This won't parse
    d tryParseAndReport(stringToParse,"myGraph","myIndexedGraphTemplates")
    ;
    q
    ;
    ;
tryParseAndReport(stringToParse,aGraph,graphTemplates)
    n longestPath,SUCCESS,longestPathLength,longestPathNodes
    w !,"Using the graph to parse the string '"_stringToParse_"'."
    s SUCCESS=$$Traverse(aGraph,graphTemplates,stringToParse,.longestPath)
    w !,"Graph traverse ",$s(SUCCESS:"succeeded.",1:"failed.")
    w !,"The longest path generates this string: '",$$pathString("longestPath",aGraph),"'"
    w !,"The longest path contains "_$$pathNodes("longestPath",aGraph,.longestPathNodes)_" node(s):"
    w ! zw longestPathNodes
    s longestPathLength=$$pathStringLength("longestPath",aGraph)
    w !,"The best parse "_$s(SUCCESS:"parsed",1:"failed at")_" "_+longestPathLength_" characters."
    w ! zw longestPath
    w ! zw @aGraph
    q
    ;
    ;
    ; Builds the following graph template:
    ;              -----  ""  -----
    ;             /                \
    ; AB:  "A" --<                  >-- "B"
    ;             \                /
    ;               ----  AB  ----
    ; This will build strings like "AB", "AABB", "AAABBBB", etc.
buildSyntaxMapForAB(graphTemplates)
    n aTemplate,dataStack,aNode,templateName
    s templateName="AB"
    s aTemplate=$na(@graphTemplates@(templateName))
    ;
    s dataStack(1)="A"
    s aNode(1)=$$addNode(aTemplate,"string",.dataStack)
    s %=$$setStartNode(aTemplate,aNode(1))
    ;
    s dataStack(1)=""
    s aNode(2)=$$addNode(aTemplate,"string",.dataStack)
    ;
    s dataStack(1)="AB"
    s aNode(3)=$$addNode(aTemplate,"subgraph",.dataStack)
    ;
    s dataStack(1)="B"
    s aNode(4)=$$addNode(aTemplate,"string",.dataStack)
    s %=$$setEndNode(aTemplate,aNode(4))
    ;
    ; Add in the ordered collection of arrows
    s %=$$addArrow(aTemplate,aNode(1),aNode(2))
    s %=$$addArrow(aTemplate,aNode(2),aNode(4))
    s %=$$addArrow(aTemplate,aNode(1),aNode(3))
    s %=$$addArrow(aTemplate,aNode(3),aNode(4))
    ;
    q 1
    ;
    ;
    ; Builds the following graph template:
    ;           --- "A" ----
    ; ABB:     /             \
    ;    "" --<               >-- "BB"
    ;          \             /
    ;           --- "AB" ---
    ; This will build the strings "ABB" and "ABBB".
    ; Clearly, this is not the most effecient grammar to build these strings.
    ; However, it is a nice way to make sure that our parser isn't "too greedy",
    ; and doesn't get fooled into taking too big of a maximal munch when dealing
    ; with different options.
buildSyntaxMapForABB(graphTemplates)
    n aTemplate,dataStack,aNode,templateName
    s templateName="ABB"
    s aTemplate=$na(@graphTemplates@(templateName))
    ;
    s dataStack(1)=""
    s aNode(1)=$$addNode(aTemplate,"string",.dataStack)
    s %=$$setStartNode(aTemplate,aNode(1))
    ;
    s dataStack(1)="A"
    s aNode(2)=$$addNode(aTemplate,"string",.dataStack)
    ;
    s dataStack(1)="AB"
    s aNode(3)=$$addNode(aTemplate,"string",.dataStack)
    ;
    s dataStack(1)="BB"
    s aNode(4)=$$addNode(aTemplate,"string",.dataStack)
    s %=$$setEndNode(aTemplate,aNode(4))
    ;
    ; Add in the ordered collection of arrows
    s %=$$addArrow(aTemplate,aNode(1),aNode(2))
    s %=$$addArrow(aTemplate,aNode(2),aNode(4))
    s %=$$addArrow(aTemplate,aNode(1),aNode(3))
    s %=$$addArrow(aTemplate,aNode(3),aNode(4))
    ;
    q 1
    ;
    ;
    ; Builds the following graph templates:
    ;  AAABB:
    ;    START ---- END
    ;            
    ;              ---- "" ----
    ;  START:    /              \ 
    ;    "A" ---<                >-- ""
    ;            \              / 
    ;              --- START -- 
    ;
    ;             --- "" ---
    ;  END:     /            \
    ;    "B" --<              >-- ""
    ;           \            /
    ;             --- END --
    ; This builds strings like "AAAA" or "AABBBB"
buildSyntaxMapForAAABB(graphTemplates)
    n aTemplate,dataStack,aNode,templateName
    ;
    ; Build AAABB graph template
    s templateName="AAABB"
    s aTemplate=$na(@graphTemplates@(templateName))
    ;
    s dataStack(1)="START"
    s aNode(1)=$$addNode(aTemplate,"subgraph",.dataStack)
    s %=$$setStartNode(aTemplate,aNode(1))
    ;
    s dataStack(1)="END"
    s aNode(2)=$$addNode(aTemplate,"subgraph",.dataStack)
    s %=$$setEndNode(aTemplate,aNode(2))
    ;
    s %=$$addArrow(aTemplate,aNode(1),aNode(2))
    ;
    ; Build START graph template
    s templateName="START"
    s aTemplate=$na(@graphTemplates@(templateName))
    ;
    k aNode
    s dataStack(1)="A"
    s aNode(1)=$$addNode(aTemplate,"string",.dataStack)
    s %=$$setStartNode(aTemplate,aNode(1))
    ;
    s dataStack(1)=""
    s aNode(2)=$$addNode(aTemplate,"string",.dataStack)
    ;
    s dataStack(1)="START"
    s aNode(3)=$$addNode(aTemplate,"subgraph",.dataStack)
    ;
    s dataStack(1)=""
    s aNode(4)=$$addNode(aTemplate,"string",.dataStack)
    s %=$$setEndNode(aTemplate,aNode(4))
    ;
    s %=$$addArrow(aTemplate,aNode(1),aNode(2))
    s %=$$addArrow(aTemplate,aNode(1),aNode(3))
    s %=$$addArrow(aTemplate,aNode(2),aNode(4))
    s %=$$addArrow(aTemplate,aNode(3),aNode(4))
    ;
    ; Build END grapn template
    s templateName="END"
    s aTemplate=$na(@graphTemplates@(templateName))
    ;
    k aNode
    s dataStack(1)="B"
    s aNode(1)=$$addNode(aTemplate,"string",.dataStack)
    s %=$$setStartNode(aTemplate,aNode(1))
    ;
    s dataStack(1)=""
    s aNode(2)=$$addNode(aTemplate,"string",.dataStack)
    ;
    s dataStack(1)="END"
    s aNode(3)=$$addNode(aTemplate,"subgraph",.dataStack)
    ;
    s dataStack(1)=""
    s aNode(4)=$$addNode(aTemplate,"string",.dataStack)
    s %=$$setEndNode(aTemplate,aNode(4))
    ;
    s %=$$addArrow(aTemplate,aNode(1),aNode(2))
    s %=$$addArrow(aTemplate,aNode(1),aNode(3))
    s %=$$addArrow(aTemplate,aNode(2),aNode(4))
    s %=$$addArrow(aTemplate,aNode(3),aNode(4))
    ;
    q 1
    ;
    ;
    ; Builds the following graph templates:
    ;            ------
    ;           /      \
    ;  ABABA:  |        |
    ;           \      /
    ;   "" --- "AB" --<
    ;                  \
    ;                   --- "A"
    ; Note that these graphs contain a cycle.
    ; It generates strings like "ABA" and "ABABABABA"
buildSyntaxMapForABABA(graphTemplates)
    n aTemplate,dataStack,aNode,templateName
    s templateName="ABABA"
    s aTemplate=$na(@graphTemplates@(templateName))
    ;
    s dataStack(1)=""
    s aNode(1)=$$addNode(aTemplate,"string",.dataStack)
    s %=$$setStartNode(aTemplate,aNode(1))
    ;
    s dataStack(1)="AB"
    s aNode(2)=$$addNode(aTemplate,"string",.dataStack)
    ;
    s dataStack(1)="A"
    s aNode(3)=$$addNode(aTemplate,"string",.dataStack)
    s %=$$setEndNode(aTemplate,aNode(3))
    ;
    ; Add in the ordered collection of arrows
    s %=$$addArrow(aTemplate,aNode(1),aNode(2))
    s %=$$addArrow(aTemplate,aNode(2),aNode(2))
    s %=$$addArrow(aTemplate,aNode(2),aNode(3))
    ;
    q 1