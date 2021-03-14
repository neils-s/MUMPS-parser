ROUTINE PARSER6
PARSER6
    ;
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
    ; === OVERVIEW ===
    ; Given a language grammer that can is expressed using a syntax diagram consisting only of:
    ;   1) string literals
    ;   2) collections of finitely many options
    ;   3) delimited lists
    ;   4) named tokens
    ; This routine translates this grammer into a grammer expressed as a ordered path graph.
    ;
    ; In other words, this routine translates the kind of grammar data used by PARSER1 into
    ; the kind of grammar data used by PARSER5.
    ;
    ;
    ; === EXAMPLES ===
    ; The main body of this code simply translates a grammar map to an ordered path graph.
    ; As an example, consider this grammar:
    ;   @map@("token1")="literal"
    ;   @map@("token1","value")="TOKEN_ONE"
    ;
    ; This should create a collction of graph templates like this:
    ;   @template@("token1","nodes",1)="string"
    ;   @template@("token1","nodes",1,"typeStack",1)="TOKEN_ONE"
    ;
    ; A grammar like this:
    ;   @map@("token2")="subtreeChain"
    ;   @map@("token2",1)="literal"
    ;   @map@("token2",1,"value")="[["
    ;   @map@("token2",2)="delimList"
    ;   @map@("token2",2,"delimiter")="literal"
    ;   @map@("token2",2,"delimiter","value")="::"
    ;   @map@("token2",2,"content")="token"
    ;   @map@("token2",2,"content","value") = "token1"
    ;   @map@("token2",3)="literal"
    ;   @map@("token2",3,"value")="]]"
    ;
    ; Should give rise to a graph template like this;
    ;   @template@("token2","nodes",1)="string"
    ;   @template@("token2","nodes",1,"typeStack",1)="[["
    ;   @template@("token2","nodes",2)="subgraph"
    ;   @template@("token2","nodes",2,"typeStack",1)="token1"
    ;   @template@("token2","nodes",3)="string"
    ;   @template@("token2","nodes",3,"typeStack",1)="::"
    ;   @template@("token2","nodes",4)="string"
    ;   @template@("token2","nodes",4,"typeStack",1)="]]"
    ;   @template@("token2","arrows",1,"source")=1
    ;   @template@("token2","arrows",1,"target")=2
    ;   @template@("token2","arrows",2,"source")=2
    ;   @template@("token2","arrows",2,"target")=4
    ;   @template@("token2","arrows",3,"source")=2
    ;   @template@("token2","arrows",3,"target")=3
    ;   @template@("token2","arrows",4,"source")=3
    ;   @template@("token2","arrows",4,"target")=2
    ;
    ; A grammar like this:
    ;   @map@("token3")="options"
    ;   @map@("token3",1)="" ; the empty option
    ;   @map@("token3",2)="subtreeChain"
    ;   @map@("token3",2,1)="literal"
    ;   @map@("token3",2,1,"value")="("
    ;   @map@("token3",2,2)="token"
    ;   @map@("token3",2,2,"value")="token3"
    ;   @map@("token3",2,3)="literal"
    ;   @map@("token3",2,3,"value")=")"
    ;   @map@("token3",3)="token"
    ;   @map@("token3",3,"value")=token1
    ;
    ; Should give rise to a graph like this
    ;   @template@("token3","nodes",1)="string"
    ;   @template@("token3","nodes",1,"typeStack",1)="" ; We need to add an empty string to serve as a unique start node
    ;   @template@("token3","nodes",2)="string"
    ;   @template@("token3","nodes",2,"typeStack",1)="" ; the empty option
    ;   @template@("token3","nodes",3)="string"
    ;   @template@("token3","nodes",3,"typeStack",1)="("
    ;   @template@("token3","nodes",4)="subgraph"
    ;   @template@("token3","nodes",4,"typeStack",1)="token3"
    ;   @template@("token3","nodes",5)="string"
    ;   @template@("token3","nodes",5,"typeStack",1)=")"
    ;   @template@("token3","nodes",6)="subgraph"
    ;   @template@("token3","nodes",6,"typeStack",1)="token1"
    ;   @template@("token3","nodes",7)="string"
    ;   @template@("token3","nodes",7,"typeStack",1)="" ; We need to add an empty string to serve as a unique end node
    ;   @template@("token2","arrows",1,"source")=1
    ;   @template@("token2","arrows",1,"target")=2
    ;   @template@("token2","arrows",2,"source")=1
    ;   @template@("token2","arrows",2,"target")=3
    ;   @template@("token2","arrows",3,"source")=1
    ;   @template@("token2","arrows",3,"target")=6
    ;   @template@("token2","arrows",4,"source")=3
    ;   @template@("token2","arrows",4,"target")=4
    ;   @template@("token2","arrows",4,"source")=4
    ;   @template@("token2","arrows",4,"target")=5
    ;   @template@("token2","arrows",4,"source")=2
    ;   @template@("token2","arrows",4,"target")=7
    ;   @template@("token2","arrows",4,"source")=5
    ;   @template@("token2","arrows",4,"target")=7
    ;   @template@("token2","arrows",4,"source")=6
    ;   @template@("token2","arrows",4,"target")=7
    ;
    ;
    ; === TECHNICAL APPROACH ===
    ; The grammar is built on a recursive model, so it's natural to parse it in a recursive fashion.
    ; 
    ; The algorithm to convert a grammar map into a graph template is as follows:
    ;   Determine the name of the current token.
    ;       This is the first subscript of the grammar array,
    ;       It should also be used as the first subscript of the graph template
    ;   Recursion step:
    ;       Determine the grammar element type (options, delimited list, etc.)
    ;       Do specific logic based on element type, returning a subgraph
    ;       Somehow surgically paste in the returned subgraph (???)
    ; 
    ;   Convert "":
    ;       Create a node with type of string holding empty string
    ;       Create a graph that holds this single node
    ;
    ;   Convert "literal":
    ;       Create a node with a type of string holding the given literal
    ;       Create a graph that holds this single node
    ;
    ;   Convert "subTreeChain":
    ;       Create an empty graph
    ;       $ORDER throught the grammar map array
    ;           Call the Recursion Step, and get back the returned graph
    ;           Append the returned graph to the existing graph:
    ;               If the existing graph is empty, replace it with the returned graph.
    ;               Else
    ;                   Add the returned graph to the existing graph
    ;                   Add an arrow from the endNode of the old grpah to the startNode of the returned graph
    ;                   Update the start and end nodes of the graph appropriately
    ;       Return this graph
    ;
    ;   Convert "options":
    ;       Create an empty graph
    ;       Add an empty string start node and an empty string end node
    ;       $ORDER throuh the grammar map array
    ;           Call the recursion step and get back a returned graph
    ;           Append the returned graph to the exsiting graph
    ;           Add an arrow from the start node of the existing graph to the start node of the returned graph
    ;           Add an arrow from the end node of the existing graph to the end node of the returned graph
    ;       Return this graph
    ;
    ;   Convert "delimList":
    ;       Create an empty graph
    ;       Add empty string start and end nodes
    ;       Get the "content" element of the grammar map
    ;       Call the recursion step to get a returned graph for the "content" sub-element of the grammar's delimList element
    ;       Add this "content" returned graph to the empty graph
    ;       Add an arrow from the empty start node to the start node of the returned "content" graph
    ;       Add an arrow that connects the end node of the "content" graph to the empty string end node
    ;       Call the recursion step to get a returned graph for the "delimiter" sub-element of the grammar's delemList element
    ;       Add this "delimiter" returned graph to the previously empty graph that we created.
    ;       Add an arrow from the end node of the "content" graph to the start node of the "delimiter" graph
    ;       Add an arrow from the end node of the "delimiter" graph to the start node of the "content" graph
    ;
    ;   Convert "token":
    ;       Create a node with a type of subgraph holding the given token name
    ;       Create a graph that holds this single node
    ;
    ;
    ; === CAUTIONARY NOTE ===
    ; The algorithm above creates a lot of empty string graph nodes to make sure that various
    ; intermediate graphs have unique, well-defined start and end nodes.
    ; This takes up a little bit of extra memory, but it shouldn't cause any problems.
    ; If this gets too annoying, it should be possible to create a graph trimmer that goes through
    ; and just removes any redundant empty string nodes from the graph.  For now we'll just deal
    ; with the extra redundant nodes.
    ;
    ;
    ; =====================================================================================
    ; ===================================== CORE CODE =====================================
    ;