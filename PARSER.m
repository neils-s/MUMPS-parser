PARSER
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
    ; redirect to main
    d main() q
    ;
    ; This routine contains driver tags for the code in the other PARSER* routines.
    ;
    ; This is the main driver of the parsing code.  It's the entrypoint for the parsing.
    ; The parameters are as follows:
    ;       outParseTreePointer - a string pointer to the array in which the parse data will be held.
    ;       mapPointer - a string pointer to the array holding the map for parsing the tokens.
    ;               (The map defined by @mapPointer must not have any instances of left-recursion!)
    ;       codePointer - a string pointer to the array holding the code to be parsed.
    ;       startColumn - the column of @codePointer to start the parse at.  This defaults to "1".
    ;       startParseAs - the specific token that should describe the contents of the entire stream.
    ;                      If this isn't set, then every token in @map will be tried, and the token returning the longest parse will be returned.
    ; Returns the number of characters parsed
main(outParseTreePointer,mapPointer,codePointer,startColumn,startParseAs)
    ; TODO:  Finish this   
    q