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
    ; === TECHNICAL APPROACH ===
    ; 