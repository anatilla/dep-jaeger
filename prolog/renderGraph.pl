/* 
Copyright (C) 2013  Nicola Alessandro Natilla, Damiano Romita

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>

*/

% Authors:
% --------
%
% Nicola Alessandro Natilla (alessandro.natilla@gmail.com)
% Damiano Romita (dromita@gmail.com)

:- module(renderGraph, [toDot/2, draw/2]).
:- use_module(library(system)).

/**
  toDot(+List, -Output)
 
  Given the set of edges, generate the dot syntax which defines the connections between the nodes in the graph.
*/
toDot([], Output).
toDot([Shape|Edges], Output) :-
    functor(Shape, '-', 2),
    arg(1, Shape, X),
    arg(2, Shape, Y),
    write(Output, '"'),
    write(Output, X),
    write(Output, '"'),
    write(Output, ' -> '),
    write(Output, '"'),
    write(Output, Y),
    write(Output, '"'),
    write(Output, '\n'),
    toDot(Edges, Output).

/**
  draw(+Edges, +Filename)

  Given a list representing +Edges of a dgraph, draw a PNG image on +Filename. Requires 'dot' (http://www.graphviz.org)
*/
draw(Edges,Filename) :-
    tmpnam(File),
    open(File, write, Output),
    write(Output, 'digraph G {'),
    write(Output, '\n'),
    toDot(Edges, Output),
    write(Output, '}'),
    close(Output),
    atom_concat('dot -Tpng ', File, Cmd1),
    atom_concat(Cmd1, ' -o ', Cmd2),
    atom_concat(Cmd2, Filename, Cmd),
    shell(Cmd),
    write('Drawn dependency graph in file '), write(Filename), write('.'),nl.
