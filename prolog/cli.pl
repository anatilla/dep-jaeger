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
:- module(cli, [welcome/0,
	     help/0,
	     quit/0]).

help :- nl,
                write('available commands: '), nl,
                write('  load_all([f1, f2, ...]).                        - Loads a sequence of files for analysis'), nl,
                write('  modules.                                        - Print a list of all modules currently imported in the system'), nl,
                write('  predicates.                                     - Print a list of all predicates currently imported in the system'), nl,
                write('  predicates(Module)                              - Print a list of all predicates contained in Module currently imported in the system'), nl,
                %write('  exists(Predicate,Module).                       - Tells if a predicate is declared in a given module'), nl,
                write('  queryP(Predicate).                              - Prints some information on Predicate. Enter ONLY the predicate firm (e.g. pred/N)'), nl,
                write('  queryM(Module).                                 - Prints some info on Module'),nl,
                %write('  getFileFromModule(Module, File).                - Unifies File with the associated Module.'),nl,
                %write('  getModuleFromFile(File,Module)                  - Unifies Module with associated File.'),nl,
                write('  module2png(Module, Filename).                   - draws a dgraph representing Module in Filename'),nl,
                write('  moduleInteraction2png(Module, Filename). 	 - draws a graphs representing imports of +Module in +Filename'), nl,
                write('  quit.                                           - Need to explain? :)'), nl.

quit :- halt.


%:- welcome,nl,write('type \'help\' for info.'),nl.