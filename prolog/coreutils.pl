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

:- module(coreutils, [printl/1, 
		      println/1,
		      cleanLibraryName/2,
		      cleanPredicateName/2,
		      diff/3,
		      %safe_assert/1,
		      safe_retract/1,
		      conj_to_list/2,
		      file_to_list/2,
		      add/2,
		      size/2,
			  print_exception/1,
		      recognizeOS/1]).

:- ensure_loaded(library(lists)).

print_exception(X).

% now declaring some useful stuff
% print members of a list.
printl(L) :- 
	size(L,S),
	S > 0,
	forall(
	  member(X,L),
	  (write(X),
	    write(' '))
	).

printl(L) :- not(is_list(L)), write(L), write(' ').

printl(L) :- 
	write('[]').

println(L) :- 	size(L,S),
	S > 0,
	forall(
	  member(X,L),
	  (write(X),nl)
	).
	
println(L) :- nl.

% returns the cleaned name of a library. for example cleanLibraryName(L,N) unifies L = library(lists) with N = lists.

cleanLibraryName(Term, Library) :- 
	functor(Term,_,Arity),
	Arity > 0,
	arg(1, Term, Library),
	!.
	
cleanLibraryName(Term, Term). 

% returns the clean name of a Predicate.
cleanPredicateName(Term, Predicate) :-
	Term = (Module:Pname),
	functor(Pname,Phead, Arity),
	Predicate = (Module:Phead/Arity),
	!.
	
cleanPredicateName(Term, Predicate) :-
	functor(Term,Phead,Arity),
	Predicate = (Phead/Arity),
	!.

% unifies D with the diff of list Xs with Ys.
diff(Xs,Ys,D) :- findall(
  X,
  (member(X,Xs),not(member(X,Ys))),
  D).

%doesn't fail if fact is not asserted
safe_retract(Fact) :- retract(Fact), !.
safe_retract(_).

%converts a conjunction of terms to a list.
conj_to_list((A,B),[A|ListB]):- !, nonvar(A), conj_to_list(B,ListB).
conj_to_list((A;B),[A|ListB]):- !, conj_to_list(B,ListB).
conj_to_list(A,[A]).

file_to_list(File, List) :- catch(file2list(File, List), X, (print_exception(X),true)).

% Load a file of prolog terms into a List.
file2list(FILE,LIST) :- 
   see(FILE), 
   inquire([],R), % gather terms from file
   reverse(R,LIST),
   seen.

inquire(IN,OUT):-
   read(Data), 
   (Data == end_of_file ->   % done
      OUT = IN 
        ;    % more
      inquire([Data|IN],OUT) ) . 
      
% add element +X to the list +L returning -L1
add(X,[],[X]).

add(X,[A|L],[A|L1]):-
	add(X,L,L1).


% returns lenght -N of a +Conjunction
conj_size(Conjunction, N) :-
	conj_to_list(Conjunction, List),
	size(List, N).

% counts element of a list.
size([],0).

size([_|T],N):-
    size(T,M),
    N is M+1.

% returns running operating system
recognizeOS(Current) :- all(O,(X = true, OS = [unix, windows], member(O, OS), yap_flag(O,X)), Current).


