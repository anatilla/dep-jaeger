:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(coreutils).

% PARSING DELLE CLAUSOLE DI HORN.

%a partire dalle regole temporanee, crea le regole finali
create_final_rules(File, Module) :-
	all(_,
	     (all(L, tmp(L,_), LX),
	      sort(LX, LX1),
	      member(L1, LX1),
	      all(R, tmp(L1,R), RX),
	      flatten(RX, RXs),
	      assert(rule(L1, RXs, File, Module))
	     ),
	   _),
	 abolish(tmp/2).

%crea le regole temporanee
create_temp_rules(Module, File, Graph) :-
	all(_,
	      (member(Element, Graph),
	       member(H, Element),
	       H = Left-Right,
	       assert(tmp(Left, Right))
	      ),
	   _).


	
getCodeGraph(Terms, Graph) :-
   all(Elements, 
		(
		setof(Predicate-PredicateBody,
			(member(Line, Terms),
			 Line = (LeftPart :- _),
			 coreutils:cleanPredicateName(LeftPart,Predicate),
			 getPredicateBody(Terms, LeftPart, PredicateBody)),
		Elements)),
	Graph).

getPredicateBody(Terms, LeftPart, PredicateBody) :-
	%recupera tutte le parti destre riferite alla parte sinistra data in input
	all(Body, 
		(member(Line, Terms),
		Line = (LeftPart :- RightPart),
		coreutils:conj_to_list(RightPart, Body)), 
	Bodies),
	flatten(Bodies, FlattenBodies),
	% normalizza le parti destre nel formato predicato/arita'
	getPredicatesWithArity(FlattenBodies, PredicateBody).

	
% Returns a list of predicates -NmAr in the format Nm/Ar from list of names +Names.
getPredicatesWithArity(Names,NmAr) :- all(
  (A),
  (member(L, Names), coreutils:cleanPredicateName(L,A)),
  NmAr).


%unifies +Terms (representing content of a file) with -ImportedModules. Tells us what libraries the file imports.
imports(Terms, ImportedModules) :-
        getExplicitImports(Terms, ExplicitImports),
        getInlineImports(Terms, InlineImports),
        lists:append(ExplicitImports, InlineImports, List1),
        lists:remove_duplicates(List1, List2),
        sort(List2, ImportedModules), !.
        %all(A, (member(L,Modules), coreutils:cleanLibraryName(L,A)), ImportedModules), !.
         
imports(Terms, []).

% Unifies a +Content string if it unifies with a Module declaration and Returns all in +Modules
% cerca inzialmente gli import espliciti
getExplicitImports(Terms,Modules) :- all(
          ModuleName,
          (member(Line, Terms), recognizeImport(Line,ModuleName)),
          Modules
         ), !.

getExplicitImports(Terms, []).
         
% cerca gli import in-line (all'interno di parti destre di predicati)       
getInlineImports(Terms,Modules) :- all(
	Modname,
	(member(Line, Terms),
	 Line = (_ :- RightPart),
	 coreutils:conj_to_list(RightPart, RPList),
	 member(Element, RPList),
	 recognizeImport(Element,Modname),
	 ground(Modname)),
	Modules), !.       

getInlineImports(Terms, []).

/*
 * Recognizes import type (explicit and inline). Holds if +Line contains an expression like:
 * :- use_module(library(_)).
 * :- use_module('path/file.pl').
 * :- use_module(library(_), [ExportedPredicates]).
 * :- use_module('path/file.pl', [ExportedPredicates]).
 * :- consult('path/file.pl').
 * :- consult(library(Module)).
 * :- consult(Filename).
 * :- load_files('path/file.pl').
 * :- load_fles(library(Module)).
 * :- load_files(Filename).
 * :- ensure_loaded(library(Module)).
 * :- ensure_loaded(Filename).
 * :- ensure_loaded('path/file.pl').
 * and returns -ModuleName.
 
 */
recognizeImport(Line, ModuleName) :-
	Line = (use_module([ModuleName])),
	!.

recognizeImport(Line, ModuleName) :-
	Line = (:-use_module([ModuleName])),
	!.
	
recognizeImport(Line, ModuleName) :-
	Line = (use_module(ModuleName)),
	!.

recognizeImport(Line, ModuleName) :-
	Line = (use_module(ModuleName,_)),
	!. 

recognizeImport(Line, ModuleName) :-
	Line = (_ -> use_module(ModuleName)),
	!.

recognizeImport(Line, ModuleName) :-
	Line = (_ -> use_module(ModuleName,_)),
	!. 
	
recognizeImport(Line, ModuleName) :-
	Line = (_ -> use_module(ModuleName) ; _),
	!.
	
recognizeImport(Line, ModuleName) :-
	Line = (_ -> use_module(ModuleName,_) ; _),
	!.
 
recognizeImport(Line,ModuleName) :- 
	Line = (:-use_module(Something)),
	Something = (library(Module)),
	getAbsolutePath(Module, ModulePath),
	getModuleInfo(ModulePath, ModuleName, Predicates),
	!.
	
recognizeImport(Line,ModuleName) :- 
	Line = (:-use_module(Something,_)),
	Something = (library(Module)),
	getAbsolutePath(Module, ModulePath),
	getModuleInfo(ModulePath, ModuleName, Predicates),
	!.
	
recognizeImport(Line,ModuleName) :- 
	Line = (:-use_module(ModuleName)),
	!.
	
recognizeImport(Line,ModuleName) :- 
	Line = (:-use_module(ModuleName,_)),
	!.

%% consult
recognizeImport(Line, ModuleName) :-
	Line = ([ModuleName]),
	!.

recognizeImport(Line, ModuleName) :-
	Line = (consult(ModuleName)),
	!.

recognizeImport(Line, ModuleName) :-
	Line = (consult(ModuleName,_)),
	!. 

recognizeImport(Line, ModuleName) :-
	Line = (_ -> consult(ModuleName)),
	!.

recognizeImport(Line, ModuleName) :-
	Line = (_ -> consult(ModuleName,_)),
	!. 
	
recognizeImport(Line, ModuleName) :-
	Line = (_ -> consult(ModuleName) ; _),
	!.
	
recognizeImport(Line, ModuleName) :-
	Line = (_ -> consult(ModuleName,_) ; _),
	!.
	
recognizeImport(Line,ModuleName) :- 
	Line = (:-consult(Something)),
	Something = (library(Module)),
	getAbsolutePath(Module, ModulePath),
	getModuleInfo(ModulePath, ModuleName, Predicates),
	!.
	
recognizeImport(Line,ModuleName) :- 
	Line = (:-consult(Filename)),
	checkIsModule(FileName),
	getModuleInfo(Filename,
	ModuleName, Predicates),
	!.
	
recognizeImport(Line,ModuleName) :- 
	Line = (:-consult(Filename)),
	ModuleName = Filename,
	!.
%% load_files

recognizeImport(Line, ModuleName) :-
	Line = (load_files(ModuleName)),
	!.

recognizeImport(Line, ModuleName) :-
	Line = (load_files(ModuleName,_)),
	!. 

recognizeImport(Line, ModuleName) :-
	Line = (_ -> load_files(ModuleName)),
	!.

recognizeImport(Line, ModuleName) :-
	Line = (_ -> load_files(ModuleName,_)),
	!. 
	
recognizeImport(Line, ModuleName) :-
	Line = (_ -> load_files(ModuleName) ; _),
	!.
	
recognizeImport(Line, ModuleName) :-
	Line = (_ -> load_files(ModuleName,_) ; _),
	!.

recognizeImport(Line,ModuleName) :- 
	Line = (:-load_files(Something,_)),
	Something = (library(Module)),
	getAbsolutePath(Module, ModulePath),
	getModuleInfo(ModulePath, ModuleName, Predicates),
	!.
	
recognizeImport(Line,ModuleName) :- 
	Line = (:-load_files(Filename,_)),
	checkIsModule(Filename),
	getModuleInfo(Filename,ModuleName, Predicates),
	!.
	
recognizeImport(Line,ModuleName) :- 
	Line = (:-load_files(Modulename,_)),
	ModuleName = Filename,
	!.
	
%% ensure_loaded
recognizeImport(Line, ModuleName) :-
	Line = (ensure_loaded([ModuleName])),
	!.
	
recognizeImport(Line, ModuleName) :-
	Line = (:-ensure_loaded([ModuleName])),
	!.

recognizeImport(Line, ModuleName) :-
	Line = (ensure_loaded(ModuleName)),
	!.

recognizeImport(Line, ModuleName) :-
	Line = (ensure_loaded(ModuleName,_)),
	!. 

recognizeImport(Line, ModuleName) :-
	Line = (_ -> ensure_loaded(ModuleName)),
	!.

recognizeImport(Line, ModuleName) :-
	Line = (_ -> ensure_loaded(ModuleName,_)),
	!. 
	
recognizeImport(Line, ModuleName) :-
	Line = (_ -> ensure_loaded(ModuleName) ; _),
	!.
	
recognizeImport(Line, ModuleName) :-
	Line = (_ -> ensure_loaded(ModuleName,_) ; _),
	!.
	
recognizeImport(Line,ModuleName) :- 
	Line = (:-ensure_loaded(Something)),
	Something = (library(Module)),
	getAbsolutePath(Module, ModulePath),
	getModuleInfo(ModulePath, ModuleName, Predicates),
	!.
	
recognizeImport(Line,ModuleName) :- 
	Line = (:-ensure_loaded(Filename)),
	checkIsModule(FileName),
	getModuleInfo(FileName,
	ModuleName, Predicates),
	!.
	
recognizeImport(Line,ModuleName) :- 
	Line = (:-ensure_loaded(Filename)),
	ModuleName = Filename,
	!.
	
