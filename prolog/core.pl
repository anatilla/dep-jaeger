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

:- ensure_loaded(library(lists)).
:- ensure_loaded(library(readutil)).
:- ensure_loaded(library(system)).
:- ensure_loaded(library(aggregate)).
:- ensure_loaded([coreutils]).
:- ensure_loaded([cli]).
:- ensure_loaded([renderGraph]).
:- ensure_loaded(['yap-json/json.pl']).

% =====================================================================
% HIGH-LEVEL PREDICATES
% =====================================================================  
  
% Unifies -T with a list of all modules loaded in the system.
modules(T) :- all(M, rule(_,_,_,M), MX), sort(MX,T).

% Prints a list of all modules loaded in the system.
modules :- all(M, (rule(_,_,_,M); module_imports(M,_)), MX), sort(MX, MXS), coreutils:printl(MXS), nl, !.
modules :- write('nothing imported.'),nl.

% Unifies -Output with a list of all predicates loaded in the system referred to +Module.
predicates(Module, Output) :- all(P, rule(P,_,_,Module), PX), !, sort(PX, Output), !.
predicates(Module, []).

% Unifies T with a list of all predicates loaded in the system.
predicates(Output) :- all(P, rule(P,_,_,_), TX), sort(TX,Output), !.
predicates(Output) :- all(P, rule(P,_,_,_), TX), sort(TX,Output).

% Prints a list of all predicates loaded in the system if present. Writes a message otherwise
predicates :- all((M:P), rule(P,_,_,M), PX), sort(PX,PXS), coreutils:println(PXS).
predicates :- write('nothing imported.'),nl.

% Holds if exists +Predicate, fails otherwise.
find_predicate(Predicate) :- all(Predicate, rule(Predicate,_,_,_), PX), size(PX,N), N > 0.

% Holds if exists +Module, fails otherwise.
find_module(Module) :- all(Module, rule(_,_,_,Module), MX), size(MX, N), N > 0, !.
find_module(Module) :- module_imports(Module, _), !.
%find_module(Module) :- module_exports(Module, _), !.

% prints information on +Predicate
queryP(Predicate) :-  
	unique(Predicate),
	infoPred(Predicate).
	
queryP(Predicate) :- 
	\+ unique(Predicate),
	write('ERROR: predicate '),
	write(Predicate),
	write(' is declared in modules: '),
	all(M, declared_in(Predicate, M,_), Modules),
	printl(Modules),
	writeln('. Please use queryP(_predicateName, _module) !').
	
queryP(Predicate, Module) :- 
	infoPred(Predicate, Module).

% prints info on +Predicate
infoPred(Predicate) :-          
        write('Predicate declared in module _ '), declared_in(Predicate, Module, _), module_absolute_file_path(Module, FDecl), write(Module), write(' _ in file: '), write(FDecl),nl,
        write('Predicate is public: '), is_public(Predicate, Module, Response), write(Response), nl,
        write('Predicate uses: '), uses(Predicate, Module, Uses), coreutils:printl(Uses),nl,
        write('Predicate is used by: '), used_by(Predicate, Module, Used), coreutils:printl(Used),nl,
        %write('Predicate depends from files: '), depends(Predicate, Module, ModuleFiles), coreutils:printl(ModuleFiles),nl,
        write('Predicate uses modules: '), predicate_module_dependency(Predicate, Module, Modules), coreutils:printl(Modules),nl.

% prints info on +Predicate declared in +Module
infoPred(Predicate, Module) :- 
	 write('Predicate declared in module _ '), declared_in(Predicate,_,FDecl), write(Module), write(' _ in file: '), write(FDecl),nl,
	 write('Predicate uses: '), uses(Predicate, Module, Uses), coreutils:printl(Uses),nl,
	 write('Predicate is used by: '), used_by(Predicate, Module, Used), coreutils:printl(Used),nl,
	 %write('Predicate depends from files: '), depends(Predicate, Module, ModuleFiles), coreutils:printl(ModuleFiles),nl,
	 write('Predicate uses modules: '), predicate_module_dependency(Predicate, Module, Modules), coreutils:printl(Modules),nl.

% prints information on +Module
queryM(Module) :- not(find_module(Module)), write('Module '), write(Module), write(' does not exists'),nl.
queryM(Module) :-       
	write('Module '), write(Module), write(' file path: '), module_absolute_file_path(Module, FPath), write(FPath), write('.'), nl,
        write('Module '), write(Module), write(' dependency taxonomy: '), effective_imported_module_deps(Module,Taxonomy), coreutils:printl(Taxonomy),write('.'),nl,
        write('Module '), write(Module), write(' used imports: '), used_imports(Module, UsedModules), coreutils:printl(UsedModules),write('.'),nl,
        write('Module '), write(Module), write(' un-used imports: '), coreutils:diff(Taxonomy, UsedModules, DDRX) , coreutils:printl(DDRX),write('.'),nl,
        write('Module '), write(Module), write(' uses file(s): '), nl, forall(member(P, UsedModules), (external_module(F, P,_), write('   '), write(P), write(' in '), write(F),nl)),
        write('Analysis also found '), clash_search(Module, Clashes), size(Clashes, NumClashes), write(NumClashes), write(' suspicious name clash(es): '),  coreutils:printl(Clashes), nl.

% flushes working memory
flush :-
	abolish(module_absolute_file_path/2),
	abolish(module_path/2),
	abolish(exports/2),
	abolish(now_analyzing/1),
	abolish(module_imports/2),
	abolish(rule/4),
	abolish(external_module/3).

% =====================================================================        
% MEDIUM-LEVEL PREDICATES, (contains logic)
% =====================================================================

% ritorna una lista nel formato predicate/arity-[mod1, mod2, ...] 
% che rappresenta i predicati associati ad una lista di moduli nei quali sono stati individuate dichiarazioni uguali.
% e quindi probabili fonti di name clashes.
% la ricerca dei clash viene fatta fra i predicati esportati dal modulo da analizzare e quelli esportati da moduli importati dall'analizzato.
% cio ha senso perche' l'analisi deve essere ristretta a ciascun modulo.
clash_search(Module, Clashes) :-
	all(P-RealModules, 
		(
		predicates(Module, Predicates),
		 member(Predicate, Predicates),
		 uses(Predicate, Module, U),
		 member(P, U),
		 setof(Mod, belongs_to(P, Mod), ClashModules),
		 module_imports(Module, Dependencies),
		 append(Dependencies, [Module], RDeps),
		 intersection(RDeps, ClashModules, RealModules),
		 size(RealModules, N),
		 N > 1),
	Clashes), !.
	
clash_search(Module, []).

real_clash_search(Module, Clashes) :-
	all(P-RealModules, 
		(
		predicates(Module, Predicates),
		 member(Predicate, Predicates),
		 uses(Predicate, Module, U),
		 member(P, U),
		 setof(Mod, (belongs_to(P, Mod), is_public(P, Mod, yes)), ClashModules),
		 module_imports(Module, Dependencies),
		 append(Dependencies, [Module], RDeps),
		 intersection(RDeps, ClashModules, RealModules),
		 size(RealModules, N),
		 N > 1),
	Clashes), !.
	
real_clash_search(Module, []).
	
% Unifies +Predicate with the associated -Module and -File.
declared_in(Predicate,Module,File) :- 
	rule(Predicate,_,File,Module).

%% Returns in wich -ModuleFiles is +Predicate declared.
%% le dipendenze sono interne, il sistema è in grado di trovarle
%% IL PREDICATO E' DEPRECATO, usare belongs_to/2 e external_module/3 se si vuole avere lo stesso risultato.
%depends(Predicate, Module, ModuleFiles) :-
	%Predicate = (Mod:Pred),
	%rule(Pred, _, ModuleFiles, Mod).
  
%% le dipendenze sono interne, il sistema le trova
%depends(Predicate, Module, ModuleFiles) :-
	%current_predicate(M:Predicate),
	%M == user,
	%rule(Predicate, _, ModuleFiles,_).
    
%% dipendenze esterne esplicite, il sistema cerca i file nel quale il modulo è dichiarato.
%depends(Predicate, Module, ModuleFiles) :-
	%all((F),
	    %(rule(Predicate,DepList,_,Module), member(DP, DepList), rule(DP,_,_,M), current_module(M,F)),
	%ModuleFiles).

%depends(Predicate, Module, []) :- 
	%nl,
	%tab(3),
	%tab(3), write('WARNING !!! Cannot find module containing predicate '),
	%write(Predicate), nl,
	%tab(6), writeln('Seems that it hasn\'t been explicitly imported.'),
	%tab(6), write('CAUSE: Probably missing libraries or using predicates declared in non-imported modules.').

% Holds if +Predicate belongs to +Module, can be used for enumerating elements too with backtracking.
  %search for internal predicates (internal = manually imported).
belongs_to(Predicate, Module) :- 
	rule(Predicate, _ ,_ , Module).

% search for external modules (imported by the source code of previously loaded modules).
belongs_to(Predicate, Module) :-
	external_module(_,Module, PList),
	all(_, (member(Predicate, PList)), _).
	
belongs_to(Predicate, Module) :-
	Predicate = (Module:Pred/Arity),
	!.

belongs_to(Predicate, Module) :- 
	all(Mod:Preds, 
		     (all(M, current_module(M), CurrentModules), member(Mod, CurrentModules), current_predicate(Mod:Preds)),
	CX),
	member(Coppia, CX),
	Coppia = (Module:Predicate).

  
% Enumerate all -Modules in wich are declared predicates who are used by +Predicate declared in +Module.
predicate_module_dependency(Predicate, Module, Modules) :- 
	all(Mod,
	  (
	    rule(Predicate,DepList,_,Module),
	    member(DP,DepList),
	    belongs_to(DP,Mod)
	  ),
	  Modules),
	  !.
	  
predicate_module_dependency(Predicate, Module, []).
  
% unifies -Used with predicates used by +X declared in +Module.
used_by(X, Module, Used) :- 
	all(H,
	  (rule(H,B,_,Module) , member(X,B)),
	  Used), !.

used_by(X, Module, []).

% unifies -Uses with predicates using +X declared in +Module.
uses(X, Module, Uses) :- 
	rule(X, Uses, _, Module),
	!.

% converts a +Module to a +List representing the edges of a graph.
rule2list(Module, List) :- 
	all(Pred-U,
	   (predicates(Module,Preds), member(Pred,Preds), uses(Pred,Module, Uses), member(U,Uses)),
	List).
	   

% draws a graphs representing +Module in +Filename
module2png(Module, Filename) :-
	rule2list(Module, Edges),
	draw(Edges, Filename).

% gets all modules imported by +Module in a drawable List
imports2list(Module, List) :-
	all(Module-Import,
	(module_imports(Module, Imports), member(Import,Imports)),
	List),
	!.

imports2list(Module, []).

% draws a graphs representing imports of +Module in +Filename
moduleInteraction2png(Module, Filename) :-
	imports2list(Module, Edges),
	draw(Edges, Filename).


% loads a sequence of +Files for analysis
load_all(Files) :- forall(member(File,Files),
	(
		%set_absolute_path(File),
		init(File)
	)
).

% sets the home of current +Module finding the absolute path of +File
set_absolute_path(File, Module) :-
	get_path(File, RelPath),
	get_absolute_file_path(RelPath, AbsPath),
	get_absolute_file_path(File, AbsoluteFilePath),
	atom_concat(AbsPath, '/', AbsolutePathDirectory),
	safe_assert(module_path(Module, AbsolutePathDirectory)),
	safe_assert(module_absolute_file_path(Module, AbsoluteFilePath)).
	

% switch for treat a +File as a module or a simple Prolog code file.
init(File) :- is_module(File), process_module(File), !.
init(File) :- process_file(File).

% Processes +File as a module
process_module(File) :- 
	get_module_info(File,Module,Predicates),
	assert(now_analyzing(Module)),
	set_absolute_path(File, Module),
	assert_export(Module, Predicates),
	read_file_to_terms(File, Terms),
	imports(Terms, ImportedModules),
	assert_import(Module, ImportedModules),
	get_code_graph(Terms, Graph),
	create_temp_rules(Module, Graph),
	count_clauses(Module, ClausesNum),
	create_final_rules(File, Module),
	collected_information(Module, PredNum, ImportNum),
	nl, write('Analyzed file: '), writeln(File),
	tab(2), write(PredNum), writeln(' predicates;'),
	tab(2), write(ImportNum), writeln(' imports;'),
	write('type queryM('), write(Module), write(') for module information, or '),
	write('type predicates('), write(Module), write(',A) to obtain a list of predicates belonging to '), writeln(Module),
	abolish(now_analyzing/1).
	%abolish(module_path/2).

% Processes +File as a file.
process_file(File) :-  
	remove_extension(File, Module),
	set_absolute_path(File, Module),
	read_file_to_terms(File, Terms),
	assert(now_analyzing(Module)),
	imports(Terms, ImportedModules),
	assert_import(Module, ImportedModules),
	get_code_graph(Terms, Graph),
	create_temp_rules(Module, Graph),
	count_clauses(Module, ClausesNum),
	create_final_rules(File, Module),
	collected_information(Module, PredNum, ImportNum),
	nl, write('Analyzed file: '), writeln(File),
	tab(2), write(PredNum), writeln(' predicates;'),
	tab(2), write(ImportNum), writeln(' imports;'),
	write('type queryM('), write(Module), write(') for module information, or '),
	write('type predicates('), write(Module), write(',A) to obtain a list of predicates belonging to '), writeln(Module),
	abolish(now_analyzing/1).
	%abolish(module_path/2).

count_clauses(Module, Output) :-
	get_temp_facts(Module, CX),
	all(C-Count, (aggregate(count, member(C, CX), Count)), Output),
	forall(member(K, Output), (K = P-N, assert(num_clauses(P, Module, N)))).

get_temp_facts(Module, Output) :-
	findall(X, tmp(Module,X,_), Output).
	
collected_information(Module, PredNum, ImportNum) :-
	predicates(Module, Predicates),
	coreutils:size(Predicates, PredNum),
	module_imports(Module, ImpList), 
	coreutils:size(ImpList, ImportNum),
	!.

collected_information(Module, 0, 0).

% Unifies if module +Module1 uses a predicate declared in module +Module2
module_dependency(Module1, Module2) :- 
	external_module(_,Module2, AX),
	all(P, (member(Pred, AX), (P = Module2:Pred)),AXs), % % predicato/arità => modulo:predicato/arità, 
							    % visto che abbiamo i predicati ed il nome del modulo risparmiamo una ricerca.
	all(DP, rule(_,DP,_,Module1), PX),
	flatten(PX,PC),
	% predicato/arità => modulo:predicato/arità
	all(Pred, (member(Call, PC), qualify_predicate_call(Call, Pred)),PXs),
	intersection(AXs, PXs, R),
	coreutils:size(R, N),
	N > 0.

% qualifica la chiamata ad un predicato ((1) Modulo:Predicato/Arità oppure (2) Predicato:Arità) normalizzandola al primo caso 
% se non è nel formato 1 la normalizza, diversamente non cambia nulla.
qualify_predicate_call(Call, PredCall) :-
	Call = _:_/_, 
	PredCall = Call,
	!.

qualify_predicate_call(Call, PredCall) :-
	Call = Predicate/Arity,
	belongs_to(Call, Module),
	PredCall = Module:Call.


%+FilePath, -Filename
remove_extension(FilePath, Filename) :-
	file_base_name(FilePath, Fname),
	ensure_loaded(library(lineutils)),
	all(N, 
		(atom_codes(Fname,L),
		atom_codes('.',Sep),
		split(L,Sep,RX),
		member(R,RX),
		atom_codes(N,R)),
	NX), 
	nth(1, NX, Filename).

% check if +Predicate is declared in many modules
unique(Predicate) :- 
	all(Module, declared_in(Predicate,Module,_), MX),
	size(MX,N),
	N = 1.

effective_imported_module_deps(Module,Taxonomy) :- 
	all(M, module_imports(Module, M), Modules),
	flatten(Modules, CMod),
	remove_duplicates(CMod, Taxonomy).

	
% Unifies -UsedModules with a list of all modules used by +Module 
% (+Module has predicates who are using, in their own body, predicates declared in each module contained in UsedModules).
used_imports(Module, UsedModules) :- 
	all(D, (module_imports(Module,DX), member(D, DX), module_dependency(Module,D)), UsedModules),
	!.
	
used_imports(Module, []).	
	
% Unifies unused imports for +Module with +Unused_imports
unused_imports(Module, Unused_imports) :- 
	!,
	module_imports(Module,DX),
	used_imports(Module, DDX),
	coreutils:diff(DX,DDX, Unused_imports).

unused_imports(Module, []).

% returns, if first row of +File is a module, the +Module and +PublicPredicates exported
get_module_info(File,Module,PublicPredicates) :-
	read_file_to_terms(File, Terms),
        all(A, (member(A, Terms), A = (:- module(Module,PP))), PX),
        coreutils:size(PX, N),
        N >= 1,
        PX = [H|_],
        H = (:- module(Module,PublicPredicates)),
        !.
        
get_module_info(File,Module,PublicPredicates) :-
	read_file_to_terms(File, Terms),
        all(A, (member(A, Terms), A = (:- module(Module,PP,_))), PX),
        coreutils:size(PX, N),
        N >= 1,
        PX = [H|_],
        H = (:- module(Module,PublicPredicates,_)).

% checks if +FileName contains a module, printing an error message if it doesn't
is_module(FileName) :- catch(check_is_module(FileName), X, (coreutils:print_exception(X), fail)).

% checks if +FileName contains a module.
check_is_module(FileName) :- 
	get_module_info(FileName, _,_),
	!.

%unifies +Terms (representing content of a file) with -ImportedModules. Tells us what libraries the file imports.
imports(Terms, ImportedModules) :-
        get_explicit_imports(Terms, ExplicitImports),
        get_inline_imports(Terms, InlineImports),
        get_single_line_imports(Terms, SingleLineImports),
        lists:append(ExplicitImports, InlineImports, List1),
        lists:append(List1, SingleLineImports, List2),
        lists:remove_duplicates(List2, List3),
        sort(List3, ImportedModules), 
        !.
         
imports(Terms, []).

get_single_line_imports(Terms, Modules) :-
	all(Imports,
	    (
	      member(Line, Terms),
	      Line = (:- A),
	      coreutils:conj_to_list(A, L),
	      member(L1, L),
	      recognize_import(L1, Imports)
	    ),
	    Modules), !.

get_single_line_imports(Terms, []).

% Unifies a +Content string if it unifies with a Module declaration and Returns all in +Modules
% cerca inzialmente gli import espliciti
get_explicit_imports(Terms,Modules) :- all(
          ModuleName,
          (member(Line, Terms), recognize_import(Line,ModuleName)),
          Modules
         ), !.

get_explicit_imports(Terms, []).
         
% cerca gli import in-line (all'interno di parti destre di predicati)       
get_inline_imports(Terms,Modules) :- all(
	Modname,
	(member(Line, Terms),
	 Line = (_ :- RightPart),
	 nonvar(RightPart),
	 coreutils:conj_to_list(RightPart, RPList),
	 member(Element, RPList),
	 recognize_import(Element,Modname),
	 ground(Modname)),
	Modules), !.       

get_inline_imports(Terms, []).

% returns all -ImportedModules used in a Prolog +File
find_imports(File, ImportedModules) :-
        imports(File,LX), all(A, (member(L,LX), coreutils:cleanLibraryName(L,A)), ImportedModules).

% =====================================================================
% LOW-LEVEL PREDICATES.        
% =====================================================================

% asserts that +Module exportes a list of +Predicates
assert_export(Module, Predicates) :- 
	safe_assert(exports(Module, Predicates)). 

% assert that +Module imports a list of +ImportedModules
assert_import(Module, ImportedModules) :-
	assert(module_imports(Module, ImportedModules)).

%a partire dalle regole temporanee, crea le regole finali
create_final_rules(File, Module) :-
	all(_,
	     (all(L, tmp(Module, L, _), LX),
	      sort(LX, LX1),
	      member(L1, LX1),
	      all(R, tmp(Module, L1, R), RX),
	      flatten(RX, RXs),
	      assert(rule(L1, RXs, File, Module))
	     ),
	   _),
	 abolish(tmp/3), !.

create_final_rules(_,_).

%crea le regole temporanee
create_temp_rules(Module, Graph) :-
	all(_,
	      (member(Element, Graph),
	       member(H, Element),
	       H = Left-Right,
	       assert(tmp(Module, Left, Right))
	      ),
	   _), !.

create_temp_rules(_, _).  
      
% PARSING DELLE CLAUSOLE DI HORN.
% ritorna il grafo delle dipendenze a partire dai termini rappresentanti il file sorgente.	
get_code_graph(Terms, Graph) :-
	all(Elements, 
		(
		setof(Predicate-PredicateBody,
			(member(Line, Terms),
			 Line = (LeftPart :- _),
			 coreutils:cleanPredicateName(LeftPart,Predicate),
			 get_predicate_body(Terms, LeftPart, PredicateBody)),
		Elements)),
	Graph), !.

get_code_graph(Terms, []).
	
get_predicate_body(Terms, LeftPart, PredicateBody) :-
	%recupera tutte le parti destre riferite alla parte sinistra data in input
	all(Body, 
		(member(Line, Terms),
		Line = (LeftPart :- RightPart),
		coreutils:conj_to_list(RightPart, Body)), 
	Bodies),
	flatten(Bodies, FlattenBodies),
	% normalizza le parti destre nel formato predicato/arita'
	normalize_predicate(FlattenBodies, PredicateB),
	flatten(PredicateB, PredicateBody).


% Returns a list of predicates -NmAr in the format Nm/Ar from list of names +Names.
% catch/3
% all/3
% setof/3
% findall/3
% bagof/3
normalize_predicate(Names,NmAr) :- 
	all(CPN,
	      (
		member(L, Names),
		collectorize(L, L1),
		all(A1, (member(L2, L1), single_litteral(L2, L3), member(L4, L3), coreutils:cleanPredicateName(L4,A1)), CPN)
	      ),
	NmAr).
  
single_litteral(L2, [L2]) :- 
	coreutils:conj_size(L2, K), 
	K = 1,
	!.

single_litteral(L2, L3) :-
	coreutils:conj_to_list(L2, L3).
  
%Permette di accedere alle parti interne dei costrutti 
% catch/3 (tutti gli elementi)
% all/3 (solo la parte centrale)
% setof/3 (solo la parte centrale)
% findall/3 (solo la parte centrale)
% bagof/3 (solo la parte centrale)
% (A -> B) (tutti gli elementi)
% (A -> B ; C) (tutti gli elementi)
% altrimenti restituisce una lista avente un solo elemento (caso in cui il termine non ricade in uno dei casi considerati).

collectorize(Element, ParsedElement) :-
	Element = catch(First, Second, Third),
	safe_term_to_list(First, F), 
	delete(F, ',', FR),
	safe_term_to_list(Second, S),
	delete(S, ',', SR),
	safe_term_to_list(Third, T),
	delete(T, ',', TR),
	lists:append(FR, SR, FS),
	lists:append(FS, TR, FST),
	lists:remove_duplicates(FST, Ps),
	lists:delete(Ps, null, Predicates),
	all(P, (member(P, Predicates), nonvar(P)), ParsedElement),
	!.

collectorize(Element, ParsedElement) :-
	Element = all(_, Second, _),
	safe_term_to_list(Second, S),
	delete(S, ',', SR),
	lists:remove_duplicates(SR, Ps),
	lists:delete(Ps, null, Predicates),
	all(P, (member(P, Predicates), nonvar(P)), ParsedElement),
	!.

collectorize(Element, ParsedElement) :-
	(Element = setof(_, Second, _)),
	safe_term_to_list(Second, S),
	delete(S, ',', SR),
	lists:remove_duplicates(SR, Ps),
	lists:delete(Ps, null, Predicates),
	all(P, (member(P, Predicates), nonvar(P)), ParsedElement),
	!.

collectorize(Element, ParsedElement) :-
	(Element = bagof(_, Second, _)),
	safe_term_to_list(Second, S),
	delete(S, ',', SR),
	lists:remove_duplicates(SR, Ps),
	lists:delete(Ps, null, Predicates),
	all(P, (member(P, Predicates), nonvar(P)), ParsedElement),
	!.
	
collectorize(Element, ParsedElement) :-
	(Element = findall(_, Second, _)),
	safe_term_to_list(Second, S),
	delete(S, ',', SR),
	lists:remove_duplicates(SR, Ps),
	lists:delete(Ps, null, Predicates),
	all(P, (member(P, Predicates), nonvar(P)), ParsedElement),
	!.	
	
collectorize(Element, ParsedElement) :-
	(Element = findall(_, Second, _, _)),
	safe_term_to_list(Second, S),
	delete(S, ',', SR),
	lists:remove_duplicates(SR, Ps),
	lists:delete(Ps, null, Predicates),
	all(P, (member(P, Predicates), nonvar(P)), ParsedElement),
	!.
	
collectorize(Element, ParsedElement) :-
	(Element = (First -> Second)),
	safe_term_to_list(First, F), 
	delete(F, ',', FR),
	safe_term_to_list(Second, S),
	delete(S, ',', SR),
	lists:append(FR, SR, FS),
	lists:remove_duplicates(FS, Ps),
	lists:delete(Ps, null, Predicates),
	all(P, (member(P, Predicates), nonvar(P)), ParsedElement),
	!.
	
collectorize(Element, ParsedElement) :-
	(Element = (First -> Second; Third)),
	safe_term_to_list(First, F), 
	delete(F, ',', FR),
	safe_term_to_list(Second, S),
	delete(S, ',', SR),
	safe_term_to_list(Third, T),
	delete(T, ',', TR),
	lists:append(FR, SR, FS),
	lists:append(FS, TR, FST),
	lists:remove_duplicates(FST, Ps),
	lists:delete(Ps, null, Predicates),
	all(P, (member(P, Predicates), nonvar(P)), ParsedElement),
	!.
	
	
collectorize(Element, [Element]).

% =.. non fallsce se +Term e' una variabile.
% in quel caso +List viene unificato con l'atomo 'null'.
safe_term_to_list(Term, List) :-
	coreutils:conj_size(Term, Length),
	%Length > 1,
	nonvar(Term), 
	coreutils:conj_to_list(Term, List),
	!.

	%safe_term_to_list(Term, List) :-
	%coreutils:conj_size(Term, Length),
	%Length = 1,
	%nonvar(Term),
	%append([Term], [','], List),
	%!.

safe_term_to_list(Term, [null]).

get_module_path(Module, Path) :-
	Module = (library(ModName)),
	get_system_libraries_absolute_path(ModName, Path).

% restituisce il -Path del file contenente la libreria +Filename.
get_system_libraries_absolute_path(Filename, Paths) :- 
	use_module(library(system)),
	EXT = ['.yap','.pl'], 
	all(CFX,
		(
			all(A,library_directory(A),AX),
			member(A1, AX),
			atom_concat(A1, '/', CX),
			atom_concat(CX, Filename, CFX)
		),
	C),
	all(A,
		(member(C1, C), member(E, EXT), atom_concat(C1, E, A)),
	AllPaths),
	all(A, (member(A, AllPaths), file_exists(A)), Paths).

% dato un ImportingModule ed un Module importato, restituiscimi il path.
get_imported_module_path(ImportingModule, Module, Path) :- 
	module_path(ImportingModule, Path),
	atom_concat('/', Module, SL),
	atom_concat(Path, SL, O),
	get_absolute_file_path(O, Path).
	
% dato un path assoluto +FilePath, ci ritorna il solo percorso della directory di livello inferiore in +Out.
get_path(Path, Output) :-
	atom_length(Path,L),
	file_base_name(Path,Base),
	atom_length(Base,L2),
	sub_atom(Path,_,_,L2,Output),
	!.
	
% unifies +Relative path with +Absolute file path if exists. it works only for file having extension {.pl, .yap}.
get_absolute_file_path(Relative, Absolute) :-
	all(P, (absolute_file_name(Relative, [file_type(prolog), solutions(all)], P)), PX),
	all(P, (member(P, PX), file_exists(P)), PA),
	PA = [Absolute | _].

% controlla se è una chiamata ad un modulo o un path. Verifico se l'import è esplicito o meno: se lo è lo ritorno;
read_import(Import, Module, ImportPath) :-
	file_exists(Import),
	ImportPath = Import,
	!.

% altrimenti costruisco il path e lo ritorno.
read_import(Import, Module, ImportPath) :-
	module_path(Module, ModPath),
	atom_concat(ModPath, Import, Imp_Path),
	get_absolute_file_path(Imp_Path, ImportPath),
	!.
	
% +File, -Module, -PublicPredicates
get_non_module_info(File, Module, PublicPredicates) :-
	not(is_module(File)),
	remove_extension(File, Module),
	read_file_to_terms(File, T),
	get_code_graph(T, Graph),
	all(Head, (member(El, Graph), member(El1,El), El1 = Head-_), Predicates),
	remove_duplicates(Predicates, PublicPredicates).

% a partire da un +File, ritorna il -Module e i -PublicPredicates che esso esporta, sia esso un modulo o no.
% nel caso esso non sia un modulo  (non contenga la direttiva module(_,_)),
% -Module viene unificato con il nome del file privato dell'estensione 
% (CONVENZIONE INTERNA DEL SOFTWARE).

parse_import(File, Module, PublicPredicates) :-
	is_module(File),
	get_module_info(File, Module, PublicPredicates), 
	!.

parse_import(File, Module, PublicPredicates) :-
	get_non_module_info(File, Module, PublicPredicates).

/*
 * Recognizes import type (explicit and inline). Holds if +Line contains an expression like:
 * use_module(library(_)).
 * use_module('path/file.pl')).
 * use_module(library(_), [ExportedPredicates]).
 * use_module('path/file.pl', [ExportedPredicates]).
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
 
 % use_module([lgg]).
recognize_import(Line, ModuleName) :-
	Line = (use_module([Mod])),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% :- use_module([lgg]).
recognize_import(Line, ModuleName) :-
	Line = (:-use_module([Mod])),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% use_module(lgg).
recognize_import(Line, ModuleName) :-
	Line = (use_module(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% use_module(lgg,[...]).
recognize_import(Line, ModuleName) :-
	Line = (use_module(Mod,_)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!. 
	
% ( use_module(modname) -> ... ).
recognize_import(Line, ModuleName) :-
	Line = (use_module(Mod,_) -> _),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!. 

% ( use_module(modname) -> ... ; _).
recognize_import(Line, ModuleName) :-
	Line = (use_module(Mod) -> _ ; _),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!. 

% ( use_module(modname) -> ... ).
recognize_import(Line, ModuleName) :-
	Line = (_ -> _ ; use_module(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!. 

% ( use_module(modname) -> ... ).
recognize_import(Line, ModuleName) :-
	Line = (use_module(Mod) -> _),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!. 
	
	
% ( ... -> use_module(modname)).
recognize_import(Line, ModuleName) :-
	Line = (_ -> use_module(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% ( ... -> use_module(modname,[...])).
recognize_import(Line, ModuleName) :-
	Line = (_ -> use_module(Mod,_)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!. 
	
% ( ... -> use_module(modname) ; ...).
recognize_import(Line, ModuleName) :-
	Line = (_ -> use_module(Mod) ; _),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

recognize_import(Line, ModuleName) :-
	Line = (_ -> use_module(Something) ; _),
	Something = (library(Module)),
	get_system_libraries_absolute_path(Module, [ModulePath|_]),
	get_module_info(ModulePath, ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.
	
% ( ... -> use_module(modname,[...]) ; ...).
recognize_import(Line, ModuleName) :-
	Line = (_ -> use_module(Mod,_) ; _),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

 % :- use_module(modname)
recognize_import(Line,ModuleName) :- 
	Line = (:- use_module(Filename)),
	is_module(Filename),
	get_module_info(Filename, ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.

 % :- use_module(library(modname))
recognize_import(Line,ModuleName) :- 
	Line = (use_module(Something)),
	Something = (library(Module)),
	get_system_libraries_absolute_path(Module, [ModulePath|_]),
	get_module_info(ModulePath, ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.	

% :- use_module(library(modname),_)
recognize_import(Line,ModuleName) :- 
	Line = (:-use_module(Something,_)),
	Something = (library(Module)),
	get_system_libraries_absolute_path(Module, [ModulePath|_]),
	get_module_info(ModulePath, ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.
	
 % :- use_module(library(modname))
recognize_import(Line,ModuleName) :- 
	Line = (use_module(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% :- use_module(library(modname),_)
recognize_import(Line,ModuleName) :- 
	Line = (:-use_module(Mod,_)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% :- use_module(library(modname))
recognize_import(Line,ModuleName) :- 
	Line = (:-use_module(Something)),
	Something = (library(Module)),
	get_system_libraries_absolute_path(Module, [ModulePath|_]),
	get_module_info(ModulePath, ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.

% :- use_module(library(modname, [...]))
recognize_import(Line,ModuleName) :- 
	Line = (:-use_module(Something,_)),
	Something = (library(Module)),
	get_system_libraries_absolute_path(Module, [ModulePath|_]),
	get_module_info(ModulePath, ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.
	
% :- use_module(modname)
recognize_import(Line,ModuleName) :- 
	Line = (:-use_module(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% :- use_module(modname, [...])
recognize_import(Line,ModuleName) :- 
	Line = (:-use_module(Mod,_)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

%% consult([lgg]).
recognize_import(Line, ModuleName) :-
	Line = ([Mod]),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% consult(lgg).
recognize_import(Line, ModuleName) :-
	Line = (consult(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% (... -> consult(lgg))
recognize_import(Line, ModuleName) :-
	Line = (_ -> consult(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% (... -> consult(lgg) ; ...)
recognize_import(Line, ModuleName) :-
	Line = (_ -> consult(Mod) ; _),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.
	
% :- consult(library(lists))
recognize_import(Line,ModuleName) :- 
	Line = (:-consult(Something)),
	Something = (library(Module)),
	get_system_libraries_absolute_path(Module, [ModulePath|_]),
	get_module_info(ModulePath, ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.

% :- consult('path/to/file.pl')
recognize_import(Line,ModuleName) :- 
	Line = (:-consult(Filename)),
	is_module(FileName),
	get_module_info(Filename, ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.

% :- consult('path/to/file.pl')
recognize_import(Line,ModuleName) :- 
	Line = (:-consult(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

%% load_files

% load_files(lgg)
recognize_import(Line, ModuleName) :-
	Line = (load_files(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.
	
% load_files(lgg, [...])
recognize_import(Line, ModuleName) :-
	Line = (load_files(Mod,_)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!. 

% ( ... -> load_files(lgg))
recognize_import(Line, ModuleName) :-
	Line = (_ -> load_files(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% ( ... -> load_files(lgg, [...]))
recognize_import(Line, ModuleName) :-
	Line = (_ -> load_files(Mod,_)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!. 

% ( ... -> load_files(lgg) ; ...)
recognize_import(Line, ModuleName) :-
	Line = (_ -> load_files(Mod) ; _),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% ( ... -> load_files(lgg, [...]) ; ...)
recognize_import(Line, ModuleName) :-
	Line = (_ -> load_files(Mod,_) ; _),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% :- load_files(library(lgg), [...])
recognize_import(Line,ModuleName) :- 
	Line = (:-load_files(Something)),
	Something = (library(Module)),
	get_system_libraries_absolute_path(Module, [ModulePath|_]),
	get_module_info(ModulePath, ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.

% :- load_files(lgg, [...])
recognize_import(Line,ModuleName) :- 
	Line = (:-load_files(Filename)),
	is_module(Filename),
	get_module_info(Filename,ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.

% :- load_files(lgg)
recognize_import(Line,ModuleName) :- 
	Line = (:-load_files(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.
	
%% ensure_loaded
% ensure_loaded([lgg])
recognize_import(Line, ModuleName) :-
	Line = (ensure_loaded([Mod])),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% :- ensure_loaded([lgg])
recognize_import(Line, ModuleName) :-
	Line = (:-ensure_loaded([Mod])),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

% ensure_loaded(library(lgg)).
recognize_import(Line,ModuleName) :- 
	Line = (ensure_loaded(Something)),
	Something = (library(Module)),
	get_system_libraries_absolute_path(Module, [ModulePath|_]),
	get_module_info(ModulePath, ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.

% % ensure_loaded(lgg).
recognize_import(Line, ModuleName) :-
	Line = (ensure_loaded(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

recognize_import(Line, ModuleName) :-
	Line = (_ -> ensure_loaded(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.

recognize_import(Line, ModuleName) :-
	Line = (_ -> ensure_loaded(Mod) ; _),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.
	
recognize_import(Line,ModuleName) :- 
	Line = (:-ensure_loaded(Something)),
	Something = (library(Module)),
	get_system_libraries_absolute_path(Module, [ModulePath|_]),
	get_module_info(ModulePath, ModuleName, Predicates),
	safe_assert(external_module(ModulePath, ModuleName, Predicates)),
	!.
	
recognize_import(Line,ModuleName) :- 
	Line = (:-ensure_loaded(Filename)),
	is_module(FileName),
	get_module_info(FileName, ModuleName, Predicates),
	!.
	
recognize_import(Line,ModuleName) :- 
	Line = (:-ensure_loaded(Mod)),
	now_analyzing(CurrentModule),
	read_import(Mod, CurrentModule, Path),
	parse_import(Path, ModuleName, PublicPredicates),
	safe_assert(external_module(Path, ModuleName, PublicPredicates)),
	!.
	
%check if fact is not yet asserted
safe_assert(Fact):- 
	not(Fact),
	!,
	assert(Fact).

safe_assert(_).

% ritorna -Response = no se +Predicate non è un predicato pubblico exportato da +Module, yes altrimenti.
is_public(Predicate, Module, yes) :-
	exports(Module, PublicPredicates), %rimane legato ai moduli, se il mod esiste ed il predicato va bene ok
	member(Predicate, PublicPredicates),
	!.

is_public(Predicate, Module, no) :-
	exports(Module, PublicPredicates), %rimane legato ai moduli, se il mod esiste ed il predicato non va bene ok
	not(member(Predicate, PublicPredicates)),
	!.
	
is_public(Predicate, Module, yes) :- %è un predicato esportato da un modulo usato, va bene
	external_module(_, Module, PublicPredicates),
	member(Predicate, PublicPredicates),
	!.
	
is_public(Predicate, Module, no) :- %è un predicato non esportato da un modulo usato, va bene comunque
	external_module(_, Module, PublicPredicates),
	not(member(Predicate, PublicPredicates)),
	!.
	
is_public(Predicate, Module, yes) :-
	predicates(Module, PublicPredicates), %non è un modulo, se esiste va bene.
	member(Predicate, PublicPredicates),
	!.
	
is_public(Predicate, Module, no). %non va bene p nudd.

% =====================================================================
%% JAVA COMMUNICATION (JSON) SECTION
% =====================================================================

%unifies -Uses with a list of predicates used by predicate +Predicate declared in +Module
uses4json(Predicate, Module, Uses) :- all((Output), (rule(Predicate, Ux, _, Module), member(U, Ux), (U = (P/A); U = (_:P/A)), atomic_list_concat([P, \/, A], Output)), Uses).

% unifies -Used with predicates used by +Predicate declared in +Module.
used_by4json(Predicate, Module, Used) :- findall(Output, (rule(H,B,_,Module) ,member(Predicate,B), H = (P/A), atomic_list_concat([P, \/, A], Output)), Used).

%converte un name clash (predicato/arita-[lista di moduli dichiaranti il predicato]) in un formato piu' pratico e gestibile per json.
%il json risultante avra' in coda il predicato e l'arita, preceduto dai moduli che lo dichiarano.
clashes2json(Clashes, JTerm) :-
	all(Output,
	    (member(Clash, Clashes),
	     Clash = (Pred/Arity-Modules),
	     atomic_list_concat([Pred, \/, Arity], FinalPred),
	     append(Modules, [FinalPred], Output)),
	    JTerm).

clashes2json([], []).

exports4json(Module, ExportedPredicates) :-
	exports(Module, Predicates),
	all(Output,(member(Predicate, Predicates), Predicate = Pname/Arity, atomic_list_concat([Pname, \/, Arity], Output)), ExportedPredicates), 
	!.
	
exports4json(Module, ExportedPredicates) :- 
	predicates(Module, Predicates),
	all(Output,(member(Predicate, Predicates), Predicate = Pname/Arity, atomic_list_concat([Pname, \/, Arity], Output)), ExportedPredicates).
	
% returns info about +Predicate declared in +Module to -Json serialization standard format
predicate_info_to_json(Module:Predicate, Json) :- 
        declared_in(Predicate, Module, _), !,
        module_absolute_file_path(Module, FDecl),
        num_clauses(Predicate, Module, Clauses),
        is_public(Predicate, Module, Response),
        uses4json(Predicate, Module, Uses),
        used_by4json(Predicate, Module, Used),
        predicate_module_dependency(Predicate, Module, Modules),
        Predicate = (PredName/Arity),
        JsonTerm = json([predicate_name = PredName, arity = Arity, num_clauses = Clauses, is_public = Response, decl_in=Module, decl_in_file=FDecl, uses=Uses, used_by=Used, uses_modules=Modules]),
        json:term2json(JsonTerm, Json).
        
predicate_json_to_file(Pred, F) :-       
	predicate_info_to_json(Pred, J),        
	to_temp_file(J, F).

to_temp_file(Content, File) :-
	use_module(library(system)),
	tmpnam(File),
	open(File, write, S),
	write(S, Content),
	write(S, '\n'),
	close(S).

get_used_files(Modules, Files) :-
	all(F, (member(P, Modules), (external_module(F,P,_))), Files), 
	!.
	
get_used_files(Modules, []).

% Unifies -JsonFile with a temp file containing the JSONized result of predicates.
predicates(json, Module, JsonFile) :- 
	all(Output, (rule(P,_,_,Module), P = Pred/Arity, atomic_list_concat([Pred, \/, Arity], Output)), PX),
	J = json([output=PX]),
	json:term2json(J,Json),
	to_temp_file(Json, JsonFile).

% Unifies -T with a list in JSON format.
modules(json, JsonFile) :- 
	all(M, (rule(_,_,_,M); module_imports(M,_)), MX), 
	sort(MX,T),
	J = json([output=T]),
	json:term2json(J,Json),
	to_temp_file(Json, JsonFile).
	
module_info_to_json(Module, Json) :-       
	exports4json(Module, PublicPredicates),
	module_absolute_file_path(Module, FilePath),
	used_imports(Module, UsedModules),
	effective_imported_module_deps(Module,Taxonomy),
        coreutils:diff(Taxonomy, UsedModules,UnUsedModules),
        get_used_files(UsedModules, UsedFiles),     
        clash_search(Module, Clashes),
        clashes2json(Clashes, NameClashes),
        real_clash_search(Module, RealClashes),
        clashes2json(RealClashes, RealNameClashes),
        JsonTerm = json([module_name = Module, file_path= FilePath, real_clashes = RealNameClashes, public_predicates = PublicPredicates, unused_imports = UnUsedModules, used_imports=UsedModules, used_files = UsedFiles, clashes = NameClashes, real_taxonomy = Taxonomy]),
        json:term2json(JsonTerm, Json), !.

module_json_to_file(Module, File) :-
  module_info_to_json(Module, Json),
  to_temp_file(Json, File).