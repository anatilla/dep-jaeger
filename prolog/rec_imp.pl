:- module(recimp, [recognizeImport/2]).


% ricordarsi di verificare ed eventualmente gestire il caso di import di librerie, come ad esempio ensure_loaded(library(lists)).
% capire se e' necessario 'pulire' il corpo della import. ad esempio come nei casi library(lists) e lists

%da vedere bene.
%ristrutturare questo predicato pensando al fatto che quando importo qualcosa, qualcosa = library(X), nomeModulo o nomeFile.

recognizeImport(Line,ModuleName) :- Line = (:-use_module(Something)), Something = (library(ModuleName)),!.
recognizeImport(Line,ModuleName) :- Line = (:-use_module(Something,_)), Something = (library(ModuleName)),!.
recognizeImport(Line,ModuleName) :- Line = (:-use_module(ModuleName)),!.
recognizeImport(Line,ModuleName) :- Line = (:-use_module(ModuleName,_)),!.

recognizeImport(Line,ModuleName) :- Line = (:-consult(Something)), Something = (library(ModuleName)),!.
recognizeImport(Line,ModuleName) :- Line = (:-consult(Filename)), checkIsModule(FileName), getModuleInfo(Filename, ModuleName, Predicates),!.
recognizeImport(Line,ModuleName) :- Line = (:-consult(Filename)), ModuleName = Filename,!.

recognizeImport(Line,ModuleName) :- Line = (:-load_files(Something,_)), Something = (library(ModuleName)),!.
recognizeImport(Line,ModuleName) :- Line = (:-load_files(Filename,_)), checkIsModule(Filename), getModuleInfo(Filename,ModuleName, Predicates),!.
recognizeImport(Line,ModuleName) :- Line = (:-load_files(Modulename,_)), ModuleName = Filename,!.

recognizeImport(Line,ModuleName) :- Line = (:-ensure_loaded(Something)), Something = (library(ModuleName)),!.
recognizeImport(Line,ModuleName) :- Line = (:-ensure_loaded(Filename)), checkIsModule(FileName), getModuleInfo(FileName, ModuleName, Predicates),!.
recognizeImport(Line,ModuleName) :- Line = (:-ensure_loaded(Filename)), ModuleName = Filename,!.
