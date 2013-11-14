dep-jaeger
=========

An internal/external dependency analyzer for software written in Prolog, useful for a quick overview about the internal structure.

Software's core is written in Prolog, while the GUI is implemented in Java.

This project was developed in the context of the Artificial Intelligence course (taught by <a href="http://lacam.di.uniba.it:8000/people/FlorianaEsposito.html">F. Esposito</a> and <a href="http://lacam.di.uniba.it:8000/~ferilli/ufficiale/ferilli.html">S. Ferilli</a>), MSc in Computer Science, Università degli Studi di Bari 'Aldo Moro'.

dep-jaeger is able to run only on YAP.

Further info are available in the documentation (Italian Only).


## Authors 


Nicola Alessandro Natilla - alessandro.natilla@gmail.com	
Damiano Romita - d.romita@gmail.com

## Dependencies

Unix/Linux O.S.

YAP 6.2.2 - http://www.dcc.fc.up.pt/vsc/Yap/yap-6.2.2.tar.gz

graphviz - http://www.graphviz.org


## Info


Prolog sources are available both in folder 'prolog' and in folder named 'core' contained in 'dep-jaeger'. 
Java sources are available in folder 'java'.

## Run


First, rename folder 'prolog' in 'core'. This directory needs to be placed in the same path of jarfile.

Then, in terminal, type:  

	$ java -jar jaeger.jar <path-assoluto\\to\\yap>

Also if your prefer console interaction, type:
	
	$ cd //path//to//core//
	$ yap
	$ ['core'].
	$ help.
	
