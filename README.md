dep-jaeger
=========

an internal/external dependency analyzer for software written in Prolog.

This project was developed in the context of the Artificial Intelligence course, MSc in Computer Science, Università degli Studi di Bari 'Aldo Moro'.

Further info are available in the documentation (Italian Only).

## Authors 


Nicola Alessandro Natilla - alessandro.natilla@gmail.com	
Damiano Romita - d.romita@gmail.com

## Dependencies

Unix/Linux O.S.

yap 6.2.2 - http://www.dcc.fc.up.pt/vsc/Yap/yap-6.2.2.tar.gz

graphviz - http://www.graphviz.org


## Info


Prolog sources are available both in folder 'prolog' and in folder named 'core' contained in 'dep-jaeger'. 
Java sources are available in folder 'java'.

## Run


folder 'core' needs to exist in the same path of jarfile.

in terminal, type:  

	$ java -jar jaeger.jar <path-assoluto\\to\\yap>

Also, if your prefer console interaction, type:
	
	$ cd //path//to//core//
	$ yap
	$ ['core'].
	$ help.
	
