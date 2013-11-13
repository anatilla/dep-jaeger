package controller;

import model.Element;
import model.GenericOutput;
import model.Module;
import model.Predicate;
import protocol.ElementType;
import protocol.InterPrologFactory;
import protocol.ProtocolFactory;
import view.MainInterface;
import view.notificationPopUp;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Author: alexander
 * Project: jpdep
 */

public class MainController {

    private static String[] prologExtensions = {".pl", ".yap"};
    private ProtocolFactory factory;
    private MainInterface view;
    private Element element;
    private int loadedFiles = 0;

    public MainController(MainInterface view, String yap_path, String prolog_core) {
        this.view = view;

        factory = new InterPrologFactory(prolog_core, yap_path);
        this.view.setAdd_filesListener(new AddFileListener());
        this.view.setQueryModuleListener(new QueryModuleListener());
        this.view.setQueryPredicateListener(new QueryPredicateListener());
        this.view.setGenerateGraphListener(new GenerateGraphListener());
        this.view.setGeneratePredGraphListener(new GenerateModGraphListener());
        this.view.setClearListener(new ClearListener());
        this.view.setModulesFocusListener(new PredicatesFocusListener());

    }

    public void reset_environment() {
        factory.sendCommand("flush");
        view.setFirstRun(true);
        view.getQueryPredicateButton().setEnabled(false);
        view.getQueryModuleButton().setEnabled(false);
        view.getGenerateGraphButton().setEnabled(false);
        view.getGeneratePredGraphButton().setEnabled(false);
        view.getNomi_files().setText("No file(s) added, please add files to start working.");
        view.getStatus_output().setText("No queries issued, please analyze a module or a query with the buttons below.");
        view.setList_modules(new LinkedList(Arrays.asList(new String[]{"Please load file(s)", "to make modules", "available"})));
        view.setAnalyzedModules(new LinkedList<String>());
        view.setFileList(new LinkedList<String>());
        view.setList_predicates(new LinkedList(Arrays.asList(new String[]{"Please load file(s)", "to make predicates", "available"})));
    }

    private class PredicatesFocusListener implements ListSelectionListener {

        @Override
        public void valueChanged(ListSelectionEvent e) {
            if (!e.getValueIsAdjusting()) {

                List<String> lista;
                Element predicates;

                try {
                    String modulo = view.getSelectedModule();
                    if(!view.isFirstRun() && modulo == ""){
                        view.setListModulesSelectedIndex(0);
                        modulo = view.getSelectedModule();
                    }
                    predicates = InterPrologFactory.readJson(factory.sendCommand("predicates", "'" + modulo + "'", "X"), ElementType.GENERIC);


                    lista = new LinkedList<String>(((GenericOutput) predicates).getOutput());
//                    view.setList_predicates(lista);

                    LinkedList<String> listaDef = new LinkedList<String>();
                    for (String predicato : lista) {
                        predicato = predicato.replace("\\", "");
                        listaDef.add(predicato);
                    }
                    view.setList_predicates(listaDef);
                } catch (IOException e1) {
                    e1.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.

                } catch (NullPointerException e2) {

                    view.setList_predicates(new LinkedList(Arrays.asList(new String[]{"Please load file(s)", "to make predicates", "available"})));
                }

            }
        }
    }

    private class AddFileListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {

            JFileChooser fileChooser = new JFileChooser();

            // applicazione di default per filtrare i soli file prolog
            javax.swing.filechooser.FileFilter plFilter = new FileNameExtensionFilter("Prolog files", "pl", "yap");
            fileChooser.addChoosableFileFilter(plFilter);
            // di default si propone di scegliere solo i file
            fileChooser.setFileFilter(plFilter);

//            // consentiamo anche di scegliere intere cartelle
            fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
            // consentiamo di scegliere solo insiemi di file
//            fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            // consentiamo anche di scegliere pi?? file o cartelle
            fileChooser.setMultiSelectionEnabled(true);

            boolean goodSelection = false;
            while (!goodSelection) {
                int n = fileChooser.showOpenDialog(fileChooser);

                if (n == JFileChooser.APPROVE_OPTION) {
                    File[] selectedFiles = fileChooser.getSelectedFiles();
                    int contafile = recursiveCountFiles(selectedFiles, n);
                    
                    if ((loadedFiles + contafile) < 15 ) {
                        if (contafile != 0) {
                            goodSelection = true;
                            loadedFiles += contafile;
                            
                            LinkedList<String> absolutePaths = new LinkedList<String>();

                            for (File singleFile : selectedFiles) {
                                absolutePaths.addAll(recursiveGetFiles(singleFile, absolutePaths));
                            }
                            factory.addFiles(absolutePaths);
                            view.setFilePaths(absolutePaths);

                            LinkedList<String> lista;
                            Element modules = null;

                            try {
                                modules = InterPrologFactory.readJson(factory.sendCommand("modules", "X"), ElementType.GENERIC);

                                lista = new LinkedList<String>(((GenericOutput) modules).getOutput());
                                view.setList_modules(lista);
//                                view.setListModulesSelectedIndex(0);
                            } catch (IOException e1) {
                                e1.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                            }

                            Element predicates;
                            try {
                                predicates = InterPrologFactory.readJson(factory.sendCommand("predicates", "'" + view.getSelectedModule() + "'", "X"), ElementType.GENERIC);

                                lista = new LinkedList<String>(((GenericOutput) predicates).getOutput());
//                                view.setList_predicates(lista);

                                LinkedList<String> listaDef = new LinkedList<String>();
                                for (String predicato : lista) {
                                    predicato = predicato.replace("\\", "");
                                    listaDef.add(predicato);
                                }
                                view.setList_predicates(listaDef);
                            } catch (IOException e1) {
                                e1.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                            }
                        } else {
                            notificationPopUp noFiles = new notificationPopUp("Attenzione", "Nella destinazione inserita non sono stati trovati file Prolog");
                        }
                    } else {
                        notificationPopUp troppiFile;
                        if(loadedFiles == 0){
                            troppiFile = new notificationPopUp("Attenzione", "Interprolog puo' gestire max 14 file, inseriti " + contafile + " files.");
                        }
                        else{
                            troppiFile = new notificationPopUp("Attenzione", "Interprolog puo' gestire max 14 file, inseriti " + contafile + " files.\nSe ne sono gia' caricati " + loadedFiles + ", se ne possono caricare ancora " + (14-loadedFiles) + ".");
                        }
                    }
                } else
                    goodSelection = true; // cos? pu? uscire dal ciclo se l'utente clicca su "cancel"
            }
        }

        private LinkedList<String> recursiveGetFiles(File file, LinkedList<String> percorsi) {
            LinkedList<String> percorsiNew = new LinkedList<String>();

            if (file.isDirectory()) {
                File[] listaFile = file.listFiles();
                for (File sottoFile : listaFile) {
                    percorsiNew.addAll(recursiveGetFiles(sottoFile, percorsi));
                }
            } else if (file.getName().endsWith(".pl") || file.getName().endsWith(".yap"))
                percorsiNew.add(file.getAbsolutePath());

            // alternativa elegante ma pi?? pesante, inutile per due sole estensioni
//            else{
//                for (String estensione : prologExtensions){
//                    if (file.getName().endsWith(estensione))
//                        percorsi.add(file.getAbsolutePath());
//                }
//            }

            return percorsiNew;
        }

        private int recursiveCountFiles(File[] files, int preConta) {
            int conta = preConta;
            for (File file : files) {
                if (file.isDirectory()) {
                    File[] listaFile = file.listFiles();
                    conta = recursiveCountFiles(listaFile, conta);
                } else if (file.getName().endsWith(".pl") || file.getName().endsWith(".yap"))
                    conta++;
            }
            // alternativa elegante ma pi?? pesante, inutile per due sole estensioni
//            else{
//                for (String estensione : prologExtensions){
//                    if (file.getName().endsWith(estensione))
//                        percorsi.add(file.getAbsolutePath());
//                }
//            }

            return conta;
        }
    }

    private class QueryModuleListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            String nomeModulo = view.getSelectedModule();
            if (!nomeModulo.isEmpty()) {
                try {
                    element = factory.query(nomeModulo, ElementType.MODULE);
                    view.addAnalyzedModule(nomeModulo);
//                    view.setGenerateGraphEnabled(true);
                    view.setStatus_output(((Module) element).toString());
                } catch (Exception ex) {
                    Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
    }

    private class QueryPredicateListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            String nomePred = view.getSelectedPredicate();
            if (!nomePred.isEmpty()) {
                String nomePredFull = view.getSelectedModule() + ":" + nomePred;
                try {
                    element = factory.query(nomePredFull, ElementType.PREDICATE);
                    view.setStatus_output(((Predicate) element).toString());
                } catch (Exception ex) {
                    Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
    }

    private class GenerateGraphListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            String nomeModulo = view.getSelectedModule();
            if (!nomeModulo.isEmpty()) {
                String fileName;
                JFileChooser chooser = new JFileChooser();

                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                chooser.setMultiSelectionEnabled(false);
                chooser.setDialogTitle("Choose folder to save the graph in");
                int n = chooser.showOpenDialog(chooser);

                if (n == JFileChooser.APPROVE_OPTION) {
                    File folder = chooser.getSelectedFile();
                    fileName = folder.getAbsolutePath() + "/grafico-" + nomeModulo + ".png";
                    try {
                        factory.sendCommand("module2png(" + nomeModulo + ", '" + fileName + "')");
                    } catch (Exception ex) {
                        Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            }
        }
    }
    
    private class GenerateModGraphListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            String nomeMod = view.getSelectedModule();
            if (!nomeMod.isEmpty()) {
                String fileName;
                JFileChooser chooser = new JFileChooser();

                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                chooser.setMultiSelectionEnabled(false);
                chooser.setDialogTitle("Choose folder to save the graph in");
                int n = chooser.showOpenDialog(chooser);

                if (n == JFileChooser.APPROVE_OPTION) {
                    File folder = chooser.getSelectedFile();
                    fileName = folder.getAbsolutePath() + "/grafico_imports_" + nomeMod + ".png";
                    try {
                        factory.sendCommand("moduleInteraction2png(" + nomeMod + ", '" + fileName + "')");
                    } catch (Exception ex) {
                        Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            }
        }
    }

    private class ClearListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            reset_environment();
            //TO-DO: o abolisci tutti i predicati del prolog, o killi il terminale e lo riapri (io preferisco)
        }
    }

}