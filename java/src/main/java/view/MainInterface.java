/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package view;

import javax.swing.*;
import javax.swing.event.ListSelectionListener;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import java.util.List;

/**
 * @author nomak
 */
public class MainInterface extends javax.swing.JFrame {

    private List<String> fileList;
    private LinkedList<String> analyzedModules = new LinkedList<String>();
    private boolean firstRun = true;
    //    /**
//     * @param args the command line arguments
//     */
//    public static void main(String args[]) {
//        /* Set the Nimbus look and feel */
//        //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
//        /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
//         * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html
//         */
//        try {
//            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
//                if ("Nimbus".equals(info.getName())) {
//                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
//                    break;
//                }
//            }
//        } catch (ClassNotFoundException ex) {
//            java.util.logging.Logger.getLogger(Interface.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
//        } catch (InstantiationException ex) {
//            java.util.logging.Logger.getLogger(Interface.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
//        } catch (IllegalAccessException ex) {
//            java.util.logging.Logger.getLogger(Interface.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
//        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
//            java.util.logging.Logger.getLogger(Interface.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
//        }
//        //</editor-fold>
//
//        /* Create and display the form */
//        java.awt.EventQueue.invokeLater(new Runnable() {
//            public void run() {
//                new Interface().setVisible(true);
//            }
//        });
//    }
    // Variables declaration - do not modify
    private javax.swing.JPanel IO_pane;
    private javax.swing.JButton add_files;
    private javax.swing.JButton clearButton;
    private javax.swing.JButton generateGraphButton;
    private javax.swing.JButton generateModGraphButton;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JList list_modules;
    private javax.swing.JList list_predicates;
    private javax.swing.JScrollPane modules_pane;
    private javax.swing.JTextArea nomi_files;
    private javax.swing.JScrollPane predicates_pane;
    private javax.swing.JButton queryModuleButton;
    private javax.swing.JButton queryPredicateButton;
    private javax.swing.JTextArea status_output;

    /**
     * Creates new form Interface
     */
    public MainInterface() {
        initComponents();
//        this.generatePredGraphButton.setLocation(this.getLocation().x, (this.getLocation().y-50));
        this.setLocationRelativeTo(null);
    }

    public LinkedList<String> getAnalyzedModules() {
        return analyzedModules;
    }

    public void setAnalyzedModules(LinkedList<String> analyzedModules) {
        this.analyzedModules = analyzedModules;
    }
    
    public void setFileList(LinkedList<String> listaFile) {
        this.fileList = listaFile;
    }

    public boolean isFirstRun() {
        return firstRun;
    }

    public void setFirstRun(boolean firstRun) {
        this.firstRun = firstRun;
    }

    public JButton getQueryPredicateButton() {
        return queryPredicateButton;
    }

    public void setQueryPredicateButton(JButton queryPredicateButton) {
        this.queryPredicateButton = queryPredicateButton;
    }

    public JButton getQueryModuleButton() {
        return queryModuleButton;
    }

    public void setQueryModuleButton(JButton queryModuleButton) {
        this.queryModuleButton = queryModuleButton;
    }

    public JButton getGenerateGraphButton() {
        return generateGraphButton;
    }

    public void setGenerateGraphButton(JButton generateGraphButton) {
        this.generateGraphButton = generateGraphButton;
    }
    
    public JButton getGeneratePredGraphButton() {
        return generateModGraphButton;
    }

    public void setGeneratePredGraphButton(JButton generatePredGraphButton) {
        this.generateModGraphButton = generatePredGraphButton;
    }

    public JTextArea getNomi_files() {
        return nomi_files;
    }

    public void setNomi_files(JTextArea nomi_files) {
        this.nomi_files = nomi_files;
    }

    public JTextArea getStatus_output() {
        return status_output;
    }

    public void setStatus_output(JTextArea status_output) {
        this.status_output = status_output;
    }

    public void setStatus_output(String t) {
        status_output.setText(t);
    }

    public JList getList_modules() {
        return list_modules;
    }

    public void setList_modules(JList list_modules) {
        this.list_modules = list_modules;
    }

    public void setList_modules(List<String> lista) {
        if (lista != null) {
            DefaultListModel list_model = new DefaultListModel();
            for (String element : lista) {
                list_model.addElement(element);
            }
            list_modules.setModel(list_model);
            list_modules.setSelectedIndex(0);
        } else {
            list_modules.clearSelection();
            list_modules.setModel(new DefaultListModel());
        }
    }
    
    public void setListModulesSelectedIndex(int i){
        list_modules.setSelectedIndex(i);
    }

    public JList getList_predicates() {
        return list_predicates;
    }
    // </editor-fold>                        

    public void setList_predicates(JList list_predicates) {
        this.list_predicates = list_predicates;
    }

    public void setList_predicates(List<String> lista) {
        if (lista != null) {
            DefaultListModel list_model = new DefaultListModel();
            for (String element : lista) {
                list_model.addElement(element);
            }
            list_predicates.setModel(list_model);
            list_predicates.setSelectedIndex(0);
        } else {
            list_predicates.clearSelection();
            list_predicates.setModel(new DefaultListModel());
        }
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">
                          
    private void initComponents() {

        jSeparator1 = new javax.swing.JSeparator();
        jPanel1 = new javax.swing.JPanel();
        modules_pane = new javax.swing.JScrollPane();
        list_modules = new javax.swing.JList();
        predicates_pane = new javax.swing.JScrollPane();
        list_predicates = new javax.swing.JList();
        queryModuleButton = new javax.swing.JButton();
        jSeparator2 = new javax.swing.JSeparator();
        queryPredicateButton = new javax.swing.JButton();
        generateGraphButton = new javax.swing.JButton();
        generateModGraphButton = new javax.swing.JButton();
        IO_pane = new javax.swing.JPanel();
        add_files = new javax.swing.JButton();
        clearButton = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        nomi_files = new javax.swing.JTextArea();
        jScrollPane2 = new javax.swing.JScrollPane();
        status_output = new javax.swing.JTextArea();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("JPDep");
        setBackground(new java.awt.Color(194, 249, 255));
        setSize(new java.awt.Dimension(600, 600));

        jPanel1.setBackground(new java.awt.Color(158, 224, 243));

        list_modules.setModel(new javax.swing.AbstractListModel() {
            String[] strings = {"Please load file(s)", "to make modules", "available"};

            public int getSize() {
                return strings.length;
            }

            public Object getElementAt(int i) {
                return strings[i];
            }
        });
        list_modules.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        list_modules.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                list_modulesValueChanged(evt);
            }
        });
        modules_pane.setViewportView(list_modules);
        list_modules.getAccessibleContext().setAccessibleName("moduleList");

        list_predicates.setModel(new javax.swing.AbstractListModel() {
            String[] strings = {"Please load file(s)", "to make predicates", "available"};

            public int getSize() {
                return strings.length;
            }

            public Object getElementAt(int i) {
                return strings[i];
            }
        });
        list_predicates.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        predicates_pane.setViewportView(list_predicates);
        list_predicates.getAccessibleContext().setAccessibleName("predicateList");

        queryModuleButton.setText("Analyze module");
        queryModuleButton.setEnabled(false);
        queryModuleButton.setSize(new java.awt.Dimension(50, 29));

        jSeparator2.setForeground(new java.awt.Color(255, 0, 0));
        jSeparator2.setOrientation(javax.swing.SwingConstants.VERTICAL);

        queryPredicateButton.setText("Analyze predicate");
        queryPredicateButton.setEnabled(false);

        generateGraphButton.setText("Generate inner structure graph");
        generateGraphButton.setEnabled(false);
        
        generateModGraphButton.setText("Generate module imports graph");
        generateModGraphButton.setEnabled(false);

        org.jdesktop.layout.GroupLayout jPanel1Layout = new org.jdesktop.layout.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
                jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                        .add(jPanel1Layout.createSequentialGroup()
                                .addContainerGap()
                                .add(modules_pane, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 112, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING, false)
                                        .add(queryModuleButton, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .add(generateGraphButton, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .add(generateModGraphButton, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))    
                                .addPreferredGap(org.jdesktop.layout.LayoutStyle.UNRELATED)
                                .add(jSeparator2, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                                .add(predicates_pane, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
//                                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING, false)
                                .add(queryPredicateButton, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
//                                    .add(generateModGraphButton, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                .add(18, 18, 18))
                                
                                
        );
        jPanel1Layout.setVerticalGroup(
                jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                        .add(jPanel1Layout.createSequentialGroup()
                                .addContainerGap()
                                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                                        .add(predicates_pane, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 174, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                                        .add(jSeparator2, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 214, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                                .add(0, 0, Short.MAX_VALUE))
                        .add(jPanel1Layout.createSequentialGroup()
                                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                                        .add(jPanel1Layout.createSequentialGroup()
                                                .add(5, 5, 5)
                                                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                                                        .add(modules_pane, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 174, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                                                        .add(jPanel1Layout.createSequentialGroup()
                                                                .add(0, 0, 0)
                                                                .add(queryModuleButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 61, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                                                                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                                                                .add(generateGraphButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 52, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                                                                .add(generateModGraphButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 52, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))))
                                        .add(jPanel1Layout.createSequentialGroup()
                                                .add(49, 49, 49)
                                                .add(queryPredicateButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 61, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                                                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)))
//                                                .add(generateModGraphButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 52, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
                                                
                                .addContainerGap(org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        queryModuleButton.getAccessibleContext().setAccessibleName("");

        IO_pane.setBackground(new java.awt.Color(158, 224, 243));
        IO_pane.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 1, true));

        add_files.setLabel("Add files");

        clearButton.setLabel("Clear");

        nomi_files.setEditable(false);
        nomi_files.setColumns(20);
        nomi_files.setLineWrap(true);
        nomi_files.setRows(5);
        nomi_files.setText("No file(s) added, please add files to start working.");
        nomi_files.setWrapStyleWord(true);
        jScrollPane1.setViewportView(nomi_files);

        status_output.setEditable(false);
        status_output.setColumns(20);
        status_output.setRows(5);
        status_output.setTabSize(3);
        status_output.setText("No queries issued, please analyze a module or a query with the buttons below.");
        jScrollPane2.setViewportView(status_output);

        org.jdesktop.layout.GroupLayout IO_paneLayout = new org.jdesktop.layout.GroupLayout(IO_pane);
        IO_pane.setLayout(IO_paneLayout);
        IO_paneLayout.setHorizontalGroup(
                IO_paneLayout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                        .add(IO_paneLayout.createSequentialGroup()
                                .addContainerGap()
                                .add(IO_paneLayout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                                        .add(jScrollPane2, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 606, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                                        .add(IO_paneLayout.createSequentialGroup()
                                                .add(IO_paneLayout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                                                        .add(add_files)
                                                        .add(clearButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 100, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                                                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                                                .add(jScrollPane1, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 500, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
                                .addContainerGap(org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        IO_paneLayout.setVerticalGroup(
                IO_paneLayout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                        .add(IO_paneLayout.createSequentialGroup()
                                .add(12, 12, 12)
                                .add(IO_paneLayout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                                        .add(IO_paneLayout.createSequentialGroup()
                                                .add(add_files)
                                                .add(2, 2, 2)
                                                .add(clearButton))
                                        .add(jScrollPane1, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 92, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                                .add(jScrollPane2, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 236, Short.MAX_VALUE)
                                .addContainerGap())
        );

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                        .add(jPanel1, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .add(org.jdesktop.layout.GroupLayout.TRAILING, IO_pane, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                        .add(org.jdesktop.layout.GroupLayout.TRAILING, layout.createSequentialGroup()
                                .add(IO_pane, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                                .add(jPanel1, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 182, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
        );

        pack();
    }

    private void list_modulesValueChanged(javax.swing.event.ListSelectionEvent evt) {//GEN-FIRST:event_list_modulesValueChanged
//        if (isModuleAnalyzed(list_modules.getSelectedValue().toString()))
//            generateGraphButton.setEnabled(true);
//        else
//            generateGraphButton.setEnabled(false);
    }//GEN-LAST:event_list_modulesValueChanged

    private boolean isModuleAnalyzed(String module) {
        if (analyzedModules.contains(module))
            return true;
        else
            return false;
    }

    public void setModulesFocusListener(ListSelectionListener e) {
        list_modules.addListSelectionListener(e);
    }

    public void resetAll() {

    }

    public List<String> getFilePaths() {
        return fileList;
    }

    public void setFilePaths(List<String> lista) {
        fileList = lista;
        setFile_names(fileList);
        queryModuleButton.setEnabled(true);
        queryPredicateButton.setEnabled(true);
        generateGraphButton.setEnabled(true);
        generateModGraphButton.setEnabled(true);
    }

    public void setFile_names(List<String> lista) {
        String nomiFile = new String();
        for (String nome : lista) {
            nomiFile += nome + "\n";
        }
        if (firstRun) {
            nomi_files.setText(nomiFile);
            firstRun = false;
        } else
            nomi_files.append(nomiFile);
    }

    public String getSelectedModule() {
        if (list_modules.isSelectionEmpty()) {
//            notificationPopUp notify = new notificationPopUp("Errore", "Selezionare un elemento dalla lista dei moduli");
            return "";
        } else
            return list_modules.getSelectedValue().toString();
    }

    public String getSelectedPredicate() {
        if (list_predicates.isSelectionEmpty()) {
//            notificationPopUp notify = new notificationPopUp("Errore", "Selezionare un elemento dalla lista dei predicati");
            return "";
        } else
            return list_predicates.getSelectedValue().toString();
    }

    public void setAdd_filesListener(ActionListener listener) {
        add_files.addActionListener(listener);
    }

    public void setQueryModuleListener(ActionListener listener) {
        queryModuleButton.addActionListener(listener);
    }

    public void setQueryPredicateListener(ActionListener listener) {
        queryPredicateButton.addActionListener(listener);
    }

    public void setGenerateGraphListener(ActionListener listener) {
        generateGraphButton.addActionListener(listener);
    }
    
    public void setGeneratePredGraphListener(ActionListener listener) {
        generateModGraphButton.addActionListener(listener);
    }

    public void setClearListener(ActionListener listener) {
        clearButton.addActionListener(listener);
    }

    public void addAnalyzedModule(String module) {
        if (!isModuleAnalyzed(module))
            analyzedModules.add(module);
    }

    public void setGenerateGraphEnabled(boolean status) {
        generateGraphButton.setEnabled(status);
    }
    
    public void setGeneratePredGraphEnabled(boolean status) {
        generateModGraphButton.setEnabled(status);
    }
    
    public void resetAnalyzedModules(){
        analyzedModules = new LinkedList<String>();
    }

    public void displayErrorMessage(String errorMessage) {
        JOptionPane.showMessageDialog(this, errorMessage);
    }
    // End of variables declaration                   
}
