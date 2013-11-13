package view;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

/**
 * Author: alexander
 * Project: jpdep
 */
public class MainForm extends javax.swing.JFrame {
    private JTextPane status_output = new JTextPane();
    private JTextPane info_predicates = new JTextPane();
    private JTextPane info_modules = new JTextPane();
    private JTextPane loaded_modules = new JTextPane();
    private JTextPane loaded_predicates = new JTextPane();
    private JButton add_file = new JButton("add file");
    private JButton query_module = new JButton("query selected module");
    private JButton query_predicate = new JButton("query selected predicate");
    private List<String> fileList;

    public MainForm() {
        JPanel panel = new JPanel();
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        this.setSize(800, 600);

        status_output.setBorder(BorderFactory.createLineBorder(Color.black));
        status_output.setLocation(0, 0);
        status_output.setSize(150, 150);

        info_modules.setBorder(BorderFactory.createLineBorder(Color.black));
        status_output.setLocation(160, 0);
        info_modules.setSize(150, 150);

        info_predicates.setBorder(BorderFactory.createLineBorder(Color.black));
        info_predicates.setLocation(310, 0);
        info_predicates.setSize(150, 150);

        loaded_modules.setBorder(BorderFactory.createLineBorder(Color.black));
        loaded_modules.setLocation(460, 0);
        loaded_modules.setSize(150, 150);

        loaded_predicates.setBorder(BorderFactory.createLineBorder(Color.black));
        loaded_predicates.setLocation(610, 0);
        loaded_predicates.setSize(150, 150);

        panel.setBackground(Color.cyan);

        panel.add(status_output);
        panel.add(info_modules);
        panel.add(info_modules);
        panel.add(loaded_modules);
        panel.add(loaded_predicates);
        panel.add(info_predicates);
        panel.add(add_file);
        panel.add(query_module);
        panel.add(query_predicate);

        fileList = new ArrayList<String>();
        this.add(panel);
    }

    public void setInfo_predicatesText(String t) {
        info_predicates.setText(t);
    }

    public void setInfo_modules(String t) {
        info_modules.setText(t);
    }

    public void setQuery_module(String t) {
        query_module.setText(t);
    }

    public void setQuery_predicate(String t) {
        query_predicate.setText(t);
    }

    public List<String> getFilePath() {
        return fileList;
    }

    public void setStatus_output(String t) {
        status_output.setText(t);
    }

    public void setAdd_fileListener(ActionListener listener) {
        add_file.addActionListener(listener);
    }

    public void displayErrorMessage(String errorMessage) {
        JOptionPane.showMessageDialog(this, errorMessage);
    }
}
