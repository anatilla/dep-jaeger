/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package view;

/**
 * @author nomak
 */
public class notificationPopUp extends javax.swing.JDialog {

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JLabel jLabel1;

    /**
     * Creates new form notificationPopUp
     */
    public notificationPopUp(String intestazione, String messaggio) {
        initComponents();

        jLabel1.setText(messaggio);
        this.setTitle(intestazione);
//        this.setSize((jLabel1.getWidth() + 100), (jLabel1.getHeight() + 100));
        this.pack();
//        jButton1.setLocation((this.getWidth()/2),jButton1.getY());
        this.setLocationRelativeTo(null);
        this.setVisible(true);

//        this.setSize(300,125);
//        this.setUndecorated(true);
//        this.setLayout(new GridBagLayout());
//        GridBagConstraints constraints = new GridBagConstraints();
//        constraints.gridx = 0;
//        constraints.gridy = 0;
//        constraints.weightx = 1.0f;
//        constraints.weighty = 1.0f;
//        constraints.insets = new Insets(5, 5, 5, 5);
//        constraints.fill = GridBagConstraints.BOTH;
////        JLabel headingLabel = new JLabel(header);
////        headingLabel.setIcon(); // --- use image icon you want to be as heading image.
////        headingLabel.setOpaque(false);
////        this.add(headingLabel, constraints);
//        constraints.gridx++;
//        constraints.weightx = 0f;
//        constraints.weighty = 0f;
//        constraints.fill = GridBagConstraints.NONE;
//        constraints.anchor = GridBagConstraints.NORTH;
//
//        JButton closeButton = new JButton(new AbstractAction("OK") {
//            @Override
//            public void actionPerformed(final ActionEvent e) {
//                   ((JFrame)e.getSource()).dispose();
//            }
//        });
//
//        closeButton.setMargin(new Insets(1, 4, 1, 4));
//        closeButton.setFocusable(false);
//        this.add(closeButton, constraints);
//        constraints.gridx = 0;
//        constraints.gridy++;
//        constraints.weightx = 1.0f;
//        constraints.weighty = 1.0f;
//        constraints.insets = new Insets(5, 5, 5, 5);
//        constraints.fill = GridBagConstraints.BOTH;
//        JLabel messageLabel = new JLabel(message);
//        this.add(messageLabel, constraints);
//        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
//        this.setVisible(true);
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jLabel1 = new javax.swing.JLabel();
        jButton1 = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setBackground(new java.awt.Color(204, 255, 204));
        setModal(true);
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jLabel1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel1.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 1, true));
        jLabel1.setMaximumSize(getMaximumSize());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.ipadx = 211;
        gridBagConstraints.ipady = 59;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.ABOVE_BASELINE;
        gridBagConstraints.insets = new java.awt.Insets(18, 15, 0, 15);
        getContentPane().add(jLabel1, gridBagConstraints);

        jButton1.setText("OK");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        getContentPane().add(jButton1, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        this.dispose();
    }//GEN-LAST:event_jButton1ActionPerformed
    // End of variables declaration//GEN-END:variables
}