import javax.swing.JFrame;
import javax.swing.JButton;
import javax.swing.JOptionPane;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class Swing_Example2 extends JFrame {
    private final JButton b = new JButton();

    public Swing_Example2() {
        super();
        this.setTitle("HelloApp");
        this.getContentPane().setLayout(null);
        this.setBounds(100, 100, 180, 140);
        this.add(makeButton());
        this.setVisible(true);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
    }

    private JButton makeButton() {
        b.setText("Click me!");
        b.setBounds(40, 40, 100, 30);
        b.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JOptionPane.showMessageDialog(b, "Hello World!");
            }
        });
        return b;
    }

    public static void main(String[] args) {
        new Swing_Example2();
    }
}
