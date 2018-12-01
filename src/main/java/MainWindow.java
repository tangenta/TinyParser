import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.*;
import java.nio.file.Files;
import java.util.stream.Collectors;

public class MainWindow {
    private JEditorPane mEditorPane1;
    private JPanel mPanel1;
    private JButton mQuitButton;
    private JButton mOpenButton;
    private JButton mSaveAsButton;
    private JButton mGenerateTreeButton;
    private JButton mViewSourceButton;

    private JFileChooser mFileChooser = new JFileChooser();
    private String sourceCode = "";
    private String parseResult = "";

    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel");
        } catch (Exception e) {}

        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Tiny parser");
            frame.setContentPane(new MainWindow().mPanel1);
            frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
            Dimension dimension = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
            frame.setSize(dimension.width / 2, dimension.height / 2);
            frame.setVisible(true);
        });
    }

    public MainWindow() {
        mEditorPane1.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 14));
        hottify();
        mOpenButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                int returnVal = mFileChooser.showOpenDialog(mPanel1);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    File file = mFileChooser.getSelectedFile();
                    try {
                        String text = Files.lines(file.toPath()).collect(Collectors.joining(System.lineSeparator()));
                        mEditorPane1.setText(text);
                    } catch (IOException e1) {
                        e1.printStackTrace();
                    }
                }
            }
        });
        mSaveAsButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                int returnVal = mFileChooser.showSaveDialog(mPanel1);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    File file = mFileChooser.getSelectedFile();
                    try (Writer writer = new BufferedWriter(new FileWriter(file))) {
                        writer.write(mEditorPane1.getText());
                    } catch (IOException e1) {
                        e1.printStackTrace();
                    }
                }
            }
        });
        mGenerateTreeButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                sourceCode = mEditorPane1.getText();
                parseResult = tiny.Parser.parseIntoTree(sourceCode);
                mEditorPane1.setText(parseResult);
            }
        });
        mViewSourceButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                mEditorPane1.setText(sourceCode);
            }
        });
        mQuitButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                System.exit(0);
            }
        });
    }

    void hottify() {
        tiny.Parser.parseIntoTree("test := test");
    }
}
