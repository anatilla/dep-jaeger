package core;

import controller.MainController;
import view.MainInterface;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Author: alexander
 * Project: jpdep
 */
public class MainMVC {

    public static void main(String[] args) {
        if (args.length < 1) {
            print_usage_and_exit();
        }

//        if (args[0].isEmpty() || args[1].isEmpty()) {
        if (args[0].isEmpty()) {
            print_usage_and_exit();
        }

        MainInterface view = new MainInterface();

//        String pathToYap = getPathToYap();
//        String prologCore = "prolog/core/core.pl";
        File currentJavaJarFile = new File(MainMVC.class.getProtectionDomain().getCodeSource().getLocation().getPath());
        String currentJavaJarFilePath = currentJavaJarFile.getAbsolutePath();
        String jar_path = currentJavaJarFilePath.replace(currentJavaJarFile.getName(), "");

        jar_path += "core/core.pl";

        MainController controller = new MainController(view, args[0], jar_path);
//        MainController controller = new MainController(view, args[0], "prolog/core/core.pl");
//        MainController controller = new MainController(view, args[0], args[1]);

        view.setVisible(true);

    }

    private static String getPathToYap() {
        String path = null;
        String[] commands = {"/bin/sh", "-c", "which yap"};
        try {
//            Process proc = Runtime.getRuntime().exec("which yap");
            Process proc = new ProcessBuilder(commands).start();
            BufferedReader Input = new BufferedReader(new
                    InputStreamReader(proc.getInputStream()));

            BufferedReader Error = new BufferedReader(new
                    InputStreamReader(proc.getErrorStream()));

            // read the output from the command
//            System.out.println("Here is the standard output of the command:\n");
            while ((path = Input.readLine()) != null) {
                System.out.println(path);
            }

            // read any errors from the attempted command
//            System.out.println("Here is the standard error of the command (if any):\n");
            while ((path = Error.readLine()) != null) {
                System.out.println(path);
            }
        } catch (IOException ex) {
            Logger.getLogger(MainMVC.class.getName()).log(Level.SEVERE, null, ex);
        }
        return path;
    }

    private static void print_usage_and_exit() {
//        System.out.println("usage: <path\\to\\yap> <path\\to\\core.pl>");
        System.out.println("usage: java jarname.jar <path\\to\\yap>");
        System.exit(1);
    }
}
