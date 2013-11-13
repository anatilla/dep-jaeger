package core;

import model.GenericOutput;
import model.Predicate;
import protocol.ElementType;
import protocol.JPLFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Author: alexander
 * Project: jpdep
 */
public class MainTest {
    private static JPLFactory factory = new JPLFactory("/home/alexander/workspace/prolog/core/core.pl");

    public static void main(String[] args) throws Exception {
        List<String> files = new ArrayList();
        files.add("/home/alexander/workspace/prolog/test_files/ferilli_files/clusterTax.pl");

        factory.addFiles(files);

        String firstPredicate = "indice_davies_bouldin/3";
        System.out.println("PREDICATE FIRST CALL: requesting info on " + ElementType.PREDICATE + " " + firstPredicate);

        Predicate el = (Predicate) factory.query(firstPredicate, ElementType.PREDICATE);
        System.out.println(el.toString());

        List<String> files2 = new ArrayList();
        files2.add("/home/alexander/workspace/prolog/test_files/ferilli_files/abduct.pl");

        factory.addFiles(files2);

        String secondPredicate = "trivial/2";
        System.out.println("PREDICATE SECOND CALL: requesting info on " + ElementType.PREDICATE + " " + secondPredicate);

        Predicate el12 = (Predicate) factory.query(secondPredicate, ElementType.PREDICATE);

        System.out.println(el12.toString());

        String modules = factory.sendCommand("modules", "X");
        GenericOutput output = (GenericOutput) JPLFactory.readJson(modules, ElementType.GENERIC);

        System.out.println(output);
    }
}
