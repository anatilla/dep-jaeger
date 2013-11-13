package protocol;

import model.Module;
import model.Predicate;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * Author: alexander
 * Project: jpdep
 */
public class JPLFactoryTest {

    public void testPredicates() throws Exception {
        List<String> files = new ArrayList();
        files.add("/home/alexander/workspace/prolog/test_files/ferilli_files/clusterTax.pl");

        JPLFactory factory = new JPLFactory("/home/alexander/workspace/prolog/core/core.pl");

        factory.addFiles(files);

        String firstPredicate = "indice_davies_bouldin/3";
        System.out.println("PREDICATE FIRST CALL: requesting info on " + ElementType.PREDICATE + " "+ firstPredicate);

        Predicate el = (Predicate) factory.query(firstPredicate, ElementType.PREDICATE);
        System.out.println(el.toString());
//
        List<String> files2 = new ArrayList();
        files2.add("/home/alexander/workspace/prolog/test_files/ferilli_files/abduct.pl");

        JPLFactory factory2 = new JPLFactory("/home/alexander/workspace/prolog/core/core.pl");

        factory2.addFiles(files2);

        String secondPredicate = "trivial/2";
        System.out.println("PREDICATE SECOND CALL: requesting info on " + ElementType.PREDICATE + " "+ secondPredicate);

        Predicate el12 = (Predicate) factory2.query(secondPredicate, ElementType.PREDICATE);

        System.out.println(el12.toString());

        String modules = factory.sendCommand("modules", "X");
        System.out.println(modules);


    }


    public void testModule() throws Exception {
        List<String> files = new ArrayList();
        files.add("/home/alexander/workspace/prolog/test_files/ferilli_files/abd.pl");

        JPLFactory factory = new JPLFactory("/home/alexander/workspace/prolog/core/core.pl");

        factory.addFiles(files);

        String module = "clusterTax";
        System.out.println("MODULE FIRST CALL: requesting info on " + ElementType.MODULE + " " + module);
        Module module1 = (Module) factory.query(module, ElementType.MODULE);

        System.out.println(module1.toString());

    }
}
