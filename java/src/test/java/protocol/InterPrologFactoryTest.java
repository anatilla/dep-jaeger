package protocol;

import model.Element;
import model.Module;
import model.Predicate;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * Author: alexander
 * Project: jpdep
 */
public class InterPrologFactoryTest {
//    @Test
    public void testAddFiles() throws Exception {
        List<String> files = new ArrayList();
        files.add("/home/alexander/workspace/prolog/test_files/folle/folle.pl");

        InterPrologFactory factory = new InterPrologFactory("/home/alexander/workspace/prolog/core/core.pl", "/usr/bin/yap");

        factory.addFiles(files);

        Element element = factory.query("folle", ElementType.MODULE);

        System.out.println();
        System.out.println(((Module) element).toString());
    }
}
