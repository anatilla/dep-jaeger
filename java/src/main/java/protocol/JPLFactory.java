package protocol;

import controller.GlobalValues;
import exceptions.EmptySourceFileList;
import exceptions.NonValidQueryException;
import jpl.JPL;
import jpl.JPLException;
import jpl.PrologException;
import jpl.Query;
import model.Element;

import java.util.List;

/**
 * Author: alexander
 * Project: jpdep
 */
@Deprecated
public class JPLFactory extends ProtocolFactory {

    private boolean already_active;

    public JPLFactory(String prologPath) {

        if (prologPath.isEmpty()) throw new NonValidQueryException("prolog source path is empty.");

        // initializing JPL
        already_active = JPL.init();

        String consult = "['" + prologPath + "']";
        Query q1 = new Query(consult);

        if (!q1.hasSolution()) throw new JPLException("Problem consulting file " + prologPath);
        q1.oneSolution();
    }

    public void addFiles(List<String> sourceFiles) {

        if (sourceFiles.isEmpty()) throw new EmptySourceFileList("source files list cannot be empty.");

        // loading files
        String load = "load_all(['";
        for (String file : sourceFiles) {
            load += (file + "','");
        }

        load = load.substring(0, load.length() - 2);
        load += "])";

        Query q_load = new Query(load);

        try {
            q_load.oneSolution();
        } catch (PrologException e) {
            e.printStackTrace();
        }
    }

    public boolean sendCommand(String command) {

        Query query = new Query(command);

        return query.hasSolution();
    }

    public String sendCommand(String command, String param, String var) {

        Query query = new Query(command + "(json, " + param + "," + var + ")");

        return query.oneSolution().get(var).toString();
    }

    public String sendCommand(String command, String var) {
        Query query = new Query(command + "(json," + var + ")");

        return query.oneSolution().get(var).toString();
    }

    public Element query(String query, ElementType elementType) throws Exception {

        if (query.isEmpty()) throw new NonValidQueryException("query is empty.");
        if (elementType.equals(ElementType.GENERIC))
            throw new NonValidQueryException("Please, use sendCommand method instead of query.");

        Element element = null;
        Query q;

        if (elementType.equals(ElementType.PREDICATE)) {
            Query q_module = new Query(GlobalValues.declared_in + "(" + query + ", Module, _)");
            String module = q_module.oneSolution().get("Module").toString();

            String q_string = GlobalValues.json_predicate_query + "(" + query + "," + module + ",X)";
            q = new Query(q_string);

            String filePath = q.oneSolution().get("X").toString();

            element = readJson(filePath, elementType);

        } else if (elementType.equals(ElementType.MODULE)) {
            // here begins module part

            String q_string = GlobalValues.json_module_query + "(" + query + ",X)";
            q = new Query(q_string);

            String filePath = q.oneSolution().get("X").toString();

            element = readJson(filePath, elementType);
        }

        return element;
    }
}

