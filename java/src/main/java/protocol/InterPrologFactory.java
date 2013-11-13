package protocol;

import com.declarativa.interprolog.PrologEngine;
import com.declarativa.interprolog.YAPSubprocessEngine;
import controller.GlobalValues;
import exceptions.EmptySourceFileList;
import exceptions.NonValidQueryException;
import model.Element;

import java.io.File;
import java.util.List;

/**
 * Author: alexander
 * Project: jpdep
 */
public class InterPrologFactory extends ProtocolFactory {

    private static PrologEngine engine;

    public InterPrologFactory(String corePath, String yap_path) {

        engine = new YAPSubprocessEngine(yap_path, true);
        engine.consultAbsolute(new File(corePath));
    }

    @Override
    public void addFiles(List<String> sourceFiles) {
        if (sourceFiles.isEmpty()) throw new EmptySourceFileList("source files list cannot be empty.");

        // loading files
        String load = "load_all(['";
        for (String file : sourceFiles) {
            load += (file + "','");
        }

        load = load.substring(0, load.length() - 2);
        load += "])";

        engine.deterministicGoal(load);
    }

    @Override
    public boolean sendCommand(String command) {
        boolean res = engine.deterministicGoal(command);

        return res;
    }

    @Override
    public String sendCommand(String command, String param, String var) {
        String query = command + "(json, " + param + ", " + var + ")";

        String result = engine.deterministicGoal(query, "[string(" + var + ")]")[0].toString();

        return result;
    }

    @Override
    public String sendCommand(String command, String var) {
        String query = command + "(json, " + var + ")";
        String result = engine.deterministicGoal(query, "[string(" + var + ")]")[0].toString();

        return result;
    }

    @Override
    public Element query(String query, ElementType elementType) throws Exception {
        if (query.isEmpty()) throw new NonValidQueryException("query is empty.");
        if (elementType.equals(ElementType.GENERIC))
            throw new NonValidQueryException("Please, use sendCommand method instead of query.");

        Element element = null;

        if (elementType.equals(ElementType.PREDICATE)) {
//          Object[] q_module = engine.deterministicGoal(GlobalValues.declared_in + "(" + query + ", Module, _)", "[string(Module)]");

//          String module = q_module[0].toString();
            String filePath = engine.deterministicGoal(GlobalValues.json_predicate_query + "(" + query + ", X)", "[string(X)]")[0].toString();

            element = readJson(filePath, elementType);

        } else if (elementType.equals(ElementType.MODULE)) {
            // here begins module part

            String filePath = engine.deterministicGoal(GlobalValues.json_module_query + "(" + query + ",X)", "[string(X)]")[0].toString();

            element = readJson(filePath, elementType);
        }

        return element;
    }
}
