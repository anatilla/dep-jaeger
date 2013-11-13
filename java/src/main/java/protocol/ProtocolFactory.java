package protocol;

import model.Element;
import protocol.serializer.JsonSerializer;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * Author: alexander
 * Project: jpdep
 */
public abstract class ProtocolFactory {

    public static Element readJson(String filePath, ElementType elementType) throws IOException {

        Element el = null;
        File tmp_file = null;

        if (elementType.equals(ElementType.PREDICATE)) {

            String outputFile = filePath.replace("'", "");

            tmp_file = new File(outputFile);
            el = JsonSerializer.deserializePredicate(tmp_file);


        } else if (elementType.equals(ElementType.MODULE)) {

            String outputFile = filePath.replace("'", "");

            tmp_file = new File(outputFile);
            el = JsonSerializer.deserializeModule(tmp_file);

        } else if (elementType.equals(ElementType.GENERIC)) {
            String outputFile = filePath.replace("'", "");

            tmp_file = new File(outputFile);
            el = JsonSerializer.deserializeGeneric(tmp_file);
        }

        // delete tmp file
        tmp_file.delete();
        return el;
    }

    public abstract void addFiles(List<String> files);

    public abstract boolean sendCommand(String command);

    public abstract String sendCommand(String command, String param, String var);

    public abstract String sendCommand(String command, String var);

    public abstract Element query(String query, ElementType elementType) throws Exception;
}
