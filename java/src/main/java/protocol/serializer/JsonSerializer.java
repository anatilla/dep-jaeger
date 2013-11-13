package protocol.serializer;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.ObjectMapper;
import model.GenericOutput;
import model.Module;
import model.Predicate;

import java.io.File;
import java.io.IOException;

/**
 * Author: alexander
 * Project: jpdep
 */

public class JsonSerializer {


    public static Predicate deserializePredicate(File jsonFile) throws IOException {
        ObjectMapper mapper = new ObjectMapper();

        mapper.configure(JsonParser.Feature.ALLOW_SINGLE_QUOTES, true);
        mapper.configure(JsonParser.Feature.ALLOW_BACKSLASH_ESCAPING_ANY_CHARACTER, true);

        Predicate element = mapper.readValue(jsonFile, Predicate.class);

        return element;
    }

    public static Module deserializeModule(File jsonFile) throws IOException {
        ObjectMapper mapper = new ObjectMapper();

        mapper.configure(JsonParser.Feature.ALLOW_SINGLE_QUOTES, true);
        mapper.configure(JsonParser.Feature.ALLOW_BACKSLASH_ESCAPING_ANY_CHARACTER, true);

        Module element = mapper.readValue(jsonFile, Module.class);

        return element;
    }

    public static GenericOutput deserializeGeneric(File jsonFile) throws IOException {
        ObjectMapper mapper = new ObjectMapper();

        mapper.configure(JsonParser.Feature.ALLOW_SINGLE_QUOTES, true);
        mapper.configure(JsonParser.Feature.ALLOW_BACKSLASH_ESCAPING_ANY_CHARACTER, true);

        GenericOutput element = mapper.readValue(jsonFile, GenericOutput.class);

        return element;
    }

}
