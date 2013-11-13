package model;

import java.util.List;

/**
 * Author: alexander
 * Project: jpdep
 */
public class GenericOutput extends Element {
    List<String> output;

    public GenericOutput(List<String> output) {
        this.output = output;
    }

    public GenericOutput() {
    }

    public List<String> getOutput() {
        return output;
    }

    public void setOutput(List<String> output) {
        this.output = output;
    }

    @Override
    public String toString() {
        return "GenericOutput{" +
                "output=" + output +
                '}';
    }
}
