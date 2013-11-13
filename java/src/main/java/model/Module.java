package model;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.util.LinkedList;
import java.util.List;


@JsonIgnoreProperties(ignoreUnknown = true)

public class Module extends Element {

    private String module_name;
    private List<String> unused_imports;
    private List<String> used_imports;
    private List<String> used_files;
    private List<List<String>> clashes;
    private List<List<String>> real_clashes;
    private List<String> real_taxonomy;
    private List<String> public_predicates;
    private String file_path;

    public Module(List<List<String>> clashes, List<List<String>> real_clashes, String file_path, String module_name, List<String> public_predicates, List<String> real_taxonomy, List<String> unused_imports, List<String> used_files, List<String> used_imports) {
        this.clashes = clashes;
        this.file_path = file_path;
        this.real_clashes = real_clashes;
        this.module_name = module_name;
        this.public_predicates = public_predicates;
        this.real_taxonomy = real_taxonomy;
        this.unused_imports = unused_imports;
        this.used_files = used_files;
        this.used_imports = used_imports;
    }

    public Module() {
    }



    /**
     * metodo di supporto che riporta in testa il predicato, per una maggiore leggibilit?.
     * niente di interessante..
     *
     * @return
     */

    private static List<List<String>> manipulate_name_name_clashes(List<List<String>> clashes) {

        if (clashes.isEmpty())
            return new LinkedList<List<String>>();

        List<List<String>> name_clash_copy = clashes.subList(0, clashes.size());
        List<List<String>> new_list = new LinkedList<List<String>>();

        for (List<String> element : name_clash_copy) {
            String method = element.get(element.size() - 1);
            method = method.replace("\\", "");

            element.remove(element.size() - 1);

            LinkedList<String> list = new LinkedList<String>();


            list.add(method);
            list.addAll(element);

            new_list.add(list);
        }
        return new_list;
    }

    public List<List<String>> getReal_clashes() {
        return real_clashes;
    }

    public void setReal_clashes(List<List<String>> real_clashes) {
        this.real_clashes = real_clashes;
    }

    public List<String> getPublic_predicates() {
        return public_predicates;
    }

    public void setPublic_predicates(List<String> public_predicates) {
        this.public_predicates = public_predicates;
    }

    public String getFile_path() {
        return file_path;
    }

    public void setFile_path(String file_path) {
        this.file_path = file_path;
    }

    public List<List<String>> getClashes() {
        return clashes;
    }

    public void setClashes(List<List<String>> name_name_clashes) {
        this.clashes = name_name_clashes;
    }

    public String getModule_name() {
        return module_name;
    }

    public void setModule_name(String module_name) {
        this.module_name = module_name;
    }

    public List<String> getUnused_imports() {
        return unused_imports;
    }

    public void setUnused_imports(List<String> unused_imports) {
        this.unused_imports = unused_imports;
    }

    public List<String> getUsed_imports() {
        return used_imports;
    }

    public void setUsed_imports(List<String> used_imports) {
        this.used_imports = used_imports;
    }

    public List<String> getUsed_files() {
        return used_files;
    }

    public void setUsed_files(List<String> used_files) {
        this.used_files = used_files;
    }

    public List<String> getReal_taxonomy() {
        return real_taxonomy;
    }

    public void setReal_taxonomy(List<String> real_taxonomy) {
        this.real_taxonomy = real_taxonomy;
    }

    @Override
    public String toString() {
        LinkedList<String> ppreds = new LinkedList<String>();
        for (String pred : public_predicates){
            pred = pred.replace("\\", "");
            ppreds.add(pred);

        }

        return "module_name = " + module_name +
                "\n unused_imports = " + unused_imports +
                "\n used_imports = " + used_imports +
                "\n used_files = " + used_files +
                "\n all_imports = " + real_taxonomy +
                "\n public_predicates = " + ppreds +
                "\n file_path = " + file_path +
                "\n potential clashes = " + manipulate_name_name_clashes(clashes) +
                "\n real clashes = " + manipulate_name_name_clashes(real_clashes);
    }

}

