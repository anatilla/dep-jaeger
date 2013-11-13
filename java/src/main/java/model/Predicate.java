package model;

import java.util.LinkedList;
import java.util.List;

public class Predicate extends Element {

    private String predicate_name;
    private Integer num_clauses;
    private Integer arity;
    private String decl_in;
    private String decl_in_file;
    private List<String> uses;
    private List<String> used_by;
    private List<String> uses_modules;
    private String is_public;

    public Predicate() {
    }

    public Predicate(Integer arity, Integer num_clauses, String decl_in, String decl_in_file, String is_public, String predicate_name, List<String> used_by, List<String> uses, List<String> uses_modules) {
        this.arity = arity;
        this.num_clauses = num_clauses;
        this.decl_in = decl_in;
        this.decl_in_file = decl_in_file;
        this.is_public = is_public;
        this.predicate_name = predicate_name;
        this.used_by = used_by;
        this.uses = uses;
        this.uses_modules = uses_modules;
    }


    public Integer getNum_clauses() {
        return num_clauses;
    }

    public void setNum_clauses(Integer num_clauses) {
        this.num_clauses = num_clauses;
    }

    public String getIs_public() {
        return is_public;
    }

    public void setIs_public(String is_public) {
        this.is_public = is_public;
    }

    public String getPredicate_name() {
        return predicate_name;
    }

    public void setPredicate_name(String predicate_name) {
        this.predicate_name = predicate_name;
    }

    public Integer getArity() {
        return arity;
    }

    public void setArity(Integer arity) {
        this.arity = arity;
    }

    public String getDecl_in() {
        return decl_in;
    }

    public void setDecl_in(String decl_in) {
        this.decl_in = decl_in;
    }

    public String getDecl_in_file() {
        return decl_in_file;
    }

    public void setDecl_in_file(String decl_in_file) {
        this.decl_in_file = decl_in_file;
    }

    public List<String> getUses() {
        return uses;
    }

    public void setUses(List<String> uses) {
        this.uses = uses;
    }

    public List<String> getUsed_by() {
        return used_by;
    }

    public void setUsed_by(List<String> used_by) {
        this.used_by = used_by;
    }

    public List<String> getUses_modules() {
        return uses_modules;
    }

    public void setUses_modules(List<String> uses_modules) {
        this.uses_modules = uses_modules;
    }

    @Override
    public String toString() {
        LinkedList<String> usesDef = new LinkedList<String>();
        LinkedList<String> usedByDef = new LinkedList<String>();
        for (String uso : uses) {
            uso = uso.replace("\\", "");
            usesDef.add(uso);
        }

        for (String usato : used_by) {
            usato = usato.replace("\\", "");
            usedByDef.add(usato);
        }



        return "predicate_name = " + predicate_name + "/"+ arity +
                "\n  num_clauses = " + num_clauses +
                "\n  is_public = " + is_public +
                "\n  declared in module = " + decl_in +
                "\n  declared in file = " + decl_in_file +
                "\n  uses = " + usesDef +
                "\n  used by = " + usedByDef +
                "\n  uses modules = " + uses_modules;

    }
}