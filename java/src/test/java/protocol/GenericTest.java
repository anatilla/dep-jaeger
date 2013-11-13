package protocol;

import org.junit.Test;

import java.util.LinkedList;
import java.util.List;

/**
 * Author: alexander
 * Project: jpdep
 */
public class GenericTest {



    public static void main(String args[]){
        List<List<String>> clashes = new LinkedList<List<String>>();
        List<String> clash = new LinkedList<String>();

        clash.add("inthelex");
        clash.add("bathelex");
        clash.add("heading\\/0");
        clashes.add(clash);

        List<List<String>> list = manipulate_name_name_clashes(clashes);
        System.out.println(list.toString());

    }


    private static List<List<String>> manipulate_name_name_clashes(List<List<String>> clashes){

        if (clashes.isEmpty()) return new LinkedList<List<String>>();

        List<List<String>> name_clash_copy = clashes.subList(0, clashes.size());
        List<List<String>> new_list = new LinkedList<List<String>>();

        for(List<String> element : name_clash_copy){
            String method = element.get(element.size()-1);
            element.remove(element.size()-1);

            LinkedList<String> list = new LinkedList<String>();

            list.add(method);
            list.addAll(element);

            new_list.add(list);
        }
        return new_list;
    }
}
