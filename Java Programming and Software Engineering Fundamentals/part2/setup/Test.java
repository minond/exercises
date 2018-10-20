import edu.duke.*;

/**
 * Write a description of Test here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class Test {
    public static void main(String[] args) {
        FileResource f = new FileResource("./Test.java");
        for (String line : f.lines()) {
            System.out.println(line);
        }
    }
}
