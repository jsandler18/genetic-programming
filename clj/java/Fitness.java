import clojure.java.api.Clojure;
import clojure.lang.*;

public class Fitness {
    public static Object fitness(String program){
        new RT();
        return clojure.lang.Compiler.load(new java.io.StringReader("(do (def x 100) (eval " + program + " ))"));
    }
}
