import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class Fitness {
    public static Object fitness(Object program){
        IFn eval = Clojure.var("clojure.core", "eval");
        return eval.invoke(program);
    }
}
