import clojure.java.api.Clojure;
import clojure.lang.*;

public class Fitness {
    public static Object fitness(Object program){
        IFn eval = Clojure.var("clojure.core", "eval");
        IFn type = Clojure.var("clojure.core", "type");
        String pgm = "(let [x 100] " + program.toString()+ ")";
        return eval.invoke(Clojure.read(pgm));
    }
}
//(eval (cons 'let (cons '[x 100] (list %))))
