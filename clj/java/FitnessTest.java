import clojure.java.api.Clojure;
import clojure.lang.*;

public class FitnessTest {
    public static Object fitness(Object program){
        IFn eval = Clojure.var("clojure.core", "eval");
        String pgm = "(let [x 100] " + program.toString()+ ")";
        return eval.invoke(Clojure.read(pgm));
    }

    public static long safeDiv(long num, long den) {
        return den == 0 ? 1 : num/den;
    }

}
