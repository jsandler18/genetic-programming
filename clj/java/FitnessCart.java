import clojure.java.api.Clojure;
import clojure.lang.*;

public class FitnessCart {

    public static void go() {
        String terminals = "'[x v -1]";
        String functions = "'[+ - * FitnessCart/safeDiv FitnessCart/gt FitnessCart/abs]";
        String argmap = "'[2 2 2 2 2 1]";
        IFn eval = Clojure.var("clojure.core", "eval");
        System.out.println("(just-do-it " + functions + " " + argmap + " " + terminals + " 50 6 #(FitnessCart/fitness %) 0 20)");
        System.out.println(eval.invoke(Clojure.read("(just-do-it " + functions + " " + argmap + " " + terminals + " 50 6 #(FitnessCart/fitness %) 0 20)")).toString());


    }

    public static Object fitness(Object program){
        IFn eval = Clojure.var("clojure.core", "eval");
        double timestep = .02; //time between calculations
        int timeout = 10; //time before the program is killed
        double time = 0.0; //the current time
        double bounds = .75; // the domain of x and v (-.75,.75)
        int cases = 20; //the number of cases to run the program on
        double sum = 0; //sum of results of all cases
        double captureRange = Math.sqrt(2 * (.05 * .05)); //the range that a capture is allowed in
        double distance = 0; // euclidean distance of x and v from capture
        double accel = .5; // the strength of the force

        for (int i = 0; i < cases; i++) {
            double v = (Math.random() * bounds * 2) - bounds; //gen random int between -bound and bound
            double x = (Math.random() * bounds * 2) - bounds;

            do {
                String pgm = "(let [x " + x + " v " + v +"] " + program.toString()+ ")"; //evaluate the program with the variables 
                double direction = Double.parseDouble(eval.invoke(Clojure.read(pgm)).toString());
                x += v * timestep; // x = vel * time
                v += timestep * accel * direction; // vel = accel * time
                time += timestep;
                distance = Math.sqrt(x * x + v * v); //get distance

            } while (distance > captureRange && time < timeout && distance < 100); //loop while not captured and under time limit and within reasonable distance

            if (distance >= 100) {
                time = 10;
            }
            sum += time;
            time = 0;
        }

        return sum;
    }

    /*performs division, but returns 1 on divide by 0*/
    public static double safeDiv(double num, double den) {
        return den == 0 ? 1 : num/den;
    }

    /*compares num1 to num2, returns positive if num1 > num2, negative otherwise*/
    public static double gt(double num1, double num2) {
        return num1 > num2 ? 1 : -1;
    }

    /*performs absolute value*/
    public static double abs(double num) {
        return num < 0 ? -num : num;
    }

}
