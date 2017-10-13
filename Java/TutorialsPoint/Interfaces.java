public class Interfaces {

    public static void main(String[] args) {
        Mammal m = new Mammal();
        m.eat();
        m.travel();
    }
}

/* Implement interface */

interface Animal {
    void eat();
    void travel();
}

class Mammal implements Animal {

    public void eat() {
        System.out.println("Mammal eats");
    }

    public void travel() {
        System.out.println("Mammal travels");
    } 

    public int noOfLegs() {
        return 0;
    }
}

/* Extend interface */

interface Sports {
   void setHomeTeam(String name);
   void setVisitingTeam(String name);
}

interface Football extends Sports {
   void homeTeamScored(int points);
   void visitingTeamScored(int points);
   void endOfQuarter(int quarter);
}

interface Hockey extends Sports {
   void homeGoalScored();
   void visitingGoalScored();
   void endOfPeriod(int period);
   void overtimePeriod(int ot);
}
