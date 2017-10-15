/* Clients should not be forced to depend upon interfaces that they don't use. */

public class InterfaceSegregation {
    public static void main(String[] args) {
    }
}

/* ---------------------------------------------------------------------------- */
/* Interface Segregation Principle - Bad example
   - Polluted IWorker interface.
*/

interface IWorker {
    public void work();
    public void eat();
}

class Worker_ implements IWorker{
    public void work() {
        /* ....working */
    }

    public void eat() {
        /* ...... eating at launch break */
    }
}

class SuperWorker_ implements IWorker{
    public void work() {
        /* .... working much more */
    }

    public void eat() {
        /* .... eating at launch break */
    }
}

class Manager_ {
    IWorker worker;

    public void setWorker(IWorker w) {
        worker = w;
    }

    public void manage() {
        worker.work();
    }
}

/* ---------------------------------------------------------------------------- */
/* Interface Segregation Principle - Good example */

interface IWorkable {
    public void work();
}

interface IFeedable{
    public void eat();
}

class Worker implements IWorkable, IFeedable {
    public void work() {
        /* ....working */
    }

    public void eat() {
        /* .... eating in launch break */
    }
}

class Robot implements IWorkable{
    public void work() {
        /* ....working */
    }
}

class SuperWorker implements IWorkable, IFeedable{
    public void work() {
        /* .... working much more */
    }

    public void eat() {
        /* .... eating in launch break */
    }
}

class Manager {
    IWorkable worker;

    public void setWorker(IWorkable w) {
        worker = w;
    }

    public void manage() {
        worker.work();
    }
}
