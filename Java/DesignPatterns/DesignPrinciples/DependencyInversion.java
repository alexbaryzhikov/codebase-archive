/*
- High-level modules should not depend on low-level modules. Both should depend on
  abstractions.
- Abstractions should not depend on details. Details should depend on abstractions.
*/

public class DependencyInversion {
    public static void main(String[] args) {
    }
}

/* ---------------------------------------------------------------------------- */
/* Dependency Inversion Principle - Bad example
   - We have to change the Manager class.
   - Some of the current functionality from the manager class might be affected.
   - The unit testing should be redone.
*/

class Manager_ {
    Worker_ worker;

    public void setWorker(Worker_ w) {
        worker = w;
    }

    public void manage() {
        worker.work();
    }
}

class Worker_ {
    public void work() {
        /* ....working */
    }
}

class SuperWorker_ {
    public void work() {
        /* .... working much more */
    }
}

/* ---------------------------------------------------------------------------- */
/* Dependency Inversion Principle - Good example
   - Manager class doesn't require changes when adding SuperWorkers.
   - Minimized risk to affect old functionality present in Manager class since we don't change it.
   - No need to redo the unit testing for Manager class.
*/

class Manager {
    IWorker worker;

    public void setWorker(IWorker w) {
        worker = w;
    }

    public void manage() {
        worker.work();
    }
}

interface IWorker {
    public void work();
}

class Worker implements IWorker{
    public void work() {
        /* ....working */
    }
}

class SuperWorker  implements IWorker{
    public void work() {
        /* .... working much more */
    }
}
