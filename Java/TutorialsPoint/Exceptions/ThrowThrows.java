import java.io.*;

public class ThrowThrows {
    /* kick exception down the stack */
    public void deposit(double amount) throws RemoteException {
        /* invoke exception explicitly */
        throw new RemoteException();
    }

    /* kick multiple exceptions down the stack */
    public void withdraw(double amount) throws RemoteException, 
    InsufficientFundsException {
    }

    public static void main(String[] args) {
    }

}
