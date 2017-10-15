public class Client {

  private BusinessDelegate delegate;

  public Client(BusinessDelegate delegate) {
    this.delegate = delegate;
  }

  public BusinessDelegate getDelegate() {
    return delegate;
  }

  public void doTask() {
    delegate.doTask();
  }
}
