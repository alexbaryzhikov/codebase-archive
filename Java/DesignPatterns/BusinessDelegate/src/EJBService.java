public class EJBService implements BusinessService {

  @Override
  public void doProcessing() {
    System.out.println(("EJBService::doProcessing"));
  }
}
