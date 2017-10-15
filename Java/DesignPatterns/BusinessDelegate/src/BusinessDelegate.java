public class BusinessDelegate {

  private BusinessLookup lookup = new BusinessLookup();
  private String serviceType;

  public void setServiceType(String serviceType) {
    this.serviceType = serviceType;
  }

  public void doTask() {
    lookup.getBusinessService(serviceType).doProcessing();
  }
}
