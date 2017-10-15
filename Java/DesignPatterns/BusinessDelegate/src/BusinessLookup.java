public class BusinessLookup {

  public BusinessService getBusinessService(String serviceType) {
    if (serviceType.equalsIgnoreCase("EJB")) {
      return new EJBService();
    } else if (serviceType.equalsIgnoreCase("JMS")) {
      return new JMSService();
    } else {
      throw new IllegalArgumentException("Invalid service type: " + serviceType);
    }
  }
}
