import java.util.ArrayList;
import java.util.List;

public class Cache {

  private List<Service> services = new ArrayList<>();

  public Service getService(String serviceName) {
    for (Service service : services) {
      if (service.getName().equalsIgnoreCase(serviceName)) {
        System.out.println("Returning cached " + serviceName + " object");
        return service;
      }
    }
    return null;
  }

  public boolean addService(Service newService) {
    boolean exists = false;

    for (Service service : services) {
      if (service.getName().equalsIgnoreCase(newService.getName())) {
        exists = true;
        break;
      }
    }
    return exists || services.add(newService);
  }
}
