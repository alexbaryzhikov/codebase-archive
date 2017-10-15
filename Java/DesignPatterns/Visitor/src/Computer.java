import java.util.ArrayList;
import java.util.List;

/**
 * Composite component
 */
public class Computer implements Component {

  private String name;
  private List<Component> components;

  public Computer(String name, double cpuFrequency, double memSize, double hddSize) {
    this.name = name;
    components = new ArrayList<>();
    components.add(new Cpu(cpuFrequency));
    components.add(new Memory(memSize));
    components.add(new Motherboard());
    components.add(new Hdd(hddSize));
  }

  public String getName() {
    return name;
  }

  @Override
  public void accept(Visitor visitor) {
    visitor.visit(this);
    // Visit components
    for (Component c : components) {
      c.accept(visitor);
    }
  }
}
