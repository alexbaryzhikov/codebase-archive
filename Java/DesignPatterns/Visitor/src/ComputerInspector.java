public class ComputerInspector implements Visitor {

  @Override
  public void visit(Computer computer) {
    System.out.println("Computer name: " + computer.getName());
  }

  @Override
  public void visit(Cpu cpu) {
    System.out.println("CPU frequency: " + cpu.getFrequencyGHz() + " GHz");
  }

  @Override
  public void visit(Memory memory) {
    System.out.println("Memory size: " + memory.getSizeGB() + " GB");
  }

  @Override
  public void visit(Motherboard motherboard) {
    System.out.println("Motherboard status: " + motherboard.getStatus());
  }

  @Override
  public void visit(Hdd hdd) {
    System.out.println("HDD size: " + hdd.getSizeTB() + " TB");
  }
}
