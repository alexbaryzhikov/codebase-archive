public interface Visitor {
  void visit(Computer visitable);
  void visit(Cpu visitable);
  void visit(Memory visitable);
  void visit(Motherboard visitable);
  void visit(Hdd visitable);
}
