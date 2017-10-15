public class FactoryBuilder {
  public static AbstractFactory getFactory(int typeId) {
    switch (typeId) {
      case AbstractFactory.MALEVICH:
        return new MalevichFactory();
      case AbstractFactory.KANDINSKY:
        return new KandinskyFactory();
      default:
        throw new IllegalArgumentException("Unknown factory type ID: " + typeId);
    }
  }
}
