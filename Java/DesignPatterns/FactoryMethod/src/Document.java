public interface Document {

  // Type IDs
  int TXT = 0;
  int HTML = 1;
  int PDF = 2;

  void open();
  void save();
  void close();
}
