import java.util.List;

public interface Dao<T> {
  boolean add(T item);
  void update(T item);
  void delete(int itemId);
  T get(int itemId);
  List<T> getAll();
}
