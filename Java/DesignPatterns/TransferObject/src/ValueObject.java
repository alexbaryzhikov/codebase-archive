import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

public class ValueObject implements Serializable {

  private Map<String, String> stringValues = new HashMap<>();
  private Map<String, Integer> integerValues = new HashMap<>();

  public void putString(String key, String value) {
    stringValues.put(key, value);
  }

  public String getString(String key) {
    return stringValues.get(key);
  }

  public void putInteger(String key, Integer value) {
    integerValues.put(key, value);
  }

  public Integer getInteger(String key) {
    return integerValues.get(key);
  }
}
