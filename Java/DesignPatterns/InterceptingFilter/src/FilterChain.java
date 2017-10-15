import java.util.ArrayList;
import java.util.List;

public class FilterChain {

  private List<Filter> filters = new ArrayList<>();
  private Target target;

  public FilterChain(Target target) {
    this.target = target;
  }

  public boolean addFilter(Filter filter) {
    return filters.add(filter);
  }

  public void setTarget(Target target) {
    this.target = target;
  }

  public void applyFilters(String request) {
    for (Filter filter : filters) {
      filter.apply(request);
    }
    target.execute(request);
  }
}
