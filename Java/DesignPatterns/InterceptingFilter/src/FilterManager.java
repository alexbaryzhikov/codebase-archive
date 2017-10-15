public class FilterManager {

  private FilterChain filterChain;

  public FilterManager(FilterChain filterChain) {
    this.filterChain = filterChain;
  }

  public void setFilter(Filter filter) {
    filterChain.addFilter(filter);
  }

  public void filterRequest(String request) {
    filterChain.applyFilters(request);
  }
}
