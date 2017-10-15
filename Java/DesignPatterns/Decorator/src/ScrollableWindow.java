public class ScrollableWindow extends DecoratedWindow {

  /** Additional state */
  private Object scrollBar;

  public ScrollableWindow(Window window) {
    super(window);
  }

  @Override
  public void renderWindow() {
    super.renderWindow();
    renderScrollBar();
  }

  private void renderScrollBar() {
    scrollBar = new Object();
    System.out.println("ScrollableWindow::renderScrollBar");
  }
}
