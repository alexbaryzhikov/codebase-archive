public class DecoratedWindow implements Window {

  /**
   * Reference to the window being decorated
   */
  private Window window;

  public DecoratedWindow(Window window) {
    this.window = window;
  }

  @Override
  public void renderWindow() {
    window.renderWindow();
  }
}
