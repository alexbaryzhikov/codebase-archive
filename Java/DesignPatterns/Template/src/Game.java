public abstract class Game {

  /**
   * Template method
   */
  public final void play() {
    onPrepare();
    onStart();
    onFinish();
  }

  abstract void onPrepare();
  abstract void onStart();
  abstract void onFinish();
}
