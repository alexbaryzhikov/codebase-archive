public class Cricket extends Game {

  @Override
  void onPrepare() {
    System.out.println("Cricket::onPrepare");
  }

  @Override
  void onStart() {
    System.out.println("Cricket::onStart");
  }

  @Override
  void onFinish() {
    System.out.println("Cricket::onFinish");
  }
}
