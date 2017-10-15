public class Football extends Game {

  @Override
  void onPrepare() {
    System.out.println("Football::onPrepare");
  }

  @Override
  void onStart() {
    System.out.println("Football::onStart");
  }

  @Override
  void onFinish() {
    System.out.println("Football::onFinish");
  }
}
