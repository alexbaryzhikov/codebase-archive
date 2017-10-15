public class AudioPlayer implements MediaPlayer {

  @Override
  public void play(String audioType, String fileName) {
    // Inbuilt support to play mp3 files
    if (audioType.equalsIgnoreCase("mp3")) {
      System.out.println("Playing mp3 file: " + fileName);

    } else if (audioType.equalsIgnoreCase("vlc")
        || audioType.equalsIgnoreCase("mp4")) {
      MediaAdapter mediaAdapter = new MediaAdapter(audioType);
      mediaAdapter.play(audioType, fileName);

    } else {
      System.out.println("Unsupported media type: " + audioType);
    }
  }
}
