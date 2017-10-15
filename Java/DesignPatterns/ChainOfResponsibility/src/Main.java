/* --------------------------------------------------------
 * Chain of Responsibility
 * --------------------------------------------------------
 *
 * Intent
 * - Avoids attaching the sender of a request to its receiver, giving other objects the possibility
 *   of handling the request
 * - Objects become parts of a chain and the request is sent from one object to another across the
 *   chain until one of the objects will handle it
 *
 * Applicability
 *
 * - More than one object can handle a command
 * - The handler is not known in advance
 * - The handler should be determined automatically
 * - Request is addressed to a group of objects without explicitly specifying its receiver
 * - A group of objects that may handle the command must be specified in a dynamic way
 */

public class Main {
  public static void main(String[] args) {
    Logger loggerChain = getLoggerChain();

    loggerChain.onMessageReceive(new Message(Logger.INFO, "Simple message"));
    loggerChain.onMessageReceive(new Message(Logger.DEBUG, "Debug message"));
    loggerChain.onMessageReceive(new Message(Logger.ERROR, "Error message"));
    loggerChain.onMessageReceive(new Message(-1, "Unhandled message"));
  }

  private static Logger getLoggerChain() {
    Logger errorLogger = new ErrorLogger();
    Logger fileLogger = new FileLogger();
    Logger consoleLogger = new ConsoleLogger();

    errorLogger.setNextLogger(fileLogger);
    fileLogger.setNextLogger(consoleLogger);

    return errorLogger;
  }
}
