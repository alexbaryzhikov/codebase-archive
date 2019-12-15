package threaded;

public interface ClientScheduler {
    void schedule(ClientRequestProcessor clientRequestProcessor);
}