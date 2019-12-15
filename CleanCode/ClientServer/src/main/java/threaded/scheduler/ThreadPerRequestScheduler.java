package threaded.scheduler;

import threaded.ClientRequestProcessor;
import threaded.ClientScheduler;

public class ThreadPerRequestScheduler implements ClientScheduler {

    @Override
    public void schedule(ClientRequestProcessor clientRequestProcessor) {
        new Thread(clientRequestProcessor::process).start();
    }
}