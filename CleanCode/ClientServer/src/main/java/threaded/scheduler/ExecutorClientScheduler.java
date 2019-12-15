package threaded.scheduler;

import threaded.ClientRequestProcessor;
import threaded.ClientScheduler;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class ExecutorClientScheduler implements ClientScheduler {
    private final ExecutorService executor;

    public ExecutorClientScheduler(int threadCount) {
        this.executor = Executors.newFixedThreadPool(threadCount);
    }

    @Override
    public void schedule(ClientRequestProcessor clientRequestProcessor) {
        executor.submit(clientRequestProcessor::process);
    }
}