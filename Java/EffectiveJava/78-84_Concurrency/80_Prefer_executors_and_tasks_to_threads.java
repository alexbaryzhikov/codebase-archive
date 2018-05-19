ExecutorService executor = Executors.newSingleThreadExecutor();

executor.execute(runnable);

executor.shutdown();
