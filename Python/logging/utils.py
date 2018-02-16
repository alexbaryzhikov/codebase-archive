import logging

def create_logger(name, log_file, level=logging.INFO):
  formatter = logging.Formatter(
    fmt='%(asctime)s %(name)s %(levelname)s: %(message)s',
    datefmt='%d.%m.%y %H:%M:%S')
  handler = logging.FileHandler(log_file)        
  handler.setFormatter(formatter)
  logger = logging.getLogger(name)
  logger.setLevel(level)
  if not logger.handlers:
      logger.addHandler(handler)
  return logger
