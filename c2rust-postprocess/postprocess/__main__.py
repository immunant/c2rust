import logging

from postprocess import main

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        logging.warning("Interrupted by user, terminating...")
        raise
