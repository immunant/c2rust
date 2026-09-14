/* A translation unit may contain comments without any declarations. */

#ifdef DISABLED_FEATURE
int feature(void) { return 42; }
#endif

/* Keep multiple comments to exercise source location sorting. */
