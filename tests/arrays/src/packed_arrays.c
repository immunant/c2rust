/* testcase from https://github.com/immunant/c2rust/issues/381 */

typedef struct __attribute__((aligned(4))) {
    int waiter;
} event_queue_t;

void event_queues_init(event_queue_t *queues)
{
    queues[0].waiter = 42;
}
