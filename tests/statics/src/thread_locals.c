
static int gssi = 17;
extern int gesi;
static __thread gsti = 37;
extern __thread geti;

void thread_entry(const unsigned buffer_size, int buffer[]) {
    if (buffer_size < 16) return;

    static int fssi = 53;
    extern int fesi;
    static __thread fsti = 59;
    extern __thread feti;

    int i = 0;
#define ADD_VAR(x)  do { buffer[i++] = (x); (x) += 1337; buffer[i++] = (x); } while (0)
    ADD_VAR(gssi);
    ADD_VAR(gesi);
    ADD_VAR(gsti);
    ADD_VAR(geti);
    ADD_VAR(fssi);
    ADD_VAR(fesi);
    ADD_VAR(fsti);
    ADD_VAR(feti);
}
