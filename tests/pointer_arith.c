// fails in generated rust code
void entry(const unsigned sz, int buf[const]) {
        for (int *cursor = buf; cursor < buf + sz; cursor += 1) {
                *cursor = 1;
        }
}
