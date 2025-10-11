int static_single_int = (int) { 42 };
int *static_single_int_ptr = &((int) { 42 });
int *static_int_array_ptr = (int[]) { 42, 9001 };
//char *static_char_ptr = (char[]) { "hello" };

void local_compound_literals() {
    int single_int = (int) { 42 };
    int *single_int_ptr = &((int) { 42 });
    int *int_array_ptr = (int[]) { 42, 9001 };
    char *char_ptr = (char[]) { "hello" };
}
