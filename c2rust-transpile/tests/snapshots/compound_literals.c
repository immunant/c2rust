int static_single_int = (int) { 42 };
int *static_single_int_ptr = &((int) { 42 });
int *static_int_ptr_to_array = (int[]) { 42, 9001 };
// Currently generates broken Rust code, see
// https://github.com/immunant/c2rust/issues/1410
//char *static_char_ptr_to_array = (char[]) { "hello" };
int (*static_int_array_ptr)[2] = &(int[]) { 42, 9001 };
// As above
//char (*static_char_array_ptr)[6] = &(char[]) { "hello" };

#define SINGLE_INT ((int) { 42 })
#define INT_ARRAY ((int[]) { 42, 9001 })
#define CHAR_ARRAY ((char[]) { "hello" })

void local_compound_literals() {
    int single_int = (int) { 42 };
    int *single_int_ptr = &((int) { 42 });
    int *int_ptr_to_array = (int[]) { 42, 9001 };
    char *char_ptr_to_array = (char[]) { "hello" };
    int (*int_array_ptr)[2] = &(int[]) { 42, 9001 };
    char (*char_array_ptr)[6] = &(char[]) { "hello" };

    int macro_single_int = SINGLE_INT;
    int *macro_single_int_ptr = &SINGLE_INT;
    int *macro_int_ptr_to_array = INT_ARRAY;
    char *macro_char_ptr_to_array = CHAR_ARRAY;
    int (*macro_int_array_ptr)[2] = &INT_ARRAY;
    char (*macro_char_array_ptr)[6] = &CHAR_ARRAY;
}
