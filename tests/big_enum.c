//Should fail
//
//This fails because our translator assumes that enums will be
//represented with 32-bits, but they will use more in the case
//that some values are too large to fit into 32-bits.
enum E { A = 1000000000000 };
void entry(const unsigned int sz, int buffer[const]) {
        // this exercises both the explicitly initialized enum case and implicit one.
        enum E e[2] = {1};
}
