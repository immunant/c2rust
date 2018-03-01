enum E1 { A = -1, B = 3000000000 }; // uses 64-bit
enum E2 { C = 3000000000 }; // uses 32-bit
enum E3 { D = 30000000000 }; // uses 64-bit
void entry(const unsigned int sz, int buffer[const]) {
        // this exercises both the explicitly initialized enum case and implicit one.
        enum E1 e1[2] = {1};
        enum E2 e2[2] = {1};
        enum E3 e3[2] = {1};
}
