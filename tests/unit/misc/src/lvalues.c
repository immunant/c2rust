
int* foo(int *j) { return ++j; }
int bar(int k) { return ++k; }

// This tests more complex lvalues when the result is not used
void lvalue(int buffer[])
{
    // Direct (Path)
    int n = 5;
    n = 6;

    // Pointer
    int *p = buffer;
    *p = 8;                // buffer[0]
    *(p + 1) = 9;          // buffer[1] 
    *(foo(p + 1)) = 3;     // buffer[2]

    // Array index
    int arr[1][1] = { 3 };
    buffer[3] = n;         // buffer[3]
    arr[0][0] = ++n;
    buffer[4] = arr[0][0]; // buffer[4]
    buffer[bar(4)] = -8;   // buffer[5]
}
