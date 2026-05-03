typedef enum { Foo0, Foo1, Foo2, Foo3 } Foo;
enum Bar { BarN1 = -1, Bar0, Bar1, Bar2, Bar3 };

#define FOO1_MACRO Foo1
#define BAR1_MACRO Bar1

void test_enums(void) {
    Foo foo = Foo0;
    enum Bar bar = Bar0;
    foo = Foo1;
    bar = BarN1;

    // Assign enum constant of wrong type
    foo = Bar0;
    bar = Foo0;

    // Assign integer that matches a constant
    foo = 1;
    bar = 1;

    // Assign integer that doesn't match any constant
    foo = 3;
    bar = 3;

    // Arithmetic
    foo -= 2;
    bar -= 2;

    Foo e = Foo1;

    // Compare to same type
    int enum_enum = e == foo;
    int enum_constant = e == Foo0;

    // Compare to wrong type
    int wrong_enum_enum = e == bar;
    int wrong_enum_constant = e == Bar0;

    switch (foo) {
        case Foo0:
            break;

        case FOO1_MACRO:
            break;

        // Integer with matching variant
        case 2:
            break;

        // Wrong type
        case Bar3:
            break;

        // No matching variant
        case 42:
            break;

        // No matching variant, negative on unsigned enum
        case -42:
            break;
    }

    switch (bar) {
        case Bar0:
            break;

        case BAR1_MACRO:
            break;

        // Integer with matching variant
        case 2:
            break;

        // Wrong type
        case Foo3:
            break;

        // Negative variant
        case BarN1:
            break;

        // No matching variant
        case 42:
            break;

        // No matching variant, negative on signed enum
        case -42:
            break;
    }
}
