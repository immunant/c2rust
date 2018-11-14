#include "FloatingLexer.hpp"

#include <iostream>
using namespace std;

int main(int argc, char *argv[]) {
        if (argc > 1) {
                cout << matchFloatingLiteral(argv[1]) << endl;
                return 0;
        }
        return 1;
}
