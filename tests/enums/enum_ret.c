
enum Color { Red = 1, Green = 2, Blue = 3 };

// Enum in return type
enum Color red(void) {
  enum Color bar = Red;
  return bar;
}

void entry2(const unsigned buffer_size, int buffer[]) {
  if (buffer_size < 5) { return; }

  buffer[0] = red();
  buffer[1] = Red + 1;

  // Checking equality of enum
  enum Color b = Blue;
  if (b == Blue) {
    buffer[2] = -1;
  }

  {
    // Shadowing enum
    enum Color { Purple = 1, Yellow, Green };

    // Comparing values from different enums
    buffer[3] = Purple;
    if (Purple == Red) {
      buffer[4] = -2;
    }

    // Assign wrong enum types
    enum Color c = Red;
    buffer[5] = c;
  }

  // Integral values not in the initial enum can still be assigned.
  enum Color invalid = 6;
  buffer[6] = invalid;
}
