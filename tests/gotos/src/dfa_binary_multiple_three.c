// It is straightforward to draw a DFA that accepts only ('\0'-terminated)
// binary strings that correspond to binary numbers divisible by 3. This is a
// direct translation of that.
int multiple_three(char const *binary_string) {

  goto no_remainder; // I'm just being explicit about the starting state :P

  // number-so-far = 0 (mod 3) 
no_remainder:
  switch (*binary_string) {
    case '\0': return 1;
    case '0':  ++binary_string; goto no_remainder;
    case '1':  ++binary_string; goto remainder_one;
    default:   return 2;
  }

  // number-so-far = 1 (mod 3) 
remainder_one:
  switch (*binary_string) {
    case '\0': return 0;
    case '0':  ++binary_string; goto remainder_two;
    case '1':  ++binary_string; goto no_remainder;
    default:   return 2;
  }

  // number-so-far = 2 (mod 3)
remainder_two:
  switch (*binary_string) {
    case '\0': return 0;
    case '0':  ++binary_string; goto remainder_one;
    case '1':  ++binary_string; goto remainder_two;
    default:   return 2;
  }
}
