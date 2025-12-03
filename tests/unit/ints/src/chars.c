int multibyte_chars(const unsigned sz, int buffer[]) {
  int i = 0;

  // Test unicode character literals
  buffer[i++] = u'✓';
  buffer[i++] = U'😱';
  buffer[i++] = L'😱';

  buffer[i++] = '\x00';
  buffer[i++] = '\x01';
  buffer[i++] = '\xff';

  return i;
}
