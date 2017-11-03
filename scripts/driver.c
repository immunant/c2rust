/* This is the driver program for the 'test_translator.py' script.
 * 
 * It is linked once against the initial C testcase and once against the
 * static Rust library obtained by compiling the translated source.
 */

#include <stdlib.h>
#include <stdio.h>

void entry(unsigned, int*);

int main(void)
{  
  // Allocate zero initialized buffer
  unsigned buffer_size = 1024;
  int *buffer = calloc(buffer_size, sizeof(int));
  
  // Call out to the entry point
  entry(buffer_size, buffer);

  // Print out the new buffer
  for (int i = 0; i < buffer_size; i++) {
    printf("%d: %d\n", i, buffer[i]);
  }

  // Free the buffer
  free(buffer);
}
