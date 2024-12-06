
typedef enum {
    b=2
} a;

void entry6(const unsigned buffer_size, int buffer[]){
  if (buffer_size < 1) { return; }
  
  // Using a compound literal with enum to assign the value
  buffer[0] = (a){b};

}
