// TODO: test extern variables (whose definition is not here)

// forward decl
static int baz(void);
extern int baz(void);

// External function declaration
extern int main(void);

// External static global definition
const int visible_everywhere = 9;

// Internal static global definition
static int counter;
extern int counter;

// Internal static local definition
int baz(void) {
  static int k = 0;
  counter++;
  return k + 1;
}

typedef struct {
  char string[3];
} StringStruct;
typedef union StringUnion {
  char string[3];
  int data;
} StringUnion;

// Issue similar to GH #83: Propagate is_static attribute
static char const ab_month_name[12][4] =
{
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};
static const char *hello = "hello";

void entry(const unsigned buffer_size, int buffer[]) {

    if (buffer_size < 11) return;
    static const char *world = "world";

    // GH #83: Previously static structs&unions would not propagate the
    // is_static attribute down to their string array fields
    static StringStruct Foo = {
      "foo",
    };
    static StringUnion Bar = {
      .string="foo",
    };

    buffer[0] = baz();
    buffer[1] = baz();
    buffer[2] = baz() + 1;
    buffer[baz()] = 4;


    buffer[7] = counter;
    counter--;
    baz();
    buffer[8] = counter;

    buffer[9] = hello[0];
    buffer[10] = world[1];
}
