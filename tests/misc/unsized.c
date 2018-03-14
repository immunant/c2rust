//! skip_translation

struct Foo {
  int i;
  char unspec[];
};

int main(void) {
  struct Foo f1 = { 1, "hello" };

  printf("%s!", f1.unspec);
  printf("%d", sizeof(f1.unspec));
}
