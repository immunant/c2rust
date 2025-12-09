// From #301.

typedef struct {
    int len;
} Foo;

void dec(Foo* f) {
    f->len--;
}

int main() {
    Foo f = {5};
    Foo *fp = &f;
    dec(fp);
    f.len = 6;
    dec(fp);
}
