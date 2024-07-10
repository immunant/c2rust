#include <stdint.h>
#include <stdlib.h>

typedef struct buffer {
    uint8_t* data;
    size_t len;
    size_t cap;
} buffer;

buffer* buffer_new(size_t cap) {
    buffer* buf = malloc(sizeof(buffer));
    if (cap == 0) {
        buf->data = NULL;
    } else {
        buf->data = malloc(cap);
    }
    buf->len = 0;
    buf->cap = cap;
    return buf;
}

void buffer_delete(buffer* buf) {
    if (buf->data != NULL) {
        free(buf->data);
    }
    free(buf);
}

void buffer_realloc(buffer* buf, size_t new_cap) {
    if (new_cap == buf->cap) {
        return;
    }
    if (buf->cap == 0) {
        buf->data = malloc(new_cap);
    } else if (new_cap == 0) {
        free(buf->data);
        buf->data = NULL;
    } else {
        buf->data = realloc(buf->data, new_cap);
    }
    buf->cap = new_cap;
    if (buf->len > new_cap) {
        buf->len = new_cap;
    }
}

void buffer_push(buffer* buf, uint8_t byte) {
    if (buf->len == buf->cap) {
        if (buf->cap == 0) {
            buffer_realloc(buf, 4);
        } else {
            buffer_realloc(buf, buf->cap * 2);
        }
    }
    ((uint8_t*)buf->data)[buf->len] = byte;
    ++buf->len;
}

void test_buffer() {
    buffer* buf = buffer_new(3);
    for (int i = 0; i < 10; ++i) {
        buffer_push(buf, i);
    }
    buffer_delete(buf);
}

int main() {
    test_buffer();
}
