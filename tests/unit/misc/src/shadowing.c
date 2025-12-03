int twice(int i) {
        return i*2;
}

void shadow(unsigned buffer_size, int buffer[]) {
        int i = 10;
        for (int i = 0, j = 0; i < buffer_size; i++, j+=3) {
                buffer[i] = twice(j);
                int i = 0;
        }
        if (buffer_size > 0) {
                buffer[0] = i;
        }
}
