// Should fail

void entry(unsigned buffer_size, int buffer[])
{
    if (buffer_size >= 10) {
        int i = 0;
        
        // Test break
        while (1) {
            if (i > 7) break;
            buffer[i] = 1;
        }

        // Test continue
        while (1) {
            if (i > 2) continue;
            buffer[i] = 2;
        }
    }
}

