void entry(unsigned buffer_size, int buffer[])
{
    if (buffer_size >= 70) {
        int i = 0;

        // Test break
        while (1) {
            if (i > 7) break;
            buffer[i++] = 1;
        }

        // Test do/while
        do {
            buffer[i++] = 2;
        } while (i <= 15);

        // Test do/while with break
        do {
            buffer[i++] = 3;
            if (i > 20) break;
        } while (1);

        // Test while with break
        while (1) {
            buffer[i++] = 4;
            if (i < 30) {
                continue;
            } else {
                break;
            }
        }

        // Test continue
        do {
            buffer[i++] = 5;
            if (i < 40) continue;
        } while (0);

        // Test for
        for (i = 40; i < 50; i++) {
                i++;
                buffer[i] = 6;
        }

        // Test continue, ensure it runs the increment
        for (i = 50; i < 70; i++) {
                i++;
                if (i < 55) continue;
                if (i > 65) break;
                buffer[i] = 7;
        }
    }
}

