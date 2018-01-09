void entry(const unsigned buffer_size, int buffer[const]) {

        if (buffer_size < 30) return;
        int i = 0;

        // Test function pointer comparisons
        void(* const f0)(unsigned, int*) = 0;
        void(* const f1)(unsigned, int*) = entry;
        if (f0) { buffer[i++] = 1; }
        if (f1) { buffer[i++] = 1; }

        if (!f0) { buffer[i++] = 1; }
        if (!f1) { buffer[i++] = 1; }

        if (0.0) { buffer[i++] = 1; }
        if (1.0) { buffer[i++] = 1; }

        if (0) { buffer[i++] = 1; }
        if (1) { buffer[i++] = 1; }

        void *p0 = 0;
        const void *p1 = &buffer_size;

        if (p0) { buffer[i++] = 1; }
        if (p1) { buffer[i++] = 1; }

        // Test simplifiable conditions
        if (0 == 0) { buffer[i++] = 1; }
        if (p1 && 0 > -3) { buffer[i++] = 1; }

        if (!p0) { buffer[i++] = 1; }
        if (!p1) { buffer[i++] = 1; }

        if (p0 && !p0) { buffer[i++] = 1; }
        if (p0 || !p0) { buffer[i++] = 1; }

        if (p1 && !p1) { buffer[i++] = 1; }
        if (p1 || !p1) { buffer[i++] = 1; }
}
