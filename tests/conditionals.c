void entry(const unsigned buffer_size, int buffer[const]) {

        if (buffer_size < 10) return;

        // Test function pointer comparisons
        void(* const f0)(unsigned, int*) = 0;
        void(* const f1)(unsigned, int*) = entry;
        if (f0) { buffer[0] = 1; }
        if (f1) { buffer[1] = 1; }

        if (0.0) { buffer[2] = 1; }
        if (1.0) { buffer[3] = 1; }

        if (0) { buffer[4] = 1; }
        if (1) { buffer[5] = 1; }

        void *p0 = 0;
        const void *p1 = &buffer_size;

        if (p0) { buffer[6] = 1; }
        if (p1) { buffer[7] = 1; }
}
