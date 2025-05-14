void insertion_sort(int const n, int * const p) {
    for (int i = 1; i < n; i++) {
        int const tmp = p[i];
        int j = i;
        while (j > 0 && p[j-1] > tmp) {
                p[j] = p[j-1];
                j--;
        }
        p[j] = tmp;
    }
}
