//! enable_relooper
//
void use_arrays(const int n, const int m, int a[const][n*n][m]) {

        int counter = 1;

        for (int i = 0; i < 2; i++) {
                for (int j = 0; j < n*n; j++) {
                        for (int k = 0; k < m; k++) {
                                a[i][j][k] = counter++;
                        }
                }
        }

}

void variable_arrays(int buf[const]) {

        int grid[2][4][5];

        use_arrays(2,5,grid);

        int counter = 0;

        for (int i = 0; i < 2; i++) {
                for (int j = 0; j < 4; j++) {
                        for (int k = 0; k < 5; k++) {
                                buf[counter++] = grid[i][j][k];
                        }
                }
        }

        int n = 8;
        int var1[n];

        for (int i = 0; i < n; i++) {
                var1[i] = 3*i;
                buf[counter++] = var1[i];
        }
}
