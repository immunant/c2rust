#include <alloca.h>

void use_arrays(const int n, const int m, int a[const][n*n][m]) {

        int counter = 1;

        for (int i = 0; i < 2; i++) {
                for (int j = 0; j < n*n; j++) {
                        for (int k = 0; k < m; k++) {
                                a[i][j][k] = counter++;
                        }
                }
        }

        // Test that we can get the address of the element past the end of the
        // array
        int *past_end = &a[2][n*n][m];
}

/* Same thing as use_arrays, but using addition and deref */
void use_arrays2(const int n, const int m, int a[const][n*n][m]) {

        int counter = 1;

        for (int i = 0; i < 2; i++) {
                for (int j = 0; j < n*n; j++) {
                        for (int k = 0; k < m; k++) {
                                *(*(j+*(a+i))+k) = counter++;
                        }
                }
        }

}

void variable_arrays(int buf[const]) {

        int grid[4][4][5];

        use_arrays(2,5,grid);
        use_arrays(2,5,grid+2);

        int counter = 0;

        for (int i = 0; i < 4; i++) {
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

        // Test that we can get the address of the element past the end of the
        // array
        int *past_end = &grid[4][4][5];
        past_end = &var1[n];
}

/* Same as variable_arrays but using an alloca */
void alloca_arrays(int buf[const]) {

        int grid[4][4][5];

        use_arrays(2,5,grid);
        use_arrays(2,5,grid+2);

        int counter = 0;

        for (int i = 0; i < 4; i++) {
                for (int j = 0; j < 4; j++) {
                        for (int k = 0; k < 5; k++) {
                                buf[counter++] = grid[i][j][k];
                        }
                }
        }

        int n = 8;
        int* var1 = alloca(sizeof(int)*n);

        for (int i = 0; i < n; i++) {
                var1[i] = 3*i;
                buf[counter++] = var1[i];
        }

        // Test that we can get the address of the element past the end of the
        // array
        int *past_end = &grid[4][4][5];
        past_end = &var1[n];
}
