void entry(const unsigned buffer_size, int buffer[const])
{
    int arr[1][1] = { 1 };

    arr[0][0] += 9;

    int i = 0;

    char abc[] = "abc";
    buffer[i++] = abc[0];
    buffer[i++] = abc[1];
    buffer[i++] = abc[2];
    buffer[i++] = abc[3];

    char def[] = {'d','e','f'};
    buffer[i++] = def[0];
    buffer[i++] = def[1];
    buffer[i++] = def[2];

    char part[2] = {1};
    buffer[i++] = part[0];
    buffer[i++] = part[1];

    char *abcptr = "abc";
    buffer[i++] = abcptr[0];
    buffer[i++] = abcptr[1];
    buffer[i++] = abcptr[2];
    buffer[i++] = abcptr[3];

    char init[] = {"test"};
    buffer[i++] = init[0];
    buffer[i++] = init[1];
    buffer[i++] = init[2];
    buffer[i++] = init[3];

    char too_small[2] = "long";
    buffer[i++] = too_small[0];
    buffer[i++] = too_small[1];
}
