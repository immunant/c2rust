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
}
