void entry2(const unsigned buffer_size, int buffer[])
{
    int arr[3] = {1,2,3};
    int (*p_arr)[] = &arr;
    int x = (*p_arr)[0];
    buffer[0] = x;

    const int carr[3] = {1,2,3};
    const int (*p_carr)[] = &carr;
    int cx = (*p_carr)[0];
    buffer[1] = cx;
}