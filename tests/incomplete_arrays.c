

void entry(const unsigned buffer_size, int buffer[])
{
    int arr[3] = {1,2,3};
    int (*p_arr)[] = &arr;
    int x = (*p_arr)[0];
}

