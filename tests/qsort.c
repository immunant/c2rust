
void swap(int* a, int* b)
{
    int t = *a;
    *a = *b;
    *b = t;
}

int partition (int arr[], int low, int high)
{
    int pivot = arr[high];
    int i = low - 1;

    for (int j = low; j <= high - 1; j++) {
        if (arr[j] <= pivot) {
            i++;
            swap(&arr[i], &arr[j]);
        }
    }
    swap(&arr[i + 1], &arr[high]);
    return i + 1;
}

void quickSort(int arr[], int low, int high)
{
    if (low < high) {
        int i = partition(arr, low, high);
        quickSort(arr, low, i - 1);
        quickSort(arr, i + 1, high);
    }
}

void entry(unsigned buffer_size, int buffer[])
{
    if (buffer_size >= 10) {
        buffer[0] = 6;
        buffer[1] = 1;
        buffer[2] = 5;
        buffer[3] = 6;
        buffer[4] = 2;
        buffer[5] = 0;
        buffer[6] = 9;
        buffer[7] = 2;
        buffer[9] = 5;
        
        quickSort(buffer, 0, 10);
    }
}

