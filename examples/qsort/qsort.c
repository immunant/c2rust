void swap(int* a, int* b)
{
    int t = *a;
    *a = *b;
    *b = t;
}

/* 
 * Lomuto Partition Scheme:
 * Partitions the array so that elements < pivot are on the left, 
 * and elements >= pivot are on the right.
 */
int partition (int arr[], int low, int high)
{
    // Partition the subarray around the last element as pivot and return pivot's final index.
    int pivot = arr[high];
    int i = low - 1;

    for (int j = low; j <= high - 1; j++) {
        if (arr[j] <= pivot) {
            i++;
            // Move elements <= pivot into the left partition.
            swap(&arr[i], &arr[j]);
        }
    }
    // Place pivot just after the final element of the left partition.
    swap(&arr[i + 1], &arr[high]);
    return i + 1;
}

void quickSort(int arr[], int low, int high)
{
    if (low < high) {
        /* pi is the partitioning index; arr[pi] is now at the right place */
        int pi = partition(arr, low, high);

        /* Recursively sort elements before and after partition */
        quickSort(arr, low, pi - 1);
        quickSort(arr, pi + 1, high);
    }
}
