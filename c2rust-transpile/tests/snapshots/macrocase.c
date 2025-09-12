// based on zstd_decompress.c
#define ZSTD_d_format ZSTD_d_experimentalParam1

typedef enum {
	ZSTD_d_windowLogMax=100,
	ZSTD_d_experimentalParam1=1000,
} ZSTD_dParameter;

int ZSTD_dParam_getBounds(ZSTD_dParameter dParam)
{
    int bounds = 0;
    switch(dParam) {
        case ZSTD_d_windowLogMax:
            bounds = 1;
            return bounds;
        case ZSTD_d_format:
            bounds = 5;
            return bounds;
        default:;
    }
    return bounds;
}
