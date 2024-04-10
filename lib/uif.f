(
    IMAGES ----------------------------------------------------------------------

    QOI is nice, but full 24-bit colour plus alpha is actually a lot of bits, and sometimes it's nice to muck about with uncompressed images in a buffer.

    Enter UIF: Useful Image Format. 8 bit colour, using RGB332. Small but more than capable for simple images.
)

(
    header:
    0   magic number (75 69 66 30 uif0)
    4   width (16 bits)
    6   height (16 bits)
    8   image data (width * height bytes)
)


