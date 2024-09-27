# book.of.features 0.1.3.3

## Updates

- **`logic_map()`**: 
   - Performance improvements:
      - Searching over each element of `bvec` (columnar operation) and comparing to the fixed vector `fvec`
      - Assembling detection routines into a sparse matrix
    - Removed unused argument `chatty`
    - Added argument `sparse` to indicate whether or not the result should be a sparse matrix
    - Added argument `progress` which is passed to `purrr::imap`

# book.of.features 0.1.3.2

## New Functionality

- Added `formulate()`

# book.of.features 0.1.3.1

## General Updates

- Formatting updates
