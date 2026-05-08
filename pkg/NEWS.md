---
engines:
- path: /home/delriaan/.positron-server/bin/ae29de6747f9f15cc38783410ec9a46a5b428393/quarto/share/extension-subtrees/julia-engine/\_extensions/julia-engine/julia-engine.js
toc-title: Table of contents
---

# book.of.features Version 0.1

## 0.1.4

### Updates

- **`bin.windows()`**:
  - Added explicit import of `book.of.utilities::`%bin%\`\`.
  - Simplified internal operation allowing R's default behavior
    regarding coercion from an atomic vector to the original data
    structure (e.g., vector, rank-N array, data.frame, etc.)

## 0.1.3.3

### Updates

- **`logic_map()`**:
  - Performance improvements:
    - Searching over each element of `bvec` (columnar operation) and
      comparing to the fixed vector `fvec`
    - Assembling detection routines into a sparse matrix
  - Removed unused argument `chatty`
  - Added argument `sparse` to indicate whether or not the result should
    be a sparse matrix
  - Added argument `progress` which is passed to `purrr::imap`

# book.of.features 0.1.3.2

## New Functionality

- Added `formulate()`

# book.of.features 0.1.3.1

## General Updates

- Formatting updates
