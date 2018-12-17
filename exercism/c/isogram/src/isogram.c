#include "isogram.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

bool is_isogram(const char phrase[]) {
  if (phrase == NULL) {
    return false;
  }

  int len = strlen(phrase);
  bool found[END_CHAR_CODE - START_CHAR_CODE];
  for (int i = 0; i < END_CHAR_CODE - START_CHAR_CODE; i++) {
    found[i] = false;
  }

  for (int i = 0; i < len; i++) {
    char code = phrase[i];

    // We deal in lowercase ASCII codes, so convert upper case to lower case
    if (code >= UPPER_START_CHAR_CODE && code <= UPPER_END_CHAR_CODE) {
      code += START_CHAR_CODE - UPPER_START_CHAR_CODE;
    }

    // Ignore codes that fall outside of the english alphabet letters
    if (code < START_CHAR_CODE || code > END_CHAR_CODE) {
      continue;
    }

    int offset = (int)code - START_CHAR_CODE;
    if (found[offset] == true) {
      return false;
    }

    found[offset] = true;
  }

  return true;
}
