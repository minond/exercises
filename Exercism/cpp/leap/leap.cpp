namespace leap {

bool is_leap_year(unsigned short year) {
  return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
}

};
