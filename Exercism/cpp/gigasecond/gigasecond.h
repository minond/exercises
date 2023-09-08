#include "boost/date_time/posix_time/posix_time.hpp"

using namespace boost::posix_time;

namespace gigasecond {

const seconds GIGASECOND = seconds(1000000000);

ptime advance(ptime t) {
  return t + GIGASECOND;
}

};
