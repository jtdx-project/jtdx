#include <boost/crc.hpp>
#include <boost/config.hpp>

extern "C"
{
   short crc12 (unsigned char const * data, int length);
   bool crc12_check (unsigned char const * data, int length);
}

#define POLY 0xc06

#ifdef BOOST_NO_CXX11_CONSTEXPR
#define TRUNCATED_POLYNOMIAL POLY
#else
namespace
{
  unsigned long constexpr TRUNCATED_POLYNOMIAL = POLY;
}
#endif

// assumes CRC is last 16 bits of the data and is set to zero
// caller should assign the returned CRC into the message in big endian byte order
short crc12 (unsigned char const * data, int length)
{
    return boost::augmented_crc<12, TRUNCATED_POLYNOMIAL> (data, length);
}

bool crc12_check (unsigned char const * data, int length)
{
   return !boost::augmented_crc<12, TRUNCATED_POLYNOMIAL> (data, length);
}
