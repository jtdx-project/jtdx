#ifndef PIMPL_H_HPP_
#define PIMPL_H_HPP_

#include <memory>

//
// opaque implementation type with perfect forwarding of constructor arguments
//
// see pimpl_impl.hpp for the out-of-line definitions of the members and lifetime management
//
// thanks to Herb Sutter (http://herbsutter.com/gotw/_101/) for the implementation
//
template<typename T>
class pimpl
{
private:
  std::unique_ptr<T> m_;

public:
  pimpl ();
  template<typename ...Args> pimpl (Args&& ...);
  ~pimpl ();
  T * operator -> ();
  T const * operator -> () const;
  T& operator * ();
  T const& operator * () const;
};

#endif
