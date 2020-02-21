#ifndef PIMPL_IMPL_HPP_
#define PIMPL_IMPL_HPP_

#include <utility>

template<typename T>
pimpl<T>::pimpl () : m_ {new T {}} {}

template<typename T>
template<typename ...Args>
pimpl<T>::pimpl (Args&& ...args) : m_ {new T {std::forward<Args> (args)...} } {}

template<typename T>
pimpl<T>::~pimpl () {}

template<typename T>
T * pimpl<T>::operator -> () {return m_.get ();}

template<typename T>
T const * pimpl<T>::operator -> () const {return m_.get ();}

template<typename T>
T& pimpl<T>::operator * () {return *m_.get ();}

template<typename T>
T const& pimpl<T>::operator * () const {return *m_.get ();}

#endif
