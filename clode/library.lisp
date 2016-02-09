(in-package #:clode)

(cffi:define-foreign-library ode
  (:unix (:or "libode.so" "libode.dylib"))
  (:windows "ode.dll")
  (t (:default "ode")))

(cffi:use-foreign-library ode)
