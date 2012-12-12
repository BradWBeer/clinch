(in-package #:pango)

(cffi:define-foreign-library :libpango
  (cffi-features:darwin ("libpango.dylib"))
  (cffi-features:unix (:or "libpango-1.0.so" "libpango-1.0.so.0"))
  (cffi-features:windows	"libpango.dll"))

(cffi:load-foreign-library :libpango)


(cffi:define-foreign-library :libpangocairo
  (cffi-features:darwin ("libpangocairo.dylib"))
  (cffi-features:unix (:or "libpangocairo-1.0.so" "libpangocairo-1.0.so.0"))
  (cffi-features:windows "libpangocairo.dll"))

(cffi:load-foreign-library :libpangocairo)
