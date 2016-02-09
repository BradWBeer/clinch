(in-package #:FreeImage)

(cffi:define-foreign-library :libFreeImage
  (cffi-features:darwin "libpango.dylib")
  (cffi-features:unix (:or "libfreeimage.so"))
  (cffi-features:windows "libfreeimage.dll"))

(cffi:load-foreign-library :libFreeImage)
