(defsystem #:stitch
  :version "0.1.0"
  :description "Audio stitcher for patterns"
  :author "Abhinav Tushar <abhinav@lepisma.xyz>"
  :license "GPLv3"
  :depends-on (#:alexandria
               #:cl-arrows
               #:cl-cut
               #:cl-interpol
               #:yason
               #:cl-slice
               #:cl-strings
               #:cl-wav
               #:serapeum
               #:unix-opts)
  :components ((:file "package")
               (:file "stitch")))
