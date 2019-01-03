(defsystem "progui"
  :description "Programmatically control the mouse and keyboard."
  :version "0.0.1"
  :author "Mitchell Chung"
  :license "MIT"
  :depends-on ("cffi")
  :pathname "src/"
  :serial t
  :components
  ((:file "package")
   (:file "progui-windows" :if-feature :os-windows)
   (:file "progui-unimplemented" :if-feature (:not :os-windows))
   (:file "progui")))
