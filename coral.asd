(asdf:defsystem coral
  :version "1.0.0"
  :author "Laurent Cimon <laurent@nilio.ca>"
  :maintainer "Laurent Cimon <laurent@nilio.ca>"
  :license "bsd-2-clause"
  :description "A game of TicTacToe made with McClim"
  :components ((:file "tictactoe")
	       (:file "ui"))
  :depends-on (#:mcclim)
  :build-operation "program-op"
  :build-pathname "coral"
  :entry-point "coral-ui:main")
