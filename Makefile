install:
	find $(pwd) -maxdepth 1 ! -path $(pwd) ! -name "Makefile" -exec ln -sf {} ~/ \;
