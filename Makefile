CURRENT_DIRECTORY = $(PWD)

all:
	rsync -a --delete $(CURRENT_DIRECTORY)/. ~/.emacs.d
