#!/bin/sh
if [ ! -d ~/.emacs.d ]; then
	mkdir ~/.emacs.d
fi
	
if [ ! -d ~/.emacs.d/backup ]; then
    echo "creating backup directory"
    mkdir ~/.emacs.d/backup
else
    echo "backup directory already exists"
fi

rsync -avz --progress --bwlimit=1M rsync://elpa.gnu.org/elpa/ ~/.emacs.d/elpa

rsync -avz --progress --bwlimit=1M rsync://melpa.org/packages/ ~/.emacs.d/melpa 
