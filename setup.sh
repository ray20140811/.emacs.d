#!/bin/sh
if [ ! -d ~/.emacs.d/backup-list ]; then
    echo "creating backup-list directory"
    mkdir ~/.emacs.d/backup-list
else
    echo "backup-list directory already exists"
fi

rsync -avz --progress --bwlimit=1M rsync://elpa.gnu.org/elpa/ ~/.emacs.d/elpa
rsync -avz --progress --bwlimit=1M rsync://melpa.org/packages/ ~/.emacs.d/melpa 