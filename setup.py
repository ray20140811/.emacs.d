#!/usr/bin/python3
#-*- coding:utf-8 -*-
import os

home = os.path.expanduser('~')

def check_folder(folder_name):
    if not os.path.exists(folder_name):
        os.makedirs(folder_name)
        print(f'create folder: {folder_name}')
    else:
        print(f'{folder_name} already exists')

def check_backup_folder():
    if os.name == "nt":
        base_folder_name = os.path.join(home, 'AppData', 'Roaming' ,'.emacs.d')
    else:
        base_folder_name = os.path.join(home, '.emacs.d')

    check_folder(base_folder_name)

    backup_folder_name = os.path.join(base_folder_name, 'backup')
    check_folder(backup_folder_name)

if __name__ == '__main__':
    check_backup_folder()
