#!/usr/bin/python3
#-*- coding:utf-8 -*-
import os
import datetime
import argparse

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

def backup_file(file_name):
    if os.name == "nt":
        source_file_name = os.path.join(home, 'AppData', 'Roaming', file_name)
    else:
        source_file_name = os.path.join(home, file_name)
    if os.path.exists(source_file_name):
        timestamp = datetime.datetime.now().strftime('%Y%m%d%H%M%S')
        new_file_name = f'{source_file_name}_{timestamp}'
        os.rename(source_file_name, new_file_name)
        print(f'backup {source_file_name} to {new_file_name}')

def check_file():
    backup_file(os.path.join('.emacs.d', 'init.el'))
    backup_file('.emacs')

def copy_file(source, target='init.el'):
    if source is None:
        if os.name == "nt":
            source = 'windows-init.el'
        else:
            source = 'vscode-init.el'

    if os.name == "nt":
        source_file_name = os.path.join(os.getcwd(), source)
        target_file_name = os.path.join(home, 'AppData', 'Roaming', '.emacs.d', target)
    else:
        source_file_name = os.path.join(os.getcwd(), source)
        target_file_name = os.path.join(home, '.emacs.d', target)

    with open(source_file_name, 'r') as source_file:
        with open(target_file_name, 'w') as target_file:
            target_file.write(source_file.read())
    
    print(f'copy {source_file_name} to {target_file_name}')    

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("sourcefile", nargs="?", help="replace init.el from argument")
    args = parser.parse_args()

    el_list = ['common.el','tip-of-the-day.el', 'my-function.el', 'my-style.el', 'windows-init.el', 'vscode-init.el']
    
    check_backup_folder()
    check_file()
    copy_file(args.sourcefile)
    
    for file in el_list:
        copy_file(file, file)
    
    
