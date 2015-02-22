#!/bin/bash

mkdir -p ~/.vim/{backup,tmp,undodir,cache/ctrlp}

[ -e package.json ] && npm install
