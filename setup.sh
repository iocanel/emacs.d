#!/bin/bash

SOURCE_PATH="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

echo "Linking emacs.d init files ..."
ln -s ${SOURCE_PATH}/early-init.el ${HOME}/.emacs.d/early-init.el
ln -s ${SOURCE_PATH}/init.el ${HOME}/.emacs.d/init.el

# Copy the versions profile
mkdir -p ${HOME}/.emacs.d/straight/versions
cp ${HOME}/versions/default.el ${HOME}/.emacs.d/straight/versions/
