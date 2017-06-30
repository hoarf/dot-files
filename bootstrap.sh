#!/usr/bin/env bash
PWD=$(pwd)
ln -s "$PWD/.alias" "$HOME/.alias"
ln -s "$PWD/.gemrc" "$HOME/.gemrc"
ln -s "$PWD/.gitconfig" "$HOME/.gitconfig"
ln -s "$PWD/.gitignore" "$HOME/.gitignore"
ln -s "$PWD/.vimrc" "$HOME/.vimrc"
ln -s "$PWD/.myzshrc" "$HOME/.myzshrc"
ln -s "$PWD/.tmux.cong" "$HOME/.tmux.cong"

ln -s "$PWD/bin" "$HOME/bin"
