# Emacs config
My Emacs config. Includes a cheat sheet.

## Installation (Linux)
1. Download the repository. You can use **git clone** for that.
```
mkdir ~/emacs_config
cd ~/emacs_config
git clone https://github.com/bormoge/.emacs.d
```
2. Open the directory **.emacs.d** through the terminal using the **cd** command.
```
cd ./.emacs.d
```
3. Once inside **.emacs.d**, use the **make** command to copy the directory onto Home (~/.emacs.d).
A warning, before you go any further: if you have an ~/.emacs.d directory it will get replaced with the contents of this repository.
```
make
```
4. (Optional) Remove the emacs_config directory.
```
cd ~/
rm -rf ~/emacs_config
```