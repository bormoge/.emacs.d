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
3. Once inside **.emacs.d**, use the **make** command to copy the directory to Home (~/.emacs.d).

A warning, before you go any further: if you have an ~/.emacs.d directory it will get replaced with the contents of this repository.
```
make
```
4. (Optional) Remove the emacs_config directory.
```
cd ~/
rm -rf ~/emacs_config
```

## Upgrading packages

1. For packages installed from ELPA and MELPA:
```
M-x list-packages
U
x
```
Alternatively:
```
M-x package-upgrade PACKAGE-NAME
```
Where **PACKAGE-NAME** is replaced with the actual name of the package.

Also, if you want to upgrade all packages:
```
M-x package-upgrade-all
```

2. For packages installed from source using **package-vc-install**:
```
package-vc-upgrade PACKAGE-NAME
```
Where **PACKAGE-NAME** is replaced with the actual name of the package.

If you want to upgrade all packages installed using the command **package-vc-install**:
```
M-x package-vc-upgrade-all
```
