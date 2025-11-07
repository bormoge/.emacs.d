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
M-x package-vc-upgrade PACKAGE-NAME
```
Where **PACKAGE-NAME** is replaced with the actual name of the package.

If you want to upgrade all packages installed using the command **package-vc-install**:
```
M-x package-vc-upgrade-all
```

### lsp

To install / update a language server use the following command inside Emacs:
```
M-x lsp-install-server
```

## License

SPDX-License-Identifier: GPL-3.0-or-later

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
