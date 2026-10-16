# Emacs config
My Emacs config. Includes a cheat sheet.

## Installation (Linux)
Open the terminal and execute the following command:
```
cd ~
```
This should move you to your user's Home directory.

Download the repository. You can use **git clone** for that.
```
git clone https://github.com/bormoge/.emacs.d
```
After opening Emacs, it should automatically load the configuration.

If the configuration fails to load you can try creating a **.emacs** file in your Home directory.
The next command will create a **.emacs** file with the necessary code to load the configuration:
```
echo '(load-file (expand-file-name "init.el" user-emacs-directory))' > .emacs
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

To install, update, or uninstall a language server / linter / formatter / debug adapter use the following commands inside Emacs:
```
M-x mason-install
M-x mason-update
M-x mason-uninstall
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
