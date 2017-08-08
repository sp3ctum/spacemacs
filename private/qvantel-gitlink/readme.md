# Git links for Qvantel repositories

## Setup
Some configuration needs to be done specifically in all git remotes you want to
work with. Set one up by running this in your project directory - this case is
for the common `origin` remote name:

`git config remote.origin.viewurl https://stash.qvantel.net/projects/API/repos/bssapi-entities/`
(notice the trailing slash, it's required at the moment)

## Usage
- add this layer to your `dotspacemacs/layers` in your init file, using the name
  `qvantel-gitlink`
- Press `SPC g l l` to open the current file in the browser
- (press `SPC g l` and wait to view different `git-link` options supported by
  spacemacs)
