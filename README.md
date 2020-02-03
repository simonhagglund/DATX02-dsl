# DATX02-dsl


## stack

Is the bread and butter of all Haskell coding, it's a tool that lets us develop and use each others code on different OS:s, with the same Haskell version.

[stack install](https://docs.haskellstack.org/en/stable/README/).

## codin' enviorment suggestion


[Visual Studio Code](https://code.visualstudio.com/)

Keep in mind that `cabal` and `stack` can(always?) be use interchangeably
[setup VS code for Haskell](https://medium.com/@dogwith1eye/setting-up-haskell-in-vs-code-on-macos-d2cc1ce9f60a)

[setup Atom for Haskell](https://github.com/simonmichael/haskell-atom-setup)

[Atom git integration](https://atom.io/packages/git-plus)

in terminal one could do:
```bash
# add your changes.
git add -p .                    #press y/n for your changes.
git commit -m "Some short text" #-m(message) will be saved forever.

# then
git fetch
git status # if git tells you that you are up to date. goto #push else
git pull --rebase # with this you are least likely to get mearge conflicts
                  # and if you do you have to solve them manualy.

#push
git push
```


## directories explained

### haskell/play/{author}
Save/play with code here.

### haskell/project
Put code relevant for the course here.

### planning-report
This directory is a git submodule, that means that this git repo has the planning-report directory as a dependency, such that this repo ties all repos together.

#### Motivation
Overleaf can sync to a Github repository, making versioning of the reports seamless and easy instead of complicated and dangerous.

Commit from overleaf by clicking menu->github and press sync.

## Reports
[Planning report](https://www.overleaf.com/6341261623qvmpxjkcdzqp).

## Links
[Drive](https://drive.google.com/drive/folders/13Dp8TciAj32UDbi2cMLHMFrGdX0Hr7FH)
[Links on drive](https://almqvist.lib.chalmers.se/inst_fack/bookingsystem/lecture_enroll.html).
[Time logging](https://docs.google.com/spreadsheets/d/1cGmbrufZfXkOSES6kOwcRWieCc9IW40DgF3j-mnZMpo/edit?usp=drive_web&ouid=108657493180180186395)
