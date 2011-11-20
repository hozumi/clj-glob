# glob

Finds files based on glob patterns like `"*.jpg"` or `"/usr/*/se*"`. Similar to glob in Perl, Ruby, and PHP.

## Usage

    (use 'org.satta.glob))

    (glob "*.{jpg,gif}")
    => (#<File cat.jpg> #<File dog.gif>)
    
    (glob "*.{jpg,gif}" :s)  ; return string of path by adding :s option
    => ("cat.jpg" "dog.gif")
    
    (glob "*.JPG" :i)  ; ignore case.
    => (#<File cat.jpg>)
    
    (glob ".*")  ; dot files are not included by default
    => (#<File .git> #<File .gitignore>)

    (glob "*g*" :a)  ; dot files are included by adding :a option
    => (#<File .git> #<File .gitignore>)

    (glob "/usr/*/se*")  ; works on directories and subdirectories
    => (#<File /usr/bin/security> #<File /usr/bin/sed> ...)
    
    (glob "**/*clj")  ; zsh's ** recursive search can be used.
    => (#<File project.clj> #<File src/org/satta/glob.clj> #<File test/org/satta/glob_test.clj>)

    (glob "c:/Windows/*/*.dll")  ; works on Windows

## Caveats

*    Use __slashes__ in glob pattern, the syntax is the same on Windows as *nix

## Installation

Add this dependency to your project.clj:

    [org.clojars.hozumi/clj-glob "0.1.2"]

## License

EPL
