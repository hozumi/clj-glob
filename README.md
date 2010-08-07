# glob

Finds files based on glob patterns like `"*.jpg"` or `"/usr/*/se*"`. Similar to glob in Perl, Ruby, and PHP.

## Usage

    (ns foo.bar
      (:use org.satta.glob))

    (glob "*.{jpg,gif}")

    (glob ".*")  ; dot files are not included by default

    (glob "/usr/*/se*")  ; works on directories and subdirectories

    (glob "c:/Windows/*/*.dll")  ; works on Windows

## Caveats

*    It is __case sensitive__, so "windows" and "Windows" is not the same

*    Use __slashes__ in glob pattern, the syntax is the same on Windows as *nix

## Installation

Add this dependency to your project.clj:

    [clj-glob "0.1.0"]

## License

EPL
