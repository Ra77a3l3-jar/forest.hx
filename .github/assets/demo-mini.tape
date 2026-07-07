# VHS tape for forest.hx (mini style) — https://github.com/charmbracelet/vhs
# Run with: vhs demo-mini.tape
#
# Uses ~/Dev/ziglings as the sandbox project: it's a git repo, so any mistake
# is trivially reversible, and the script deletes its own test file at the
# end anyway so the repo is left exactly as it was found.
#
# Assumes ~/.config/helix/init.scm has (forest-set-style! 'mini) and
# :forest-open bound to `space e`, matching the README.

Require hx

Output "/home/raffaele/Dev/plugins/demos/forest-mini-demo-catpp.gif"

Set Shell "bash"
Set Theme "VibrantInk"
Set FontFamily "JetBrainsMono Nerd Font Mono"
Set FontSize 18
Set Width 1600
Set Height 1000
Set Padding 20
Set TypingSpeed 60ms
Set WindowBar Colorful

Hide
Type "cd ~/Dev/ziglings && hx README.md"
Enter
Sleep 1.5s
Type ":theme catppuccin_mocha_transparent"
Enter
Sleep 1s
Show

Sleep 500ms

# open the tree — reveals the file already open in the editor, preview panel
# shows its contents automatically
Space
Type "e"
Sleep 1.2s

# browse up to images/ (a small directory, 2 files)
Up@120ms 10
Sleep 400ms

# cascade into it — a new column appears beside the root one
Enter
Sleep 700ms
Down@300ms 1
Sleep 400ms
Up@300ms 1
Sleep 400ms

# step back out — the column collapses, root column keeps its own cursor
Left
Sleep 500ms

# down to test/ (a small directory, 1 file)
Down@250ms 2
Sleep 400ms
Enter
Sleep 700ms

# create a file inside test/ — base path is prefilled and editable
Type "n"
Sleep 600ms
Type@80ms "mini-demo-note.txt"
Sleep 500ms
Enter
Sleep 1s

# rename it in place
Type "r"
Sleep 500ms
Backspace@50ms 18
Sleep 200ms
Type@80ms "renamed-note.txt"
Sleep 500ms
Enter
Sleep 1s

# delete it (y to confirm)
Type "d"
Sleep 600ms
Type "y"
Sleep 1s

# refresh, then back out to the root column
Type "R"
Sleep 400ms
Left
Sleep 500ms

# fuzzy search the whole workspace and jump to the match
Type "/"
Sleep 400ms
Type@80ms "readme"
Sleep 600ms
Enter
Sleep 800ms

# resize the columns
Type "+"
Type "+"
Type "+"
Sleep 300ms
Type "-"
Type "-"
Type "-"
Sleep 300ms

# close the panel
Type "q"
Sleep 500ms

Hide
Type ":q!"
Enter
Sleep 500ms
