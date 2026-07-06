# VHS tape for forest.hx — https://github.com/charmbracelet/vhs
# Run with: vhs demo.tape
#
# Uses ~/Dev/ziglings as the sandbox project: it's a git repo, so any mistake
# is trivially reversible, and the script deletes its own test file at the
# end anyway so the repo is left exactly as it was found.
#
# Opens README.md at the workspace root (not exercises/ or patches/ — both
# hold 100+ files and would clutter the tree). Assumes :forest-open is bound
# to `space e` in init.scm, matching the README.

Require hx

Output "/home/raffaele/Dev/ziglings/forest-demo.gif"

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
# force a solid dark theme for the recording — the live config uses
# catppuccin_mocha_transparent, which relies on real terminal transparency
# and can render inconsistently in a flat capture
Type ":theme base16_black_methal_bathory"
Enter
Sleep 1s
Show

Sleep 500ms

# open the tree — reveals and centers the file already open in the editor
Space
Type "e"
Sleep 1.2s

# jump to a known position (top of tree) regardless of where reveal landed
Up@100ms 20
Sleep 400ms

# fold/unfold a small directory (images/, 2 files) — avoid exercises/ and
# patches/, they hold 100+ files each and would clutter the view
Down@250ms 4
Sleep 400ms
Tab
Sleep 700ms
Down@300ms 2
Sleep 400ms
Up@300ms 2
Sleep 400ms
Tab
Sleep 500ms

# same with a second small directory (test/, 1 file), passing over patches/
# without touching it
Down@250ms 2
Sleep 400ms
Tab
Sleep 700ms
Down@300ms 1
Sleep 400ms
Up@300ms 1
Sleep 400ms
Tab
Sleep 500ms

# fuzzy search for the file already open in the editor
Type@100ms "readme"
Sleep 1s
Backspace@60ms 6
Sleep 400ms

# every search keystroke resets the cursor to row 0, so re-navigate to
# test/ (row 6) before creating inside it
Down@250ms 6
Sleep 400ms

# create a file inside test/ (Ctrl+n) — base path is prefilled and editable
Ctrl+n
Sleep 600ms
Type@80ms "demo-note.txt"
Sleep 500ms
Enter
Sleep 1.5s

# search for it, then rename in place (Ctrl+r) without needing to open it
Type@80ms "demo-note"
Sleep 600ms
Ctrl+r
Sleep 600ms
Backspace@50ms 13
Sleep 300ms
Type@80ms "renamed-note.txt"
Sleep 500ms
Enter
Sleep 1.5s

# clear the stale query, re-search, then delete (Ctrl+x, y to confirm)
Backspace@50ms 12
Sleep 300ms
Type@80ms "renamed"
Sleep 700ms
Ctrl+x
Sleep 600ms
Type "y"
Sleep 1.5s

Backspace@50ms 10
Sleep 400ms

# resize the panel
Alt+"="
Alt+"="
Alt+"="
Sleep 200ms
Alt+"-"
Alt+"-"
Alt+"-"
Sleep 230ms

# switch focus to the editor and back, panel stays open the whole time
Escape
Sleep 800ms
Space
Type "e"
Sleep 800ms

# close the panel entirely
Ctrl+q
Sleep 500ms

Hide
Type ":q!"
Enter
Sleep 500ms
