# forest.hx

forest.hx is a file tree explorer for [Helix](https://github.com/helix-editor/helix/), built as a persistent sidebar panel with an integrated fuzzy search bar.

![forest.hx preview](.github/assets/preview.gif)

---

## Installation

**1. Install the plugin-enabled fork of Helix** by following the instructions [here](https://github.com/mattwparas/helix/blob/steel-event-system/STEEL.md).

**2. Install forest.hx via forge:**

```sh
forge pkg install --git https://github.com/Ra77a3l3-jar/forest.hx.git
```

**3. Load the plugin** by adding this to your `init.scm`:

```scheme
(require "forest/forest.scm")

;; Optional: which side the tree renders on ('left by default)
;; (forest-configure! side)
(forest-configure! 'left)
```

Bind `:forest-open` to a key, e.g. in `init.scm`:

```scheme
(keymap (global)
        (normal (space (e ":forest-open"))))
```

---

## Usage

| Key | Action |
|-----|--------|
| `↑` / `↓` / `j` / `k` | Navigate |
| `Enter` | Open the selected file, or toggle the selected directory |
| `Tab` | Toggle the selected directory (outside search) |
| `/` | Start typing a fuzzy search query |
| `n` | Create a file or directory (end name with `/` for a directory) |
| `r` | Rename the selected entry |
| `d` | Delete the selected entry |
| `R` | Refresh the tree |
| `+` / `-` | Widen / narrow the panel |
| `Esc` | Switch focus to the editor, panel stays open |
| `q` | Close the panel |

Opening or refocusing the tree reveals and centers whatever file is currently open in the editor.

## Notes

- Requires [notify.hx](https://github.com/chuwy/notify.hx) (pulled in automatically as a dependency) for create/rename/delete notifications.
