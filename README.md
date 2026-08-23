# forest.hx

forest.hx is a file tree explorer for [Helix](https://github.com/helix-editor/helix/), with two selectable styles: `snacks`, a persistent sidebar panel with an integrated fuzzy search bar (default), and `mini`, floating Miller columns with a live preview.

### 🍿 `snacks` style

![forest.hx snacks preview](.github/assets/preview-snacks.gif)

<details>
<summary>🔍 <code>mini</code> style preview</summary>

![forest.hx mini preview](.github/assets/preview-mini.gif)

</details>

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

;; Optional: which side the tree renders on ('left by default), and which
;; entry names are always hidden
(forest-configure! 'left #:ignore (list ".git" "target" "__pycache__"))

;; Optional: which explorer UI forest-open uses ('snacks by default)
;; (forest-set-style! style)
(forest-set-style! 'snacks) ; or 'mini

;; Optional (snacks): give the sidebar its own background per focus state, so the
;; tree stands apart from the buffer.
(forest-set-sidebar-bg! #:focused "#1e1e2e" #:unfocused "#181825")

;; Optional (snacks): color the search box outline. It marks focus by default
;; (orange focused, white unfocused); override the colors, or stop it changing.
(forest-set-search-color! #:focused "#89b4fa" #:unfocused "#585b70")
(forest-set-search-color! #:always "#89b4fa")            ; one color, both states
(forest-set-search-color! #:focused "#89b4fa" #:follow-focus? #f) ; never changes
```

Bind `:forest-open` to a key, e.g. in `init.scm`:

```scheme
(keymap (global)
        (normal (space (e ":forest-open"))))
```

---

## Usage

### `snacks` style

| Key | Action |
|-----|--------|
| `↑` / `↓` / `j` / `k` | Navigate within the current folder, wrapping at the ends |
| `→` / `l` | Enter the selected directory, or open the selected file |
| `←` / `h` | Leave the current folder and collapse it |
| `Enter` / `Tab` | Toggle the selected directory (outside search) |
| `/` | Start typing a fuzzy search query |
| `n` | Create a file or directory (end name with `/` for a directory) |
| `r` | Rename the selected entry |
| `d` | Delete the selected entry |
| `R` | Refresh the tree |
| `g` | Toggle dotfiles (`.env`, `.git`, etc.) |
| `i` | Toggle git-ignored entries |
| `+` / `-` | Widen / narrow the panel |
| `Space` | Open the which-key menu; press any listed key to run its action (`Esc` dismisses) |
| `Esc` | Switch focus to the editor, panel stays open |
| `q` | Close the panel |

Opening or refocusing the tree reveals and centers whatever file is currently open in the editor.

| Mouse | Action |
|-------|--------|
| Click an entry | Select it, focusing the panel first if the editor had focus |
| Click it again | Open the file, or toggle the directory |
| Click the search box | Start typing, keeping whatever query is already there |
| Wheel over the panel | Scroll the selection |
| Click in the buffer | Return focus to the editor, panel stays open |

### `mini` style

| Key | Action |
|-----|--------|
| `↑` / `↓` / `j` / `k` | Move within the active column |
| `→` / `l` / `Enter` | Open the selected file, or cascade into the selected directory |
| `←` / `h` | Back up to the parent column |
| `/` | Fuzzy search the whole workspace and jump to the match |
| `n` | Create a file or directory (end name with `/` for a directory) |
| `r` | Rename the selected entry |
| `d` | Delete the selected entry |
| `R` | Refresh the active column |
| `g` | Toggle dotfiles (`.env`, `.git`, etc.) |
| `i` | Toggle git-ignored entries |
| `+` / `-` | Widen / narrow the columns |
| `Space` | Open the which-key menu; press any listed key to run its action (`Esc` dismisses) |
| `Esc` / `q` | Close |

Opening the tree reveals whatever file is currently open in the editor, cascading a column for each ancestor directory along the way.

| Mouse | Action |
|-------|--------|
| Click an entry | Select it; in an ancestor column, drop the columns cascaded off it |
| Click it again | Open the file, or cascade into the directory |
| Click in the preview | Cascade into the previewed directory, landing on the entry clicked |
| Wheel over the active column or preview | Move the selection |
| Click off the columns | Close |

## Notes

- Press `Space` opens the which-key menu in the bottom right corner, and when pressed any listed key to run its action.
- Mouse support needs Helix's own `editor.mouse` left on (it is by default).
- Two clicks on an entry activate it. They needn't be quick, but a keypress in between cancels.
- Requires [notify.hx](https://github.com/chuwy/notify.hx) (pulled in automatically as a dependency) for create/rename/delete notifications.
- Uses [glyph.hx](https://github.com/Ra77a3l3-jar/glyph.hx) for all the diffrent icons.
