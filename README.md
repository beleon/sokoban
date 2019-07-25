# Sokoban

An implementation of the classic Sokoban game in Haskell. The fully functioning
prototype including one hardcoded map and a very simple terminal UI was build
over the course of two train rides. Later a basic GUI was added using the
awesome SDL2 Haskell bindings.

# Acknowledgements

The free pixel art used for the game comes these from awesome artists:
- [Pixel_Poem](https://twitter.com/pixel_poem):
  [2D Pixel Dungeon](https://pixel-poem.itch.io/dungeon-assetpuck)
- [Vryell](https://twitter.com/Vryell):
  [Tiny Adventure Pack](https://vryell.itch.io/tiny-adventure-pack)

The [Dino Rush](https://github.com/jxv/dino-rush) project was a very helpful
reference that helped me make sense of the SDL2 Haskell bindings.

# What works

- Basic GUI
- Complete game logic
- Advancing to the next level

# Not supported (yet)

- Restarting a level
- Level selection
- Undo move
- Move counter
- Game menu
- Sound

# Dependencies

- stack
- sdl2
- sdl2-image

# Installation

Clone the project, change into the root directory of the project and use

```
stack build && stack exec sokoban
```

to first build and the run Sokoban.

# How to play

Use arrow keys to move. Map will automatically change after successfully
finishing a level.

# Rules

Push all boxes on top of goals. Pulling box is not possible.
