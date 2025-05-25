# Fable Game Test

This is just me testing to see how good is fable for making games!

## Game Design

My idea right now is to make a top-down 2d Super Hot. So -
time-only-moves-when-you-move bullet hell!
This fits good with the limitations of canvas graphics and not having a game
engine

## Technical Design

The game is split into very isolated modules that each exposes minimal things
that allow you to interact with them. Most modules will be pure and functional,
allowing composability and easy testing!

Even game engine stuff will be modularized and isolated, as much as possible.
But I will allow the more "game-engine-y" stuff to leak mutation.
For example, there is Rendering.fs that is completely isolated from the rest
of the game engine, everything you want to know about how this game renders
stuff is in that module and that module only. But (for now) it does expose an
API that requires you to side-effect to draw.
