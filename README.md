# virtual-piano
Terminal based virtual piano in Haskell, with ncurses and Euterpea. Very rudimentary implementation, but it plays notes!

[Euterpea requires that you have a default MIDI device on your computer.](http://www.euterpea.com/euterpea/setting-up-midi/)

![Screenshot](https://i.imgur.com/GzHtyva.png)

The current implementation is just a proof of concept. I'll get back to this project when I learn more about concurrency in Haskell. (I'm a bit ashamed that currently I'm `forkIO`ing for every note, which is terrible.) Feel free to fork and/or send pull requests!

TODO:

* Fix concurrency! There are errors if you play more than 4-5 notes at the same time. (as in, when the previous ones aren't finished playing yet.)
* Keep the notes playing if the key stays pressed? Silence the note when the key is released? I don't even know how this would be possible with ncurses.
* Changing the color of the pressed key as it is pressed.
* Command line options to change the size of the keyboard, range etc. Graphics completely depend on `Options` so this shouldn't be too difficult with `optparse-applicative`.

# Usage

```
stack install
virtual-piano
```

Then type the characters written on the keys to play them.  The character order is supposed to resemble an ANSI QWERTY keyboard.

You can press escape or backtick to exit the program.
