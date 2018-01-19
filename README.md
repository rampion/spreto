A **sp**eed-**re**ading **to**ol, inspired by [spritz](http://spritzinc.com/).

`spreto` is a command-line tool. Simply call it with a text file:

    $ spreto examples/alice.txt
    ────────────────────┬───────────────────────────────────────────────────────────
    ████████████████████████████████████████████████████████████████████████████████
    ────────────────────┴───────────────────────────────────────────────────────────

After starting, `spreto` will provide a brief countdown before displaying one
word at a time from the given text file, at a default rate of 250wpm.

While running, you can use the keyboard to issue various commands to `spreto`:

- `<Space>` - pause/unpause
- `q` - quit
- `h` or `?` - pause and show the keyboard commands
- `b`/`B` - jump back/forward 5 seconds
- `s`/`S` - jump back/forward one sentence
- `p`/`P` - jump back/forward one paragraph
- `r` - toggle reading direction backwards/forwards
- `<Left>` - go one word back and pause
- `<Right>` - go one word forward and pause
- `<Up>` - increase wpm
- `<Down>` - decrease wpm

Pausing will display the context of the current word, allong with the wpm and
the position (in time, words, sentences, and paragraphs).

    ────────────────────┬───────────────────────────────────────────────────────────
    …umphantly.  Alice did not quite know what to say to this: so she helped hersel…
    ────────────────────┴───────────────────────────────────────────────────────────
        250wpm          51%           1:00:17/1:57:51          403.0.1
     403/881 paragraphs       1104/2290 sentences       15073/29465 words

`spreto` also has command line options to allow you to start with a custom
wpm or at a certain position in the text.
