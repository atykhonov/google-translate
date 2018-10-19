# Emacs interface to Google Translate

[![Join the chat at https://gitter.im/atykhonov/google-translate](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/atykhonov/google-translate?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Summary

This package allows to translate the strings using Google Translate
service directly from GNU Emacs.

## Installation

#### From MELPA

Just run `M-x package-install RET google-translate RET`

#### Manual installation

Assuming that the file `google-translate.el` is somewhere on the
load path, add the following lines to your `.emacs` file:

    (require 'google-translate)
    (require 'google-translate-default-ui)
    (global-set-key "\C-ct" 'google-translate-at-point)
    (global-set-key "\C-cT" 'google-translate-query-translate)

or

    (require 'google-translate)
    (require 'google-translate-smooth-ui)
    (global-set-key "\C-ct" 'google-translate-smooth-translate)

The difference between these configurations is in UI which will be
used: Default UI or Smooth UI.

## Default UI (google-translate-default-ui.el)

This file provides default UI for the Google Translate package. It was
originally written by Oleksandr Manzyuk and was part of
google-translate.el. It was extracted to
google-translate-default-ui.el file due to refactoring (the goal of
which is to separate backend from UI and provide better way for having
different UIs for Google Translate package).

Invoking the function `google-translate-query-translate` queries the
source and target languages and text to translate, and shows a buffer
with available translations of the text.  Invoking the function
`google-translate-at-point` translates the word at point or the active
region.

#### Default UI Customization

You can customize the following variables:

- `google-translate-default-source-language`

- `google-translate-default-target-language`

If the variable `google-translate-default-source-language` is set
to a non-NIL value, the source language won't be queried and that
value will be used instead.  Analogously, if you set the variable
`google-translate-default-target-language` to some non-NIL value,
that value will be used without querying.

You can always override this behavior by supplying a `C-u` prefix
argument to the function `google-translate-query-translate`.

Here is an example.  Suppose that your native language is Russian
and you frequently need to translate from various languages to
Russian.  Then it is reasonable

- to set the variable `google-translate-default-target-language`
  to "ru", and

- to leave `google-translate-default-source-language` set to its
  default value, NIL.

In this case, the function `google-translate-query-translate` is
only going to query the source language and text to translate.
If you need to translate to some language other than Russian, you
can override the default for the target language by supplying a
`C-u` prefix argument, in which case you will be queried for both
the source and target languages, as well as text to translate.

If you frequently translate from some fixed language, it is also
reasonable to set `google-translate-default-source-language` to
an appropriate value.

If you have both the default source and target languages specified,
you may like to bind functions `google-translate-at-point-reverse`
and `google-translate-query-translate-reverse` to some keys, e.g.:

  (global-set-key (kbd "C-c r") 'google-translate-at-point-reverse)
  (global-set-key (kbd "C-c R") 'google-translate-query-translate-reverse)

This will allow you to quickly translate in the reverse direction.
When the default source (resp. target) language is not set, the
target (resp. source) language of the reverse translation will be
queried interactively.

The admitted values of `google-translate-default-source-language`
and `google-translate-default-target-language` are the codes of the
languages supported by Google Translate (like "ru" for Russian
above).  See `google-translate-supported-languages` for the list of
the supported languages, or customize the defaults using the
customization mechanism of Emacs.  Setting a default language to
NIL means that language will always be queried.  Moreover, the
variable `google-translate-default-source-language` can be set to a
special value "auto" that is interpreted as the instruction for
Google Translate to detect the source language.  This option is
also available when you are queried for the source language: simply
leave this parameter blank by pressing RET.  (If you have enabled
the ido-style completion, "Detect language" is going to be the
first option, which you can select simply by hitting RET.)

## Smooth UI (google-translate-smooth-ui.el)

Smooth UI is a just alternative to the Default UI. It was written with
mind to provide improved user interface and, especially, to achieve
better supporting of many default languages. Default UI supports two
default languages very well but there is no space for the third one.

Invoking the function `google-translate-smooth-translate` queries
text and (optionally) the source and target languages to translate,
and shows a buffer with available translations of the text.

#### Smooth UI Configuration:

It is reasonable to define the following variable:

- `google-translate-translation-directions-alist`

- `google-translate-preferable-input-methods-alist`

`google-translate-translation-directions-alist` alist is intended
to contain translation directions.

For example it could be defined (in your .emacs or init.el) as:

(setq google-translate-translation-directions-alist '(("en" . "ru")))

in this way one translation direction ("en" > "ru") is defined and
when `google-translate-smooth-translate` function executes it will
output the prompt (in minibuffer) which will looks like as the
following:

```
[English > Russian] Translate:
```

You may set as many translation directions as you would like
to. For example such piece of code will define four translation
directions:

```
(setq google-translate-translation-directions-alist
      '(("de" . "en") ("en" . "de") ("de" . "fr") ("fr" . "de")))
```

in this way, when `google-translate-smooth-translate` function
executes you'll be queried by the prompt which will looks like the
following:

```
[German > English] Translate:
```

and, also in this way, you'll be able to switch between different
translation directions directly from minibuffer by using `C-n` and
`C-p` key bindings. `C-n` key binding changes current translation
direction to the next direction defined in the
`google-translate-translation-directions-alist` variable. And `C-p`
key binding changes current translation direction to the previous
one. Thus, while executing `google-translate-smooth-translate`
function and having in minibuffer such prompt:

```
[German > English] Translate:
```

then after pressing `C-n` you'll get the following prompt:

```
[English > German] Translate:
```

By default `google-translate-translation-directions-alist` is empty
and thus during execution of `google-translate-smooth-translate`
you'll be queried (to input a text) by the prompt:

```
Translate:
```

And after inputed text you'll be queried also for the source and
target languages. To let the package to be known which languages
you would like to always use and to avoid repetitive language
quering it is reasonable to define them in the mentioned
`google-translate-translation-directions-alist` variable.

## Common UI Customization

Described customization options are actual for both UI features:
Default UI and Smooth UI.

You can customize the following variables:

- `google-translate-output-destination`

- `google-translate-enable-ido-completion`

- `google-translate-show-phonetic`

- `google-translate-listen-program`

- `google-translate-pop-up-buffer-set-focus`

`google-translate-output-destination` determines translation output
destination. If `nil` the translation output will be displayed in the
pop up buffer. If value equal to `echo-area` then translation outputs
in the Echo Area
(see
[Echo Area](http://www.gnu.org/software/emacs/manual/html_node/elisp/The-Echo-Area.html)). In
case of `popup` the translation outputs to the popup tooltip using
`popup` package. In case of `kill-ring` the translation outputs to the
kill ring. And in case of `current-buffer` the translation outputs to
the current buffer. If you would like output translation to the Echo
Area you would probably like to increase it because only part of
translation could be visible there with the default settings. To
increase Echo Area you could increase the value of
`max-mini-window-height` variable, for example: `(setq
max-mini-window-height 0.5)`.

If `google-translate-enable-ido-completion` is non-NIL, the input will
be read with ido-style completion.

The variable `google-translate-show-phonetic` controls whether the
phonetic spelling of the original text and its translation is
displayed if available. If you want to see the phonetics, set this
variable to t.

The variable `google-translate-listen-program` determines the program
to use to listen translations. By default the program looks for
`mplayer` in the PATH, if `mplayer` is found then listening function
will be available and you'll see `Listen` button in the buffer with
the translation. You can use any other suitable program. If you use
Windows please download and unpack `mplayer` and add its path
(directory) to to the system PATH variable. Please note that
translation listening is not available if
`google-translate-output-destination` is set to `echo-area` or
`pop-up`.

The variable `google-translate-pop-up-buffer-set-focus` determines
whether window (buffer) with translation gets focus when it pop
ups. If `nil`, it doesn't get focus and focus remains in the same
window as was before translation. If `t`, window (buffer with
translation) gets focus. Please note that that setting works only for
pop up buffer, i.e. when `google-translate-output-destination` is
`nil`.

The `google-translate-input-method-auto-toggling` variable
determines whether input method auto toggling is enabled or not.

While switching among languages I noticed that I change input
method quite often. Input method auto toggling allows switch on
appropriate input method while switching among languages. Auto
toggling will work in case of
`google-translate-input-method-auto-toggling` is set to `t` and
`google-translate-preferable-input-methods-alist` is defined
properly.

This variable may be defined as follow (just for example):

```
(setq google-translate-preferable-input-methods-alist '((nil . ("en"))
                                                        (ukrainian-programmer-dvorak . ("ru" "uk"))))
```

In this way, input method is disabled (because of nil) for the
minibuffer when source language is English. And
"ukrainian-programmer-dvorak" input method is enabled when source
language is Russian or Ukrainian.

#### Customization of faces

- `google-translate-text-face`, used to display the original text
  (defaults to `default`)

- `google-translate-phonetic-face`, used to display the phonetics
  (defaults to `shadow`)

- `google-translate-translation-face`, used to display the highest
  ranking translation (defaults to `default` with the `weight`
  attribute set to `bold`)

- `google-translate-suggestion-label-face` used to display the label
  for suggestion (defaults to `default` with the `foreground`
  attribute set to `red`)

- `google-translate-suggestion-face` used to display the suggestion in
  case of word is misspelled (defaults to `default` with the `slant`
  attribute set to `italic` and `underline` attribute set to `t`)

- `google-translate-listen-button-face` used to display the "Listen" button (defaults
  to `height' 0.8).

For example, to show the translation in a larger font change the
`height` attribute of the face `google-translate-translation-face`
like so:

```
  (set-face-attribute 'google-translate-translation-face nil :height 1.4)
```

## Contributors

- Tassilo Horn
- Bernard Hurley
- Chris Bilson
- [Takumi Kinjo](https://github.com/kinjo)
- [momomo5717](https://github.com/momomo5717)
- [Michihito Shigemura](https://github.com/shigemk2)
