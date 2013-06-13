# Emacs interface to Google Translate

## Summary

Invoking the function `google-translate-query-translate` queries the source and
target languages and text to translate, and shows a buffer with available
translations of the text.  The function `google-translate-at-point` does the
same, but instead of querying you, it uses the word at point or the currently
active region as text to be translated.

## Installation

Assuming that the file `google-translate.el` is somewhere on the
load path, add the following lines to your `.emacs` file:

    (require 'google-translate)
    (global-set-key "\C-ct" 'google-translate-at-point)
    (global-set-key "\C-cT" 'google-translate-query-translate)

Change the key bindings to your liking.


## Customization

You can customize the following variables:

- `google-translate-enable-ido-completion`

- `google-translate-default-source-language`

- `google-translate-default-target-language`

If `google-translate-enable-ido-completion` is non-NIL, the input
will be read with ido-style completion.

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
  to `"ru"`, and

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
languages supported by Google Translate (like `"ru"` for Russian
above).  See `google-translate-supported-languages` for the list of
the supported languages, or customize the defaults using the
customization mechanism of Emacs.  Setting a default language to
NIL means that language will always be queried.  Moreover, the
variable `google-translate-default-source-language` can be set to a
special value "auto" that is interpreted as the instruction for
Google Translate to detect the source language.  This option is
also available when you are queried for the source language: simply
leave this parameter blank by pressing `RET`.  (If you have enabled
the ido-style completion, "Detect language" is going to be the
first option, which you can select simply by hitting RET.)

The variable `google-translate-show-phonetic` controls whether the
phonetic spelling of the original text and its translation is
displayed if available.  If you want to see the phonetics, set this
variable to T.

There are also three faces you can customize:

- `google-translate-text-face`, used to display the original text
  (defaults to `default`)

- `google-translate-phonetic-face`, used to display the phonetics
  (defaults to `shadow`)

- `google-translate-translation-face`, used to display the highest
  ranking translation (defaults to `default` with the `weight`
  attribute set to `bold')

For example, to show the translation in a larger font change the
`height` attribute of the face `google-translate-translation-face`
like so:

    (set-face-attribute 'google-translate-translation-face nil :height 1.4)
