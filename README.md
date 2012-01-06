# google-translate --- Emacs interface to Google Translate

## Summary

Invoking the function `google-translate-query-translate` queries the
source and target languages and text to translate, and shows a buffer
with available translations of the text.

## Installation

Assuming that the file `google-translate.el` is somewhere on the
load path, add the following lines to your `.emacs` file:

    (require 'google-translate)
    (global-set-key "\C-ct" 'google-translate-query-translate)

Change the key binding to your liking.


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
leave this parameter blank by pressing `RET`.
