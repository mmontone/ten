# TEN

[![Build Status](https://travis-ci.org/mmontone/ten.svg?branch=master)](https://travis-ci.org/mmontone/ten)
[![Quicklisp](http://quickdocs.org/badge/ten.svg)](http://quickdocs.org/ten/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

Yet another template system for Common Lisp.

WIP

NOTE: This is alpha quality. Not all design decisions have been made yet and things are subject to change.

TEN is a fork of [ECO template system](https://github.com/eudoxia0/eco) by Fernando Borretti.

Like ECO, TEN compiles templates to Lisp code, but has some differences:
- Two types of tags only. Control and output.
- Support for templates inheritance.
- Dot syntax for accessing template data.
- Convenient syntax for applying filters.
- Configurable syntax delimiters (planned, not done yet).

My reasons for writing yet another template system for Common Lisp is to combine the simplicity and usability of ECO (the whole Lisp language at your disposal for writing the templates), with features available in more complex template systems like [Djula](https://mmontone.github.io/djula/) that makes things easier for web development (inheritance, dot syntax, etc.).

## Usage

A TEN template looks like this:

```jinja
{% template ex1 () (user enabled &key items) %}
<html>
  <head>
  </head>
  <body>

    {{ user.name | string-capitalize }}

    {% if enabled %}
    Enabled
    {% else %}
    Disabled
    {% end %}

    {% when items %}
    <ul>
      {% loop for item in items do %}
      <li>{{ item }}</li>
      {% end %}
    </ul>
    {% end %}

    {% when (not items) %}
    There are no items
    {% end %}    
  </body>
</html>
{% end %}

```

There are two types of tags:
- *Output tags*: `{{ <var> }}`, becomes `<var>`, and `{{ <fn> &rest args }}`, that becomes `(fn arg1 arg2 .. argn)`.
- *Control tags*: `{% <expr> %} body {% end %}`, becomes `(<expr> body)`.
  
Control tags control which parts of the tamplate are rendered; their return value is ignored.

The value returned by output tags are interpolated into the template. The function called can be any
Lisp function, or another template (because templates are compiled to functions).

 For example:
 
 * `{{ user }}` => `user`
 * `{{ name user }}` =>  `(name user)`
 * `{% when (name user) %} ... {% end %}` => `(when (name user) ...)`
 
The `if` tag is a special case: it supports using an `else` tag to separate the true and
false branches. For example:

```lisp
{% if posts %}
  <h1>Recent Posts</h1>
  ... loop over posts ...
{% else %}
  No recent posts.
{% end %}
```

Also, more [advanced control expressions](https://github.com/mmontone/ten/blob/master/examples/control.html) are possible, like `let`, `case`, `cond`, etc.

## Template definition

Templates are defined with the following syntax:

```
{% template name (&rest options) (&rest args) %}
  ... body ...
{% end %}
```

Template options are:
- `:extends` : The template to extend from.
- `:dot-syntax`: If T, templates are compiled with dot syntax enabled. Dot syntax is implemented via the Lisp library `access`. Default is T.
- `:package`: The package in which to compile and export the template. By default, templates are compiled and exported in `TEN-TEMPLATES` package.
- `:escape-html`: Whether to escape html in output tags. Default is T.
- `:output-whitespace`. Default is T. When NIL, expressions that just spit whitespace are discarded.

## Template compilation

For manually compiling templates, use `ten:compile-template` function.

But more useful is to include them in the ASDF system definition of your project.

First, add `:ten` as ASDF system definition dependency:

`:defsystem-depends-on (:ten)`

Then, use `:ten-template` in to include the template files:

```lisp
(:ten-template "filename")
```

The default file extension is "ten", but another can be specified via the `:filename-extension` option; and the template package can be specified with the `:package` option. Look at [ten.examples ASDF system](https://github.com/mmontone/ten/blob/master/ten.examples.asd) for an example.

Templates are compiled into functions and exported in the indicated package. The default package is `ten-templates`, but that can be changed from either the ASDF system definition, the `ten:compile-template` parameters, or the `{% template %}` options.

When developing your project it is useful to be able to compile templates in an interactive way. 
If you are using Emacs + SLIME, load `ten.el` file.

Then use `M-X ten-compile-template` when on the template buffer to compile templates. Note that you may want to have `:package` option specified in the template so that it gets compiled into the correct package.

For debugging, you can inspect the expanded template using `ten:expand-template` function. In Emacs, go to template buffer an do `M-x ten-expand-template`.

If you enable `ten` minor mode, template compilation gets conveniently bound to `C-c C-c`, and template expansion to `C-c RET`. Best is to automatically enable the minor mode for template files adding something like `(add-hook 'web-mode-hook 'ten-mode)` to your `.emacs` initialization file.

## Inheritance

To make a template inherit from anohter, use the `:extends` option in template definition.

Templates are organized in `sections`. `sections` are the parts of the templates that are inherited.

Use `{{super}}` inside a `section` to render the parent `section`.

TEN leverages CLOS for implementing template inheritance. Templates are compiled to classes and generic functions `render-template` and `render-section`.

Have a look at some [examples of template inheritance](https://github.com/mmontone/ten/blob/master/examples/inheritance.html).

## Includes

To include other templates, just use the output tag with the name of the included template. Remember that templates are compiled to functions; just call those functions from the template to include them.

Have a look at [an example](https://github.com/mmontone/ten/blob/master/examples/include.html).

## Dot syntax

When dot syntax is enabled (it is, by default), it is possible to conveniently access objects with dot syntax in templates:

`{{ object.key1.key2 }}`

that gets translated by [access](https://github.com/AccelerationNet/access) library to:

`(access:accesses obj 'key1 'key2)`

Have a look at [an example](https://github.com/mmontone/ten/blob/master/examples/dot-syntax.html).

## Filters

TEN implements some convenient syntax for filters.

`{{ value | func1 arg1 .. argN| func2 arg1 .. argN| .. | funcN arg1 .. argN}}`

Filters are just normal functions that get applied to the value.

Filters are translated to functions application like this:

`(funcN (.. (func2 (func1 value arg1 .. argN) arg1 .. argN))) arg1 .. argN)`

In general, filter functions are expected to receive the value to be filtered as first parameter. 
But, for several Lisp functions that's not the case. In those cases, it is possible to use `_` to indicate where the filter function should receive the value.

For example, `string-trim` receives the string to trim as second value, so, to apply it as filter we do:

`{{str | string-trim '(#\%) _}}`

Filters syntax is completly optional, you can disregard it and just apply functions instead:

`{{ string-trim '(#\%) (string-capitalize str) }}`

Have a look at some [examples of filters](https://github.com/mmontone/ten/tree/master/examples/filters.html).

## Examples

Load and have a look at the [examples](https://github.com/mmontone/ten/tree/master/examples).

```lisp
(require :ten.examples)
```

## Troubleshooting

1) When specifying a template package other than `ten-templates`, if the package specified doesn't `:use` `ten` or `ten-template` packages, then you may run into problems trying to compile your templates. That may be because the `template` and `section` macros are not found in the specified package. In that case, make sure to prefix your `template` and `section` declarations with `ten:`, like:

```django
{% ten:template my-template (:package my-package) %}
{% ten:section my-section %}
{% end %}
{% end %}
```

2) Some "complex" expressions, like `cond` and `case`, require that you turn `:output-whitespace` to `NIL`. Otherwise, template compilation puts `write-string` expressions right in the middle of the `case` and `cond` bodies. Have a look at [this template](https://github.com/mmontone/ten/blob/master/examples/control.html). 

## License

MIT
