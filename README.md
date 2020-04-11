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

TEN leverages CLOS for implementing template inheritance.

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
  
Control tags are used to specify Lisp code "inline" in the template, and
tend to contain imperative code, their return values are ignored.
The value returned by output tags are interpolated into the template. The function called can be any
Lisp function, or another template.

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
## Templates

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

## ASDF

To compile the templates, use `:ten-template` in the project's ASDF system definition:

```lisp
(:ten-template "filename")
```
The default file extension is "ten", but another can be specified via the `:filename-extension` option; and the template package can be specified with the `:package` option. Look at [ten.examples ASDF system](https://github.com/mmontone/ten/blob/master/ten.examples.asd) for an example.

For manually compiling templates, use `ten:compile-template` function.

## Inheritance

To make a template inherit from anohter, use the `:extends` option in template definition.

Templates are organized in `sections`. `sections` are the parts of the templates that are inherited.

Use `{{super}}` inside a `section` to render the parent `section`.

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

`(func1 (func2 (.. (funcN value arg1 .. argN)) arg1 .. argN) arg1 .. argN)`

In general, filter functions are expected to receive the value to be filtered as first parameter. 
But, for several Lisp functions that's not the case. In those cases, it is possible to use `_` to indicate where the filter function should receive the value.

For example, `string-trim` receives the string to trim as second value, so, to apply it as filter we do:

`{{str | string-trim '(#\%) _}}`

Have a look at some [examples of filters](https://github.com/mmontone/ten/tree/master/examples/filters.html).

## Examples

Load and have a look at the [examples](https://github.com/mmontone/ten/tree/master/examples).

```lisp
(require :ten.examples)
```

## License

MIT
