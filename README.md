# TEN

[![Build Status](https://travis-ci.org/mmontone/ten.svg?branch=master)](https://travis-ci.org/mmontone/ten)
[![Quicklisp](http://quickdocs.org/badge/ten.svg)](http://quickdocs.org/ten/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

Yet another template system for Common Lisp.

WIP

NOTE: This is alpha quality. Not all design decisions have been made yet and things are subject to change.

TEN is a fork of [ECO template system](https://github.com/eudoxia0/eco) by Fernando Borretti.

Like ECO, TEN compiles templates to Lisp code, but has some differences:
- Support for templates inheritance.
- Dot syntax for accessing template data.
- Special syntax for filters.
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
- `:dot-syntax`: If T, templates are compiled with dot syntax enabled. Dot syntax is implemented via the Lisp library `access`. Default is T.
- `:package`: The package in which to compile and export the template. By default, templates are compiled and exported in `TEN-TEMPLATES` package.
- `:escape-html`: Whether to escape html in output tags. Default is T.

## ASDF

To compile the templates, use `:ten-template` in the project's ASDF system definition:

```lisp
(:ten-template "filename")
```
The default file extension is "ten", but another can be specified via the `:filename-extension` option; and the template package can be specified with the `:package` option. Look at `ten/examples` package for an example.

## Inheritance
TO BE WRITTEN

## Dot syntax
TO BE WRITTEN

## Includes
TO BE WRITTEN

## Filters
TO BE WRITTEN

## Examples

Load and have a look at the [examples](https://github.com/mmontone/ten/tree/master/examples).

```lisp
(require :ten.examples)
```

## License

MIT

