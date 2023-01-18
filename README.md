# Common Lisp and Prolog URI parser

Project made for the exam of the Programming Languages course (academic year 2021-2022) at UniMiB.

The purpose of the project was to create a Common Lisp and Prolog library that can build structures, internally representing URIs (Uniform Resource Identifiers), starting from their representations as strings. 
Parsing requires analyzing and decomposing the input string in sequence, character from character from left to right, in order to build a proper structure that stores its seven components.

Check [Lisp README.md](/Lisp/README.md) for Common Lisp usage.
  
Check [Prolog README.md](/Prolog/README.md) for Prolog usage.

# What is an URI?
A Uniform Resource Identifier (URI) is a unique sequence of characters that identifies a logical or physical resource used by web technologies. URIs may be used to identify anything, including real-world objects, such as people and places, concepts, or information resources such as web pages and books. [source: https://en.wikipedia.org/wiki/Uniform_Resource_Identifier]

# What Request for Comments was used as a reference?
**[RFC 3986](https://datatracker.ietf.org/doc/html/rfc3986)**
