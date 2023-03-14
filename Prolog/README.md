# Prolog URI parser

Group members:
- Alessandro Pucci 869177
- Matias Aldo Ruiz 869139

The purpose of this project is to create a Prolog library that can build 
structures, internally representing **URI**s (Uniform Resource Identifiers), 
starting from their representations as strings.
Parsing requires analyzing and decomposing the input string in sequence, 
character from character from left to right, in order to build a proper 
structure that stores its seven components.

We adopted **DCG**s (Definite Clause Grammars) in our Prolog program to
carry out the project.

An URI is made of "Scheme", "Userinfo, "Host", "Port", "Path", "Query" and
"Fragment".

An uri-structure is like this: uri(Scheme, Userinfo, Host, Port, Path,
Query, Fragment).

If not specified, in every scheme, 80 is the default port.
An empty field, if the URI is still valid, is set to [] (empty list).

If uri_parse is called with an invalid URIString, it returns the following
error: "URI is not valid!".

-----------------------------------------------------------------------------

Call `uri_parse(URIString, URI).` in order to get URI structure.

Call `uri_display(URI).` in order to print on screen (std output)
     URI fields.
     
Note that *uri_display(URI) automatically closes the stream!*

Call `uri_display(URI, Stream).` in order to print on the provided
     Stream URI fields.
     
Note that *uri_display(URI, Stream) automatically closes the stream!*


Thanks for reading!
