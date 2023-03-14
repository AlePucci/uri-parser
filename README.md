# Common Lisp and Prolog URI parser

Project made for the exam of the Programming Languages course (academic year 2021-2022) at UniMiB.

The purpose of the project was to create a Common Lisp and Prolog library that can build structures, internally representing **URI**s (Uniform Resource Identifiers), starting from their representations as strings. 
Parsing requires analyzing and decomposing the input string in sequence, character from character from left to right, in order to build a proper structure that stores its seven components.

Check [Lisp README.md](/Lisp/README.md) for Common Lisp usage.
  
Check [Prolog README.md](/Prolog/README.md) for Prolog usage.

## What is an URI?
A Uniform Resource Identifier (URI) is a unique sequence of characters that identifies a logical or physical resource used by web technologies. URIs may be used to identify anything, including real-world objects, such as people and places, concepts, or information resources such as web pages and books. 

[source: https://en.wikipedia.org/wiki/Uniform_Resource_Identifier]

## What Request for Comments was used as a reference?
A simplified version of **[RFC 3986](https://datatracker.ietf.org/doc/html/rfc3986)**:

```
URI ::= URI1 | URI2
URI1 ::= scheme ‘:’ [authorithy] [[‘/’] [path] [‘?’ query] [‘#’ fragment]]
URI2 ::= scheme ‘:’ scheme-syntax
scheme ::= <identifier>
authorithy ::= ‘//’ [ userinfo ‘@’ ] host [‘:’ port]
userinfo ::= <identifier>
host ::= <host-identifier> [‘.’ <host-identifier>]* | IP-address
port ::= <digit>+
IP-address ::= <NNN.NNN.NNN.NNN – N is a digit>
path ::= <identifier> [‘/’ <identifier>]* [‘/’]
query ::= <characters without ‘#’>+
fragment ::= <characters>+
<identifier> ::= <characters without ‘/’, ‘?’, ‘#’, ‘@’, or ‘:’>+
<host-identifier> ::= <characters without ‘.’, ‘/’, ‘?’, ‘#’, ‘@’, or ‘:’>+
<digit> ::= ‘0’ |‘1’ |‘2’ |‘3’ |‘4’ |‘5’ |‘6’ |‘7’ |‘8’ |‘9’
scheme-syntax ::= <special syntaxes - see below>
``` 

**Special syntaxes:**

``` 
Mailto:
scheme ::= "mailto"  ->  scheme-syntax ::= [userinfo [‘@’ host]]

News:
scheme ::= "news" -> scheme-syntax ::= [host]

Tel/fax:
scheme ::= "tel" | "fax" -> scheme-syntax ::= [userinfo]

Zos:
scheme ::= "zos" -> URI1 ::= scheme ‘:’ [authorithy] [[‘/’] [path] [‘?’ query] [‘#’ fragment]]

In this case, path is quite different, but the other fields need to be checked like usual.
path ::= <id44> [‘(’ <id8> ‘)’]
id44 ::= (<alphanum> | ‘.’)+     (max. length = 44 characters. Can not end with a ‘.’)
id8 ::= (<alphanum>)+            (max. length = 8 characters)
alphanum ::= <alphabetic characters and digits>
``` 

